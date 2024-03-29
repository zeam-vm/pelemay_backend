defmodule NodeActivator.Utils do
  @moduledoc false

  require Logger

  @spec generate_node_name(binary()) :: atom()
  def generate_node_name(prefix) do
    prefix = Regex.replace(~r/\s/, prefix, "-")
    random_string = :crypto.strong_rand_bytes(5) |> Base.encode32(case: :lower)

    :"#{prefix}_#{random_string}@#{get_hostname()}"
  end

  @spec get_hostname() :: binary()
  def get_hostname() do
    Logger.debug("os.type: #{inspect(:os.type())}")

    hostname_cmd = System.find_executable("hostname")

    if is_nil(hostname_cmd) do
      raise RuntimeError, "Fail to find the \"hostname\" command."
    end

    {result, exit_code} =
      case :os.type() do
        {:unix, :darwin} -> System.cmd(hostname_cmd, ["-f"])
        {:unix, _} -> System.cmd(hostname_cmd, ["-i"])
        {:win32, _} -> System.cmd(hostname_cmd, [])
      end

    if exit_code > 0 do
      raise RuntimeError, "Fail to execute the \"hostname\" command."
    end

    hostname =
      result
      |> String.trim()

    Logger.debug("short hostname: #{hostname}")

    hostname =
      hostname
      |> to_fully_qualified_hostname(:os.type())
      |> expand_ipv6()

    Logger.debug("fully qualified hostname: #{hostname}")
    hostname
  end

  defp to_fully_qualified_hostname(hostname, {:unix, _}), do: hostname

  defp to_fully_qualified_hostname(hostname, {:win32, _}) do
    ping_cmd = System.find_executable("ping")

    if is_nil(ping_cmd) do
      raise RuntimeError, "Fail to find the \"ping\" command."
    end

    {result, exit_code} = System.cmd(ping_cmd, ["/a", "/n", "1", hostname])

    if exit_code > 0 do
      raise RuntimeError, "Fail to execute the \"ping\" command."
    end

    result
    |> String.trim()
    |> String.split("\n")
    |> Enum.at(1)
    |> then(&Regex.named_captures(~r/Reply from (?<ip>[0-9a-f:.]+)/, &1))
    |> Map.get("ip")
  end

  defp expand_ipv6(hostname) do
    cond do
      Regex.match?(~r/^([0-9a-f]{1,4}:){7}[0-9a-f]{1,4}$/, hostname) ->
        hostname
        |> String.split(":")
        |> Enum.map(&"000#{&1}")
        |> Enum.map_join(":", &String.slice(&1, -4..-1))

      Regex.match?(~r/^([0-9a-f]{1,4}[:]{1,2})+[0-9a-f]{1,4}$/, hostname) ->
        hostname
        |> String.split("::")
        |> Enum.map(&String.split(&1, ":"))
        |> Enum.map(&{&1, Enum.count(&1)})
        |> Enum.unzip()
        |> then(fn {l, n} ->
          [
            Enum.at(l, 0),
            1..(8 - Enum.sum(n))
            |> Enum.map(fn _ -> "0" end),
            Enum.at(l, 1)
          ]
        end)
        |> List.flatten()
        |> Enum.join(":")
        |> expand_ipv6()

      true ->
        hostname
    end
  end
end
