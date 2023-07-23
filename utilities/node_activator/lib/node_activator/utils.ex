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

    result
    |> String.trim()
    |> to_fully_qualified_hostname(:os.type())
  end

  defp to_fully_qualified_hostname(hostname, {:unix, _}), do: hostname

  defp to_fully_qualified_hostname(hostname, {:win32, _}) do
    Logger.debug("short hostname: #{hostname}")
    ping_cmd = System.find_executable("ping")

    if is_nil(ping_cmd) do
      raise RuntimeError, "Fail to find the \"ping\" command."
    end

    {result, exit_code} = System.cmd(ping_cmd, ["/a", "/n", "1", hostname])

    if exit_code > 0 do
      raise RuntimeError, "Fail to execute the \"ping\" command."
    end

    r =
      result
      |> String.trim()
      |> String.split("\n")
      |> Enum.at(1)
      |> then(&Regex.named_captures(~r/Reply from (?<ip>[0-9a-f:.]+)/, &1))
      |> Map.get("ip")

    Logger.debug("fully qualified hostname: #{r}")
    r
  end
end
