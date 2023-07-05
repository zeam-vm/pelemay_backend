defmodule NodeActivator.Utils do
  @moduledoc false

  @spec generate_node_name(binary()) :: atom()
  def generate_node_name(prefix) do
    prefix = Regex.replace(~r/\s/, prefix, "-")
    random_string = :crypto.strong_rand_bytes(5) |> Base.encode32(case: :lower)

    :"#{prefix}_#{random_string}@#{get_hostname()}"
  end

  @spec get_hostname() :: binary()
  def get_hostname() do
    hostname_cmd = System.find_executable("hostname")

    if is_nil(hostname_cmd) do
      raise RuntimeError, "Fail to find the \"hostname\" command."
    end

    {result, exit_code} =
      case :os.type() do
        {:unix, _} -> System.cmd(hostname_cmd, ["-f"])
        {:win32, _} -> System.cmd(hostname_cmd, [])
        _ -> raise RuntimeError, "Unknown os type."
      end

    if exit_code > 0 do
      raise RuntimeError, "Fail to execute the \"hostname\" command."
    end

    String.trim(result)
  end
end
