defmodule NodeActivator do
  @moduledoc File.read!("README.md")
             |> String.split("<!-- MODULEDOC -->")
             |> Enum.fetch!(1)

  require Logger

  @epmd_port 4369

  @doc """
  Activates Node.
  """
  @spec run(binary()) :: {:ok, node()}
  def run(node_name_prefix) do
    unless Node.alive?() do
      empd_options =
        case :os.type() do
          {:unix, _} -> [port: @epmd_port]
          {:win32, _} -> [port: @epmd_port, daemon: false]
        end

      launch_epmd(empd_options)
      Logger.info("wait launching epmd...")
      wait_launching_epmd(5)
      Logger.info("done.")
      name = generate_node_name(node_name_prefix)
      Logger.info("Node.start(#{name})")
      {:ok, _} = Node.start(name)
    end

    {:ok, Node.self()}
  end

  defp wait_launching_epmd(0), do: raise(RuntimeError, "Fail to launch epmd.")

  defp wait_launching_epmd(count) do
    unless can_connect?(@epmd_port) do
      Process.sleep(1000)
      wait_launching_epmd(count - 1)
    end

    :ok
  end

  defp can_connect?(port) do
    case :gen_tcp.connect(~c'localhost', port, [:binary, active: false], 1000) do
      {:ok, socket} ->
        Logger.info("active.")
        :gen_tcp.close(socket)
        true

      {:error, reason} ->
        Logger.debug("Fail to connect due to #{inspect(reason)}.")
        false
    end
  end

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
      end

    if exit_code > 0 do
      raise RuntimeError, "Fail to execute the \"hostname\" command."
    end

    String.trim(result)
  end

  @spec launch_epmd(keyword) :: :ok
  def launch_epmd(options \\ [daemon: true]) do
    epmd_cmd = System.find_executable("epmd")
    epmd_options = epmd_options_to_list(options)

    if is_nil(epmd_cmd) do
      Logger.error("Fail to find epmd.")
      raise RuntimeError, "Fail to find epmd."
    end

    spawn_link(fn -> do_launch_epmd(epmd_cmd, epmd_options) end)

    :ok
  end

  defp do_launch_epmd(epmd_cmd, epmd_options) do
    {result, exit_code} = System.cmd(epmd_cmd, epmd_options, parallelism: true)

    options_and_result = "#{Enum.join(epmd_options, " ")}: #{result}"

    if exit_code == 0 do
      Logger.info("epmd " <> options_and_result)
    else
      Logger.error("epmd " <> options_and_result <> ": error_code: #{exit_code}")
      raise RuntimeError, "Fail to launch epmd " <> options_and_result
    end
  end

  @spec epmd_options_to_list(keyword) :: [binary()]
  defp epmd_options_to_list(options) do
    Enum.map(options, fn
      {:adress, list} when is_binary(list) -> ["-address", list]
      {:port, no} when is_integer(no) -> ["-port", "#{no}"]
      {:debug, true} -> ["-debug"]
      {:debug, false} -> []
      {:daemon, true} -> ["-daemon"]
      {:daemon, false} -> []
      {:relaxed_command_check, true} -> ["-relaxed_command"]
      {:relaxed_command_check, false} -> []
      {:packet_timeout, seconds} when is_integer(seconds) -> ["-packet_timeout", "#{seconds}"]
      {:delay_accept, seconds} when is_integer(seconds) -> ["-delay_accept", "#{seconds}"]
      {:delay_write, seconds} when is_integer(seconds) -> ["-delay_write", "#{seconds}"]
      {:kill, true} -> ["-kill"]
      {:kill, false} -> []
      {:names, true} -> ["-names"]
      {:names, false} -> []
      {:stop, name} when is_binary(name) -> ["-stop", name]
    end)
    |> List.flatten()
  end
end
