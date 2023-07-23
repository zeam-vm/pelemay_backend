defmodule NodeActivator.Epmd do
  @moduledoc false

  require Logger

  @epmd_port 4369

  @doc """
  Launch the the `epmd` process.
  """
  @spec launch_epmd() :: :ok
  def launch_epmd() do
    options =
      case :os.type() do
        {:unix, _} -> [port: @epmd_port]
        {:win32, _} -> [port: @epmd_port]
      end

    epmd_cmd = System.find_executable("epmd")
    epmd_options = epmd_options_to_list(options)

    if is_nil(epmd_cmd) do
      Logger.error("Fail to find epmd.")
      raise RuntimeError, "Fail to find epmd."
    end

    unless epmd_running?() do
      spawn_link(fn -> do_launch_epmd(epmd_cmd, epmd_options) end)
    end

    Logger.info("wait launching epmd...")
    wait_launching_epmd(5)
    Logger.info("done.")
    :ok
  end

  # Checks if the `epmd` process is running.
  @spec epmd_running?() :: boolean()
  def epmd_running?() do
    case :gen_tcp.connect(~c'localhost', @epmd_port, [:binary, active: false], 1000) do
      {:ok, socket} ->
        Logger.info("Port epmd is active.")
        :gen_tcp.close(socket)
        true

      {:error, reason} ->
        Logger.warning("Fail to connect due to #{inspect(reason)}.")
        false
    end
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

  # Waits at most x seconds for the epmd process to be ready. Raises on timeout.
  defp wait_launching_epmd(0), do: raise(RuntimeError, "Fail to launch epmd.")

  defp wait_launching_epmd(count) do
    unless epmd_running?() do
      Process.sleep(1000)
      wait_launching_epmd(count - 1)
    end

    :ok
  end

  # Converts a keyword list to a list of strings for used of `System.cmd/3`.
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
