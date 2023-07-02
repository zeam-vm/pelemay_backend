defmodule NodeActivator do
  @moduledoc File.read!("README.md")
             |> String.split("<!-- MODULEDOC -->")
             |> Enum.fetch!(1)

  require Logger

  @epmd_port 4369

  @doc """
  Activates Node.
  """
  @spec run(binary()) :: {:ok, node() | pid()} | {:error, term()}
  def run(name) do
    if Node.alive?() do
      {:ok, Node.self()}
    else
      case :os.type() do
        {:unix, _} -> launch_epmd(port: @epmd_port)
        {:win32, _} -> launch_epmd(port: @epmd_port, daemon: false)
      end

      Logger.info("wait launching epmd...")
      wait_launching_epmd(5)
      Logger.info("done.")
      name = name |> name_with_random_key() |> String.to_atom()
      Logger.info("Node.start(#{name})")
      Node.start(name)
    end
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

  @spec name_with_random_key(binary()) :: binary()
  def name_with_random_key(name) do
    "#{name}_#{:crypto.strong_rand_bytes(5) |> Base.encode32(case: :lower)}@#{hostname_f()}"
  end

  @spec hostname_f() :: binary()
  def hostname_f() do
    hostname = System.find_executable("hostname")

    if is_nil(hostname) do
      raise RuntimeError, "Fail to find the \"hostname\" command."
    else
      {result, exit_code} =
        case :os.type() do
          {:unix, _} -> System.cmd(hostname, ["-f"])
          {:win32, _} -> System.cmd(hostname, [])
        end

      if exit_code == 0 do
        String.trim(result)
      else
        raise RuntimeError, "Fail to execute the \"hostname\" command."
      end
    end
  end

  @spec launch_epmd(keyword) :: :ok
  def launch_epmd(options \\ [daemon: true]) do
    options =
      options
      |> Enum.map(fn
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

    epmd_path = System.find_executable("epmd")

    if is_nil(epmd_path) do
      Logger.error("Fail to find epmd.")
      raise RuntimeError, "Fail to find epmd."
    else
      spawn_link(fn -> launch_epmd(epmd_path, options) end)
      :ok
    end
  end

  defp launch_epmd(epmd_path, options) do
    {result, exit_code} = System.cmd(epmd_path, options, parallelism: true)

    if exit_code == 0 do
      Logger.info("epmd #{Enum.join(options, " ")}: #{result}")
    else
      # credo:disable-for-next-line
      Logger.error("epmd #{Enum.join(options, " ")}: #{result}", error_code: exit_code)
      raise RuntimeError, "Fail to launch epmd #{Enum.join(options, " ")}: #{result}"
    end
  end
end
