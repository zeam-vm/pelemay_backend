defmodule NodeActivator do
  @moduledoc """
  Documentation for `NodeActivator`.
  """
  require Logger

  @doc """
  Activates Node.
  """
  @spec run(binary()) :: {:ok, node() | pid()} | {:error, term()}
  def run(name) do
    if Node.alive?() do
      {:ok, Node.self()}
    else
      launch_epmd()
      name = :"#{name}_#{:crypto.strong_rand_bytes(5) |> Base.encode32(case: :lower)}"
      Node.start(name)
    end
  end

  @spec launch_epmd(keyword) :: :ok | :error
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
      :error
    else
      {result, exit_code} = System.cmd(epmd_path, options)

      if exit_code == 0 do
        Logger.info("epmd #{Enum.join(options, " ")}: #{result}")
        :ok
      else
        # credo:disable-for-next-line
        Logger.error("epmd #{Enum.join(options, " ")}: #{result}", error_code: exit_code)
        :error
      end
    end
  end
end