defmodule SpawnCoElixir.CoElixir do
  @moduledoc false

  use GenServer
  require Logger
  alias SpawnCoElixir.CoElixirLookup

  @impl true
  def init(a_process \\ %{options: [code: "", host_name: "host", co_elixir_name: "co_elixir"]}) do
    {:ok, a_process}
  end

  @impl true
  def handle_cast(:spawn_co_elixir, a_process) do
    unless a_process[:running] do
      ret = self()
      spawn_link(fn -> handle_cast_s(spawn_co_elixir(ret, a_process), ret) end)
    end

    {
      :noreply,
      Map.put(a_process, :running, true)
    }
  end

  @impl true
  def handle_cast({:worker_node, worker_node}, a_process) do
    Logger.info("Register worker #{inspect(worker_node)}")

    {
      :noreply,
      Map.put(a_process, :worker_node, worker_node)
    }
  end

  @impl true
  def handle_cast(:exit, a_process) do
    case Map.get(a_process, :worker_node) do
      nil ->
        Logger.error("Not found worker_node")

        {
          :noreply,
          a_process
          |> Map.put(:running, false)
        }

      :stopped ->
        {
          :noreply,
          a_process
          |> Map.put(:running, false)
          |> Map.put(:worker_node, nil)
        }

      worker_node ->
        Logger.info("Exit #{inspect(worker_node)}")
        Node.spawn(worker_node, System, :halt, [])
        CoElixirLookup.delete_entry(worker_node)

        {
          :noreply,
          a_process
          |> Map.put(:running, false)
          |> Map.put(:worker_node, :stopped)
        }
    end
  end

  def start_link(
        a_process \\ %{options: [code: "", host_name: "host", co_elixir_name: "co_elixir"]}
      ) do
    case Map.get(a_process, :options) do
      nil ->
        {:error, "No match options: #{inspect(a_process)}."}

      options ->
        case options[:host_name] do
          nil ->
            {:error, "No match host_name: #{inspect(a_process)}."}

          host_prefix ->
            NodeActivator.run(host_prefix)
            {:ok, pid} = GenServer.start_link(__MODULE__, a_process)
            GenServer.cast(pid, :spawn_co_elixir)
            {:ok, pid}
        end
    end
  end

  def workers() do
    CoElixirLookup.list_worker_nodes()
  end

  def stop(worker_node) do
    case CoElixirLookup.get_worker_pid(worker_node) do
      nil ->
        Logger.warning("Not found worker_node #{worker_node}.")
        :ok

      pid when is_pid(pid) ->
        Logger.info("Found worker_node {#{worker_node}, #{inspect(pid)}}")
        GenServer.cast(pid, :exit)
    end
  end

  defp handle_cast_s({:ok, 0}, ret) do
    Logger.info("Exit CoElixir.")
    GenServer.cast(ret, :exit)
  end

  defp handle_cast_s({:ok, _exit_code}, ret) do
    Logger.info("Reboot CoElixir.")
    GenServer.cast(ret, :exit)
    GenServer.cast(ret, :spawn_co_elixir)
  end

  defp spawn_co_elixir(pid, a_process) do
    options = a_process[:options]
    code = options[:code]
    worker_node = NodeActivator.Utils.generate_node_name(options[:co_elixir_name])

    :ok = CoElixirLookup.put_entry(worker_node, pid)

    try do
      GenServer.cast(pid, {:worker_node, worker_node})

      deps =
        case options[:deps] do
          nil -> []
          deps -> deps
        end

      program = """
      defmodule SpawnCoElixir.CoElixir.Worker do
        def run() do
          Mix.install(#{inspect(deps)})

          #{code}

          receive do
            :end -> :ok
          end
        end
      end

      case Node.connect(:"#{node()}") do
        true ->
          SpawnCoElixir.CoElixir.Worker.run()
          :ok

        _ -> raise RuntimeError, "Node #{node()} cannot connect."
      end
      """

      Logger.info("spawn #{inspect(worker_node)}...")

      {_result, exit_code} =
        System.cmd(
          "elixir",
          [
            "--name",
            Atom.to_string(worker_node),
            "-e",
            program
          ],
          into: IO.stream()
        )

      Logger.info("exit #{inspect(worker_node)} with exit_code #{exit_code}")

      {:ok, exit_code}
    after
      :ok = CoElixirLookup.delete_entry(worker_node)
    end
  end
end
