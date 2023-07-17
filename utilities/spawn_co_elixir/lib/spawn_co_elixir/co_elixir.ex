defmodule SpawnCoElixir.CoElixir do
  @moduledoc false

  use GenServer
  require Logger
  alias SpawnCoElixir.CoElixirLookup

  ## Client API

  @spec start_link([SpawnCoElixir.co_elixir_option()]) :: {:ok, pid}
  def start_link(options \\ []) do
    server_options = [
      co_elixir_name: Keyword.fetch!(options, :co_elixir_name),
      code: Keyword.fetch!(options, :code),
      deps: Keyword.fetch!(options, :deps),
      host_name: Keyword.fetch!(options, :host_name)
    ]

    NodeActivator.run(server_options[:host_name])
    {:ok, pid} = GenServer.start_link(__MODULE__, server_options)
    GenServer.cast(pid, :spawn_co_elixir)
    {:ok, pid}
  end

  @spec stop(node) :: :ok
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

  ## GenServer callbacks

  @impl true
  def init(options \\ []) do
    a_process = %{options: options, running: false, worker_node: nil}

    {:ok, a_process}
  end

  @impl true
  def handle_cast(:spawn_co_elixir, a_process) do
    unless a_process.running do
      ret = self()
      spawn_link(fn -> handle_cast_s(spawn_co_elixir(ret, a_process.options), ret) end)
    end

    {:noreply, %{a_process | running: true}}
  end

  @impl true
  def handle_cast({:worker_node, worker_node}, a_process) do
    Logger.info("Register worker #{inspect(worker_node)}")

    {:noreply, %{a_process | worker_node: worker_node}}
  end

  @impl true
  def handle_cast(:exit, a_process) do
    case Map.get(a_process, :worker_node) do
      nil ->
        Logger.error("Not found worker_node")

        {:noreply, %{a_process | running: false}}

      :stopped ->
        {:noreply, %{a_process | running: false, worker_node: nil}}

      worker_node ->
        Logger.info("Exit #{inspect(worker_node)}")
        Node.spawn(worker_node, System, :halt, [])
        CoElixirLookup.delete_entry(worker_node)

        {:noreply, %{a_process | running: false, worker_node: :stopped}}
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

  defp spawn_co_elixir(pid, options) do
    code = Keyword.fetch!(options, :code)
    deps = Keyword.fetch!(options, :deps)
    node_name_prefix = Keyword.fetch!(options, :co_elixir_name)

    worker_node = NodeActivator.Utils.generate_node_name(node_name_prefix)
    :ok = CoElixirLookup.put_entry(worker_node, pid)

    try do
      GenServer.cast(pid, {:worker_node, worker_node})

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
