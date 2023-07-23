defmodule SpawnCoElixir.CoElixir do
  @moduledoc false

  use GenServer
  require Logger
  alias SpawnCoElixir.CoElixirLookup
  alias SpawnCoElixir.CoElixirWorkerSpawner

  ## Client API

  @spec start_link([SpawnCoElixir.co_elixir_option()]) :: GenServer.on_start()
  def start_link(options \\ []) do
    server_options = [
      co_elixir_name: Keyword.fetch!(options, :co_elixir_name),
      code: Keyword.fetch!(options, :code),
      deps: Keyword.fetch!(options, :deps),
      host_name: Keyword.fetch!(options, :host_name)
    ]

    NodeActivator.run(server_options[:host_name])
    GenServer.start_link(__MODULE__, server_options)
  end

  @spec stop(node) :: :ok | {:error, any}
  def stop(worker_node) do
    case CoElixirLookup.get_worker_pid(worker_node) do
      nil ->
        Logger.warning("Not found worker_node #{worker_node}.")
        {:error, :not_found}

      pid when is_pid(pid) ->
        Logger.info("Found worker_node {#{worker_node}, #{inspect(pid)}}")
        GenServer.call(pid, :exit_co_elixir)
    end
  end

  ## GenServer callbacks

  @impl true
  def init(options \\ []) do
    a_process = %{options: options, running: false, worker_node: nil}

    {:ok, a_process, {:continue, :spawn_co_elixir}}
  end

  @impl true
  def handle_continue(:spawn_co_elixir, a_process) do
    options = a_process.options
    node_name_prefix = Keyword.fetch!(options, :co_elixir_name)
    worker_node = NodeActivator.Utils.generate_node_name(node_name_prefix)
    this_pid = self()
    this_node = Node.self()

    if a_process.running do
      {:noreply, a_process}
    else
      Logger.info("spawning #{inspect(worker_node)}")

      spawn_link(fn ->
        :ok = GenServer.call(this_pid, {:register_worker_node, worker_node})

        CoElixirWorkerSpawner.run(this_node, worker_node, options)
        |> handle_continue_spawn_co_elixir_s(this_pid, worker_node)
      end)

      {:noreply, %{a_process | running: true}}
    end
  end

  defp handle_continue_spawn_co_elixir_s(:ok, _this_pid, worker_node) do
    Logger.info("spawned #{inspect(worker_node)}")
  end

  defp handle_continue_spawn_co_elixir_s({:error, exit_code}, this_pid, worker_node) do
    Logger.info("could not spawn #{inspect(worker_node)}: exit code #{exit_code}")
    :ok = GenServer.call(this_pid, :reboot_co_elixir)
  end

  defp handle_continue_spawn_co_elixir_s(:error, this_pid, worker_node) do
    Logger.info("could not spawn #{inspect(worker_node)}: without exit code")
    :ok = GenServer.call(this_pid, :reboot_co_elixir)
  end

  @impl true
  def handle_call({:register_worker_node, worker_node}, {_pid_from, _}, a_process) do
    Logger.info("registering #{inspect(worker_node)}")
    :ok = CoElixirLookup.put_entry(worker_node, self())

    {:reply, :ok, %{a_process | worker_node: worker_node}}
  end

  @impl true
  def handle_call({:deregister_worker_node, worker_node}, {_pid_from, _}, a_process) do
    Logger.info("deregistering #{inspect(worker_node)}")
    :ok = CoElixirLookup.delete_entry(worker_node)

    {:reply, :ok, %{a_process | worker_node: nil}}
  end

  @impl true
  def handle_call(:exit_co_elixir, {_pid_from, _}, a_process) do
    Logger.info("exiting #{inspect(a_process.worker_node)}")
    :ok = do_exit_co_elixir(a_process.worker_node)

    {:reply, :ok, %{a_process | running: false, worker_node: nil}}
  end

  @impl true
  def handle_call(:reboot_co_elixir, {_pid_from, _}, a_process) do
    Logger.info("rebooting #{inspect(a_process.worker_node)}")
    :ok = do_exit_co_elixir(a_process.worker_node)

    {:noreply, %{a_process | running: false, worker_node: nil}, {:continue, :spawn_co_elixir}}
  end

  defp do_exit_co_elixir(worker_node) do
    if worker_node do
      _pid = Node.spawn(worker_node, System, :halt, [])
      :ok = CoElixirLookup.delete_entry(worker_node)
    end

    :ok
  end
end
