defmodule SpawnCoElixir.CoElixir do
  @moduledoc """
  SpawnCoElixir.CoElixir
  """

  use GenServer

  @impl true
  def init(a_process \\ %{options: [code: "", host_name: "host", co_elixir_name: "co_elixir"]}) do
    {:ok, a_process}
  end

  @impl true
  def handle_cast(:spawn_co_elixir, a_process) do
    unless a_process[:running] do
      spawn_link(fn ->
        spawn_co_elixir(a_process)
        GenServer.cast(:exit, a_process)
      end)
    end

    {
      :noreply,
      Map.put(a_process, :running, true)
    }
  end

  @impl true
  def handle_cast(:exit, a_process) do
    {
      :noreply,
      Map.put(a_process, :running, false)
    }
  end

  def start_link(
        a_process \\ %{options: [code: "", host_name: "host", co_elixir_name: "co_elixir"]}
      ) do
    {:ok, pid} = GenServer.start_link(__MODULE__, a_process)
    GenServer.cast(pid, :spawn_co_elixir)
  end

  def workers() do
    :ets.tab2list(:spawn_co_elixir_co_elixir_lookup)
    |> Enum.map(fn {node, _pid} -> node end)
  end

  defp spawn_co_elixir(a_process) do
    options = a_process[:options]
    NodeActivator.run(options[:host_name])
    code = options[:code]
    worker_node = NodeActivator.Utils.generate_node_name(options[:co_elixir_name])

    :ets.insert(:spawn_co_elixir_co_elixir_lookup, {worker_node, self()})

    try do
      deps =
        case options[:deps] do
          nil -> []
          deps -> deps
        end

      program =
        """
        #{code}

        defmodule SpawnCoElixir.CoElixir.Worker do
          def run() do
            Mix.install(#{inspect(deps)})

            receive do
              :end -> :ok
            end
          end
        end

        case Node.connect(:"#{node()}") do
          true ->
            SpawnCoElixir.CoElixir.Worker.run()
            :ok

          _ -> raise RuntimeError, "Node cannot connect."
        end
        """

      {_result, _exit_code} =
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

      :ok
    after
      :ets.delete(:spawn_co_elixir_co_elixir_lookup, worker_node)
    end
  end
end
