defmodule SpawnCoElixirTest do
  use ExUnit.Case
  doctest SpawnCoElixir
  require Logger

  @waiting_msec 20
  @counter_waiting 100
  @table :spawn_co_elixir_watch_dog_timer
  @key :watch_dog_timer

  setup do
    Node.stop()
    :ets.new(@table, [:set, :protected, :named_table])

    on_exit(fn ->
      Node.stop()
    end)
  end

  test "launch and exit co_elixir" do
    refute Node.alive?()
    assert Enum.empty?(SpawnCoElixir.workers())
    assert Supervisor.count_children(SpawnCoElixir.DynamicSupervisor).workers == 0

    # run
    result =
      SpawnCoElixir.run(code: "GenServer.start_link(SpawnCoElixir.WatchDogTimer, [])")

    assert {:ok, pid} = result
    assert is_pid(pid)
    assert Node.alive?()

    # wait until the node is registered
    :ets.insert(@table, {@key, false})

    r =
      Stream.unfold({false, @counter_waiting}, fn
        {true, _} -> nil
        {_, 0} -> nil
        {false, count} -> assert_worker_nodes_working(count)
      end)
      |> Enum.reduce(false, fn {r, _}, acc -> r or acc end)

    assert r

    worker_nodes = SpawnCoElixir.workers()
    Logger.debug("worker_nodes = #{inspect(worker_nodes)}")

    # verify supervision
    assert [{:undefined, _pid, :worker, [SpawnCoElixir.CoElixir]}] =
             Supervisor.which_children(SpawnCoElixir.DynamicSupervisor)

    # exit

    Logger.debug("Exit all nodes #{inspect(worker_nodes)}")

    r =
      worker_nodes
      |> Enum.map(fn worker_node ->
        case SpawnCoElixir.stop(worker_node) do
          :ok ->
            true

          r ->
            Logger.error("SpawnCoElixir.stop(#{inspect(worker_node)}) return #{inspect(r)}")
            false
        end
      end)
      |> Enum.reduce(true, fn x, acc -> x and acc end)

    assert r

    :ets.insert(@table, {@key, false})

    r =
      Stream.unfold({false, @counter_waiting}, fn
        {true, _} -> nil
        {_, 0} -> nil
        {false, count} -> wait_worker_nodes(count)
      end)
      |> Enum.reduce(false, fn {r, _}, acc -> r or acc end)

    refute r

    r =
      worker_nodes
      |> Enum.map(fn worker_node ->
        case Node.ping(worker_node) do
          :pang ->
            true

          p ->
            Logger.error("Node.ping(#{worker_node}) returns #{p}")
            false
        end
      end)
      |> Enum.reduce(true, fn x, acc -> x and acc end)

    assert r

    assert Enum.empty?(SpawnCoElixir.workers())

    r =
      worker_nodes
      |> Enum.map(fn worker_node ->
        case SpawnCoElixir.stop(worker_node) do
          {:error, :not_found} ->
            true

          r ->
            Logger.error("SpawnCoElixir.stop(#{worker_node}) return #{inspect(r)}")
            false
        end
      end)
      |> Enum.reduce(true, fn x, acc -> x and acc end)

    assert r
  end

  defp assert_worker_nodes_working(count) do
    Process.sleep(@waiting_msec)

    case SpawnCoElixir.workers() do
      [] ->
        Logger.debug("Workers are []")
        {{false, count - 1}, {false, count - 1}}

      worker_nodes ->
        worker_nodes
        |> Enum.map(&Node.ping(&1))
        |> Enum.reduce(false, fn
          :pong, _ -> true
          _, acc -> acc
        end)
        |> case do
          false ->
            Logger.debug("All workers #{inspect(worker_nodes)} are not responded.")
            {{false, count - 1}, {false, count - 1}}

          true ->
            Logger.debug("A worker in #{inspect(worker_nodes)} is responded")
            {{true, 0}, {true, 0}}
        end
    end
  end

  defp wait_worker_nodes(count) do
    Process.sleep(@waiting_msec)

    case SpawnCoElixir.workers() do
      [] -> {{false, count - 1}, {false, count - 1}}
      _ -> lookup_watch_dog_timer(count)
    end
  end

  defp lookup_watch_dog_timer(count) do
    :ets.lookup(@table, @key)
    |> lookup_watch_dog_timer_s(count)
  end

  defp lookup_watch_dog_timer_s(nil, _) do
    Logger.error("ETS table #{@key} not found")
    nil
  end

  defp lookup_watch_dog_timer_s([], _) do
    Logger.error("ETS table #{@key} does not contain any term")
    nil
  end

  defp lookup_watch_dog_timer_s([{@key, result}], count) do
    Logger.debug("#{@key} is #{result}")
    {{result, count - 1}, {result, count - 1}}
  end
end
