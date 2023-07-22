defmodule SpawnCoElixirTest do
  use ExUnit.Case
  doctest SpawnCoElixir

  setup do
    on_exit(fn -> Node.stop() end)
  end

  test "launch and exit co_elixir" do
    refute Node.alive?()
    assert Enum.empty?(SpawnCoElixir.workers())
    assert Supervisor.count_children(SpawnCoElixir.DynamicSupervisor).workers == 0

    # run
    assert {:ok, pid} = SpawnCoElixir.run()
    assert is_pid(pid)
    assert Node.alive?()

    # wait until the node is registered
    Process.sleep(500)
    assert [worker_node] = SpawnCoElixir.workers()
    assert :pong = Node.ping(worker_node)

    # verify supervision
    assert [{:undefined, _pid, :worker, [SpawnCoElixir.CoElixir]}] =
             Supervisor.which_children(SpawnCoElixir.DynamicSupervisor)

    # exit
    assert :ok = SpawnCoElixir.stop(worker_node)
    Process.sleep(500)
    assert :pang = Node.ping(worker_node)
    assert Enum.empty?(SpawnCoElixir.workers())
    assert {:error, :not_found} = SpawnCoElixir.stop(worker_node)
  end
end
