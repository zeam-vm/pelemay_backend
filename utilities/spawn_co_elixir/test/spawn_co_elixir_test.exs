defmodule SpawnCoElixirTest do
  use ExUnit.Case
  doctest SpawnCoElixir

  setup do
    on_exit(fn -> Node.stop() end)
  end

  describe "run and exit" do
    test "launch and exit co_elixir" do
      refute Node.alive?()

      {result, pid} = SpawnCoElixir.run()
      assert result == :ok
      assert is_pid(pid)

      SpawnCoElixir.workers()
      |> Enum.map(fn n ->
        assert Node.ping(n) == :pong
        n
      end)
      |> Enum.each(fn n -> SpawnCoElixir.stop(n) end)
    end
  end
end
