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

      Process.sleep(200)
      workers = SpawnCoElixir.workers()
      Process.sleep(200)
      assert workers == SpawnCoElixir.workers()

      SpawnCoElixir.workers()
      |> Enum.map(fn n ->
        assert Node.ping(n) == :pong
        n
      end)
      |> Enum.each(fn n -> SpawnCoElixir.stop(n) end)

      Process.sleep(200)
    end

    test "launch and exit multiple co_elixirs" do
      refute Node.alive?()

      {result, pid} = SpawnCoElixir.run()
      assert result == :ok
      assert is_pid(pid)

      {result, pid} = SpawnCoElixir.run()
      assert result == :ok
      assert is_pid(pid)

      {result, pid} = SpawnCoElixir.run()
      assert result == :ok
      assert is_pid(pid)

      Process.sleep(600)
      workers = SpawnCoElixir.workers()
      Process.sleep(200)
      assert workers == SpawnCoElixir.workers()

      SpawnCoElixir.workers()
      |> Enum.map(fn n ->
        assert Node.ping(n) == :pong
        n
      end)
      |> Enum.each(fn n -> SpawnCoElixir.stop(n) end)

      Process.sleep(200)
    end
  end

  describe "abort and rerun" do
    test "launch and exit co_elixir" do
      refute Node.alive?()

      {result, pid} =
        SpawnCoElixir.run(
          code: "Process.sleep(100); System.halt(1)",
          host_name: "host",
          co_elixir_name: "co_elixir"
        )

      assert result == :ok
      assert is_pid(pid)

      Process.sleep(200)
      workers = SpawnCoElixir.workers()
      Process.sleep(400)
      refute workers == SpawnCoElixir.workers()

      SpawnCoElixir.workers()
      |> Enum.each(fn n -> SpawnCoElixir.stop(n) end)

      Process.sleep(200)
    end
  end
end
