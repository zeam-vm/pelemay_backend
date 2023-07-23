defmodule SpawnCoElixir.WatchDogTimer do
  @moduledoc false
  use GenServer

  @waiting_msec 10
  @table :spawn_co_elixir_watch_dog_timer
  @key :watch_dog_timer

  @impl true
  def init(state) do
    :ets.new(@table, [:set, :protected, :named_table])
    {:ok, state, {:continue, :clock}}
  end

  @impl true
  def handle_continue(:clock, state) do
    spawn_link(fn -> coroutine() end)
    {:noreply, state}
  end

  defp coroutine() do
    Node.list()
    |> Enum.each(fn node ->
      Node.spawn(node, fn ->
        :ets.insert(@table, {@key, true})
      end)
    end)

    Process.sleep(@waiting_msec)
    GenServer.cast(SpawnCoElixir.Test.WatchDogTimer, :next_clock)
  end

  @impl true
  def handle_cast(:next_clock, state) do
    {:noreply, state, {:continue, :clock}}
  end
end
