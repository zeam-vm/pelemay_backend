defmodule SpawnCoElixir.CoElixir.Worker do
  @moduledoc false
  @waiting_msec 20
  @counter_waiting 100

  require Logger

  def run(node_from) do
    Stream.unfold({false, @counter_waiting}, fn
      {_, 0} -> nil
      {true, _} -> nil
      {:ignored, count} -> next_count(count, node_from)
      {false, count} -> next_count(count, node_from)
    end)
    |> Enum.reduce(false, fn
      {true, _}, _ -> true
      {false, _}, acc -> acc
    end)
    |> case do
      true -> find(node_from)
      false -> raise RuntimeError, "could not connect to #{node_from}"
    end
  end

  defp next_count(count, node_from) do
    Process.sleep(@waiting_msec)
    n = Node.connect(node_from)
    Logger.debug("NodeActivator.epmd_running?: #{NodeActivator.epmd_running?()}")
    Logger.debug("Node.connect(#{inspect(node_from)}): #{inspect(n)}")

    {
      {n, count - 1},
      {n, count - 1}
    }
  end

  defp find(node_from) do
    Logger.debug("CoElixir #{Node.self()} and host #{node_from} are connected.")

    receive do
      :end -> :ok
    end
  end
end
