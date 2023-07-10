defmodule SpawnCoElixir do
  @moduledoc """
  Documentation for `SpawnCoElixir`.
  """

  def init() do
    :ets.new(:spawn_co_elixir_co_elixir_lookup, [:set, :public, :named_table])
  end

  def run(options \\ [code: "", host_name: "host", co_elixir_name: "co_elixir"]) do
    {:ok, _pid} = DynamicSupervisor.start_child(SpawnCoElixir.DynamicSupervisor, {SpawnCoElixir.CoElixir, %{options: options}})
  end

  defdelegate stop(worker_node), to: SpawnCoElixir.CoElixir

  defdelegate workers, to: SpawnCoElixir.CoElixir
end
