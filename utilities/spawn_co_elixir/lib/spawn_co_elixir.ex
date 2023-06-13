defmodule SpawnCoElixir do
  @moduledoc """
  Documentation for `SpawnCoElixir`.
  """

  def init() do
    :ets.new(:spawn_co_elixir_co_elixir_lookup, [:set, :public, :named_table])
  end

  def run(options \\ [name: "co_elixir"]) do
  end
end
