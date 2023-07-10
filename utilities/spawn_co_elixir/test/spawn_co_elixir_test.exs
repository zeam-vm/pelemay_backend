defmodule SpawnCoElixirTest do
  use ExUnit.Case
  doctest SpawnCoElixir

  setup do
    on_exit(fn -> Node.stop() end)
  end
end
