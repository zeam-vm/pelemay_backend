defmodule SpawnCoElixir.CoElixirLookupTest do
  use ExUnit.Case
  doctest SpawnCoElixir.CoElixirLookup
  alias SpawnCoElixir.CoElixirLookup

  test "table is already started by application" do
    assert CoElixirLookup.table_exist?()

    assert {:error, {:already_exists, :spawn_co_elixir_co_elixir_lookup}} =
             CoElixirLookup.create_table()
  end

  test "basic read write operations" do
    assert CoElixirLookup.list_worker_nodes() == []

    foo_pid = make_pid()
    :ok = CoElixirLookup.put_entry(:foo_node, foo_pid)
    assert CoElixirLookup.list_worker_nodes() == [:foo_node]

    bar_pid = make_pid()
    :ok = CoElixirLookup.put_entry(:bar_node, bar_pid)
    assert CoElixirLookup.list_worker_nodes() == [:foo_node, :bar_node]

    assert CoElixirLookup.get_worker_pid(:foo_node) == foo_pid
    assert CoElixirLookup.get_worker_pid(:bar_node) == bar_pid

    :ok = CoElixirLookup.delete_entry(:foo_node)
    assert CoElixirLookup.list_worker_nodes() == [:bar_node]
  end

  # makes a fake worker pid
  defp make_pid do
    {:ok, pid} = Task.start_link(fn -> :ok end)
    pid
  end
end
