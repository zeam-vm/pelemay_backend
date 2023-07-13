defmodule SpawnCoElixir.CoElixirLookup do
  @moduledoc false

  @table_name :spawn_co_elixir_co_elixir_lookup

  ## table operations

  @spec create_table() :: :ok
  def create_table() do
    _ref = :ets.new(@table_name, [:set, :public, :named_table])
    :ok
  rescue
    _ -> {:error, {:already_exists, @table_name}}
  end

  @spec delete_table() :: :ok
  def delete_table() do
    if table_exist?(), do: :ets.delete(@table_name)
    :ok
  end

  @spec table_exist?() :: boolean()
  def table_exist?() do
    :ets.whereis(@table_name) != :undefined
  end

  ## read operations

  @spec list_worker_nodes() :: [node]
  def list_worker_nodes() do
    :ets.select(@table_name, [{{:"$1", :"$2"}, [], [:"$1"]}])
  end

  @spec get_worker_pid(node) :: pid | nil
  def get_worker_pid(worker_node) when is_atom(worker_node) do
    case :ets.lookup(@table_name, worker_node) do
      [] -> nil
      [{^worker_node, pid}] -> pid
    end
  end

  ## write operations

  @spec put_entry(node, pid) :: :ok
  def put_entry(worker_node, pid) when is_atom(worker_node) and is_pid(pid) do
    true = :ets.insert(@table_name, {worker_node, pid})
    :ok
  end

  @spec delete_entry(node) :: :ok
  def delete_entry(worker_node) when is_atom(worker_node) do
    true = :ets.delete(@table_name, worker_node)
    :ok
  end
end
