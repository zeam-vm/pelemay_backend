defmodule SpawnCoElixir do
  @moduledoc File.read!("README.md")
             |> String.split("<!-- MODULEDOC -->")
             |> Enum.fetch!(1)

  @typedoc """
  The CoElixir server option

  * `:code` - Elixir code to be run by `CoElixir.Worker`
  * `:deps` - dependencies for `CoElixir.Worker`
  * `:host_name` - the node name prefix for host
  * `:co_elixir_name` - the node name prefix for `CoElixir`
  """
  @type co_elixir_option ::
          {:code, binary}
          | {:deps, [atom | tuple]}
          | {:host_name, binary}
          | {:co_elixir_name, binary}

  @doc """
  Starts a supervised CoElixir worker.
  """
  @spec run([co_elixir_option]) :: DynamicSupervisor.on_start_child()
  def run(options \\ []) do
    co_elixir_options = [
      code: options[:code] || "",
      deps: options[:deps] || [],
      host_name: options[:host_name] || "host",
      co_elixir_name: options[:co_elixir_name] || "co_elixir"
    ]

    DynamicSupervisor.start_child(
      SpawnCoElixir.DynamicSupervisor,
      {SpawnCoElixir.CoElixir, co_elixir_options}
    )
  end

  @doc """
  Stops a CoElixir worker by node name.
  """
  @spec stop(node) :: :ok
  defdelegate stop(worker_node), to: SpawnCoElixir.CoElixir

  @doc """
  Lists all running CoElixir worker nodes.
  """
  @spec workers :: [node]
  defdelegate workers, to: SpawnCoElixir.CoElixirLookup, as: :list_worker_nodes
end
