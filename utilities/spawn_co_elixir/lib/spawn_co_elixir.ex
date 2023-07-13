defmodule SpawnCoElixir do
  @moduledoc File.read!("README.md")
             |> String.split("<!-- MODULEDOC -->")
             |> Enum.fetch!(1)

  @typedoc """
  The CoElixir server option

  * `:code` - Elixir code to be run by SpawnCoElixir.CoElixir.Worker
  * `:host_name` - the node name prefix for host
  * `:co_elixir_name` - the node name prefix for CoElixir
  """
  @type co_elixir_option ::
          {:code, binary}
          | {:host_name, binary}
          | {:co_elixir_name, binary}

  @doc """
  Starts a CoElixir server as a child of `SpawnCoElixir.DynamicSupervisor`.
  """
  @spec run([co_elixir_option]) :: {:ok, pid}
  def run(options \\ []) do
    co_elixir_options = [
      code: options[:code] || "",
      host_name: options[:host_name] || "host",
      co_elixir_name: options[:co_elixir_name] || "co_elixir"
    ]

    {:ok, _pid} =
      DynamicSupervisor.start_child(
        SpawnCoElixir.DynamicSupervisor,
        {SpawnCoElixir.CoElixir, %{options: co_elixir_options}}
      )
  end

  @doc """
  Stops a CoElixir server.
  """
  defdelegate stop(worker_node), to: SpawnCoElixir.CoElixir

  @doc """
  Lists all running CoElixir servers.
  """
  defdelegate workers, to: SpawnCoElixir.CoElixir
end
