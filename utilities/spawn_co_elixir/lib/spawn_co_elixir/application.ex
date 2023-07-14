defmodule SpawnCoElixir.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  alias SpawnCoElixir.CoElixirLookup

  @impl true
  def start(_type, _args) do
    CoElixirLookup.create_table()

    children = [
      # Starts a worker by calling: SpawnCoElixir.Worker.start_link(arg)
      # {SpawnCoElixir.Worker, arg}
      {DynamicSupervisor, strategy: :one_for_one, name: SpawnCoElixir.DynamicSupervisor}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: SpawnCoElixir.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
