defmodule DistributedComputingBench.BumblebeeBench do
  @moduledoc """
  Documentation for `DistributedComputingBench.BumblebeeBench`.
  """

  require Logger

  def run() do
    SpawnCoElixir.run(
      code: File.read!("lib/distributed_computing_bench/bumblebee_bench/runner.exs"),
      deps: [:spawn_co_elixir, :http_downloader, :nx, :exla, :bumble_bee, :benchee],
      host_name: "bumblebee_bench",
      co_elixir_name: "bumblebee_bench_worker"
    )
  end
end
