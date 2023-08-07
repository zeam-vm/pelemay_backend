defmodule DistributedComputingBench.BumblebeeBench do
  @moduledoc """
  Documentation for `DistributedComputingBench.BumblebeeBench`.
  """

  require Logger

  def run() do
    System.cmd(
      "mix",
      [
        "run",
        "-r",
        "lib/distributed_computing_bench/bumblebee_bench/runner.exs",
        "-e",
        "DistributedComputingBench.BumblebeeBench.Runner.run"
      ],
      env: [
        {"MIX_ENV", "bumblebee_bench"}
      ]
    )
  end
end
