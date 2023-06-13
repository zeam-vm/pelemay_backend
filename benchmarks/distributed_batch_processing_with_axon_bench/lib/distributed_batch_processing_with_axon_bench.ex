defmodule DistributedBatchProcessingWithAxonBench do
  @moduledoc """
  Documentation for `DistributedBatchProcessingWithAxonBench`.
  """

  def run() do
    NodeActivator.run("primary")
  end
end
