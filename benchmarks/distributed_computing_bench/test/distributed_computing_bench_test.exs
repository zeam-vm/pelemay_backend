defmodule DistributedComputingBenchTest do
  use ExUnit.Case
  doctest DistributedComputingBench

  test "greets the world" do
    assert DistributedComputingBench.hello() == :world
  end
end
