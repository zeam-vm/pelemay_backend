defmodule NodeActivatorTest.EpmdTest do
  use ExUnit.Case
  doctest NodeActivator.Epmd
  alias NodeActivator.Epmd

  describe "launch_epmd" do
    test "does not raise error" do
      assert :ok = Epmd.launch_epmd()
    end
  end
end
