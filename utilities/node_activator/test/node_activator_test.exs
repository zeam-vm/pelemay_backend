defmodule NodeActivatorTest do
  use ExUnit.Case
  doctest NodeActivator

  test "run" do
    NodeActivator.run("test")
    Process.sleep(500)
    assert Node.alive?() == true
  end

  test "launch_epmd" do
    assert NodeActivator.launch_epmd() == :ok
  end
end
