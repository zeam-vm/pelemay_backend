defmodule NodeActivatorTest do
  use ExUnit.Case
  doctest NodeActivator

  test "greets the world" do
    assert NodeActivator.hello() == :world
  end
end
