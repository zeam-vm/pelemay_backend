defmodule BackendDecoratorTest do
  use ExUnit.Case
  doctest BackendDecorator

  test "greets the world" do
    assert BackendDecorator.hello() == :world
  end
end
