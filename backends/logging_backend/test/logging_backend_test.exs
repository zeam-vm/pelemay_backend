defmodule LoggingBackendTest do
  use ExUnit.Case
  doctest LoggingBackend

  test "greets the world" do
    assert LoggingBackend.hello() == :world
  end
end
