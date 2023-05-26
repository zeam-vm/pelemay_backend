defmodule DecoratingBackendTest do
  use ExUnit.Case
  doctest DecoratingBackend

  test "greets the world" do
    assert DecoratingBackend.hello() == :world
  end
end
