defmodule PelemayBackendTest do
  use ExUnit.Case
  doctest PelemayBackend

  test "greets the world" do
    assert PelemayBackend.hello() == :world
  end
end
