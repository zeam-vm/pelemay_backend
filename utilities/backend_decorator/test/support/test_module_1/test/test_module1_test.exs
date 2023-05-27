defmodule TestModule1Test do
  use ExUnit.Case
  doctest TestModule1

  test "greets the world" do
    assert TestModule1.hello() == :world
  end
end
