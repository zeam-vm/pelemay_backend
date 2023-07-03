defmodule NodeActivatorTest do
  use ExUnit.Case
  doctest NodeActivator

  alias NodeActivator.Epmd
  alias NodeActivator.Utils

  setup do
    on_exit(fn -> Node.stop() end)
  end

  describe "run" do
    test "starts node with correct name" do
      refute Node.alive?()

      {:ok, node_name} = NodeActivator.run("test prefix")

      assert Node.alive?()
      assert Node.self() == node_name
      assert "test-prefix" <> <<_::binary>> = to_string(node_name)
    end

    test "is idempotent" do
      {:ok, node_name1} = NodeActivator.run("test prefix")
      {:ok, node_name2} = NodeActivator.run("test prefix")

      assert node_name1 == node_name2
    end
  end

  describe "launch_epmd" do
    test "does not raise error" do
      assert :ok = Epmd.launch_epmd()
    end
  end
end
