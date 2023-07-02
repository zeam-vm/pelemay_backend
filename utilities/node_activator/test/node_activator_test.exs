defmodule NodeActivatorTest do
  use ExUnit.Case
  doctest NodeActivator

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

  describe "generate_node_name" do
    test "generates unique atom" do
      node_name1 = NodeActivator.generate_node_name("test prefix")
      node_name2 = NodeActivator.generate_node_name("test prefix")

      assert is_atom(node_name1)
      refute node_name1 == node_name2
    end

    test "starts with provided prefix" do
      node_name = NodeActivator.generate_node_name("test prefix")

      assert node_name |> Atom.to_string() |> String.starts_with?("test-prefix_")
    end

    test "ends with hostname" do
      node_name = NodeActivator.generate_node_name("test prefix")
      hostname = NodeActivator.get_hostname()

      assert node_name |> Atom.to_string() |> String.ends_with?(hostname)
    end
  end

  describe "get_hostname" do
    test "returns non-blank string" do
      hostname = NodeActivator.get_hostname()

      assert is_binary(hostname)
      assert String.length(hostname) > 1
    end
  end

  describe "launch_epmd" do
    test "does not raise error" do
      assert :ok = NodeActivator.launch_epmd()
    end
  end
end
