defmodule NodeActivator.UtilsTest do
  use ExUnit.Case
  doctest NodeActivator.Utils
  alias NodeActivator.Utils

  describe "generate_node_name" do
    test "generates unique atom" do
      node_name1 = Utils.generate_node_name("test prefix")
      node_name2 = Utils.generate_node_name("test prefix")

      assert is_atom(node_name1)
      refute node_name1 == node_name2
    end

    test "starts with provided prefix" do
      node_name = Utils.generate_node_name("test prefix")

      assert node_name |> Atom.to_string() |> String.starts_with?("test-prefix_")
    end

    test "ends with hostname" do
      node_name = Utils.generate_node_name("test prefix")
      hostname = Utils.get_hostname()

      assert node_name |> Atom.to_string() |> String.ends_with?(hostname)
    end
  end

  describe "get_hostname" do
    test "returns non-blank string" do
      hostname = Utils.get_hostname()

      assert is_binary(hostname)
      assert String.length(String.trim(hostname)) > 0
    end
  end
end
