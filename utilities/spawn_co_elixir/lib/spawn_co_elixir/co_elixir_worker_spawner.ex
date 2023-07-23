defmodule SpawnCoElixir.CoElixirWorkerSpawner do
  @moduledoc false
  require Logger

  @waiting_msec 20
  @counter_waiting 100

  @spec run(node, node, keyword) :: :ok | :error | {:error, non_neg_integer}
  def run(node_from, worker_node, options) do
    code = Keyword.fetch!(options, :code)
    deps = Keyword.fetch!(options, :deps) ++ [{:spawn_co_elixir, path: "."}]

    name_or_sname =
      if "#{worker_node}" =~ "." do
        "--name"
      else
        "--sname"
      end

    pid = self()

    spawn(fn ->
      {_, exit_status} =
        spawn_elixir(name_or_sname, worker_node, deps, code, node_from)

      send(pid, exit_status)
    end)

    Stream.unfold({false, @counter_waiting}, fn
      {_, 0} -> nil
      {true, _} -> nil
      {:ignored, count} -> next_count(count, worker_node)
      {false, count} -> next_count(count, worker_node)
    end)
    |> Enum.reduce(false, fn
      {true, _}, _ -> true
      {false, _}, acc -> acc
    end)
    |> epilogue(worker_node)
  end

  defp next_count(count, worker_node) do
    Process.sleep(@waiting_msec)
    n = Node.connect(worker_node)

    {
      {n, count - 1},
      {n, count - 1}
    }
  end

  defp spawn_elixir(name_or_sname, worker_node, deps, code, node_from) do
    System.cmd(
      "elixir",
      [
        name_or_sname,
        Atom.to_string(worker_node),
        "-e",
        integrated_code(deps, code, node_from)
      ],
      into: IO.stream()
    )
  end

  defp integrated_code(deps, code, node_from) do
    "Mix.install(#{deps_listing(deps)}); "
    |> Kernel.<>(
      case code do
        "" -> ""
        code -> "#{code}; "
      end
    )
    |> Kernel.<>(
      "#{atom_listing(node_from)} |> List.to_atom() |> SpawnCoElixir.CoElixir.Worker.run()"
    )
  end

  defp deps_listing(list) when is_list(list) do
    list
    |> Enum.map_join(", ", &deps_listing(&1))
    |> then(&"[#{&1}]")
  end

  defp deps_listing({a, b}) do
    "{#{deps_listing(a)}, #{deps_listing(b)}}"
  end

  defp deps_listing(atom) when is_atom(atom) do
    r = "#{inspect(atom)}"

    if Regex.match?(~r/\"/, r) do
      "#{atom_listing(atom)} |> List.to_atom()"
    else
      r
    end
  end

  defp deps_listing(str) when is_binary(str) do
    "#{string_listing(str)} |> List.to_string()"
  end

  defp string_listing(str) do
    str
    |> String.to_charlist()
    |> Enum.join(", ")
    |> then(&"[#{&1}]")
  end

  defp atom_listing(atom) do
    atom
    |> Atom.to_charlist()
    |> Enum.join(", ")
    |> then(&"[#{&1}]")
  end

  defp epilogue(true, worker_node) do
    Logger.info("Node #{worker_node} is connected.")
    :ok
  end

  defp epilogue(false, worker_node) do
    Logger.debug("NodeActivator.epmd_running?: #{NodeActivator.epmd_running?()}")

    receive do
      exit_status ->
        Logger.error("exit_status of #{worker_node} is #{inspect(exit_status)}")
        {:error, exit_status}
    after
      1000 ->
        Logger.error("Timeout to receive exit_status from #{worker_node}")
        :error
    end
  end
end
