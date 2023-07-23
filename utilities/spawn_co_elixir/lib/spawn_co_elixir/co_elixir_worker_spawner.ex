defmodule SpawnCoElixir.CoElixirWorkerSpawner do
  @moduledoc false
  require Logger

  @spec run(node, node, keyword) :: :ok | :error | {:error, non_neg_integer}
  def run(node_from, worker_node, options) do
    code = Keyword.fetch!(options, :code)
    modules = Keyword.fetch!(options, :modules)
    deps = Keyword.fetch!(options, :deps)

    name_or_sname =
      if "#{worker_node}" =~ "." do
        "--name"
      else
        "--sname"
      end

    pid = self()

    spawn(fn ->
      {_, exit_status} =
        System.cmd(
          "elixir",
          [
            name_or_sname,
            Atom.to_string(worker_node),
            "-e",
            build_program(node_from, modules, code, deps)
          ],
          into: IO.stream()
        )

      send(pid, exit_status)
    end)

    Stream.unfold({false, 20}, fn
      {_, 0} -> nil
      {true, _} -> nil
      {:ignored, count} -> next_count(count, worker_node)
      {false, count} -> next_count(count, worker_node)
    end)
    |> Enum.reduce(false, fn
      {true, _}, _ -> true
      {false, _}, acc -> acc
    end)
    |> case do
      true ->
        Logger.info("Node #{worker_node} is connected.")
        :ok

      false ->
        receive do
          exit_status ->
            Logger.error("exit_status of #{worker_node} is #{exit_status}")
            exit_status
        after
          1000 ->
            Logger.error("Timeout to receive exit_status from #{worker_node}")
            nil
        end
        |> case do
          nil -> :error
          exit_status -> {:error, exit_status}
        end
    end
  end

  defp next_count(count, worker_node) do
    Process.sleep(100)
    n = Node.connect(worker_node)

    {
      {n, count - 1},
      {n, count - 1}
    }
  end

  defp build_program(node_from, modules, code, deps) do
    """
    #{modules}

    defmodule SpawnCoElixir.CoElixir.Worker do
      def run() do
        Mix.install(#{inspect(deps)})

        #{code}

        receive do
          :end -> :ok
        end
      end
    end

    case Node.connect(:"#{node_from}") do
      true ->
        SpawnCoElixir.CoElixir.Worker.run()
        :ok

      _ -> raise RuntimeError, "could not connect to #{node_from}"
    end
    """
    |> convert(:os.type())
  end

  defp convert(code, {:unix, _}), do: code

  defp convert(code, {:win32, _}) do
    Logger.debug(inspect(code))
    String.replace(code, "\r\n", "\n")
  end
end
