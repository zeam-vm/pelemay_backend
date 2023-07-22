defmodule SpawnCoElixir.CoElixirWorkerSpawner do
  @moduledoc false

  @spec run(node, node, keyword) :: :ok | {:error, non_neg_integer}
  def run(node_from, worker_node, options) do
    code = Keyword.fetch!(options, :code)
    deps = Keyword.fetch!(options, :deps)

    {_, exit_status} =
      System.cmd(
        "elixir",
        [
          "--sname",
          Atom.to_string(worker_node),
          "-e",
          build_program(node_from, code, deps)
        ],
        into: IO.stream()
      )

    if exit_status == 0 do
      :ok
    else
      {:error, exit_status}
    end
  end

  defp build_program(node_from, code, deps) do
    """
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
  end
end
