defmodule PelemayBackend.Defn do
  @moduledoc false

  require Logger

  @doc false
  def __stream__(_key, _input, _acc, _vars, _fun, [_args], _options) do
  end

  @doc false
  def __jit__(key, vars, fun, args_list, options) do
    __compile__(key, vars, fun, options).(args_list)
  end

  @doc false
  def __compile__(_key, _vars, _fun, options) do
    # Logger.debug(
    #  "__compile__(key: #{inspect(key)}, vars: #{inspect(vars)}, fun: #{inspect(fun)}, options: #{inspect(options)})"
    # )

    # Logger.debug("fun #{inspect(fun)}(#{inspect(vars)}): #{inspect(fun.(vars))}")

    {_run_options, _compile_options} = Keyword.pop(options, :run_options, [])

    code =
      """
      aloadt 0
      copy
      is_scalar
      skip {10, {:if, true}}
      aloadt 1
      is_scalar
      skip {4, {:if, true}}
      sende 'multiply with two vectors is not supported.'
      pop2
      pop2
      return
      scal 1
      sendt
      return
      aloadt 1
      copy
      swap
      scal 1
      sendt
      """
      |> PelemayBackend.Engine.assemble()

    fn [args] ->
      args =
        Enum.map(args, fn a ->
          cond do
            is_struct(a, Nx.Tensor) ->
              {
                Nx.size(a),
                Nx.shape(a),
                Nx.type(a),
                Nx.to_binary(a)
              }

            true ->
              a
          end
        end)

      try do
        case PelemayBackend.Engine.execute(code, args, self()) do
          :ok -> :ok
          {:error, reason} -> raise RuntimeError, message: List.to_string(reason)
        end
      rescue
        e in ErlangError -> raise RuntimeError, message: List.to_string(e.original)
      end

      receive do
        {:result, binary, shape, type} ->
          Nx.from_binary(binary, type)
          |> Nx.reshape(shape)
          |> then(&[&1])

        {:error, reason} ->
          raise RuntimeError, message: List.to_string(reason)
      after
        5000 ->
          raise RuntimeError, message: "timeout"
      end
    end
  end
end
