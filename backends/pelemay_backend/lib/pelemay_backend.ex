defmodule PelemayBackend do
  @moduledoc ~S"""
  An integrated lightweight tensor backend for Nx.
  """

  @behaviour Nx.Defn.Compiler

  @doc """
  A shortcut for `Nx.Defn.jit/2` with the Pelemay backend compiler.

      iex> PelemayBackend.jit(&Nx.add(&1, &1)).(Nx.tensor([1, 2, 3]))
      #Nx.Tensor<
        s64[3]
        [2, 4, 6]
      >

  ## Options

  It accepts the same option as `Nx.Defn.jit/2` plus:

    * `:debug` - print compile and debugging information, defaults to `false`.

    * `:cache` - cache the results of compilation, defaults to `true`.

    * `:client` - an atom representing the client to use. The default
      client is chosen on this order: `:cuda`, `:rocm`, `:tpu`, and `:host`.

    * `:device_id` - the default device id to run the computation on.
      Defaults to the `:default_device_id` on the client

  """
  def jit(function, options \\ []) do
    Nx.Defn.jit(function, Keyword.put(options, :compiler, PelemayBackend))
  end

  @doc """
  A shortcut for `Nx.Defn.jit_apply/3` with the Pelemay backend compiler.

      iex> PelemayBackend.jit_apply(&Nx.add(&1, &1), [Nx.tensor([1, 2, 3])])
      #Nx.Tensor<
        s64[3]
        [2, 4, 6]
      >

  See `jit/2` for supported options.
  """
  def jit_apply(function, args, options \\ []) do
    Nx.Defn.jit_apply(function, args, Keyword.put(options, :compiler, PelemayBackend))
  end

  @doc """
  A shortcut for `Nx.Defn.compile/3` with the Pelemay backend compiler.

      iex> fun = PelemayBackend.compile(&Nx.add(&1, &1), [Nx.template({3}, {:s, 64})])
      iex> fun.(Nx.tensor([1, 2, 3]))
      #Nx.Tensor<
        s64[3]
        [2, 4, 6]
      >

  ## Options

  It accepts the same option as `Nx.Defn.compile/3` plus:

    * `:debug` - print compile and debugging information, defaults to `false`.

    * `:cache` - cache the results of compilation, defaults to `true`.
      You can set it to false if you plan to compile the function only
      once and store the compile contents somewhere.

    * `:client` - an atom representing the client to use. The default
      client is chosen on this order: `:cuda`, `:rocm`, `:tpu`, and `:host`.

    * `:device_id` - the default device id to run the computation on.
      Defaults to the `:default_device_id` on the client

  """
  def compile(function, args, options \\ []) do
    Nx.Defn.compile(function, args, Keyword.put(options, :compiler, PelemayBackend))
  end

  @doc """
  Starts streaming the given anonymous function with just-in-time
  compilation.

  At least two arguments are expected:

    1. The first argument is a tensor template of the data to
       be streamed in

    2. The second argument is a tensor with the stream initial state

  The streaming function must return a two element tuple, the
  first element is the data to be sent and the second is the
  accumulator.

  For each streamed chunk, you must call `Nx.Stream.send/2` and
  `Nx.Stream.recv/1`. You don't need to call `recv` immediately
  after `send`, but doing so can be a useful mechanism to provide
  backpressure. Once all chunks are sent, you must use `Nx.Stream.done/1`
  to receive the accumulated result. Let's see an example:

      defmodule Streamed do
        import Nx.Defn

        defn sum(tensor, acc) do
          {acc, tensor + acc}
        end
      end

  Now let's invoke it:

      stream = PelemayBackend.stream(&Streamed.sum/2, [Nx.template({}, {:s, 64}), 0])

      for i <- 1..5 do
        Nx.Stream.send(stream, i)
        IO.inspect {:chunk, Nx.Stream.recv(stream)}
      end

      IO.inspect {:result, Nx.Stream.done(stream)}

  It will print:

      {:chunk, 0}
      {:chunk, 1}
      {:chunk, 2}
      {:chunk, 3}
      {:chunk, 4}
      {:result, 5}

  **Note:** While any process can call `Nx.Stream.send/2`, EXLA
  expects the process that starts the streaming to be the one
  calling `Nx.Stream.recv/1` and `Nx.Stream.done/1`.

  See `jit/2` for supported options.
  """
  def stream(function, args, options \\ []) do
    Nx.Defn.stream(function, args, Keyword.put(options, :compiler, PelemayBackend))
  end

  @doc """
  Checks if the compilation of function with args is cached.

  Note that hooks are part of the cache, and
  therefore they must be included in the options.

  ## Examples

      iex> fun = fn a, b -> Nx.add(a, b) end
      iex> left = Nx.tensor(1, type: {:u, 8})
      iex> right = Nx.tensor([1, 2, 3], type: {:u, 16})
      iex> PelemayBackend.jit(fun).(left, right)
      iex> PelemayBackend.cached?(fun, [left, right])
      true
      iex> PelemayBackend.cached?(fun, [left, Nx.tensor([1, 2, 3, 4], type: {:u, 16})])
      false

  Compiled functions are also cached, unless cache is set to false:

      iex> fun = fn a, b -> Nx.subtract(a, b) end
      iex> left = Nx.tensor(1, type: {:u, 8})
      iex> right = Nx.tensor([1, 2, 3], type: {:u, 16})
      iex> PelemayBackend.compile(fun, [left, right], cache: false)
      iex> PelemayBackend.cached?(fun, [left, right])
      false
      iex> PelemayBackend.compile(fun, [left, right])
      iex> PelemayBackend.cached?(fun, [left, right])
      true

  """
  def cached?(function, args, options \\ []) do
    function |> jit([{PelemayBackend, cached_check()} | options]) |> apply(args)
  catch
    {:cached?, bool} -> bool
  end

  @doc """
  Checks if the JIT compilation of stream with
  args is cached.

  Note that hooks are part of the cache, and
  therefore they must be included in the options.

  ## Examples

      iex> left = Nx.tensor(1, type: {:u, 8})
      iex> right = Nx.tensor([1, 2, 3], type: {:u, 16})
      iex> fun = fn x, acc -> {acc, Nx.add(x, acc)} end
      iex> stream = PelemayBackend.stream(fun, [left, right])
      iex> Nx.Stream.done(stream)
      iex> PelemayBackend.stream_cached?(fun, [left, right])
      true
      iex> PelemayBackend.stream_cached?(fun, [left, Nx.tensor([1, 2, 3, 4], type: {:u, 16})])
      false
  """
  def stream_cached?(function, args, options \\ []) do
    stream(function, args, [{PelemayBackend, cached_check()} | options])
  catch
    {:cached?, bool} -> bool
  end

  defp cached_check do
    expr_cache_fun = fn key, _callback ->
      if res = PelemayBackend.Defn.LockedCache.get(key) do
        {nil, res}
      else
        throw({:cached?, false})
      end
    end

    comp_cache_fun = fn key, _callback ->
      throw({:cached?, PelemayBackend.Defn.LockedCache.get(key) != nil})
    end

    {expr_cache_fun, comp_cache_fun}
  end

  @impl true
  defdelegate __compile__(key, vars, fun, opts), to: PelemayBackend.Defn

  @impl true
  defdelegate __jit__(key, vars, fun, args, opts), to: PelemayBackend.Defn

  @impl true
  defdelegate __stream__(key, input, acc, vars, fun, args, opts), to: PelemayBackend.Defn
end
