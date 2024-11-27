defmodule PelemayBackend.Backend do
  @moduledoc ~S"""
  An integrated lightweight tensor backend for Nx.
  """
  require Logger

  @behaviour Nx.Backend
  # @enforce_keys [:state]

  @spec __struct__ :: %PelemayBackend.Backend{state: nil}
  @doc false
  defstruct [:state]

  alias Nx.Tensor, as: T
  alias PelemayBackend.Backend, as: B

  import Nx.Shared

  @impl true
  def constant(%{type: type, shape: shape} = out, constant, _backend_options) do
    data = :binary.copy(number_to_binary(constant, type), Nx.size(shape))
    from_binary(out, data)
  end

  @impl true
  def from_binary(t, binary, _backend_options), do: from_binary(t, binary)

  defp from_binary(t, binary) when is_binary(binary), do: %{t | data: %B{state: binary}}
  defp from_binary(t, other), do: %{t | data: %B{state: IO.iodata_to_binary(other)}}

  @impl true
  def backend_copy(tensor, backend, opts) do
    backend_transfer(tensor, backend, opts)
  end

  @impl true
  def backend_transfer(tensor, Nx.Tensor, _opts) do
    tensor
  end

  def backend_transfer(tensor, Nx.BinaryBackend, _opts) do
    tensor
  end

  def backend_transfer(tensor, PelemayBackend.Backend, _opts) do
    tensor
  end

  def backend_transfer(tensor, backend, opts) do
    backend.from_binary(tensor, to_binary(tensor), opts)
  end

  @impl true
  def backend_deallocate(_tensor) do
    :ok
  end

  @impl true
  def to_batched(out, tensor, opts) do
    leftover = opts[:leftover]

    batch_size = elem(out.shape, 0)
    axis_size = elem(tensor.shape, 0)

    remainder = rem(axis_size, batch_size)
    num_full_batches = div(axis_size, batch_size)

    range =
      if remainder != 0 and leftover == :repeat do
        0..num_full_batches
      else
        0..(num_full_batches - 1)
      end

    Stream.map(range, fn
      ^num_full_batches ->
        expr_fun = fn tensor ->
          Nx.concatenate([
            Nx.slice_along_axis(tensor, num_full_batches * batch_size, remainder),
            Nx.slice_along_axis(tensor, 0, batch_size - remainder)
          ])
        end

        jit(expr_fun, [tensor])

      i ->
        expr_fun = fn tensor, start_idx ->
          Nx.slice_along_axis(tensor, start_idx, batch_size)
        end

        start_idx = i * batch_size
        jit(expr_fun, [tensor, start_idx])
    end)
  end

  @impl true
  def to_binary(%{type: {_backend_options, size}} = t, limit) do
    limit = limit * div(size, 8)
    binary = to_binary(t)

    if byte_size(binary) == limit do
      binary
    else
      binary_part(binary, 0, limit)
    end
  end

  defp to_binary(%T{data: %{state: data}}), do: data

  @impl true
  def inspect(%T{} = tensor, inspect_opts) do
    limit = if inspect_opts.limit == :infinity, do: :infinity, else: inspect_opts.limit + 1

    tensor
    |> to_binary(min(limit, Nx.size(tensor)))
    |> then(&Nx.Backend.inspect(tensor, &1, inspect_opts))
    |> maybe_add_signature(tensor)
  end

  defp maybe_add_signature(result, _tensor) do
    result
  end

  ## JIT callbacks

  @impl true
  def concatenate(out, tensors, axis) do
    out = Nx.to_template(out)

    expr_fun = fn tensors ->
      Nx.Defn.Expr.concatenate(out, Tuple.to_list(tensors), axis)
    end

    jit(expr_fun, [List.to_tuple(tensors)])
  end

  @impl true
  def slice(out, tensor, start_indices, lengths, strides) do
    out = Nx.to_template(out)

    if Enum.all?(start_indices, &is_integer/1) do
      expr_fun = fn tensor ->
        Nx.Defn.Expr.slice(out, tensor, start_indices, lengths, strides)
      end

      jit(expr_fun, [tensor])
    else
      expr_fun = fn tensor, start_indices ->
        Nx.Defn.Expr.slice(out, tensor, Tuple.to_list(start_indices), lengths, strides)
      end

      jit(expr_fun, [tensor, List.to_tuple(start_indices)])
    end
  end

  @impl true
  def put_slice(out, tensor, start_indices, slice) do
    out = Nx.to_template(out)

    if Enum.all?(start_indices, &is_integer/1) do
      expr_fun = fn tensor, slice ->
        Nx.Defn.Expr.put_slice(out, tensor, start_indices, slice)
      end

      jit(expr_fun, [tensor, slice])
    else
      expr_fun = fn tensor, start_indices, slice ->
        Nx.Defn.Expr.put_slice(out, tensor, Tuple.to_list(start_indices), slice)
      end

      jit(expr_fun, [tensor, List.to_tuple(start_indices), slice])
    end
  end

  @impl true
  def optional(_name, args, fun) do
    # Here we take the leading tensor arguments and pass them as JIT arguments
    {tensors, rest} = Enum.split_while(args, &is_struct(&1, Nx.Tensor))

    wrapper_fun = fn tensors ->
      tensors = Tuple.to_list(tensors)
      apply(fun, tensors ++ rest)
    end

    jit(wrapper_fun, [List.to_tuple(tensors)])
  end

  # [:add, :subtract, :multiply, :power, :remainder, :divide, :atan2, :min, :max, :quotient] ++
  binary_ops =
    [:add, :subtract, :power, :remainder, :divide, :atan2, :min, :max, :quotient] ++
      [:bitwise_and, :bitwise_or, :bitwise_xor, :left_shift, :right_shift] ++
      [:equal, :not_equal, :greater, :less, :greater_equal, :less_equal] ++
      [:logical_and, :logical_or, :logical_xor]

  unary_ops =
    [:exp, :expm1, :log, :log1p, :sigmoid, :cos, :sin, :tan] ++
      [:cosh, :sinh, :tanh, :acos, :asin, :atan, :acosh, :asinh, :atanh] ++
      [:sqrt, :rsqrt, :cbrt, :is_nan, :is_infinity, :erf, :erfc, :erf_inv] ++
      [:abs, :bitwise_not, :ceil, :conjugate, :floor, :negate, :round, :sign] ++
      [:count_leading_zeros, :population_count, :real, :imag]

  callbacks =
    [
      {:eye, [:backend_options], []},
      {:iota, [:axis, :backend_options], []},
      {:random_uniform, [:min, :max, :backend_options], [:min, :max]},
      {:random_normal, [:mu, :sigma, :backend_options], [:mu, :sigma]},
      {:as_type, [:tensor], [:tensor]},
      {:bitcast, [:tensor], [:tensor]},
      {:reshape, [:tensor], [:tensor]},
      {:squeeze, [:tensor, :axes], [:tensor]},
      {:broadcast, [:tensor, :shape, :axes], [:tensor]},
      {:transpose, [:tensor, :axes], [:tensor]},
      {:pad, [:tensor, :pad_value, :padding_config], [:tensor, :pad_value]},
      {:reverse, [:tensor, :axes], [:tensor]},
      {:dot, [:left, :c1, :b1, :right, :c2, :b2], [:left, :right]},
      {:clip, [:tensor, :min, :max], [:tensor, :min, :max]},
      {:take, [:tensor, :indices, :axis], [:tensor, :indices]},
      {:take_along_axis, [:tensor, :indices, :axis], [:tensor, :indices]},
      {:gather, [:input, :indices], [:input, :indices]},
      {:select, [:pred, :on_true, :on_false], [:pred, :on_true, :on_false]},
      {:conv, [:tensor, :kernel, :opts], [:tensor, :kernel]},
      {:all, [:tensor, :opts], [:tensor]},
      {:any, [:tensor, :opts], [:tensor]},
      {:sum, [:tensor, :opts], [:tensor]},
      {:product, [:tensor, :opts], [:tensor]},
      {:reduce_max, [:tensor, :opts], [:tensor]},
      {:reduce_min, [:tensor, :opts], [:tensor]},
      {:argmax, [:tensor, :opts], [:tensor]},
      {:argmin, [:tensor, :opts], [:tensor]},
      {:reduce, [:tensor, :acc, :opts, :fun], [:tensor, :acc]},
      {:window_reduce, [:tensor, :acc, :shape, :opts, :fun], [:tensor, :acc]},
      {:window_sum, [:tensor, :shape, :opts], [:tensor]},
      {:window_product, [:tensor, :shape, :opts], [:tensor]},
      {:window_max, [:tensor, :shape, :opts], [:tensor]},
      {:window_min, [:tensor, :shape, :opts], [:tensor]},
      {:map, [:tensor, :opts, :fun], [:tensor]},
      {:sort, [:tensor, :opts], [:tensor]},
      {:argsort, [:tensor, :opts], [:tensor]},
      {:window_scatter_max, [:tensor, :source, :init_value, :window_dims, :opts],
       [:tensor, :source, :init_value]},
      {:window_scatter_min, [:tensor, :source, :init_value, :window_dims, :opts],
       [:tensor, :source, :init_value]},
      {:indexed_add, [:tensor, :indices, :updates], [:tensor, :indices, :updates]},
      {:indexed_put, [:tensor, :indices, :updates], [:tensor, :indices, :updates]},
      {:cholesky, [:tensor], [:tensor]},
      {:lu, [:tensor, :opts], [:tensor]},
      {:qr, [:tensor, :opts], [:tensor]},
      {:triangular_solve, [:a, :b, :opts], [:a, :b]},
      {:eigh, [:tensor, :opts], [:tensor]},
      {:svd, [:tensor, :opts], [:tensor]},
      {:fft, [:tensor, :opts], [:tensor]},
      {:ifft, [:tensor, :opts], [:tensor]}
    ] ++
      for(op <- binary_ops, do: {op, [:left, :right], [:left, :right]}) ++
      for(op <- unary_ops, do: {op, [:tensor], [:tensor]})

  for {name, args, tensor_args} <- callbacks do
    args = Enum.map(args, &Macro.var(&1, __MODULE__))
    _tensor_args = Enum.map(tensor_args, &Macro.var(&1, __MODULE__))

    @impl true
    defdelegate unquote(name)(out, unquote_splicing(args)), to: Nx.BinaryBackend
  end

  binary_ops = [:multiply]

  unary_ops = []

  callbacks =
    [] ++
      for(op <- binary_ops, do: {op, [:left, :right], [:left, :right]}) ++
      for(op <- unary_ops, do: {op, [:tensor], [:tensor]})

  for {name, args, tensor_args} <- callbacks do
    args = Enum.map(args, &Macro.var(&1, __MODULE__))
    tensor_args = Enum.map(tensor_args, &Macro.var(&1, __MODULE__))

    @impl true
    def unquote(name)(out, unquote_splicing(args)) do
      out = Nx.to_template(out)

      expr_fun = fn unquote_splicing(tensor_args) ->
        Nx.Defn.Expr.unquote(name)(out, unquote_splicing(args))
      end

      jit(expr_fun, [unquote_splicing(tensor_args)])
    end
  end

  defp jit(fun, args) do
    # Logger.debug("fun: #{inspect(fun)}")
    # Logger.debug("args: #{inspect(args)}")
    PelemayBackend.jit_apply(fun, args, on_conflict: :force)
  end

  ## Conversion helpers

  defp number_to_binary(number, type),
    do: match_types([type], do: <<write!(number, 0)>>)
end
