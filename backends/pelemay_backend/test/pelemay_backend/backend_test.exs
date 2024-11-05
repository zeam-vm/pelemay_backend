defmodule PelemayBackend.BackendTest do
  use ExUnit.Case
  doctest PelemayBackend.Backend

  setup do
    Nx.default_backend(PelemayBackend.Backend)
    :ok
  end

  test "multiply scalar and vector" do
    assert Nx.multiply(2.0, Nx.tensor([1.0, 2.0], type: {:f, 32})) ==
             Nx.tensor([2.0, 4.0], type: {:f, 32})

    assert Nx.multiply(Nx.tensor([1.0, 2.0], type: {:f, 32}), 2.0) ==
             Nx.tensor([2.0, 4.0], type: {:f, 32})

    assert Nx.multiply(4.0, Nx.tensor([3.0, 4.0, 5.0], type: {:f, 32})) ==
             Nx.tensor([12.0, 16.0, 20.0], type: {:f, 32})

    assert Nx.multiply(2.0, Nx.tensor([1.0, 2.0], type: {:f, 64})) ==
             Nx.tensor([2.0, 4.0], type: {:f, 64})

    assert Nx.multiply(4.0, Nx.tensor([3.0, 4.0, 5.0], type: {:f, 64})) ==
             Nx.tensor([12.0, 16.0, 20.0], type: {:f, 64})

    assert_raise RuntimeError, fn ->
      Nx.multiply(Nx.tensor([1.0, 2.0]), Nx.tensor([2.0, 4.0]))
    end
  end

  @precision_error_doctests [
    expm1: 1,
    erfc: 1,
    erf: 1,
    cosh: 1,
    tanh: 1,
    asin: 1,
    asinh: 1,
    atanh: 1,
    ceil: 1,
    sigmoid: 1,
    fft: 2,
    ifft: 2
  ]

  @temporarily_broken_doctests [
    # XLA currently doesn't support complex conversion
    as_type: 2
  ]

  @inherently_unsupported_doctests [
    # XLA requires signed and unsigned tensors to be at least of size 32
    random_uniform: 4
  ]

  @unrelated_doctests [
    default_backend: 1
  ]

  @untest_doctests [
    # is_tensor: 1,
    # all: 2,
    all_close: 3,
    # any: 2,
    # argmax: 2,
    # argmin: 2,
    # mean: 2,
    # product: 2,
    # reduce: 4,
    # reduce_max: 2,
    # reduce_min: 2,
    # standard_deviation: 2,
    # sum: 2,
    # variance: 2,
    # backend_copy: 2,
    # backend_deallocate: 1,
    # backend_transfar: 2,
    # default_backend: 1,
    # global_default_backend: 1,
    # deserialize: 2,
    # from_numpy: 2,
    # from_numpy_archive: 2,
    # serialize: 2,
    # to_batched: 3,
    # to_backed_list: 3,
    # to_binary: 2,
    # to_flat_list: 2,
    # to_heatmap: 2,
    # to_number: 2,
    # to_template: 1,
    # to_tensor: 1,
    # eye: 2,
    # from_binary: 3,
    # iota: 2,
    # make_diagonal: 2,
    # put_diagonal: 3,
    # random_normal: 2,
    # random_normal: 4,
    # random_uniform: 2,
    # random_uniform: 4,
    # shuflle: 2,
    # sigil_M: 2,
    # sigil_V: 2,
    # take_diagonal: 2,
    # template: 3,
    # tensor: 2,
    cumulative_max: 2,
    cumulative_min: 2,
    cumulative_product: 2,
    cumulative_sum: 2,
    # abs: 1,
    # acos: 1,
    # acosh: 1,
    # add: 2,
    # asin: 1,
    # asinh: 1,
    # atan2: 2,
    # atan: 1,
    # atanh: 1,
    # bitwise_and: 2,
    # bitwise_not: 1,
    # bitwise_or: 2,
    # bitwise_xor: 2,
    # cbrt: 1,
    # ceil: 1,
    # clip: 3,
    complex: 2,
    # conjugate: 1,
    # cos: 1,
    # cosh: 1,
    # count_leading_zeros: 1,
    # divide: 2,
    # equal: 2,
    # erf: 1,
    # erf_inv: 1,
    # erlc: 1,
    # exp: 1,
    # expm1: 1,
    # floor: 1,
    # greater: 2,
    # greater_equal: 2,
    # imag: 1,
    # is_infinity: 1,
    # is_nan: 1,
    # left_shift: 2,
    # less: 2,
    # less_equal: 2,
    # log1p: 1,
    # log: 1,
    # logical_and: 2,
    logical_not: 1,
    # logical_or: 2,
    # logical_xor: 2,
    map: 3,
    # max: 2,
    # min: 2,
    multiply: 2,
    # negate: 1,
    # not_equal: 2,
    phase: 1,
    # population_count: 1,
    # power: 2,
    # quotient: 2,
    # real: 1,
    # remainder: 2,
    # right_shift: 2,
    # round: 1,
    # rsqrt: 1,
    # select: 3,
    # sigmoid: 1,
    # sign: 1,
    # sin: 1,
    # sinh: 1,
    # sqrt: 1,
    # subtract: 2,
    # tan: 1,
    # tanh: 1,
    # gather: 2,
    # indexed_add: 3,
    # indexed_put: 3,
    put_slice: 3,
    slice: 4,
    # slice_along_axis: 4,
    # take: 3,
    # take_along_axis: 3,
    # argsort: 2,
    concatenate: 2,
    # conv: 3,
    dot: 2,
    # dot: 4,
    # dot: 6,
    # fft: 2,
    # ifft: 2,
    # outer: 2,
    # reverse: 2,
    # sort: 2,
    # stack: 2,
    # axes: 1,
    # axis_index: 2,
    # axis_size: 2,
    # broadcast: 3,
    # byte_size: 1,
    # compatible?: 2,
    # flatten: 1,
    # names: 1,
    # new_axis: 3,
    # pad: 3,
    # rank: 1,
    # reshape: 3,
    # shape: 1,
    # size: 1,
    # squeeze: 2,
    # tile: 2,
    # transpose: 2,
    # as_type: 2,
    # bitcast: 2,
    # type: 1,
    # window_max: 3,
    # window_mean: 3,
    # window_min: 3,
    # window_product: 3,
    # window_reduce: 5,
    window_scatter_max: 5,
    window_scatter_min: 5
    # window_sum: 3
  ]

  doctest Nx,
    except:
      [:moduledoc] ++
        @precision_error_doctests ++
        @temporarily_broken_doctests ++
        @inherently_unsupported_doctests ++
        @unrelated_doctests ++
        @untest_doctests
end
