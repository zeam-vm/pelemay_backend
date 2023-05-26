defmodule DecoratingBackend do
  @moduledoc """
  A backend to decorate the specified `based_backend` with the functions
  before and after a set of functions in the backend. The set can be specified
  with the style of
  [AspectJ, which is an AOP language](https://en.wikipedia.org/wiki/Aspect-oriented_programming),
  and with grouping written in [hexdocs of Nx](https://hexdocs.pm/nx/Nx.html),
  for example, Aggregates, Backend, Conversion, and so on.
  """

  @doc """
  Hello world.

  ## Examples

      iex> DecoratingBackend.hello()
      :world

  """
  def hello do
    :world
  end
end
