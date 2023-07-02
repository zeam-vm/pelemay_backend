defmodule PelemayBackend do
  @moduledoc File.read!("README.md")
             |> String.split("<!-- MODULEDOC -->")
             |> Enum.fetch!(1)

  @doc """
  Hello world.

  ## Examples

      iex> PelemayBackend.hello()
      :world

  """
  def hello do
    :world
  end
end
