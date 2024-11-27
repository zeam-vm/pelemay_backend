defmodule PelemayBackend.NIF do
  @moduledoc """
  Documentation for `PelemayBackend.NIF`.
  """
  require Logger

  @on_load :load_nif

  @doc false
  def load_nif do
    nif_file = ~c'#{Application.app_dir(:pelemay_backend, "priv/libnif")}'

    case :erlang.load_nif(nif_file, 0) do
      :ok -> :ok
      {:error, {:reload, _}} -> :ok
      {:error, reason} -> Logger.error("Failed to load NIF: #{inspect(reason)}")
    end
  end

  def execute_engine(_code, _args, _pid), do: :erlang.nif_error(:not_loaded)
end
