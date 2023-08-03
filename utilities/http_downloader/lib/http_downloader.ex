defmodule HttpDownloader do
  @moduledoc File.read!("README.md")
             |> String.split("<!-- MODULEDOC -->")
             |> Enum.fetch!(1)

  require Logger

  @type url :: URI.t() | String.t()

  @spec download(url(), keyword()) :: {:ok, binary() | term()} | {:error, Exception.t()}
  def download(source_url, req_options \\ []) do
    case Req.get(source_url, [finch_request: &finch_request/4] ++ req_options) do
      {:ok, response} -> {:ok, response.body}
      {:error, reason} -> {:error, reason}
    end
  end

  @spec download!(url(), keyword()) :: binary() | term()
  def download!(source_url, req_options \\ []) do
    Req.get!(source_url, [finch_request: &finch_request/4] ++ req_options).body
  end

  @spec basename_from_uri(url() | struct()) :: String.t()
  def basename_from_uri(url) when is_binary(url) do
    Path.basename(url)
  end

  def basename_from_uri(url) when is_map_key(url, :path) do
    URI.parse(url) |> Map.get(:path) |> Path.basename()
  end

  @spec download_files([url()], String.t()) :: [String.t()]
  def download_files(files, dst_path) do
    Enum.map(files, fn url ->
      basename = basename_from_uri(url)

      dst_path = Path.join(dst_path, basename)

      dst_path
      |> File.exists?()
      |> case do
        true ->
          Logger.info("File #{basename} has already been downloaded.")

        false ->
          Logger.info("File #{basename} will be downloaded...")
          download!(url, output: dst_path, max_redirects: 5, redirect_log_level: false)
      end
    end)
  end

  defp finch_request(req_request, finch_request, finch_name, finch_options) do
    acc = Req.Response.new()

    case Finch.stream(finch_request, finch_name, acc, &handle_message/2, finch_options) do
      {:ok, response} -> {req_request, response}
      {:error, reason} -> {req_request, reason}
    end
  end

  defp handle_message({:status, status}, response), do: %{response | status: status}

  defp handle_message({:headers, headers}, response) do
    {_, total_size} =
      Enum.find(headers, fn
        {"content-length", v} -> String.to_integer(v)
        {_, _} -> nil
      end)

    response
    |> Map.put(:headers, headers)
    |> Map.put(:private, %{total_size: String.to_integer(total_size), downloaded_size: 0})
  end

  defp handle_message({:data, data}, response) do
    total_size = response.private.total_size

    if total_size > 0 do
      handle_message_data(data, response)
    else
      response
    end
  end

  defp handle_message_data(data, response) do
    new_downloaded_size = response.private.downloaded_size + byte_size(data)
    ProgressBar.render(new_downloaded_size, response.private.total_size, suffix: :bytes)

    response
    |> Map.update!(:body, &(&1 <> data))
    |> Map.update!(:private, &%{&1 | downloaded_size: new_downloaded_size})
  end
end
