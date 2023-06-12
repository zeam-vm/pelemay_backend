defmodule OnnxToAxonBench do
  @moduledoc """
  Documentation for `OnnxToAxonBench`.
  """
  require Logger

  @spec run() :: any()
  def run() do
    init()

    onnx_uri = [
      "https://huggingface.co/ScottMueller/Cats_v_Dogs.ONNX/resolve/main/cats_v_dogs.onnx",
      "https://huggingface.co/ScottMueller/Cat_Dog_Breeds.ONNX/resolve/main/cat_dog_breeds.onnx",
      "https://github.com/onnx/models/raw/main/vision/classification/resnet/model/resnet101-v1-7.onnx"
    ]

    inputs =
      onnx_uri
      |> Enum.map(fn url -> basename_from_uri(url) end)
      |> Enum.map(fn basename -> {basename, basename} end)
      |> Map.new()

    setup_onnx(onnx_uri)

    Benchee.run(
      %{
        "AxonOnnx.import |> Nx.serialize" => fn path_to_onnx ->
          get_axon_from_onnx(path_to_onnx)
        end
      },
      inputs: inputs,
      before_each: fn basename ->
        Path.join(path_models_onnx(), basename)
      end,
      memory_time: 2
    )
  end

  @spec priv() :: String.t()
  def priv() do
    Application.app_dir(:onnx_to_axon_bench, "priv")
  end

  @spec path_models_onnx() :: binary()
  def path_models_onnx() do
    Path.join(priv(), "models/onnx")
  end

  @spec path_models_axon() :: binary()
  def path_models_axon() do
    Path.join(priv(), "models/onnx")
  end

  @spec path_data() :: binary()
  def path_data() do
    Path.join(priv(), "data")
  end

  @spec download(Req.url(), keyword()) :: {:ok, binary() | term()} | {:error, Exception.t()}
  def download(source_url, req_options \\ []) do
    case Req.get(source_url, [finch_request: &finch_request/4] ++ req_options) do
      {:ok, response} -> {:ok, response.body}
      {:error, reason} -> {:error, reason}
    end
  end

  @spec download!(Req.url(), keyword()) :: binary() | term()
  def download!(source_url, req_options \\ []) do
    Req.get!(source_url, [finch_request: &finch_request/4] ++ req_options).body
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

    cond do
      total_size > 0 -> handle_message_data(data, response)
      true -> response
    end
  end

  defp handle_message_data(data, response) do
    new_downloaded_size = response.private.downloaded_size + byte_size(data)
    ProgressBar.render(new_downloaded_size, response.private.total_size, suffix: :bytes)

    response
    |> Map.update!(:body, &(&1 <> data))
    |> Map.update!(:private, &%{&1 | downloaded_size: new_downloaded_size})
  end

  @spec basename_from_uri(Req.url()) :: String.t()
  def basename_from_uri(url) when is_binary(url) do
    Path.basename(url)
  end

  def basename_from_uri(url) do
    URI.parse(url) |> Map.get(:path) |> Path.basename()
  end

  @spec init() :: :ok
  def init() do
    File.mkdir_p!(path_models_onnx())
    File.mkdir_p!(path_models_axon())
    File.mkdir_p!(path_data())
  end

  @spec download_files(list(Req.url()), String.t()) :: list(String.t())
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

  @spec setup_onnx(list(String.t())) :: list(String.t())
  def setup_onnx(files) do
    download_files(files, path_models_onnx())
  end

  @spec setup_data(list(String.t())) ::
          list(:ok | {:ok, list({charlist(), String.t()})} | {:error, any()})
  def setup_data(files) do
    download_files(files, path_data())

    files
    |> Flow.from_enumerable(max_demand: 1)
    |> Flow.map(fn url -> extract_from_url(url) end)
    |> Enum.to_list()
  end

  @spec extract_from_url(Req.url()) ::
          :ok | {:ok, list({charlist(), String.t()})} | {:error, any()}
  def extract_from_url(url) do
    Path.join(path_data(), basename_from_uri(url))
    |> File.read!()
    |> extract_tar_from_string()
  end

  @spec extract_tar_from_string(binary()) ::
          :ok | {:ok, list({charlist(), String.t()})} | {:error, any()}
  def extract_tar_from_string(contents) do
    :erl_tar.extract({:binary, contents}, [
      :compressed,
      {:cwd, path_data() |> String.to_charlist()}
    ])
  end

  @spec axon_name_from_onnx_path(binary()) :: String.t()
  def axon_name_from_onnx_path(onnx_path) do
    model_root = onnx_path |> Path.basename() |> Path.rootname()
    "#{model_root}.axon"
  end

  @spec axon_path_from_onnx_path(binary()) :: binary()
  def axon_path_from_onnx_path(onnx_path) do
    Path.join(path_models_axon(), axon_name_from_onnx_path(onnx_path))
  end

  @spec get_axon_from_onnx(binary()) :: any()
  def get_axon_from_onnx(path_to_onnx_file) do
    path_to_onnx_file
    |> AxonOnnx.import()
    |> then(fn {_model, parameters} ->
      # Keep model
      Nx.serialize(parameters)
    end)
  end
end
