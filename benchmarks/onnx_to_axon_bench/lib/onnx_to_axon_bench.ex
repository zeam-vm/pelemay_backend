defmodule OnnxToAxonBench do
  @moduledoc """
  Documentation for `OnnxToAxonBench`.
  """
  require Logger

  @onnx_urls [
    "https://huggingface.co/ScottMueller/Cats_v_Dogs.ONNX/resolve/main/cats_v_dogs.onnx",
    "https://huggingface.co/ScottMueller/Cat_Dog_Breeds.ONNX/resolve/main/cat_dog_breeds.onnx",
    "https://github.com/onnx/models/raw/main/vision/classification/resnet/model/resnet101-v1-7.onnx"
  ]

  @spec run() :: any()
  def run() do
    init()

    inputs =
      @onnx_urls
      |> Enum.map(fn url -> OnnxToAxonBench.Utils.HTTP.basename_from_uri(url) end)
      |> Enum.map(fn basename -> {basename, basename} end)
      |> Map.new()

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

  @spec init() :: :ok
  def init() do
    File.mkdir_p!(path_models_onnx())
    File.mkdir_p!(path_models_axon())
    File.mkdir_p!(path_data())

    setup_onnx(@onnx_urls)

    :ok
  end

  @spec setup_onnx(list(String.t())) :: list(String.t())
  def setup_onnx(files) do
    OnnxToAxonBench.Utils.HTTP.download_files(files, path_models_onnx())
  end

  @spec setup_data(list(String.t())) ::
          list(:ok | {:ok, list({charlist(), String.t()})} | {:error, any()})
  def setup_data(files) do
    OnnxToAxonBench.Utils.HTTP.download_files(files, path_data())

    files
    |> Flow.from_enumerable(max_demand: 1)
    |> Flow.map(fn url -> extract_from_url(url) end)
    |> Enum.to_list()
  end

  @spec extract_from_url(Req.url()) ::
          :ok | {:ok, list({charlist(), String.t()})} | {:error, any()}
  def extract_from_url(url) do
    Path.join(path_data(), OnnxToAxonBench.Utils.HTTP.basename_from_uri(url))
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
