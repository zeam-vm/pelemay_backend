defmodule OnnxToAxonBench do
  @moduledoc File.read!("README.md")
             |> String.split("<!-- MODULEDOC -->")
             |> Enum.fetch!(1)

  require Logger

  @onnx_urls [
    "https://huggingface.co/ScottMueller/Cats_v_Dogs.ONNX/resolve/main/cats_v_dogs.onnx",
    "https://huggingface.co/ScottMueller/Cat_Dog_Breeds.ONNX/resolve/main/cat_dog_breeds.onnx",
    "https://github.com/onnx/models/raw/main/vision/classification/resnet/model/resnet101-v1-7.onnx"
  ]

  @spec run(keyword()) :: :ok
  def run(options \\ []) do
    onnx_urls = options[:onnx_urls] || @onnx_urls

    setup(onnx_urls)
    inputs = benchee_inputs(onnx_urls)

    Benchee.run(
      %{
        "AxonOnnx.import |> Nx.serialize" => fn path_to_onnx ->
          get_axon_from_onnx(path_to_onnx)
        end
      },
      inputs: inputs,
      memory_time: 2
    )

    :ok
  end

  @spec benchee_inputs([binary()]) :: %{binary() => binary()}
  def benchee_inputs(onnx_urls) do
    for onnx_url <- onnx_urls, into: %{} do
      basename = Path.basename(onnx_url)
      {basename, Path.join(path_models_onnx(), basename)}
    end
  end

  @spec priv() :: binary()
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

  @spec setup([binary()]) :: :ok
  def setup(onnx_urls) do
    File.mkdir_p!(path_models_onnx())
    File.mkdir_p!(path_models_axon())
    File.mkdir_p!(path_data())

    setup_onnx(onnx_urls)

    :ok
  end

  @spec setup_onnx([binary()]) :: [binary()]
  def setup_onnx(files) do
    OnnxToAxonBench.Utils.HTTP.download_files(files, path_models_onnx())
  end

  @spec setup_data([binary()]) :: [:ok | {:ok, [{charlist(), binary()}]} | {:error, any()}]
  def setup_data(files) do
    OnnxToAxonBench.Utils.HTTP.download_files(files, path_data())

    files
    |> Flow.from_enumerable(max_demand: 1)
    |> Flow.map(fn url -> extract_from_url(url) end)
    |> Enum.to_list()
  end

  @spec extract_from_url(URI.t() | String.t()) ::
          :ok | {:ok, [{charlist(), binary()}]} | {:error, any()}
  def extract_from_url(url) do
    Path.join(path_data(), OnnxToAxonBench.Utils.HTTP.basename_from_uri(url))
    |> File.read!()
    |> extract_tar_from_string()
  end

  @spec extract_tar_from_string(binary()) ::
          :ok | {:ok, [{charlist(), binary()}]} | {:error, any()}
  def extract_tar_from_string(contents) do
    :erl_tar.extract({:binary, contents}, [
      :compressed,
      {:cwd, path_data() |> String.to_charlist()}
    ])
  end

  @spec axon_name_from_onnx_path(binary()) :: binary()
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
