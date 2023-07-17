defmodule OnnxToAxonBench do
  @moduledoc File.read!("README.md")
             |> String.split("<!-- MODULEDOC -->")
             |> Enum.fetch!(1)

  require Logger
  alias OnnxToAxonBench.Utils.HTTP

  @onnx_urls [
    "https://huggingface.co/ScottMueller/Cats_v_Dogs.ONNX/resolve/main/cats_v_dogs.onnx",
    "https://huggingface.co/ScottMueller/Cat_Dog_Breeds.ONNX/resolve/main/cat_dog_breeds.onnx",
    "https://github.com/onnx/models/raw/main/vision/classification/resnet/model/resnet101-v1-7.onnx"
  ]

  @doc """
  Run the benchmark jobs for ONNX model URLs.

  ## Examples

      % cd benchmarks/onnx_to_axon_bench
      % mix run -e "OnnxToAxonBench.run"
  """
  @spec run(keyword()) :: :ok
  def run(options \\ []) do
    onnx_urls = options[:onnx_urls] || @onnx_urls

    File.mkdir_p!(onnx_models_dir())
    HTTP.download_files(onnx_urls, onnx_models_dir())
    inputs = onnx_urls_to_inputs(onnx_urls)

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

  @doc """
  Convert a list of ONNX model source URLs to a mapping of basename to the
  path to the download model.
  """
  @spec onnx_urls_to_inputs([binary()]) :: %{binary() => binary()}
  def onnx_urls_to_inputs(onnx_urls) do
    for onnx_url <- onnx_urls, into: %{} do
      basename = Path.basename(onnx_url)
      {basename, Path.join(onnx_models_dir(), basename)}
    end
  end

  @spec priv_dir() :: binary()
  defp priv_dir() do
    :filename.basedir(:user_cache, "onnx_to_axon_bench")
  end

  @spec onnx_models_dir() :: binary()
  defp onnx_models_dir() do
    Path.join(priv_dir(), "models/onnx")
  end

  @spec get_axon_from_onnx(binary()) :: any()
  defp get_axon_from_onnx(path_to_onnx_file) do
    {_model, parameters} = AxonOnnx.import(path_to_onnx_file)
    Nx.serialize(parameters)
  end
end
