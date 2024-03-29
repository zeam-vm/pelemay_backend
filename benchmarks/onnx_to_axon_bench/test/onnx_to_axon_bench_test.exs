defmodule OnnxToAxonBenchTest do
  use ExUnit.Case
  doctest OnnxToAxonBench
  alias OnnxToAxonBench

  describe "benchee_inputs" do
    test "returns a map of basename to onnx file path" do
      onnx_src_urls = ["https://example.com/1/2/3/cats_v_dogs.onnx"]

      %{"cats_v_dogs.onnx" => onnx_path} = OnnxToAxonBench.onnx_urls_to_inputs(onnx_src_urls)

      priv_dir = :filename.basedir(:user_cache, "onnx_to_axon_bench")
      assert onnx_path == "#{priv_dir}/models/onnx/cats_v_dogs.onnx"
    end
  end
end
