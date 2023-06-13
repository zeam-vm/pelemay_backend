defmodule DistributedBatchProcessingWithAxonBench.MixProject do
  use Mix.Project

  def project do
    [
      app: :distributed_batch_processing_with_axon_bench,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:dialyxir, "~> 1.3", only: [:dev], runtime: false},
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:nx, "~> 0.5"},
      {:axon, "~> 0.5"},
      {:exla, "~> 0.5"},
      {:axon_onnx, "~> 0.4"},
      {:benchee, "~> 1.1"},
      {:req, "~> 0.3.8"},
      {:progress_bar, "~> 2.0"},
      {:flow, "~> 1.2"}
    ]
  end
end
