defmodule DistributedComputingBench.MixProject do
  use Mix.Project

  @version "0.1.0"

  def project do
    [
      app: :distributed_computing_bench,
      version: @version,
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      preferred_cli_env: %{
        credo: :test,
        dialyzer: :test,
        docs: :docs,
        "hex.publish": :docs,
        "hex.build": :docs
      }
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
      {:dialyxir, "~> 1.3", only: :test, runtime: false},
      {:credo, "~> 1.7", only: :test, runtime: false},
      {:ex_doc, "~> 0.29", only: :docs, runtime: false},
      {:nx, "~> 0.5"},
      {:axon, "~> 0.5"},
      {:axon_onnx, "~> 0.4"},
      {:benchee, "~> 1.1"},
      {:req, "~> 0.3.8"},
      {:progress_bar, "~> 2.0"}
    ]
  end
end
