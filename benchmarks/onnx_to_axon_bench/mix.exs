defmodule OnnxToAxonBench.MixProject do
  use Mix.Project

  @version "0.1.0"
  @source_url "https://github.com/zeam-vm/pelemay_backend"
  @homepage_url "https://zeam-vm.github.io/pelemay_backend"

  def project do
    [
      app: :onnx_to_axon_bench,
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
      },

      # Docs
      name: "OnnxToAxonBench",
      source_url: @source_url,
      homepage_url: @homepage_url,
      docs: [
        main: "OnnxToAxonBench",
        extras: ["README.md"]
      ]
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
      {:http_downloader, "~> 0.1"},
      {:nx, "~> 0.5"},
      {:axon, "~> 0.5"},
      {:axon_onnx, "~> 0.4"},
      {:benchee, "~> 1.1"},
      {:flow, "~> 1.2"}
    ]
  end
end
