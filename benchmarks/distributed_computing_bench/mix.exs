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
        bumblebee_bench: :bumblebee_bench,
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
    case Mix.env() do
      :bumblebee_bench ->
        [
          extra_applications: [:logger, :exla]
        ]

      :test ->
        [
          extra_applications: [:logger, :exla]
        ]

      _ ->
        [
          extra_applications: [:logger]
        ]
    end
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:dialyxir, "~> 1.3", only: :test, runtime: false},
      {:credo, "~> 1.7", only: :test, runtime: false},
      {:ex_doc, "~> 0.29", only: :docs, runtime: false},
      {:http_downloader, "~> 0.1"},
      {:spawn_co_elixir, "~> 0.3"},
      {:nx, "~> 0.5"},
      {:bumblebee, "~> 0.4.2", only: [:bumblebee_bench, :test]},
      {:exla, "~> 0.5", only: [:bumblebee_bench, :test]},
      {:benchee, "~> 1.1"}
    ]
  end
end
