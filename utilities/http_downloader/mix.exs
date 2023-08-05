defmodule HttpDownloader.MixProject do
  use Mix.Project

  @version "0.1.0"
  @source_url "https://github.com/zeam-vm/pelemay_backend"

  def project do
    [
      app: :http_downloader,
      version: @version,
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      docs: docs(),
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
      {:req, "~> 0.3.8"},
      {:progress_bar, "~> 2.0"}
    ]
  end

  defp docs do
    [
      main: "HttpDownloader",
      source_url_pattern:
        "#{@source_url}/blob/v#{@version}/utilities/http_downloaderr/%{path}#L%{line}",
      extras: [
        "README.md"
      ]
    ]
  end
end
