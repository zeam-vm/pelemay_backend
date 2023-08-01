defmodule SpawnCoElixir.MixProject do
  use Mix.Project

  @version "0.3.0"
  @source_url "https://github.com/zeam-vm/pelemay_backend"

  def project do
    [
      app: :spawn_co_elixir,
      version: @version,
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      docs: docs(),
      name: "SpawnCoElixir",
      description: "SpawnCoElixir spawns cooperative Elixir nodes that are supervised.",
      package: package(),
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
      extra_applications: [:logger],
      mod: {SpawnCoElixir.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:dialyxir, "~> 1.3", only: :test, runtime: false},
      {:credo, "~> 1.7", only: :test, runtime: false},
      {:ex_doc, "~> 0.29", only: :docs, runtime: false},
      {:node_activator, "~> 0.2"}
    ]
  end

  defp package do
    [
      maintainers: ["Susumu Yamazaki", "Masatoshi Nishiguchi"],
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => @source_url}
    ]
  end

  defp docs do
    [
      main: "SpawnCoElixir",
      source_url_pattern:
        "#{@source_url}/blob/v#{@version}/utilities/spawn_co_elixir/%{path}#L%{line}",
      extras: [
        "README.md",
        "CHANGELOG.md"
      ]
    ]
  end
end
