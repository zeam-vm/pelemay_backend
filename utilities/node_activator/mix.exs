defmodule NodeActivator.MixProject do
  use Mix.Project

  @version "0.2.0"
  @source_url "https://github.com/zeam-vm/pelemay_backend"

  def project do
    [
      app: :node_activator,
      version: @version,
      elixir: "~> 1.14",
      deps: deps(),
      docs: docs(),
      name: "NodeActivator",
      description: "A module to activate VM nodes",
      package: package()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger, :crypto]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:dialyxir, "~> 1.3", only: [:dev], runtime: false},
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:ex_doc, "~> 0.29", only: :dev, runtime: false}
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
      main: "NodeActivator",
      source_url_pattern:
        "#{@source_url}/blob/v#{@version}/utilities/node_activator/%{path}#L%{line}",
      extras: [
        "README.md",
        "CHANGELOG.md"
      ]
    ]
  end
end
