defmodule BackendDecorator.MixProject do
  use Mix.Project

  @nx_version "0.5.3"
  @version "0.1.0-dev"

  def project do
    [
      app: :backend_decorator,
      version: @version,
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
      {:nx, "~> #{@nx_version}"}
    ]
  end
end
