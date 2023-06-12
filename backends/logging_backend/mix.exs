defmodule LoggingBackend.MixProject do
  use Mix.Project

  @version "0.1.0-dev"

  def project do
    [
      app: :logging_backend,
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
      {:dialyxir, "~> 1.3", only: [:dev], runtime: false},
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:backend_decorator, path: "../../utilities/backend_decorator"}
    ]
  end
end
