if :erlang.system_info(:otp_release) < ~c"24" do
  Mix.raise("Nx requires Erlang/OTP 24+")
end

defmodule PelemayBackend.MixProject do
  use Mix.Project

  @version "0.1.0-dev"

  def project do
    [
      app: :pelemay_backend,
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
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
    ]
  end
end
