# if :erlang.system_info(:otp_release) < ~c"24" do
#   Mix.raise("Nx requires Erlang/OTP 24+")
# end

defmodule PelemayBackend.MixProject do
  use Mix.Project

  @version "0.1.0-dev"

  def project do
    [
      app: :pelemay_backend,
      version: @version,
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      compilers: [:pelemay_backend, :elixir_make] ++ Mix.compilers(),
      aliases: [
        "compile.pelemay_backend": &compile/1
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {PelemayBackend.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:nx, "== 0.3.0"},
      {:ex_doc, "== 0.29.4", only: :dev, runtime: false},
      {:openblas_builder, "~> 0.1.0-dev", github: "zeam-vm/openblas_builder", branch: "main"},
      {:elixir_make, "== 0.7.6", runtime: false},
      {:nimble_parsec, "== 1.3.0"},
      {:dialyxir, "~> 1.3", only: [:dev], runtime: false},
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false}
    ]
  end

  defp compile(_) do
    OpenBLASBuilder.extract_archive!()

    OpenBLASBuilder.compile_matched!([
      {"interface", "cblas_sscal"},
      {"interface", "sscal"},
      {"interface", "cblas_scopy"},
      {"interface", "scopy"},
      {"interface", "cblas_dscal"},
      {"interface", "dscal"},
      {"interface", "cblas_dcopy"},
      {"interface", "dcopy"},
      {"driver/others", "memory"},
      {"driver/others", "blas_l1_thread"},
      {"driver/others", "blas_server"},
      {"driver/others", "parameter"},
      {"driver/others", "openblas_env"},
      {"driver/others", "openblas_error_handle"},
      {"driver/others", "divtable"}
    ])
    |> Map.values()
    |> Enum.join(" ")
    |> then(&System.put_env("OPENBLAS_OBJ", &1))

    {:ok, []}
  end
end
