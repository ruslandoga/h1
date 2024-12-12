defmodule Hx.MixProject do
  use Mix.Project

  def project do
    [
      app: :hx,
      version: "0.1.0",
      elixir: "~> 1.17",
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
      {:picohttpparser, github: "ruslandoga/picohttpparser"},
      {:plug_cowboy, "~> 2.7", only: :bench},
      {:bandit, "~> 1.6", only: :bench},
      {:plug, "~> 1.16"},
      {:finch, "~> 0.19.0", only: :test}
    ]
  end
end
