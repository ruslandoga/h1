defmodule Hx.MixProject do
  use Mix.Project

  def project do
    [
      app: :hx,
      version: "0.1.0",
      elixir: "~> 1.18",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [extra_applications: [:logger]]
  end

  defp deps do
    [
      {:picohttpparser, github: "ruslandoga/picohttpparser"},
      {:plug_cowboy, "~> 2.7", only: [:bench, :test]},
      {:bandit, "~> 1.6", only: [:bench, :test]},
      {:benchee, "~> 1.3", only: :bench},
      {:finch, "~> 0.19.0", only: :test},
      {:stream_data, "~> 1.1", only: :test},
      {:telemetry, "~> 1.0"}
    ]
  end
end
