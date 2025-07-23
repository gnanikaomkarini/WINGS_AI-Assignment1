defmodule Gnanika.MixProject do
  use Mix.Project

  def project do
    [
      app: :gnanika,
      version: "0.1.0",
      elixir: "~> 1.12",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger, :finch],
      mod: {Gnanika.Application, []}
    ]
  end

  defp deps do
    [
      {:finch, "~> 0.16"}
    ]
  end
end
