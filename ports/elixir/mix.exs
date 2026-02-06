defmodule SpriteTool.MixProject do
  use Mix.Project

  def project do
    [
      app: :sprite_tool,
      version: "0.1.0",
      elixir: "~> 1.15",
      escript: [main_module: SpriteTool.CLI],
      deps: deps()
    ]
  end

  def application do
    [extra_applications: [:logger]]
  end

  defp deps do
    [
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false}
    ]
  end
end
