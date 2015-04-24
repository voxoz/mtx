defmodule MTX.Mixfile do
  use Mix.Project

  def project do
    [app: :mtx,
     version: "1.0.0",
     description: "Metrics Client",
     package: package]
  end

  defp package do
    [files: ~w(c_src include priv src LICENSE package.exs README.md rebar.config),
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/synrc/mtx"}]
   end
end
