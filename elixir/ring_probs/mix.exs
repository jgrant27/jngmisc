defmodule RingProbs.Mixfile do
  use Mix.Project

  def project do
    [app: :ring_probs,
     version: "0.1.0",
     elixir: "> 0.13.2",
     escript_main_module: RingProbsAlt,
     escript_embed_elixir: true,
     deps: deps]
  end

  # Configuration for the OTP application
  #
  # Type `mix help compile.app` for more information
  def application do
    [applications: [],
     mod: {RingProbsAlt, []}]
  end

  # Dependencies can be hex.pm packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1"}
  #
  # Type `mix help deps` for more examples and options
  defp deps() do
    []
  end
end
