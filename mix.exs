defmodule Haproxy_check.Mixfile do
  use Mix.Project

  def project do
    [app: :haproxy_check,
     version: "0.0.2",
     language: :erlang,
     escript: escript,
     deps: deps]
  end

#  def application do
#    [applications: [], mod: {:haproxy, []}]
#  end

  defp escript do
    [ main_module: :haproxy_check_cli,
      embedd_elixir: false,
      shebang: "#! /usr/bin/env escript\n"]
  end

  defp deps do
    [
      {:mix_erlang_tasks, "0.1.0"},
      {:getopt, "0.8.2"},
    ]
  end
end
