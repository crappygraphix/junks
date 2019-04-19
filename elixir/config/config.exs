# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# General application configuration
config :junks,
  ecto_repos: [Junks.Repo],
  frontend: "reflex-dom"
  # frontend: "elm"

config :junks, Junks.Mailer,
  adapter: Swoosh.Adapters.Mailgun,
  domain: "mg.junks.info"

# Configures the endpoint
config :junks, JunksWeb.Endpoint,
  url: [host: "172.28.128.200"],
  render_errors: [view: JunksWeb.ErrorView, accepts: ~w(html json)],
  pubsub: [name: Junks.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:user_id]

# Import API Key related configs
import_config "keys.exs"

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
