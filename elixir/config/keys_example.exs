# This file is excluded from Source Control!!!
# It contains configurations related to API Keys.

use Mix.Config

config :junks, Junks.Mailer,
  api_key: "my-api-key"

config :junks, JunksWeb.Endpoint,
  secret_key_base: "my-long-random-secret"
