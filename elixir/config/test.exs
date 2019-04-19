use Mix.Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :junks, JunksWeb.Endpoint,
  http: [port: 4001],
  server: false

# Print only warnings and errors during test
config :logger, level: :warn

# Configure your database
config :junks, Junks.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: "postgres",
  password: "postgres",
  database: "junks_test",
  hostname: "localhost",
  pool: Ecto.Adapters.SQL.Sandbox

config :junks, Junks.Mailer,
  adapter: Swoosh.Adapters.Test
