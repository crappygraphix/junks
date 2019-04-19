.PHONY: drop-dev-db
drop-dev-db:
	MIX_ENV=test mix ecto.drop
	mix ecto.drop

.PHONY: setup-dev-db
setup-dev-db:
	mix ecto.create
	mix ecto.migrate

.PHONY: reset-dev-db
reset-db: drop-dev-db setup-dev-db

# Deployments

.PHONY: copy-assets
copy-assets:
	sudo cp -r assets/static priv

.PHONY: build
build: copy-assets
	sudo mix deps.get --only prod
	sudo MIX_ENV=prod mix compile
	sudo MIX_ENV=prod mix phx.digest
	sudo MIX_ENV=prod mix release --env=prod

.PHONY: deploy
deploy:
	cd ~/junks-app && \
	sudo tar xfz ~/junks/elixir/_build/prod/rel/junks/releases/0.0.1/junks.tar.gz
	sudo service junks restart

.PHONY: db-reset
db-reset:
	sudo MIX_ENV=prod mix ecto.drop
	sudo MIX_ENV=prod mix ecto.create
	sudo MIX_ENV=prod mix ecto.migrate

db-migrate:
	sudo MIX_ENV=prod mix ecto.migrate
