
.PHONY: build
build:
	sass sass/materialize.scss css/junks.css --style compressed

.PHONY: move
move:
	rm -rf ../elixir/assets/static/materialize
	mkdir -p ../elixir/assets/static/materialize

	cp ./css/junks.css ../elixir/assets/static/materialize/
	cp ./css/junks.css.map ../elixir/assets/static/materialize/
	cp ./js/bin/materialize.min.js ../elixir/assets/static/materialize/
	cp ./js/bin/jquery.min.js ../elixir/assets/static/materialize/

.PHONY: dist
dist: build move
