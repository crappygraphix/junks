
.PHONY: build
build:
	sass sass/main.scss css/konsoul.css --style compressed

.PHONY: move
move:
	rm -rf ../elixir/assets/static/konsoul
	mkdir -p ../elixir/assets/static/konsoul

	cp ./css/konsoul.css ../elixir/assets/static/konsoul/
	cp ./css/konsoul.css.map ../elixir/assets/static/konsoul/

.PHONY: dist
dist: build move
