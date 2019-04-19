.PHONY: clean
clean:
	rm -rf build

.PHONY: build
build:
	npm run build

.PHONY: test
test:
	npm run test

.PHONY: move
move:
	rm -rf ../elixir/assets/static/react
	mkdir -p ../elixir/assets/static/react
	cp -r build/* ../elixir/assets/static/react/
	mv ../elixir/assets/static/react/index.html ../elixir/lib/junks_web/templates/page/react.html.eex

.PHONY: deploy
deploy:
	rm -rf ../elixir/priv/static/react
	cp -r ../elixir/assets/static/react ../elixir/priv/static/

.PHONY: dist
dist: build move deploy
