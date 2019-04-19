.PHONY: clean
clean: rd-clean rw-clean ex-clean

.PHONY: ex-clean
ex-clean:
	rm -rf elixir/_build
	rm -rf elixir/assets/static

# Haskell Actions

.PHONY: rd-clean
rd-clean:
	cd reflex-dom && make clean

.PHONY: rd-build
rd-build:
	cd reflex-dom && make build

.PHONY: rd-dist
rd-dist:
	cd reflex-dom && make dist

# React Actions

.PHONY: rw-clean
rw-clean:
	cd react-web && make clean

.PHONY: rw-build
rw-build:
	cd react-web && make build

.PHONY: rw-dist
rw-dist:
	cd react-web && make dist

.PHONY: rw-test
rw-test:
	cd react-web && make test

# MaterializeCSS

.PHONY: m-dist
m-dist:
	cd materialize && make dist

# Konsoul

.PHONY: k-dist
k-dist:
	cd konsoul && make dist

# Markdown
.PHONY: md-dist
md-dist:
	cd markdown-editor && make dist

# Root Assets
.PHONY: root-dist
root-dist:
	cd root-assets && make dist

# Material Icons
.PHONY: mi-dist
mi-dist:
	cd material-icons && make dist

.PHONY: assets
assets: m-dist k-dist md-dist mi-dist root-dist
	rm -rf elixir/priv/static
	mkdir elixir/priv/static
	cp -r elixir/assets/static elixir/priv

.PHONY: everything
everything: clean rd-dist rw-dist assets
