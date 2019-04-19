NIX_CACHES := --option binary-caches "https://cache.nixos.org https://nixcache.reflex-frp.org"
NIX_CACHE_KEYS := --option binary-cache-public-keys "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
NIX_RUN := nix-shell --run
NIX_GHC := --argstr compiler ghc843
AUDIT_FLAGS := -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints
PROD_OPTS := '-Werror'
AUDIT_OPTS := '-Werror $(AUDIT_FLAGS) -v0' -v0
GHC_PROD_OPTS := --ghc-options=$(PROD_OPTS)
GHC_AUDIT_OPTS := --ghc-options=$(AUDIT_OPTS)
GHCJS_PROD_OPTS := --ghcjs-options=$(PROD_OPTS)
GHCJS_AUDIT_OPTS := --ghcjs-options=$(AUDIT_OPTS)
CABAL_CONFIGURE := cabal new-configure
CABAL_CONFIGURE_GHCJS := cabal new-configure --ghcjs -f frontend
CABAL_BUILD := cabal new-build -v
CABAL_TEST := cabal new-test
CABAL_RUN := cabal new-run
NIX_RUN_GHC = nix-shell -A shells.ghc --run
NIX_RUN_GHCJS = nix-shell -A shells.ghcjs --run
PRJ_FILE_GHC = --project-file=cabal.project
PRJ_FILE_GHCJS = --project-file=cabal-ghcjs.project

BUILD_DIR := ${HOME}/reflex-dist
BUILD_OPTIONS = --builddir=$(BUILD_DIR)

DIST_BUILD_PATH := $(BUILD_DIR)/build/x86_64-linux/ghcjs-0.2.1

.PHONY: clean
clean:
	rm -rf generated
	rm -rf dist
	rm -rf dist-export
	rm -rf dist-newstyle
	rm -rf $(BUILD_DIR)

.PHONY: build-ghc
build-ghc:
	$(NIX_RUN_GHC) "$(CABAL_BUILD) $(PRJ_FILE_GHC) $(BUILD_OPTIONS) $(GHC_PROD_OPTS) $(BUILD_OPTIONS) junks-frontend-exe"

.PHONY: build
build:
	$(NIX_RUN_GHCJS) "$(CABAL_BUILD) $(PRJ_FILE_GHCJS) $(BUILD_OPTIONS) $(GHCJS_PROD_OPTS) $(BUILD_OPTIONS) junks-frontend-exe"

.PHONY: copy
copy:
	./make-helpers/copy-artifacts.sh

.PHONY: compress
compress:
	cd generated/js && rm -f all.js
	# --compress --mangle removed as there were creating errors
	# Specifically "n.codePointAt" is not a function
	cd generated/js && uglifyjs lib.js > lib.min.js
	cd generated/js && uglifyjs out.js > out.min.js
	cd generated/js && uglifyjs rts.js > rts.min.js
	cd generated/js && mv -f lib.min.js lib.js
	cd generated/js && mv -f out.min.js out.js
	cd generated/js && mv -f rts.min.js rts.js
	cd generated/js && gzip -k --best lib.js
	cd generated/js && gzip -k --best out.js
	cd generated/js && gzip -k --best rts.js

.PHONY: prune
prune:
	cd generated/js && rm -f all.js.externs
	cd generated/js && rm -f manifest.webapp
	cd generated/js && rm -f out.stats
	cd generated && rm -f index.html

.PHONY: move
move:
	rm -rf ../elixir/assets/static/reflex-dom
	mkdir -p ../elixir/assets/static/reflex-dom
	cp -r generated/. ../elixir/assets/static/reflex-dom

.PHONY: deploy
deploy:
	rm -rf ../elixir/priv/static/reflex-dom
	cp -r ../elixir/assets/static/reflex-dom ../elixir/priv/static/reflex-dom

.PHONY: dist
dist: build copy compress prune move deploy

.PHONY: debug-js-dist
debug-js-dist: build copy prune move deploy
