HS_FILES = $(shell git ls-files '*.hs' | grep -v 'vendored/')
CABAL_FILES = $(shell git ls-files '*.cabal' | grep -v 'vendored/')

.PHONY: ghcid
ghcid:
	ghcid -c "cabal repl build-system" -l=hlint

.PHONY: run
run:
	cabal run build-system:exe:build-system-exe

.PHONY: update
update:
	cabal update

.PHONY: build
build:
	cabal build all -j4

.PHONY: freeze
freeze:
	cabal freeze --enable-tests --enable-benchmarks

.PHONY: format
format:
	@ormolu --mode inplace $(HS_FILES) && echo "Ormolu success!"

.PHONY: hlint
hlint:
	@hlint $(HS_FILES)

.PHONY: format-cabal
format-cabal:
	@cabal-fmt -i $(CABAL_FILES)
