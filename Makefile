.PHONY: clean
clean:
	cabal clean

.PHONY: build
build:
	cabal build --enable-tests --write-ghc-environment-files=always

.PHONY: test-lecture1
test-lecture1:
	cabal test doctest-lecture1 --enable-tests --test-show-details=direct
	cabal run exercises-test --enable-tests -- -m "Lecture1"

.PHONY: test-all
test-all:
	cabal test all --enable-tests --test-show-details=direct
