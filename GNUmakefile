all:
test:
.PHONY: all test

all: dist/build/ngram/ngram
test: dist/build/ngram/ngram
	$< < COPYING

dist/setup-config: Setup.lhs ngram.cabal
	runhaskell $< configure --user
dist/build/ngram/ngram: Setup.lhs dist/setup-config $(wildcard *.hs *.x)
	runhaskell $< build
