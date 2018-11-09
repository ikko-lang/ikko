ikko: Makefile ikko.cabal stack.yaml src/*.hs
	stack build

lint:
	hlint src/ --ignore "Reduce duplication"

test: lint
	stack test

test-inference:
	stack test ikko:test-infer

test-parser:
	stack test ikko:test-parser

test-first-pass:
	stack test ikko:test-first-pass

test-graph:
	stack test ikko:test-graph

run:
	stack build
	stack exec ikko -- examples/fib.at

tags:
	hasktags -e src

.PHONY: run ikko
