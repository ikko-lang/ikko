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

run: ikko
	stack exec ikko -- examples/fib.ik

tags:
	hasktags -e src

check_examples: ikko
	@./build/check_examples.sh

.PHONY: run ikko
