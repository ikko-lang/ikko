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
	@for F in examples/*.ik; do echo -n "."; stack exec ikko -- $$F > /dev/null || echo "\n$$F failed"; done && echo "\nall good"

.PHONY: run ikko
