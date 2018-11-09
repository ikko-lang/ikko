#!/usr/bin/env bash

cd $(dirname "${BASH_SOURCE[0]}")

set -e

LIMIT=${LIMIT:-1000}
COMPILER=../.stack-work/dist/x86_64-linux/Cabal-2.2.0.1/build/ikko/ikko
TMP_FILE=$(mktemp)

echo "Using temp file $TMP_FILE"

for I in $(seq 1 $LIMIT); do
    python3 gencode.py > $TMP_FILE
    RESULT=$($COMPILER $TMP_FILE) || (echo $I; cat $TMP_FILE; exit 1)
    if [ "$RESULT" != "42" ]; then
        echo "Got '$RESULT' for the input"
        cat $TMP_FILE
        echo
    fi
done
