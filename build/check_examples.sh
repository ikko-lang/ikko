#!/usr/bin/env bash

cd $(dirname "$BASH_SOURCE[0]")/../

for F in examples/*.ik; do
    echo -n "."
    if ! stack exec ikko -- "$F" > /dev/null; then
        echo -e "\n$F failed"
        FAILED=1
    fi
done

for F in examples/errors/*.ik; do
    echo -n "."
    # these *should* fail
    if stack exec ikko -- "$F" 2> /dev/null; then
        echo -e "\n$F should have failed but didn't"
        FAILED=1
    fi
done

if [ -n "$FAILED" ]; then
    echo -e "\nfailures"
    exit 1
fi

echo -e "\nall good"
