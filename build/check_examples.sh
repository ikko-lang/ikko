#!/usr/bin/env bash

cd $(dirname "$BASH_SOURCE[0]")/../

BIN=".stack-work/install/x86_64-linux/157bf6244c9792b25fd59b58e6e4e87ef65813a154039f39d8a959445a467a47/8.6.5/bin/ikko"

for F in examples/*.ik examples/projecteuler/*.ik examples/generated/*.ik; do
    echo -n "."
    if ! $BIN "$F" > /dev/null; then
        echo -e "\n$F failed"
        FAILED=1
    fi
done

for F in examples/errors/*.ik; do
    echo -n "."
    # these *should* fail
    if $BIN "$F" 2> /dev/null; then
        echo -e "\n$F should have failed but didn't"
        FAILED=1
    fi
done

if [ -n "$FAILED" ]; then
    echo -e "\nfailures"
    exit 1
fi

echo -e "\nall good"
