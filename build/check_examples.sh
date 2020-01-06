#!/usr/bin/env bash

cd $(dirname "$BASH_SOURCE[0]")/../

for F in examples/*.ik; do
    echo -n "."
    if ! stack exec ikko -- "$F" > /dev/null; then
        echo -e "\n$F failed"
        FAILED=1
    fi
done

if [ -n "$FAILED" ]; then
    echo -e "\nfailures"
    exit 1
fi

echo -e "\nall good"
