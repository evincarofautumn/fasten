#!/bin/bash
time -p \
    ( \
    /Users/jonathanpurdy/Projects/mono/mono/mini/mono-sgen \
    "$@" \
    ~/Projects/benchmarker/tests/csolden/Health/Health/bin/Debug/Health.exe -l 10 -t 40 \
    > output \
    2> errors \
    ) 2>timing

# /Users/jonathanpurdy/Projects/benchmarker/tests/zorn/lcscbench/lcscbench.exe /Users/jonathanpurdy/Projects/benchmarker/tests/zorn/lcscbench/input1.cs

perl -ne 'if (/real\s+([\d.]+)/) { printf "%f", $1; }' timing
