#!/bin/bash
time -p \
    ( \
    /Users/jonathanpurdy/Projects/mono/mono/mini/mono-sgen \
    ~/Projects/benchmarker/tests/csolden/Health/Health/bin/Debug/Health.exe \
    -l 10 -t 40 \
    > /dev/null \
    ) 2>timing

perl -ne 'if (/real\s+([\d.]+)/) { printf "%f", $1; }' timing
