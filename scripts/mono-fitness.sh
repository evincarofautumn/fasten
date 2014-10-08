#!/bin/bash

#    /Users/schani/Work/mono/benchmarker/tests/csolden/Health/Health/bin/Debug/Health.exe -l 10 -t 60 \
#    /Users/schani/Work/mono/benchmarker/tests/binarytrees/binarytrees.exe 18 \

(time -p \
    "$TIMEOUT_UTIL" 5m \
    ./mini/mono-sgen \
    "$@" \
    "$BENCHMARKER_DIR/tests/csolden/Perimeter/Perimeter/bin/Debug/Perimeter.exe" -l 17 \
    ) >/dev/null 2>timing
RESULT=$?

if [ $RESULT -eq 0 ] ; then
    TIME=`perl -ne 'if (/real\s+([\d.]+)/) { printf "%f", $1; }' timing`
    echo -------------------------------------------- >>results
    git --no-pager diff >>results
    echo ----------------------- TIME $TIME >>results
    echo $TIME
else
    echo -------------------------------------------- >>run-failures
    git --no-pager diff >>run-failures
    exit 1
fi

# ~/Projects/benchmarker/tests/csolden/Health/Health/bin/Debug/Health.exe -l 10 -t 40
# /Users/jonathanpurdy/Projects/benchmarker/tests/zorn/lcscbench/lcscbench.exe /Users/jonathanpurdy/Projects/benchmarker/tests/zorn/lcscbench/input1.cs
