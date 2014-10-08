#!/bin/bash

SAVE_FILES="results build-failures run-failures"

MONO_BUILD="`pwd`/mono-build.sh"
MONO_FITNESS="`pwd`/mono-fitness.sh"

if [ ! -x "$MONO_BUILD" -o ! -x "$MONO_FITNESS" ] ; then
    echo "Script must be run with scripts/ as the working directory."
    exit 1
fi

FASTEN_EXE="`pwd`/../Fasten/bin/Debug/Fasten.exe"

if [ ! -f "$FASTEN_EXE" ] ; then
    echo "Fasten executable $FASTEN_EXE not found."
    exit 1
fi

if which timeout >& /dev/null ; then
    TIMEOUT=timeout
elif which gtimeout >& /dev/null ; then
    TIMEOUT=gtimeout
else
    echo "timeout utility not found."
    exit 1
fi

if [ $# -ne 3 ] ; then
    echo "Usage: $0 RUN-NAME MONO-DIR BENCHMARKER-DIR"
    exit 1
fi

RUN_NAME="$1"
MONO_DIR="$2"
BENCHMARKER_DIR="$3"

if [ ! -d "$MONO_DIR/mono/metadata" ] ; then
    echo "Directory $MONO_DIR doesn't seem to be a mono source directory."
    exit 1
fi

if [ ! -d "$BENCHMARKER_DIR/tests" ] ; then
    echo "Directory $BENCHMARKER_DIR doesn't seem to be a benchmarker directory."
    exit 1
fi

pushd "$MONO_DIR/mono"
rm -f $SAVE_FILES

TIMEOUT_UTIL="$TIMEOUT" BENCHMARKER_DIR="$BENCHMARKER_DIR" mono "$FASTEN_EXE" --generations 100 --population 40 --files "(\\.c|\\.h)$" --reset "git checkout ." --build "$MONO_BUILD" --fitness "$MONO_FITNESS" .

popd
for fn in $SAVE_FILES ; do
    mv "$MONO_DIR/mono/$fn" "$RUN_NAME-$fn"
done
