#!/bin/bash

if make ; then
    true
else
    echo ---------------------------------------- >>build-failures
    git --no-pager diff >>build-failures
    exit 1
fi
