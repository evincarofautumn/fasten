#!/bin/sh
mono ../Fasten/bin/Debug/Fasten.exe --files continuous-function.c --reset "git checkout ." --build make --fitness continuous-function .
