#!/bin/bash
AXOPATCH=$HOME/git/AxoStudio/patch/
cp -av xpatch.cpp $AXOPATCH/
make -C $AXOPATCH
