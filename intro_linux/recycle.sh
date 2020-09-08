#!/bin/bash

if [ "$#" -lt 1 ]; then
    echo "Usage: $0 <recycle-items>"
    exit 1
fi

RECYCLE_BIN=./recycle_bin

mkdir -p $RECYCLE_BIN

mv "$@" $RECYCLE_BIN