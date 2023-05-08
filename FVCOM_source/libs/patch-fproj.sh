#!/bin/sh

if [ x$1 = "xIntel" ]; then
    sed -i 's/^DEFS = .*/& -DIFORT/' $2/Makefile
fi
