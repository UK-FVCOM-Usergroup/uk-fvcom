#!/bin/sh

sed -i "
s,\.\./\.\.,$1,; s,\$(INSTALLDIR),$2,
s/^incl.*/#&/
s/(AR)/(AR) rc/
/ *SHELL.*/a \
  RANLIB = ranlib\nFC = ifort
" $3/makefile

mkdir -p $2/include
mkdir -p $2/lib
mkdir -p $2/bin

