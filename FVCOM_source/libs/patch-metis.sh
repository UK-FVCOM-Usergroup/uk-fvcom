#!/bin/sh

sed -i "
s,\.\./\.\.,$1,; s,\$(INSTALLDIR),$2,
s/^incl.*/#&/
s/(AR)/(AR) rc/
/ *SHELL.*/a \RANLIB = ranlib
" $3/makefile

mkdir -p $2/include
mkdir -p $2/lib
mkdir -p $2/bin

(cd $3; patch < $1/libs/metis-4.0.patch)
