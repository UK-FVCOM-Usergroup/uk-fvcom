#!/bin/csh

# if not set in the environment point to the code here
setenv GOTM_BASE /data00/jqi/AFINAL/FVCOM4.0/GOTM/code/
#export FABM_BASE=~/FABM/code

set compiler=ifort
mkdir -p $compiler
cd $compiler
rm -rf *
cmake $GOTM_BASE/src \
      -DGOTM_USE_FABM=OFF \
      -DCMAKE_Fortran_COMPILER=$compiler \
      -DCMAKE_INSTALL_PREFIX=/data00/jqi/AFINAL/FVCOM4.0/GOTM/install/$compiler

make install
cd ..
