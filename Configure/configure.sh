#!/bin/bash
#Song Qian 2011-10-20
#2011-10-23 test Linux x86_64 intel (ifort icc  mpif90 mpicc)
#                Linux i686   intel (ifort icc  mpif90 mpicc)
argc=$1
os=`uname`;
mach=`uname -m`;

if [[ $1 == "series" ]]
then
for cc in icc cc; do
find_cc=`which $cc`
if test -n "$find_cc";
then
  clabel=$cc;
  break;
fi
done

for fc in ifort f90 pgf90 gfortran xlf90 ; do
find_fc=`which $fc`
if test -n "$find_fc";
then
  flabel=$fc;
  break;
fi
done
echo "$os"_"$mach"_"$clabel"_"$flabel"

elif [[ $1 == "parallel" ]]
then
for cc in mpicc; do
find_cc=`which $cc`
if test -n "$find_cc";
then
  clabel=$cc;
  break;
fi
done

for fc in mpif90 mpxlf90 ; do
find_fc=`which $fc`
if test -n "$find_fc";
then
  flabel=$fc;
  break;
fi
done
echo "$os"_"$mach"_"$clabel"_"$flabel"

else 
echo "Argument needed. You should type "
echo "./configure.sh series" 
echo "or"
echo "./configure.sh parallel"
fi


if test -f ./arch/"$os"_"$mach"_"$clabel"_"$flabel"
then
cat ./arch/make.inc_preamble > make.inc
cat ./arch/"$os"_"$mach"_"$clabel"_"$flabel" >> make.inc
cat ./arch/make.inc_afteramble >> make.inc
mv make.inc ../FVCOM_source/.
else
echo "Error: this configuration is not exist."
exit
fi

