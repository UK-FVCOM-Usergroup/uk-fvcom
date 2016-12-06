#!/usr/bin/env bash
#
# Convert WRF output to HEATING_ON input for FVCOM. Give WRF output files as
# arguments to the script:
#
# $ ./convert.sh <file1.nc> <file2.nc> ... <filen.nc>
#
# Set the casename variable to match that of your project.
#
# Pierre Cazenave (Plymouth Marine Laboratory) 2015-11-10

set -eu

LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$(readlink -f ./install/lib)

casename=aqua_v05

for i in "$@"; do
    # All files have to be in the current directory (wrf_to_fvcom seems to
    # ignore paths in the input and output files). The fix for that is for
    # another day.

    # Clear out current symlinks and replace with the current file to process.
    if [ -h $(basename "$i") ]; then
        rm $(basename "$i")
    fi
    ln -s "$i"

    year=$(echo $i | cut -f1 -d- | rev | cut -f1 -d_ | rev)
    month=$(echo $i | cut -f2 -d-)
    mkdir -p output/$year/$month || true

    ./wrf_to_fvcom -i $(basename "$i") -o "${casename}_wnd_wrf.nc" -hindcast -latitude 50.

    mv "${casename}_wnd_wrf.nc" output/$year/$month
    if [ -h $(basename "$i") ]; then
        rm ./$(basename "$i")
    fi
done

