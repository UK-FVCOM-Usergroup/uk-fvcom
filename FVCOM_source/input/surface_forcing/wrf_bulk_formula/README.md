# Compiling the FVCOM WRF bulk formula code

0. Compile the main FVCOM source code.
1. Run `get_libs.sh`.
2. Edit the `makefile`, changing `FVCOM` to the root of the FVCOM source code.
3. Edit the `makefile` to reflect your compiler definitions.
4. Type `make libs` to compile the HDF5 and netCDF4 libraries. If you prefer to use your own versions, edit the paths in the relevant makefile to reflect the location of those libraries. See the `makefile.ARCHER` for an example of how to do this.

# Running the resulting code.

1. From a terminal, run the binary:

```
LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$(readlink -f ./install/lib) \
                ./wrf_to_fvcom -i /path/to/your/wrf/output.nc \
                -o /path/to/your/fvcom/input/forcing.nc \
                -hindcast -latitude 50.0
```

Adjust `-hindcast` to `-forecast` as appropriate. The output of `-help` is:

```
 ================================================
 wrf2fvcom version 0.14 (2015-10-26) (Bulk method: COARE 2.6SN)

  wrf2fvcom -i wrf_netcdf_input_file -o fvcom_forcing_file \
  -forecast -s surface_meterological_variables_file

  Current options available are:
  -help     : Print this information
  -h        : Print this information
  -i        : WRF input file (netcdf format)
  -o        : forcing output name - default is ARWout
  -s        : Extract and save necessary fields for backup
  -debug    : Print some debug information
  -forecast : flag for forecast data
  -hindcast : flag for hindcast data
  -noglobal : do not save global attributes
  -latitude : the heat flux calculation latitude (+42N by default)
```

# Batch conversion

For converting a lot of files, I created a simple batch script to do that called `convert.sh`. Run it as:

```
 $ ./convert.sh "wrfout_d03_2003-01-01_00:00:00" "wrfout_d03_2003-02-01_00:00:00" ... "wrfout_d03_2003-12-01_00:00:00"
```

The converted files are put in the output/$year/$month directory corresponding to each WRF output file (the year and month are taken from the WRF file names).
