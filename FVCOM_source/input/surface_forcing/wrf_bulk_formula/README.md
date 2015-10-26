# Compiling the FVCOM WRF bulk formula code

1. Run `get_libs.sh`.
2. Edit the `makefile`, changing `INSTALLDIR` to the current directory.
3. Edit the `makefile` to reflect your compiler definitions.
4. Type `make`.

# Running the resulting code.

1. From a terminal, run the binary:

```
LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$(readlink -f ./install/lib) \
                ./wrf_to_fvcom -i /path/to/your/wrf/output.nc \
                -o /path/to/your/fvcom/input/forcing.nc \
                -hindcast
```

Adjust `-hindcast` to `-forecast` as appropriate. The output of `-help` is:

```
 ================================================
 wrf2fvcom version 0.13 (2007-06-27) (Bulk method: COARE 2.6SN)

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
  -noglobal : don't save global attributes
```
