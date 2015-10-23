#!/usr/bin/env bash

# Compile the libraries in serial for the WRF bulk conversion tool.

PACKAGES      = hdf5-1.8.15-patch1 netcdf-4.1.3
INSTALLDIR    = /home_nfs/pica/src/uk-fvcom/FVCOM_source/input/surface_forcing/wrf_bulk_formula/install/
DEF_FLAGS     = -C
COMPILER      = -DIFORT
FC            = ifort
CC            = icc
CPP           = /lib/cpp
CPPFLAGS      = $(DEF_FLAGS) -DINTEL -P
CXXFLAGS      = -O3
CFLAGS        = -O3
FFLAGS        = -O3
LIBS          = -L$(INSTALLDIR)/lib -lnetcdf -I$(INSTALLDIR)/include
EXEC          = wrf_to_fvcom


all:
	mkdir libs || true
	for item in $(PACKAGES); do (tar xf $$item.tgz -Clibs/) || exit 1; done
	cd libs/hdf5-1.8.15-patch1 && ./configure CC=$(CC) CFLAGS=$(CFLAGS) CXX=$(CC) CXXFLAGS=$(CXXFLAGS) F77=$(FC) FFLAGS=$(FFLAGS) --prefix=$(INSTALLDIR)
	cd libs/hdf5-1.8.15-patch1 && make -j install
	cd libs/netcdf-4.1.3 && ./configure CC=$(CC) CFLAGS=$(CFLAGS) CXX=$(CC) CXXFLAGS=$(CXXFLAGS) F77=$(FC) F90=$(FC) FFLAGS=$(FFLAGS) --prefix=$(INSTALLDIR) --build=$(MACHTYPE) CPPFLAGS=-I$(INSTALLDIR)/include LDFLAGS=-L$(INSTALLDIR)/lib
	cd libs/netcdf-4.1.3 && make -j install
	$(FC) $(FFLAGS) $(LIBS) -o $(EXEC)

clean:
	rm -rf libs $(EXEC)

clobber:
	rm -rf libs $(EXEC)
	rm -rf $(INSTALLDIR)/*

