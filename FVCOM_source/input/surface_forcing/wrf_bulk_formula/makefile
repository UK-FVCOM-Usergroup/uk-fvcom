#!/usr/bin/env bash

# Compile the libraries in serial for the WRF bulk conversion tool.

PACKAGES      = hdf5-1.8.15-patch1 netcdf-4.1.3
FVCOM         = $(HOME)/Code/uk-fvcom/FVCOM_source
FVCOMLIBS     = $(FVCOM)/libs/install/
INSTALLDIR    = $(FVCOM)/input/surface_forcing/wrf_bulk_formula/install/
# for mod_time stuff.
MODS          = $(FVCOM)
DEF_FLAGS     = -C
COMPILER      = -DIFORT
FC            = ifort
CC            = icc
CPP           = /lib/cpp
CPPFLAGS      = $(DEF_FLAGS) -DINTEL -P
CXXFLAGS      = -O3
CFLAGS        = -O3
FFLAGS        = -O3
LIBS          = -L$(INSTALLDIR)/lib -lnetcdf -lnetcdff -I$(INSTALLDIR)/include
EXEC          = wrf_to_fvcom

all:
	$(FC) -I$(MODS) $(MODS)/mod_time.o $(EXEC).f90 $(FFLAGS) $(LIBS) -o $(EXEC)

libs:
	mkdir libs || true
	for item in $(PACKAGES); do (tar xf $$item.tar.gz -Clibs/) || exit 7; done
	cd libs/hdf5-1.8.15-patch1 && ./configure CC=$(CC) CFLAGS=$(CFLAGS) CXX=$(CC) CXXFLAGS=$(CXXFLAGS) F77=$(FC) FFLAGS=$(FFLAGS) --prefix=$(INSTALLDIR)
	cd libs/hdf5-1.8.15-patch1 && make -j install
	cd libs/netcdf-4.1.3 && ./configure CC=$(CC) CFLAGS=$(CFLAGS) CXX=$(CC) CXXFLAGS=$(CXXFLAGS) F77=$(FC) F90=$(FC) FFLAGS=$(FFLAGS) --prefix=$(INSTALLDIR) --build=$(MACHTYPE) CPPFLAGS=-I$(INSTALLDIR)/include LDFLAGS=-L$(INSTALLDIR)/lib
	cd libs/netcdf-4.1.3 && make -j install

clean:
	rm -rf libs $(EXEC)

clobber:
	rm -rf libs $(EXEC)
	rm -rf $(INSTALLDIR)*

