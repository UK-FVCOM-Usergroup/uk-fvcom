# $Id$
# $Name$
# $Revision$
#-----------BEGIN MAKEFILE-------------------------------------------------
            SHELL         = /bin/sh

MYINSTALLDIR = $(INSTALLDIR)

PACKAGES =	proj	fproj	julian	metis	hdf5-1.10.2	netcdf-4.6.1	netcdf-fortran-4.4.4


all:
	for item in $(PACKAGES); do (./untar.sh $$item ) || exit 1; done
	mkdir -p $(MYINSTALLDIR)/bin
	cd proj && ./configure CC=$(CC) CFLAGS=-O3 CXX=$(CC) CXXFLAGS=-O3 F77=$(FC) FFLAGS=-O3 --prefix=$(MYINSTALLDIR) --disable-shared
	cd proj && make -j install
	cd fproj && ./configure CPPFLAGS='$(COMPILER)' CC=$(CC) CFLAGS=-O3 CXX=$(CXX) CXXFLAGS=-O3 FC=$(FC) FFLAGS=-O3 --with-proj4=$(MYINSTALLDIR) --prefix=$(MYINSTALLDIR)
	cd fproj && make install
	cd hdf5-1.10.2 && ./configure CC=$(CC) CFLAGS=-O3 CXX=$(CC) CXXFLAGS=-O3 F77=$(FC) FFLAGS=-O3 --prefix=$(MYINSTALLDIR) 
	cd hdf5-1.10.2 && make install
	cd netcdf-4.6.1 && ./configure CC=$(CC) CFLAGS=-O3 CXX=$(CC) CXXFLAGS=-O3 F77=$(FC) F90=$(FC) FFLAGS=-O3 --prefix=$(MYINSTALLDIR) --build=$(MACHTYPE) CPPFLAGS=-I$(MYINSTALLDIR)/include LDFLAGS=-L$(MYINSTALLDIR)/lib
	cd netcdf-4.6.1 && make install
	cd netcdf-fortran-4.4.4 && ./configure CC=$(CC) FC=$(FC) FFLAGS=-O3 --prefix=$(MYINSTALLDIR) CPPFLAGS=-I$(MYINSTALLDIR)/include LDFLAGS=-L$(MYINSTALLDIR)/lib LIBS="-lhdf5 -lhdf5_hl -lnetcdf" LD_LIBRARY_PATH=$(MYINSTALLDIR)/lib:$(LD_LIBRARY_PATH)
	cd netcdf-fortran-4.4.4 && make install
	cd metis && sed -i 's/log2/ilog2/g' util.c proto.h rename.h
	cd metis && make -j install
	cd julian && make -j install



clean:
	for item in $(PACKAGES); do (cd $$item && make clean) || exit 1; done
	cd netcdf-4.6.1 && make distclean
	cd netcdf-fortran-4.4.4 && make distclean

clobber:
	for item in $(PACKAGES); do (rm -rf $$item) || exit 1; done
	rm -rf $(MYINSTALLDIR)/*

includes::
include ../make.inc
