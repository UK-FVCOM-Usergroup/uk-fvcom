#-----------BEGIN MAKEFILE-------------------------------------------------
            SHELL         = /bin/sh
            DEF_FLAGS     = -P -C -traditional 
            EXEC          = rca_eutro

#==========================================================================
#  BEGIN USER DEFINITION SECTION
#==========================================================================
#        SELECT MODEL OPTIONS
#          SELECT FROM THE FOLLOWING OPTIONS BEFORE COMPILING CODE
#          SELECT/UNSELECT BY COMMENTING/UNCOMMENTING LINE (#)
#--------------------------------------------------------------------------

# For em64t computers at SMAST (tuna)
#             IOLIBS       =  -L/hosts/mao/usr/medm/install/netcdf/3.6.3/em64t/lib  -lnetcdf
#             IOINCS       =  -I/hosts/mao/usr/medm/install/netcdf/3.6.3/em64t/include

# For em64t computers at SMAST (guppy)
#             IOLIBS       =  -L/hosts/salmon01/data00/medm/em64t/lib64  -lnetcdf
#             IOINCS       =  -I/hosts/salmon01/data00/medm/em64t/include64

# For i386 computers at SMAST (salmon and minke)
#	      IOLIBS       =  -L/hosts/mao/usr/medm/install/netcdf/3.6.3/ia32/lib  -lnetcdf
#             IOINCS       =  -I/hosts/mao/usr/medm/install/netcdf/3.6.3/ia32/include

# For Typhoeus and Hydra
#              IOLIBS       =  -L/usr/share/netcdf/lib  -lnetcdf
#              IOINCS       =  -I/usr/share/netcdf/include

         IOLIBS    = -L/usr/local/install/netcdf-3.6.2/gcc-4.4.7_ifort-15.0.3/lib64 -lnetcdff -lnetcdf
         IOINCS     = -I/usr/local/install/netcdf-3.6.2/gcc-4.4.7_ifort-15.0.3/include
#         IOLIBS    = -L/usr/local/install/netcdf-3.6.2/gcc-4.4.7-16_ifort-14.0.2.144/lib64 -lnetcdff -lnetcdf
#         IOINCS     = -I/usr/local/install/netcdf-3.6.2/gcc-4.4.7-16_ifort-14.0.2.144/include
#         IOLIBS    = -L/usr/local/install/netcdf-3.6.2/gcc-4.4.7-16_ifort-16.0.0.109/lib64 -lnetcdff
#         IOINCS     = -I/usr/local/install/netcdf-3.6.2/gcc-4.4.7-16_ifort-16.0.0.109/include

#--------------------------------------------------------------------------
#        MULTI_PROCESSOR    INCLUDES PARALLELIZATION WITH MPI
#                           REQUIRES LINKING MPI LIBRARIES OR COMPILING
#                           WITH A PRELINKED SCRIPT (mpif90/mpf90/etc)
#                           DEFAULT: NO PARALLEL CAPABILITY
#                           UNCOMMENT TO INCLUDE MPI PARALLEL CAPABILITY
#--------------------------------------------------------------------------

              FLAG_1 = -DMULTIPROCESSOR

# For em64t computers at SMAST (guppy)
#              PARLIB = -L/hosts/salmon01/data00/medm/em64t/lib64 -lmetis 

# For i386 computers at SMAST (salmon and minke)
#              PARLIB = -L/hosts/salmon01/data00/medm/lib -lmetis 
#              PARLIB = -L../METIS_source -lmetis 

# For Typhoeus and Hydra
              PARLIB = -L/usr/local/install/metis/gcc-4.4.7/lib64 -lmetis 


#--------------------------------------------------------------------------
#        SPHERICAL          SELECT SPHERICAL COORDINATES FOR INTEGRATION
#                           DEFAULT: CARTESIAN
#                           UNCOMMENT TO SELECT SPHERICAL COORDINATES
#--------------------------------------------------------------------------

#             FLAG_2 = -DSPHERICAL

#--------------------------------------------------------------------------
#        FLOODYING/DRYING   INCLUDE WET/DRY TREATMENT OF DOMAIN
#                           CAN BE ACTIVATED/DEACTIVATED AT RUN TIME WITH
#                           INPUT FILE CONTROL.  (SEE exa_run.dat) FILE
#                           DEFAULT: NO FLOODYING/DRYING INCLUDED
#                           UNCOMMENT TO INCLUDE FLOODYING/DRYING
#--------------------------------------------------------------------------

#             FLAG_3 = -DWET_DRY

#--------------------------------------------------------------------------
#        SELECT COMPILER/PLATFORM SPECIFIC DEFINITIONS
#          SELECT FROM THE FOLLOWING PLATFORMS OR USE "OTHER" TO DEFINE
#          THE FOLLOWING VARIABLES:
#          CPP:  PATH TO C PREPROCESSOR 
#           FC:  PATH TO FORTRAN COMPILER (OR MPI COMPILE SCRIPT)
#          OPT:  COMPILER OPTIONS
#       MPILIB:  PATH TO MPI LIBRARIES (IF NOT LINKED THROUGH COMPILE SCRIPT)
#--------------------------------------------------------------------------

#--------------------------------------------------------------------------
#        COMPAQ/ALPHA Definitions
#--------------------------------------------------------------------------
#lncompaqCPPFLAGS   = $(DEF_FLAGS)  -DCOMPAQ
#lncompaqCPP        = /bin/cpp
#lncompaqFC         = f90
#lncompaqDEBFLGS    =  # -check bounds -check overflow -g 
#lncompaqOPT        =  -fast -arch ev6 -fpe1
#--------------------------------------------------------------------------
#  CRAY Definitions         
#--------------------------------------------------------------------------
#lncray   CPPFLAGS = $(DEF_FLAGS) -DCRAY 
#lncray   CPP      = /opt/ctl/bin/cpp
#lncray   FC       = f90
#lncray   DEBFLGS  =  
#lncray   OPT      =
#--------------------------------------------------------------------------
#   Linux/Portland Group Definitions 
#--------------------------------------------------------------------------
#lnportlandCPP      = /usr/bin/cpp
#lnportlandCPPFLAGS = $(DEF_FLAGS) 
#lnportlandFC       = pgf90
#lnportlandDEBFLGS  = -Mbounds -g -Mprof=func
#lnportlandOPT      = #-fast  -Mvect=assoc,cachesize:512000,sse  
#--------------------------------------------------------------------------
#  Intel Compiler Definitions
#--------------------------------------------------------------------------
# CPP      = /usr/bin/cpp
# CPPFLAGS = $(DEF_FLAGS) -DINTEL 
# FC       = ifort
# DEBFLGS  = -check all
# Use 'OPT = -O0 -g' for fast compile to test the make 
# Use 'OPT = -xP' for fast run on em64t (Hydra, Typhoeus and Guppy)
# Use 'OPT = -xN' for fast run on em64t (Hydra, Typhoeus and Guppy)
#lnintel OPT      = -O0 -g  
#         OPT      = -xP 

# Do not set static for use with visit!
#         VISOPT   = -Wl,--export-dynamic
#         LDFLAGS  = $(VISITLIBPATH)
#--------------------------------------------------------------------------
#  Intel/MPI Compiler Definitions (SMAST)      
#--------------------------------------------------------------------------
CPP      = /usr/bin/cpp
CPPFLAGS = $(DEF_FLAGS) -DINTEL   
FC       = mpif90
DEBFLGS  = #-check all
#DEBFLGS  = 
# Use 'OPT = -O0 -g'  for fast compile to test the make 
# Use 'OPT = -xP'      for fast run on em64t (Hydra, Typhoeus and Guppy)
# Use 'OPT = -xN'      for fast run on em64t (Salmon and Minke)
#OPT      = #-O0 -g 
#          OPT      = -xP
          OPT      = -O3
#          OPT      = -xN

# Do not set static for use with visit!
#         VISOPT   = -Wl,--export-dynamic
#         LDFLAGS  = $(VISITLIBPATH)
#--------------------------------------------------------------------------
#  gfortran defs 
#--------------------------------------------------------------------------
#         CPP      = /usr/bin/cpp 
#         CPPFLAGS = $(DEF_FLAGS)  -DGFORTRAN
#         FC       = gfortran  -O3 
#         DEBFLGS  = 
#         OPT      = 
#         CLIB     = 
#--------------------------------------------------------------------------
#  absoft / mac os x defs
#--------------------------------------------------------------------------
#         CPP      = /usr/bin/cpp 
#         CPPFLAGS = $(DEF_FLAGS)  -DABSOFT  
#         FC       = f90  -O3  -lU77
#         DEBFLGS  = 
#         OPT      = 
#         CLIB     = 
#--------------------------------------------------------------------------
#  IBM/AIX Definitions
#--------------------------------------------------------------------------
#         CPPFLAGS = $(DEF_FLAGS) -DAIX 
#         CPP      = /usr/local/bin/cpp
#         FC       = mpxlf90 -qsuffix=f=f90
#         DEBFLGS  =  # -qcheck -C -g
#         OPT      = -O -qarch=pwr4 -qtune=pwr4  -bmaxdata:0x80000000 -qhot -qmaxmem=8096
#--------------------------------------------------------------------------
#  APPLE OS X/XLF Definitions (G5)
#--------------------------------------------------------------------------
#         CPPFLAGS = $(DEF_FLAGS) -DAIX
#         CPP      = /usr/bin/cpp
#         FC       =  /opt/ibmcmp/xlf/8.1/bin/xlf90 -qsuffix=f=f90
#         DEBFLGS  =  # -qcheck -C -g
#         OPT      = -O5 -qarch=g5 -qtune=g5  -qhot -qmaxmem=8096 -qunroll=yes -Wl,-stack_size,10000000
#--------------------------------------------------------------------------
#  OTHER                   
#--------------------------------------------------------------------------
#         CPPFLAGS = $(DEF_FLAGS)
#         CPP      = 
#         FC       = 
#         DEBFLGS  = 
#         OPT      = 
#==========================================================================
#  END USER DEFINITION SECTION
#==========================================================================

         FFLAGS = $(DEBFLGS) $(OPT)
         MDEPFLAGS = --cpp --fext=f90 --file=-
         RANLIB = ranlib

#--------------------------------------------------------------------------
#  CAT Preprocessing Flags
#--------------------------------------------------------------------------
           CPPARGS =  $(CPPFLAGS) $(DEF_FLAGS) $(FLAG_1) $(FLAG_2) $(FLAG_3) 
#--------------------------------------------------------------------------
#  Libraries           
#--------------------------------------------------------------------------

            LIBS  =	$(CLIB) $(PARLIB) $(IOLIBS) $(MPILIB) 

            INCS  = 	$(IOINCS) 

#--------------------------------------------------------------------------
#  Preprocessing and Compilation Directives
#--------------------------------------------------------------------------
.SUFFIXES: .o .f90 .F  

.F.o:
	$(CPP) $(CPPARGS) $(INCS) $*.F > $*.f90
	$(FC)  -c $(FFLAGS) $(INCS) $*.f90 
	\rm $*.f90

#--------------------------------------------------------------------------
#  RCA Source Code.
#--------------------------------------------------------------------------

# Note: Change the order of modules at your own risk!

MAIN =  mod_prec.F mod_utils.F mod_types.F mod_var.F mod_rca.F mod_sed.F \
        mod_par.F mod_ncd.F ncdio.F \
        utilities.F tge.F domdec.F genmap.F pdomdec.F allocate_rca.F allocate_sed.F \
        cell_area.F viscofh.F iunitcheck.F \
        rca.F rca01.F rca02.F rca03.F rca04.F rca05.F rca06.F \
        rca07.F rca08.F rca09.F rca10.F rca11.F rca12.F \
        rca13.F rcabyss.F rcadwrit.F eutro.F\
        rcaadv.F rcamess.F rcamprof.F \
        rcaprnt.F sedmodlsubs.F \
        sinter.F stoich.F vssedmodl.F \
	fmter.F vdif.F vertvl.F fct_s.F print_vals.F

 SRCS = $(MAIN)

# object list is composed of free (*.F) and fixed (*.F90) source
 OBJS = $(SRCS:.F=.o) $(FIXED:.F90=.o)


#--------------------------------------------------------------------------
#  Linking Directives
#--------------------------------------------------------------------------

$(EXEC):	$(OBJS)
		$(FC) $(FFLAGS) $(VISOPT) $(LDFLAGS) $(OBJS) $(LIBS) -o $(EXEC)


#--------------------------------------------------------------------------
#  Target to create dependecies.
#--------------------------------------------------------------------------

depend:
		makedepf90  $(SRCS) >> makedepends


#--------------------------------------------------------------------------
#  Tar Up Code                           
#--------------------------------------------------------------------------

#JQItarfile:
#JQI	tar cvf fvcom.tar *.F *.F90  makefile exa_run.dat makedepends RELEASE_NOTES 

#--------------------------------------------------------------------------
#  Cleaning targets.
#--------------------------------------------------------------------------

clean:
		/bin/rm -f *.o *.mod

clobber:	clean
		/bin/rm -f *.f90 *.o rca_eutro

#--------------------------------------------------------------------------
#  Common rules for all Makefiles - do not edit.
#--------------------------------------------------------------------------

emptyrule::

#--------------------------------------------------------------------------
#  Empty rules for directories that do not have SUBDIRS - do not edit.
#--------------------------------------------------------------------------

install::
	@echo "install in $(CURRENT_DIR) done"

install.man::
	@echo "install.man in $(CURRENT_DIR) done"

Makefiles::

includes::
include ./makedepends
