#-----------BEGIN MAKEFILE-------------------------------------------------
            SHELL         = /bin/sh
            DEF_FLAGS     = -P -C -traditional 
            EXEC          = fvcom_gem
#==========================================================================
#  BEGIN USER DEFINITION SECTION
#==========================================================================
#        SELECT MODEL OPTIONS
#          SELECT FROM THE FOLLOWING OPTIONS BEFORE COMPILING CODE
#          SELECT/UNSELECT BY COMMENTING/UNCOMMENTING LINE (#)
#          CODE MUST BE CLEANED (with "make clean") AND
#          RECOMPILED IF NEW SET OF OPTIONS IS DESIRED
#--------------------------------------------------------------------------

#--------------------------------------------------------------------------
#        PRECISION          DEFAULT PRECISION: SINGLE                     
#                           UNCOMMENT TO SELECT DOUBLE PRECISION
#--------------------------------------------------------------------------

#            FLAG_1 =  -DDOUBLE_PRECISION 

#--------------------------------------------------------------------------
#        SPHERICAL          SELECT SPHERICAL COORDINATES FOR INTEGRATION
#                           DEFAULT: CARTESIAN
#                           UNCOMMENT TO SELECT SPHERICAL COORDINATES
#--------------------------------------------------------------------------

#             FLAG_2 = -DSPHERICAL

#--------------------------------------------------------------------------
#        MULTI_PROCESSOR    INCLUDES PARALLELIZATION WITH MPI
#                           REQUIRES LINKING MPI LIBRARIES OR COMPILING
#                           WITH A PRELINKED SCRIPT (mpif90/mpf90/etc)
#                           DEFAULT: NO PARALLEL CAPABILITY
#                           UNCOMMENT TO INCLUDE MPI PARALLEL CAPABILITY
#--------------------------------------------------------------------------

              FLAG_3 = -DMULTIPROCESSOR

# For em64t computers at SMAST (guppy)
#              PARLIB = -L/hosts/salmon01/data00/medm/em64t/lib64 -lmetis 

# For i386 computers at SMAST (salmon and minke)
#              PARLIB = -L/hosts/salmon01/data00/medm/lib -lmetis 
              PARLIB = -L./METIS_source -lmetis 

# For Typhoeus and Hydra
#              PARLIB = -L/usr/local/lib -lmetis 



#--------------------------------------------------------------------------
#        NETCDF OUTPUT      DUMP OUTPUT INTO NETCDF FILES (yes/no)
#                           REQUIRES SYSTEM DEPENDENT NETCDF LIBRARIES
#                           COMPILED WITH SAME F90 COMPILER
#                           SET PATH TO LIBRARIES WITH IOLIBS      
#                           SET PATH TO INCLUDE FILES (netcdf.mod) WITH IOINCS
#                           DEFAULT: NO NETCDF OUTPUT
#                           UNCOMMENT TO INCLUDE NETCDF OUTPUT CAPABILITY
#--------------------------------------------------------------------------
           FLAG_4       =  -DNETCDF_IO

# For em64t computers at SMAST (guppy)
#             IOLIBS       =  -L/hosts/salmon01/data00/medm/em64t/lib64  -lnetcdf
#             IOINCS       =  -I/hosts/salmon01/data00/medm/em64t/include64
              IOLIBS       =  -L/hosts/mao.smast.umassd.edu/usr/medm/opt/netcdf/4.3.1.1/lib -lnetcdff -lnetcdf
              IOINCS       =  -I/hosts/mao.smast.umassd.edu/usr/medm/opt/netcdf/4.3.1.1/include
# For i386 computers at SMAST (salmon and minke)
#	      IOLIBS       =  -L/hosts/mao/usr/medm/install/netcdf/3.6.3/ia32/lib  -lnetcdf
#             IOINCS       =  -I/hosts/mao/usr/medm/install/netcdf/3.6.3/ia32/include

# For Typhoeus and Hydra
#              IOLIBS       =  -L/usr/share/netcdf/lib  -lnetcdf
#              IOINCS       =  -I/usr/share/netcdf/include
#              IOLIBS       =  -L/usr/local/install/netcdf/gcc_ifort/3.6.2/lib  -lnetcdf
#              IOINCS       =  -I/usr/local/install/netcdf/gcc_ifort/3.6.2/include


#--------------------------------------------------------------------------
#       NG_OI_ASSIMILATION  INCLUDE NUDGING BASED DATA ASSIMILATION AND 
#                           OPTIMAL INTERPOLATION(OI) BASED DATA 
#                           ASSIMILATION FOR CURRENT/TEMP/SALINITY/SST
#                           CAN BE ACTIVATED/DEACTIVATED AT RUN TIME WITH 
#                           INPUT FILE CONTROL.  (SEE exa_run.dat) FILE 
#                           DEFAULT: NO DATA ASSIMILATION INCLUDED 
#                           UNCOMMENT TO INCLUDE DATA ASSIMILATION 
#--------------------------------------------------------------------------

#             FLAG_5 = -DNG_OI_ASSIM

# For Build on em64t computer (Guppy) 
#             OILIB  = -L/usr/X11R6/lib64 -L/usr/local/lib64 -lmkl_lapack -lmkl_em64t -lguide -lpthread

# For Build on Cluster (Typhoeus and Hydra)
#             OILIB  = -L/usr/X11R6/lib64 -L/usr/local/lib/em64t -lmkl_lapack -lmkl_em64t -lguide -lpthread

#--------------------------------------------------------------------------
#       ARCTIC OCEAN INCLUDED (If you chose this flag, FLAG_2 should be 
#                              selected)
#--------------------------------------------------------------------------

#             FLAG_6 = -DNORTHPOLE

#--------------------------------------------------------------------------
#         Run One-D Mode with Biological Model
#--------------------------------------------------------------------------

#             FLAG_7 = -DONE_D_MODEL



#             FLAG_8 = -DBIO_OBC

#--------------------------------------------------------------------------
#     Vertical Avection Term Calculation
#       commented   : generalized vertical coordinate (s or sigma)
#       uncommented : uniform-distributed sigma coordinate only
#--------------------------------------------------------------------------
#	     FLAG_9 = -DVerSigma

#--------------------------------------------------------------------------
#       GENERAL BIOLOGICAL MODEL:    UNCOMMENT TO INCLUDE MODEL
#-------------------------------------------------------------------------- 
#            FLAG_25 = -DBioGen
            BIOLIB       = -L./BIO_source_newinp -lbiomodule
            BIOINCS      = -I./BIO_source_newinp

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
#        CPPFLAGS   = $(DEF_FLAGS)  -DCOMPAQ
#        CPP        = /bin/cpp
#        FC         = f90
#        DEBFLGS    =  # -check bounds -check overflow -g 
#        OPT        =  -fast -arch ev6 -fpe1
#--------------------------------------------------------------------------
#  CRAY Definitions         
#--------------------------------------------------------------------------
#         CPPFLAGS = $(DEF_FLAGS) -DCRAY 
#         CPP      = /opt/ctl/bin/cpp
#         FC       = f90
#         DEBFLGS  =  
#         OPT      =
#--------------------------------------------------------------------------
#   Linux/Portland Group Definitions 
#--------------------------------------------------------------------------
#         CPP      = /usr/bin/cpp
#         CPPFLAGS = $(DEF_FLAGS) 
#         FC       = pgf90
#         DEBFLGS  = -Mbounds -g -Mprof=func
#         OPT      = #-fast  -Mvect=assoc,cachesize:512000,sse  
#--------------------------------------------------------------------------
#  Intel Compiler Definitions
#--------------------------------------------------------------------------
#         CPP      = /usr/bin/cpp
#         CPPFLAGS = $(DEF_FLAGS) -DINTEL 
#         FC       = ifort
#         DEBFLGS  = #-check all
# Use 'OPT = -O0 -g' for fast compile to test the make 
# Use 'OPT = -xP' for fast run on em64t (Hydra, Typhoeus and Guppy)
# Use 'OPT = -xN' for fast run on em64t (Hydra, Typhoeus and Guppy)
#         OPT      = -O0 -g  
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
# Use 'OPT = -O0 -g'  for fast compile to test the make 
# Use 'OPT = -xP'      for fast run on em64t (Hydra, Typhoeus and Guppy)
# Use 'OPT = -xN'      for fast run on em64t (Salmon and Minke)
#         OPT      = -O0 -g 
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
           CPPARGS =  $(CPPFLAGS) $(DEF_FLAGS) $(FLAG_1) $(FLAG_2) \
   $(FLAG_3) $(FLAG_4) $(FLAG_5) $(FLAG_6)  \
   $(FLAG_7) $(FLAG_8) $(FLAG_9) $(FLAG_10) \
   $(FLAG_11) $(FLAG_12) $(FLAG_13) $(FLAG_14) \
   $(FLAG_15) $(FLAG_16) $(FLAG_17) $(FLAG_18) \
   $(FLAG_19) $(FLAG_20) $(FLAG_21) $(FLAG_22) \
   $(FLAG_23) $(FLAG_24) $(FLAG_25) $(FLAG_26) \
   $(FLAG_27) $(FLAG_28) $(FLAG_29) $(FLAG_30) \
   $(FLAG_31)
#--------------------------------------------------------------------------
#  Libraries           
#--------------------------------------------------------------------------

            LIBS  =	$(PV3LIB) $(CLIB)  $(PARLIB) $(IOLIBS)	\
			$(MPILIB) $(GOTMLIB) $(KFLIB) $(BIOLIB)	\
			$(OILIB) $(VISITLIB) $(PETSC_LIB)

            INCS  = 	$(IOINCS) $(GOTMINCS) $(BIOINCS) $(VISITINC) $(PETSC_FC_INCLUDES)

#--------------------------------------------------------------------------
#  Preprocessing and Compilation Directives
#--------------------------------------------------------------------------
.SUFFIXES: .o .f90 .F .F90 

.F.o:
	$(CPP) $(CPPARGS) $(INCS) $*.F > $*.f90
	$(FC)  -c $(FFLAGS) $(INCS) $*.f90 
	\rm $*.f90
.F90.o:
	$(CPP) $(CPPFLAGS) $(CPPDEFS) $(INCS) $(INCLDIR) $< > $*.f  
	$(FC) -c $(FFLAGS) $(FIXEDFLAGS) $(INCS) $(INCLDIR) $*.f  
	\rm $*.f90


#--------------------------------------------------------------------------
#  FVCOM Source Code.
#--------------------------------------------------------------------------

# Note: Change the order of modules at your own risk!

MODS  = mod_prec.F	mod_types.F	mod_main.F	mod_utils.F	\
	mod_par.F	mod_inp.F	mod_clock.F	mod_spherical.F	\
	mod_obcs.F	mod_northpole.F	mod_ncdin.F	mod_bio_obc.F	\
	mod_bio_3D.F	mod_onedtide.F	mod_ncdout.F	 

MAIN  = utilities.F	alloc_vars.F	archive.F	bcmap.F		\
	bracket.F	cell_area.F	data_run.F	domdec.F	\
	genmap.F	iofiles.F	pdomdec.F	bcs_force.F	\
	report.F	sinter.F	startup.F	getbdno.F	\
	fvcom_gem.F	tge.F		vertvl.F	water_depth.F	\
	closefiles.F	viscofh.F	print_vals.F 	\
	ncdio.F		fct_bio.F	adjust_bio.F

 SRCS = $(MODS) $(MAIN) 

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

tarfile:
	tar cvf fvcom.tar *.F *.F90  makefile exa_run.dat makedepends RELEASE_NOTES 

#--------------------------------------------------------------------------
#  Cleaning targets.
#--------------------------------------------------------------------------

clean:
		/bin/rm -f *.o *.mod

clobber:	clean
		/bin/rm -f *.f90 *.o fvcom_gem

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
