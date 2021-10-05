#-----------BEGIN MAKEFILE-------------------------------------------------
            SHELL         = /bin/sh
            DEF_FLAGS     = -P -C -traditional 
            EXEC          = wqm
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
#        MULTI_PROCESSOR    INCLUDES PARALLELIZATION WITH MPI
#                           REQUIRES LINKING MPI LIBRARIES OR COMPILING
#                           WITH A PRELINKED SCRIPT (mpif90/mpf90/etc)
#                           DEFAULT: NO PARALLEL CAPABILITY
#                           UNCOMMENT TO INCLUDE MPI PARALLEL CAPABILITY
#--------------------------------------------------------------------------

#              FLAG_1 = -DMULTIPROCESSOR
#              PARLIB = -L../METIS_source -lmetis
#              PARLIB = -L/hosts/salmon01/data00/medm/lib -lmetis 

#--------------------------------------------------------------------------
#        NETCDF OUTPUT      DUMP OUTPUT INTO NETCDF FILES (yes/no)
#                           REQUIRES SYSTEM DEPENDENT NETCDF LIBRARIES
#                           COMPILED WITH SAME F90 COMPILER
#                           SET PATH TO LIBRARIES WITH IOLIBS      
#                           SET PATH TO INCLUDE FILES (netcdf.mod) WITH IOINCS
#                           DEFAULT: NO NETCDF OUTPUT
#                           UNCOMMENT TO INCLUDE NETCDF OUTPUT CAPABILITY
#--------------------------------------------------------------------------
           FLAG_2       =  -DNETCDF_IO
	   IOLIBS       =  -L/home/tykim/Net_CDF/netcdf-install/lib  -lnetcdf
           IOINCS       =  -I/home/tykim/Net_CDF/netcdf-install/include
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
#  Intel Compiler Definitions
#--------------------------------------------------------------------------
         CPP      = /usr/bin/cpp
         CPPFLAGS = $(DEF_FLAGS) -DINTEL 
         FC       = /opt/intel/fce/9.1/bin/ifort      
         DEBFLGS  = #-check all 
         OPT      = #-O3 -xN -axN -tpp7
         CLIB     = #-static-libcxa 
#--------------------------------------------------------------------------
#  Intel Compiler Definitions
#--------------------------------------------------------------------------
#         CPP      = /usr/bin/cpp
#         CPPFLAGS = $(DEF_FLAGS) -DINTEL 
#         FC       = mpif90     
#         DEBFLGS  = #-check all 
#         OPT      = #-O3 -xN -axN -tpp7
#         CLIB     = #-static-libcxa 
#--------------------------------------------------------------------------
#   Linux/Portland Group Definitions 
#--------------------------------------------------------------------------
#         CPP      = /usr/bin/cpp
#         CPPFLAGS = $(DEF_FLAGS) 
#         FC       = pgf90
#         DEBFLGS  = -Mbounds -g -Mprof=func
#         OPT      = #-fast  -Mvect=assoc,cachesize:512000,sse  
#--------------------------------------------------------------------------
#  gfortran defs 
#--------------------------------------------------------------------------
#         CPP      = /usr/bin/cpp 
#         CPPFLAGS = $(DEF_FLAGS)  -DGFORTRAN
#         FC       = gfortran  -O3 
#         DEBFLGS  = 
#         OPT      = 
#         CLIB     = 
#==========================================================================
#  END USER DEFINITION SECTION
#==========================================================================

         FFLAGS = $(DEBFLGS) $(OPT) 
         MDEPFLAGS = --cpp --fext=f90 --file=-
         RANLIB = ranlib

#--------------------------------------------------------------------------
#  CAT Preprocessing Flags
#--------------------------------------------------------------------------
           CPPARGS = $(CPPFLAGS) $(DEF_FLAGS) $(FLAG_1) $(FLAG_2) 
#--------------------------------------------------------------------------
#  Libraries           
#--------------------------------------------------------------------------

            LIBS  = $(CLIB)  $(PARLIB) $(IOLIBS) $(MPILIB)
#            LIBS  = -L../SPARSKIT2 -lskit $(PARLIB)
#             LIBS  = $(PARLIB) $(PETSC_LIB)
#           INCS  = $(IOINCS)$(PETSC_FC_INCLUDES)$(IOINCS)
           INCS  = $(IOINCS)

#--------------------------------------------------------------------------
#  Preprocessing and Compilation Directives
#--------------------------------------------------------------------------
.SUFFIXES: .o .f90 .F .F90 

.F.o:
	$(CPP) $(CPPARGS) $(INCS) $*.F > $*.f90
	$(FC)  -c $(FFLAGS) $(INCS) $*.f90
	\rm $*.f90

#--------------------------------------------------------------------------
#  FVCOM Source Code.
#--------------------------------------------------------------------------


MAIN  = mod_prec.F	mod_types.F	mod_utils.F	mod_par.F	\
	mod_var.F	mod_ncd.F	mod_obcs.F	wqm_modules.F	\
	wqm_alg.F	wqm_sed.F	wqm_inputs.F	\
	wqm_kin.F	wqm_owq.F	wqm_sav.F	wqm_sf.F	\
	wqm_main.F	hydro.F 	tvds.F			\
	ncdio.F 	utilities.F	tge.F		cell_area.F	\
	pdomdec.F	bcmap.F 	domdec.F	genmap.F	\
	adv_wqm.F	fct_nut.F       vdif_wqm.F	vertvl.F	\
	viscofh.F       bcond_wqm.F	bcs_force.F	bracket.F		

 SRCS = $(MAIN)  

 OBJS = $(SRCS:.F=.o)

#--------------------------------------------------------------------------
#  Linking Directives               
#--------------------------------------------------------------------------

$(EXEC):	$(OBJS)
		$(FC) $(FFLAGS) $(LDFLAGS) -o $(EXEC) $(OBJS) $(LIBS)

#--------------------------------------------------------------------------
#  Target to create dependecies.
#--------------------------------------------------------------------------

depend:
		makedepf90  $(SRCS) >> makedepends


#--------------------------------------------------------------------------
#  Tar Up Code                           
#--------------------------------------------------------------------------

tarfile:
	tar cvf fvcom.tar *.F  makefile exa_run.dat makedepends RELEASE_NOTES 

#--------------------------------------------------------------------------
#  Cleaning targets.
#--------------------------------------------------------------------------

clean:
		/bin/rm -f *.o *.mod

clobber:	clean
		/bin/rm -f *.f90 *.o fvcom

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
