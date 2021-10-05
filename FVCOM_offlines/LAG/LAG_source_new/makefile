#/=============================================================================/
#Copyright (c) 2007, The University of Massachusetts Dartmouth 
#Produced at the School of Marine Science & Technology 
#Marine Ecosystem Dynamics Modeling group
#All rights reserved.
#
#The FVCOM Offline Lagrangian Model has been developed by the joint UMASSD-WHOI
#research team.   For details of authorship and attribution of credit please see
#the FVCOM technical manual or contact the MEDM group.
#
# 
#This file is part of FVCOM. For details, see http://fvcom.smast.umassd.edu/ The
#full copyright notice is contained in the file COPYRIGHT located in the root
#directory of the FVCOM code. This original header must be maintained in all
#distributed versions.
#
#THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
#ANY EXPRESS OR  IMPLIED WARRANTIES, INCLUDING,  BUT NOT  LIMITED TO, THE
#IMPLIED WARRANTIES OF MERCHANTABILITY AND  FITNESS FOR A PARTICULAR  PURPOSE
#ARE  DISCLAIMED.  
#
#/-----------------------------------------------------------------------------/
#CVS VERSION INFORMATION
#$Id: $
#$Name: $
#$Revision: $
#/=============================================================================/
#
# Configuration file FVCOM Code
#
            SHELL      = /bin/sh
            DEF_FLAGS  = -P -C -traditional 
            EXEC       = ptraj
####################### MEDM ENVIRONMENT #####################################
# Use the environmental variables, LIBPATH and INCLUDEPATH, set by MODULE 
# to find the packages used by this build.
colon=:
empty=
dashI= $(empty) -I
dashL= $(empty) -L
# ### UNCOMMENT HERE!
             LIBDIR       =  -L$(subst $(colon),$(dashL),$(LIBPATH))
             INCDIR       =  -I$(subst $(colon),$(dashI),$(INCLUDEPATH))

###########################################################################
#
#--------------------------------------------------------------------------
#        NETCDF OUTPUT      NETCDF IS NOW REQUIRED TO COMPILE FVCOM
#                           DUMP OUTPUT INTO NETCDF FILES (yes/no)
#                           REQUIRES SYSTEM DEPENDENT NETCDF LIBRARIES
#                           COMPILED WITH SAME F90 COMPILER
#                           SET PATH TO LIBRARIES WITH IOLIBS      
#                           SET PATH TO INCLUDE FILES (netcdf.mod) WITH IOINCS
#--------------------------------------------------------------------------
             IOLIBS       =  -lnetcdf  
             IOINCS       =  
#--------------------------------------------------------------------------


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
#
            FLAG_1 =  -DDOUBLE_PRECISION 
#
#--------------------------------------------------------------------------
#        SPHERICAL          SELECT SPHERICAL COORDINATES FOR INTEGRATION
#                           DEFAULT: CARTESIAN
#                           UNCOMMENT TO SELECT SPHERICAL COORDINATES
#--------------------------------------------------------------------------

            FLAG_2 = -DSPHERICAL

#
#--------------------------------------------------------------------------
#        OUTPUT             DEFAULT : ASCII FILE                     
#                           UNCOMMENT TO SELECT NETCDF
#--------------------------------------------------------------------------
#
            FLAG_3 =  -DOUT_NETCDF 
#
#--------------------------------------------------------------------------
#  Intel Compiler Definitions
#--------------------------------------------------------------------------

         CPP      = /usr/bin/cpp
         CPPFLAGS = $(DEF_FLAGS) -DINTEL 
         FC       = ifort #-Vaxlib 
         DEBFLGS  =  -check all 
         OPT      = #-xP
         CLIB     = #-static-libcxa 
#--------------------------------------------------------------------------
#  SALMON Definitions
#--------------------------------------------------------------------------
#
#         CPPARGS  = $(FLAG_1)
#         CPP      = /usr/bin/cpp
#         EXEC     = ptraj
#         LIBS     = -L./netcdf/lib -lnetcdf 
#         INCS     = -I./netcdf/include
#         FC       = ifort -axN -xN
#         OPT      =  

#--------------------------------------------------------------------------
#  IBM/AIX Definitions
#--------------------------------------------------------------------------
#         CPPARGS  = $(FLAG_1)
#         EXEC     = filter  
#         CPPFLAGS = -P -C  -DMULTIPROCESSOR -DAIX
#         CPP      = /usr/local/bin/cpp

#         FC       = mpxlf90 -qsuffix=f=f90
#         DEBFLGS   =   -check all -qcheck -C -g
#         OPT      = -O -qarch=pwr4 -qtune=pwr4  -bmaxdata:0x80000000 -qhot -qmaxmem=8096

#--------------------------------------------------------------------------

         FFLAGS = $(DEBFLGS) $(OPT) 
         MDEPFLAGS = --cpp --fext=F --file=-
         RANLIB = ranlib

#--------------------------------------------------------------------------
#  CAT Preprocessing Flags
#--------------------------------------------------------------------------
         CPPARGS  = $(FLAG_1) $(FLAG_2) $(FLAG_3) $(CPPFLAGS)

#--------------------------------------------------------------------------
#  Preprocessing and Comilation Directives
#--------------------------------------------------------------------------
.SUFFIXES: .o .f90 .F .F90 

.f90.o:
	$(CPP) $(CPPARGS) $*.f90 > $*.F90
	$(FC)  -c $(FFLAGS) $(INCDIR) $(IOINCS) $*.F90
	\rm $*.F90

#--------------------------------------------------------------------------
#  Filter Source Code.
#--------------------------------------------------------------------------

MODS = mod_prec.f90 mod_ncd.f90 mod_var.f90 mod_inp.f90

MAIN = alloc_vars.f90 triangle_grid_edge.f90 offlag.f90 util.f90 data_run.f90 ncdio.f90

SRCS = $(MODS) $(MAIN) 

OBJS = $(SRCS:.f90=.o)

#--------------------------------------------------------------------------
#  Linking Directives               
#--------------------------------------------------------------------------

$(EXEC):	$(OBJS)
		$(FC) $(INCDIR) $(IOINCS) $(FFLAGS) $(LDFLAGS) -o $(EXEC) $(OBJS) $(LIBDIR) $(IOLIBS)

#--------------------------------------------------------------------------
#  Target to create dependecies.
#--------------------------------------------------------------------------

depend:
		makedepf90  $(SRCS) > makedepends

#--------------------------------------------------------------------------
#  Cleaning targets.
#--------------------------------------------------------------------------

clean:
		/bin/rm -f *.o *.mod ptraj

#clobber:	clean
#		/bin/rm -f *.f90

#--------------------------------------------------------------------------
#  Common rules for all Makefiles - do not edit.
#--------------------------------------------------------------------------

emptyrule::

#--------------------------------------------------------------------------
#  Empty rules for directories that do not have SUBDIRS - do not edit.
#--------------------------------------------------------------------------

includes::

include ./makedepends
