#
# Configuration file for METIS Code
#
            SHELL = /bin/sh

###
# NAME AND COMPILE OPTIONS HERE
###


            LIB      = libmetis
            MOPT     = -O3
            INCLUDES = -I.
            C_FLAGS    = $(COPTIONS) $(MOPT) $(INCLUDES)
#--------------------------------------------------------------------------
#  Preprocessing and Compilation Directives
#--------------------------------------------------------------------------

OBJS = coarsen.o fm.o initpart.o match.o ccgraph.o memory.o \
       pmetis.o pqueue.o refine.o util.o timing.o debug.o \
       bucketsort.o graph.o stat.o kmetis.o kwayrefine.o \
       kwayfm.o balance.o ometis.o srefine.o sfm.o separator.o \
       mincover.o mmd.o mesh.o meshpart.o frename.o fortran.o \
       myqsort.o compress.o parmetis.o estmem.o \
       mpmetis.o mcoarsen.o mmatch.o minitpart.o mbalance.o \
       mrefine.o mutil.o mfm.o mkmetis.o mkwayrefine.o mkwayfmh.o \
       mrefine2.o minitpart2.o mbalance2.o mfm2.o \
       kvmetis.o kwayvolrefine.o kwayvolfm.o subdomains.o partition.o

.c.o:
	$(CC) $(C_FLAGS) -c $*.c

libmetis.a: $(OBJS)
	$(AR) $@ $(OBJS)
	$(RANLIB) $@

clean:
	rm -f *.o

clobber:
	rm -f *.o ; rm -f libmetis.a

install: 
	make libmetis.a
	chmod 755 libmetis.a
	cp libmetis.a $(INSTALLDIR)/lib

includes::
include make_NWGOFS.inc
#include ./makedepends
# DO NOT DELETE
