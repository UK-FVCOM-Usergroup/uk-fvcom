
/*
 * Copyright 1997, Regents of the University of Minnesota
 *
 * partdmesh.c
 *
 * This file reads in the element node connectivity array of a mesh and 
 * partitions both the elements and the nodes using KMETIS on the dual graph.
 *
 * Started 9/29/97
 * George
 *
 * $Id: partdmesh.c,v 1.1 1998/11/27 17:59:38 karypis Exp $
 *
 */

/*
 * in frename.c, 6 subroutine names are mapped to the following suroutine
 * this supports the situation where the fortran compiler namespace is different
 * which results in the fortran routine calling a variety of different names
 * for Partition including:
 * 	partition
 * 	PARTITION
 * 	partition_
 * 	PARTITION_
 * 	partition__
 * 	PARTITION__
 * if the fortran compiler gives an error during linking that the routine
 * partition can't be found and you are sure you are linking in the metis 
 * libraries correctly, check carefully the name of the subroutine the compiler
 * is looking for.  If it is a different variant on "partition", add this 
 * variant at the bottom of "frename.c" and recompile the library*/

#include <metis.h>


void partition_(int* nprocsin, int* nein, int* nnin, int* nv[], int* proc[])
{
  int i, j, etype, numflag=0, nparts, edgecut,nn,ne;
  idxtype *elmnts, *epart, *npart;
  char etypestr[4][5] = {"TRI", "TET", "HEX", "QUAD"};
   
  nparts = *nprocsin;
  etype = 1; /* triangles */
  nn = *nnin;
  ne = *nein;
  elmnts = idxmalloc(3*(ne), "ReadMesh: elmnts");

  for (j=3*ne, i=0; i<j; i++) {
       elmnts[i] = (*nv)[i]-1;
  }

  epart = idxmalloc(ne, "main: epart");
  npart = idxmalloc(nn, "main: npart");


  METIS_PartMeshDual(&ne, &nn, elmnts, &etype, &numflag, &nparts, &edgecut, epart, npart);
/*  printf(" edgecut %d\n",edgecut);*/

  for (j=ne, i=0; i<j; i++) {
       (*proc)[i] = epart[i];
  }

/*  printf("  %d-way Edge-Cut: %7d, Balance: %5.2f\n", nparts, edgecut, ComputeElementBalance(ne, nparts, epart));*/


  GKfree(&elmnts, &epart, &npart, LTERM);

}

