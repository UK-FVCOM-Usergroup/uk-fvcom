!===============================================================================!
! DEFINE FLOATING POINT PRECISION USING KIND                                    !
!===============================================================================!
MODULE MOD_PREC
   IMPLICIT NONE
#  if defined (MULTIPROCESSOR)
   include "mpif.h"
#  endif

!--Single Precision Coding------------------------------------------------------!
#  if !defined (DOUBLE_PRECISION)
   INTEGER, PARAMETER :: SP = SELECTED_REAL_KIND(6,30)
#  if defined (MULTIPROCESSOR)
   INTEGER, PARAMETER :: MPI_F = MPI_REAL
#  endif
   
#  else 
!--Double Precision Coding------------------------------------------------------!
   INTEGER, PARAMETER :: SP = SELECTED_REAL_KIND(12,300)
#  if defined (MULTIPROCESSOR)
   INTEGER, PARAMETER :: MPI_F = MPI_DOUBLE_PRECISION
#  endif
#  endif


  
   INTEGER, PARAMETER :: DP     = SELECTED_REAL_KIND(12,300)
#  if defined (MULTIPROCESSOR)
   INTEGER, PARAMETER :: MPI_DP = MPI_DOUBLE_PRECISION
#  endif
   

END MODULE MOD_PREC

