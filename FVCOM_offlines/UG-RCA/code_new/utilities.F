!==============================================================================|
!     UTILITIES FILE                                                           |
!      PSTOP:   HALTS PROGRAM CORRECTLY                                        |
!      WRITE_BANNER: WRITE FVCOM BANNER TO OUTPUT                              |
!==============================================================================|


!==============================================================================|
      SUBROUTINE PSTOP(ERREUR)              
!==============================================================================|
!#if defined (MULTIPROCESSOR)
!!  USE MPI
!  include "mpif.h"
!  INTEGER IERR
!!  CALL MPI_ABORT(MPI_COMM_WORLD,IERR)
!  CALL MPI_FINALIZE(IERR)
!#endif 
       CHARACTER*10 ERREUR
       print*,'PSTOP at ',ERREUR
      STOP
      END SUBROUTINE PSTOP
!==============================================================================|

