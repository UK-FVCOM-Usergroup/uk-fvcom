!==============================================================================|
!  DECOMPOSE THE DOMAIN BY ELEMENTS USING METIS GRAPH PARTITIONING TOOL        |
!    RETURNS[EL_PID(1:NGL)]                                                    |
!    EL_PID(I) = Processor ID Of Owner of Element I                            |
!==============================================================================|

   SUBROUTINE DOMDEC(NGL,NPROCS,EL_PID,MSR)              
# if defined (MULTIPROCESSOR)

   USE ALL_VARS, ONLY : NVG
!==============================================================================|
   IMPLICIT NONE
   include "mpif.h"
   INTEGER, INTENT(IN)  :: NGL,NPROCS
   INTEGER, INTENT(OUT) :: EL_PID(NGL)
   LOGICAL, INTENT(IN)  :: MSR
   INTEGER, ALLOCATABLE :: NVT(:)
   INTEGER :: I,NTEMP,IERR,ii
!==============================================================================|

!
!----------------READ IN NODE LIST FROM ***_grd.dat FILE-----------------------!
!
   IF(MSR)THEN
   ALLOCATE(NVT(3*NGL))

   II = 0
   DO I=1,NGL
     II = II+1
     NVT(II)   = NVG(I,1)    !TEMP(I,1)
     NVT(II+1) = NVG(I,1)    !TEMP(I,3)
     NVT(II+2) = NVG(I,1)    !TEMP(I,2)
   END DO

!
!-------------DECOMPOSE ELEMENTS USING METIS GRAPH PARTITIONING ---------------!
!
   CALL PARTITION(NPROCS,NGL,MAXVAL(NVT),loc(NVT),loc(EL_PID))
   EL_PID = EL_PID + 1
   DEALLOCATE(NVT)
   END IF

   CALL MPI_BCAST(EL_PID,NGL,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)


# endif
   END SUBROUTINE DOMDEC
!==============================================================================|
