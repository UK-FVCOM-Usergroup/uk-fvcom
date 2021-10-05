!==============================================================================|
!  DECOMPOSE THE DOMAIN BY ELEMENTS USING METIS GRAPH PARTITIONING TOOL        |
!    RETURNS[EL_PID(1:NGL)]                                                    |
!    EL_PID(I) = Processor ID Of Owner of Element I                            |
!==============================================================================|

   SUBROUTINE DOMDEC(NPROCS,EL_PID)              
# if defined (MULTIPROCESSOR)

     use mpi
     USE ALL_VARS, ONLY : NGL,NVG,MSR
!==============================================================================|
   IMPLICIT NONE
!   include "mpif.h"
   INTEGER, INTENT(IN)  :: NPROCS
   INTEGER, INTENT(OUT) :: EL_PID(NGL)
   INTEGER, ALLOCATABLE :: NVT(:)
   INTEGER :: I,IERR
!==============================================================================|

!
!-------------DECOMPOSE ELEMENTS USING METIS GRAPH PARTITIONING ---------------!
!
   IF(MSR)THEN
     ALLOCATE(NVT(3*NGL))
     DO I=1,NGL
       NVT((I-1)*3+1) = NVG(I,1)
       NVT((I-1)*3+2) = NVG(I,3)
       NVT((I-1)*3+3) = NVG(I,2)
     END DO

     CALL PARTITION(NPROCS,NGL,MAXVAL(NVT),loc(NVT),loc(EL_PID))
     EL_PID = EL_PID + 1
     DEALLOCATE(NVT)
   END IF

!---------------------BROADCAST RESULT TO ALL PROCESSORS-----------------------!

!   do i=1,ngl
!   if(msr)write(500,'(2I20)')i,el_pid(i)
!   if(msr)read(500,*)ii,el_pid(i)
!   end do
!   call pstop


   CALL MPI_BCAST(EL_PID,NGL,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)


# endif
   END SUBROUTINE DOMDEC
!==============================================================================|
