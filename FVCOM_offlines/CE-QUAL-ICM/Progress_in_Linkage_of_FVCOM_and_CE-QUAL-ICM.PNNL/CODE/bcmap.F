   SUBROUTINE BCMAP

!==============================================================================!
   USE SIZES
   USE ALL_VARS
   USE BCS
   USE MOD_OBCS
   USE MOD_UTILS
# if defined (MULTIPROCESSOR)
   USE MOD_PAR
# endif

   IMPLICIT NONE

   INTEGER              :: I,I1,I2,NCNT,IERR,J,JC
   INTEGER, ALLOCATABLE :: TEMP1(:),TEMP2(:),TEMP3(:),TEMP4(:),&
                           TEMP5(:),TEMP6(:),TEMP7(:),ITEMP(:)
   INTEGER :: INOBC 			   
   CHARACTER(LEN=100) ::ISTR

   INOBC = 13
!------------------------------------------------------------------------------!

!==============================================================================|
!   OPEN BOUNDARY CONDITION NODES                                              |
!==============================================================================|

!----------------------------REPORT--------------------------------------------!

   IF(MSR)WRITE(*,*  )'!'
   IF(MSR)WRITE(*,*)'!           SETTING UP OPEN BOUNDARY NODES  '
   IF(MSR)WRITE(*,*  )'!'

   IOBCN = 0
   IBCN  = 0
 
   ISTR = "./inputs/"//trim(casename)
    CALL FOPEN(INOBC, TRIM(ISTR)//'_obc.dat',"cfr")
    REWIND(INOBC)

      READ(INOBC,*)IOBCN_GL
!RGL added below
!      Print*, 'IOBCN_GL', IOBCN_GL
   IF(IOBCN_GL > 0)THEN

!------------Read in Open Boundary Nodes and Temperature/Salinity Conditions---!

     ALLOCATE(I_OBC_GL(IOBCN_GL))
     ALLOCATE(TYPE_OBC_GL(IOBCN_GL))
     ALLOCATE(NUT_OBC_GL(NCP,IOBCN_GL))
      
!    READ nutrient open boundary values
     DO I=1,IOBCN_GL
       READ(INOBC,*) I1,I_OBC_GL(I),TYPE_OBC_GL(I), &
             (NUT_OBC_GL(I2,I),I2=1,NCP)
     END DO

     CLOSE(INOBC)

!----------------------Make Sure It Is In Global Domain------------------------!

     DO I=1,IOBCN_GL
       IF((I_OBC_GL(I) > MGL))THEN
         WRITE(*,*)'==================ERROR=================================='
         WRITE(*,*)'OPEN BOUNDARY NODE NUMBER',I,'IS NOT IN THE'
         WRITE(*,*)'GLOBAL DOMAIN'
         WRITE(*,*)'CHECK INPUT FILE AND ENSURE OPEN BOUNDARY NODES <= ',MGL
         WRITE(*,*)'========================================================='
         CALL PSTOP
       END IF
     END DO

!----------Shift Open Boundary Node List,Type,Salt,and Temp to Local-----------!

     IF(SERIAL)THEN

       IOBCN    = IOBCN_GL

       ALLOCATE(I_OBC_N(IOBCN))
       I_OBC_N = I_OBC_GL
     END IF

#    if defined (MULTIPROCESSOR)
     IF(PAR)THEN
       ALLOCATE(TEMP1(IOBCN_GL))
       NCNT = 0
       !!SET UP LOCAL OPEN BOUNDARY NODES
       DO I=1,IOBCN_GL
         I1 = NLID( I_OBC_GL(I) )
         IF(I1 /= 0)THEN
           NCNT = NCNT + 1
           TEMP1(NCNT) = I1
         END IF
       END DO
       IOBCN = NCNT

       IF(NCNT > 0)THEN
         ALLOCATE(I_OBC_N(NCNT))
         I_OBC_N  = TEMP1(1:NCNT)
       END IF

       DEALLOCATE(TEMP1)
     END IF
#    endif

   END IF !!IOBCN_GL > 0

!==============================================================================|
!   REPORT AND CHECK RESULTS                                                   |
!==============================================================================|
   ALLOCATE(TEMP1(NPROCS))
   TEMP1(1)  = IOBCN

# if defined (MULTIPROCESSOR)
   CALL MPI_GATHER(IOBCN,  1,MPI_INTEGER,TEMP1,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
# endif

   
   IF(MSR)WRITE(*,100)'!  IOBCN                 :',IOBCN_GL,   (TEMP1(I),I=1,NPROCS)
   DEALLOCATE(TEMP1)

   RETURN
   100 FORMAT(1X,A26,I6," =>",2X,4(I5,1H,))
   END SUBROUTINE BCMAP
!==============================================================================|
