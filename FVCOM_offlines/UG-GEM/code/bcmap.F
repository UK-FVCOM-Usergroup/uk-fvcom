
   SUBROUTINE BCMAP

!==============================================================================!
   USE ALL_VARS
# if defined (MULTIPROCESSOR)
   USE MOD_PAR
# endif

   USE MOD_OBCS

   IMPLICIT NONE

   INTEGER              :: I,I1,I2,NCNT,IERR,J
   INTEGER, ALLOCATABLE :: TEMP1(:),TEMP2(:),TEMP3(:),TEMP4(:),&
                           TEMP5(:),TEMP6(:),TEMP7(:),ITEMP(:)
!------------------------------------------------------------------------------!

!==============================================================================|
!   OPEN BOUNDARY CONDITION NODES                                              |
!==============================================================================|

!----------------------------REPORT--------------------------------------------!

   IF(MSR)WRITE(IPT,*  )'!'
   IF(MSR)WRITE(IPT,*)'!           SETTING UP OPEN BOUNDARY NODES  '
   IF(MSR)WRITE(IPT,*  )'!'

   IOBCN = 0
   IBCN  = 0

   IF(IOBCN_GL > 0)THEN

!------------Read in Open Boundary Nodes and Temperature/Salinity Conditions---!

     ALLOCATE(I_OBC_GL(IOBCN_GL))
     ALLOCATE(TYPE_OBC_GL(IOBCN_GL))
!GWC ADD OBC FORCING FOR TEMP/SALT
     ALLOCATE(TEMP_OBC_GL(IOBCN_GL)) ; TEMP_OBC_GL = 0.
     ALLOCATE(SALT_OBC_GL(IOBCN_GL)) ; SALT_OBC_GL = 0.
!GWC
     READ(INOBC,*)

!JQI     IF(TS_NUDGING_OBC)THEN
!      GWC READ IN SALT/TEMP OBC FORCING
!JQI       DO I=1,IOBCN_GL
!JQI         READ(INOBC,*) I1,I_OBC_GL(I),TYPE_OBC_GL(I),TEMP_OBC_GL(I),SALT_OBC_GL(I) 
!JQI       END DO
!      GWC
!JQI     ELSE
       DO I=1,IOBCN_GL
         READ(INOBC,*) I1,I_OBC_GL(I),TYPE_OBC_GL(I) 
       END DO
!JQI     END IF

     CLOSE(INOBC)

!----------------------Make Sure It Is In Global Domain------------------------!

     DO I=1,IOBCN_GL
       IF((I_OBC_GL(I) > MGL))THEN
         WRITE(IPT,*)'==================ERROR=================================='
         WRITE(IPT,*)'OPEN BOUNDARY NODE NUMBER',I,'IS NOT IN THE'
         WRITE(IPT,*)'GLOBAL DOMAIN'
         WRITE(IPT,*)'CHECK INPUT FILE AND ENSURE OPEN BOUNDARY NODES <= ',MGL
         WRITE(IPT,*)'========================================================='
         CALL PSTOP
       END IF
     END DO

!-------------------Ensure OBC Types are Valid-[1-->10]------------------------!

     DO I=1,IOBCN_GL
       IF(TYPE_OBC_GL(I) < 1 .OR. TYPE_OBC_GL(I) > 10)THEN
         IF(MSR)THEN
           WRITE(*,*)'ERROR: Outer Boundary Node Type in File:'
           WRITE(*,*)'casename_obc.dat Must be >0 and <= 11'
         END IF
         CALL PSTOP
       END IF
     END DO

!----------Shift Open Boundary Node List,Type,Salt,and Temp to Local-----------!

     IF(SERIAL)THEN

       IOBCN    = IOBCN_GL

       ALLOCATE(I_OBC_N(IOBCN))
       I_OBC_N = I_OBC_GL
       ALLOCATE(TYPE_OBC(IOBCN))
       TYPE_OBC = TYPE_OBC_GL
!      GWC ADD TEMP/SALT
       ALLOCATE(TEMP_OBC(IOBCN)) ; TEMP_OBC = 0.0
       TEMP_OBC = TEMP_OBC_GL
       ALLOCATE(SALT_OBC(IOBCN)) ; SALT_OBC = 0.0
       SALT_OBC = SALT_OBC_GL
!      GWC
     END IF

#    if defined (MULTIPROCESSOR)
     IF(PAR)THEN
       ALLOCATE(TEMP1(IOBCN_GL))
       ALLOCATE(TEMP2(IOBCN_GL))
       ALLOCATE(TEMP3(IOBCN_GL))
       ALLOCATE(TEMP4(IOBCN_GL))
       NCNT = 0
       !!SET UP LOCAL OPEN BOUNDARY NODES
       DO I=1,IOBCN_GL
         I1 = NLID( I_OBC_GL(I) )
         IF(I1 /= 0)THEN
           NCNT = NCNT + 1
           TEMP1(NCNT) = I1
           TEMP2(NCNT) = TYPE_OBC_GL(I)
!          GWC ADD TEMP/SALT
           TEMP3(NCNT) = TEMP_OBC_GL(I)
           TEMP4(NCNT) = SALT_OBC_GL(I)
!          GWC 
         END IF
       END DO
       IOBCN = NCNT

       IF(NCNT > 0)THEN
         ALLOCATE(I_OBC_N(NCNT),TYPE_OBC(NCNT))
         ALLOCATE(TEMP_OBC(NCNT)) ; TEMP_OBC = 0.0
         ALLOCATE(SALT_OBC(NCNT)) ; SALT_OBC = 0.0
         I_OBC_N  = TEMP1(1:NCNT)
         TYPE_OBC = TEMP2(1:NCNT)
         TEMP_OBC = TEMP3(1:NCNT)
         SALT_OBC = TEMP4(1:NCNT)
       END IF

       DEALLOCATE(TEMP1,TEMP2,TEMP3,TEMP4)
     END IF
#    endif

!----------------------Set 11 Types Open Boundary Nodes Arrays------------------!

     CALL SEPARATE_OBC

# if defined (TIDE_OUTPUT)
     ALLOCATE(ITEMP(NGL))
     IOBCELL_GL = 0
     DO 993 I = 1, NGL
        DO J = 1, IOBCN_GL
           IF(FLOAT(NVG(I,1)-I_OBC_GL(J))*FLOAT(NVG(I,2)-I_OBC_GL(J))*FLOAT(NVG(I,3)-I_OBC_GL(J))==0) THEN
              IOBCELL_GL = IOBCELL_GL + 1
              ITEMP(IOBCELL_GL) = I
              GO TO 993
           END IF
        END DO
 993 CONTINUE
     ALLOCATE(I_OBCELL_GL(IOBCELL_GL))
     I_OBCELL_GL(1:IOBCELL_GL) = ITEMP(1:IOBCELL_GL)
     DEALLOCATE(ITEMP)

     ALLOCATE(ITEMP(MGL))
     IOBCNODE_GL = 0
     DO 994 I = 1, MGL
        DO J = 1, IOBCELL_GL
           IF(FLOAT(NVG(I_OBCELL_GL(J),1)-I)*FLOAT(NVG(I_OBCELL_GL(J),2)-I)*FLOAT(NVG(I_OBCELL_GL(J),3)-I)==0) THEN
              IOBCNODE_GL = IOBCNODE_GL + 1
              ITEMP(IOBCNODE_GL) = I
              GO TO 994
           END IF
        END DO
 994 CONTINUE
     ALLOCATE(I_OBCNODE_GL(IOBCNODE_GL))
     I_OBCNODE_GL(1:IOBCNODE_GL) = ITEMP(1:IOBCNODE_GL)
     DEALLOCATE(ITEMP)
# endif

   END IF !!IOBCN_GL > 0

!==============================================================================|
!   REPORT AND CHECK RESULTS                                                   |
!==============================================================================|
   ALLOCATE(TEMP1(NPROCS),TEMP2(NPROCS),TEMP3(NPROCS),TEMP4(NPROCS))
   ALLOCATE(TEMP5(NPROCS),TEMP6(NPROCS))
   TEMP1(1)  = IOBCN
   TEMP2(1) = IBCN(1)
   TEMP3(1) = IBCN(2)
   TEMP4(1) = IBCN(3)
   TEMP5(1) = IBCN(4)
   TEMP6(1) = IBCN(5)

# if defined (MULTIPROCESSOR)
   CALL MPI_GATHER(IOBCN,  1,MPI_INTEGER,TEMP1,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
   CALL MPI_GATHER(IBCN(1),1,MPI_INTEGER,TEMP2,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
   CALL MPI_GATHER(IBCN(2),1,MPI_INTEGER,TEMP3,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
   CALL MPI_GATHER(IBCN(3),1,MPI_INTEGER,TEMP4,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
   CALL MPI_GATHER(IBCN(4),1,MPI_INTEGER,TEMP5,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
   CALL MPI_GATHER(IBCN(5),1,MPI_INTEGER,TEMP6,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
# endif

   
   IF(MSR)WRITE(IPT,100)'!  IOBCN                 :',IOBCN_GL,   (TEMP1(I),I=1,NPROCS)
   IF(MSR)WRITE(IPT,100)'!  IBCN(1)               :',IBCN_GL(1), (TEMP2(I),I=1,NPROCS)
   IF(MSR)WRITE(IPT,100)'!  IBCN(2)               :',IBCN_GL(2), (TEMP3(I),I=1,NPROCS)
   IF(MSR)WRITE(IPT,100)'!  IBCN(3)               :',IBCN_GL(3), (TEMP4(I),I=1,NPROCS)
   IF(MSR)WRITE(IPT,100)'!  IBCN(4)               :',IBCN_GL(4), (TEMP5(I),I=1,NPROCS)
   IF(MSR)WRITE(IPT,100)'!  IBCN(5)               :',IBCN_GL(5), (TEMP6(I),I=1,NPROCS)
   DEALLOCATE(TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6)

   RETURN
   100 FORMAT(1X,A26,I6," =>",2X,4(I5,1H,))
   END SUBROUTINE BCMAP
!==============================================================================|
