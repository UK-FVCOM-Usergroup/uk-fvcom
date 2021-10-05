!==============================================================================|
!   Set Up the Following Boundary Conditions:                                  |
!     Freshwater River Water Quality Input                                     |
!==============================================================================|

   SUBROUTINE BCS_FORCE           

!------------------------------------------------------------------------------|

   USE SIZES
   USE ALL_VARS
   USE BCS
   USE MOD_UTILS
# if defined (MULTIPROCESSOR)
   USE MOD_PAR
# endif

   IMPLICIT NONE
   CHARACTER(LEN=80) :: COMT
   REAL(SP) :: TTIME
   REAL(SP) :: FTEMP1,FTEMP2,FTEMP3,RBUF1,RBUF2,RBUF3
   REAL(SP), ALLOCATABLE :: RTEMP(:),RTEMP1(:,:,:)
   INTEGER,  ALLOCATABLE :: TEMP(:),TEMP2(:),TEMP3(:),TEMP4(:),NODE_SBC(:)
   INTEGER,  ALLOCATABLE :: TEMPD(:,:),TEMP2D(:,:),TEMP3D(:,:)
   INTEGER   I,J,K,NQTIME,NBFWTIME,ISBCN1,INMAX,IOS,NCNT,IGL,IERR,JN
   CHARACTER(LEN=13) :: TSTRING
   CHARACTER(LEN=100) :: ISTR
   INTEGER :: INRIV,NJ
   
   INRIV = 61

!------------------------------------------------------------------------------|


!----------------------------REPORT--------------------------------------------!
   IF(MSR)WRITE(*,*  )'!'
   IF(MSR)WRITE(*,*)'!           SETTING UP PRESCRIBED BOUNDARY CONDITIONS   '
   IF(MSR)WRITE(*,*  )'!'

!==============================================================================|
!   Input River/Dam/Intake/Outfall Boundary Values                             |
!==============================================================================|

!
!-------Check Selected Combination for Validity--------------------------------!
!
   ISTR = "./inputs/"//trim(casename)
   CALL FOPEN(INRIV, TRIM(ISTR)//'_riv.dat',"cfr")
!
   REWIND(INRIV)
   READ(INRIV,'(A4,2X,A10)') INFLOW_TYPE,POINT_ST_TYPE

   IF(INFLOW_TYPE /= 'edge' .AND. INFLOW_TYPE /= 'node') THEN
     CALL PERROR(6,"INFLOW TYPE NOT CORRECT","SHOULD BE edge or node")
   END IF

   IF(POINT_ST_TYPE /= 'calculated' .AND. POINT_ST_TYPE /= 'specified') THEN
     CALL PERROR(6,"POINT_ST TYPE NOT CORRECT","SHOULD BE calculated or specified")
   END IF

!
!--Read in Number of Discharge Nodes/Edges-------------------------------------!
!
   IF(MSR)THEN
     READ(INRIV,*) NUMQBC_GL
   END IF

#  if defined (MULTIPROCESSOR)
   IF(PAR)CALL MPI_BCAST(NUMQBC_GL,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
#  endif

   NUMQBC = 0
   IF(NUMQBC_GL > 0)THEN

     ALLOCATE(TEMP(NUMQBC_GL),TEMP2(NUMQBC_GL),TEMP3(NUMQBC_GL))
     IF(MSR)THEN
       DO I=1,NUMQBC_GL
         READ(INRIV,*) TEMP(I)
       END DO
     END IF

#    if defined (MULTIPROCESSOR)
     IF(PAR)CALL MPI_BCAST(TEMP,NUMQBC_GL,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
#    endif

!
!--Determine Global--> Local Mapping of Freshwater Discharge Nodes
!
     IF(SERIAL)THEN
       NUMQBC = NUMQBC_GL
       IF(INFLOW_TYPE == 'node') THEN
         ALLOCATE(INODEQ(NUMQBC))
         INODEQ = TEMP
       ELSE IF(INFLOW_TYPE == 'edge') THEN
         ALLOCATE(ICELLQ(NUMQBC))
         ICELLQ = TEMP(1:NUMQBC)
       END IF
     END IF

#   if defined (MULTIPROCESSOR)
     IF(PAR)THEN
       NCNT = 0
       IF(INFLOW_TYPE == 'node') THEN
         DO I=1,NUMQBC_GL
           IF(NLID(TEMP(I)) /= 0)THEN
             NCNT = NCNT + 1
             TEMP2(NCNT) = NLID(TEMP(I))
             TEMP3(NCNT) = I
           END IF
         END DO
         NUMQBC = NCNT
         ALLOCATE(INODEQ(NUMQBC),RIV_GL2LOC(NUMQBC))
         INODEQ = TEMP2(1:NCNT)
         RIV_GL2LOC = TEMP3(1:NCNT)
       ELSE IF(INFLOW_TYPE == 'edge') THEN
         DO I=1,NUMQBC_GL
           IF(ELID(TEMP(I)) /= 0)THEN
             NCNT = NCNT + 1
             TEMP2(NCNT) = ELID(TEMP(I))
             TEMP3(NCNT) = I
           END IF
         END DO
         NUMQBC = NCNT
         ALLOCATE(ICELLQ(NUMQBC),RIV_GL2LOC(NUMQBC))
         ICELLQ = TEMP2(1:NCNT)
         RIV_GL2LOC = TEMP3(1:NCNT)
       END IF
     END IF
#   endif

     DEALLOCATE(TEMP,TEMP2,TEMP3)

!
!----Read in Time Dependent DataSets (DQDIS,DSDIS,DTDIS)------------------------!
!
     IF(MSR)READ(INRIV,*) NQTIME

#    if defined (MULTIPROCESSOR)
     IF(PAR)CALL MPI_BCAST(NQTIME,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
#    endif

     QBC_TM%NTIMES = NQTIME
     QBC_TM%LABEL  = "Freshwater Discharge"
     ALLOCATE(QBC_TM%TIMES(NQTIME))
     ALLOCATE(RTEMP1(NUMQBC_GL,NCP,NQTIME))

     IF(MSR)THEN
       DO I = 1, NQTIME
         READ(INRIV,*) TTIME
         QBC_TM%TIMES(I) = TTIME
         DO NJ=1,NCP
           READ(INRIV,*) (RTEMP1(J,NJ,I),J = 1,NUMQBC_GL)
         END DO

       END DO
     END IF

#    if defined (MULTIPROCESSOR)
     IF(PAR)CALL MPI_BCAST(QBC_TM%TIMES,NQTIME,MPI_F,0,MPI_COMM_WORLD,IERR)
     IF(PAR)CALL MPI_BCAST(RTEMP1(:,:,:),NUMQBC_GL*NQTIME*NCP,MPI_F,0,MPI_COMM_WORLD,IERR)
#    endif

!
!----TRANSFORM TO LOCAL ARRAYS-------------------------------------------------|
!

     IF(NUMQBC > 0)THEN
       ALLOCATE(DWDIS(NUMQBC,NCP,NQTIME))

       IF(SERIAL)THEN
         DWDIS(1:NUMQBC_GL,:,:) = RTEMP1(1:NUMQBC_GL,:,:)
       END IF

#     if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO I=1,NQTIME
           DO NJ = 1,NCP
             DWDIS(1:NUMQBC,NJ,I) = RTEMP1(RIV_GL2LOC(1:NUMQBC),NJ,I)
           END DO
         END DO
       END IF
#     endif

     END IF

     DEALLOCATE(RTEMP1)

     CLOSE(INRIV)

   END IF !! NUMQBC_GL > 0

   RETURN
   END SUBROUTINE BCS_FORCE
!==============================================================================|
