!==============================================================================|
!   Set Up the Following Boundary Conditions:                                  |
!     Bottom Freshwater (Groundwater) Info			               |
!     Freshwater River Discharge					       |
!==============================================================================|

   SUBROUTINE BCS_FORCE           
!------------------------------------------------------------------------------|
   USE ALL_VARS
   USE BCS
   USE MOD_CLOCK
   USE MOD_UTILS
   USE MOD_1D
   USE MOD_BIO_3D
# if defined (MULTIPROCESSOR)
   USE MOD_PAR
# endif
   USE MOD_OBCS

   IMPLICIT NONE
   CHARACTER(LEN=80) :: COMT
   REAL(SP) :: QPREC,QEVAP,WDS,WDD,HFLUX,HSHORT
   REAL(SP) :: TX,TY,CD,TTIME,BFWTIME
   REAL(SP) :: FTEMP1,FTEMP2,FTEMP3,RBUF1,RBUF2,RBUF3
   REAL(SP), ALLOCATABLE :: RTEMP(:),RTEMP1(:,:),RTEMP2(:,:,:)
   REAL(SP), ALLOCATABLE :: RTEMP11(:,:) 
   INTEGER,  ALLOCATABLE :: TEMP(:),TEMP2(:),TEMP3(:),TEMP4(:),NODE_SBC(:)
   REAL,  ALLOCATABLE :: TEMPD(:),RBUF(:)
   INTEGER   I,J,K,NQTIME,NBFWTIME,ISBCN1,INMAX,IOS,NCNT,IGL,IERR,JN,N1
   CHARACTER(LEN=13) :: TSTRING

!------------------------------------------------------------------------------|


!----------------------------REPORT--------------------------------------------!
   IF(MSR)WRITE(IPT,*  )'!'
   IF(MSR)WRITE(IPT,*)'!           SETTING UP PRESCRIBED BOUNDARY CONDITIONS   '
   IF(MSR)WRITE(IPT,*  )'!'

!==============================================================================|
!  Ground Water Information  BFWQDIS: m^3/s                                                 |
!==============================================================================|
!
!--------------determine global number of groundwater points and bcast---------!
!
   IF(MSR)THEN
    WRITE(IOPRT,*)'GROUNDWATER INFORMATION'
    READ(INBFW ,1000) COMT
    WRITE(IOPRT,1000) COMT
    READ(INBFW ,*) IBFW_GL
    WRITE(IOPRT,*) IBFW_GL
   END IF

#  if defined (MULTIPROCESSOR)
   IF(PAR)CALL MPI_BCAST(IBFW_GL,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
#  endif

   IBFW = 0
   IF(IBFW_GL > 0) THEN
    NCNT = 0

!
!--------------input node numbers for ground water inflow----------------------!
!
    ALLOCATE( NODE_BFW(IBFW_GL) )

    IF(MSR)THEN
     READ(INBFW ,*) (NODE_BFW(I),I=1,IBFW_GL)
     WRITE(IOPRT,*) (NODE_BFW(I),I=1,IBFW_GL)
!
!-----------------ensure all nodes exist in global domain----------------------!
!
     DO I=1,IBFW_GL
      IF(NODE_BFW(I) > MGL)THEN
       WRITE(IPT,*)'==================ERROR=================================='
       WRITE(IPT,*)'GROUND WATER NODE NUMBER',I,'IS NOT IN THE GLOBAL DOMAIN'
       WRITE(IPT,*)'ENSURE GROUNDWATER NODES <= ',MGL
       WRITE(IPT,*)'========================================================='
       CALL PSTOP
      END IF
     END DO
    END IF

#   if defined (MULTIPROCESSOR)
    IF(PAR)CALL MPI_BCAST(NODE_BFW,IBFW_GL,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
#   endif
!
!----------------------Shift To Local Domain If Parallel-----------------------!
!
    IF(SERIAL) IBFW = IBFW_GL

#   if defined (MULTIPROCESSOR)
    IF(PAR)THEN
     ALLOCATE(TEMP2(IBFW_GL) , TEMP3(IBFW_GL))
     DO I=1,IBFW_GL
      IF(NLID(NODE_BFW(I)) /= 0)THEN
       NCNT = NCNT + 1
       TEMP2(NCNT) = NLID(NODE_BFW(I))
       TEMP3(NCNT) = I
      END IF
     END DO
     IBFW = NCNT

     DEALLOCATE(NODE_BFW)
     ALLOCATE(NODE_BFW(IBFW),BFW_GL2LOC(IBFW))
     NODE_BFW   = TEMP2(1:IBFW)
     BFW_GL2LOC = TEMP3(1:IBFW)
     DEALLOCATE(TEMP2,TEMP3)
    END IF
#   endif

!
!----INPUT NUMBER OF DATA TIMES FOR GROUNDWATER DATA---------------------------!
!
    BFW_TM%LABEL = "Groundwater"
    IF(MSR)THEN
     READ(INBFW ,*) NBFWTIME
     WRITE(IOPRT,*) NBFWTIME
    END IF

#   if defined (MULTIPROCESSOR)
    IF(PAR)CALL MPI_BCAST(NBFWTIME,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
#   endif

    BFW_TM%NTIMES = NBFWTIME
    ALLOCATE(BFW_TM%TIMES(NBFWTIME))

!
!----READ IN FRESH WATER FLUX AT EACH TIME=BFWTIME-----------------------------!
!
    ALLOCATE(RTEMP(IBFW_GL))
    ALLOCATE(RTEMP11(IBFW_GL,NTT))  

    ALLOCATE(BFWQDIS(IBFW,NBFWTIME))      ; BFWQDIS  = 0.0_SP
    ALLOCATE(BFWQWDIS(IBFW,NTT,NBFWTIME)) ; BFWQWDIS = 0.0_SP 

    DO I=1,NBFWTIME
     IF(MSR)THEN
      READ(INBFW,*) BFWTIME
      WRITE(IOPRT,5000) BFWTIME
      BFW_TM%TIMES(I) = BFWTIME
      READ(INBFW,*) (RTEMP(J),J = 1,IBFW_GL)
      DO N1 = 1,NTT
       READ(INBFW,*) (RTEMP11(J,N1),J = 1,IBFW_GL)
      END DO
     END IF

#    if defined (MULTIPROCESSOR)
     IF(PAR)CALL MPI_BCAST(RTEMP,IBFW_GL,MPI_F,0,MPI_COMM_WORLD,IERR)
     IF(PAR)CALL MPI_BCAST(RTEMP11,IBFW_GL*NTT,MPI_F,0,MPI_COMM_WORLD,IERR)
#    endif

     IF(SERIAL)BFWQDIS(1:IBFW_GL,I)  = RTEMP(1:IBFW_GL)
     IF(SERIAL)BFWQWDIS(1:IBFW_GL,1:NTT,I) = RTEMP11(1:IBFW_GL,1:NTT)

#    if defined (MULTIPROCESSOR)
     IF(PAR)THEN
      DO J=1,IBFW
       BFWQDIS(J,I)  = RTEMP(BFW_GL2LOC(J))
       DO N1 = 1,NTT
        BFWQWDIS(J,N1,I) = RTEMP11(BFW_GL2LOC(J),N1)
       END DO
      END DO
     END IF
#    endif

     IF(MSR)WRITE(IOPRT,5000) (RTEMP(J),J = 1,IBFW_GL) 
     IF(MSR)THEN
      DO N1=1,NTT
       WRITE(IOPRT,5000) (RTEMP11(J,N1),J = 1,IBFW_GL) 
      END DO
     END IF 
    END DO
    DEALLOCATE(RTEMP,RTEMP11)


#   if defined (MULTIPROCESSOR)
    IF(PAR)CALL MPI_BCAST(BFW_TM%TIMES,NBFWTIME,MPI_F,0,MPI_COMM_WORLD,IERR)
#   endif

   END IF !!IBFW_GL > 0

!
!--REPORT RESULTS--------------------------------------------------------------!
!
   ALLOCATE(TEMP(NPROCS))
   TEMP(1)  = IBFW

#  if defined (MULTIPROCESSOR)
   CALL MPI_GATHER(IBFW,1,MPI_INTEGER,TEMP,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
#  endif

   IF(IBFW_GL == 0)THEN
    IF(MSR)WRITE(IPT,*)'!  GROUNDWATER FLUX      :    NONE'
   ELSE
    IF(MSR)WRITE(IPT,*)'!'
    IF(MSR)WRITE(IPT,100)'!  GROUNDWATER POINTS    :',IBFW_GL, (TEMP(I),I=1,NPROCS)
    IF(NBFWTIME > 0)THEN
     IF(MSR)WRITE(IPT,101)'!  GWATER DATA BEGIN     :',BFW_TM%TIMES(1)
     IF(MSR)WRITE(IPT,101)'!  GWATER DATA END       :',BFW_TM%TIMES(NBFWTIME)
    END IF
   END IF
   DEALLOCATE(TEMP)

!==============================================================================|
!   Input River/Dam/Intake/Outfall Boundary Values                             |
!==============================================================================|

!
!-------Check Selected Combination for Validity--------------------------------!
!
   REWIND(INRIV)
   READ(INRIV,'(A4,2X,A10)') INFLOW_TYPE,POINT_ST_TYPE
   IF(MSR)WRITE(IOPRT,*) 'River Inflow Information'
   IF(MSR)WRITE(IOPRT,*) 'INFLOW_TYPE==',INFLOW_TYPE
   IF(MSR)WRITE(IOPRT,*) 'POINT_ST_TYPE==',POINT_ST_TYPE

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
!
!--Read in Freshwater Discharge Nodes------------------------------------------!
!
    ALLOCATE(TEMP(NUMQBC_GL),TEMP2(NUMQBC_GL),TEMP3(NUMQBC_GL))
    IF(MSR)THEN
     DO I=1,NUMQBC_GL
      READ(INRIV,*) TEMP(I)
     END DO
    END IF

#   if defined (MULTIPROCESSOR)
    IF(PAR)CALL MPI_BCAST(TEMP,NUMQBC_GL,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
#   endif

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
!      ICELLQ = TEMP(1:NCNT)
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
!----Read in Freshwater Flux Vertical Distribution-----------------------------!
!

!JQI     ALLOCATE(RTEMP1(NUMQBC_GL,KBM1))
!JQI     IF(MSR)THEN
!JQI       DO I = 1, NUMQBC_GL
!JQI         READ(INRIV ,*) J,(RTEMP1(I,K),K = 1,KBM1)
!JQI         WRITE(IOPRT,*) J,(RTEMP1(I,K),K = 1,KBM1)
!JQI       END DO
!JQI     END IF

!JQI#    if defined (MULTIPROCESSOR)
!JQI     IF(PAR)CALL MPI_BCAST(RTEMP1,NUMQBC_GL*KBM1,MPI_F,0,MPI_COMM_WORLD,IERR)
!JQI#    endif

!
!----TRANSFORM TO LOCAL ARRAYS-------------------------------------------------|
!
!JQI     IF(NUMQBC > 0)THEN
!JQI     ALLOCATE(VQDIST(NUMQBC,KBM1))

!JQI     IF(SERIAL) VQDIST = RTEMP1

!JQI#   if defined (MULTIPROCESSOR)
!JQI     IF(PAR)THEN
!JQI       DO I=1,NUMQBC
!JQI         DO K=1,KBM1
!JQI           VQDIST(I,K) = RTEMP1(RIV_GL2LOC(I),K)
!JQI         END DO
!JQI       END DO
!JQI     END IF
!JQI#   endif
!JQI     END IF

!JQI     DEALLOCATE(RTEMP1)

!
!----Read in Time Dependent DataSets (DWDIS)-----------------------------------!
!
    IF(MSR)READ(INRIV,*) NQTIME

#   if defined (MULTIPROCESSOR)
    IF(PAR)CALL MPI_BCAST(NQTIME,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
#   endif

    QBC_TM%NTIMES = NQTIME
    QBC_TM%LABEL  = "Freshwater Discharge"
    ALLOCATE(QBC_TM%TIMES(NQTIME))
    ALLOCATE(RTEMP2(NUMQBC_GL,NTT,NQTIME))

    IF(MSR)THEN
     DO I = 1, NQTIME
      READ(INRIV,*) TTIME
      QBC_TM%TIMES(I) = TTIME
      DO N1 = 1,NTT
       READ(INRIV,*) (RTEMP2(J,N1,I),J = 1,NUMQBC_GL)
      END DO 
      WRITE(IOPRT,5000) TTIME
      DO N1 = 1,NTT 
       WRITE(IOPRT,5000) (RTEMP2(J,N1,I),J = 1,NUMQBC_GL)
      END DO 
     END DO
    END IF

#   if defined (MULTIPROCESSOR)
    IF(PAR)CALL MPI_BCAST(QBC_TM%TIMES,NQTIME,MPI_F,0,MPI_COMM_WORLD,IERR)
    IF(PAR)CALL MPI_BCAST(RTEMP2,NUMQBC_GL*NTT*NQTIME,MPI_F,0,MPI_COMM_WORLD,IERR)
#   endif

!
!----TRANSFORM TO LOCAL ARRAYS-------------------------------------------------|
!
    IF(NUMQBC > 0)THEN
     ALLOCATE(DWDIS(NUMQBC,NTT,NQTIME))

     IF(SERIAL)THEN
      DWDIS(1:NUMQBC_GL,:,:) = RTEMP2(1:NUMQBC_GL,:,:)
     END IF
     
#    if defined (MULTIPROCESSOR)
     IF(PAR)THEN
      DO I=1,NQTIME
       DO N1=1,NTT
        DWDIS(1:NUMQBC,N1,I) = RTEMP2(RIV_GL2LOC(1:NUMQBC),N1,I)
       END DO 
      END DO
     END IF
#    endif

    END IF

    DEALLOCATE(RTEMP2)

    CLOSE(INRIV)
!
!--REPORT RESULTS--------------------------------------------------------------!
!
    ALLOCATE(TEMP(NPROCS))
    ALLOCATE(RBUF(NTT))
    TEMP(1)  = NUMQBC
    ALLOCATE(TEMPD(NTT));    TEMPD = 0.0_SP
!   FTEMP1 = 0.0_SP; FTEMP2 = 0.0_SP; FTEMP3 = 0.0_SP;
    IF(NUMQBC > 0)THEN
     DO N1=1,NTT
      TEMPD = MAXVAL(DWDIS(:,N1,:))
     END DO
    END IF 
    RBUF = TEMPD
   
#   if defined (MULTIPROCESSOR)
    IF(PAR)CALL MPI_GATHER(NUMQBC,1,MPI_INTEGER,TEMP,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
    IF(PAR)THEN
     DO N1=1,NTT
      CALL MPI_REDUCE(TEMPD(N1),RBUF(N1),1,MPI_F,MPI_MAX,0,MPI_COMM_WORLD,IERR)
     END DO
    END IF 
#   endif

   END IF !! NUMQBC_GL > 0

   IF(MSR)WRITE(IPT,*)'!'
   IF(NUMQBC_GL == 0)THEN
    IF(MSR)WRITE(IPT,*)'!  FRESHWATER FLUX       :    NONE'
   ELSE
    IF(MSR)WRITE(IPT,100)'!  FRESHWATER POINTS     :',NUMQBC_GL, (TEMP(I),I=1,NPROCS)
    IF(MSR)CALL GETTIME(TSTRING,3600*INT(QBC_TM%TIMES(1)))
    IF(MSR)WRITE(IPT,102)'!  FWATER DATA BEGIN     :  ',TSTRING
    IF(MSR)CALL GETTIME(TSTRING,3600*INT(QBC_TM%TIMES(QBC_TM%NTIMES)))
    IF(MSR)WRITE(IPT,102)'!  FWATER DATA END       :  ',TSTRING
    DO N1=1,NTT 
     IF(MSR)WRITE(IPT,101)'!  MAX DWDIS --N1--     :',RBUF(N1)
    END DO
    DEALLOCATE(TEMP,TEMPD,RBUF)
   END IF


!
!--Format Statements-----------------------------------------------------------!
!

   100  FORMAT(1X,A26,I6," =>",2X,4(I5,1H,))
   101  FORMAT(1X,A26,F10.4)  
   102  FORMAT(1X,A28,A13)  
   1000 FORMAT(A80)
   5000 FORMAT(8E14.5)

   RETURN
   END SUBROUTINE BCS_FORCE
!==============================================================================|
