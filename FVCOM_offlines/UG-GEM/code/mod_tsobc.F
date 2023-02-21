#if defined (TS_OBC)
MODULE MOD_TSOBC
   USE MOD_TYPES
   USE MOD_PREC
   USE MOD_INP
   USE CONTROL
   USE LIMS
# if defined (MULTIPROCESSOR)
   USE MOD_PAR
# endif
   USE MOD_OBCS
   IMPLICIT NONE
!----------------open boundary conditions of temperature and salinity ------------!
   LOGICAL  :: TSOBC_ON                       !!T and S Series OBC ACTIVE
   TYPE(BC)               :: TSO_TM           !!TIME MAPPING FOR TEMP & SALI OBC
   REAL(SP), ALLOCATABLE  :: TEMPOBC(:,:,:)   !!T Series at OB sigma layer
   REAL(SP), ALLOCATABLE  :: SALTOBC(:,:,:)   !!S Series at OB sigma layer
   REAL(SP)               :: ALPHA_SERIES_OBC !!Series OBC NUDGING COEFFICIENT 
   SAVE
   
   CONTAINS  !--------------------------------------------------------------------!
             ! READ_TSOBC  : READ T and S Series at OPEN BOUNDARY SIGMA LAYER  !
             ! SET_TSOBC_PARAM : set parament for T/S obc series nudging       !
	     !--------------------------------------------------------------------! 

   SUBROUTINE READ_TSOBC
!========READ temp & Salinity Series at open boundary sigma layer========!
   USE MOD_CLOCK

   IMPLICIT NONE
   REAL(SP), ALLOCATABLE :: TEMPOBC_GL(:,:,:),SALTOBC_GL(:,:,:)
   REAL(SP), ALLOCATABLE :: TEMP3(:,:,:),TEMP4(:,:,:)
   REAL(SP) :: FTEMP1
   INTEGER :: NCNT,I,J,K,I1,IERR,ntmp
   CHARACTER(LEN=13) :: TSTRING
   CHARACTER(LEN=120) :: FNAME
   LOGICAL :: FEXIST
       
   FNAME = "./"//TRIM(INPDIR)//"/"//trim(casename)//"_tsobc.dat"
!
!--Make Sure T/S OBC TimeSeries File Exists------------------------------------!
!
   INQUIRE(FILE=TRIM(FNAME),EXIST=FEXIST)
   IF(MSR .AND. .NOT.FEXIST)THEN
     WRITE(IPT,*)'CURRENT OBSERVATION FILE: ',FNAME,' DOES NOT EXIST'
     WRITE(IPT,*)'HALTING.....'
     CALL PSTOP
   END IF
!
!--Read Number of T/S OBC TimeSeries-------------------------------------------!
!
      NCNT = 0     
   IF(MSR)THEN
      OPEN(1,FILE=TRIM(FNAME),STATUS='OLD')
   
      REWIND(1)
!
!----Input Number of Data Times for T/S at Open Boundary Every Sigma Layers---!
!
      NCNT = 0     
      DO WHILE(.TRUE.)
        READ(1,*,END=10) FTEMP1
        READ(1,*)
        DO J=1,IOBCN_GL
           READ(1,*)
	   READ(1,*)
        ENDDO	  
        NCNT = NCNT + 1
      END DO
 10   CONTINUE
      REWIND(1)

      IF(NCNT == 0)CALL PERROR(6,"NO DATA PROVIDED FOR TEMP AND SALI OBC")
   END IF

   TSO_TM%NTIMES = NCNT 

#  if defined (MULTIPROCESSOR)
   IF(PAR)CALL MPI_BCAST(TSO_TM%NTIMES,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
#  endif

!
!----Read in Data Times and Global Heat Flux/Short Wave Radiation Data---------!
!

   ALLOCATE(TSO_TM%TIMES(TSO_TM%NTIMES))         ; TSO_TM%TIMES = 0.
   ALLOCATE(TEMPOBC_GL(IOBCN_GL,KBM1,TSO_TM%NTIMES)) ; TEMPOBC_GL = 0.
   ALLOCATE(SALTOBC_GL(IOBCN_GL,KBM1,TSO_TM%NTIMES)) ; SALTOBC_GL = 0.

   IF(MSR)THEN
      DO J=1,TSO_TM%NTIMES

         READ(1,*) TSO_TM%TIMES(J)               !time(days)
	 READ(1,*)
         DO I=1,IOBCN_GL
            READ(1,*) ntmp,(TEMPOBC_GL(I,K,J),K=1,KBM1)   !temp in OB sigma layer
         ENDDO
          DO I=1,IOBCN_GL
            READ(1,*) ntmp,(SALTOBC_GL(I,K,J),K=1,KBM1)   !sali in OB sigma layer
         ENDDO

      END DO
      IF(CASENAME == 'gom') then      
        TSO_TM%TIMES = 24.0*(TSO_TM%TIMES+31.0)    !shift to model hours
      ELSE
        TSO_TM%TIMES = 24.0*(TSO_TM%TIMES)    !shift to model hours
      ENDIF      
!
!--Close T/S OBC TimeSeries Global File---------------------------------------!
!
      CLOSE(1)
   END IF
!
!----Broadcast Data------------------------------------------------------------!
!

#  if defined (MULTIPROCESSOR)
   IF(PAR)THEN
     CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
     CALL MPI_BCAST(TSO_TM%TIMES,TSO_TM%NTIMES,MPI_F,0,MPI_COMM_WORLD,IERR)
     CALL MPI_BCAST(TEMPOBC_GL,TSO_TM%NTIMES*IOBCN_GL*KBM1,MPI_F,0,MPI_COMM_WORLD,IERR)
     CALL MPI_BCAST(SALTOBC_GL,TSO_TM%NTIMES*IOBCN_GL*KBM1,MPI_F,0,MPI_COMM_WORLD,IERR)
   END IF
#  endif

!
!----TRANSFORM TO LOCAL ARRAYS-------------------------------------------------|
!

   IF(SERIAL)THEN
      
      IOBCN    = IOBCN_GL
      ALLOCATE(TEMPOBC(IOBCN,KBM1,TSO_TM%NTIMES)) ; TEMPOBC=0.0
      ALLOCATE(SALTOBC(IOBCN,KBM1,TSO_TM%NTIMES)) ; SALTOBC=0.0

      TEMPOBC(1:IOBCN_GL,:,:) = TEMPOBC_GL(1:IOBCN_GL,:,:)
      SALTOBC(1:IOBCN_GL,:,:) = SALTOBC_GL(1:IOBCN_GL,:,:)
   END IF

#  if defined (MULTIPROCESSOR)
   IF(PAR)THEN
      ALLOCATE(TEMP3(IOBCN_GL,KBM1,TSO_TM%NTIMES))
      ALLOCATE(TEMP4(IOBCN_GL,KBM1,TSO_TM%NTIMES))
      NCNT = 0
      !!SET UP LOCAL OPEN BOUNDARY NODES
      DO I=1,IOBCN_GL
         I1 = NLID( I_OBC_GL(I) )
         IF(I1 /= 0)THEN
           NCNT = NCNT + 1
           TEMP3(NCNT,:,:) = TEMPOBC_GL(I,:,:)
           TEMP4(NCNT,:,:) = SALTOBC_GL(I,:,:)
         END IF
      END DO
      IOBCN = NCNT

      IF(NCNT > 0)THEN
         ALLOCATE(TEMPOBC(NCNT,KBM1,TSO_TM%NTIMES)) ; TEMPOBC = 0.0
         ALLOCATE(SALTOBC(NCNT,KBM1,TSO_TM%NTIMES)) ; SALTOBC = 0.0
         TEMPOBC = TEMP3(1:NCNT,:,:)
         SALTOBC = TEMP4(1:NCNT,:,:)
      END IF

      DEALLOCATE(TEMP3,TEMP4)
   END IF
#  endif

   DEALLOCATE(TEMPOBC_GL,SALTOBC_GL)

   IF(MSR)WRITE(IPT,101)'!  TEMP/SALT OBC READ      :    COMPLETE'
   101  FORMAT(1X,A26,F10.4)  
!
!--REPORT RESULTS--------------------------------------------------------------!
!
   IF(MSR)WRITE(IPT,*)'!'
   IF(MSR)WRITE(IPT,*)'!  TEMP/SALT OBC DATA     :    SET'

   IF(TSO_TM%NTIMES > 0)THEN
     CALL GETTIME(TSTRING,3600*INT(TSO_TM%TIMES(1)))
     IF(MSR)WRITE(IPT,102)'!  TEMP/SALT DATA BEGIN  :  ',TSTRING        
     CALL GETTIME(TSTRING,3600*INT(TSO_TM%TIMES(TSO_TM%NTIMES)))
     IF(MSR)WRITE(IPT,102)'!  TEMP/SALT DATA END    :  ',TSTRING
   END IF
   102  FORMAT(1X,A28,A13)  

   RETURN
   END SUBROUTINE READ_TSOBC

!==============================================================================|
!   ste parament for temp and salt obc series nudging                          |
!==============================================================================|
   SUBROUTINE SET_TSOBC_PARAM
   USE MOD_PREC
   USE CONTROL
   IMPLICIT NONE
   INTEGER  INTVEC(150),ISCAN,KTEMP
   CHARACTER(LEN=120) :: FNAME
   FNAME = "./"//trim(casename)//"_run.dat"
!------------------------------------------------------------------------------|
!     "TSOBC_ON"   !! 
!------------------------------------------------------------------------------|     
   ISCAN = SCAN_FILE(TRIM(FNAME),"TSOBC_ON",LVAL = TSOBC_ON)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING TSOBC_ON: ',ISCAN
     IF(ISCAN == -2)THEN
       WRITE(IPT,*)'VARIABLE NOT FOUND IN INPUT FILE: ',TRIM(FNAME)
     END IF
     CALL PSTOP 
   END IF
   
!------------------------------------------------------------------------------|
!     "ALPHA_SERIES_OBC"   !!
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"ALPHA_SERIES_OBC",FSCAL = ALPHA_SERIES_OBC)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING ALPHA_SERIES_OBC: ',ISCAN
     CALL PSTOP
   END IF
   IF(.not.TSOBC_ON) ALPHA_SERIES_OBC = 0.0_SP

!==============================================================================|
!            SCREEN REPORT OF SET T and S Series OBC  VARIABlES                !
!==============================================================================|
   IF(MSR) THEN  
     WRITE(IOPRT,*) '!                                                   !'     
     WRITE(IOPRT,*) '!------SPECIFY T/S Series OBC NUDGING VARIABlES-----!'     
     WRITE(IOPRT,*) '!                                                   !'     
     WRITE(IOPRT,*) '!  # TSOBC_ON              :',TSOBC_ON
     WRITE(IOPRT,*) '!  # ALPHA_SERIES_OBC      :',ALPHA_SERIES_OBC
     WRITE(IOPRT,*) '!---------------------------------------------------!'     
     
   END IF
   
   
   RETURN
   END SUBROUTINE SET_TSOBC_PARAM
      
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

END MODULE MOD_TSOBC
# else
  SUBROUTINE DUM5
  END SUBROUTINE DUM5
# endif
