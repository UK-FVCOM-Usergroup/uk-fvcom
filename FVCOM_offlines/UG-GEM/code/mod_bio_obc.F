MODULE MOD_BIO_OBC
#if defined (BIO_OBC)
   USE MOD_TYPES
   USE MOD_PREC
   USE MOD_INP
   USE CONTROL
   USE LIMS
   USE MOD_1D
# if defined (MULTIPROCESSOR)
   USE MOD_PAR
# endif
   USE MOD_OBCS
   USE MOD_NUTRIENT, ONLY: NNN,INN
   IMPLICIT NONE
!----------------open boundary conditions of temperature and salinity ------------!
   LOGICAL  :: BIO_OBC_ON                       !!BIO Series OBC ACTIVE
   TYPE(BC)               :: BIO_O_TM           !!TIME MAPPING FOR BIO OBC
   REAL(SP), ALLOCATABLE  :: BIOOBC(:,:,:,:)   !!BIO Series at OB sigma layer
   REAL(SP)               :: ALPHA_SERIES_OBC   !!Series OBC NUDGING COEFFICIENT 
   SAVE
   
   CONTAINS  !--------------------------------------------------------------------!
             ! READ_BIO_OBC  : READ BIO Series at OPEN BOUNDARY SIGMA LAYER       !
             ! SET_BIO_OBC_PARAM : set parament for BIO obc series nudging        !
	     !--------------------------------------------------------------------! 

   SUBROUTINE READ_BIO_OBC
!========READ biological series at open boundary sigma layer========!
   USE MOD_CLOCK

   IMPLICIT NONE
   REAL(SP), ALLOCATABLE :: BIOOBC_GL(:,:,:,:)
   REAL(SP), ALLOCATABLE :: TEMP3(:,:,:,:)
   REAL(SP) :: FTEMP1
   INTEGER :: NCNT,I,J,K,I1,IERR,ntmp,N1
   CHARACTER(LEN=13) :: TSTRING
   CHARACTER(LEN=120) :: FNAME
   LOGICAL :: FEXIST
       
   FNAME = "./"//TRIM(INPDIR)//"/"//trim(casename)//"_bio_obc.dat"
   print*,FNAME
!
!--Make Sure BIO OBC TimeSeries File Exists------------------------------------!
!
   INQUIRE(FILE=TRIM(FNAME),EXIST=FEXIST)
   IF(MSR .AND. .NOT.FEXIST)THEN
     WRITE(IPT,*)'CURRENT OBSERVATION FILE: ',FNAME,' DOES NOT EXIST'
     WRITE(IPT,*)'HALTING.....'
     CALL PSTOP
   END IF
!
!--Read Number of BIO OBC TimeSeries-------------------------------------------!
!
   NCNT = 0     
   IF(MSR)THEN
    OPEN(1,FILE=TRIM(FNAME),STATUS='OLD')
    REWIND(1)
!
!----Input Number of Data Times for BIO at Open Boundary Every Sigma Layers---!
!
    NCNT = 0     
    DO WHILE(.TRUE.)
     READ(1,*,END=10) FTEMP1
     READ(1,*)
!!JQIJQI     DO N1=1,NTT
      DO J=1,IOBCN_GL
       READ(1,*)
      ENDDO	  
!!JQIJQI     END DO 
     NCNT = NCNT + 1
    END DO
 10 CONTINUE
    REWIND(1)

    IF(NCNT == 0)CALL PERROR(6,"NO DATA PROVIDED FOR BIO OBC")
   END IF

   BIO_O_TM%NTIMES = NCNT 

#  if defined (MULTIPROCESSOR)
   IF(PAR)CALL MPI_BCAST(BIO_O_TM%NTIMES,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
#  endif

!
!----Read in Data Times and Global Heat Flux/Short Wave Radiation Data---------!
!
   ALLOCATE(BIO_O_TM%TIMES(BIO_O_TM%NTIMES))              ; BIO_O_TM%TIMES = 0.
   ALLOCATE(BIOOBC_GL(IOBCN_GL,KBM1,NTT,BIO_O_TM%NTIMES)) ; BIOOBC_GL = 0.

   IF(MSR)THEN
    DO J=1,BIO_O_TM%NTIMES
     READ(1,*) BIO_O_TM%TIMES(J)               !time(days)
     READ(1,*)
!JQIJQI     DO N1=1,NTT
     DO N1=1,NNN
      DO I=1,IOBCN_GL
       READ(1,*) ntmp,(BIOOBC_GL(I,K,N1+INN-1,J),K=1,KBM1)   !bio in OB sigma layer
      END DO 
     ENDDO
    END DO
    BIO_O_TM%TIMES = 24.0*(BIO_O_TM%TIMES)    !shift to model hours
!
!--Close BIO OBC TimeSeries Global File---------------------------------------!
!
    CLOSE(1)
   END IF
!
!----Broadcast Data------------------------------------------------------------!
!

#  if defined (MULTIPROCESSOR)
   IF(PAR)THEN
    CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
    CALL MPI_BCAST(BIO_O_TM%TIMES,BIO_O_TM%NTIMES,MPI_F,0,MPI_COMM_WORLD,IERR)
    CALL MPI_BCAST(BIOOBC_GL,BIO_O_TM%NTIMES*IOBCN_GL*KBM1*NTT,MPI_F,0,MPI_COMM_WORLD,IERR)
   END IF
#  endif

!
!----TRANSFORM TO LOCAL ARRAYS-------------------------------------------------|
!

   IF(SERIAL)THEN
    IOBCN    = IOBCN_GL
    ALLOCATE(BIOOBC(IOBCN,KBM1,NTT,BIO_O_TM%NTIMES)) ; BIOOBC=0.0
    BIOOBC(1:IOBCN_GL,:,:,:) = BIOOBC_GL(1:IOBCN_GL,:,:,:)
   END IF

#  if defined (MULTIPROCESSOR)
   IF(PAR)THEN
    ALLOCATE(TEMP3(IOBCN_GL,KBM1,NTT,BIO_O_TM%NTIMES))
    NCNT = 0
    !!SET UP LOCAL OPEN BOUNDARY NODES
    DO I=1,IOBCN_GL
     I1 = NLID( I_OBC_GL(I) )
     IF(I1 /= 0)THEN
      NCNT = NCNT + 1
      TEMP3(NCNT,:,:,:) = BIOOBC_GL(I,:,:,:)
     END IF
    END DO
    IOBCN = NCNT

    IF(NCNT > 0)THEN
     ALLOCATE(BIOOBC(NCNT,KBM1,NTT,BIO_O_TM%NTIMES)) ; BIOOBC = 0.0
     BIOOBC = TEMP3(1:NCNT,:,:,:)
    END IF

    DEALLOCATE(TEMP3)
   END IF
#  endif

   DEALLOCATE(BIOOBC_GL)

   IF(MSR)WRITE(IPT,101)'!  BIO OBC READ      :    COMPLETE'
101 FORMAT(1X,A26,F10.4)  
!
!--REPORT RESULTS--------------------------------------------------------------!
!
   IF(MSR)WRITE(IPT,*)'!'
   IF(MSR)WRITE(IPT,*)'!  BIO OBC DATA     :    SET'

   IF(BIO_O_TM%NTIMES > 0)THEN
    CALL GETTIME(TSTRING,3600*INT(BIO_O_TM%TIMES(1)))
    IF(MSR)WRITE(IPT,102)'!  BIO DATA BEGIN  :  ',TSTRING        
    CALL GETTIME(TSTRING,3600*INT(BIO_O_TM%TIMES(BIO_O_TM%NTIMES)))
    IF(MSR)WRITE(IPT,102)'!  BIO DATA END    :  ',TSTRING
   END IF
102 FORMAT(1X,A28,A13)  

   RETURN
   END SUBROUTINE READ_BIO_OBC

!==============================================================================|
!   set parament for time series of biological values nudging                  |
!==============================================================================|
   SUBROUTINE SET_BIO_OBC_PARAM
   USE MOD_PREC
   USE CONTROL
   IMPLICIT NONE
   INTEGER  INTVEC(150),ISCAN,KTEMP
   CHARACTER(LEN=120) :: FNAME
   FNAME = "./"//trim(casename)//"_run.dat"
!------------------------------------------------------------------------------|
!     "BIO_OBC_ON"   !! 
!------------------------------------------------------------------------------|     
   ISCAN = SCAN_FILE(TRIM(FNAME),"BIO_OBC_ON",LVAL = BIO_OBC_ON)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING BIO_OBC_ON: ',ISCAN
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
   IF(.NOT. BIO_OBC_ON) ALPHA_SERIES_OBC = 0.0_SP

!==============================================================================|
!            SCREEN REPORT OF SET BIO Series OBC  VARIABlES                    !
!==============================================================================|
   IF(MSR) THEN  
     WRITE(IOPRT,*) '!                                                   !'     
     WRITE(IOPRT,*) '!------SPECIFY BIO Series OBC NUDGING VARIABlES-----!'     
     WRITE(IOPRT,*) '!                                                   !'     
     WRITE(IOPRT,*) '!  # BIO_OBC_ON            :',BIO_OBC_ON
     WRITE(IOPRT,*) '!  # ALPHA_SERIES_OBC      :',ALPHA_SERIES_OBC
     WRITE(IOPRT,*) '!---------------------------------------------------!'     
     
   END IF
   
   
   RETURN
   END SUBROUTINE SET_BIO_OBC_PARAM
      
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
# endif
END MODULE MOD_BIO_OBC
