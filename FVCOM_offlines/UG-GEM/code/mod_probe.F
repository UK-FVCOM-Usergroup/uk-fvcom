MODULE PROBES 
!==============================================================================!
!  PROBES MOD: CONTROLS TIME SERIES OUTPUT OF QUANTITIES AT SELECT LOCATIONS   !
!                                                                              !
!  CONTROL:  IF PROBE_ON = T IN RUNTIME PARAMETER CONTROL FILE TIME SERIES DUMP! 
!            IS ACTIVATED                                                      !
!                                                                              !
!  SETUP: TIME SERIES SETUP FILES FOR EACH QUANTITY AT EACH LOCATION ARE PLACED!
!         IN THE INPUT DIRECTORY                                               !
!                                                                              !
!  EXAMPLE SETUP FILE:                                                         !
!                                                                              !
!     !===========TIME SERIES FILE FOR OBSERVATION POINT 1
!     D_LOC = 1012 
!     D_TYP = element
!     D_TIT = river_mouth_u
!     D_DES = observation data set 1
!     K_ONE = 1 
!     K_TWO = 1 
!     O_INT = 1
!     VAR   = u
!     VNAME = x-velocity
!                                                                              !
!  SETUP FILE NAMING CONVENTION:                                               !
!     ***_timeseriesXX.dat                                                     !
!     *** = case id tag           (ex: gom/sat/mhb, etc)                       !
!     XX  = digit from 01 to 99, not necessarily sequential                    !
!                                                                              !
!  SETUP VARIABLE DESCRIPTION:                                                 !
!     D_LOC = element/node location of time-series data                        !
!     D_TYP = type of location ("element" or "node")                           !
!     D_TIT = title of location, used for time series output filename          !
!     D_DES = description of data (placed in time series output header)        !
!     O_INT = printing interval for data                                       !
!     K_ONE = initial sigma level 
!     K_TWO = final sigma level 
!     VAR   = variable to output in time series                                !
!     VNAME = variable description (placed in time series output header)       !
!                                                                              !
!     CURRENT VARS SET UP:                                                     !
!       u,v,w,ww,t1,s1,ua,va                                              !
!                                                                              !
!  NOTE:                                                                       !
!    IF TIME SERIES DUMPING IS ACTIVE, CHECK CODE STANDARD OUTPUT FOR          !
!    CORRECTNESS OF TIME SERIES SETUP                                          ! 
!                                                                              !
!  NOTE 2:							               !
!    IF FILE ALREADY EXIST, A NEW FILE WILL BE CREATED NAMED FILENAME-01       !
!    IF FILENAME-01 EXISTS, A NEW FILE WILL BE CREATED NAMED FILENAME-02       !
!    ETC                                                                       !
!==============================================================================!

   USE MOD_PREC
   IMPLICIT NONE
   SAVE
!
!--Probe Object Type               
!
   TYPE PROBE_OBJ          
     INTEGER  :: O_INT                                        !!ITERATION OUTPUT FREQUENCY
     INTEGER  :: D_LOC                                        !!LOCAL ELEMENT/NODE LOCATION   
     INTEGER  :: D_LOC_GL                                     !!GLOBAL ELEMENT/NODE LOCATION   
     INTEGER  :: K_ONE                                        !!INITIAL SIGMA LEVEL
     INTEGER  :: K_TWO                                        !!FINAL SIGMA LEVEL
     INTEGER  :: O_NUM                                        !!OBJECT NUMBER    
     REAL(SP) :: XLOC                                         !!X POSITION OF DATA LOCATION
     REAL(SP) :: YLOC                                         !!Y POSITION OF DATA LOCATION
     REAL(SP) :: DPTH                                         !!BATHYMETRIC DEPTH AT DATA LOC
     REAL(SP), POINTER, DIMENSION(:)              :: TMP      !!USED FOR STORING DATA 
     CHARACTER(LEN=80)                            :: D_TIT    !!OBJECT TITLE (FOR FILENAMING) 
     CHARACTER(LEN=80)                            :: D_DES    !!OBJECT DESCRIPTION
     CHARACTER(LEN=80)                            :: D_TYP    !!LOCATION TYPE ["element","node"] 
     CHARACTER(LEN=80)                            :: VAR      !!VARIABLE TO DUMP
     CHARACTER(LEN=80)                            :: VNAME    !!VARIABLE NAMES 
     CHARACTER(LEN=120)                           :: FILENAME !!FILE NAME      
   END TYPE PROBE_OBJ
   
!
!--Probe Variables                    
!
   INTEGER,PARAMETER             :: MAX_PROBE = 100  !!MAXIMUM NUMBER OF PROBE OBJECTS
   LOGICAL                          PROBE_ON         !!TRUE IF TIME SERIES IS ACTIVE
   INTEGER                          N_PROBE          !!NUMBER OF PROBE OBJECTS 
   TYPE(PROBE_OBJ)               :: PROBE(MAX_PROBE) !!PROBE OBJECTS



   CONTAINS !------------------------------------------------------------------!
            ! SET_PROBES          :   READ PROBE OBJECTS FROM INPUT            !
            ! OPEN_PROBES         :   OPEN PROBE OUTPUT FILES                  !
            ! PROBE_STORE         :   ACCUMULATE DATA IN STORAGE ARRAY         !
            ! DUMP_PROBE_DATA     :   WRITE DATA TO PROBE FILES                !
            ! -----------------------------------------------------------------!

!==============================================================================|
!==============================================================================|

   SUBROUTINE SET_PROBES     

!------------------------------------------------------------------------------|
!  READ IN TIME SERIES OBJECTS FROM INPUT                                      |
!------------------------------------------------------------------------------|

   USE MOD_PREC
   USE ALL_VARS
   USE MOD_INP
#  if defined (MULTIPROCESSOR)
   USE MOD_PAR
#  endif

   IMPLICIT NONE
   LOGICAL FEXIST,ISLOCAL
   INTEGER TOT_PROBES
   CHARACTER(LEN=120) :: FNAME
   CHARACTER(LEN=80 ) :: DATA_TYP,TEMP_TIT
   CHARACTER(LEN=2  ) :: NAC
   INTEGER :: I,II,J,IBND,CNT,DATA_LOC,ISCAN,IERR,PROCMAX,K_ONE_TMP,K_TWO_TMP
   INTEGER, ALLOCATABLE :: PROBE_OBJ_EXIST(:)
   INTEGER, ALLOCATABLE :: OBJ_NUMBER(:)

!------------------------------------------------------------------------------|
!  CHECK ***_run.dat TO SEE IF TIME SERIES LOGGING IS ACTIVATED                |
!------------------------------------------------------------------------------|
                                                                                                             
   FNAME = "./"//trim(casename)//"_run.dat"
                                                                                                             
   ISCAN = SCAN_FILE(TRIM(FNAME),"PROBE_ON",LVAL = PROBE_ON)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING PROBE_ON: ',ISCAN
     IF(ISCAN == -2)THEN
       WRITE(IPT,*)'VARIABLE NOT FOUND IN INPUT FILE: ',TRIM(FNAME)
     END IF
     CALL PSTOP
   END IF

!------------------------------------------------------------------------------|
!  IF TIME SERIES ACTIVE SEARCH FOR TIME SERIES OBJECT FILES IN INPUT DIRECTORY|
!------------------------------------------------------------------------------|
   TOT_PROBES = 0 
   IF(PROBE_ON)THEN

   ALLOCATE(PROBE_OBJ_EXIST(MAX_PROBE),OBJ_NUMBER(MAX_PROBE))
   PROBE_OBJ_EXIST = 0
   CNT = 0
   DO I=1,MAX_PROBE-1
     WRITE(NAC,'(I2.2)')I
     FNAME = "./"//TRIM(INPDIR)//"/"//trim(casename)//'_timeseries'//NAC//'.dat'
     INQUIRE(FILE=TRIM(FNAME),EXIST=FEXIST)
     IF(FEXIST)THEN
       CNT = CNT + 1
       PROBE_OBJ_EXIST(I) = 1
       OBJ_NUMBER(CNT) = I
     END IF
   END DO

   TOT_PROBES = SUM(PROBE_OBJ_EXIST) !!TOTAL NUMBER OF TIME SERIES REQUESTS 
   
!------------------------------------------------------------------------------|
!  OPEN AND READ IN EXISTING TIME SERIES FILES                                 |
!------------------------------------------------------------------------------|

   IF(TOT_PROBES > 0)THEN
   N_PROBE = 0
   DO I=1,TOT_PROBES
     ISLOCAL = .FALSE.

     !=============FILE======================================================
     WRITE(NAC,'(I2.2)')OBJ_NUMBER(I)
     FNAME = "./"//TRIM(INPDIR)//"/"//trim(casename)//'_timeseries'//NAC//'.dat'

     !============SCAN FOR DATA POINT LOCATION==============================
     ISCAN = SCAN_FILE(TRIM(FNAME),"D_LOC",ISCAL = DATA_LOC)
     IF(ISCAN /= 0)THEN
       WRITE(IPT,*)'ERROR READING D_LOC: ',ISCAN
       IF(ISCAN == -2)THEN
         WRITE(IPT,*)'VARIABLE NOT FOUND IN INPUT FILE: ',TRIM(FNAME)
       END IF
       CALL PSTOP 
     END IF
     !============SCAN FOR INITIAL SIGMA LEVEL==============================
     ISCAN = SCAN_FILE(TRIM(FNAME),"K_ONE",ISCAL = K_ONE_TMP)
     IF(ISCAN /= 0)THEN
       WRITE(IPT,*)'ERROR READING K_ONE: ',ISCAN
       IF(ISCAN == -2)THEN
         WRITE(IPT,*)'VARIABLE NOT FOUND IN INPUT FILE: ',TRIM(FNAME)
       END IF
       CALL PSTOP
     END IF
     !============SCAN FOR FINAL SIGMA LEVEL================================
     ISCAN = SCAN_FILE(TRIM(FNAME),"K_TWO",ISCAL = K_TWO_TMP)
     IF(ISCAN /= 0)THEN
       WRITE(IPT,*)'ERROR READING K_TWO: ',ISCAN
       IF(ISCAN == -2)THEN
         WRITE(IPT,*)'VARIABLE NOT FOUND IN INPUT FILE: ',TRIM(FNAME)
       END IF
       CALL PSTOP
     END IF
     !============ENSURE SIGMA RANGE IS TENABLE=============================
     IF(K_ONE_TMP > KB .OR. K_TWO_TMP > KB)THEN
       WRITE(IPT,*)'K RANGE NOT CORRECT IN FILE',FNAME
       WRITE(IPT,*)'MAKE SURE K_ONE AND K_TWO ARE LESS THAN OR EQUAL TO:',KB 
       CALL PSTOP
     END IF
     IF(K_ONE_TMP >  K_TWO_TMP )THEN
       WRITE(IPT,*)'K RANGE NOT CORRECT IN FILE',FNAME
       WRITE(IPT,*)'MAKE SURE K_TWO IS GREATER THAN OR EQUAL TO K_ONE:' 
       CALL PSTOP
     END IF

     !============SCAN FOR DATA POINT TYPE (element/node)===================
     ISCAN = SCAN_FILE(TRIM(FNAME),"D_TYP",CVAL = DATA_TYP)
     IF(ISCAN /= 0)THEN
       WRITE(IPT,*)'ERROR READING D_TYP: ',ISCAN
       IF(ISCAN == -2)THEN
         WRITE(IPT,*)'VARIABLE NOT FOUND IN INPUT FILE: ',TRIM(FNAME)
       END IF
       CALL PSTOP
     END IF

     !============ENSURE DATA POINT TYPE IS CORRECT=========================
     IF(DATA_TYP /= "element" .AND. DATA_TYP /= "node")THEN
       WRITE(IPT,*)'N_TYPE NOT CORRECT IN FILE',FNAME
       WRITE(IPT,*)'IS: ',TRIM(DATA_TYP),' SHOULD BE "element" or "node" '
       CALL PSTOP
     END IF

     !============SEE IF DATA POINT IS IN THE GLOBAL DOMAIN=================
     IF(DATA_TYP == "element")THEN
       IF(DATA_LOC > NGL)THEN 
         WRITE(IPT,*)'DATA LOCATION',DATA_LOC,'FOR TIME SERIES FILE',FNAME
         WRITE(IPT,*)'IS NOT IN GLOBAL DOMAIN: 1 --> ',NGL
         CALL PSTOP
       END IF
     ELSE 
       IF(DATA_LOC > MGL)THEN 
         WRITE(IPT,*)'DATA LOCATION',DATA_LOC,'FOR TIME SERIES FILE',FNAME
         WRITE(IPT,*)'IS NOT IN GLOBAL DOMAIN: 1 --> ',MGL
         CALL PSTOP
       END IF
     END IF
     PROBE(I)%K_ONE = K_ONE_TMP
     PROBE(I)%K_TWO = K_TWO_TMP

     !============SEE IF DATA POINT IS IN THE LOCAL DOMAIN==================
#    if defined (MULTIPROCESSOR)
     IF(NPROCS > 1)THEN
     IF(DATA_TYP == "element")THEN
       IF(ELID(DATA_LOC) > 0) ISLOCAL = .TRUE.
     ELSE 
       IF(NLID(DATA_LOC) > 0) ISLOCAL = .TRUE.
     END IF
     END IF
#    endif

     !==SPECIAL CASE: DATA POINT IS AN INTER-PROCESSOR BOUNDARY NODE========
     !  MUST SELECT ONLY ONE PROCESSOR TO WRITE TIME SERIES
#    if defined (MULTIPROCESSOR)
     IF(PAR)THEN
     IF(DATA_TYP == "node" .AND. NLID(DATA_LOC) > 0)THEN
     IF(NDE_ID(NLID(DATA_LOC)) == 1)THEN   !!BOUNDARY NODE
     DO II=1,NBN
       IF(BN_LST(II) == DATA_LOC) IBND = II
     END DO
       !----Choose Processor of Lowest ID to be responsible for node
       PROCMAX = 10000
       DO J=1,NPROCS
         IF(BN_NEY(IBND,J)==1) THEN
           IF(J < PROCMAX) PROCMAX = J
         END IF
       END DO

       IF(PROCMAX /= MYID) ISLOCAL = .FALSE. !!NOT RESPONSIBLE FOR TIME SERIES
     END IF
     END IF
     END IF
#    endif 
     IF(NPROCS==1)ISLOCAL = .TRUE.
   
     
     IF(.NOT. ISLOCAL)CYCLE   !!BREAK LOOP IF NOT LOCAL

     !============IF LOCAL READ IN TIME SERIES OBJECT===========================
     N_PROBE = N_PROBE + 1


     !-------------Transform Data Location From Global to Local-------------
     PROBE(N_PROBE)%D_LOC_GL = DATA_LOC
     PROBE(N_PROBE)%D_LOC = 0
     IF(NPROCS == 1)THEN
       IF(DATA_TYP == "element") PROBE(N_PROBE)%D_LOC = DATA_LOC
       IF(DATA_TYP == "node") PROBE(N_PROBE)%D_LOC    = DATA_LOC
     END IF
#    if defined (MULTIPROCESSOR)
     IF(NPROCS > 1)THEN
       IF(DATA_TYP == "element") PROBE(N_PROBE)%D_LOC = ELID(DATA_LOC)
       IF(DATA_TYP == "node") PROBE(N_PROBE)%D_LOC    = NLID(DATA_LOC)
     END IF
#    endif

     PROBE(N_PROBE)%D_TYP = DATA_TYP 
     PROBE(N_PROBE)%O_NUM = OBJ_NUMBER(I)
  
     !-------------Scan For Data Title (To Be Used In Filename)-------------
     ISCAN = SCAN_FILE(TRIM(FNAME),"D_TIT",CVAL = TEMP_TIT)  
     IF(ISCAN /= 0)THEN
       WRITE(IPT,*)'ERROR READING D_TIT: ',ISCAN
       IF(ISCAN == -2)THEN
         WRITE(IPT,*)'VARIABLE NOT FOUND IN INPUT FILE: ',TRIM(FNAME)
       END IF
       CALL PSTOP
     END IF
     PROBE(N_PROBE)%D_TIT = TRIM(CASENAME)//"_"//TRIM(TEMP_TIT)//".dat"
     
     !-------------Scan For Data Description (General Info)-----------------
     ISCAN = SCAN_FILE(TRIM(FNAME),"D_DES",CVAL = PROBE(N_PROBE)%D_DES)
     IF(ISCAN /= 0)THEN
       WRITE(IPT,*)'ERROR READING D_DES: ',ISCAN
       IF(ISCAN == -2)THEN
         WRITE(IPT,*)'VARIABLE NOT FOUND IN INPUT FILE: ',TRIM(FNAME)
       END IF
       CALL PSTOP
     END IF
     !-------------Scan For Data Print Interval-----------------------------
     ISCAN = SCAN_FILE(TRIM(FNAME),"O_INT",ISCAL = PROBE(N_PROBE)%O_INT)
     IF(ISCAN /= 0)THEN
       WRITE(IPT,*)'ERROR READING O_INT: ',ISCAN
       IF(ISCAN == -2)THEN
         WRITE(IPT,*)'VARIABLE NOT FOUND IN INPUT FILE: ',TRIM(FNAME)
       END IF
       CALL PSTOP
     END IF
     !-------------Scan For Output Variable---------------------------------
     ISCAN = SCAN_FILE(TRIM(FNAME),"VAR",CVAL = PROBE(N_PROBE)%VAR)
     IF(ISCAN /= 0)THEN
       WRITE(IPT,*)'ERROR READING VAR: ',ISCAN
       IF(ISCAN == -2)THEN
         WRITE(IPT,*)'VARIABLE NOT FOUND IN INPUT FILE: ',TRIM(FNAME)
       END IF
       CALL PSTOP
     END IF
       
     !-------------Scan For Output Variable Descriptor----------------------
     ISCAN = SCAN_FILE(TRIM(FNAME),"VNAME",CVAL = PROBE(N_PROBE)%VNAME)
     IF(ISCAN /= 0)THEN
       WRITE(IPT,*)'ERROR READING VNAME: ',ISCAN
       IF(ISCAN == -2)THEN
         WRITE(IPT,*)'VARIABLE NOT FOUND IN INPUT FILE: ',TRIM(FNAME)
       END IF
       CALL PSTOP
     END IF

     !----------------Set Local Depth/Location Info-----------------------
     IF(PROBE(N_PROBE)%D_TYP == "element")THEN
       PROBE(N_PROBE)%XLOC = XC(PROBE(N_PROBE)%D_LOC) + VXMIN
       PROBE(N_PROBE)%YLOC = YC(PROBE(N_PROBE)%D_LOC) + VYMIN
       PROBE(N_PROBE)%DPTH = H1(PROBE(N_PROBE)%D_LOC) 
     ELSE
       PROBE(N_PROBE)%XLOC = VX(PROBE(N_PROBE)%D_LOC) + VXMIN 
       PROBE(N_PROBE)%YLOC = VY(PROBE(N_PROBE)%D_LOC) + VYMIN 
       PROBE(N_PROBE)%DPTH =  H(PROBE(N_PROBE)%D_LOC) 
     END IF
  END DO
  
  ELSE !! (NO TIME SERIES OBJECTS) 
    IF(MSR)THEN
      WRITE(IPT,*)'!            TIME SERIES OBJECT DATA                '
      WRITE(IPT,*)'!  # TIME SERIES OBJECTS :  NONE FOUND'
    END IF
  END IF

  ELSE !! (PROBE_ON FALSE IN PARAMETER INPUT FILE)
    IF(MSR)THEN
      WRITE(IPT,*)'!            TIME SERIES OBJECT DATA                '
      WRITE(IPT,*)'!  # TIME SERIES LOGGING :  NOT ACTIVE'
    END IF
  END IF
!------------------------------------------------------------------------------|
!  PRINT STATISTICS ON TIME SERIES OBJECTS TO OUTPUT                           |
!------------------------------------------------------------------------------|
   IF(PROBE_ON .AND.  TOT_PROBES > 0)THEN
   IF(MSR)THEN
     WRITE(IPT,*)
     WRITE(IPT,*)'!         TIME SERIES OBJECT DATA               '
     WRITE(IPT,*)" OBJ#  PROC GLOBAL  LOCAL   DATA_TYPE  VAR  FILENAME"
   END  IF
#  if defined (MULTIPROCESSOR)
   IF(NPROCS > 1)CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
#  endif
   DO I=1,N_PROBE  
     WRITE(IPT,101)PROBE(I)%O_NUM,MYID,PROBE(I)%D_LOC_GL,PROBE(I)%D_LOC, &
            TRIM(PROBE(I)%D_TYP),TRIM(PROBE(I)%VAR),PROBE(I)%D_TIT
   END DO
#  if defined (MULTIPROCESSOR)
   IF(NPROCS > 1)CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
#  endif
   END IF

!------------------------------------------------------------------------------|
!  OPEN UP OUTPUT FILES AND WRITE HEADER INFORMATION                           |
!------------------------------------------------------------------------------|
   CALL OPEN_PROBES

!------------------------------------------------------------------------------|
!  ALLOCATE STORAGE DATA AND INITIALIZE                                        |
!------------------------------------------------------------------------------|

  DO I=1,N_PROBE
    ALLOCATE(PROBE(I)%TMP(PROBE(I)%K_ONE:PROBE(I)%K_TWO)) ; PROBE(I)%TMP = 0.0_SP
  END DO
 
101 FORMAT(I5,I5,I8,I8,A10,A8,2X,A40)
   END SUBROUTINE SET_PROBES      

!==============================================================================|

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

!==============================================================================|
                                                                                                                            
   SUBROUTINE OPEN_PROBES   
                                                                                                                            
!------------------------------------------------------------------------------|
!  CREATE FILE NAMES AND WRITE HEADER INFORMATION FOR EACH TS OBJECT           |
!------------------------------------------------------------------------------|
                                                                                                                            
   USE MOD_PREC
   USE ALL_VARS
   IMPLICIT NONE
   INTEGER I,IUNIT,ICNT
   CHARACTER(LEN=120) :: FNAME
   CHARACTER(LEN=2  ) :: NAC 
   CHARACTER(LEN=3  ) :: APPEND
   LOGICAL FEXIST
  
!
!--Open Up Files -> If File Exists Create Secondary File (-01,-02, etc)
!
  
   DO I=1,N_PROBE 
     ICNT  = 0
     FNAME = "./"//TRIM(OUTDIR)//"/timeseries/"//TRIM(PROBE(I)%D_TIT)
     INQUIRE(FILE=FNAME,EXIST=FEXIST)
     IF(FEXIST)THEN
       101 CONTINUE
       ICNT = ICNT + 1
       WRITE(NAC,'(I2.2)')ICNT
       APPEND = "-"//NAC
       FNAME = "./"//TRIM(OUTDIR)//"/timeseries/"//TRIM(PROBE(I)%D_TIT)//TRIM(APPEND)
       INQUIRE(FILE=FNAME,EXIST=FEXIST)
       IF(FEXIST)GOTO 101
     END IF
       
     IUNIT = PROBE(I)%O_NUM + 100
     OPEN(UNIT=IUNIT,FILE=FNAME,FORM='FORMATTED')
     WRITE(IUNIT,*)TRIM(PROBE(I)%D_DES)
     WRITE(IUNIT,*)TRIM(PROBE(I)%VNAME)
     WRITE(IUNIT,*)
     WRITE(IUNIT,*)'         K1            K2           '
     WRITE(IUNIT,'(2(I12,3X))')PROBE(I)%K_ONE,PROBE(I)%K_TWO
     WRITE(IUNIT,*)'     X(M)          Y(M)            DEPTH(M)'
     WRITE(IUNIT,'(3(F12.3,3X))')PROBE(I)%XLOC,PROBE(I)%YLOC,PROBE(I)%DPTH
     WRITE(IUNIT,*)
     WRITE(IUNIT,*)'DATA FOLLOWS:'
     CLOSE(IUNIT)
     PROBE(I)%FILENAME = FNAME
   END DO
                                                                                                                            
   END SUBROUTINE OPEN_PROBES    
!==============================================================================|

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

!==============================================================================|

   SUBROUTINE DUMP_PROBE_DATA
                                                                                                                            
!------------------------------------------------------------------------------|
!  WRITE TIME SERIES DATA TO TIME SERIES DATA FILES                            |
!------------------------------------------------------------------------------|
                                                                                                                            
   USE MOD_PREC
   USE ALL_VARS
   IMPLICIT NONE
   INTEGER I,K,K1,K2,IUNIT

  
!==============================================================================!
!  MAIN LOOP OVER TIME SERIES OUTPUT                                           !
!==============================================================================!
   DO I=1,N_PROBE

!----Determine if Data Location is Owned by This Processor-------------------
     IF(PROBE(I)%D_LOC == 0)CYCLE 

!----Return if not on Time Series Write Interval-----------------------------
     IF(MOD(IINT,PROBE(I)%O_INT) /= 0)CYCLE 

!----Open File For Write-----------------------------------------------------
     IUNIT = PROBE(I)%O_NUM + 100
     OPEN(UNIT=IUNIT,FILE=PROBE(I)%FILENAME,FORM='FORMATTED',POSITION='APPEND')

!----Write Data to File------------------------------------------------------
     K1 = PROBE(I)%K_ONE ; K2 = PROBE(I)%K_TWO
     CALL PROBE_STORE(I)
     WRITE(IUNIT,*)THOUR,(PROBE(I)%TMP(K),K=K1,K2)
     
!----Reset Data Field to Zero------------------------------------------------
     PROBE(I)%TMP = 0.0_SP

  END DO

  RETURN
  END SUBROUTINE DUMP_PROBE_DATA

!==============================================================================!
!  END MAIN LOOP OVER TIME SERIES OUTPUT                                       !
!==============================================================================!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
                                                                                                                            
!==============================================================================|
                                                                                                                            
   SUBROUTINE PROBE_STORE(I)
                                                                                                                            
!------------------------------------------------------------------------------|
!  PUT TIME SERIES DATA IN TEMPORARY ARRAY                                     |
!------------------------------------------------------------------------------|
                                                                                                                            
   USE MOD_PREC
   USE ALL_VARS
   IMPLICIT NONE
   INTEGER, INTENT(IN)         :: I 
   INTEGER                     :: DLOC,K1,K2
   REAL(SP)                    :: FINT
   CHARACTER(LEN=80)           :: DVAR

!--Set Data Location and Variable------------------------------------------
   DLOC  = PROBE(I)%D_LOC
   DVAR  = TRIM(PROBE(I)%VAR)
   K1    = PROBE(I)%K_ONE
   K2    = PROBE(I)%K_TWO

!--Store Data In Temporary Array-------------------------------------------
   SELECT CASE(DVAR)

   CASE("u")
     PROBE(I)%TMP(K1:K2) = U(DLOC,K1:K2)
   CASE("v")
     PROBE(I)%TMP(K1:K2) = V(DLOC,K1:K2)
   CASE("w")
     PROBE(I)%TMP(K1:K2) = W(DLOC,K1:K2)
   CASE("ww")
     PROBE(I)%TMP(K1:K2) = WW(DLOC,K1:K2)
   CASE("q2")
     PROBE(I)%TMP(K1:K2) = Q2(DLOC,K1:K2)
   CASE("q2l")
     PROBE(I)%TMP(K1:K2) = Q2L(DLOC,K1:K2)
   CASE("l")
     PROBE(I)%TMP(K1:K2) = L(DLOC,K1:K2)
   CASE("km")
     PROBE(I)%TMP(K1:K2) = KM(DLOC,K1:K2)
   CASE("kq")
     PROBE(I)%TMP(K1:K2) = KQ(DLOC,K1:K2)
   CASE("kh")
     PROBE(I)%TMP(K1:K2) = KH(DLOC,K1:K2)
   CASE("t1")
     PROBE(I)%TMP(K1:K2) = T1(DLOC,K1:K2)
   CASE("s1")
     PROBE(I)%TMP(K1:K2) = S1(DLOC,K1:K2)
   CASE("rho1")
     PROBE(I)%TMP(K1:K2) = RHO1(DLOC,K1:K2)
   CASE("ua")
     PROBE(I)%TMP(K1:K1) = UA(DLOC)
   CASE("va")
     PROBE(I)%TMP(K1:K1) = VA(DLOC)
   CASE("el")
     PROBE(I)%TMP(K1:K1) = EL(DLOC)
   CASE DEFAULT
     WRITE(IPT,*)'VARIABLE: ',DVAR,' HAS NOT BEEN SET UP'
     WRITE(IPT,*)'FOR TIME SERIES OUTPUT'
     WRITE(IPT,*)'MODIFY MOD_TS.F/SUBROUTINE PROBE_STORE'
     CALL PSTOP
   END SELECT 

   END SUBROUTINE PROBE_STORE
!==============================================================================|

END MODULE PROBES 
!==============================================================================|
