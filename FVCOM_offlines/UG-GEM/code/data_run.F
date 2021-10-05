!==============================================================================|
!   Input Parameters Which Control the Model Run                               |
!==============================================================================|

   SUBROUTINE DATA_RUN            

!------------------------------------------------------------------------------|

   USE ALL_VARS
   USE MOD_UTILS
   USE MOD_INP

   IMPLICIT NONE
   REAL(SP) REALVEC(150)
   CHARACTER(LEN=80) :: CHARVEC(150)
   INTEGER  INTVEC(150),ISCAN,KTEMP
   REAL(SP) ZKUTMP,ZKLTMP
   CHARACTER(LEN=120) :: FNAME
   INTEGER I


!==============================================================================|
!   READ IN VARIABLES AND SET VALUES                                           |
!==============================================================================|

   FNAME = "./"//trim(casename)//"_run.dat"

!------------------------------------------------------------------------------|
!     "INFO FILE"   !!
!------------------------------------------------------------------------------|

! David Changed default IPT to 6 to print error reading infofile to screen
   IPT =6
   ISCAN = SCAN_FILE(FNAME,"INFOFILE",CVAL = INFOFILE)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING INFOFILE: ',ISCAN
     CALL PSTOP
   END IF

!
!-----------------OPEN RUNTIME INFO FILE---------------------------------------!
!
   IF(TRIM(INFOFILE) /= "screen")THEN
      IPT = 71
      CALL FOPEN(IPT, TRIM(INFOFILE) ,"ofr")
!   ELSE
!     IPT = 6
   END IF

!
!-----------------WRITE BANNER ------------------------------------------------!
!
   IF(NPROCS > 1 .AND. MSR)THEN
   CALL WRITE_BANNER(IPT)
   END IF


!------------------------------------------------------------------------------|
!   INTERNAL TIME STEP (DTI) 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(TRIM(FNAME),"DTI",FSCAL = DTI)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING DTI: ',ISCAN
     CALL PSTOP 
   END IF
!------------------------------------------------------------------------------|
!     "NSTEPS"   -NUMBER OF TIME STEPS TO RUN MODEL                  
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"NSTEPS",ISCAL = NSTEPS)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING NSTEPS: ',ISCAN
     CALL PSTOP 
   END IF

!------------------------------------------------------------------------------|
!     "RESTART -CONTROLS RESTART TYPE 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"RESTART",CVAL = RESTART)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING RESTART: ',ISCAN
     CALL PSTOP 
   END IF
!------------------------------------------------------------------------------|
!     "HORZMIX"   -HORIZONTAL DIFFUSION TYPE
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"HORZMIX",CVAL = HORZMIX)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING HORZMIX: ',ISCAN
     CALL PSTOP 
   END IF
!------------------------------------------------------------------------------|
!     "HORCON"   !!HORIZONTAL DIFFUSION COEFFICIENT
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"HORCON",FSCAL = HORCON)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING HORCON: ',ISCAN
     CALL PSTOP
   END IF
!------------------------------------------------------------------------------|
!     "HPRNU"   !!HORIZONTAL DIFFUSION COEFFICIENT
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"HPRNU",FSCAL = HPRNU)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING HPRNU: ',ISCAN
     CALL PSTOP
   END IF
!------------------------------------------------------------------------------|
!     "VERTMIX"   !!VERTICAL DIFFUSION TYPE
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"VERTMIX",CVAL = VERTMIX)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING VERTMIX: ',ISCAN
     CALL PSTOP 
   END IF
!------------------------------------------------------------------------------|
!     "UMOL"   !!VERTICAL DIFFUSION COEFFICIENT
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"UMOL",FSCAL = UMOL)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING UMOL: ',ISCAN
     CALL PSTOP
   END IF
!------------------------------------------------------------------------------|
!     "VPRNU"   !!VERTICAL DIFFUSION COEFFICIENT
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"VPRNU",FSCAL = VPRNU)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING VPRNU: ',ISCAN
     CALL PSTOP
   END IF

!------------------------------------------------------------------------------|
!     "IRESTART"   !!
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"IRESTART",ISCAL = IRESTART)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING IRESTART: ',ISCAN
     CALL PSTOP 
   END IF
!------------------------------------------------------------------------------|
!     "IREPORT"   !!
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"IREPORT",ISCAL = IREPORT)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING IREPORT: ',ISCAN
     CALL PSTOP
   END IF

!------------------------------------------------------------------------------|
!     "KSL"   !!
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"KSL",ISCAL = KSL)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING KSL: ',ISCAN
     CALL PSTOP
   END IF

!------------------------------------------------------------------------------|
!     "DPTHSL"   !! 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"DPTHSL",FVEC = REALVEC,NSZE = KTEMP)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING DPTHSL: ',ISCAN
     CALL PSTOP
   END IF
   IF(MSR)THEN
     IF(KTEMP /= KSL)THEN
       WRITE(*,*)'NUMBER OF SPECIFIED DEPTHS IN DEPTHSL IS NOT EQUAL TO KSL' 
       WRITE(*,*)'KSL: ',KSL
       WRITE(*,*)'DPTHSL: ',REALVEC       !DPTHSL
     END IF
   END IF
  
   ALLOCATE(DPTHSL(KSL))
   DPTHSL(1:KSL)= REALVEC(1:KSL)

!------------------------------------------------------------------------------|
!     "MGL"   !!
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"MGL",ISCAL = MGL)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING MGL: ',ISCAN
     CALL PSTOP 
   END IF

!------------------------------------------------------------------------------|
!     "NGL"   !!
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"NGL",ISCAL = NGL)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING NGL: ',ISCAN
     CALL PSTOP 
   END IF

!------------------------------------------------------------------------------|
!     "KB"   !!
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"KB",ISCAL = KB)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING KB: ',ISCAN
     CALL PSTOP 
   END IF

!------------------------------------------------------------------------------|
!     "CASETITLE"   !!
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"CASETITLE",CVAL = CASETITLE)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING CASETITLE: ',ISCAN
     CALL PSTOP 
   END IF

!------------------------------------------------------------------------------|
!     "INPDIR"   !!DIRECTORY FOR INPUT FILES            
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"INPDIR",CVAL = INPDIR)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING INPDIR: ',ISCAN
     CALL PSTOP 
   END IF
   I = LEN_TRIM(INPDIR)
   IF(INPDIR(I:I) == "/") INPDIR(I:I) = " "

!------------------------------------------------------------------------------|
!     "OUTDIR"   !!
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"OUTDIR",CVAL = OUTDIR)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING OUTDIR: ',ISCAN
     CALL PSTOP 
   END IF
   I = LEN_TRIM(OUTDIR)
   IF(OUTDIR(I:I) == "/") OUTDIR(I:I) = " "

!------------------------------------------------------------------------------|
!  BIO_VARIABLES CONTROL FLAG
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(TRIM(FNAME),"BIO_FCT",LVAL = BIO_FCT)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING BIO_FCT: ',ISCAN
     IF(ISCAN == -2)THEN
       WRITE(IPT,*)'VARIABLE NOT FOUND IN INPUT FILE: ',TRIM(FNAME)
       WRITE(IPT,*)'PLEASE ADD LOGICAL (T/F) VARIABLE "BIO_FCT" TO INPUT FILE'
     END IF
     CALL PSTOP
   END IF
                                                                                                                            
!------------------------------------------------------------------------------|
!     "NOHYDFILNA"   -NUMBER OF HYD NETCDF FILES                  
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"NOHYDFILNA",ISCAL = NOHYDFILNA)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING NOHYDFILNA: ',ISCAN
     CALL PSTOP 
   END IF

!------------------------------------------------------------------------------|
!     "HYDFILNA"   !! 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"HYDFILNA",CVEC = CHARVEC,NSZE = KTEMP)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING HYDFILNA: ',ISCAN
     CALL PSTOP
   END IF
   IF(MSR)THEN
     IF(KTEMP /= NOHYDFILNA)THEN
       WRITE(*,*)'NUMBER OF NETCDF HYDRO FILES IS NOT EQUAL TO NOHYDFILNA' 
       WRITE(*,*)'NOHYDFILNA: ',NOHYDFILNA
       WRITE(*,*)'NUMBER OF HYDFILNA: ',CHARVEC       
     END IF
   END IF
  
   ALLOCATE(HYDFILNA(NOHYDFILNA))
   HYDFILNA(1:NOHYDFILNA)= CHARVEC(1:NOHYDFILNA)

!------------------------------------------------------------------------------|
!     "INTERVAL_HYD"   -THE INTERVAL (IN HOURS) OF DATA IN HYD NETCDF FILES                  
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"INTERVAL_HYD",FSCAL = INTERVAL_HYD)
   IF(ISCAN /= 0)THEN
     WRITE(IPT,*)'ERROR READING INTERVAL_HYD: ',ISCAN
     CALL PSTOP 
   END IF

!==============================================================================|
!            SET PHYSICAL PARAMETERS                                           !
!==============================================================================|

   KBM1 = KB-1 ; KBM2 = KB-2 
   IEND=NSTEPS
   DAYS=NSTEPS*DTI/24.0_SP/3600.0_SP
   IINT = 0
   IF(IREPORT == 0) IREPORT = IEND+2

!==============================================================================|
!            ERROR CHECKING                                                    !
!==============================================================================|


   IF(RESTART /= 'cold_start' .AND. RESTART /= 'hot_cold_s' .AND. &
         RESTART /= 'hot_start')THEN
     IF(MSR)WRITE(IPT,*) 'RESTART NOT CORRECT --->',RESTART   
     IF(MSR)WRITE(IPT,*) 'SHOULD BE "cold_start","hot_cold_s", or "hot_start"'
     STOP
   END IF
   IF(HORZMIX /= 'constant' .AND. HORZMIX /= 'closure') THEN
     IF(MSR)WRITE(IPT,*) 'HORZMIX NOT CORRECT --->',HORZMIX  
     IF(MSR)WRITE(IPT,*) 'SHOULD BE "constant" or "closure"'
     STOP
   END IF

   IF(KB > 200)THEN
     WRITE(IPT,*)'KB EXCEEDS 200'
     WRITE(IPT,*)'THIS WILL CAUSE ERROR IN SEVERAL READ STATEMENTS SINCE'
     WRITE(IPT,*)'ASSOCIATED FORMAT STATEMENT ASSUMES MAX KB OF 200'
     WRITE(IPT,*)'GREP CODE FOR READ AND 200 TO SEE'
     call PSTOP
   END IF

!==============================================================================|
!            REPORTING                                                         !
!==============================================================================|
   IF(MSR)WRITE(IPT,*)'!  # STD SALINITY LEVELS :',KSL
   IF(MSR)WRITE(IPT,*)'!  # OF SIGMA LEVELS     :',KB

!==============================================================================|
!            SCREEN REPORT OF SET VARIABlES                                    !
!==============================================================================|
   IF(MSR)THEN
   WRITE(IPT,*)'!  # DTI                 :',DTI
   WRITE(IPT,*)'!  # NSTEPS              :',NSTEPS
   WRITE(IPT,*)'!  # RESTART             :',TRIM(RESTART)
   WRITE(IPT,*)'!  # HORZMIX             :',TRIM(HORZMIX)
   WRITE(IPT,*)'!  # HORCON              :',HORCON
   WRITE(IPT,*)'!  # HPRNU               :',HPRNU
   WRITE(IPT,*)'!  # VERTMIX             :',TRIM(VERTMIX)
   WRITE(IPT,*)'!  # UMOL                :',UMOL
   WRITE(IPT,*)'!  # VPRNU               :',VPRNU
   WRITE(IPT,*)'!  # IRESTART            :',IRESTART   
   WRITE(IPT,*)'!  # IREPORT             :',IREPORT    
   WRITE(IPT,*)'!  # KSL                 :',KSL      
   WRITE(IPT,*)'!  # DPTHSL              :',DPTHSL   
   WRITE(IPT,*)'!  # KB                  :',KB   
   WRITE(IPT,*)'!  # CASETITLE           :',TRIM(CASETITLE)
   WRITE(IPT,*)'!  # OUTDIR              :',TRIM(OUTDIR   )
   WRITE(IPT,*)'!  # INPDIR              :',TRIM(INPDIR   )
   WRITE(IPT,*)'!  # INFOFILE            :',TRIM(INFOFILE )

   IF(BIO_FCT)THEN
     WRITE(IPT,*)'!  # TEMP/SAL AVERAGING  :  ACTIVE'
   ELSE
     WRITE(IPT,*)'!  # TEMP/SAL AVERAGING  :  INACTIVE'
   END IF
  
END IF


!==============================================================================|
!            FORMATS                                                           |
!==============================================================================|
 101  FORMAT(A10," = ",F10.6)
 102  FORMAT(A10," = ",I10)
 103  FORMAT(A10," = ",A25)
1000  FORMAT (80a1)
4000  FORMAT (3i10,1x,a10)
5000  FORMAT (a10,2e10.3)
6000  FORMAT (3(2x,a8),4x,a6)

   RETURN
   END SUBROUTINE DATA_RUN    
!------------------------------------------------------------------------------|
