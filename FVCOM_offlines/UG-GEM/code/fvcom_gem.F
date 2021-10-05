   PROGRAM FVCOM_GEM

!==============================================================================!
!  INCLUDE MODULES                                                             !
!==============================================================================!

   USE ALL_VARS
#  if defined (MULTIPROCESSOR)
   USE MOD_PAR  
#  endif

   USE MOD_CLOCK
#  if defined (SPHERICAL)
   USE MOD_SPHERICAL
#  if defined (NORTHPOLE)
   USE MOD_NORTHPOLE
#  endif
#  endif

   USE MOD_NCDIN
   USE MOD_NCDOUT
   USE BCS
!!JQI   USE PROBES     
   USE MOD_BIO_3D
   USE MOD_BIO_OBC
#  if defined (ONE_D_MODEL)
   USE MOD_ONEDTIDE
#  endif   

   USE MOD_OBCS

!------------------------------------------------------------------------------|
   IMPLICIT NONE

   REAL(SP), ALLOCATABLE :: FTEMP(:),FTEMP2(:)
   CHARACTER(LEN=13) :: TSTRING
!JQIJQI   INTEGER :: IH,THOUR_HYD
   REAL(SP) :: IH,THOUR_HYD
   INTEGER  :: I,J
!JQIJQI   
!JQI   real(sp),allocatable :: swrad_tmp(:)
   real(sp) :: SPCP, ROSEA, SPRO
!JQIJQI   

!==============================================================================!
!  FVCOM_GEM VERSION                                                               !
!==============================================================================!

   FVCOM_VERSION     = 'FVCOM_GEM_1.0'
   FVCOM_WEBSITE     = 'http://fvcom.smast.umassd.edu'
   INSTITUTION       = 'School for Marine Science and Technology'
   NETCDF_TIMESTRING = 'seconds after 00:00:00'

!==============================================================================!
!   SETUP PARALLEL ENVIRONMENT                                                 !
!==============================================================================!

   SERIAL = .TRUE. 
   PAR    = .FALSE. 
   MSR    = .TRUE.
   MYID   = 1
   NPROCS = 1
#  if defined (MULTIPROCESSOR)
   CALL INIT_MPI_ENV(MYID,NPROCS,SERIAL,PAR,MSR)
#  endif

!==============================================================================!
!   IMPORT CASENAME FROM COMMAND LINE                                          !
!==============================================================================!

   CALL GET_CASENAME(CASENAME)

!==============================================================================!
!   SETUP MODEL RUN                                                            !
!==============================================================================!

!
!  READ PARAMETERS CONTROLLING MODEL RUN
!
   CALL DATA_RUN

#  if defined (ONE_D_MODEL)
   CALL GET_UMAX
#  endif      
!
!  OPEN INPUT/OUTPUT FILES
!
   CALL IOFILES

!
!  READ PARAMETERS CONTROLLING BIO OBC Series NUDGING
!
#  if defined (BIO_OBC)
   CALL SET_BIO_OBC_PARAM
#  endif   

!
!  DETERMINE NUMBER OF ELEMENTS AND NODES IN THE MODEL
!
   NCFILE =TRIM(HYDFILNA(1))
   CALL NCD_READ_GRID(NCFILE)
   IF(MSR) WRITE(*,*)  '!  # OF NODES            :',MGL
   IF(MSR) WRITE(*,*)  '!  # OF ELEMENTS         :',NGL
   IF(MSR) WRITE(*,*)  '!  # OF SIGMA LAYERS     :',KBM1
   IF(MSR) WRITE(*,*)
   IF(MSR) WRITE(*,*)  '!      MESH READING      :    FINISHED'

   CALL NCD_READ_SHAPE(NCFILE)

   CALL GETBDNO
!
!  READ PARAMETERS CONTROLLING NETCDF OUTPUT
!
   CALL SET_NCD_IO
!
!  DECOMPOSE DOMAIN BY ELEMENTS USING METIS
!
#  if defined (MULTIPROCESSOR) 
   ALLOCATE(EL_PID(NGL))  ;  EL_PID = 1
   IF(PAR)CALL DOMDEC(NPROCS,EL_PID)
#  endif 
!
!  GENERATE GLOBAL<==>LOCAL ELEMENT/NODE MAPPING
!
#  if defined (MULTIPROCESSOR)
   IF(PAR)CALL GENMAP
#  endif
!
!  MAP OPEN BOUNDARY CONDITION NODES TO LOCAL DOMAIN
!
   CALL BCMAP

   KBV=KB
!   CALL GET_PARAMETER
   CALL GET_PARAMETER_NEW
!
!  INPUT AND SETUP BOUNDARY FORCING (HEAT/RIVERS/WIND/etc)
!
   CALL BCS_FORCE

#  if defined (BIO_OBC)
!
!  INPUT BIO OBC Series NUDGING VARIABLES
!
   IF(BIO_OBC_ON) CALL READ_BIO_OBC
#  endif 
!
!  ALLOCATE FLOWFIELD VARIABLES
!
   CALL ALLOC_VARS
!
!  ALLOCATE ELM1 AND ELM2 FOR ORLANSKI RADIATION OPEN BOUNDARY CONDITION
!
   CALL ALLOC_OBC_DATA
!
!  ALLOCATE SPHERICAL COORDINATE SYSTEM VARS
!
#  if defined (SPHERICAL)
   CALL ALLOC_SPHERE_VARS
#  endif
!
!  SHIFT GRID/CORIOLIS/BATHYMETRY TO LOCAL DOMAIN
!
   CALL PDOMDEC

!
!  SET UP GRID METRICS (FLUX EDGES/CONTROL VOLUMES/ETC)
!

   CALL TRIANGLE_GRID_EDGE      !Set up fluxes and control Volumes
   CALL CELL_AREA               !Calculate Element and Control Volume Areas

#  if defined (SPHERICAL) && (NORTHPOLE)
!  FIND THE NODE NUMBER OF NORTH POLE IF EXIST
   CALL FIND_NORTHPOLE
   CALL FIND_CELLSIDE
#  endif   

!  SET ISBCE AND ISONB CORRECTLY IN HALO CELLS/NODES
#  if defined (MULTIPROCESSOR)
   ALLOCATE(FTEMP(0:NT)) ; FTEMP = ISBCE
   IF(PAR)CALL EXCHANGE(EC,NT,1,MYID,NPROCS,FTEMP)
   ISBCE = FTEMP
   DEALLOCATE(FTEMP)
   ALLOCATE(FTEMP(0:MT)) ; FTEMP = ISONB
   IF(PAR)CALL EXCHANGE(NC,MT,1,MYID,NPROCS,FTEMP)
   ISONB = FTEMP
   DEALLOCATE(FTEMP)
#  endif

!
!  SETUP OPEN BOUNDARY METRICS                                   
!
   CALL SETUP_OBC 

!JQI   CALL SET_BNDRY               !Boundary Condition Metrics

!
!  INITIALIZE FLOWFIELD  --> [T,S,U,V,EL,D,Q2,Q2L]
!
   CALL STARTUP

#  if defined (SPHERICAL)              
   CALL CAL_CENTER      
#  endif
!================================================================================
!
!  EXCHANGE SHAPE FACTOR INFORMATION
!
#  if defined (MULTIPROCESSOR)
   IF(PAR)CALL EXCHANGE(EC,NT,1,MYID,NPROCS,ART)
#  endif

!
!  INITIALIZE TIME SERIES OBJECTS 
!
!!JQI   CALL SET_PROBES      

!
!  REPORT STATISTICS ON INITIAL VALUES
!
   CALL REPORT('INITIAL VALUE INFORMATION')

!
!  CALCULATE INTERNAL TIME STEP AND SET INTEGRATION LIMITS
!
   ISTART=IINT+1

   CALL START_CLOCK


!
!  REPORT INTEGRATION INITIAL TIMES 
!
   IF(MSR)CALL REPORT_SIMTIME

   THOUR_HYD = 0
   I_HYD = 1
   IFILE = 1
   
   CALL NCD_READ_TIME(NCFILE)
   
!JQI   open(2,file='./heatflux/gom_hfx.dat',form='unformatted')
!JQI   rewind(2)
!JQI   read(2)
!JQI   read(2)

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  BEGIN MAIN LOOP OVER PHYSICAL TIME STEPS                                    |
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   DO IINT=ISTART,IEND
    TIME =DTI*FLOAT(IINT)/86400.0_SP
    THOUR=DTI*FLOAT(IINT)/3600.0_SP
     
    IH = THOUR
    
    IF(IH >= THOUR_HYD)THEN
     CALL NCD_READ(NCFILE,U,V,UA,VA,KH,EL,S1,T1,SWRAD,I_HYD-1)
     D = H + EL
#    if defined (MULTIPROCESSOR)
     IF(PAR) CALL EXCHANGE(NC,MT,1,MYID,NPROCS,EL)
     IF(PAR) CALL EXCHANGE(NC,MT,1,MYID,NPROCS,D)
#    endif

!!!JQIJQIJQI
     SPCP  = 4.2174E3_SP
     ROSEA = 1.023E3_SP
     SPRO  = SPCP*ROSEA

!JQI     allocate(swrad_tmp(mgl))   ; swrad_tmp = 0.0_sp
!JQI     read(2)
!JQI     read(2) (SWRAD_TMP(i),i=1,mgl)

!JQI      IF(SERIAL) SWRAD(1:MGL) = SWRAD_TMP(1:MGL)

!JQI# if defined (MULTIPROCESSOR)
!JQI      IF(PAR)THEN
!JQI       DO I=1,M
!JQI        SWRAD(I) = SWRAD_TMP(NGID(I))
!JQI       END DO

!JQI       DO I=1,NHN
!JQI        SWRAD(I+M) = SWRAD_TMP(HN_LST(I))
!JQI       END DO
!JQI      END IF
!JQI# endif
!JQI     deallocate(swrad_tmp)
!!!JQIJQIJQI
     
     swrad = -swrad/spro

     CALL N2E2D(EL,EL1)
     CALL N2E2D(D,D1)
     IF(PAR)CALL EXCHANGE(EC,NT,1,MYID,NPROCS,D1) !gwc  [necessary]
     IF(PAR)CALL EXCHANGE(EC,NT,1,MYID,NPROCS,EL1) !gwc  [necessary]

     CALL BCOND_BFW      ! SPECIFY BOTTOM FRESH WATER INPUT BOUNDARY CONDITION

     CALL VISCOF_H       ! Calculate horizontal diffusion coefficient        !
     CALL VERTVL         ! Calculate/Update Sigma Vertical Velocity (Omega)  !

     DO I=1,IOBCN     
       J=I_OBC_N(I)
       UARD_OBCN(I)=-(ELF(J)-EL(J))*ART1(J)/DTI-XFLUX_OBCN(I)
     END DO
       
     THOUR_HYD = THOUR_HYD + INTERVAL_HYD
     I_HYD = I_HYD + 1
       
     IF(I_HYD > ITIME)THEN
      I_HYD = 1
      IFILE = IFILE + 1
      NCFILE = HYDFILNA(IFILE)
      CALL NCD_READ_TIME(NCFILE)
     END IF
    END IF 

!==============================================================================!
!    TURBULENCE MODEL SECTION                                                  |
!==============================================================================!
    IF(VERTMIX == 'closure')THEN
    !===================Original FVCOM MY-2.5/Galperin 1988 Model=============!
    ELSE
     KH = UMOL*VPRNU
    END IF  

#   if defined (MULTIPROCESSOR)
    IF(PAR)CALL EXCHANGE(NC,MT,KB,MYID,NPROCS,KH)
#   endif

    CALL BIO_3D1D(CDF_INT)                          

!==============================================================================!
!   OUTPUT SCREEN REPORT/TIME SERIES DATA/OUTPUT FILES                         |
!==============================================================================!

    IF(MSR)CALL REPORT_TIME(IINT,ISTART,IEND,TIME*86400,IPT) 
    IF(MOD(IINT,IREPORT)==0) CALL REPORT("FLOW FIELD STATS")
!!JQI    CALL DUMP_PROBE_DATA 

#   if !defined (AIX) && !defined(ABSOFT)
    CALL FLUSH(6)
#   endif
!
!-------------UPDATE THE VISUALIZATION SERVER----------------------------------!
!
!     CALL PRINT_VALS
    CALL ARCHIVE

   END DO !!MAIN LOOP
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  END MAIN LOOP OVER PHYSICAL TIME STEPS                                    !
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

!==============================================================================!
!  CLOSE UP COMPUTATION                                                        !
!==============================================================================!
   IINT = IEND
   CALL CLOSEFILES

   CALL REPORT('FINAL VALUES INFORMATION')

   IF(MSR)THEN
    WRITE(IPT,*) ; WRITE(IPT,*)'Computation completed, congratulations!'
    CALL GET_CLOCK 
    CALL GETTIME(TSTRING,INT(TCURRENT-TINIT))
   END IF

   CALL PSTOP
   END PROGRAM FVCOM_GEM
!==============================================================================!
