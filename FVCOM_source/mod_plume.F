! ---------------------------------------------------------------------
! This model estimates the flux of tracer from a point release at the
! bottom. It is based on idealized physics proposed by Morton, Talyer &
! Turner, 1956: "Turbulent gravitational convection" (the MTT model)
      
! The tracer flux at the fluid-source interface is read, the model
! estimates the depth the plume will reach a hydrostatically stable
! configuration based on entrainment of water from the sides.
! A net flux of temperature, salinity and tracer is
! released at the depth predicted by the MTT model.

! Important note!:
! ---
! MTT theory assumes that the buoyancy flux and stratification is
! independent of time. However, the stratification changes with 
! with time in FVCOM, and this module updates the source buoyancy flux
! continuously since the sea water density changes with time.
!  This plume model assumes that the plume adapts to the changing 
! environment and reaches a stable configuration instantaneously. 
! This is not the case in nature, and should be accounted for in 
! the future development of this code.
       
! Copyright (c) Akaplan-niva 2019
! ----------------------------------------------------------------------
! REVISION HISTORY:
! Original authors: Håvard Espenes     (hes@akvaplan.niva.no)
!                   Qin Zhou           (qin@akvaplan.niva.no)
!                   Ole Anders Nøst    (oan@akvaplan.niva.no)
!                   -> March 2019

! Ps. To use this module, you must activate both "PLUME" and
!     "PLUME_CALCULATED" when you compile the code.

MODULE MOD_PLUME
    USE ALL_VARS
    USE MOD_UTILS
    USE MOD_PREC
    IMPLICIT NONE
    SAVE
	
    LOGICAL PLUME_CALCULATE_ON
    LOGICAL PLUME_DIFFUSOR_ON
    INTEGER(SP) :: PLUME_RELEASE_NODEID          ! GLOBAL NODE ID WHERE PLUME IS RELEASED
    INTEGER(SP) :: PLUME_RELEASE_SIGMALAYER      ! THE SIGMA LAYER WHERE PLUME IS RELEASED
    REAL(SP)    :: PLUME_DIFFUSOR_UP             ! Number of diffusors pointing down
    REAL(SP)    :: PLUME_DIFFUSOR_DOWN           ! Number of diffusors pointing upward
    REAL(SP)    :: PLUME_DIFFUSOR_DISTANCE       ! Distance between diffusors
    REAL(SP)    :: PLUME_DIFFUSOR_RADIUS         ! Radius of the plume source
    REAL(SP)    :: PLUME_DIFFUSOR_DEPTH          ! Depth between bottom and diffusor
    REAL(SP)    :: PLUME_INITIAL_TEMPERATURE     ! INITIAL PLUME TEMPERATURE
    REAL(SP)    :: PLUME_INITIAL_SALINITY        ! INITIAL PLUME SALINITY
    REAL(SP)    :: PLUME_INITIAL_CONCENTRATION   ! INITIAL PLUME CONCENTRATION
    REAL(SP)    :: PLUME_INITIAL_VOLUME_FLUX     ! INITIAL PLUME VOLUMN FLUX
    REAL(SP)    :: PIPE_RADIUS                   ! RADIUS OF THE PIPE PROVIDING THE VOLUMEFLUX

    NAMELIST /NML_PLUME_CALCULATED/            &
    & PLUME_CALCULATE_ON,                      &
    & PLUME_DIFFUSOR_ON,                       &
    & PLUME_RELEASE_NODEID,                    &
    & PLUME_RELEASE_SIGMALAYER,                &
    & PLUME_DIFFUSOR_UP,                       &
    & PLUME_DIFFUSOR_DOWN,                     &
    & PLUME_DIFFUSOR_DISTANCE,                 &
    & PLUME_DIFFUSOR_RADIUS,                   &
    & PLUME_DIFFUSOR_DEPTH,                    &
    & PLUME_INITIAL_TEMPERATURE,               &
    & PLUME_INITIAL_SALINITY,                  &
    & PLUME_INITIAL_CONCENTRATION,             &
    & PLUME_INITIAL_VOLUME_FLUX,               &
    & PIPE_RADIUS
	
    CONTAINS

!==============================================================================|
!   Input Parameters for the Plume Model                
!==============================================================================|

      SUBROUTINE PLUME_CALCULATE_NAMELIST_INITIALIZE
      USE control, only : casename
      IMPLICIT NONE
      
      PLUME_CALCULATE_ON            =  .FALSE.                
      PLUME_DIFFUSOR_ON             =  .FALSE.
      PLUME_RELEASE_NODEID          =  0                 
      PLUME_RELEASE_SIGMALAYER      =  0   
      PLUME_DIFFUSOR_UP             =  0.0_SP
      PLUME_DIFFUSOR_DOWN           =  0.0_SP
      PLUME_DIFFUSOR_DISTANCE       =  0.0_SP
      PLUME_DIFFUSOR_RADIUS         =  0.0_SP
      PLUME_DIFFUSOR_DEPTH          =  0.0_SP
      PLUME_INITIAL_TEMPERATURE     =  0.0_SP          
      PLUME_INITIAL_SALINITY        =  0.0_SP        
      PLUME_INITIAL_CONCENTRATION   =  0.0_SP              
      PLUME_INITIAL_VOLUME_FLUX     =  0.0_SP            
      PIPE_RADIUS                   =  0.0_SP
   
      RETURN
      END SUBROUTINE PLUME_CALCULATE_NAMELIST_INITIALIZE
   
!------------------------------------------------------------------------------|
      SUBROUTINE PLUME_CALCULATE_NAMELIST_PRINT
      USE CONTROL, ONLY : IPT
   
      IMPLICIT NONE
      
      WRITE(UNIT=IPT,NML=NML_PLUME_CALCULATED)
   
      RETURN
      END SUBROUTINE PLUME_CALCULATE_NAMELIST_PRINT  
   
!------------------------------------------------------------------------------|
      SUBROUTINE PLUME_CALCULATE_NAMELIST_READ    
      USE CONTROL, ONLY : casename,NMLUNIT
      
      IMPLICIT NONE
   
      INTEGER :: IOS, I
      CHARACTER(LEN=120) :: FNAME
      CHARACTER(LEN=160) :: PATHNFILE
   
      IF(DBG_SET(DBG_SBR)) &
      & WRITE(IPT,*) "Subroutine Begins: Read_Plume_Calculate_Namelist;"

      IOS = 0

      FNAME = "./"//trim(casename)//"_run.nml"

      CALL FOPEN(NMLUNIT,trim(FNAME),'cfr')

   !READ NAME LIST FILE
      REWIND(NMLUNIT)

   ! Read IO Information
      READ(UNIT=NMLUNIT, NML=NML_PLUME_CALCULATED,IOSTAT=ios)
      if(ios .NE. 0 ) Then
        if(DBG_SET(dbg_log)) write(UNIT=IPT,NML=NML_PLUME_CALCULATED) 
        Call Fatal_error("Can Not Read NameList NML_PLUME_CALCULATED from file: "//trim(FNAME))
        !"
      end if
      CLOSE(NMLUNIT)
 
      RETURN
      END SUBROUTINE PLUME_CALCULATE_NAMELIST_READ

!==============================================================================|
! Allocate variables output from the plume model	
!==============================================================================|
      SUBROUTINE PLUME_CALCULATE_INIT
      ALLOCATE(FLXPLM(0:MT,KB)) ;              FLXPLM     =   ZERO ! 
      ALLOCATE(VDISTPLM(0:MT,KB)) ;            VDISTPLM   =   ZERO ! 
      ALLOCATE(TPLM(0:MT,KB)) ;                TPLM       =   ZERO ! 
      ALLOCATE(SPLM(0:MT,KB)) ;                SPLM       =   ZERO !
      ALLOCATE(FABMPLM(0:MT,KB,1)) ;           FABMPLM    =   ZERO !
   
!==============================================================================|
      END SUBROUTINE PLUME_CALCULATE_INIT
!==============================================================================|

!==============================================================================|
  SUBROUTINE UPDATE_PLUMES_CALCULATED(PFLUX,VDIST,PT,PS,PTR)
! --------------------------------------------------------------------------------------
!     This routine reads the volume-, momentum- and buoyancyflux, as well as the
!     initial concentration of salt, tracer and temperature to give an estimate of
!     where the plume will stop, the flux of matter to that designated sigma layer
!     and the flux of matter from the layers where the plume entrains water from the
!     surroundings.

!     Parameters and variables:
!     ----------------
!     NAMELIST INPUT:
!     ----------------
!     PIPE_RADIUS:  Radius of the pipe providing the effluent
!     SOURCE_VOL:   Source volume flux
!     PTEMP:        Initial plume temperature
!     PSALT:        Initial plume salinity
!     PTRACER:      Initial plume concentration
!     Inode:        Node where the plume was released
!     IFABM:        Id of FABM variable
!     INS:          Input sigma layer

!     PLUME INITIALIZATION VARIABLES
!     ------------------------------
!     PRHO:         Plume density at the source
!     SOURCE_BUOY:  Plume buoyancy flux

!     OUTPUT:
!     ----------------
!     PT:           Temperature of plume at the top, surroundings everywhere else
!     PS:           Salinity of the plume at the top, surroundings everywhere else
!     PTR:          FABM tracer concentration at the top, surroundings everwhere else
!     PFLUX:        Volume flux caused by the plume
!     VDIST:        The nodes influenced by the plume, as well as sigma layers

!     LOCAL:
!     ---------------
!     DST:          Distance from releasenode to the rest of the nodes in the domain
!     entcoef:      Entrainment coefficient
!     area:         Area covered by the plume at MAXsigma
!     radius:       Plume radius at MAXsigma
!     Wn,Vn,Fn:     Updated volume-, momentum- and buoyancy flux
!     PDEPTH:       Thickness of sigma layer (I,K)
!     NN:           Nyquist frequency (intertial stratication frequency)
!     TEMPPF:       Temperature in sigma layers, plume value at sigmalayer of release
!     SALTPF:       Salinity                  ------||------
!     FABMPF:       FABM tracer concentration ------||-------
!     TMP:          Dummy variable to store data while reallocating RELEASENODES
!     WEIGHT_TMP:   Dummy variable to store data while reallocating WEIGHT
!     WEIGHT:       Weight number to ensure mass conservation when releasing the flux
!                   over several nodes.
!     RELEASENODES: The nodes over which we release the plume.
!     INIT_V:       Initial volume source term, used when releasing the flux over
!                   several nodes.
!     PRESSURE:     Pressure where the plume is released, used to estimate density.
! ------------------------------------------------------------------------------------
      USE EQS_OF_STATE
      USE ALL_VARS
      USE MOD_UTILS
      USE MOD_PAR, ONLY: NGID
# if defined (FABM)
      USE MOD_FABM_DATA
# endif

      IMPLICIT NONE
      REAL(SP)                                            :: SOURCE_MOM, SOURCE_VOL, SOURCE_BUOY
      REAL(SP)                                            ::  PLUME_AREA,DST,PTEMP,PSALT,PTRACER
      REAL(SP), INTENT(OUT), DIMENSION(0:MT,KB), OPTIONAL ::                          PTR, PT,PS
      INTEGER,  INTENT(OUT), DIMENSION(0:MT,KB)           ::                               VDIST
      REAL(SP), INTENT(OUT), DIMENSION(0:MT,KB)           ::                               PFLUX
      INTEGER                                             ::       A,B,C,INS,IFABM,INODE,K,I,J,O
      INTEGER                                             ::                            NEWROUND
      REAL(SP)                                            :: INIT_V,entcoef,AREA,radius,Wn,Vn,Fn
      REAL(SP), DIMENSION(KB)                             ::      PDEPTH,NN,TEMPPF,SALTPF,FABMPF
      INTEGER,  ALLOCATABLE                               ::             TMP(:), RELEASENODES(:)
      REAL(SP), ALLOCATABLE                               ::            WEIGHT_TMP(:), WEIGHT(:)
      LOGICAL                                             ::                           RUN_PLUME
      REAL(SP), DIMENSION(1:2)                            ::            PRHO, PRESSURE, PTT, PSS
   
      ! Diffusor stuff
      REAL(SP) :: PLUME_SOURCE, PLUME_ANGLE, PLUME_MERGING_DISTANCE
      REAL(SP) :: Rdiff, Wdiff, PI, DIFF_VOL, DIFF_MOM, DIFF_BUOY
      REAL(SP) :: PSF, PTF, PTRF, TOT_PLUMES



!     Constants and ICs
      entcoef      = 0.07_SP  ! The exact value of this parameter is disputed
      PI           = 4.0_SP*ATAN(1.0_SP)

!===========Added by Qin, need to be modified=================================
      INODE        =    PLUME_RELEASE_NODEID     
      INS          =    PLUME_RELEASE_SIGMALAYER      
      PTEMP        =    PLUME_INITIAL_TEMPERATURE     
      PSALT        =    PLUME_INITIAL_SALINITY        
# if defined (FABM)
      PTRACER      =    PLUME_INITIAL_CONCENTRATION
      IFABM        =    1
# endif       
      SOURCE_VOL   =    PLUME_INITIAL_VOLUME_FLUX
      SOURCE_MOM   =    SOURCE_VOL/(PIPE_RADIUS)
!=================================================================================
! Initializing the code. The routine will only be run for the plume release node.
      RUN_PLUME    = .FALSE.
      DO I = 1,M
         IF (INODE.EQ.NGID(I)) THEN
            RUN_PLUME = .TRUE.
            INODE     = I
            EXIT
         END IF
      END DO

      PFLUX = ZERO
      VDIST = ZERO
      
      IF (RUN_PLUME) THEN
! ---
! Calculate the buoyancy flux based on input salt and temperature
! ---
     PRESSURE = -GRAV_N(INODE)*1.025_SP*(ZZ(INODE,INS)*D(INODE))*0.1_SP 
     ! H: I could not get this function work for scalar T and S, hence
     !    passing the variables as vectors instead.
     PTT      = PTEMP
     PSS      = PSALT

     SELECT CASE(SEA_WATER_DENSITY_FUNCTION)
     CASE(SW_DENS1)
       CALL FOFONOFF_MILLARD(PSS,PTT,PRESSURE,0.0_SP,PRHO)

     CASE(SW_DENS2)
       CALL DENS2G(PSS,PTT,PRHO)

     CASE(SW_DENS3)      
       CALL JACKET_MCDOUGALL(PSS,PTT,PRESSURE,PRHO)                                                   

     CASE DEFAULT
      CALL FATAL_ERROR("INVALID DENSITY FUNCTION SELECTED:",&
           & "   "//TRIM(SEA_WATER_DENSITY_FUNCTION) )
     END SELECT

! --- 
! Estimate the buoyancy flux
! ---
     SOURCE_BUOY = GRAV_N(INODE)*(RHO1(INODE,INS)-PRHO(1))*SIZE(RHO1(INODE,:))&
           &/(SUM(RHO1(INODE,:))) * SOURCE_VOL
     IF (SOURCE_BUOY.LT.0) THEN
           CALL FATAL_ERROR("INVALID BUOYANCY FLUX, MOD_PLUME DOES NOT SUPPORT&
           & SINKING PLUMES AT THE MOMENT.")
     END IF

!===========================================================================
! Buoyancy frequency, stolen from the gotm routine
! ----
      DO K=2,KBM1
         NN(K)       = -GRAV_N(INODE)*(RHO1(INODE,K-1)-RHO1(INODE,K))/(DZZ(INODE,K-1)*D(INODE))
         IF (NN(K).LT.ZERO) THEN
            NN(K)    = 0.0_SP
         END IF
         NN(K)       = SQRT(NN(K))
      END DO
      NN(1)          = 0.0_SP
      NN(KB)         = 0.0_SP
      PDEPTH         = DZZ(INODE,:)*D(INODE)

!     Initializing plume temperature and salinity
!     Filling vectors with data
      IF ((PRESENT(PT)).AND.(PRESENT(PS))) THEN
         PT          = T1
         PS          = S1
      END IF
      IF (PRESENT(PTR)) THEN
         PTR         = FABM_ALL(:,:,IFABM) !PTR(:,:,IFABM) = FABM_ALL(:,:,IFABM)
      END IF

!     Integrate from the input sigma layer, activate it
      K              = INS    ! INPUT sigma layer
      VDIST(INODE,K) = 1

!     Interaction between plume and surroundings
      IF ((PRESENT(PT)).AND.(PRESENT(PS))) THEN
         TEMPPF(K)   = SOURCE_VOL*PTEMP
         SALTPF(K)   = SOURCE_VOL*PSALT
      END IF

      IF (PRESENT(PTR)) THEN
         FABMPF(K)   = SOURCE_VOL*PTRACER
      END IF

      INIT_V = SOURCE_VOL

      IF (PLUME_DIFFUSOR_ON) THEN
!===========================================================================
! Enhanced initial entrainment due to use of diffusors
! ----
! ===========================================================================
         ! Must change the source terms to account for enhanced entrainment
         ! due to diffusors. We only account for the mixing due to the
         ! momentum supplied by the tube
         
         ! To begin with (proof of concept): Look at diffusors pointing in
         ! the same direction.

         ! For a given line of plumes: When will they coalesce?
         !                 Our answer: When they expand far enough to the
         !                             sides, such that they grow into 
         !                             each other.

         ! Ps. This routine is only valid for relatively small (ie. <6m) 
         ! vertical displacements before they coalesce.
! ============================================================================ 
         TOT_PLUMES           = PLUME_DIFFUSOR_UP + PLUME_DIFFUSOR_DOWN
         Rdiff                = PLUME_DIFFUSOR_RADIUS
         Wdiff                = SOURCE_VOL/(TOT_PLUMES*Rdiff**2)
         IF (Rdiff.GT.(PLUME_DIFFUSOR_DISTANCE/2)) THEN
            CALL FATAL_ERROR("The diffusor radius is bigger than the diffusor separation distance/2")
            !"
         END IF
         ! Diffusor source and momentum fluxes
         DIFF_MOM  = Rdiff * Wdiff
         DIFF_VOL  = Rdiff**2 * Wdiff
         DIFF_BUOY = Rdiff**2 * Wdiff * (SOURCE_MOM**2/SOURCE_VOL)

         ! We also need to update the plume temperature and salinity
         IF ((PRESENT(PT)).AND.(PRESENT(PS))) THEN
            PTF    = DIFF_VOL*PTEMP
            PSF    = DIFF_VOL*PSALT
         END IF

         IF (PRESENT(PTR)) THEN
            PTRF   = DIFF_VOL*PTRACER
         END IF

         ! Idea: Integrate these equations until they merge
         K = INS
         big: DO WHILE (.TRUE.)
             ! Integrate until the diffusor plume radius is equal to half
             ! the distance between the diffusors.

             ! Perhaps smart to integrate with higher resolution since we expect
             ! the diffusors to be more efficient than the "super plume"
             K = K-1
             ! Run the plume model
             DO I = 1,10
               Wn             = DIFF_VOL    + 2.0_SP*entcoef*DIFF_MOM*PDEPTH(K)/10.0_SP
               Vn             = DIFF_MOM**4 + 2.0_SP*DIFF_BUOY*DIFF_VOL*PDEPTH(K)/10.0_SP
               Fn             = DIFF_BUOY   - (NN(K)**2)*DIFF_VOL*PDEPTH(K)/10.0_SP
               IF (ISNAN(Vn**(0.25_SP))) THEN
                  EXIT big
               END IF
               
               IF ((PRESENT(PS)).AND.(PRESENT(PT))) THEN
                  PSF         = PSF  + 2.0_SP*(entcoef*DIFF_MOM*PDEPTH(K)/10.0_SP)*S1(INODE,K)
                  PTF         = PTF  + 2.0_SP*(entcoef*DIFF_MOM*PDEPTH(K)/10.0_SP)*T1(INODE,K)
               END IF

# if defined (FABM)
               IF (PRESENT(PTR)) THEN
                  PTRF        = PTRF + 2.0_SP*(entcoef*DIFF_MOM*PDEPTH(K)/10.0_SP)*FABM_ALL(INODE,K,IFABM)
               END IF
# endif
               
               PFLUX(INODE,INS) = PFLUX(INODE,INS) - 2.0_SP*entcoef*SOURCE_MOM*PDEPTH(K)/10
               
               DIFF_VOL       = Wn
               DIFF_MOM       = Vn**(0.25_SP)
               DIFF_BUOY      = Fn
               IF ((DIFF_VOL/DIFF_MOM).GT.(PLUME_DIFFUSOR_DISTANCE/2)) THEN
                  EXIT big
               END IF
            END DO
         END DO big
         
         IF (PRESENT(PT)) THEN
            PTEMP    = PTF/DIFF_VOL
            PSALT    = PSF/DIFF_VOL
         END IF
         IF (PRESENT(PTR)) THEN
            PTRACER  = PTRF/DIFF_VOL
         END IF
         
         ! The area of the big plume should be TOT_PLUMES times bigger than the small ones
         ! Hence the radius should be SQRT(TOT_PLUMES) times bigger
         Rdiff                = SQRT(TOT_PLUMES)*DIFF_VOL/DIFF_MOM

         ! Now, assume that these plumes coalesce and form a perfect "superplume". This is
         ! the new input for the plume model.
         SOURCE_VOL          = (Rdiff**2)*DIFF_MOM**2/DIFF_VOL    ! VOLsource  = r**2*w
         SOURCE_MOM          = Rdiff*(DIFF_MOM**2/DIFF_VOL)       ! MOMsource  = rw
         SOURCE_BUOY         = SOURCE_VOL*DIFF_BUOY/DIFF_VOL      ! BUOYsource = r**2*w*g'
         
         ! Update the initial temperature-, salinity- and tracer-flux.
         IF ((PRESENT(PT)).AND.(PRESENT(PS))) THEN
            TEMPPF(INS)   = SOURCE_VOL*PTEMP
            SALTPF(INS)   = SOURCE_VOL*PSALT
         END IF
         
         IF (PRESENT(PTR)) THEN
            FABMPF(INS)   = SOURCE_VOL*PTRACER
         END IF

         K = INS
      END IF
      !INIT_V = SOURCE_VOL ! Storing the source volume flux for later

! ======================================================================================
!                         And then finally: The plume model
! ======================================================================================
!     Initialize stuff
      ALLOCATE(RELEASENODES(1))
      ALLOCATE(WEIGHT(1))
      RELEASENODES(1)    = INODE
      WEIGHT(1)          = 1.0_SP
      AREA               = ART1(INODE)
      DO WHILE (.TRUE.)
         K               = K-1
         radius          = SOURCE_VOL/SOURCE_MOM
         PLUME_AREA      = PI*radius**2

         IF (PLUME_AREA.GT.AREA) THEN
            I = 0
            outer: DO WHILE (PLUME_AREA.GT.AREA)
               I = I+1
               inner: DO J = 1,NTSN(RELEASENODES(I))
                  O = NBSN(RELEASENODES(I),J)
                  ! To avoid storing the same node twice
                  DO A = 1,SIZE(RELEASENODES)
                     IF (RELEASENODES(A).EQ.O) THEN
                        CYCLE inner
                     END IF
                  END DO
                  VDIST(O,K) = 1
                  AREA       = AREA+ART1(O)

                  ! Identify the nodes covering the rising plume, and store them
                  ALLOCATE(TMP(SIZE(RELEASENODES)+1))
                  TMP(1:SIZE(RELEASENODES))   = RELEASENODES
                  TMP(SIZE(RELEASENODES)+1)   = O
                  CALL MOVE_ALLOC(TMP,RELEASENODES)

                  ! Do not stop storing more releasenodes until you cover the entire plume
                  IF (AREA.GT.PLUME_AREA) THEN
                     ALLOCATE(WEIGHT_TMP(SIZE(RELEASENODES)))
                     WEIGHT_TMP(1:SIZE(WEIGHT)) = WEIGHT
                     WEIGHT_TMP(SIZE(WEIGHT)+1:SIZE(RELEASENODES)) = (1-SUM(WEIGHT))/(SIZE(RELEASENODES)-SIZE(WEIGHT))
                     CALL MOVE_ALLOC(WEIGHT_TMP, WEIGHT)
                     EXIT outer
                  END IF
              END DO inner
               
              ! We got this far, thus all of the nodes in the last NBSN were used, and they should
              ! get their fair share of the flux
              ALLOCATE(WEIGHT_TMP(SIZE(RELEASENODES)))
              WEIGHT_TMP = ART1(INODE)/PLUME_AREA
              CALL MOVE_ALLOC(WEIGHT_TMP, WEIGHT)
           END DO outer
        END IF
        VDIST(RELEASENODES,K) = 1
        
        IF ((ISNAN(SOURCE_MOM)).OR.(K.EQ.1)) THEN
           ! Plume reached the top, releasing it there. (NB: Hydrostatically unstable...)
           DO I = 1,SIZE(RELEASENODES)
              PFLUX(RELEASENODES(I),K)      =  -SUM(PFLUX(RELEASENODES(I),:))+INIT_V*WEIGHT(I)
              IF ((PRESENT(PT)).AND.(PRESENT(PS))) THEN
                 PT(RELEASENODES(I),K)      = (TEMPPF(K+1)/SOURCE_VOL)
                 PS(RELEASENODES(I),K)      = (SALTPF(K+1)/SOURCE_VOL)
              END IF
              IF (PRESENT(PTR)) THEN
                 PTR(RELEASENODES(I),K)     = (FABMPF(K+1)/SOURCE_VOL)
              END IF
           END DO
           EXIT
        END IF

        ! Solving the plume model equations
        Wn             = SOURCE_VOL    + 2.0_SP*entcoef*SOURCE_MOM*PDEPTH(K)
        Vn             = SOURCE_MOM**4 + 2.0_SP*SOURCE_BUOY*SOURCE_VOL*PDEPTH(K)
        Fn             = SOURCE_BUOY   - (NN(K)**2)*SOURCE_VOL*PDEPTH(K)
         
        ! Finding the flux og S,T and TRACER upwards from given sigma layer, thus accounting
        ! for entrainment of matter from the sides.
        IF ((PRESENT(PS)).AND.(PRESENT(PT))) THEN
           SALTPF(K)   = SALTPF(K+1) + 2.0_SP*entcoef*SOURCE_MOM*PDEPTH(K)*S1(INODE,K)
           TEMPPF(K)   = TEMPPF(K+1) + 2.0_SP*entcoef*SOURCE_MOM*PDEPTH(K)*T1(INODE,K)
        END IF
# if defined (FABM)
        IF (PRESENT(PTR)) THEN
           FABMPF(K)   = FABMPF(K+1) + 2.0_SP*entcoef*SOURCE_MOM*PDEPTH(K)*FABM_ALL(INODE,K,IFABM)
        END IF
# endif

        ! Distribute the flux over the nodes influenced by the plume at every sigma layer
        DO J = 1,SIZE(RELEASENODES)
           PFLUX(RELEASENODES(J),K) = -2.0_SP*entcoef*SOURCE_MOM*PDEPTH(K)*WEIGHT(J)
        END DO

        ! We update the plume parameters if the plume is still rising
        SOURCE_MOM     = Vn**(0.25_SP) ! Momentum flux
        SOURCE_VOL     = Wn     ! Volume flux
        SOURCE_BUOY    = Fn     ! Buoyancy flux
        
        IF (.NOT.(ISNAN(SOURCE_MOM))) THEN
           radius      = SOURCE_VOL/SOURCE_MOM ! Plume radius
        END IF
      END DO
      print *, 'utvanning: Vf_new/Vf_old  = ', SOURCE_VOL/INIT_V
      END IF
  RETURN
  END SUBROUTINE UPDATE_PLUMES_CALCULATED

END MODULE MOD_PLUME
