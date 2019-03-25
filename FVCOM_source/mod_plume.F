! ---------------------------------------------------------------------
! This model estimates the flux of tracer from a point release at the
! bottom. It is based on idealized physics proposed by Morton, Talyer &
! Turner, 1956: "Turbulent gravitational convection" (the MTT model)
      
! The tracer flux at the fluid-source interface is read, the model
! estimates the depth the plume will reach a hydrostatically stable
! configuration based on entrainment of water from the sides.
! A net flux of temperature, salinity and tracer is
! released at the depth predicted by the mtt model.

! Important note!:
! MTT theory assumes that the buoyancy flux and stratification is
! independent of time. However, the stratification changes with 
! with time in FVCOM. By using this plume model we assume that the 
! plume adapts to the changing environment and reaches a stable
! configuration instantaneously. This is not the case in nature, 
! and should be accounted for in the future development of this code.
       
! Copyright (c) Akaplan-niva 2019
! ----------------------------------------------------------------------
! REVISION HISTORY:
! Original authors: Håvard Espenes     (hes@akvaplan.niva.no)
!                   Qin Zhou           (qin@akvaplan.niva.no)
!                   Ole Anders Nøst    (oan@akvaplan.niva.no)

! Ps. To use this module, you must activate both "PLUME" and
!     "PLUME_CALCULATED" when you compile the code.

MODULE MOD_PLUME
    USE ALL_VARS
    USE MOD_UTILS
    USE MOD_PREC
    IMPLICIT NONE
    SAVE
	
    LOGICAL PLUME_CALCULATE_ON
    INTEGER(SP) :: PLUME_RELEASE_NODEID          ! GLOBAL NODE ID WHERE PLUME IS RELEASED
    INTEGER(SP) :: PLUME_RELEASE_SIGMALAYER      ! THE SIGMA LAYER WHERE PLUME IS RELEASED
    REAL(SP)    :: PLUME_INITIAL_TEMPERATURE     ! INITIAL PLUME TEMPERATURE
    REAL(SP)    :: PLUME_INITIAL_SALINITY        ! INITIAL PLUME SALINITY
    REAL(SP)    :: PLUME_INITIAL_CONCENTRATION   ! INITIAL PLUME CONCENTRATION
    REAL(SP)    :: PLUME_INITIAL_MOMENTUM_FLUX   ! INITIAL PLUME MOMENTUM FLUX
    REAL(SP)    :: PLUME_INITIAL_VOLUME_FLUX     ! INITIAL PLUME VOLUMN FLUX
    REAL(SP)    :: PLUME_INITIAL_BOUYANCY_FLUX   ! INITIAL PLUME BOUYANCY FLUX
	
    NAMELIST /NML_PLUME_CALCULATED/            &
    & PLUME_CALCULATE_ON,                      &
    & PLUME_RELEASE_NODEID,                    &
    & PLUME_RELEASE_SIGMALAYER,                &
    & PLUME_INITIAL_TEMPERATURE,               &
    & PLUME_INITIAL_SALINITY,                  &
    & PLUME_INITIAL_CONCENTRATION,             &
    & PLUME_INITIAL_MOMENTUM_FLUX,             &
    & PLUME_INITIAL_VOLUME_FLUX,               &
    & PLUME_INITIAL_BOUYANCY_FLUX             
	
	
    CONTAINS

!==============================================================================|
!   Input Parameters for the Plume Model                
!==============================================================================|

   SUBROUTINE PLUME_CALCULATE_NAMELIST_INITIALIZE
   USE control, only : casename
   IMPLICIT NONE
   
   PLUME_CALCULATE_ON            =  .FALSE.                
   PLUME_RELEASE_NODEID          =  0                 
   PLUME_RELEASE_SIGMALAYER      =  0         
   PLUME_INITIAL_TEMPERATURE     =  0.0_SP          
   PLUME_INITIAL_SALINITY        =  0.0_SP        
   PLUME_INITIAL_CONCENTRATION   =  0.0_SP            
   PLUME_INITIAL_MOMENTUM_FLUX   =  0.0_SP  
   PLUME_INITIAL_VOLUME_FLUX     =  0.0_SP            
   PLUME_INITIAL_BOUYANCY_FLUX   =  0.0_SP            
   
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
   end if
   CLOSE(NMLUNIT)
 
   RETURN
   END SUBROUTINE PLUME_CALCULATE_NAMELIST_READ

!==============================================================================|
! Allocate variables output from the plume model	
!==============================================================================|
 SUBROUTINE PLUME_CALCULATE_INIT
   ALLOCATE(FLXPLM(0:MT,KB)) ;              FLXPLM     =   ZERO  ! 
   ALLOCATE(VDISTPLM(0:MT,KB)) ;            VDISTPLM   =   ZERO  ! 
   ALLOCATE(TPLM(0:MT,KB)) ;                TPLM       =   ZERO  ! 
   ALLOCATE(SPLM(0:MT,KB)) ;                SPLM       =   ZERO  !
   ALLOCATE(FABMPLM(0:MT,KB,1)) ;           FABMPLM    =   ZERO  !
   
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
!     SOURCE_MOM:   Source momentum flux
!     SOURCE_VOL:   Source volume flux
!     SOURCE_BUOY:  Source buoyancy flux
!     PTEMP:        Initial plume temperature
!     PSALT:        Initial plume salinity
!     PTRACER:      Initial plume concentration
!     Inode:        Node where the plume was released
!     IFABM:        Id of FABM variable
!     INS:          Input sigma layer

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
!     MAXsigma:     Sigma layer where the plume stops
!     HYDsigma:     Sigma layer where the plume is in hydrostatic equilibrium
!     entcoef:      Entrainment coefficient
!     area:         Area covered by the plume at MAXsigma
!     radius:       Plume radius at MAXsigma
!     vertspeed:    Vertical velocity of the plume
!     Wn,Vn,Fn:     Updated volume, momentum and buoyancy flux
!     PDEPTH:       Thickness of sigma layer
!     NN:           Nyquist frequency (intertial stratication frequency)
!     TEMPPF:       Temperature in sigma layers, plume value at sigmalayer of release
!     SALTPF:       Salinity                  ------||------
!     FABMPF:       FABM tracer concentration ------||-------
!     TMP:          Dummy variable to store data while reallocating RELEASENODES
!     RELEASENODES: The nodes we release the plume in
! ------------------------------------------------------------------------------------

      USE ALL_VARS
      USE MOD_UTILS
      !USE MOD_PAR, ONLY: EGID
# if defined (FABM)
      USE MOD_FABM_DATA
# endif
      IMPLICIT NONE
      REAL(SP)                                            :: SOURCE_MOM, SOURCE_VOL, SOURCE_BUOY
      REAL(SP)                                            ::  PLUME_AREA,DST,PTEMP,PSALT,PTRACER
      REAL(SP), INTENT(OUT), DIMENSION(0:MT,KB), OPTIONAL ::                          PTR, PT,PS
      INTEGER,  INTENT(OUT), DIMENSION(0:MT,KB)           ::                               VDIST
      REAL(SP), INTENT(OUT), DIMENSION(0:MT,KB)           ::                               PFLUX
      INTEGER                                             ::             INS,IFABM,INODE,K,I,J,O
      REAL(SP)                                            ::        entcoef,AREA,radius,Wn,Vn,Fn
      REAL(SP), DIMENSION(KB)                             ::      PDEPTH,NN,TEMPPF,SALTPF,FABMPF
      INTEGER,  ALLOCATABLE                               ::             TMP(:), RELEASENODES(:)
      REAL(SP), ALLOCATABLE                               ::            WEIGHT_TMP(:), WEIGHT(:)
      LOGICAL                                             ::                           RUN_PLUME

!     Constants and ICs
      entcoef      = 0.07_SP  ! The exact value of this parameter is disputed
      !CALL GENMAP             ! To get EGID?

!===========Added by Qin, need to be modified=================================
      INODE        =    PLUME_RELEASE_NODEID     
      INS          =    PLUME_RELEASE_SIGMALAYER      
      PTEMP        =    PLUME_INITIAL_TEMPERATURE     
      PSALT        =    PLUME_INITIAL_SALINITY        
# if defined (FABM)
      PTRACER      =    PLUME_INITIAL_CONCENTRATION
      IFABM        =    1
# endif   
      SOURCE_MOM   =    PLUME_INITIAL_MOMENTUM_FLUX   
      SOURCE_VOL   =    PLUME_INITIAL_VOLUME_FLUX     
      SOURCE_BUOY  =    PLUME_INITIAL_BOUYANCY_FLUX
!=================================================================================
! Initializing the code. The routine will only be run for the plume release node.
      RUN_PLUME    = .TRUE.
      !DO I = 1,MT
      !   IF (INODE.EQ.EGID(I)) THEN
      !      RUN_PLUME = .TRUE.
      !      INODE = I
      !   END IF
      !END DO
# if defined (FABM)
      IF (PRESENT(PTR)) THEN
         PTR = ZERO
      END IF
# endif
      IF (PRESENT(PT)) THEN
         PT = ZERO
         PS = ZERO
      END IF
      PFLUX = ZERO
      VDIST = ZERO
      
      IF (RUN_PLUME) THEN
!=================================================================================
! Buoyancy frequency squared, stolen from the gotm routine
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
!===========================================================================
! The plume model - Designed exclusively for rising plumes.
!                   Can be fixed quite easilly with an if statement
!                   which reverses the integration direction.
! ----
!     Filling vectors with data
      IF ((PRESENT(PT)).AND.(PRESENT(PS))) THEN
         PT          = T1
         PS          = S1
      END IF
      IF (PRESENT(PTR)) THEN
         PTR         = FABM_ALL(:,:,IFABM) !PTR(:,:,IFABM) = FABM_ALL(:,:,IFABM)
      END IF
!     Basic stuff
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

! ======================================================================================
!                                The MTT plume model
! ======================================================================================
!     Initialize stuff
      ALLOCATE(RELEASENODES(1))
      ALLOCATE(WEIGHT(1))
      RELEASENODES(1)    = INODE
      WEIGHT(1)          = 1.0_SP
      AREA               = ART1(INODE)
      DO WHILE (.TRUE.)
         K = K-1
         radius          = SOURCE_VOL/SOURCE_MOM
         PLUME_AREA      = PI*radius**2
         ! If the plume covers an area bigger than the CV, then we need to distribute it
         ! over the surrounding nodes as well. Primitive procedure, will need further
         ! development to let the plume expand over more nodes than those connected
         ! to INODE as well.
         IF (PLUME_AREA.GT.AREA) THEN
            outer: DO WHILE (PLUME_AREA.GT.AREA)
               ! Will spread the plume over the nodes surrounding INODE, but not the nodes
               ! surrounding them again. Will need future improvements. The best solution may
               ! be to expand the loop underneath.
               DO J = 1,NTSN(INODE)
                  O = NBSN(INODE,J)
                  VDIST(O,K) = 1
                  AREA       = AREA+ART1(O)
                  ! Identify the nodes covering the rising plume, and store them
                  ALLOCATE(TMP(SIZE(RELEASENODES)+1))
                  TMP(1:SIZE(RELEASENODES))   = RELEASENODES
                  TMP(SIZE(RELEASENODES)+1)   = O
                  CALL MOVE_ALLOC(TMP,RELEASENODES)
                  ! If the area covered by the RELEASENODES is greater than 
                  ! the plume, exit the loop
                  IF (AREA.GT.PLUME_AREA) THEN
                     EXIT outer
                  END IF
               END DO
            END DO outer

            ! Have now found the number of surrounding nodes, sets how much we should
            ! weight of those nodes.
            ALLOCATE(WEIGHT_TMP(SIZE(RELEASENODES)))
            WEIGHT_TMP(1) = ART1(INODE)/PLUME_AREA
            WEIGHT_TMP(2:SIZE(RELEASENODES)) = (1.0_SP-WEIGHT_TMP(1))/(SIZE(RELEASENODES)-1.0_SP)
            CALL MOVE_ALLOC(WEIGHT_TMP, WEIGHT)
         END IF
         VDIST(RELEASENODES,K) = 1

         IF ((ISNAN(SOURCE_MOM)).OR.(K.EQ.1)) THEN
            ! Plume reached the top, releasing it there. (NB: Hydrostatically unstable...)
            ! FIX THE WEIGHT-STUFF LATER ON
            DO I = 1,SIZE(RELEASENODES)
               PFLUX(RELEASENODES(I),K)      =  SOURCE_VOL * WEIGHT(I)  ! This should be the correct way of doing it?
               !PFLUX                         =  PFLUX                   ! *PI? Think about it... 
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
         SOURCE_MOM     = Vn**(0.25_SP)            ! Momentum flux
         SOURCE_VOL     = Wn                       ! Volume flux
         SOURCE_BUOY    = Fn                       ! Buoyancy flux

         IF (.NOT.(ISNAN(SOURCE_MOM))) THEN
            radius      = SOURCE_VOL/SOURCE_MOM    ! Plume radius
         END IF
      END DO
      END IF
  RETURN
  END SUBROUTINE UPDATE_PLUMES_CALCULATED
END MODULE MOD_PLUME
