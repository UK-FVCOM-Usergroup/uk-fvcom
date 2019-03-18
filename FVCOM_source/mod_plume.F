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
  SUBROUTINE UPDATE_PLUMES_CALCULATED(PFLUX,PT,PS,VDIST,PTR)
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
!     DEPTH:        Thickness of sigma layer
!     NN:           Nyquist frequency (intertial stratication frequency)
!     TEMPPF:       Temperature in sigma layer where the plume is
!     SALTPF:       Salinity in  ------||------
!     FABMPF:       FABM tracer concentratio in ------||-------
!     TMP:          Dummy variable to store data while reallocating RELEASENODES
!     RELEASENODES: The nodes we release the plume in
! ------------------------------------------------------------------------------------

      USE ALL_VARS
      USE MOD_UTILS
# if defined (FABM)
      USE MOD_FABM_DATA
# endif
      IMPLICIT NONE
      REAL(SP)                                              ::    SOURCE_MOM, SOURCE_VOL, SOURCE_BUOY
      REAL(SP)                                              ::             DST, PTEMP, PSALT, PTRACER
      REAL(SP), INTENT(OUT), DIMENSION(0:MT,KB)             ::                            PT,PS,PFLUX
      INTEGER,  INTENT(OUT), DIMENSION(0:MT,KB)             ::                                  VDIST
      REAL(SP), INTENT(OUT), DIMENSION(0:MT,KB), OPTIONAL   ::                                    PTR
      INTEGER                                               ::  INS,IFABM,Inode,K,I,MAXsigma,HYDsigma
      REAL(SP)                                              :: entcoef,area,radius,vertspeed,Wn,Vn,Fn
      REAL(SP), DIMENSION(KB)                               ::          DEPTH,NN,TEMPPF,SALTPF,FABMPF
      INTEGER,  ALLOCATABLE                                 ::                TMP(:), RELEASENODES(:)

!     Constants and ICs
      entcoef      = 0.07_SP  ! The exact value of this parameter is disputed

!===========Added by Qin, need to be modified=================================
      INODE        =    PLUME_RELEASE_NODEID     
      INS          =    PLUME_RELEASE_SIGMALAYER      
      PTEMP        =    PLUME_INITIAL_TEMPERATURE     
      PSALT        =    PLUME_INITIAL_SALINITY        
      PTRACER      =    PLUME_INITIAL_CONCENTRATION   
      SOURCE_MOM   =    PLUME_INITIAL_MOMENTUM_FLUX   
      SOURCE_VOL   =    PLUME_INITIAL_VOLUME_FLUX     
      SOURCE_BUOY  =    PLUME_INITIAL_BOUYANCY_FLUX   
!============================================================================

!============================================================================
! Buoyancy frequency squared, stolen from the gotm routine
! ----
      DO K=2,KBM1
         NN(K) = -GRAV_N(Inode)*(RHO1(Inode,K-1)-RHO1(Inode,K))/(DZZ(Inode,K-1)*D(Inode))
         IF (NN(K).LT.ZERO) THEN
            NN(K) = 0.0_SP
         END IF
         NN(K) = SQRT(NN(K))
      END DO
      NN(1)  = 0.0_SP
      NN(KB) = 0.0_SP
      DEPTH  = -DZ(Inode,:)*D(Inode)

!===========================================================================
! The plume model
! ----
!     Filling vectors with data
      PT      = T1
      PS      = S1
      PTR     = FABM_ALL(:,:,IFABM)
      PFLUX   = 0.0_SP
      
!     Integrate the equations vertically (from the bottom to the top), and solve the 
!     equations to get the plume parameters.
      K                   = INS    ! INPUT sigma layer
      VDIST(Inode,K)      = 1
      radius              = SOURCE_VOL/SOURCE_MOM
      vertspeed           = SOURCE_MOM**2/SOURCE_VOL
!====
!     Add a "IF (SOURCE_BUOY > 0) THEN" loop for sinking plumes...
!====
      TEMPPF(K)      = SOURCE_MOM*PTEMP   + entcoef*2*pi*radius*vertspeed*(DEPTH(K))*T1(Inode,K)
      SALTPF(K)      = SOURCE_MOM*PSALT   + entcoef*2*pi*radius*vertspeed*(DEPTH(K))*S1(Inode,K)
# if defined (FABM)
      FABMPF(K)      = SOURCE_MOM*PTRACER + entcoef*2*pi*radius*vertspeed*(DEPTH(K))*FABM_ALL(Inode,K,IFABM)
# else
      ! The plume modellen always calculates some tracerflux, can be solved
      ! better in the future. For this project, we will always use FABM.
      FABMPF(K)      = 0
# endif
      PFLUX(Inode,K) = -2*entcoef*SOURCE_MOM*DEPTH(K)
      DO WHILE (.TRUE.)
         K  = K-1
         IF (K == 0) THEN
            EXIT
         ENDIF
         IF (K.GT.SIZE(NN)) THEN
            EXIT
         END IF
            
         Wn = SOURCE_VOL    + 2*entcoef*SOURCE_MOM*DEPTH(K)
         IF (ISNAN(Wn)) THEN
            AREA = 0.0_SP
            DO I = 1,MT
               DST = SQRT((VX(I)-VX(Inode))**2+(VY(I)-VY(Inode))**2)
               IF (DST.LT.radius) THEN
                  VDIST(I,K) = 1
                  AREA = AREA+ART1(I)
                  IF (ALLOCATED(RELEASENODES)) THEN
                     ALLOCATE(TMP(SIZE(RELEASENODES)+1))
                     TMP(1:SIZE(RELEASENODES))   = RELEASENODES
                     TMP(SIZE(RELEASENODES)+1)   = I
                     CALL MOVE_ALLOC(TMP,RELEASENODES)
                  ELSE
                     ALLOCATE(RELEASENODES(1))
                     RELEASENODES(1) = I
                  END IF
               END IF
            END DO            
            DO I = 1,SIZE(RELEASENODES)
               PFLUX(RELEASENODES(I),K)      = SOURCE_VOL*AREA/(2.0_SP*pi*radius**2)
               PT(RELEASENODES(I),K)         = (TEMPPF(K+1)/SOURCE_VOL)*AREA/(2*pi*radius**2)
               PS(RELEASENODES(I),K)         = (SALTPF(K+1)/SOURCE_VOL)*AREA/(2*pi*radius**2)
               PTR(RELEASENODES(I),K)        = (FABMPF(K+1)/SOURCE_VOL)*AREA/(2*pi*radius**2)
               EXIT
            END DO
         END IF
         VDIST(Inode,K)    = 1
         Vn                     = SOURCE_MOM**4 + 2.0_SP*SOURCE_BUOY*SOURCE_VOL*DEPTH(K)
         Fn                     = SOURCE_BUOY   - NN(K)**2

!     Update the plume S, T and FABMtracer due to entrained water
         radius         = Wn/Vn
         vertspeed      = Vn**2/Wn

!     Finding the flux upwards from given sigma layer 
         SALTPF(K)      = SALTPF(K+1) + entcoef*2*pi*radius*vertspeed*DEPTH(K)*S1(Inode,K)
         TEMPPF(K)      = TEMPPF(K+1) + entcoef*2*pi*radius*vertspeed*DEPTH(K)*T1(Inode,K)
# if defined (FABM)
         FABMPF(K)      = FABMPF(K+1) + entcoef*2*pi*radius*vertspeed*DEPTH(K)*FABM_ALL(Inode,K,IFABM)
# else
         FABMPF(K)      = 0.0_SP
# endif
         PFLUX(Inode,K) = -2*entcoef*SOURCE_MOM*DEPTH(K)
         
!     We update the plume parameters if the plume is still rising
         SOURCE_MOM        = Vn**(0.25)        ! Momentum flux
         SOURCE_VOL        = Wn                ! Volume flux
         SOURCE_BUOY       = Fn                ! Buoyancy flux
      END DO
  END SUBROUTINE UPDATE_PLUMES_CALCULATED
END MODULE MOD_PLUME
