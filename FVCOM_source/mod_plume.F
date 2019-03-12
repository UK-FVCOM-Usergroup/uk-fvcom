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



    SUBROUTINE UPDATE_PLUMES_CALCULATED(FLUX,TMPT,SLT,TRACER,gridinfo)
! --------------------------------------------------------------------------------------
!     This routine reads the volume-, momentum- and buoyancyflux, as well as the
!     initial concentration of salt, tracer and temperature to give an estimate of
!     where the plume will stop, the flux of matter to that designated sigma layer
!     and the flux of matter from the layers where the plume entrains water from the
!     surroundings.

!     Variables:
!     ----------------
!     NAMELIST INPUT
!     V0:           Source momentum flux
!     W0:           Source volume flux
!     F0:           Source buoyancy flux
!     PTEMP:        Initial plume temperature
!     PSALT:        Initial plume salinity
!     PTRACER:      Initial plume concentration
!     Inode:        Node where the plume was released
!     IFABM:        Id of FABM variable
!     INS:          Input sigma layer

!     OUTPUT:
!     T:            Temperature flux caused by the plume
!     S:            Salinity flux caused by the plume
!     F:            Tracer flux caused by the plume
!     VDIST:        The nodes influenced by the plume, as well as sigma layers
! ------------------------------------------------------------------------------------

      USE ALL_VARS
      USE MOD_UTILS
      IMPLICIT NONE
      REAL(SP)                                       ::                        V0, W0, F0
      REAL(SP)                                       ::   PTEMP, PSALT, PTRACER
      REAL(SP), INTENT(  OUT), ALLOCATABLE           ::  TMPT(:,:),SLT(:,:),FLUX(:,:)
      REAL(SP), INTENT(  OUT), ALLOCATABLE, OPTIONAL ::                       TRACER(:,:)
      INTEGER,  INTENT(  OUT), ALLOCATABLE           ::                        VDIST(:,:)
      INTEGER                                        ::                   INS,IFABM,Inode
      REAL(SP), ALLOCATABLE                          ::    MYDENS(:), DEPTH(:), nyfreq(:)
      REAL(SP)                                       ::         entcoef, zhyd, ztop, area
      REAL(SP)                                       ::                            radius

!     Constants and ICs
      entcoef        = 0.07_SP  ! The exact value of this parameter is disputed

!===========Added by Qin, need to be modified=================================
      INODE        =    PLUME_RELEASE_NODEID     
      REL_SIGLAY   =    PLUME_RELEASE_SIGMALAYER      
      PTEMP        =    PLUME_INITIAL_TEMPERATURE     
      PSALT        =    PLUME_INITIAL_SALINITY        
      PTRACER      =    PLUME_INITIAL_CONCENTRATION   
      V0           =    PLUME_INITIAL_MOMENTUM_FLUX   
      W0           =    PLUME_INITIAL_VOLUME_FLUX     
      F0           =    PLUME_INITIAL_BOUYANCY_FLUX   

!=======================================================================
      DEPTH          = -H(Inode)*DZ(Inode,:) ! Thickness of sigma layer
      MYDENS         = RHO1(Inode,:)

      !CALL BUOYFREQ(MYDENS, DEPTH, nyfreq)
      !CALL INTVERT(W0,V0,F0,PTEMP,PTRACER,PSALT,entcoef,INS,nyfreq,DEPTH, &
      !             MAXsigma,HYDsigma,IFABM,Inode,FLUX,TMPT,SLT,TRACER)  

      ALLOCATE(FLUX(0:MT,KBM1))
      ALLOCATE(TMPT(0:MT,KBM1))
      ALLOCATE(SALT(0:MT,KBM1))
      ALLOCATE(VDIST(0:MT,KBM1))
      FLUX(:)     = 0.0_SP
      TMPT(:)     = 0.0_SP
      SALT(:)     = 0.0_SP
      VDIST(:)    = 0

      radius         = W0/V0
      area           = 2*3.14159*radius**2
      
!     Må fordele fluksen over et gitt antall noder. Hvilke noder faller innenfor
!     plumens radius?

!     Se til TVD koden for rettledning
      END SUBROUTINE UPDATE_PLUMES_CALCULATED
       
      SUBROUTINE BUOYFREQ(DENS,DEPTH,nyfreq)
      USE ALL_VARS
      IMPLICIT NONE
      REAL(SP), ALLOCATABLE, INTENT(IN ) ::          DENS(:), DEPTH(:)
      REAL(SP), ALLOCATABLE, INTENT(OUT) ::                  nyfreq(:)
      REAL(SP)                           ::     Rup, Rdown, g, rhoref
      INTEGER                            ::                         K
      
      g      = 9.81_SP
      rhoref = 1024.0_SP
      allocate(nyfreq(1:SIZE(DENS)))
      
      DO K = 1,KBM1
         ! Her interpolerer jeg slik at jeg får nyquist frekvensen sentrert over 
         ! midtpunktet til sigmanivået?
         IF (K == 1) THEN
            Rup   = DENS(K)
            Rdown = (DENS(K)*DEPTH(K+1)+DENS(K+1)*DEPTH(K))/(DEPTH(K)+DEPTH(K+1))
         ELSE IF (K == KBM1) THEN
            Rup   = (DENS(K)*DEPTH(K-1)+DENS(K-1)*DEPTH(K))/(DEPTH(K)+DEPTH(K-1))
            Rdown = DENS(K)
         ELSE
            Rup   = (DENS(K)*DEPTH(K-1)+DENS(K-1)*DEPTH(K))/(DEPTH(K)+DEPTH(K-1))
            Rdown = (DENS(K)*DEPTH(K+1)+DENS(K+1)*DEPTH(K))/(DEPTH(K)+DEPTH(K+1))
         END IF
         
         ! Nyquist ferkvensen i andre
         nyfreq(K)     = -g/rhoref * ((Rup-Rdown)/(DEPTH(K)))
      
         ! Sets the nyquist frequency to 0 in non-hydrostatically stable environments
         IF (nyfreq(K).LT.0.0_SP) THEN
            nyfreq(K) = 0.0_SP
         END IF
         
         nyfreq(K)     = SQRT(nyfreq(K))
      END DO
      END SUBROUTINE BUOYFREQ

      SUBROUTINE INTVERT(W0,V0,F0,PLUME_TEMP,PLUME_TRACER,PLUME_SALT,entcoef,INS, &
                         &nyfreq,DEPTH,MAXsigma,HYDsigma,IFABM,Inode,FLUX,TMPT,SLT,TRACER)
      USE ALL_VARS
      USE MOD_FABM_DATA
      IMPLICIT NONE
      REAL(SP), INTENT(INOUT)                    ::                          W0, V0, F0
      REAL(SP), INTENT(IN   )                    ::   PLUME_TEMP, PLUME_TRACER, entcoef
      REAL(SP), INTENT(IN   )                    ::                          PLUME_SALT
      REAL(SP), INTENT(IN   ), ALLOCATABLE       ::                 nyfreq(:), DEPTH(:)
      INTEGER,  INTENT(IN   )                    ::                   INS, IFABM, Inode
      INTEGER,  INTENT(  OUT)                    ::                  MAXsigma, HYDsigma
      REAL(SP), INTENT(  OUT), DIMENSION(0:KBM1) ::                TMPT,SLT,TRACER,FLUX
      REAL(SP), DIMENSION(0:KBM1)                ::          TEMPFLUX,SALTFLUX,FABMFLUX
      REAL(SP)                                   ::       radius, vertspeed, Wn, Vn, Fn
      INTEGER                                    ::                                   K
!     Integrate the equations vertically (from the bottom to the top), and solve the 
!     equations to get the plume parameters.

      K           = INS    ! INPUT sigma layer
      FLUX(K)     = 0.0_SP

      radius      = W0/V0
      vertspeed   = V0**2/W0

!     Add a "IF (F0 > 0) THEN" greie, og integrere i motsatt retning om det viser seg
!     at plumen synker.
      TEMPFLUX(K) = V0*PLUME_TEMP   + entcoef*2*pi*radius*vertspeed*(DEPTH(K))*T1(Inode,K)
      SALTFLUX(K) = V0*PLUME_SALT   + entcoef*2*pi*radius*vertspeed*(DEPTH(K))*S1(Inode,K)
      FABMFLUX(K) = V0*PLUME_TRACER + entcoef*2*pi*radius*vertspeed*(DEPTH(K))*FABM_ALL(Inode,K,IFABM)
     
      DO WHILE (.TRUE.)
         K  = K-1
         IF (K == 0) THEN
            EXIT
         ENDIF
         IF (K.GT.SIZE(nyfreq)) THEN
            EXIT
         END IF
            
         Wn = W0    + 2*entcoef*V0*DEPTH(K)
         IF (ISNAN(Wn)) THEN
            MAXsigma = K
            EXIT
         END IF
         
         IF ((F0.GT.0).AND.(Fn.LE.0)) THEN
            HYDsigma = K
         END IF
         Vn          = V0**4 + 2.0_SP*F0*W0*0.5_SP*DEPTH(K)
         Fn          = F0    - nyfreq(K)**2

!     Update the plume S, T and FABMtracer due to entrained water
         radius      = Wn/Vn
         vertspeed   = Vn**2/Wn

!     Finding the flux upwards from given sigma layer 
         SALTFLUX(K) = SALTFLUX(K+1) + entcoef*2*pi*radius*vertspeed*DEPTH(K)*S1(Inode,K)
         TEMPFLUX(K) = TEMPFLUX(K+1) + entcoef*2*pi*radius*vertspeed*DEPTH(K)*T1(Inode,K)
         FABMFLUX(K) = FABMFLUX(K+1) + entcoef*2*pi*radius*vertspeed*DEPTH(K)*FABM_ALL(Inode,K,IFABM)
         FLUX(K)     = entcoef*2*pi*radius*vertspeed*(DEPTH(K)-DEPTH(K+1))

!     We update the plume parameters if the plume is still rising
         V0          = Vn**(0.25)        ! Momentum flux
         W0          = Wn                ! Volume flux
         F0          = Fn                ! Buoyancy flux
      END DO
      END SUBROUTINE INTVERT
END MODULE MOD_PLUME
