MODULE MOD_FABM_3D
# if defined(FABM)

  USE fabm
  USE fabm_config
  USE fabm_driver, only: type_base_driver, driver

  USE MOD_FABM_DATA
  USE ALL_VARS
  USE MOD_UTILS
  USE MOD_WD

  IMPLICIT NONE

  PRIVATE

  PUBLIC FABM_FVCOM_INITIALIZE, FABM_ALLOCATE, FABM_INITIAL, FABM_UPDATE

  REAL(SP), ALLOCATABLE :: FABM_F(:,:,:)          ! FORECASTED VARIABLES
  REAL(SP), ALLOCATABLE :: XFLUX_OBCB(:,:,:)      ! advective flux into nodes at open boundary; set in FABM_ADV, used in FABM_BCOND
  REAL(SP), ALLOCATABLE,TARGET :: PFABMPXB(:,:)   ! X gradient at bottom; set in FABM_ADV, used in FABM_MIXING
  REAL(SP), ALLOCATABLE,TARGET :: PFABMPYB(:,:)   ! Y gradient at bottom; set in FABM_ADV, used in FABM_MIXING

  ! Environment
  REAL(SP), ALLOCATABLE,TARGET :: CELL_THICKNESS(:,:)
  REAL(SP), ALLOCATABLE,TARGET :: DENSITY(:,:)
  REAL(SP), ALLOCATABLE,TARGET :: PRESSURE(:,:)
  REAL(SP), ALLOCATABLE,TARGET :: DEPTH(:,:)
  REAL(SP), ALLOCATABLE,TARGET :: WIND_SPEED(:)
  REAL(SP), ALLOCATABLE,TARGET :: BOTTOM_STRESS(:)

  type,extends(type_base_driver) :: type_fvcom_driver
  contains
     procedure :: fatal_error => fvcom_driver_fatal_error
     procedure :: log_message => fvcom_driver_log_message
  end type

CONTAINS

   SUBROUTINE FABM_FVCOM_INITIALIZE()
      ALLOCATE(type_fvcom_driver::driver)
      CALL fabm_create_model_from_yaml_file(model)
   END SUBROUTINE FABM_FVCOM_INITIALIZE

   SUBROUTINE FABM_ALLOCATE()
    INTEGER :: IFABM

    ! Tell FABM about the extents of the spatial domain.
    call fabm_set_domain(model,M,KB)

    ! Provide FABM with pointer to the mask.
    call fabm_set_mask(model,ISWETN(1:M))

    ! Send environmental data to FABM
    ! NB the idea is to send *any* fields that are already available in FVCOM, and of potential use to biogeochemical models.
    ! A predefined list of such variables is available at https://sourceforge.net/p/fabm/wiki/List_of_standard_variables/
    call fabm_link_bulk_data(model,standard_variables%temperature,T1(1:M,:))
    call fabm_link_bulk_data(model,standard_variables%practical_salinity,S1(1:M,:))
    call fabm_link_horizontal_data(model,standard_variables%surface_downwelling_shortwave_flux,SWRAD_WATTS(1:M))
    call fabm_link_horizontal_data(model,standard_variables%longitude,LON(1:M))
    call fabm_link_horizontal_data(model,standard_variables%latitude,LAT(1:M))
    call fabm_link_horizontal_data(model,standard_variables%bottom_depth,D(1:M))
    call fabm_link_horizontal_data(model,standard_variables%bottom_depth_below_geoid,H(1:M))

    ! TODO send standard_variables%number_of_days_since_start_of_the_year [scalar!] - must be a real number with fraction indicating time of day

    ! Allocate arrays for derived environmental variables that will be computed from FVCOM fields.

    ! Cell thickness in m (from D and DZ)
    IF (model%variable_needs_values(standard_variables%cell_thickness)) THEN
       ALLOCATE(CELL_THICKNESS(0:MT,KB))
       call fabm_link_bulk_data(model,standard_variables%cell_thickness,CELL_THICKNESS(1:M,:))
    END IF

    ! Density in kg m-3 (from RHO1)
    IF (model%variable_needs_values(standard_variables%density)) THEN
       ALLOCATE(DENSITY(0:MT,KB))
       call fabm_link_bulk_data(model,standard_variables%density,DENSITY(1:M,:))
    END IF

    ! Cell center depth in m (from D and ZZ)
    IF (model%variable_needs_values(standard_variables%depth)) THEN
       ALLOCATE(DEPTH(0:MT,KB))
       call fabm_link_bulk_data(model,standard_variables%depth,DEPTH(1:M,:))
    END IF

    ! Pressure in dbar (from gravity, depth and reference density)
    IF (model%variable_needs_values(standard_variables%pressure)) THEN
       ALLOCATE(PRESSURE(0:MT,KB))
       call fabm_link_bulk_data(model,standard_variables%pressure,PRESSURE(1:M,:))
    END IF

    ! Wind speed in m s-1 (from UUWIND, VVWIND)
    IF (model%variable_needs_values(standard_variables%wind_speed)) THEN
       ALLOCATE(WIND_SPEED(0:MT))
       call fabm_link_horizontal_data(model,standard_variables%wind_speed,WIND_SPEED(1:M))
    END IF

    ! Bottom stress in Pa (from square of bottom shear velocity TAUBM_N)
    IF (model%variable_needs_values(standard_variables%bottom_stress)) THEN
       ALLOCATE(BOTTOM_STRESS(0:MT))
       call fabm_link_horizontal_data(model,standard_variables%bottom_stress,BOTTOM_STRESS(1:M))
    END IF

    ALLOCATE(FABM_ALL(0:MT,KB,size(model%state_variables)));         FABM_ALL    =  0.001_SP
    ALLOCATE(FABM_ALL_BT(0:MT,size(model%bottom_state_variables)));  FABM_ALL_BT =  0.001_SP
    ALLOCATE(FABM_ALL_SF(0:MT,size(model%surface_state_variables))); FABM_ALL_SF =  0.001_SP
    ALLOCATE(FABM_F(0:MT,KB,size(model%state_variables)));           FABM_F       =  0.001_SP
    ALLOCATE(FABM_MEAN(0:MT,KB,size(model%state_variables)));        FABM_MEAN    =  0.001_SP
    ALLOCATE(FABM_TOTALS(size(model%conserved_quantities)));         FABM_TOTALS = 0

    ! Provide FABM with pointers to state variable data.
    DO IFABM=1,size(model%state_variables)
      CALL fabm_link_bulk_state_data(model,IFABM,FABM_ALL(1:M,:,IFABM))
    END DO
    DO IFABM=1,size(model%bottom_state_variables)
      CALL fabm_link_bottom_state_data(model,IFABM,FABM_ALL_BT(1:M,IFABM))
    END DO
    DO IFABM=1,size(model%surface_state_variables)
      CALL fabm_link_surface_state_data(model,IFABM,FABM_ALL_SF(1:M,IFABM))
    END DO

    ! Make sure FABM has all required data
    CALL fabm_check_ready(model)

    ALLOCATE(XFLUX_OBCB(0:MT,KB,size(model%state_variables))) ; XFLUX_OBCB  =  0.0_SP
    ALLOCATE(PFABMPXB(MT,size(model%state_variables)))        ; PFABMPXB    = ZERO
    ALLOCATE(PFABMPYB(MT,size(model%state_variables)))        ; PFABMPYB    = ZERO
   END SUBROUTINE

   SUBROUTINE PREPARE_ENVIRONMENT()
      INTEGER :: K
      REAL(SP), DIMENSION(0:MT) :: UUWIND_N,VVWIND_N

      ! Compute environmental fields that FVCOM does not provide

      IF (ALLOCATED(CELL_THICKNESS)) THEN
         ! Cell thickness (m) from dimensionless sigma thickness DZ and column height D
         DO K=1,KBM1
            CELL_THICKNESS(:,K) = DZ(:,K)*D
         END DO
      END IF

      IF (ALLOCATED(DEPTH)) THEN
         ! Cell center depth (m) from dimensionless sigma depth ZZ and column height D
         ! Note that ZZ is negative while we want positive depth; hence the - sign.
         DO K=1,KBM1
            DEPTH(:,K) = -ZZ(:,K)*D
         END DO
      END IF

      IF (ALLOCATED(PRESSURE)) THEN
         ! Pressure (dbar) from gravity, depth, and reference density
         ! NB could be made more accurate by using actual density.
         DO K=1,KBM1
            PRESSURE(:,K) = -GRAV_N(:)*1.025_SP*ZZ(:,K)*D*0.1_SP
         END DO
      END IF

      IF (ALLOCATED(WIND_SPEED)) THEN
         ! Wind speed (m s-1)  by interpolating individual x,y wind components from elements to nodes
         CALL E2N2D(UUWIND,UUWIND_N)
         CALL E2N2D(VVWIND,VVWIND_N)
         WIND_SPEED = SQRT(UUWIND_N**2 + VVWIND_N**2)
      END IF

      IF (ALLOCATED(DENSITY)) THEN
         ! From sigma in CGS units (g cm-3) to actual density in MKS units (kg m-3)
         DENSITY = (1+RHO1)*1000
      END IF

      IF (ALLOCATED(BOTTOM_STRESS)) THEN
         ! Bottom stress from square of shear velocity - multiply with density
         BOTTOM_STRESS = TAUBM_N*(1+RHO1(:,KBM1))*1000
      END IF
   END SUBROUTINE

   SUBROUTINE FABM_INITIAL()
      INTEGER :: K
      REAL(SP), DIMENSION(M,size(model%state_variables))         :: DUMMY
      REAL(SP), DIMENSION(M,size(model%bottom_state_variables))  :: DUMMY_BT
      REAL(SP), DIMENSION(M,size(model%surface_state_variables)) :: DUMMY_SF

      IF (.NOT.ALLOCATED(FABM_ALL)) CALL FATAL_ERROR("FABM_INITIAL called before FABM_ALLOCATE")

      CALL PREPARE_ENVIRONMENT

      ! Allow FABM to perform its default initialization.
      CALL fabm_initialize_surface_state(model,1,M,1)
      CALL fabm_initialize_bottom_state (model,1,M,KBM1)
      DO K=1,KBM1
        CALL fabm_initialize_state(model,1,M,K)
      END DO

      ! Initialize all diagnostics
      call fabm_do_surface(model,1,M,1,   DUMMY,DUMMY_SF)
      call fabm_do_bottom (model,1,M,KBM1,DUMMY,DUMMY_BT)
      DO K=1,KBM1
         call fabm_do(model,1,M,K,DUMMY)
      END DO

      ! Compute initial value of conserved quantity integrals
      CALL CHECK_CONSERVATION
   END SUBROUTINE FABM_INITIAL

SUBROUTINE FABM_UPDATE
     USE MOD_PAR

     INTEGER :: IFABM

    !     write (*,*) 'at timestep start ',maxval(FABM_ALL(1:M,1:KBM1,1))-minval(FABM_ALL(1:M,1:KBM1,1))

     ! Integrate biogeochemical sources-sinks
     CALL FABM_SOURCES(DTI)  ! This operates on FABM_ALL
    !     write (*,*) 'after FABM sources',maxval(FABM_ALL(1:M,1:KBM1,1))-minval(FABM_ALL(1:M,1:KBM1,1))
     IF (.NOT.CHECK_STATE(.FALSE.)) CALL FATAL_ERROR('Invalid FABM state at the end of FABM_SOURCES')
     CALL CALC_VERTICAL_MOVEMENT(DTI)
     IF (.NOT.CHECK_STATE(.FALSE.)) CALL FATAL_ERROR('Invalid FABM state at the end of CALC_VERTICAL_MOVEMENT')
!     CALL FABM_VERTICAL_MOVEMENT(DTI)
    !     write (*,*) 'after FABM vertical movement',maxval(FABM_ALL(1:M,1:KBM1,1))-minval(FABM_ALL(1:M,1:KBM1,1))
# if !defined (ONE_D_MODEL)
     CALL FABM_ADV  ! This operates on FABM_F
     IF (.NOT.CHECK_STATE(.FALSE.)) CALL FATAL_ERROR('Invalid FABM state at the end of FABM_ADV')
    !     write (*,*) 'after FABM_ADV    ',maxval(FABM_F(1:M,1:KBM1,1))-minval(FABM_F(1:M,1:KBM1,1))

#   if defined (MULTIPROCESSOR)
     DO IFABM=1,size(model%state_variables)
        IF(PAR)CALL NODE_MATCH(1,NBN,BN_MLT,BN_LOC,BNC,MT,KB,MYID,NPROCS,FABM_F(:,:,IFABM))
     END DO
#   endif

    IF(SCALAR_POSITIVITY_CONTROL) CALL FCT_FABM       !Conservation Correction of FABM_F  !

# endif
! end if defined 1D

     ! Vertical mixing
# if !defined (ONE_D_MODEL)
     CALL FABM_MIXING(FABM_F)   ! This operates on the supplied argument
     IF (.NOT.CHECK_STATE(.FALSE.)) CALL FATAL_ERROR('Invalid FABM state at the end of FABM_MIXING')
    !     write (*,*) 'after FABM_MIXING ',maxval(FABM_F(1:M,1:KBM1,1))-minval(FABM_F(1:M,1:KBM1,1))
# else
     CALL FABM_MIXING(FABM_ALL) ! This operates on the supplied argument
     IF (.NOT.CHECK_STATE(.FALSE.)) CALL FATAL_ERROR('Invalid FABM state at the end of FABM_MIXING')
    !     write (*,*) 'after FABM_MIXING ',maxval(FABM_ALL(1:M,1:KBM1,1))-minval(FABM_ALL(1:M,1:KBM1,1))
# endif

# if defined (MULTIPROCESSOR)
     IF(PAR)CALL FABM_EXCHANGE(FABM_F)
# endif

     CALL FABM_BCOND  ! This operates on FABM_F
    !     write (*,*) 'after FABM_BCOND  ',maxval(FABM_F(1:M,1:KBM1,1))-minval(FABM_F(1:M,1:KBM1,1))

# if !defined (ONE_D_MODEL)
     FABM_ALL=FABM_F                                       !Update to new time level  !
# endif

     IF (.NOT.CHECK_STATE(.FALSE.)) CALL FATAL_ERROR('Invalid FABM state at the end of FABM_UPDATE')

# if defined (MULTIPROCESSOR)
     IF(PAR)CALL FABM_EXCHANGE(FABM_ALL)
# endif

     CALL CHECK_CONSERVATION
END SUBROUTINE FABM_UPDATE

FUNCTION CHECK_STATE(REPAIR_STATE) RESULT(VALID_STATE)
      LOGICAL, INTENT(IN) :: REPAIR_STATE
      LOGICAL :: VALID_STATE
      INTEGER :: K

      ! Check state variable values. On return, VALID_STATE will be .TRUE. if the state has been found valid, .FALSE. if not.
      ! If REPAIR_STATE is .TRUE., this routine will automatically clip invalid values to the nearest valid value.
      ! In that case, the state will be valid after return even if VALID_STATE = .FALSE. [which is then indicative of
      ! clipping having occured)
      CALL fabm_check_surface_state(model,1,M,1,REPAIR_STATE,VALID_STATE)
      if (.NOT.(VALID_STATE.OR.REPAIR_STATE)) RETURN
      CALL fabm_check_bottom_state(model,1,M,KBM1,REPAIR_STATE,VALID_STATE)
      if (.NOT.(VALID_STATE.OR.REPAIR_STATE)) RETURN
      DO K=1,KBM1
         CALL fabm_check_state(model,1,M,K,REPAIR_STATE,VALID_STATE)
         if (.NOT.(VALID_STATE.OR.REPAIR_STATE)) RETURN
      END DO
END FUNCTION CHECK_STATE

SUBROUTINE CHECK_CONSERVATION()
     INTEGER  :: K, IFABM, IERR
     REAL(SP), DIMENSION(M,SIZE(model%conserved_quantities)) :: LOCAL
     REAL(DP), DIMENSION(SIZE(model%conserved_quantities))   :: TOTALS_LOC

     TOTALS_LOC = 0
     DO K=1,KBM1
        CALL fabm_get_conserved_quantities(model,1,M,K,LOCAL)
        DO IFABM=1,SIZE(model%conserved_quantities)
           TOTALS_LOC(IFABM) = TOTALS_LOC(IFABM) + SUM(DBLE(LOCAL(1:M,IFABM)*DZ(1:M,K)*D(1:M)*ART1(1:M)))
        END DO
     END DO
     CALL fabm_get_horizontal_conserved_quantities(model,1,M,1,LOCAL)
     DO IFABM=1,SIZE(model%conserved_quantities)
        TOTALS_LOC(IFABM) = TOTALS_LOC(IFABM) + SUM(DBLE(LOCAL(1:M,IFABM)*ART1(1:M)))
     END DO

     IF(PAR) THEN
        ! NB: similar to logic in mod_report
        CALL MPI_REDUCE(TOTALS_LOC,FABM_TOTALS,SIZE(TOTALS_LOC),MPI_DP,MPI_SUM,MSRID-1,MPI_FVCOM_GROUP,IERR)
     ELSE
        FABM_TOTALS = TOTALS_LOC
     END IF

     IF(MSR) THEN
        DO IFABM=1,SIZE(model%conserved_quantities)
           WRITE (IPT,'(A,E15.7,A)') 'Domain integral of '//TRIM(model%conserved_quantities(IFABM)%name),FABM_TOTALS(IFABM),' '//TRIM(model%conserved_quantities(IFABM)%units)//'*m3'
        END DO
     END IF
END SUBROUTINE CHECK_CONSERVATION

SUBROUTINE FABM_SOURCES(DELTA_T)
     REAL(SP), INTENT(IN) :: DELTA_T
     INTEGER  :: I, K, IFABM
     REAL(SP), DIMENSION(M,size(model%state_variables))         :: SMS_PEL,FLUX_SF,FLUX_BT
     REAL(SP), DIMENSION(M,size(model%bottom_state_variables))  :: SMS_BT
     REAL(SP), DIMENSION(M,size(model%surface_state_variables)) :: SMS_SF
     REAL(SP), DIMENSION(M)                                     :: DUMMY

     CALL PREPARE_ENVIRONMENT

     ! Ensure that light attenuation, absorption, scattering are up to date in FABM.
     DO K=1,KBM1
        CALL fabm_get_light_extinction(model,1,M,K,DUMMY)
     END DO

     ! Allow light models in FABM to update.
     ! NB this is the only operation in FABM that operates over K - everything else operates over I.
     DO I=1,M
        CALL fabm_get_light(model,1,KBM1,I)
     END DO

     FLUX_SF = 0
     FLUX_BT = 0
     SMS_SF = 0
     SMS_BT = 0

     ! Get sources-sinks for interface-attached state variables, and cross-interface
     ! fluxes (air-sea gas exchange, sedimentation, pelagic-benthic diffusion flux)
     call fabm_do_surface(model,1,M,1,   FLUX_SF,SMS_SF)
     call fabm_do_bottom (model,1,M,KBM1,FLUX_BT,SMS_BT)

     ! Integrate sources-sinks for the pelagic
     DO K=1,KBM1
        ! Retrieve sources-sinks for current layer K.
        SMS_PEL = 0
        call fabm_do(model,1,M,K,SMS_PEL)

        IF (K==1) THEN
           ! Top-most layer: include surface flux of pelagic state variables.
           DO IFABM=1,size(model%state_variables)
              SMS_PEL(1:M,IFABM) = SMS_PEL(1:M,IFABM) + FLUX_SF(1:M,IFABM)/(DZ(1:M,K)*D(1:M))
           END DO
        END IF
        IF (K==KBM1) THEN
           ! Bottom-most layer: include bottom flux of pelagic state variables.
           DO IFABM=1,size(model%state_variables)
              SMS_PEL(1:M,IFABM) = SMS_PEL(1:M,IFABM) + FLUX_BT(1:M,IFABM)/(DZ(1:M,KBM1)*D(1:M))
           END DO
        END IF

        ! Forward Euler time integration of pelagic state.
        FABM_ALL(1:M,K,:) = FABM_ALL(1:M,K,:) + DELTA_T*SMS_PEL
     END DO

     ! Forward Euler time integration of interface-attached state variables.
     FABM_ALL_SF(1:M,:) = FABM_ALL_SF(1:M,:) + DELTA_T*SMS_SF
     FABM_ALL_BT(1:M,:) = FABM_ALL_BT(1:M,:) + DELTA_T*SMS_BT

END SUBROUTINE FABM_SOURCES

!==========================================================================
! Calculate Depositional Flux and Update Concentration from Settling
!==========================================================================
  Subroutine CALC_VERTICAL_MOVEMENT(DELTA_T)
     REAL(SP), INTENT(IN) :: DELTA_T
     INTEGER :: I,K,IFABM
     INTEGER  :: mcyc,ncyc
     REAL(SP), DIMENSION(M,KBM1,size(model%state_variables)) :: w_ct
     REAL(SP), DIMENSION(KBM1) :: C,DX
     REAL(SP) :: eps,DTmax,DTdep
     REAL(SP),PARAMETER ::SETTLE_CFL = 1.0_SP

     ! Get vertical velocities at layer centers
     DO K=1,KBM1
        CALL fabm_get_vertical_movement(model,1,M,K,w_ct(:,K,:))
     END DO

  eps = epsilon(eps)
  !Loop over Horizontal Domain       
     DO I=1,M
#  if defined (WET_DRY)
      ! no calculation when dry nodes
       IF(ISWETN(I) == 1) THEN
#  endif
  !Loop over FABM variables       
         DO IFABM=1,SIZE(model%state_variables)

          !setup 1D concentration and grid arrays
          C(1:kbm1) = FABM_ALL(I,1:KBM1,IFABM)
          DX(1:kbm1) = DZ(I,1:KBM1)*D(I)
      

      !set up cycles (use max(CFL) == 1)
      DTmax = Settle_CFL*minval(ABS(DX(1:KBM1)/w_ct(I,1:KBM1,IFABM)))
      mcyc = int(DELTA_T/DTmax + 1. - eps)
      DTdep = DELTA_T/float(mcyc)
!         write (*,*) 'In FABM vertical with w_ct ',w_ct(I,1:KBM1,IFABM)
!         write (*,*) 'In FABM vertical with Mcyc and DTdep ',mcyc,DTdep
      !call flux-limited settling equation
      do ncyc = 1,mcyc
          call SETTLE_FLUX_FABM(KBM1,C,DX,w_ct(I,1:KBM1,IFABM),DTdep)
      end do

      !store solution in 3D array
       FABM_ALL(I,1:KBM1,IFABM) = C(1:KBM1)

         END DO
#  if defined (WET_DRY)
      ! no calculation when dry nodes
       END IF
#  endif

     END DO
  End Subroutine CALC_VERTICAL_MOVEMENT

Subroutine SETTLE_FLUX_FABM(n,c,dx,wset,deltat) 
! Adapted from mod_scal Calc_VFlux
! Uses sigma layers velocities rather than interface velocities
! Interface velocities are linearly interpolated here
    use mod_utils, only : LIMLED2
    implicit none
  integer , intent(in   ) :: n
	  real(sp), intent(inout) ::  c(n)
	  real(sp), intent(in   ) :: dx(n)
	  real(sp), intent(in   ) :: wset(n)
	  real(sp), intent(in   ) :: deltat 
	  real(sp) :: conv(n+1),diss(n+1)
	  real(sp) :: cin(0:n+1)
	  real(sp) :: win(0:n+1)
	  real(sp) :: dis4,wvel,sl_u,sl_f
          real(sp) :: sl_h(0:n+1)
  integer  :: i


  !transfer to working array
  cin(1:n) = c(1:n)
  win(1:n) = wset(1:n) 
  sl_h(1:n) = dx(1:n)

  !surface bcs (no flux)
  cin(0)  =  -cin(1) 
  sl_h(0) = dx(1)

  !bottom bcs (extrapolate)
  cin(n+1) = -cin(n) 
  sl_h(n+1) = dx(n)

  !flux computation
  do i=2,n
    wvel    = .5*(win(i)+win(i-1))  !settle velocity at interface
    dis4    = wvel/2.
    sl_u = 2.0_SP*(cin(i)-cin(i+1))/(dx(i)+dx(i+1))
    sl_f = 2.0_SP*(cin(i-2)-cin(i-1))/(dx(i-2)+dx(i-1))
    conv(i) = wvel*(cin(i)+cin(i-1))/2. 
    diss(i) = dis4*(cin(i-1)-cin(i)-0.5_SP*LIMLED2(sl_u,sl_f,2.0_SP)*(dx(i-1)+dx(i))) 
!    diss(i) = dis4*(cin(i)-cin(i-1)-lim(cin(i+1)-cin(i),cin(i-1)-cin(i-2))) 
  end do

  !zero out surface flux
  conv(1) = 0.0_SP ; diss(1) = 0.0_SP
  ! zero out bottom flux
  conv(n+1) = 0.0_SP
  diss(n+1) = 0.0_SP

  !update
  do i=1,n
    c(i) = cin(i) + (deltat/dx(i))*(-conv(i+1)+conv(i) + diss(i+1)-diss(i)) 
  end do


 End Subroutine SETTLE_FLUX_FABM


SUBROUTINE FABM_VERTICAL_MOVEMENT(DELTA_T)
     REAL(SP), INTENT(IN) :: DELTA_T
     INTEGER :: I,K,IFABM
     REAL(SP), DIMENSION(M,KBM1,size(model%state_variables)) :: w_ct
     REAL(SP), DIMENSION(KBM1-1,size(model%state_variables)) :: w_if, flux_if
     REAL(SP), DIMENSION(KBM1,  size(model%state_variables)) :: flux_ct
     REAL(SP), PARAMETER :: epsilon_w = 1e-7_SP

     ! Get vertical velocities at layer centers
     DO K=1,KBM1
        CALL fabm_get_vertical_movement(model,1,M,K,w_ct(:,K,:))
     END DO

     DO I=1,M
#  if defined (WET_DRY)
       IF(ISWETN(I) == 1) THEN
#  endif
         ! Linearly interpolate to velocities at the interfaces between layers
         ! TODO: account for differences in layer heights (now we simply use the unweighted average of center velocities)
         w_if(1:KBM1-1,:) = (w_ct(I,1:KBM1-1,:)+w_ct(I,2:KBM1,:))/2

         ! Convert velocities in m s-1 to mass fluxes (e.g., mol m-2 s-1) - both at interfaces between layers.
         DO IFABM=1,SIZE(model%state_variables)
            DO K=1,KBM1-1
               IF (w_if(K,IFABM)<0) THEN
                  ! Downward - use concentration from same level (velocity is defined on bottom interface)
                  flux_if(K,IFABM) = max(w_if(K,IFABM),min(-DZ(I,K)*D(I)/DELTA_T+epsilon_w,0._SP))*FABM_ALL(I,K,IFABM)
               ELSE
                  ! Upward - use concentration from level below (velocity is defined on bottom interface)
                  flux_if(K,IFABM) = min(w_if(K,IFABM),max(DZ(I,K+1)*D(I)/DELTA_T-epsilon_w,0._SP))*FABM_ALL(I,K+1,IFABM)
               END IF
            END DO
         END DO

         ! Combine interfacial mass fluxes (top + bottom) into total fluxes per layer
         flux_ct(1,       :) = flux_if(1,       :)
         flux_ct(2:KBM1-1,:) = flux_if(2:KBM1-1,:) - flux_if(1:KBM1-2,:)
         flux_ct(KBM1,    :) =                     - flux_if(KBM1-1,  :)

         ! Convert mass fluxes (m-2) into source terms (m-3) by dividing by layer height
         DO IFABM=1,SIZE(model%state_variables)
            FABM_ALL(I,1:KBM1,IFABM) = FABM_ALL(I,1:KBM1,IFABM) + DELTA_T*flux_ct(1:KBM1,IFABM)/DZ(I,1:KBM1)/D(I)
         END DO
#  if defined (WET_DRY)
      END IF
#  endif
   END DO  ! I=1:M
END SUBROUTINE FABM_VERTICAL_MOVEMENT

SUBROUTINE FABM_EXCHANGE(F)
     USE MOD_PAR

     REAL(SP), DIMENSION(0:MT,KB,size(model%state_variables)) :: F
#if defined (MULTIPROCESSOR)
     INTEGER :: IFABM
     REAL(SP),ALLOCATABLE :: F_T(:,:)

     ALLOCATE(F_T(0:MT,KB))
     DO IFABM=1,size(model%state_variables)
        F_T(:,:)  = F(:,:,IFABM)
        CALL AEXCHANGE(NC,MYID,NPROCS,F_T)
        F(:,:,IFABM)  = F_T(:,:)  
     END DO 
     DEALLOCATE(F_T)
#endif
  END SUBROUTINE FABM_EXCHANGE

  SUBROUTINE FABM_MIXING(F)
  !------------------------------------------------------------------------------|
  !     vertical mixing adopted from FVCOM, VDIF_TS                              !
  !------------------------------------------------------------------------------|
  USE ALL_VARS
  USE MOD_UTILS
  USE BCS
#  if defined (WET_DRY)
  USE MOD_WD
#  endif

# if defined (THIN_DAM)
  USE MOD_DAM,only : NODE_DAM1_N,NODE_DAM2_N,NODE_DAM3_N&
                      &,I_NODE_DAM1_N,I_NODE_DAM2_N,I_NODE_DAM3_N,KDAM
# endif

  IMPLICIT NONE
  INTEGER :: I,K,J,KI,IFABM
  REAL(DP) :: TMP,TMP1,TMP2,TMP3,FKH,UMOLPR
  REAL(SP), DIMENSION(0:MT,KB,size(model%state_variables)) :: F
  REAL(DP), DIMENSION(M,KB)     :: FF,AF,CF,VHF,VHPF
  REAL(DP), DIMENSION(M)        :: KHBOTTOM

# if defined (THIN_DAM)
  REAL(DP) :: ftmp,stmp
# endif

  IF(DBG_SET(DBG_SBR)) WRITE(IPT,*)"Start: FABM_MIXING :"

  UMOLPR = UMOL*1.E0_SP
  !
  !------------------------------------------------------------------------------!
  !                                                                              !
  !        the following section solves the equation                             !
  !         dti*(kh*f')'-f=-fb                                                   !
  !                                                                              !
  !------------------------------------------------------------------------------!

  DO K = 2, KBM1
     DO I = 1, M
#  if !defined (WET_DRY)
        !       IF (D(I) > 0.0_SP) THEN
#  else
        IF(ISWETN(I) == 1)THEN
#  endif
           FKH = KH(I,K)

           IF(K == KBM1) THEN
              KHBOTTOM(I)=FKH
           END IF

           AF(I,K-1)=-DTI*(FKH+UMOLPR)/(DZ(I,K-1)*DZZ(I,K-1)*D(I)*D(I))
           CF(I,K)=-DTI*(FKH+UMOLPR)/(DZ(I,K)*DZZ(I,K-1)*D(I)*D(I))
# if defined(WET_DRY)
        END IF
# endif
     END DO
  END DO

  DO IFABM=1,size(model%state_variables)

  !------------------------------------------------------------------------------!
  !   surface bcs;                                                        !
  !------------------------------------------------------------------------------!
  DO I = 1, M
#  if !defined (WET_DRY)
     !     IF (D(I) > 0.0_SP) THEN
#  else
     IF(ISWETN(I) == 1)THEN
#  endif
        VHF(I,1) = AF(I,1) / (AF(I,1)-1.)
        VHPF(I,1) = -F(I,1,IFABM) / (AF(I,1)-1.)
#  if defined (WET_DRY)
     END IF
# endif
  END DO
  !------------------------------------------------------------------------------!
  !   Interior;                                                        !
  !------------------------------------------------------------------------------!
  DO K = 2, KBM2
     DO I = 1, M
#  if !defined (WET_DRY)
        !     IF (D(I) > 0.0_SP) THEN
#  else
        IF(ISWETN(I) == 1)THEN
#  endif
           VHPF(I,K)=1./ (AF(I,K)+CF(I,K)*(1.-VHF(I,K-1))-1.)
           VHF(I,K) = AF(I,K) * VHPF(I,K)
           VHPF(I,K) = (CF(I,K)*VHPF(I,K-1)-DBLE(F(I,K,IFABM)))*VHPF(I,K)
#  if defined (WET_DRY)
        END IF
# endif
     END DO
  END DO

#  if !defined (WET_DRY)
  FF(1:M,1:KBM1) = F(1:M,1:KBM1,IFABM)
#  else
  DO  K = 1, KBM1
     DO  I = 1, M
        IF(ISWETN(I) == 1)THEN
           FF(I,K) = F(I,K,IFABM)
        END IF
     END DO
  END DO
#  endif

  !------------------------------------------------------------------------------!
  !   Bottom bcs;                                                        !
  !------------------------------------------------------------------------------!

  ! THIS PIECE OF CODE DESPERATELY NEEDS TO BE CLARIFIED
  ! AND STREAMLINED

  DO I = 1, M
     IF (ISONB(I) /= 2) THEN
#  if defined (WET_DRY)
        IF(ISWETN(I) == 1)THEN
#  endif
           TMP1=PFABMPXB(I,IFABM)*COS(SITA_GD(I))+PFABMPYB(I,IFABM)*SIN(SITA_GD(I))
           TMP2=AH_BOTTOM(I)*PHPN(I)
           TMP3=KHBOTTOM(I)+UMOLPR+AH_BOTTOM(I)*PHPN(I)*PHPN(I)
           TMP=TMP1*TMP2/TMP3*(KHBOTTOM(I)+UMOLPR)

           ! -------------------------------------------------------------------
   IF(NOFLUX_BOT_CONDITION)THEN
       IF (TMP1 < 0.0_SP) TMP=0.0_SP
       TMP = -TMP
   ELSE	   
           TMP = 0.0_SP
   END IF	   
           ! -------------------------------------------------------------------

!           FF(I,KBM1) = (CF(I,KBM1)*VHPF(I,KBM2)-FF(I,KBM1)) &
!                /(CF(I,KBM1)*(1._SP-VHF(I,KBM2))-1._SP)


           FF(I,KBM1) = (CF(I,KBM1)*VHPF(I,KBM2)-FF(I,KBM1)+DTI*(-TMP)/(D(I)*DZ(I,KBM1))) &
                /(CF(I,KBM1)*(1._SP-VHF(I,KBM2))-1._SP)


#  if defined (WET_DRY)
        END IF
# endif
     END IF
  END DO

  DO  K = 2, KBM1
     KI = KB - K
     DO  I = 1, M
        IF(ISONB(I) /= 2) THEN
#  if defined (WET_DRY)
           IF(ISWETN(I) == 1)THEN
#  endif
              FF(I,KI) = (VHF(I,KI)*FF(I,KI+1)+VHPF(I,KI))
#  if defined (WET_DRY)
           END IF
# endif
        END IF


     END DO
  END DO

  DO I = 1, M
#  if defined (WET_DRY)
     IF(ISWETN(I)*ISWETNT(I) == 1 )then
#  endif
        DO K = 1, KBM1
           F(I,K,IFABM) = FF(I,K)
        END DO
#  if defined (WET_DRY)
     END IF
#  endif
  END DO

#  if defined (THIN_DAM)
   DO K=1,KBM1
     DO I=1,NODE_DAM1_N
       IF(K<=KDAM(I_NODE_DAM1_N(I,1)).AND.K<=KDAM(I_NODE_DAM1_N(I,2)) )THEN
          FTMP=F(I_NODE_DAM1_N(I,1),K,IFABM)*ART1(I_NODE_DAM1_N(I,1)) &
            & +F(I_NODE_DAM1_N(I,2),K,IFABM)*ART1(I_NODE_DAM1_N(I,2))
          STMP=ART1(I_NODE_DAM1_N(I,1))+ART1(I_NODE_DAM1_N(I,2))
          F(I_NODE_DAM1_N(I,1),K,IFABM)=FTMP/STMP
          F(I_NODE_DAM1_N(I,2),K,IFABM)=FTMP/STMP
       END IF
     END DO

     DO I=1,NODE_DAM2_N
       IF(K<=KDAM(I_NODE_DAM2_N(I,1)).AND.K<=KDAM(I_NODE_DAM2_N(I,2)) &
          & .AND.K<=KDAM(I_NODE_DAM2_N(I,2)) )THEN
          FTMP= F(I_NODE_DAM2_N(I,1),K,IFABM)*ART1(I_NODE_DAM2_N(I,1)) &
           &   +F(I_NODE_DAM2_N(I,2),K,IFABM)*ART1(I_NODE_DAM2_N(I,2)) &
           &   +F(I_NODE_DAM2_N(I,3),K,IFABM)*ART1(I_NODE_DAM2_N(I,3)) 
          STMP=ART1(I_NODE_DAM2_N(I,1))+ART1(I_NODE_DAM2_N(I,2)) &
           &   +ART1(I_NODE_DAM2_N(I,3))
          F(I_NODE_DAM2_N(I,1),K,IFABM)=FTMP/STMP
          F(I_NODE_DAM2_N(I,2),K,IFABM)=FTMP/STMP
          F(I_NODE_DAM2_N(I,3),K,IFABM)=FTMP/STMP
       END IF
     END DO

     DO I=1,NODE_DAM3_N
       IF(K<=KDAM(I_NODE_DAM3_N(I,1)).AND.K<=KDAM(I_NODE_DAM3_N(I,2)) &
   & .AND.K<=KDAM(I_NODE_DAM3_N(I,3)).AND.K<=KDAM(I_NODE_DAM3_N(I,4)) )THEN
          FTMP =F(I_NODE_DAM3_N(I,1),K,IFABM)*ART1(I_NODE_DAM3_N(I,1)) &
           &   +F(I_NODE_DAM3_N(I,2),K,IFABM)*ART1(I_NODE_DAM3_N(I,2)) &
           &   +F(I_NODE_DAM3_N(I,3),K,IFABM)*ART1(I_NODE_DAM3_N(I,3)) &
           &   +F(I_NODE_DAM3_N(I,4),K,IFABM)*ART1(I_NODE_DAM3_N(I,4))  
          STMP =ART1(I_NODE_DAM3_N(I,1)) + ART1(I_NODE_DAM3_N(I,2)) &
           &  + ART1(I_NODE_DAM3_N(I,3)) + ART1(I_NODE_DAM3_N(I,4))
          F(I_NODE_DAM3_N(I,1),K,IFABM)=FTMP/STMP
          F(I_NODE_DAM3_N(I,2),K,IFABM)=FTMP/STMP
          F(I_NODE_DAM3_N(I,3),K,IFABM)=FTMP/STMP
          F(I_NODE_DAM3_N(I,4),K,IFABM)=FTMP/STMP
       END IF
     END DO
   END DO
#  endif

  END DO ! IFABM

  IF(DBG_SET(DBG_SBR)) WRITE(IPT,*)"End: FABM_MIXING"
  RETURN
END SUBROUTINE FABM_MIXING


!=============================================================================!
   SUBROUTINE FABM_ADV  
!=============================================================================!
!                                                                             !
!   This subroutine is used to calculate the horizontal advection and         !
!   and diffusion terms for the state variables of the adjustable biomodel    !
!=============================================================================!
  USE ALL_VARS
  USE LIMS
  USE BCS
  USE MOD_OBCS
  USE MOD_PAR
  USE MOD_WD
# if defined (SEMI_IMPLICIT)
  USE MOD_SEMI_IMPLICIT
# endif

  IMPLICIT NONE
  REAL(SP), DIMENSION(0:MT,KB,size(model%state_variables))  :: XFLUX,XFLUX_ADV 
  REAL(SP), DIMENSION(M)           :: PUPX,PUPY,PVPX,PVPY  
  REAL(SP), DIMENSION(M)           :: PFPX,PFPY,PFPXD,PFPYD,VISCOFF
  REAL(SP), DIMENSION(3*(NT),KBM1) :: DTIJ 
  REAL(SP), DIMENSION(3*(NT),KBM1) :: UVN
  REAL(SP) :: FFD,FF1   !,X11,Y11,X22,Y22,X33,Y33,TMP1,TMP2,XI,YI
  REAL(SP) :: FIJ1,FIJ2,UN ! removed DXA,DYA,DXB,DYB
  REAL(SP) :: TXX,TYY,FXX,FYY,VISCOF,EXFLUX,TEMP,STPOINT
  REAL(SP) :: FACT,FM1
  INTEGER  :: I,I1,I2,IA,IB,J,J1,J2,JTMP,K,JJ,IFABM
  REAL(SP) :: WQM1MIN, WQM1MAX, WQM2MIN, WQM2MAX

#  if defined (SEMI_IMPLICIT)
   REAL(SP) :: UN1
   REAL(SP), DIMENSION(3*(NT),KBM1) :: UVN1
   REAL(SP), DIMENSION(3*(NT),KBM1) :: DTIJ1
#  endif
#  if defined (MPDATA)
  REAL(SP) :: WQMMIN,WQMMAX,XXXX
  REAL(SP), DIMENSION(0:MT,KB)     :: WQM_S    !! temporary salinity in modified upwind
  REAL(SP), DIMENSION(0:MT,KB)     :: WQM_SF   !! temporary salinity in modified upwind
  REAL(SP), DIMENSION(0:MT,KB)     :: WWWS     
  REAL(SP), DIMENSION(0:MT,KB)     :: WWWSF   
  REAL(SP), DIMENSION(0:MT)        :: DTWWWS  
  REAL(SP), DIMENSION(0:MT,KB)     :: ZZZFLUX !! temporary total flux in corrected part
  REAL(SP), DIMENSION(0:MT,KB)     :: BETA    !! temporary beta coefficient in corrected part
  REAL(SP), DIMENSION(0:MT,KB)     :: BETAIN  !! temporary beta coefficient in corrected part
  REAL(SP), DIMENSION(0:MT,KB)     :: BETAOUT !! temporary beta coefficient in corrected part
  REAL(SP), DIMENSION(0:MT,KB)     :: FABM_FRESH    !! for source term
  INTEGER ITERA, NTERA
#  endif




  IF(DBG_SET(DBG_SBR)) WRITE(IPT,*)"Start: FABM_ADV"
  !------------------------------------------------------------------------------!


  SELECT CASE(HORIZONTAL_MIXING_TYPE)
  CASE ('closure')
     FACT = 1.0_SP
     FM1  = 0.0_SP
  CASE('constant')
     FACT = 0.0_SP
     FM1  = 1.0_SP
  CASE DEFAULT
     CALL FATAL_ERROR("UNKNOW HORIZONTAL MIXING TYPE:",&
          & TRIM(HORIZONTAL_MIXING_TYPE) )
  END SELECT

# if defined (SPHERICAL)
    CALL FATAL_ERROR("SPHERICAL OPTION NOT IMPLEMENTED IN FABM_ADV")
# endif
# if defined (THIN_DAM)
    CALL FATAL_ERROR("THIN_DAM OPTION NOT IMPLEMENTED IN FABM_ADV")
# endif
# if !defined(MPDATA)
#   if defined (SEMI_IMPLICIT)
      CALL FATAL_ERROR("SEMI_IMPLICIT OPTION NOT IMPLEMENTED IN FABM_ADV")
#   endif
#   if defined (NH)
      CALL FATAL_ERROR("NH OPTION NOT IMPLEMENTED IN FABM_ADV")
#   endif
#   if defined (LIMITER_VER_ADV)
      CALL FATAL_ERROR("LIMITER_VER_ADV OPTION NOT IMPLEMENTED IN FABM_ADV")
#   endif
# endif

  !
  !--Initialize Fluxes-----------------------------------------------------------!
  !
  XFLUX     = 0.0_SP
  XFLUX_ADV = 0.0_SP

  !
  !--Loop Over Control Volume Sub-Edges And Calculate Normal Velocity------------!
  !
!!#  if !defined (WET_DRY)
  DO I=1,NCV
     I1=NTRG(I)
     !     DTIJ(I)=DT1(I1)
     DO K=1,KBM1
       DTIJ(I,K) = DT1(I1)*DZ1(I1,K)
       UVN(I,K)  = V(I1,K)*DLTXE(I) - U(I1,K)*DLTYE(I)
#      if defined (SEMI_IMPLICIT)
       DTIJ1(I,K) = D1(I1)*DZ1(I1,K)
       UVN1(I,K) = VF(I1,K)*DLTXE(I) - UF(I1,K)*DLTYE(I)
#      endif
     END DO
  END DO


  !
  !--Calculate the Advection and Horizontal Diffusion Terms----------------------!
  !

 DO IFABM=1,size(model%state_variables)
  DO K=1,KBM1

     PFPX  = 0.0_SP
     PFPY  = 0.0_SP
     PFPXD = 0.0_SP
     PFPYD = 0.0_SP
     DO I=1,M
        DO J=1,NTSN(I)-1
           I1=NBSN(I,J)
           I2=NBSN(I,J+1)

#    if defined (WET_DRY)
         IF(ISWETN(I1) == 0 .AND. ISWETN(I2) == 1)THEN
          FFD=0.5_SP*(FABM_ALL(I,K,IFABM)+FABM_ALL(I2,K,IFABM)-FABM_MEAN(I,K,IFABM)-FABM_MEAN(I2,K,IFABM))
          FF1=0.5_SP*(FABM_ALL(I,K,IFABM)+FABM_ALL(I2,K,IFABM))
	 ELSE IF(ISWETN(I1) == 1 .AND. ISWETN(I2) == 0)THEN
          FFD=0.5_SP*(FABM_ALL(I1,K,IFABM)+FABM_ALL(I,K,IFABM)-FABM_MEAN(I1,K,IFABM)-FABM_MEAN(I,K,IFABM))
          FF1=0.5_SP*(FABM_ALL(I1,K,IFABM)+FABM_ALL(I,K,IFABM))
	 ELSE IF(ISWETN(I1) == 0 .AND. ISWETN(I2) == 0)THEN
          FFD=FABM_ALL(I,K,IFABM)-FABM_MEAN(I,K,IFABM)
          FF1=FABM_ALL(I,K,IFABM)
	 ELSE
          FFD=0.5_SP*(FABM_ALL(I1,K,IFABM)+FABM_ALL(I2,K,IFABM)-FABM_MEAN(I1,K,IFABM)-FABM_MEAN(I2,K,IFABM))
          FF1=0.5_SP*(FABM_ALL(I1,K,IFABM)+FABM_ALL(I2,K,IFABM))
	 END IF 
#    else	 
           FFD=0.5_SP*(FABM_ALL(I1,K,IFABM)+FABM_ALL(I2,K,IFABM)-FABM_MEAN(I1,K,IFABM)-FABM_MEAN(I2,K,IFABM))
           FF1=0.5_SP*(FABM_ALL(I1,K,IFABM)+FABM_ALL(I2,K,IFABM))
#    endif	 
	 

           
           PFPX(I)=PFPX(I)+FF1*DLTYTRIE(i,j)
           PFPY(I)=PFPY(I)+FF1*DLTXTRIE(i,j)
           PFPXD(I)=PFPXD(I)+FFD*DLTYTRIE(i,j)
           PFPYD(I)=PFPYD(I)+FFD*DLTXTRIE(i,j)
           

        END DO

# if !defined (THIN_DAM)
        PFPX(I)=PFPX(I)/ART2(I)
        PFPY(I)=PFPY(I)/ART2(I)
        PFPXD(I)=PFPXD(I)/ART2(I)
        PFPYD(I)=PFPYD(I)/ART2(I)
# endif

     END DO

     IF(K == KBM1)THEN
        DO I=1,M
           PFABMPXB(I,IFABM) = PFPX(I)
           PFABMPYB(I,IFABM) = PFPY(I)
        END DO
     END IF

     DO I = 1,M
        VISCOFF(I)=VISCOFH(I,K)
     END DO

     ! JornB: added based on appearance in adv_s.F 12/03/2015
     IF(K == KBM1) THEN
        AH_BOTTOM(1:M) = (FACT*VISCOFF(1:M) + FM1)*NN_HVC(1:M)
     END IF


     DO I=1,NCV_I
        IA=NIEC(I,1)
        IB=NIEC(I,2)



        FIJ1=FABM_ALL(IA,K,IFABM)+DLTXNCVE(I,1)*PFPX(IA)+DLTYNCVE(I,1)*PFPY(IA)
        FIJ2=FABM_ALL(IB,K,IFABM)+DLTXNCVE(I,2)*PFPX(IB)+DLTYNCVE(I,2)*PFPY(IB)

        WQM1MIN=MINVAL(FABM_ALL(NBSN(IA,1:NTSN(IA)-1),K,IFABM))
        WQM1MIN=MIN(WQM1MIN, FABM_ALL(IA,K,IFABM))
        WQM1MAX=MAXVAL(FABM_ALL(NBSN(IA,1:NTSN(IA)-1),K,IFABM))
        WQM1MAX=MAX(WQM1MAX, FABM_ALL(IA,K,IFABM))
        WQM2MIN=MINVAL(FABM_ALL(NBSN(IB,1:NTSN(IB)-1),K,IFABM))
        WQM2MIN=MIN(WQM2MIN, FABM_ALL(IB,K,IFABM))
        WQM2MAX=MAXVAL(FABM_ALL(NBSN(IB,1:NTSN(IB)-1),K,IFABM))
        WQM2MAX=MAX(WQM2MAX, FABM_ALL(IB,K,IFABM))
        IF(FIJ1 < WQM1MIN) FIJ1=WQM1MIN
        IF(FIJ1 > WQM1MAX) FIJ1=WQM1MAX
        IF(FIJ2 < WQM2MIN) FIJ2=WQM2MIN
        IF(FIJ2 > WQM2MAX) FIJ2=WQM2MAX

        UN=UVN(I,K)
#      if defined (SEMI_IMPLICIT)
       UN1=UVN1(I,K)
#      endif

        !VISCOF=HORCON*(FACT*(VISCOFF(IA)+VISCOFF(IB))*0.5_SP + FM1)
        
        ! David moved HPRNU and added HVC
        VISCOF=(FACT*0.5_SP*(VISCOFF(IA)*NN_HVC(IA)+VISCOFF(IB)*NN_HVC(IB)) + FM1*0.5_SP*(NN_HVC(IA)+NN_HVC(IB)))
        
        TXX=0.5_SP*(PFPXD(IA)+PFPXD(IB))*VISCOF
        TYY=0.5_SP*(PFPYD(IA)+PFPYD(IB))*VISCOF

        FXX=-DTIJ(I,K)*TXX*DLTYE(I)
        FYY= DTIJ(I,K)*TYY*DLTXE(I)

#       if !defined (SEMI_IMPLICIT)
        EXFLUX=-UN*DTIJ(I,K)* &
             ((1.0_SP+SIGN(1.0_SP,UN))*FIJ2+(1.0_SP-SIGN(1.0_SP,UN))*FIJ1)*0.5_SP+FXX+FYY
#       else
        EXFLUX=-UN*DTIJ(I,K)* &
           ((1.0_SP+SIGN(1.0_SP,UN))*FIJ2+(1.0_SP-SIGN(1.0_SP,UN))*FIJ1)*0.5_SP
        EXFLUX=(1.0_SP-IFCETA)*EXFLUX+IFCETA*(-UN1*DTIJ1(I,K)*((1.0_SP+SIGN(1.0_SP,UN1))*FIJ2+(1.0_SP-SIGN(1.0_SP,UN1))*FIJ1)*0.5_SP)+FXX+FYY
#       endif

        XFLUX(IA,K,IFABM)=XFLUX(IA,K,IFABM)+EXFLUX
        XFLUX(IB,K,IFABM)=XFLUX(IB,K,IFABM)-EXFLUX

        XFLUX_ADV(IA,K,IFABM)=XFLUX_ADV(IA,K,IFABM)+(EXFLUX-FXX-FYY)
        XFLUX_ADV(IB,K,IFABM)=XFLUX_ADV(IB,K,IFABM)-(EXFLUX-FXX-FYY)

     END DO


  END DO !!SIGMA LOOP
 END DO !to size(model%state_variables)

  !
  !-Accumulate Fluxes at Boundary Nodes
  !
# if defined (MULTIPROCESSOR)
 DO IFABM=1,size(model%state_variables)
  IF(PAR)CALL NODE_MATCH(0,NBN,BN_MLT,BN_LOC,BNC,MT,KB,MYID,NPROCS,XFLUX(:,:,IFABM),XFLUX_ADV(:,:,IFABM))
 END DO
# endif

 DO IFABM=1,size(model%state_variables)
  DO K=1,KBM1
     IF(IOBCN > 0) THEN
        DO I=1,IOBCN
           I1=I_OBC_N(I)
           XFLUX_OBCB(I,K,IFABM)=XFLUX_ADV(I1,K,IFABM)
        END DO
     END IF
  END DO
 END DO

 DO IFABM=1,size(model%state_variables)
  
  !--Set Boundary Conditions-For Fresh Water Flux--------------------------------!
  !
# if defined (MPDATA)

  !   S. HU
  !   Using smolarkiewicz, P. K; A fully multidimensional positive definite advection
  !   transport algorithm with small implicit diffusion, Journal of Computational
  !   Physics, 54, 325-362, 1984
  !-----------------------------------------------------------------

  !-----combine all the horizontal flux first-----------------------------------

  !-------fresh water part--------------

  FABM_FRESH=FABM_ALL(:,:,IFABM)


  IF(RIVER_TS_SETTING == 'calculated') THEN
     IF(RIVER_INFLOW_LOCATION == 'node') THEN
        IF(NUMQBC > 0) THEN
           DO J=1,NUMQBC
              JJ=INODEQ(J)
              STPOINT=FABMDIS(J,IFABM)
              DO K=1,KBM1
                 FABM_FRESH(JJ,K)=FABMDIS(J,IFABM)
                 XFLUX(JJ,K,IFABM)=XFLUX(JJ,K,IFABM) - QDIS(J)*VQDIST(J,K)*STPOINT
              END DO
           END DO
        END IF
     ELSE IF(RIVER_INFLOW_LOCATION == 'edge') THEN
        IF(NUMQBC > 0) THEN
           DO J=1,NUMQBC
              J1=N_ICELLQ(J,1)
              J2=N_ICELLQ(J,2)
              STPOINT=FABMDIS(J,IFABM)!!ASK LIU SHOULD THIS BE STPOINT1(J1)/STPOINT2(J2)
              DO K=1,KBM1
                 FABM_FRESH(J1,K)=FABMDIS(J,IFABM)
                 FABM_FRESH(J2,K)=FABMDIS(J,IFABM)
                 XFLUX(J1,K,IFABM)=XFLUX(J1,K,IFABM)-  &
                      QDIS(J)*RDISQ(J,1)*VQDIST(J,K)*STPOINT
                 XFLUX(J2,K,IFABM)=XFLUX(J2,K,IFABM)-  &
                      QDIS(J)*RDISQ(J,2)*VQDIST(J,K)*STPOINT
              END DO
           END DO
        END IF
     END IF
  END IF
  !


  ! The horizontal term of advection is neglected here
  DO K=1,KBM1
     DO I=1,M
        IF(ISONB(I) == 2) THEN
           XFLUX(I,K,IFABM)=0.
        ENDIF
     END DO
  END DO

  ! Initialize variables of MPDATA
  WQM_S=0._SP
  WQM_SF=0._SP
  WWWS=0._SP
  WWWSF=0._SP
  DTWWWS=0._SP
  ZZZFLUX=0._SP
  BETA=0._SP
  BETAIN=0._SP
  BETAOUT=0._SP

  !!   first loop for vertical upwind
  !!   flux including horizontal and vertical upwind
  DO K=1,KBM1
     DO I=1,M
#    if defined (WET_DRY)
        IF(ISWETN(I)*ISWETNT(I) == 1) THEN
#    endif

           IF(K == 1) THEN
              TEMP = -(WTS(I,K+1)-ABS(WTS(I,K+1)))*FABM_ALL(I,K,IFABM)   &
                   -(WTS(I,K+1)+ABS(WTS(I,K+1)))*FABM_ALL(I,K+1,IFABM) &
                   +(WTS(I,K)+ABS(WTS(I,K)))*FABM_ALL(I,K,IFABM)    
           ELSE IF(K == KBM1) THEN
              TEMP = +(WTS(I,K)-ABS(WTS(I,K)))*FABM_ALL(I,K-1,IFABM)     &
                   +(WTS(I,K)+ABS(WTS(I,K)))*FABM_ALL(I,K,IFABM)
           ELSE
              TEMP = -(WTS(I,K+1)-ABS(WTS(I,K+1)))*FABM_ALL(I,K,IFABM)   &
                   -(WTS(I,K+1)+ABS(WTS(I,K+1)))*FABM_ALL(I,K+1,IFABM) &
                   +(WTS(I,K)-ABS(WTS(I,K)))*FABM_ALL(I,K-1,IFABM)     &
                   +(WTS(I,K)+ABS(WTS(I,K)))*FABM_ALL(I,K,IFABM)
           END IF
           TEMP = 0.5_SP*TEMP 

           IF(K == 1)THEN
              WQMMAX = MAXVAL(FABM_ALL(NBSN(I,1:NTSN(I)),K,IFABM))
              WQMMIN = MINVAL(FABM_ALL(NBSN(I,1:NTSN(I)),K,IFABM))
              WQMMAX = MAX(WQMMAX,FABM_ALL(I,K+1,IFABM),FABM_ALL(I,K,IFABM),FABM_FRESH(I,K))
              WQMMIN = MIN(WQMMIN,FABM_ALL(I,K+1,IFABM),FABM_ALL(I,K,IFABM),FABM_FRESH(I,K))
           ELSEIF(K == KBM1) THEN
              WQMMAX = MAXVAL(FABM_ALL(NBSN(I,1:NTSN(I)),K,IFABM))
              WQMMIN = MINVAL(FABM_ALL(NBSN(I,1:NTSN(I)),K,IFABM))
              WQMMAX = MAX(WQMMAX,FABM_ALL(I,K-1,IFABM),FABM_ALL(I,K,IFABM),FABM_FRESH(I,K))
              WQMMIN = MIN(WQMMIN,FABM_ALL(I,K-1,IFABM),FABM_ALL(I,K,IFABM),FABM_FRESH(I,K))
           ELSE
              WQMMAX = MAXVAL(FABM_ALL(NBSN(I,1:NTSN(I)),K,IFABM))
              WQMMIN = MINVAL(FABM_ALL(NBSN(I,1:NTSN(I)),K,IFABM))
              WQMMAX = MAX(WQMMAX,FABM_ALL(I,K+1,IFABM),FABM_ALL(I,K-1,IFABM),FABM_ALL(I,K,IFABM),FABM_FRESH(I,K))
              WQMMIN = MIN(WQMMIN,FABM_ALL(I,K+1,IFABM),FABM_ALL(I,K-1,IFABM),FABM_ALL(I,K,IFABM),FABM_FRESH(I,K))
           END IF


           ZZZFLUX(I,K) = TEMP*(DTI/DT(I))/DZ(I,K) + XFLUX(I,K,IFABM)/ART1(I)*(DTI/DT(I))/DZ(I,K) 
           XXXX = ZZZFLUX(I,K)*DT(I)/DTFA(I)+FABM_ALL(I,K,IFABM)-FABM_ALL(I,K,IFABM)*DT(I)/DTFA(I) 

           BETA(I,K)=0.5*(1.-SIGN(1.0_SP,XXXX)) * (WQMMAX-FABM_ALL(I,K,IFABM))/(ABS(XXXX)+1.E-10) &
                +0.5*(1.-SIGN(1.0_SP,-XXXX)) * (FABM_ALL(I,K,IFABM)-WQMMIN)/(ABS(XXXX)+1.E-10)

           WQM_SF(I,K)=FABM_ALL(I,K,IFABM)-MIN(1.,BETA(I,K))*XXXX

#    if defined (WET_DRY)
        END IF
#    endif
     END DO
  END DO  !! SIGMA LOOP

  !----------------------------------------------------------------------------------------
  NTERA = 4
  DO ITERA=1,NTERA   !! Smolaricizw Loop 
     IF(ITERA == 1)THEN
        WWWSF  = WTS
        WQM_S   = WQM_SF
        DTWWWS = DT
     ELSE
        WWWSF  = WWWS
        WQM_S   = WQM_SF
        DTWWWS = DTFA
     END IF
     DO K=2,KBM1
        DO I=1,M
           TEMP=ABS(WWWSF(I,K))-DTI*(WWWSF(I,K))*(WWWSF(I,K))/DZ(I,K)/DTWWWS(I)
           WWWS(I,K)=TEMP*(WQM_S(I,K-1)-WQM_S(I,K))/(ABS(WQM_S(I,K-1))+ABS(WQM_S(I,K))+1.E-14)

           IF(TEMP < 0.0_SP .OR. WQM_S(I,K) == 0.0_SP)THEN 
              WWWS(I,K)=0. 
           END IF
        END DO
     END DO
     DO I=1,M
        WWWS(I,1)=0.
     END DO

     DO I=1,M
        WQMMAX = MAXVAL(FABM_ALL(NBSN(I,1:NTSN(I)),1,IFABM))
        WQMMIN = MINVAL(FABM_ALL(NBSN(I,1:NTSN(I)),1,IFABM))
        WQMMAX = MAX(WQMMAX,FABM_ALL(I,2,IFABM),FABM_ALL(I,1,IFABM),FABM_FRESH(I,1))
        WQMMIN = MIN(WQMMIN,FABM_ALL(I,2,IFABM),FABM_ALL(I,1,IFABM),FABM_FRESH(I,1))

        TEMP=0.5*((WWWS(I,2)+ABS(WWWS(I,2)))*WQM_S(I,2))*(DTI/DTFA(I))/DZ(I,1)
        BETAIN(I,1)=(WQMMAX-WQM_S(I,1))/(TEMP+1.E-10)

        TEMP=0.5*((WWWS(I,1)+ABS(WWWS(I,1)))*WQM_S(I,1)-        &
             (WWWS(I,2)-ABS(WWWS(I,2)))*WQM_S(I,1))*(DTI/DTFA(I))/DZ(I,1)
        BETAOUT(I,1)=(WQM_S(I,1)-WQMMIN)/(TEMP+1.E-10)

        WWWSF(I,1)=0.5*MIN(1.,BETAOUT(I,1))*(WWWS(I,1)+ABS(WWWS(I,1))) + &
             0.5*MIN(1.,BETAIN(I,1))*(WWWS(I,1)-ABS(WWWS(I,1)))
     END DO

     ! JornB: use KBM1-1 as in adv_s, rather than KBM1
     DO K=2,KBM1-1
        DO I=1,M
           WQMMAX = MAXVAL(FABM_ALL(NBSN(I,1:NTSN(I)),K,IFABM))
           WQMMIN = MINVAL(FABM_ALL(NBSN(I,1:NTSN(I)),K,IFABM))
           WQMMAX = MAX(WQMMAX,FABM_ALL(I,K+1,IFABM),FABM_ALL(I,K-1,IFABM),FABM_FRESH(I,K))
           WQMMIN = MIN(WQMMIN,FABM_ALL(I,K+1,IFABM),FABM_ALL(I,K-1,IFABM),FABM_FRESH(I,K))

           TEMP=0.5*((WWWS(I,K+1)+ABS(WWWS(I,K+1)))*WQM_S(I,K+1)-  &
                (WWWS(I,K)-ABS(WWWS(I,K)))*WQM_S(I,K-1))*(DTI/DTFA(I))/DZ(I,K)
           BETAIN(I,K)=(WQMMAX-WQM_S(I,K))/(TEMP+1.E-10)

           TEMP=0.5*((WWWS(I,K)+ABS(WWWS(I,K)))*WQM_S(I,K)-        &
                (WWWS(I,K+1)-ABS(WWWS(I,K+1)))*WQM_S(I,K))*(DTI/DTFA(I))/DZ(I,K)
           BETAOUT(I,K)=(WQM_S(I,K)-WQMMIN)/(TEMP+1.E-10)

           WWWSF(I,K)=0.5*MIN(1.,BETAIN(I,K-1),BETAOUT(I,K))*(WWWS(I,K)+ABS(WWWS(I,K))) + &
                0.5*MIN(1.,BETAIN(I,K),BETAOUT(I,K-1))*(WWWS(I,K)-ABS(WWWS(I,K)))
        END DO
     END DO

     ! JornB: added this section based on adv_s implementation
     K=KBM1
     DO I=1,M
        WQMMAX = MAXVAL(FABM_ALL(NBSN(I,1:NTSN(I)),K,IFABM))
        WQMMIN = MINVAL(FABM_ALL(NBSN(I,1:NTSN(I)),K,IFABM))
        WQMMAX = MAX(WQMMAX,FABM_ALL(I,K-1,IFABM),FABM_ALL(I,K,IFABM),FABM_FRESH(I,K))
        WQMMIN = MIN(WQMMIN,FABM_ALL(I,K-1,IFABM),FABM_ALL(I,K,IFABM),FABM_FRESH(I,K))

        TEMP=0.5*((WWWS(I,K+1)+ABS(WWWS(I,K+1)))*WQM_S(I,K+1)-  &
             (WWWS(I,K)-ABS(WWWS(I,K)))*WQM_S(I,K-1))*(DTI/DTFA(I))/DZ(I,K)
        BETAIN(I,K)=(WQMMAX-WQM_S(I,K))/(TEMP+1.E-10)

        TEMP=0.5*((WWWS(I,K)+ABS(WWWS(I,K)))*WQM_S(I,K)-        &
             (WWWS(I,K+1)-ABS(WWWS(I,K+1)))*WQM_S(I,K))*(DTI/DTFA(I))/DZ(I,K)
        BETAOUT(I,K)=(WQM_S(I,K)-WQMMIN)/(TEMP+1.E-10)

        WWWSF(I,K)=0.5*MIN(1.,BETAIN(I,K-1),BETAOUT(I,K))*(WWWS(I,K)+ABS(WWWS(I,K))) + &
             0.5*MIN(1.,BETAIN(I,K),BETAOUT(I,K-1))*(WWWS(I,K)-ABS(WWWS(I,K)))
     END DO


     WWWS=WWWSF 

     DO K=1,KBM1
        DO I=1,M
#      if defined (WET_DRY)
           IF(ISWETN(I)*ISWETNT(I) == 1) THEN
#      endif
              IF(K == 1) THEN
                 TEMP = -(WWWS(I,K+1)-ABS(WWWS(I,K+1)))*WQM_S(I,K)   &
                      -(WWWS(I,K+1)+ABS(WWWS(I,K+1)))*WQM_S(I,K+1) &
                      +(WWWS(I,K)+ABS(WWWS(I,K)))*WQM_S(I,K)
              ELSE IF(K == KBM1) THEN
                 TEMP = +(WWWS(I,K)-ABS(WWWS(I,K)))*WQM_S(I,K-1)     &
                      +(WWWS(I,K)+ABS(WWWS(I,K)))*WQM_S(I,K)
              ELSE
                 TEMP = -(WWWS(I,K+1)-ABS(WWWS(I,K+1)))*WQM_S(I,K)   &
                      -(WWWS(I,K+1)+ABS(WWWS(I,K+1)))*WQM_S(I,K+1) &
                      +(WWWS(I,K)-ABS(WWWS(I,K)))*WQM_S(I,K-1)     &
                      +(WWWS(I,K)+ABS(WWWS(I,K)))*WQM_S(I,K)
              END IF
              TEMP = 0.5_SP*TEMP
              WQM_SF(I,K)=(WQM_S(I,K)-TEMP*(DTI/DTFA(I))/DZ(I,K)) 
#      if defined (WET_DRY)
           END IF
#      endif
        END DO
     END DO  !! SIGMA LOOP
  END DO  !! Smolarvizw Loop
  !--------------------------------------------------------------------------
  ! End of smolarkiewicz upwind loop
  !--------------------------------------------------------------------------
#  endif


# if ! defined(MPDATA)
!
!--Calculate the Vertical Terms------------------------------------------------
!

  DO I=1,M
#    if defined (WET_DRY)
     IF(ISWETN(I)*ISWETNT(I) == 1) THEN
#    endif


        DO K=1, KBM1

           IF(K == 1) THEN
              TEMP=-WTS(I,K+1)*(FABM_ALL(I,K,IFABM)*DZ(I,K+1)+FABM_ALL(I,K+1,IFABM)*DZ(I,K))/   &
                   (DZ(I,K)+DZ(I,K+1))
           ELSE IF(K == KBM1) THEN
              TEMP= WTS(I,K)*(FABM_ALL(I,K,IFABM)*DZ(I,K-1)+FABM_ALL(I,K-1,IFABM)*DZ(I,K))/(DZ(I,K)+DZ(I,K-1))
           ELSE
              TEMP= WTS(I,K)*(FABM_ALL(I,K,IFABM)*DZ(I,K-1)+FABM_ALL(I,K-1,IFABM)*DZ(I,K))/(DZ(I,K)+DZ(I,K-1))-&
                   WTS(I,K+1)*(FABM_ALL(I,K,IFABM)*DZ(I,K+1)+FABM_ALL(I,K+1,IFABM)*DZ(I,K))/(DZ(I,K)+DZ(I,K+1))
           END IF

!
!--Total Fluxes ---------------------------------------------------------------
!
           IF(ISONB(I) == 2) THEN
              XFLUX(I,K,IFABM)=TEMP*ART1(I)
           ELSE
              XFLUX(I,K,IFABM)=XFLUX(I,K,IFABM)+TEMP*ART1(I)
           END IF
        ENDDO
#    if defined (WET_DRY)
     END IF
#    endif
  END DO  !! SIGMA LOOP

  !
  !--Set Boundary Conditions-For Fresh Water Flux--------------------------------!
  !
  IF(RIVER_TS_SETTING == 'calculated') THEN
     IF(RIVER_INFLOW_LOCATION == 'node') THEN
        IF(NUMQBC > 0) THEN
           DO J=1,NUMQBC
              JJ=INODEQ(J)
              STPOINT=FABMDIS(J,IFABM)
              DO K=1,KBM1
                 XFLUX(JJ,K,IFABM)=XFLUX(JJ,K,IFABM) - QDIS(J)*VQDIST(J,K)*STPOINT
              END DO
           END DO
        END IF
     ELSE IF(RIVER_INFLOW_LOCATION == 'edge') THEN
        IF(NUMQBC > 0) THEN
           DO J=1,NUMQBC
              J1=N_ICELLQ(J,1)
              J2=N_ICELLQ(J,2)
              STPOINT=FABMDIS(J,IFABM) !!ASK LIU SHOULD THIS BE STPOINT1(J1)/STPOINT2(J2)
              DO K=1,KBM1
                 XFLUX(J1,K,IFABM)=XFLUX(J1,K,IFABM)-QDIS(J)*RDISQ(J,1)*VQDIST(J,K)*STPOINT
                 XFLUX(J2,K,IFABM)=XFLUX(J2,K,IFABM)-QDIS(J)*RDISQ(J,2)*VQDIST(J,K)*STPOINT
              END DO
           END DO
        END IF
     END IF
  END IF
!---------------------------------------------------------------------
# endif

  !--Update Variables------------------------------------------------------------!
  !
#  if defined (WET_DRY)
  DO I=1,M
     IF(ISWETN(I)*ISWETNT(I) == 1 )THEN
        DO K=1,KBM1
#    if !defined (MPDATA)     
#      if !defined (SEMI_IMPLICIT)

#      if !defined (THIN_DAM)
           FABM_F(I,K,IFABM)=(FABM_ALL(I,K,IFABM)-XFLUX(I,K,IFABM)/ART1(I)*(DTI/(DT(I)*DZ(I,K))))*(DT(I)/DTFA(I))
#      else
#      endif

#      else
#      endif
#    else
           FABM_F(I,K,IFABM)=WQM_SF(I,K)
#    endif              
        END DO

     ELSE ! IF NOT WET S STAYS THE SAME
        DO K=1,KBM1
#      if !defined (SEMI_IMPLICIT)
           FABM_F(I,K,IFABM)=FABM_ALL(I,K,IFABM)
#      else
#      endif
        END DO
     END IF
  END DO

# else

  DO I=1,M
     DO K=1,KBM1
#    if !defined (MPDATA)     
#      if !defined (SEMI_IMPLICIT)

#      if !defined (THIN_DAM)
        FABM_F(I,K,IFABM)=(FABM_ALL(I,K,IFABM)-XFLUX(I,K,IFABM)/ART1(I)*(DTI/(DT(I)*DZ(I,K))))*(DT(I)/DTFA(I))
#      else
#      endif

#      else
#      endif
#    else
        FABM_F(I,K,IFABM)=WQM_SF(I,K)
#    endif              
     END DO

  END DO

# endif

  END DO !do IFABM=1,size(model%state_variables)

  IF(DBG_SET(DBG_SBR)) WRITE(IPT,*)"End: FABM_ADV"

END SUBROUTINE FABM_ADV
!==============================================================================|

  SUBROUTINE FABM_BCOND()
!==============================================================================|
!   Set Boundary Conditions for FABM                                         |
!==============================================================================|

!------------------------------------------------------------------------------|
   USE ALL_VARS
   USE BCS
   USE MOD_OBCS
   IMPLICIT NONE
   REAL(SP) :: T2D,T2D_NEXT,T2D_OBC,XFLUX2D,TMP,RAMP_BIO
   INTEGER  :: I,J,K,J1,J11,J22,NCON2,IFABM
   REAL(SP) ::WQMMAX,WQMMIN

!   ALLOCATE(WDIS(NUMQBC,ntt))     ;WDIS      = ZERO
!------------------------------------------------------------------------------|

! make use of masks for  OBC. Non suplied variables will get diluted (in the case of the rivers) or fixed
! to interior values for OBCs. 
!------------------------------------------------------------------------------|
!!!! River nutrient treatment for FABM as SPECIFIED rather than CALCULATED which is handled during advection
! FABMDIS is initialised to zero so we don't need a mask. Not-read values will be zero
!--SET CONDITIONS FOR FRESH WATER INFLOW for SPECIFIED CASE!!---------------------------------|
!
   IF(RIVER_TS_SETTING == 'specified') THEN
     ! Impact of rivers is accounted for by OVERWRITING concentrations at inflow nodes with
     ! the concentrations in the river. Note: the alternative RIVER_TS_SETTING=='calculated'
     ! updates the concentration at the inflow nodes by taking into account their volume, the
     ! local concentration, the river concentration and the river flow rate. This scenario is
     ! handled in FABM_ADV.
     IF(NUMQBC > 0) THEN
       IF(RIVER_INFLOW_LOCATION == 'node') THEN
         DO I=1,NUMQBC
           J11=INODEQ(I)
           DO K=1,KBM1
             DO IFABM=1,size(model%state_variables)
               FABM_F(J11,K,IFABM) = FABMDIS(I,IFABM)
             END DO
           END DO
         END DO
       ELSE IF(RIVER_INFLOW_LOCATION == 'edge') THEN
         DO I=1,NUMQBC
           J11=N_ICELLQ(I,1)
           J22=N_ICELLQ(I,2)
           DO K=1,KBM1
             DO IFABM=1,size(model%state_variables)
               FABM_F(J11,K,IFABM)=FABMDIS(I,IFABM)
               FABM_F(J22,K,IFABM)=FABMDIS(I,IFABM)
             END DO
           END DO
         END DO
       END IF
     END IF
   END IF

       
   IF(IOBCN > 0) THEN
!
!  SET CONDITIONS ON OUTER BOUNDARY
! Variables presence is a function of variable presence in NETCDF file
! different handling is only needed when tracers come into the domain
     IF(OBC_FABM_NUDGING) CALL UPDATE_OBC_FABM(IntTime,FABM_OBC)
     RAMP_BIO = TANH(FLOAT(IINT)/FLOAT(IRAMP+1))
     DO IFABM=1,size(model%state_variables)
       DO I=1,IOBCN
         J=I_OBC_N(I)
         IF(UARD_OBCN(I) > 0.0_SP) THEN
           ! Outward flow over open boundary. Imposed boundary condition has no effect.
           J1=NEXT_OBC(I)

           ! Depth-average mass at current boundary node and its interior neighbor (T2D, T2D_NEXT),
           ! as well as mass flux over the boundary (XFLUX2D). Note that T2D_NEXT is the average of
           ! the forecast field (i.e., after advection), whereas T2D is the average of the current field.
           T2D=0.0_SP
           T2D_NEXT=0.0_SP
           XFLUX2D=0.0_SP
           DO K=1,KBM1
            T2D=T2D+FABM_ALL(J,K,IFABM)*DZ(J,K)
            T2D_NEXT=T2D_NEXT+FABM_F(J1,K,IFABM)*DZ(J1,K)
            XFLUX2D=XFLUX2D+XFLUX_OBCB(I,K,IFABM)           !XFLUX_OBCB is set in ADVECTION routine
           END DO

           ! Add flux across open boundary to existing flux for this node [which presumably did not yet
           ! include the flux over the boundary itself]
           TMP=XFLUX2D+T2D*UARD_OBCN(I)

           ! Forecast depth-averaged tracer at the boundary based on previous value and flux TMP.
           T2D_OBC=(T2D*DT(J)-TMP*DTI/ART1(J))/D(J)

           ! For the new depth-explicit value at the boundary, combine the forecast depth-averaged value
           ! with the differences from the the depth-averaged value as forecast for the neighbouring node.
           DO K=1,KBM1
             FABM_ALL(J,K,IFABM)=T2D_OBC+(FABM_ALL(J1,K,IFABM)-T2D_NEXT)
           END DO

         DO K=1,KBM1
           ! Minimum and maximum of the values at [horizontally] neighboring cell centers.
           WQMMAX = MAXVAL(FABM_ALL(NBSN(J,1:NTSN(J)),K,IFABM))
           WQMMIN = MINVAL(FABM_ALL(NBSN(J,1:NTSN(J)),K,IFABM))

           ! Minimum and maximum of the values at [vertically] neighbouring top and bottom interfaces of the cell.
           ! Values at the interfaces are found through linear interpolation, accounting for differences in sigma height.
           IF(K == 1)THEN
            WQMMAX = MAX(WQMMAX,(FABM_ALL(J,K,IFABM)*DZ(J,K+1)+FABM_ALL(J,K+1,IFABM)*DZ(J,K))/  &
	             (DZ(J,K)+DZ(J,K+1)))
            WQMMIN = MIN(WQMMIN,(FABM_ALL(J,K,IFABM)*DZ(J,K+1)+FABM_ALL(J,K+1,IFABM)*DZ(J,K))/  &
	             (DZ(J,K)+DZ(J,K+1)))
           ELSE IF(K == KBM1)THEN
            WQMMAX = MAX(WQMMAX,(FABM_ALL(J,K,IFABM)*DZ(J,K-1)+FABM_ALL(J,K-1,IFABM)*DZ(J,K))/  &
	             (DZ(J,K)+DZ(J,K-1)))
            WQMMIN = MIN(WQMMIN,(FABM_ALL(J,K,IFABM)*DZ(J,K-1)+FABM_ALL(J,K-1,IFABM)*DZ(J,K))/  &
	             (DZ(J,K)+DZ(J,K-1)))
           ELSE
            WQMMAX = MAX(WQMMAX,(FABM_ALL(J,K,IFABM)*DZ(J,K-1)+FABM_ALL(J,K-1,IFABM)*DZ(J,K))/  &
	             (DZ(J,K)+DZ(J,K-1)), &
                    (FABM_ALL(J,K,IFABM)*DZ(J,K+1)+FABM_ALL(J,K+1,IFABM)*DZ(J,K))/  &
		     (DZ(J,K)+DZ(J,K+1)))
            WQMMIN = MIN(WQMMIN,(FABM_ALL(J,K,IFABM)*DZ(J,K-1)+FABM_ALL(J,K-1,IFABM)*DZ(J,K))/  &
	             (DZ(J,K)+DZ(J,K-1)), &
                    (FABM_ALL(J,K,IFABM)*DZ(J,K+1)+FABM_ALL(J,K+1,IFABM)*DZ(J,K))/  &
		     (DZ(J,K)+DZ(J,K+1)))
           END IF
 
           ! Clip to extremes of all neighbours. The effect of this should be dampening of grid-scale oscillations.
           IF(WQMMIN-FABM_F(J,K,IFABM) > 0.0_SP)FABM_F(J,K,IFABM) = WQMMIN
           IF(FABM_F(J,K,IFABM)-WQMMAX > 0.0_SP)FABM_F(J,K,IFABM) = WQMMAX

         END DO

          ELSE
           ! Inward flux over open boundary. Optionally nudge towards prescribed values.
           IF(OBC_FABM_NUDGING .AND. FABM_OBC_PRESCRIBED(IFABM))  THEN
             DO K=1,KBM1
               FABM_F(J,K,IFABM) = FABM_ALL(J,K,IFABM) - OBC_FABM_NUDGING_TIMESCALE*RAMP_BIO*(FABM_ALL(J,K,IFABM)&
                      &-FABM_OBC(I,K,IFABM))
             END DO
           ELSE
             ! No OBC information exist or no nudging is required. Keep previous interior values
             DO K=1,KBM1
               FABM_F(J,K,IFABM)=FABM_ALL(J,K,IFABM)
             END DO
           END IF
         END IF
       END DO
     END DO !!OUTER LOOP OVER BIO-VARIABLES IFABM=1,size(model%state_variables)

   END IF

!
!--SET BOUNDARY CONDITIONS-----------------------------------------------------|
!
       FABM_ALL(0,:,:)=ZERO ! don't know what this is doing here
   RETURN

  END SUBROUTINE FABM_BCOND
!==============================================================================!

  SUBROUTINE fvcom_driver_fatal_error(self,location,message)
     CLASS (type_fvcom_driver), INTENT(INOUT) :: self
     CHARACTER(LEN=*),          INTENT(IN)    :: location,message
     CALL FATAL_ERROR('FABM '//TRIM(location)//': '//TRIM(message))
  END SUBROUTINE

  SUBROUTINE fvcom_driver_log_message(self,message)
     CLASS (type_fvcom_driver), INTENT(INOUT) :: self
     CHARACTER(LEN=*),          INTENT(IN)    :: message
     WRITE (IPT,*) TRIM(message)
  END SUBROUTINE
!==========================================================================
! Calculate LED Limiter L(u,v) FOR vertical flux as per Sediment module - RJT2015  
!==========================================================================
  FUNCTION Lim(a,b)
     REAL(SP) :: lim,a,b
     REAL(SP) :: q,R
     REAL(SP) :: eps
     REAL(SP),PARAMETER ::qlim = 1.0
  eps = epsilon(eps)
  
 ! exponent
 ! qlim = 0. !1st order
 ! qlim = 1. !minmod
 ! qlim = 2. !van leer

  R = abs(   (a-b)/(abs(a)+abs(b)+eps) )**qlim
  lim = .5*(1-R)*(a+b)

  END FUNCTION Lim

SUBROUTINE FCT_FABM
!==============================================================================|
!  FLUX CONTROL FOR FABM variables                                                        |
!==============================================================================|
  !#  if defined (WET_DRY)

  !==============================================================================|
  USE BCS
  USE MOD_OBCS
  IMPLICIT NONE
  REAL(SP):: FMAX,FMIN
  INTEGER :: I,J,K,K1
  INTEGER :: IFABM
!==============================================================================|

  IF(DBG_SET(DBG_SBR)) WRITE(IPT,*)"Start: fct_FABM"

  fabm: DO IFABM=1,size(model%state_variables)
   nodes: DO I=1,M

     ! SKIP OPEN BOUNDARY NODES
     IF(IOBCN > 0)THEN
        DO J=1,IOBCN
           IF(I == I_OBC_N(J)) CYCLE nodes
        END DO
     END IF

     ! SKIP RIVER INFLOW POINTS
     IF(NUMQBC > 0)THEN
        DO J=1,NUMQBC
           IF(RIVER_INFLOW_LOCATION == 'node')THEN
              IF(I == INODEQ(J)) CYCLE nodes
           END IF
           IF(RIVER_INFLOW_LOCATION == 'edge')THEN
              IF(I == N_ICELLQ(J,1) .OR. I == N_ICELLQ(J,2)) CYCLE nodes
           END IF
        END DO
     END IF

     ! SKIP GROUND WATER INFLOW POINTS
     IF(BFWDIS(I) .GT. 0.0_SP .and. GROUNDWATER_SALT_ON) CYCLE nodes

     K1 = 1
     IF(PRECIPITATION_ON) K1 = 2
!     DO K=1,KBM1
     DO K=K1,KBM1
        FMAX = MAXVAL(FABM_ALL(NBSN(I,1:NTSN(I)),K,IFABM))
        FMIN = MINVAL(FABM_ALL(NBSN(I,1:NTSN(I)),K,IFABM))

        IF(K == 1)THEN
           FMAX = MAX(FMAX,(FABM_ALL(I,K,IFABM)*DZ(I,K+1)+FABM_ALL(I,K+1,IFABM)*DZ(I,K))/  &
                (DZ(I,K)+DZ(I,K+1)))
           FMIN = MIN(FMIN,(FABM_ALL(I,K,IFABM)*DZ(I,K+1)+FABM_ALL(I,K+1,IFABM)*DZ(I,K))/  &
                (DZ(I,K)+DZ(I,K+1)))
        ELSE IF(K == KBM1)THEN
           FMAX = MAX(FMAX,(FABM_ALL(I,K,IFABM)*DZ(I,K-1)+FABM_ALL(I,K-1,IFABM)*DZ(I,K))/  &
                (DZ(I,K)+DZ(I,K-1)))
           FMIN = MIN(FMIN,(FABM_ALL(I,K,IFABM)*DZ(I,K-1)+FABM_ALL(I,K-1,IFABM)*DZ(I,K))/  &
                (DZ(I,K)+DZ(I,K-1)))
        ELSE
           FMAX = MAX(FMAX,(FABM_ALL(I,K,IFABM)*DZ(I,K-1)+FABM_ALL(I,K-1,IFABM)*DZ(I,K))/  &
                (DZ(I,K)+DZ(I,K-1)),                             &
                (FABM_ALL(I,K)*DZ(I,K+1,IFABM)+FABM_ALL(I,K+1,IFABM)*DZ(I,K))/           &
                (DZ(I,K)+DZ(I,K+1)))
           FMIN = MIN(FMIN,(FABM_ALL(I,K,IFABM)*DZ(I,K-1)+FABM_ALL(I,K-1,IFABM)*DZ(I,K))/  &
                (DZ(I,K)+DZ(I,K-1)),                             &
                (FABM_ALL(I,K,IFABM)*DZ(I,K+1)+FABM_ALL(I,K+1,IFABM)*DZ(I,K))/           &
                (DZ(I,K)+DZ(I,K+1)))
        END IF

        IF(FMIN-FABM_F(I,K,IFABM) > 0.0_SP)FABM_F(I,K,IFABM) = FMIN
        IF(FABM_F(I,K,IFABM)-FMAX > 0.0_SP)FABM_F(I,K,IFABM) = FMAX

     END DO
   END DO nodes
  END DO fabm

!  WHERE(SF1 < 0.0_SP)SF1=0.0_SP TRying to avoid brute force clipping...
  
  IF(DBG_SET(DBG_SBR)) WRITE(IPT,*)"End: fct_fabm"
  !#  endif
END SUBROUTINE FCT_FABM
!==============================================================================|

# endif

END MODULE MOD_FABM_3D
