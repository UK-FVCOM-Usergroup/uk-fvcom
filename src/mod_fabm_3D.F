MODULE MOD_FABM_3D
# if defined(FABM)

  USE fabm
  USE fabm_config
  USE fabm_types, only: domain_bottom, attribute_length, get_safe_name, type_universal_standard_variable
  USE fabm_driver, only: type_base_driver, driver

  USE MOD_FABM_DATA
  USE ALL_VARS
  USE MOD_UTILS
  USE MOD_WD
  USE MOD_SPHERICAL
  USE MOD_NORTHPOLE
! for accessing global node id in error reporting
  USE MOD_PAR

  ! For reading FABM forcing data from NetCDF:
  USE MOD_NCTOOLS

# if defined (SEDIMENT)
# if defined (ORIG_SED)
  USE MOD_SED,only:csed
# elif defined (CSTMS_SED)
  USE MOD_SED_CSTMS,only:csed
# endif
# endif

  IMPLICIT NONE

  PRIVATE

  PUBLIC FABM_FVCOM_INITIALIZE, FABM_ALLOCATE, FABM_INITIALIZE_STATE, FABM_INITIALIZE_DIAGNOSTICS, FABM_UPDATE, REPORT_FABM
  PUBLIC NAME_LIST_INITIALIZE_FABM, NAME_LIST_READ_FABM
  PUBLIC FABM_UPDATE_SALINITY, FABM_UPDATE_DEPTH

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
! DUMMY VARIABLE TO FIX VALUES IN SELECTED ERSEM VARIABLES
! RJT use sparingly 
!  REAL(SP), ALLOCATABLE,TARGET :: ERSEM1(:,:)
!  INTEGER :: ERSEMid
!RJT 
! For RK integration
  REAL(SP), ALLOCATABLE, TARGET     :: FABM_FB1(:,:,:)    !! temporary salinity in RK

# if defined (SEDIMENT)
  REAL(SP), ALLOCATABLE,TARGET :: SPM_FABM(:,:)   ! mg/m^3
  REAL(SP), ALLOCATABLE,TARGET :: SPM_FABM_g(:,:) ! g/m^3
! No longer needed... they are directly calculated in light_iop_ady_spm... 
!  REAL(SP), ALLOCATABLE,TARGET :: SPM_ABS_FABM(:,:)
!  REAL(SP), ALLOCATABLE,TARGET :: SPM_BB_FABM(:,:)
# endif
  REAL(SP), ALLOCATABLE        :: FABM_OLD_BOTTOM_THICKNESS(:)
  REAL(SP) :: DAYS_SINCE_YEAR_START
  LOGICAL :: COMPUTE_DAYS_SINCE_YEAR_START

  LOGICAL :: USE_FABM_SALINITY
  LOGICAL :: USE_FABM_BOTTOM_THICKNESS
  LOGICAL :: FABM_DEBUG
  LOGICAL :: PRINT_STATE

  NAMELIST /NML_FABM/                 &
         & STARTUP_FABM_TYPE,         &
#  if defined (OFFLINE_FABM)
         & OFFLINE_FABM_FILE,         &
#  endif
         & USE_FABM_SALINITY,         &
         & USE_FABM_BOTTOM_THICKNESS, &
         & FABM_DEBUG, &
         & FABM_DIAG_OUT

  TYPE (type_universal_standard_variable),PARAMETER :: mass_concentration_of_solute &
     = type_universal_standard_variable(name='mass_concentration_of_solute',units='kg/m**3', aggregate_variable=.true.)
  TYPE (type_universal_standard_variable),PARAMETER :: volume_fraction_of_particulates &
     = type_universal_standard_variable(name='volume_fraction_of_particulates',units='-', aggregate_variable=.true.)
  TYPE (type_universal_standard_variable),PARAMETER :: mass_concentration_of_silt &
     = type_universal_standard_variable(name='mass_concentration_of_silt', aggregate_variable=.true.)

  REAL(SP), POINTER :: FABM_SALT_DENSITY(:,:)
  REAL(SP), POINTER :: FABM_BOTTOM_THICKNESS(:)

  INTEGER, ALLOCATABLE, TARGET :: MASK_NONE(:), MASK_SAFE(:)
!  LOGICAL, ALLOCATABLE :: DISABLE_TRANSPORT(:)
!  INTEGER :: IFABMadv ! value of number of variables to advect
  type,extends(type_base_driver) :: type_fvcom_driver
  contains
     procedure :: fatal_error       => fvcom_driver_fatal_error
     procedure :: log_message       => fvcom_driver_log_message
     procedure :: describe_location => fvcom_driver_describe_location
  end type

  TYPE TYPE_FABM_INPUT
     TYPE (type_fabm_horizontal_variable_id) :: FABM_ID_HZ
     TYPE (type_fabm_interior_variable_id)   :: FABM_ID
     CHARACTER(LEN=160)                 :: PATH
     LOGICAL                            :: TWOD=.TRUE.
     REAL(SP), ALLOCATABLE              :: DATA_HZ(:)
     REAL(SP), ALLOCATABLE              :: DATA(:,:)
     TYPE(NCFILE),           POINTER    :: FILE => NULL()
     TYPE(NCVAR),            POINTER    :: N    => NULL()
     TYPE(NCVAR),            POINTER    :: P    => NULL()
     TYPE (TYPE_FABM_INPUT), POINTER    :: NEXT => NULL()
  END TYPE

  TYPE (TYPE_FABM_INPUT), POINTER, SAVE :: FIRST_FABM_INPUT => NULL()

   CONTAINS

   SUBROUTINE NAME_LIST_INITIALIZE_FABM

   !--Parameters in namelist NML_FABM
   STARTUP_FABM_TYPE = "'constant' or 'set values'"
# if defined (OFFLINE_FABM)
   OFFLINE_FABM_FILE = "DO NOT ADD UNTIL FVCOM IS RUNNING BY ITS SELF FIRST"
# endif
   USE_FABM_SALINITY = .FALSE.
   USE_FABM_BOTTOM_THICKNESS = .FALSE.
   FABM_DEBUG = .FALSE.
   FABM_DIAG_OUT = .TRUE.

   END SUBROUTINE NAME_LIST_INITIALIZE_FABM  

   SUBROUTINE NAME_LIST_READ_FABM
   USE CONTROL

   INTEGER :: IOS
   CHARACTER(LEN=120):: FNAME

   if(DBG_SET(dbg_sbr)) &
        & write(IPT,*) "Subroutine Begins: NAME_LIST_READ_FABM;"

    FNAME = "./"//trim(casename)//"_run.nml"

    if(DBG_SET(dbg_io)) &
         & write(IPT,*) "NAME_LIST_READ_FABM: File: ",trim(FNAME)

    CALL FOPEN(NMLUNIT,trim(FNAME),'cfr')

    ! Read FVCOM-FABM coupler settings.
    READ(UNIT=NMLUNIT, NML=NML_FABM,IOSTAT=IOS)
    IF (IOS/=0) THEN
       IF (DBG_SET(dbg_log)) WRITE(UNIT=IPT,NML=NML_FABM)
       IF(IOS<0) then
          ! End of file reached - that means the namelist was not present - warn and continue
          IF (DBG_SET(dbg_log)) WRITE (IPT,*) 'WARNING: namelist NML_FABM not present in '//trim(FNAME)//'. Using default settings for FVCOM-FABM coupler.'
       ELSE
          ! Another error (typically unknown variables or incorrect data types) - stop altogether.
          Call FATAL_ERROR('Cannot read nameList NML_FABM from file: ' //trim(FNAME))
       END IF
    END IF

    CLOSE(NMLUNIT)
   END SUBROUTINE NAME_LIST_READ_FABM

   SUBROUTINE FABM_FVCOM_INITIALIZE()
      ALLOCATE(type_fvcom_driver::driver)
      model => fabm_create_model()
      IF (USE_FABM_SALINITY) CALL model%require_data(mass_concentration_of_solute%in_interior())
      IF (USE_FABM_BOTTOM_THICKNESS) CALL model%require_data(volume_fraction_of_particulates%at_bottom())
   END SUBROUTINE FABM_FVCOM_INITIALIZE

   SUBROUTINE FABM_ALLOCATE()
    USE MOD_NESTING
    INTEGER :: IFABM,II
!RJT for fix ERSEM variable
   CHARACTER(LEN=256) :: NAME_
!    TYPE(type_fabm_interior_variable_id) :: ID
!RJT

    IF(DBG_SET(DBG_SBR)) write(ipt,*) "Start: FABM_ALLOCATE"
    ALLOCATE(MASK_NONE(M)); MASK_NONE = 1
!    ALLOCATE(MASK_SAFE(0:MT)); MASK_SAFE = 1  ! in case we need to restrict
!    ERSEM depths to deeper columns than set by ISWETN mask

    call CONFIGURE_OUTPUT

    ! Tell FABM about the extents of the spatial domain.
    call model%set_domain(M,KB, seconds_per_time_unit=DTI)
!    call model%set_domain(M,KB)
    call model%set_domain_stop(M,KBM1)
    IF(DBG_SET(DBG_SBR)) write(ipt,*) "In: FABM_ALLOCATE after set_domain"
# if defined(WET_DRY)
    ! Provide FABM with pointer to the real mask.
      call model%set_mask(ISWETN(1:M))
# else
      call model%set_mask(MASK_NONE)
# endif

    ! Send environmental data to FABM
    ! NB the idea is to send *any* fields that are already available in FVCOM, and of potential use to biogeochemical models.
    ! A predefined list of such variables is available at https://sourceforge.net/p/fabm/wiki/List_of_standard_variables/
    call model%link_interior_data(fabm_standard_variables%temperature,T1(1:M,:))
!    call model%link_interior_data(spm1_from_host_model,fvcom_model_name(1:M,:)) ! FOR NEW SPM variables defined in ERSEM/src/shared.F90


    call model%link_interior_data(fabm_standard_variables%practical_salinity,S1(1:M,:))
    call model%link_horizontal_data(fabm_standard_variables%surface_downwelling_shortwave_flux,SWRAD_WATTS(1:M))
    call model%link_horizontal_data(fabm_standard_variables%longitude,LON(1:M))
    call model%link_horizontal_data(fabm_standard_variables%latitude,LAT(1:M))
    call model%link_horizontal_data(fabm_standard_variables%bottom_depth,D(1:M))
    call model%link_horizontal_data(fabm_standard_variables%bottom_depth_below_geoid,H(1:M))

    ! Allocate arrays for derived environmental variables that will be computed from FVCOM fields.
# if defined (SEDIMENT)
! what if we compiled with sediment but are not running with sediment model active?
    IF(SEDIMENT_MODEL)THEN
!RJT June_2016
    ! Suspended sediment concentration from FVCOM - currently only one variable
    ! and for testing purposes it is constant everywhere. 
    ! The commented code would only work if SPM was part of the
    ! standard_variable list. We are taking a shortcut here by using ESS as the
    ! hook, which is not a standard variable
    IF (model%variable_needs_values(fabm_standard_variables%mass_concentration_of_suspended_matter)) THEN
       ALLOCATE(SPM_FABM_g(0:MT,KB))
       call model%link_interior_data(fabm_standard_variables%mass_concentration_of_suspended_matter,SPM_FABM_g(1:M,:)) 
    END IF

    IF (model%variable_needs_values(mass_concentration_of_silt%in_interior())) THEN
       ALLOCATE(SPM_FABM(0:MT,KB))
       call model%link_interior_data(mass_concentration_of_silt%in_interior(),SPM_FABM(1:M,:))
    END IF

!    IF (model%variable_needs_values(type_interior_standard_variable(name='absorption_of_silt'))) THEN
!       ALLOCATE(SPM_ABS_FABM(0:MT,KB))
!       call model%link_interior_data(type_interior_standard_variable(name='absorption_of_silt'),SPM_ABS_FABM(1:M,:))
!    END IF

!    IF (model%variable_needs_values(type_interior_standard_variable(name='backscatter_of_silt'))) THEN
!       ALLOCATE(SPM_BB_FABM(0:MT,KB))
!       call model%link_interior_data(type_interior_standard_variable(name='backscatter_of_silt'),SPM_BB_FABM(1:M,:))
!    END IF

! RJT June_2016
    ENDIF ! Check sediment model is active
# endif
    ! Cell thickness in m (from D and DZ)
    IF (model%variable_needs_values(fabm_standard_variables%cell_thickness)) THEN
       ALLOCATE(CELL_THICKNESS(0:MT,KB))
       call model%link_interior_data(fabm_standard_variables%cell_thickness,CELL_THICKNESS(1:M,:))
    END IF

    ! Density in kg m-3 (from RHO1)
    IF (model%variable_needs_values(fabm_standard_variables%density)) THEN
       ALLOCATE(DENSITY(0:MT,KB))
       call model%link_interior_data(fabm_standard_variables%density,DENSITY(1:M,:))
    END IF

    ! Cell center depth in m (from D and ZZ)
    IF (model%variable_needs_values(fabm_standard_variables%depth)) THEN
       ALLOCATE(DEPTH(0:MT,KB))
       call model%link_interior_data(fabm_standard_variables%depth,DEPTH(1:M,:))
    END IF

    ! Pressure in dbar (from gravity, depth and reference density)
    IF (model%variable_needs_values(fabm_standard_variables%pressure)) THEN
       ALLOCATE(PRESSURE(0:MT,KB))
       call model%link_interior_data(fabm_standard_variables%pressure,PRESSURE(1:M,:))
    END IF

    ! Wind speed in m s-1 (from UUWIND, VVWIND)
    IF (model%variable_needs_values(fabm_standard_variables%wind_speed)) THEN
       ALLOCATE(WIND_SPEED(0:MT))
       call model%link_horizontal_data(fabm_standard_variables%wind_speed,WIND_SPEED(1:M))
    END IF

    ! Bottom stress in Pa (from square of bottom shear velocity TAUBM_N)
    IF (model%variable_needs_values(fabm_standard_variables%bottom_stress)) THEN
       ALLOCATE(BOTTOM_STRESS(0:MT))
       call model%link_horizontal_data(fabm_standard_variables%bottom_stress,BOTTOM_STRESS(1:M))
    END IF

    COMPUTE_DAYS_SINCE_YEAR_START = model%variable_needs_values(fabm_standard_variables%number_of_days_since_start_of_the_year)
    IF (COMPUTE_DAYS_SINCE_YEAR_START) call model%link_scalar(fabm_standard_variables%number_of_days_since_start_of_the_year,DAYS_SINCE_YEAR_START)

    ALLOCATE(FABM_ALL(0:MT,KB,size(model%interior_state_variables)));         FABM_ALL    =  0.001_SP
    ALLOCATE(FABM_ALL_BT(0:MT,size(model%bottom_state_variables)));  FABM_ALL_BT =  0.001_SP
    ALLOCATE(FABM_ALL_SF(0:MT,size(model%surface_state_variables))); FABM_ALL_SF =  0.001_SP
    ALLOCATE(FABM_F(0:MT,KB,size(model%interior_state_variables)));           FABM_F       =  0.001_SP
!    ALLOCATE(FABM_MEAN(0:MT,KB,size(model%interior_state_variables)));        FABM_MEAN    =  0.001_SP
    ALLOCATE(FABM_TOTALS(size(model%conserved_quantities)));         FABM_TOTALS = 0
    ALLOCATE(DISABLE_TRANSPORT(size(model%interior_state_variables)))
    DO IFABM=1,SIZE(model%interior_state_variables)
       DISABLE_TRANSPORT(IFABM) = model%interior_state_variables(IFABM)%properties%get_logical('disable_transport',default=.false.)
    END DO
    IFABMadv = size(model%interior_state_variables) - COUNT(DISABLE_TRANSPORT) ! get the number of advecting variables 
! Build map of advectable variables in model%interior_state_variables    
    ALLOCATE(IFABMadv_idx(IFABMadv))
    if (NESTING_ON.AND.FABM_NESTING_ON)  ALLOCATE(IFABMnest_idx(IFABMadv))
    II = 1
    DO IFABM=1,SIZE(model%interior_state_variables)
       IF (.not. DISABLE_TRANSPORT(IFABM)) THEN
          IFABMadv_idx(II)=IFABM
          II=II+1
       END IF
    END DO
! link model variable to dummy ERSEM1 variable. 
!   NAME_ ='O2_o'
!      DO IFABM=1,SIZE(model%interior_state_variables)
!       write(ipt,*) "FABM state_variable ", TRIM(model%interior_state_variables(IFABM)%name)
!         IF (TRIM(model%interior_state_variables(IFABM)%name)==TRIM(NAME_)) then
!       ALLOCATE(ERSEM1(0:MT,KB))
!       ERSEM1 = 350.0_SP
!       ERSEMid = IFABM
!      write(ipt,*) "FABM ALLOCATE: FOUND VARIABLE ",TRIM(NAME_)," in the model configuration" 
 
!    ELSE 
!      write(ipt,*) "FABM ALLOCATE: unabled to find ",TRIM(NAME_)," in the model configuration" 
!    END IF
!   ENDDO


    ! Provide FABM with pointers to state variable data.
    DO IFABM=1,size(model%interior_state_variables)
      CALL model%link_interior_state_data(IFABM,FABM_ALL(1:M,:,IFABM))
    END DO
    DO IFABM=1,size(model%bottom_state_variables)
      CALL model%link_bottom_state_data(IFABM,FABM_ALL_BT(1:M,IFABM))
    END DO
    DO IFABM=1,size(model%surface_state_variables)
      CALL model%link_surface_state_data(IFABM,FABM_ALL_SF(1:M,IFABM))
    END DO

    CALL INITIALIZE_FABM_INPUTS()

    ! Make sure FABM has all required data
    CALL model%start()

    IF (USE_FABM_SALINITY) THEN
       FABM_SALT_DENSITY => model%get_data(model%get_interior_variable_id(mass_concentration_of_solute%in_interior()))
       IF (.NOT.ASSOCIATED(FABM_SALT_DENSITY)) CALL FATAL_ERROR("BUG: FABM failed to created mass_concentration_of_solute variable.")
    END IF
    IF (USE_FABM_BOTTOM_THICKNESS) THEN
       FABM_BOTTOM_THICKNESS => model%get_data(model%get_horizontal_variable_id(volume_fraction_of_particulates%at_bottom()))
       IF (.NOT.ASSOCIATED(FABM_BOTTOM_THICKNESS)) CALL FATAL_ERROR("BUG: FABM failed to created volume_fraction_of_particulates variable.")
       ALLOCATE(FABM_OLD_BOTTOM_THICKNESS(1:M))
       FABM_OLD_BOTTOM_THICKNESS = 0
    END IF

    ALLOCATE(XFLUX_OBCB(0:MT,KB,size(model%interior_state_variables))) ; XFLUX_OBCB  =  0.0_SP
    ALLOCATE(PFABMPXB(MT,size(model%interior_state_variables)))        ; PFABMPXB    = ZERO
    ALLOCATE(PFABMPYB(MT,size(model%interior_state_variables)))        ; PFABMPYB    = ZERO
    IF(DBG_SET(DBG_SBR)) write(ipt,*) "End: FABM_ALLOCATE"
   END SUBROUTINE

   SUBROUTINE PREPARE_ENVIRONMENT()
      INTEGER :: K
      REAL(SP), DIMENSION(0:MT) :: UUWIND_N,VVWIND_N
      TYPE(TIME) :: Time_yearstart
      CHARACTER(LEN=120) :: string_local
      integer :: status

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
# if defined (SEDIMENT)
      IF(SEDIMENT_MODEL)THEN
!RJT June_2016
! One of these two will be true (either using light or light_iop)
! Light.F90 in ERSEM uses silt concentration
      ! Aggregate FVCOM SPM variables into a single one for passing on to ERSEM
      IF (ALLOCATED(SPM_FABM)) THEN
         ! Pass on SPM concentrations in mg/m3. FVCOM units are in Kg/m3 or g/L!! needs
         ! to be scaled!
         ! FVCOM variable would be sed(nsed)%conc...
            SPM_FABM = CSED*1000000.0_SP ! This should be changed to the appropriate SPM variables from FVCOM (either only the silt fraction or sum of smaller size fractions)
      END IF
      IF (ALLOCATED(SPM_FABM_g)) THEN
         ! Pass on SPM concentrations in g/m3. FVCOM units are in Kg/m3!! needs
         ! to be scaled!
         ! FVCOM variable would be sed(nsed)%conc...
            SPM_FABM_g = CSED*1000.0_SP ! This should be changed to the appropriate SPM variables from FVCOM (either only the silt fraction or sum of smaller size fractions)
      END IF
      ! Light_iop_ady_spm.F90 only needs specific absorption and backscattering so we can skip
      ! aggregation here. 
      !NO LONGER APPLIES...Specific absorption and backscattering are hard coded
      ! here. Should be in namelist... ideally in FABM.yaml but this requires
      ! addiging the variables to ERSEM code (share.F90, CMakelist, ERSEM model
      ! library) as they should be new variables and a new light_iop_spm.F90
      ! file. 
!      IF (ALLOCATED(SPM_ABS_FABM)) THEN
         ! Pass on SPM absorptions in 1/m FVCOM. Make it depth dependent here
         ! for testing purposes.
!        DO K=1,KBM1
!         SPM_ABS_FABM(:,K)  =   ( 3* (( (D*ZZ(:,K)-MIN_DEPTH ) / (15.0_SP-MIN_DEPTH))**2 ))  &
!                    - ( 2*(( (D*ZZ(:,K)-MIN_DEPTH)/(15.0_SP-MIN_DEPTH))**3)   )
!         WHERE (D*ZZ(:,K) .le. MIN_DEPTH)    SPM_ABS_FABM(:,K) = 0.0_SP
!         WHERE (D*ZZ(:,K) .ge. 15.0_SP)      SPM_ABS_FABM(:,K) = 1.0_SP

!        ENDDO
      ! using hard coded absorption coefficient. If using SPM concentrations,
      ! this will need to be the specific absorption coefficient x SPM
      ! concentraions. 
        !SPM_ABS_FABM = SPM_ABS_FABM*0.7_SP
      ! absorption relationship from Babin et al 2003 with abs* = 0.031m2/g , abs* = 0.031/(10.0_SP**3.0_SP) m2/mg

!        SPM_ABS_FABM = (0.031_SP/(10.0_SP**3.0_SP))*CSED*10.0_SP**6.0_SP ! This should be changed to the appropriate SPM variables from FVCOM (either only the silt fraction or sum of smaller size fractions). I think they have an specific order in CSED... so shouldn't be a problem. 
!      END IF

!      IF (ALLOCATED(SPM_BB_FABM)) THEN
         ! Pass on SPM backscatter coefficient in 1/m FVCOM. 
!        DO K=1,KBM1
!         SPM_BB_FABM(:,K)  =   ( 3* (( (D*ZZ(:,K)-MIN_DEPTH ) / (15.0_SP-MIN_DEPTH))**2 ))  &
!                    - ( 2*(( (D*ZZ(:,K)-MIN_DEPTH)/(15.0_SP-MIN_DEPTH))**3)   )
!         WHERE (D*ZZ(:,K) .le. MIN_DEPTH)    SPM_BB_FABM(:,K) = 0.0_SP
!         WHERE (D*ZZ(:,K) .ge. 15.0_SP)      SPM_BB_FABM(:,K) = 1.0_SP
!        ENDDO
      ! using hard coded backscattering coefficient. If using SPM
      ! concentrations, this will need to be the specific backscattering
      ! coefficient x SPM concentraions
        !SPM_BB_FABM = SPM_BB_FABM*0.1_SP
      ! backscattering relationship from Martinez-Vicente et al 2010 with bbp* = 0.0055m2/g , abs* = 0.0055/(10.0_SP**3.0_SP) m2/mg

!        SPM_BB_FABM = (0.0055_SP/(10.0_SP**3.0_SP))*CSED*10.0_SP**6.0_SP ! This should be changed to the appropriate SPM variables from FVCOM (either only the silt fraction or sum of smaller size fractions)
!      END IF


! RJT June_2016
      ENDIF ! Sediment model active
# endif

      IF (COMPUTE_DAYS_SINCE_YEAR_START) THEN
         string_local = WRITE_DATETIME(IntTime,3,'UTC')
         Time_yearstart = READ_DATETIME(string_local(1:4)//'-01-01T00:00:00.000Z','ymd','UTC',status)
         DAYS_SINCE_YEAR_START = DAYS(IntTime-Time_yearstart)
      END IF

      CALL UPDATE_FABM_INPUTS(IntTime)
   END SUBROUTINE

   SUBROUTINE FABM_INITIALIZE_STATE()
      INTEGER :: K
      IF(DBG_SET(DBG_SBR)) write(ipt,*) "Start: FABM_INITIALIZE_STATE" 
      IF (.NOT.ALLOCATED(FABM_ALL)) CALL FATAL_ERROR("FABM_INITIALIZE_STATE called before FABM_ALLOCATE")

      CALL PREPARE_ENVIRONMENT

      ! Provide FABM with pointer to the dummy mask ("nothing masked") to include dry points in initialization.
      call model%set_mask(MASK_NONE)

      ! Allow FABM to perform its default initialization.
      CALL model%initialize_surface_state(1,M)
      CALL model%initialize_bottom_state (1,M)
      DO K=1,KBM1
        CALL model%initialize_interior_state(1,M,K)
      END DO

# if defined(WET_DRY)
    ! Provide FABM with pointer to the real mask.
      call model%set_mask(ISWETN(1:M))
# else
      call model%set_mask(MASK_NONE)
# endif
      IF(DBG_SET(DBG_SBR)) write(ipt,*) "End: FABM_INITIALIZE_STATE" 

   END SUBROUTINE FABM_INITIALIZE_STATE

   SUBROUTINE FABM_INITIALIZE_DIAGNOSTICS()

      IF(DBG_SET(DBG_SBR)) write(ipt,*) "Start: FABM_INITIALIZE_DIAGNOSTICS" 

      ! Provide FABM with pointer to the dummy mask ("nothing masked") to ensure even diagnostics on dry points have a value.
      call model%set_mask(MASK_NONE)

      ! Initialize all diagnostics (but skip time integration)
      CALL FABM_SOURCES(DTI,UPDATE=.FALSE.)


# if defined(WET_DRY)
    ! Provide FABM with pointer to the real mask.
      call model%set_mask(ISWETN(1:M))
# else
      call model%set_mask(MASK_NONE)
# endif

      ! Compute initial value of conserved quantity integrals
      CALL CHECK_CONSERVATION
      IF(DBG_SET(DBG_SBR)) write(ipt,*) "End: FABM_INITIALIZE_STATE" 

   END SUBROUTINE FABM_INITIALIZE_DIAGNOSTICS

SUBROUTINE FABM_UPDATE
     USE MOD_PAR
     USE MOD_NESTING
# if defined (SEMI_IMPLICIT)
     USE MOD_SEMI_IMPLICIT
# endif
     INTEGER :: IFABM
     LOGICAL :: VALID
     REAL(SP) :: RCOFT  ! for RK integration
     INTEGER :: ITER ! for RK integration
     IF(DBG_SET(DBG_SBR)) write(ipt,*) "Start: FABM_UPDATE" 

     ! Integrate biogeochemical sources-sinks
     CALL FABM_SOURCES(DTI,UPDATE=.TRUE.)  ! This operates on FABM_ALL
     IF (FABM_DEBUG) THEN
       PRINT_STATE = .TRUE.
       IF (.NOT.CHECK_STATE(.TRUE.)) THEN
          WRITE (IPT,'(A)') 'WARNING: state was clipped to correct out-of-bounds values after FABM_SOURCES'
       END IF
       PRINT_STATE = .FALSE.
     END IF

     ! Vertical movement (e.g., sinking or floating)
!     CALL CALC_VERTICAL_MOVEMENT(DTI)
     CALL FABM_VERTICAL_MOVEMENT(DTI)
     IF (FABM_DEBUG) THEN
       IF (.NOT.CHECK_STATE(.TRUE.)) WRITE (IPT,'(A)') 'WARNING: state was clipped to correct out-of-bounds values after FABM_VERTICAL_MOVEMENT'
     END IF

# if !defined (ONE_D_MODEL)

#     if defined (MULTIPROCESSOR)
! RJT just to check this is needed...
     IF(PAR)CALL FABM_EXCHANGE(FABM_ALL)
#     endif
! RJT 2023

#     if !defined (SEMI_IMPLICIT)
      IF(.NOT. RK_3D_ON)THEN
#     endif
!-----------syshi 20191013--------------
#   if defined(SEMI_IMPLICIT)
         TSC_FABM_ALL=FABM_ALL
         DO STAGE = 1,KSTAGE_FABM
#   endif
!-----------syshi 20191013--------------

     ! Advection and horizontal diffusion
            CALL FABM_ADV  ! This applies advection and horizontal diffusion operators to FABM_ALL to compute forecast field FABM_F
            IF (FABM_DEBUG) THEN
               PRINT_STATE = .TRUE.
               IF (.NOT.CHECK_STATE(.TRUE.)) WRITE (IPT,'(A)') 'WARNING: state was clipped to correct out-of-bounds values after FABM_ADV'
               PRINT_STATE = .FALSE.
            END IF
            ! Redirect FABM to forecast pelagic state FABM_F to ensure that any further FABM calls (check_state, check_conservation)
            ! operate on the right data.
            DO IFABM=1,size(model%interior_state_variables)
               CALL model%link_interior_state_data(IFABM,FABM_F(1:M,:,IFABM))
            END DO

#   if defined (MULTIPROCESSOR)
            DO IFABM=1,size(model%interior_state_variables)
               IF(PAR)CALL NODE_MATCH(1,NBN,BN_MLT,BN_LOC,BNC,MT,KB,MYID,NPROCS,FABM_F(:,:,IFABM))
            END DO
#   endif

!-----------syshi 20191013--------------
#   if defined(SEMI_IMPLICIT)
            FABM_ALL=TSC_FABM_ALL
#   endif
!-----------syshi 20191013--------------
            IF(SCALAR_POSITIVITY_CONTROL) CALL FCT_FABM       !Conservation Correction of FABM_F  !
            ! Remove the impact of FABM_ADV on the open boundary nodes that experience inflow.
            ! For those, we want to run water column simulations that only include sources, residual vertical movement and vertical mixing.
            ! We are not ignoring non-advective variables here but we do in BCOND RJT
            CALL FABM_BCOND_PREPARE
# endif
! end if defined 1D

     ! Vertical mixing
# if !defined (ONE_D_MODEL)
! We need to remove non advecting/mixing variables from FABM_ALL for this case RJT
            CALL FABM_MIXING(FABM_F)   ! This operates on the supplied argument
# else
! We need to remove non advecting/miixing variables from FABM_ALL for this case RJT
            CALL FABM_MIXING(FABM_ALL) ! This operates on the supplied argument
# endif
            IF (FABM_DEBUG) THEN
               PRINT_STATE = .TRUE.
               IF (.NOT.CHECK_STATE(.TRUE.)) WRITE (IPT,'(A)') 'WARNING: state was clipped to correct out-of-bounds values after FABM_MIXING'
               PRINT_STATE = .FALSE.
            END IF

! FABM_exchange needs to remain before call to BCOND
# if defined (MULTIPROCESSOR)
            IF(PAR)CALL FABM_EXCHANGE(FABM_F)
# endif
     ! Process open boundaries (and depending on RIVER_TS_SETTING, also rivers)
            CALL FABM_BCOND  ! This operates on FABM_F
#   if defined(SEMI_IMPLICIT)
         ENDDO
#   endif
! RK FABM code 
# if !defined (SEMI_IMPLICIT)
      ELSE
         ALLOCATE(FABM_FB1(0:MT,KB,size(model%interior_state_variables))); FABM_FB1 = 0.0_SP
         FABM_FB1 = FABM_F
         RCOFT = 0.5_SP
         DO ITER = 1,2
            CALL FABM_ADV_RK(FABM_FB1)      !Advection                 !
#   if defined(MULTIPROCESSOR)
            DO IFABM=1,size(model%interior_state_variables)
               IF(PAR)CALL NODE_MATCH(1,NBN,BN_MLT,BN_LOC,BNC,MT,KB,MYID,NPROCS,FABM_F(:,:,IFABM))
            END DO
#    endif
 
      !#                                                   if !defined !(DOUBLE_PRECISION)
         IF(SCALAR_POSITIVITY_CONTROL) CALL FCT_FABM            !Conservation Correction   !
      !#                                                   endif
         CALL FABM_MIXING(FABM_F)   ! This operates on the supplied argument
      ! FABM_exchange needs to remain before call to BCOND
# if defined (MULTIPROCESSOR)
         IF(PAR)CALL FABM_EXCHANGE(FABM_F)
# endif
! Process open boundaries (and depending on RIVER_TS_SETTING, also rivers)
         CALL FABM_BCOND  ! This operates on FABM_F
! this needs looking at because FABM_ALL is not updated until later
      FABM_ALL = FABM_F
      IF(ITER /= 2)THEN
         FABM_ALL = RCOFT*FABM_ALL+(1.0_SP-RCOFT)*FABM_FB1
      END IF
     END DO
     DEALLOCATE(FABM_FB1)
   END IF
# endif
 
     ! Nesting procedure should go after BCOND treatments shouldn't it?  At least that is how it es elsewhere
     IF(NESTING_ON.AND.FABM_NESTING_ON) THEN
            CALL SET_VAR(intTime,FABM_ALL=FABM_F)
     END IF
! !-----------syshi 20191013--------------
! #   if defined(SEMI_IMPLICIT)
!       ENDDO
! #   endif
!---------------------THINK ABOUT THE CODE BELOW----------------------
!-----------syshi 20191013--------------
# if defined (MULTIPROCESSOR)
     IF(PAR)CALL FABM_EXCHANGE(FABM_F)
# endif
! end of moved block for NESTING... I don't know if the FABM_exchange should have needed to remain before call to BCOND or if we need it here?
     ! Check validity of state variables and "correct" (typicalyl: clip) where needed.
     ! This may be needed because some of the logic in FABM_BCOND is not monotonicity-preserving.
     IF (FABM_DEBUG) THEN
       IF (.NOT.CHECK_STATE(.TRUE.)) WRITE (IPT,'(A)') 'WARNING: state was clipped to correct out-of-bounds values after FABM_BCOND'
!       WHERE (ISNAN(FABM_F)) FABM_F = ZERO
       IF (ANY(ISNAN(FABM_F))) CALL FATAL_ERROR("NaNs found in FABM state at the end of FABM_UPDATE.")
     ELSE
       VALID = CHECK_STATE(.TRUE.)
       IF (DBG_SET(DBG_VRB).AND..NOT.VALID) WRITE (IPT,'(A)') 'WARNING: state was clipped to correct out-of-bounds values at the end of FABM_UPDATE'
     END IF

# if !defined (ONE_D_MODEL)
  DO IFABM=1,size(model%interior_state_variables)
      IF (.not. DISABLE_TRANSPORT(IFABM) ) then    
        FABM_ALL(:,:,IFABM) = FABM_F(:,:,IFABM)      ! Update to new time level skipping non-advective variables
      END IF
  END DO
# endif
! RESTORE OXYGEN VALUES TO OUR CONSTANT VALUE RJT
!     FABM_ALL(:,:,ERSEMid) = ERSEM1

     ! Redirect FABM to new pelagic state FABM_ALL to ensure that any future FABM calls operate on the right data.
     DO IFABM=1,size(model%interior_state_variables)
       CALL model%link_interior_state_data(IFABM,FABM_ALL(1:M,:,IFABM))
     END DO

# if defined (MULTIPROCESSOR)
     IF(PAR)CALL FABM_EXCHANGE(FABM_ALL)
# endif
!---------------------THINK ABOUT THE CODE ABOVE - compared to internal_step lines 1680----------------------

END SUBROUTINE FABM_UPDATE

SUBROUTINE REPORT_FABM
   INTEGER :: IFABM

   ! Compute conserved quantity integrals
   CALL CHECK_CONSERVATION

   IF(MSR) THEN
      DO IFABM=1,SIZE(model%conserved_quantities)
         WRITE (IPT,'(1X,A,E15.7,1X,A)') '!  INTEGRATED '//TRIM(model%conserved_quantities(IFABM)%name)//' :',FABM_TOTALS(IFABM),TRIM(model%conserved_quantities(IFABM)%units)//'*m3'
      END DO
   END IF
END SUBROUTINE REPORT_FABM

SUBROUTINE FABM_UPDATE_SALINITY
   INTEGER :: I

      IF(DBG_SET(DBG_SBR)) write(ipt,*) "Start: FABM_UPDATE_SALINITY" 

   ! Note that FABM salinity is expressed in g/L, NOT in g/kg as absolute salinity (TEOS 2010), which would come closer to PSU.
   ! Hence, we here divide by density in kg/L to get g/kg.
   ! [NB density residual rho will be the density computed previously for the old salinity value]
   IF (.NOT.USE_FABM_SALINITY) RETURN

   DO I = 1,M

#  if defined (WET_DRY)
     ! Only update salinity of wet nodes
     IF (ISWETN(I)==1) THEN
#  endif

       S1(I,1:KB) = FABM_SALT_DENSITY(I,1:KB)/(1+RHO1(I,1:KB))

#  if defined (WET_DRY)
     END IF
#  endif

   END DO

   ! Use updated salinity also on elements
   CALL N2E3D(S1,S)
      IF(DBG_SET(DBG_SBR)) write(ipt,*) "End: FABM_UPDATE_SALINITY" 

END SUBROUTINE FABM_UPDATE_SALINITY

SUBROUTINE FABM_UPDATE_DEPTH
   ! Logic in this routine was taken from UPDATE_DEPTH in mod_sed.F
   USE MOD_OBCS
   USE MOD_PAR

   INTEGER  :: I,I1,J1,J2
   LOGICAL  :: ISONB2

   IF (.NOT.USE_FABM_BOTTOM_THICKNESS) RETURN
   
   IF(DBG_SET(DBG_SBR)) write(ipt,*) "START: FABM_UPDATE_DEPTH"

   DO I = 1,M

#  if defined (WET_DRY)
     ! Only update depth of wet nodes
     IF (ISWETN(I)==1) THEN
#  endif

!
!   DEPTHS ON OPEN BOUNDARY NODES AND THE NODES NEXT TO THOSE NODES ARE KEPT INVARIANT
!
     ISONB2=.FALSE.
     DO I1 = 1,IOBCN
       J1 = I_OBC_N(I1)
       J2 = NEXT_OBC(I1)
       IF(I==J1 .OR. I==J2) ISONB2=.TRUE.
     END DO
     IF(.NOT. ISONB2) THEN
        H(I) = H(I) - (FABM_BOTTOM_THICKNESS(I)-FABM_OLD_BOTTOM_THICKNESS(I))
        FABM_OLD_BOTTOM_THICKNESS(I) = FABM_BOTTOM_THICKNESS(I)
     END IF

#  if defined (WET_DRY)
     END IF
#  endif

   END DO
! Extend H to halo nodes
# if defined (MULTIPROCESSOR)
   IF(PAR)CALL AEXCHANGE(NC,MYID,NPROCS,H )
# endif

   CALL N2E2D(H,H1)

   D   = H + EL
   DT  = H + ET
   D1  = H1+EL1
   DT1 = H1+ET1

   IF(DBG_SET(DBG_SBR)) write(ipt,*) "END: FABM_UPDATE_DEPTH"

END SUBROUTINE FABM_UPDATE_DEPTH

FUNCTION CHECK_STATE(REPAIR_STATE) RESULT(VALID_STATE)
      LOGICAL, INTENT(IN) :: REPAIR_STATE
      LOGICAL :: VALID_STATE
      INTEGER :: K

      ! Provide FABM with pointer to the dummy mask ("nothing masked") to include dry regions when checking/overriding state.
      call model%set_mask(MASK_NONE)

      ! Check state variable values. On return, VALID_STATE will be .TRUE. if the state has been found valid, .FALSE. if not.
      ! If REPAIR_STATE is .TRUE., this routine will automatically clip invalid values to the nearest valid value.
      ! In that case, the state will be valid after return even if VALID_STATE = .FALSE. [which is then indicative of
      ! clipping having occured)
      CALL model%check_surface_state(1,M,REPAIR_STATE,VALID_STATE)
      if (.NOT.(VALID_STATE.OR.REPAIR_STATE)) RETURN
      CALL model%check_bottom_state(1,M,REPAIR_STATE,VALID_STATE)
      if (.NOT.(VALID_STATE.OR.REPAIR_STATE)) RETURN
      DO K=1,KBM1
         CALL model%check_interior_state(1,M,K,REPAIR_STATE,VALID_STATE)
         if (.NOT.(VALID_STATE.OR.REPAIR_STATE)) RETURN
      END DO


# if defined(WET_DRY)
    ! Provide FABM with pointer to the real mask.
      call model%set_mask(ISWETN(1:M))
# else
      call model%set_mask(MASK_NONE)
# endif

END FUNCTION CHECK_STATE

SUBROUTINE CHECK_CONSERVATION()
     USE MOD_PAR

     INTEGER  :: I, IBND, K, IFABM, IERR
     REAL(SP), DIMENSION(M,SIZE(model%conserved_quantities)) :: LOCAL
     REAL(DP), DIMENSION(SIZE(model%conserved_quantities))   :: TOTALS_LOC

     REAL(DP), ALLOCATABLE :: WEIGHTS(:)

     ! Provide FABM with pointer to the dummy mask ("nothing masked") to include dry regions when computing conserved quantity totals.
     call model%set_mask(MASK_NONE)

     ALLOCATE(WEIGHTS(1:M))
     WEIGHTS = ART1(1:M)
#   if defined (MULTIPROCESSOR) 
     IF(PAR) THEN
        DO I=1,M
           IF(NDE_ID(I) == 1) THEN   !! INTERNAL BOUNDARY NODE (SHARED BETWEEN SUBDOMAINS)
              DO IBND=1,NBN
                IF(BN_LOC(IBND) == I) WEIGHTS(I) = WEIGHTS(I)/BN_MLT(IBND)
              END DO
           END IF
        END DO
     END IF
#   endif

     TOTALS_LOC = 0
     DO K=1,KBM1
        CALL model%get_interior_conserved_quantities(1,M,K,LOCAL)
        DO IFABM=1,SIZE(model%conserved_quantities)
           TOTALS_LOC(IFABM) = TOTALS_LOC(IFABM) + SUM(DBLE(LOCAL(1:M,IFABM)*DZ(1:M,K)*D(1:M))*WEIGHTS)
        END DO
     END DO
     CALL model%get_horizontal_conserved_quantities(1,M,LOCAL)
     DO IFABM=1,SIZE(model%conserved_quantities)
        TOTALS_LOC(IFABM) = TOTALS_LOC(IFABM) + SUM(LOCAL(1:M,IFABM)*WEIGHTS)
     END DO

# if defined(WET_DRY)
    ! Provide FABM with pointer to the real mask.
      call model%set_mask(ISWETN(1:M))
# else
      call model%set_mask(MASK_NONE)
# endif


# if defined (MULTIPROCESSOR) 
     IF(PAR) THEN
        ! NB: similar to logic in mod_report
        CALL MPI_REDUCE(TOTALS_LOC,FABM_TOTALS,SIZE(TOTALS_LOC),MPI_DP,MPI_SUM,MSRID-1,MPI_FVCOM_GROUP,IERR)
     ELSE
        FABM_TOTALS = TOTALS_LOC
     END IF
# else
     FABM_TOTALS = TOTALS_LOC
# endif

     DEALLOCATE(WEIGHTS)
END SUBROUTINE CHECK_CONSERVATION

SUBROUTINE FABM_SOURCES(DELTA_T,UPDATE)
     REAL(SP), INTENT(IN) :: DELTA_T
     LOGICAL,  INTENT(IN) :: UPDATE
     INTEGER  :: I, K, IFABM
     REAL(SP), DIMENSION(M,size(model%interior_state_variables))         :: SMS_PEL,FLUX_SF,FLUX_BT
     REAL(SP), DIMENSION(M,size(model%bottom_state_variables))  :: SMS_BT
     REAL(SP), DIMENSION(M,size(model%surface_state_variables)) :: SMS_SF
     REAL(SP), DIMENSION(M)                                     :: DUMMY

  IF(DBG_SET(DBG_SBR)) WRITE(IPT,*)"Start: FABM_SOURCES :"

     CALL PREPARE_ENVIRONMENT
# if defined (WET_DRY)
! make the mask more restricted RJT
!      WHERE(D < 1.0_SP) MASK_SAFE=0
!     call model%set_mask(MASK_SAFE(1:M))
     call model%set_mask(ISWETN(1:M))
!     MASK_SAFE = 1 ! restore to no mask
# endif
     ! Ensure that light attenuation, absorption, scattering are up to date in FABM.
     CALL model%prepare_inputs(REAL(IINT,SP) )
!     CALL model%prepare_inputs()

     FLUX_SF = 0.
     FLUX_BT = 0.
     SMS_SF = 0.
     SMS_BT = 0.

     ! Get sources-sinks for interface-attached state variables, and cross-interface
     ! fluxes (air-sea gas exchange, sedimentation, pelagic-benthic diffusion flux)
     call model%get_surface_sources(1,M,FLUX_SF,SMS_SF)
     call model%get_bottom_sources (1,M,FLUX_BT,SMS_BT)

     ! Integrate sources-sinks for the pelagic
     DO K=1,KBM1
        ! Retrieve sources-sinks for current layer K.
        SMS_PEL = 0
        call model%get_interior_sources(1,M,K,SMS_PEL)

        IF (K==1) THEN
           ! Top-most layer: include surface flux of pelagic state variables.
           DO IFABM=1,size(model%interior_state_variables)
              SMS_PEL(1:M,IFABM) = SMS_PEL(1:M,IFABM) + FLUX_SF(1:M,IFABM)/(DZ(1:M,K)*D(1:M))
           END DO
        END IF
        IF (K==KBM1) THEN
           ! Bottom-most layer: include bottom flux of pelagic state variables.
           DO IFABM=1,size(model%interior_state_variables)
              SMS_PEL(1:M,IFABM) = SMS_PEL(1:M,IFABM) + FLUX_BT(1:M,IFABM)/(DZ(1:M,KBM1)*D(1:M))
           END DO
        END IF

        ! Forward Euler time integration of pelagic state.
        IF (UPDATE) FABM_ALL(1:M,K,:) = FABM_ALL(1:M,K,:) + DELTA_T*SMS_PEL
     END DO

     ! Forward Euler time integration of interface-attached state variables.
     IF (UPDATE) THEN
        FABM_ALL_SF(1:M,:) = FABM_ALL_SF(1:M,:) + DELTA_T*SMS_SF
        FABM_ALL_BT(1:M,:) = FABM_ALL_BT(1:M,:) + DELTA_T*SMS_BT
     END IF

     CALL model%finalize_outputs()

END SUBROUTINE FABM_SOURCES

!==========================================================================
! Calculate Depositional Flux and Update Concentration from Settling
!==========================================================================
  Subroutine CALC_VERTICAL_MOVEMENT(DELTA_T)
     REAL(SP), INTENT(IN) :: DELTA_T
     INTEGER :: I,K,IFABM
     INTEGER  :: mcyc,ncyc
     REAL(SP), DIMENSION(M,KBM1,size(model%interior_state_variables)) :: w_ct
     REAL(SP), DIMENSION(KBM1) :: C,DX,W
     REAL(SP) :: eps,DTmax,DTdep
     REAL(SP),PARAMETER ::SETTLE_CFL = 1.0_SP
  IF(DBG_SET(DBG_SBR)) WRITE(IPT,*)"Start: CALC_VERTICAL_MOVEMENT :"

     ! Get vertical velocities at layer centers
     DO K=1,KBM1
        CALL model%get_vertical_movement(1,M,K,w_ct(:,K,:))
     END DO

  eps = epsilon(eps)
  !Loop over Horizontal Domain       
     DO I=1,M
#  if defined (WET_DRY)
      ! no calculation when dry nodes
       IF(ISWETN(I) == 1) THEN
#  endif
  !Loop over FABM variables       
         DO IFABM=1,SIZE(model%interior_state_variables)

          !setup 1D concentration and grid arrays
          C(1:kbm1) = FABM_ALL(I,1:KBM1,IFABM)
          DX(1:kbm1) = DZ(I,1:KBM1)*D(I)
          W(1:kbm1) = w_ct(I,1:KBM1,IFABM)
      

      !set up cycles (use max(CFL) == 1)
      DTmax = Settle_CFL*minval(ABS(DX(1:KBM1)/W(1:KBM1)))
      mcyc = int(DELTA_T/DTmax + 1. - eps)
      DTdep = DELTA_T/float(mcyc)
!         write (*,*) 'In FABM vertical with w_ct ',w_ct(I,1:KBM1,IFABM)
!         write (*,*) 'In FABM vertical with Mcyc and DTdep ',mcyc,DTdep
      !call flux-limited settling equation
      do ncyc = 1,mcyc
          call SETTLE_FLUX_FABM(KBM1,C,DX,W,DTdep)
      end do

      !store solution in 3D array
       FABM_ALL(I,1:KBM1,IFABM) = C(1:KBM1)

         END DO
#  if defined (WET_DRY)
      ! no calculation when dry nodes
       END IF
#  endif

     END DO
  IF(DBG_SET(DBG_SBR)) WRITE(IPT,*)"End: CALC_VERTICAL_MOVEMENT :"
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
	  real(sp) :: dis4,wvel,sl_u,sl_f
          real(sp) :: sl_h(0:n+1)
  integer  :: i

  IF(DBG_SET(DBG_SBR)) WRITE(IPT,*)"Start: SETTLE_FLUX_FABM :"

  !transfer to working array
  cin(1:n) = c(1:n)
  sl_h(1:n) = dx(1:n)

  !surface bcs (no flux - to achieve that, make sure mean of cin(1) and cin(0) is zero)
  cin(0)  =  -cin(1) 
  sl_h(0) = dx(1)

  !bottom bcs (no flux - to achieve that, make sure mean of cin(n) and cin(n+1) is zero)
  cin(n+1) = -cin(n) 
  sl_h(n+1) = dx(n)

  !flux computation
  do i=2,n
    wvel    = .5*(wset(i)+wset(i-1))  !settle velocity at interface
    dis4    = wvel/2.
    sl_u = 2.0_SP*(cin(i)-cin(i+1))/(sl_h(i)+sl_h(i+1))
    sl_f = 2.0_SP*(cin(i-2)-cin(i-1))/(sl_h(i-2)+sl_h(i-1))
    conv(i) = wvel*(cin(i)+cin(i-1))/2. 
!    diss(i) = dis4*(cin(i-1)-cin(i)-0.5_SP*LIMLED2(sl_u,sl_f,2.0_SP)*(sl_h(i-1)+sl_h(i))) 
    diss(i) = dis4*(cin(i)-cin(i-1)-lim(cin(i+1)-cin(i),cin(i-1)-cin(i-2))) 
  end do

  !zero out surface flux
  conv(1) = 0.0_SP ; diss(1) = 0.0_SP
  ! zero out bottom flux
  conv(n+1) = 0.0_SP
  diss(n+1) = 0.0_SP

  !update
  do i=1,n
    c(i) = cin(i) + (deltat/sl_h(i))*(-conv(i+1)+conv(i) + diss(i+1)-diss(i)) 
  end do

  IF(DBG_SET(DBG_SBR)) WRITE(IPT,*)"End: SETTLE_FLUX_FABM :"

 End Subroutine SETTLE_FLUX_FABM


SUBROUTINE FABM_VERTICAL_MOVEMENT(DELTA_T)
  
  USE MOD_NESTING
     REAL(SP), INTENT(IN) :: DELTA_T
     INTEGER :: I,K,IFABM,  II, I_TMP
     REAL(SP), DIMENSION(M,KBM1,size(model%interior_state_variables)) :: w_ct
     REAL(SP), DIMENSION(KBM1-1,size(model%interior_state_variables)) :: w_if, flux_if
     REAL(SP), DIMENSION(KBM1,  size(model%interior_state_variables)) :: flux_ct
     REAL(SP), PARAMETER :: max_courant = 0.99_SP
     LOGICAL :: IN_NESTING

  IF(DBG_SET(DBG_SBR)) WRITE(IPT,*)"Start: FABM_VERTICAL_MOVEMENT :"

     ! Get vertical velocities at layer centers
     DO K=1,KBM1
        CALL model%get_vertical_movement(1,M,K,w_ct(:,K,:))
     END DO

     DO I=1,M
#  if defined (WET_DRY)
       IF(ISWETN(I) == 1) THEN
#  endif
         IF(FABM_NESTING_ON) THEN
          IF(NESTING_ON) THEN
           IN_NESTING=.FALSE.
           DO II = 1, NESTING_GRID%MT
             I_TMP = NESTING_GRID%NLID_X(II)
             IF(I_TMP==I)THEN
               IN_NESTING=.TRUE.
               w_ct(I,:,:)=0.0_SP
               EXIT
             END IF
           END DO
           IF(IN_NESTING)EXIT
          END IF
        END IF
         ! Linearly interpolate to velocities at the interfaces between layers
         ! TODO: account for differences in layer heights (now we simply use the unweighted average of center velocities)
         !w_if(1:KBM1-1,:) = (w_ct(I,1:KBM1-1,:)+w_ct(I,2:KBM1,:))/2
         DO K=1,KBM1-1
             w_if(K,:) = (w_ct(I,K,:)*DZ(I,K+1)*D(I)+w_ct(I,K+1,:)*DZ(I,K)*D(I))/((DZ(I,K+1)+DZ(I,K))*D(I))
             !IF(-D(I)*Z(I,K+1)>1000.0_SP)w_if(K,:)=0.0_SP !zero vertical velocity in the deep canyon by J. Ge
         END DO
         !IF(D(I)>1000.0_SP)w_if(K,:) = 0.0_SP
!         write (*,*) 'Vertical velocities    ',maxval(w_if(1:KBM1-1,:)),minval(w_if(1:KBM1-1,:))         
         ! Convert velocities in m s-1 to mass fluxes (e.g., mol m-2 s-1) - both at interfaces between layers.
         DO IFABM=1,SIZE(model%interior_state_variables)
            DO K=1,KBM1-1
               IF (w_if(K,IFABM)<0) THEN
                  ! Downward - use concentration from same level (velocity is defined on bottom interface)
                  flux_if(K,IFABM) = max(w_if(K,IFABM),min(-DZ(I,K)*D(I)/DELTA_T*max_courant,0._SP))*FABM_ALL(I,K,IFABM)
               ELSE
                  ! Upward - use concentration from level below (velocity is defined on bottom interface)
                  flux_if(K,IFABM) = min(w_if(K,IFABM),max(DZ(I,K+1)*D(I)/DELTA_T*max_courant,0._SP))*FABM_ALL(I,K+1,IFABM)
               END IF
            END DO
         END DO

         ! Combine interfacial mass fluxes (top + bottom) into total fluxes per layer
         flux_ct(1,       :) = flux_if(1,       :)
         flux_ct(2:KBM1-1,:) = flux_if(2:KBM1-1,:) - flux_if(1:KBM1-2,:)
         flux_ct(KBM1,    :) =                     - flux_if(KBM1-1,  :)

         ! Convert mass fluxes (m-2) into source terms (m-3) by dividing by layer height
         DO IFABM=1,SIZE(model%interior_state_variables)
            FABM_ALL(I,1:KBM1,IFABM) = FABM_ALL(I,1:KBM1,IFABM) + DELTA_T*flux_ct(1:KBM1,IFABM)/DZ(I,1:KBM1)/D(I)
         END DO
#  if defined (WET_DRY)
      END IF
#  endif
   END DO  ! I=1:M
  IF(DBG_SET(DBG_SBR)) WRITE(IPT,*)"End: FABM_VERTICAL_MOVEMENT :"
END SUBROUTINE FABM_VERTICAL_MOVEMENT

SUBROUTINE FABM_EXCHANGE(F)
     USE MOD_PAR

     REAL(SP), DIMENSION(0:MT,KB,size(model%interior_state_variables)) :: F
#if defined (MULTIPROCESSOR)
     INTEGER :: IFABM
     REAL(SP),ALLOCATABLE :: F_T(:,:)

  IF(DBG_SET(DBG_SBR)) WRITE(IPT,*)"Start: FABM_EXCHANGE :"

     ALLOCATE(F_T(0:MT,KB))
     DO IFABM=1,size(model%interior_state_variables)
        F_T(:,:)  = F(:,:,IFABM)
        CALL AEXCHANGE(NC,MYID,NPROCS,F_T)
        F(:,:,IFABM)  = F_T(:,:)  
     END DO 
     DEALLOCATE(F_T)
  IF(DBG_SET(DBG_SBR)) WRITE(IPT,*)"End: FABM_EXCHANGE :"

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
!DIR$ ATTRIBUTES NOINLINE :: FABM_MIXING

# if defined (THIN_DAM)
  USE MOD_DAM,only : NODE_DAM1_N,NODE_DAM2_N,NODE_DAM3_N&
                      &,I_NODE_DAM1_N,I_NODE_DAM2_N,I_NODE_DAM3_N,KDAM
# endif

  IMPLICIT NONE
  INTEGER :: I,K,J,KI,IFABM
  REAL(DP) :: TMP,TMP1,TMP2,TMP3,FKH,UMOLPR
  REAL(SP), DIMENSION(0:MT,KB,size(model%interior_state_variables)) :: F
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

  DO IFABM=1,size(model%interior_state_variables)
      IF (.not. DISABLE_TRANSPORT(IFABM) ) then    

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
     ENDIF! IF (.not. DISABLE_TRANSPORT(IFABM) ) then    

  END DO ! IFABM

  IF(DBG_SET(DBG_SBR)) WRITE(IPT,*)"End: FABM_MIXING"
  RETURN
END SUBROUTINE FABM_MIXING

SUBROUTINE FABM_ADV_RK(FABM_FB1)
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
   USE MOD_SPHERICAL  !syshi 20191013
   USE MOD_NORTHPOLE  !syshi 20191013
   USE MOD_NESTING
# if defined (THIN_DAM)
   USE MOD_DAM, only : kdam,N_DAM_MATCH,IS_DAM
# endif

# if defined (TVD)
   USE MOD_TVD
# endif

   IMPLICIT NONE
   REAL(SP), DIMENSION(0:MT,KB,size(model%interior_state_variables))  :: XFLUX,XFLUX_ADV
   REAL(SP), DIMENSION(M)           :: PUPX,PUPY,PVPX,PVPY
   REAL(SP), DIMENSION(M)           :: PFPX,PFPY,PFPXD,PFPYD,VISCOFF
   REAL(SP), DIMENSION(3*(NT),KBM1) :: DTIJ
   REAL(SP), DIMENSION(3*(NT),KBM1) :: UVN
   REAL(SP) :: UTMP,VTMP,SITAI,FFD,FF1 !,X11,Y11,X22,Y22,X33,Y33 !,TMP1,TMP2 !,XI,YI
   REAL(SP) :: DXA,DYA,DXB,DYB,FIJ1,FIJ2,UN
   REAL(SP) :: TXX,TYY,FXX,FYY,VISCOF,EXFLUX,TEMP,STPOINT
   REAL(SP) :: FACT,FM1
   INTEGER  :: I,I1,I2,IA,IB,J,J1,J2,K,JTMP,JJ,IFABM

# if defined (TVD)
   REAL(SP), DIMENSION(1:M)             :: dstA,dstB
   REAL(SP), DIMENSION(1:NCV)           :: XUA,XUB,YUA,YUB
   REAL(SP) :: LX,LY,minA,minB,FAB_upstreamA,FAB_upstreamB,A_limiter
   REAL(SP) :: B_limiter,smoothrA,smoothrB
#else
   REAL(SP) :: FABM1MIN, FABM1MAX, FABM2MIN, FABM2MAX
# endif

# if defined (LIMITER_VER_ADV)
   REAL(SP)  CONV_S(1:KB), DISS_S(1:KB)
   REAL(SP)  SL_H(0:KB), S_TEMP(0:KB)
   REAL(SP)  SL_U, SL_F
# endif

# if defined (THIN_DAM)
   INTEGER :: NX
# endif

   REAL(SP), DIMENSION(0:MT,KB,size(model%interior_state_variables))     :: FABM_FB1    !! temporary salinity in RK
   REAL(SP) :: FABM_MEAN

   IF(DBG_SET(DBG_SBR)) WRITE(IPT,*)"Start: FABM_ADV_RK"
 !------------------------------------------------------------------------------!


   SELECT CASE(HORIZONTAL_MIXING_TYPE)
     CASE ('closure')
       FACT = 1.0_SP
       FM1  = 0.0_SP
     CASE('constant')
       FACT = 0.0_SP
       FM1  = 1.0_SP
     CASE DEFAULT
       CALL FATAL_ERROR("UNKNOW HORIZONTAL MIXING TYPE in FABM adv:",&
       & TRIM(HORIZONTAL_MIXING_TYPE) )
   END SELECT

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
           ! USE U,V
           UVN(I,K)  = V(I1,K)*DLTXE(I) - U(I1,K)*DLTYE(I)
       END DO
   END DO

   !
   !--Calculate the Advection and Horizontal Diffusion Terms----------------------!
   !

   DO IFABM=1,size(model%interior_state_variables)
       DO K=1,KBM1
           PFPX  = 0.0_SP
           PFPY  = 0.0_SP
           PFPXD = 0.0_SP
           PFPYD = 0.0_SP
           FABM_MEAN = SUM(FABM_ALL(1:M,K,IFABM))/M
           DO I=1,M
               DO J=1,NTSN(I)-1
                   I1=NBSN(I,J)
                   I2=NBSN(I,J+1)

#    if defined (WET_DRY)
                   IF(ISWETN(I1) == 0 .AND. ISWETN(I2) == 1)THEN
                       FFD=0.5_SP*((FABM_ALL(I,K,IFABM)-FABM_MEAN)+(FABM_ALL(I2,K,IFABM)-FABM_MEAN))
                       FF1=0.5_SP*(FABM_ALL(I,K,IFABM)+FABM_ALL(I2,K,IFABM))
                   ELSE IF(ISWETN(I1) == 1 .AND. ISWETN(I2) == 0)THEN
                       FFD=0.5_SP*((FABM_ALL(I1,K,IFABM)-FABM_MEAN)+(FABM_ALL(I,K,IFABM)-FABM_MEAN))
                       FF1=0.5_SP*(FABM_ALL(I1,K,IFABM)+FABM_ALL(I,K,IFABM))
                   ELSE IF(ISWETN(I1) == 0 .AND. ISWETN(I2) == 0)THEN
                       FFD=FABM_ALL(I,K,IFABM)-FABM_MEAN
                       FF1=FABM_ALL(I,K,IFABM)
                   ELSE
                       FFD=0.5_SP*((FABM_ALL(I1,K,IFABM)-FABM_MEAN)+(FABM_ALL(I2,K,IFABM)-FABM_MEAN))
                       FF1=0.5_SP*(FABM_ALL(I1,K,IFABM)+FABM_ALL(I2,K,IFABM))
                   END IF
#    else
                   FFD=0.5_SP*((FABM_ALL(I1,K,IFABM)-FABM_MEAN)+(FABM_ALL(I2,K,IFABM)-FABM_MEAN))
                   FF1=0.5_SP*(FABM_ALL(I1,K,IFABM)+FABM_ALL(I2,K,IFABM))
#    endif

                   PFPX(I)=PFPX(I)+FF1*DLTYTRIE(i,j)
                   PFPY(I)=PFPY(I)+FF1*DLTXTRIE(i,j)
                   PFPXD(I)=PFPXD(I)+FFD*DLTYTRIE(i,j)
                   PFPYD(I)=PFPYD(I)+FFD*DLTXTRIE(i,j)

               END DO ! J- Loop
! gather all neighboring control volumes connecting at dam node
# if defined (THIN_DAM)
               IF(IS_DAM(I)==1.AND.K<=KDAM(I))THEN
                   DO NX=1,N_DAM_MATCH(I,1)
                       DO J=1,NTSN(N_DAM_MATCH(I,NX+1))-1
                           I1=NBSN(N_DAM_MATCH(I,NX+1),J)
                           I2=NBSN(N_DAM_MATCH(I,NX+1),J+1)
                           FFD=0.5_SP*((FABM_ALL(I1,K,IFABM)-FABM_MEAN)+(FABM_ALL(I2,K,IFABM)-FABM_MEAN))
                           FF1=0.5_SP*(FABM_ALL(I1,K,IFABM)+FABM_ALL(I2,K,IFABM))
                           PFPX(I)=PFPX(I)+FF1*DLTYTRIE(N_DAM_MATCH(I,NX+1),j)
                           PFPY(I)=PFPY(I)+FF1*DLTXTRIE(N_DAM_MATCH(I,NX+1),j)
                           PFPXD(I)=PFPXD(I)+FFD*DLTYTRIE(N_DAM_MATCH(I,NX+1),j)
                           PFPYD(I)=PFPYD(I)+FFD*DLTXTRIE(N_DAM_MATCH(I,NX+1),j)
                       END DO
                   END DO
               END IF
# endif
# if !defined (THIN_DAM)
               PFPX(I)=PFPX(I)/ART2(I)
               PFPY(I)=PFPY(I)/ART2(I)
               PFPXD(I)=PFPXD(I)/ART2(I)
               PFPYD(I)=PFPYD(I)/ART2(I)
# else
               IF(IS_DAM(I)==1.AND.K<=KDAM(I))THEN
                   PFPX(I)=PFPX(I)/(ART2(I)+SUM(ART2(N_DAM_MATCH(I,2:1+N_DAM_MATCH(I,1)))))
                   PFPY(I)=PFPY(I)/(ART2(I)+SUM(ART2(N_DAM_MATCH(I,2:1+N_DAM_MATCH(I,1)))))
                   PFPXD(I)=PFPXD(I)/(ART2(I)+SUM(ART2(N_DAM_MATCH(I,2:1+N_DAM_MATCH(I,1)))))
                   PFPYD(I)=PFPYD(I)/(ART2(I)+SUM(ART2(N_DAM_MATCH(I,2:1+N_DAM_MATCH(I,1)))))
               ELSE
                   PFPX(I)=PFPX(I)/ART2(I)
                   PFPY(I)=PFPY(I)/ART2(I)
                   PFPXD(I)=PFPXD(I)/ART2(I)
                   PFPYD(I)=PFPYD(I)/ART2(I)
               END IF
# endif

           END DO ! M loop

           IF(K == KBM1)THEN
               DO I=1,M
                   PFABMPXB(I,IFABM) = PFPX(I)
                   PFABMPYB(I,IFABM) = PFPY(I)
               END DO
           END IF

           DO I = 1,M
               VISCOFF(I)=VISCOFH(I,K)
           END DO

           IF(K == KBM1) THEN
               AH_BOTTOM(1:M) = (FACT*VISCOFF(1:M) + FM1)*NN_HVC(1:M)
               ! !********************Jadon**************************************
               ! --- This Could be useful in large domain implementations ----
               !     DO I = 1,M
               !       AH_BOTTOM(i) = (FACT*VISCOFF(i) + FM1) * NN_HVC(i)
               !       if (h(i)>800.0) then
               !       elseif (h(i)<200.0) then
               !         AH_BOTTOM(i)=AH_BOTTOM(i)*0.05
               !       else
               !         AH_BOTTOM(i)=AH_BOTTOM(i)*(1-(800.0-h(i))/600.0*0.95)
               !      endif
               !     END DO
               !********************************************************************
           END IF


           DO I=1,NCV_I
               IA=NIEC(I,1)
               IB=NIEC(I,2)


# if !defined(TVD)
               FIJ1=FABM_ALL(IA,K,IFABM)+DLTXNCVE(I,1)*PFPX(IA)+DLTYNCVE(I,1)*PFPY(IA)
               FIJ2=FABM_ALL(IB,K,IFABM)+DLTXNCVE(I,2)*PFPX(IB)+DLTYNCVE(I,2)*PFPY(IB)

               FABM1MIN=MINVAL(FABM_ALL(NBSN(IA,1:NTSN(IA)-1),K,IFABM))
               FABM1MIN=MIN(FABM1MIN, FABM_ALL(IA,K,IFABM))
               FABM1MAX=MAXVAL(FABM_ALL(NBSN(IA,1:NTSN(IA)-1),K,IFABM))
               FABM1MAX=MAX(FABM1MAX, FABM_ALL(IA,K,IFABM))
               FABM2MIN=MINVAL(FABM_ALL(NBSN(IB,1:NTSN(IB)-1),K,IFABM))
               FABM2MIN=MIN(FABM2MIN, FABM_ALL(IB,K,IFABM))
               FABM2MAX=MAXVAL(FABM_ALL(NBSN(IB,1:NTSN(IB)-1),K,IFABM))
               FABM2MAX=MAX(FABM2MAX, FABM_ALL(IB,K,IFABM))
               IF(FIJ1 < FABM1MIN) FIJ1=FABM1MIN
               IF(FIJ1 > FABM1MAX) FIJ1=FABM1MAX
               IF(FIJ2 < FABM2MIN) FIJ2=FABM2MIN
               IF(FIJ2 > FABM2MAX) FIJ2=FABM2MAX
# else

               ! ------------------------------------------------------------------------------------------ !
               !                                 Drawing the TVD scheme
               ! ------------------------------------------------------------------------------------------ !
               !     If A is upstream of B
               FAB_upstreamA=FABM_ALL(Anear_node(I),K,IFABM)+PFPX(Anear_node(I))*XUAdist(I)+PFPY(Anear_node(I))*YUAdist(I)
               !     If B is upstream of A
               FAB_upstreamB=FABM_ALL(Bnear_node(I),K,IFABM)+PFPX(Bnear_node(I))*XUBdist(I)+PFPY(Bnear_node(I))*YUBdist(I)

               !    The smoothness-parameter
               ! -----------------------------------
               !     Fortran gets grumpy if the values turn infinite, which can be solved with a if-loop
               IF (FABM_ALL(IA,K,IFABM).EQ.FABM_ALL(IB,K,IFABM)) THEN
                   smoothrA    = 0.0_SP
                   smoothrB    = 0.0_SP
               ELSE IF (FABM_ALL(IA,K,IFABM).LT.1.E-10.AND.FABM_ALL(IB,K,IFABM).LT.1.E-10) THEN
                   smoothrA    = 0.0_SP
                   smoothrB    = 0.0_SP
               ELSE
                   !         FAB_upstreamA is the "far upstream node", FABM_ALL(IA,K) is upstream of FABM_ALL(IB,K)
                   smoothrA    = (FABM_ALL(IA,K,IFABM)-FAB_upstreamA)/(FABM_ALL(IB,K,IFABM)-FABM_ALL(IA,K,IFABM))
                   smoothrB    = (FABM_ALL(IB,K,IFABM)-FAB_upstreamB)/(FABM_ALL(IA,K,IFABM)-FABM_ALL(IB,K,IFABM))
               END IF

               !     The Flux-limiter
               !      (superbee)
               A_limiter = MAX(0.0_SP, MIN(1.0_SP, 2.0_SP*smoothrA), MIN(2.0_SP, smoothrA))
               B_limiter = MAX(0.0_SP, MIN(1.0_SP, 2.0_SP*smoothrB), MIN(2.0_SP, smoothrB))

               !      (minmod)
               !       A_limiter = MAX(0.0_SP,MIN(1.0_SP,smoothrA))
               !       B_limiter = MAX(0.0_SP,MIN(1.0_SP,smoothrB))

               !      (To check if the upwind scheme is working)
               !       A_limiter = 0.0_SP;
               !       B_limiter = 0.0_SP;

               !     Calculating the tracer-concentration at the cell-wall in-between IA and IB
               FIJ1     = FABM_ALL(IA,K,IFABM)+0.5_SP*A_LIMITER*(FABM_ALL(IB,K,IFABM)-FABM_ALL(IA,K,IFABM))
               FIJ2     = FABM_ALL(IB,K,IFABM)+0.5_SP*B_LIMITER*(FABM_ALL(IA,K,IFABM)-FABM_ALL(IB,K,IFABM))

               ! TVD done.
# endif

               UN=UVN(I,K)

               !VISCOF=HORCON*(FACT*(VISCOFF(IA)+VISCOFF(IB))*0.5_SP + FM1)

               ! David moved HPRNU and added HVC
               VISCOF=(FACT*0.5_SP*(VISCOFF(IA)*NN_HVC(IA)+VISCOFF(IB)*NN_HVC(IB)) + FM1*0.5_SP*(NN_HVC(IA)+NN_HVC(IB)))
               !*******************************Jadon*************************************
               ! ---- Could be useful in large implementations with shelf breaks -----------------
               !       VISCOF=(FACT*0.5_SP*(VISCOFF(IA)*NN_HVC(IA)+VISCOFF(IB)*NN_HVC(IB)) + FM1*0.5_SP*(NN_HVC(IA)+NN_HVC(IB)))
               !     if (min(h(ia),h(ib))>800.0) then
               !     elseif (min(h(ia),h(ib))<200.0) then
               !       VISCOF=VISCOF*0.05
               !     else
               !       VISCOF=VISCOF*(1-(800.0-min(h(ia),h(ib)))/600.0*0.95)
               !     endif
               !*****************************************************************************
               TXX=0.5_SP*(PFPXD(IA)+PFPXD(IB))*VISCOF
               TYY=0.5_SP*(PFPYD(IA)+PFPYD(IB))*VISCOF

               FXX=-DTIJ(I,K)*TXX*DLTYE(I)
               FYY= DTIJ(I,K)*TYY*DLTXE(I)

! Same statement for either TVD or without TVD after replacing FAB12_B with FIJ2 and FAB12_A with FIJ2
               EXFLUX=-UN*DTIJ(I,K)* &
                   ((1.0_SP+SIGN(1.0_SP,UN))*FIJ2+(1.0_SP-SIGN(1.0_SP,UN))*FIJ1)*0.5_SP+FXX+FYY

               XFLUX(IA,K,IFABM)=XFLUX(IA,K,IFABM)+EXFLUX
               XFLUX(IB,K,IFABM)=XFLUX(IB,K,IFABM)-EXFLUX

               XFLUX_ADV(IA,K,IFABM)=XFLUX_ADV(IA,K,IFABM)+(EXFLUX-FXX-FYY)
               XFLUX_ADV(IB,K,IFABM)=XFLUX_ADV(IB,K,IFABM)-(EXFLUX-FXX-FYY)

#      if defined (THIN_DAM)
           IF(K<=KDAM(IA).AND.IS_DAM(IA)==1)THEN
               IF(N_DAM_MATCH(IA,1)==1)THEN
                   XFLUX(N_DAM_MATCH(IA,2),K,IFABM) = XFLUX(N_DAM_MATCH(IA,2),K) + EXFLUX
                   XFLUX_ADV(N_DAM_MATCH(IA,2),K,IFABM) = XFLUX_ADV(N_DAM_MATCH(IA,2),K),IFABM +(EXFLUX-FXX-FYY)
               END IF
               IF(N_DAM_MATCH(IA,1)==2)THEN
                   XFLUX(N_DAM_MATCH(IA,2),K,IFABM) = XFLUX(N_DAM_MATCH(IA,2),K,IFABM) + EXFLUX
                   XFLUX(N_DAM_MATCH(IA,3),K,IFABM) = XFLUX(N_DAM_MATCH(IA,3),K,IFABM) + EXFLUX
                   XFLUX_ADV(N_DAM_MATCH(IA,2),K,IFABM) = XFLUX_ADV(N_DAM_MATCH(IA,2),K,IFABM) +(EXFLUX-FXX-FYY)
                   XFLUX_ADV(N_DAM_MATCH(IA,3),K,IFABM) = XFLUX_ADV(N_DAM_MATCH(IA,3),K,IFABM) +(EXFLUX-FXX-FYY)
               END IF
               IF(N_DAM_MATCH(IA,1)==3)THEN
                   XFLUX(N_DAM_MATCH(IA,2),K,IFABM) = XFLUX(N_DAM_MATCH(IA,2),K,IFABM) + EXFLUX
                   XFLUX(N_DAM_MATCH(IA,3),K,IFABM) = XFLUX(N_DAM_MATCH(IA,3),K,IFABM) + EXFLUX
                   XFLUX(N_DAM_MATCH(IA,4),K,IFABM) = XFLUX(N_DAM_MATCH(IA,4),K,IFABM) + EXFLUX
                   XFLUX_ADV(N_DAM_MATCH(IA,2),K) = XFLUX_ADV(N_DAM_MATCH(IA,2),K,IFABM) +(EXFLUX-FXX-FYY)
                   XFLUX_ADV(N_DAM_MATCH(IA,3),K) = XFLUX_ADV(N_DAM_MATCH(IA,3),K,IFABM) +(EXFLUX-FXX-FYY)
                   XFLUX_ADV(N_DAM_MATCH(IA,4),K) = XFLUX_ADV(N_DAM_MATCH(IA,4),K,IFABM) +(EXFLUX-FXX-FYY)
               END IF
           END IF
           IF(K<=KDAM(IB).AND.IS_DAM(IB)==1)THEN
               IF(N_DAM_MATCH(IB,1)==1)THEN
                   XFLUX(N_DAM_MATCH(IB,2),K,IFABM) = XFLUX(N_DAM_MATCH(IB,2),K,IFABM) - EXFLUX
                   XFLUX_ADV(N_DAM_MATCH(IB,2),K,IFABM) = XFLUX_ADV(N_DAM_MATCH(IB,2),K,IFABM) - (EXFLUX-FXX-FYY)
               END IF
               IF(N_DAM_MATCH(IB,1)==2)THEN
                   XFLUX(N_DAM_MATCH(IB,2),K,IFABM) = XFLUX(N_DAM_MATCH(IB,2),K,IFABM) - EXFLUX
                   XFLUX(N_DAM_MATCH(IB,3),K,IFABM) = XFLUX(N_DAM_MATCH(IB,3),K,IFABM) - EXFLUX
                   XFLUX_ADV(N_DAM_MATCH(IB,2),K,IFABM) = XFLUX_ADV(N_DAM_MATCH(IB,2),K,IFABM) - (EXFLUX-FXX-FYY)
                   XFLUX_ADV(N_DAM_MATCH(IB,3),K,IFABM) = XFLUX_ADV(N_DAM_MATCH(IB,3),K,IFABM) - (EXFLUX-FXX-FYY)
               END IF
               IF(N_DAM_MATCH(IB,1)==3)THEN
                   XFLUX(N_DAM_MATCH(IB,2),K,IFABM) = XFLUX(N_DAM_MATCH(IB,2),K,IFABM) - EXFLUX
                   XFLUX(N_DAM_MATCH(IB,3),K,IFABM) = XFLUX(N_DAM_MATCH(IB,3),K,IFABM) - EXFLUX
                   XFLUX(N_DAM_MATCH(IB,4),K,IFABM) = XFLUX(N_DAM_MATCH(IB,4),K,IFABM) - EXFLUX
                   XFLUX_ADV(N_DAM_MATCH(IB,2),K,IFABM) = XFLUX_ADV(N_DAM_MATCH(IB,2),K,IFABM) - (EXFLUX-FXX-FYY)
                   XFLUX_ADV(N_DAM_MATCH(IB,3),K,IFABM) = XFLUX_ADV(N_DAM_MATCH(IB,3),K,IFABM) - (EXFLUX-FXX-FYY)
                   XFLUX_ADV(N_DAM_MATCH(IB,4),K,IFABM) = XFLUX_ADV(N_DAM_MATCH(IB,4),K,IFABM) - (EXFLUX-FXX-FYY)
               END IF
           END IF
#      endif
           END DO ! I, NCV_I loop

#    if defined (SPHERICAL)
           CALL ADV_S_XY(XFLUX(:,:,IFABM),XFLUX_ADV(:,:,IFABM),PFPX,PFPY,PFPXD,PFPYD,VISCOFF,K,0.0_SP)
#    endif

       END DO !!SIGMA LOOP K, KBM1
   END DO !to size(model%interior_state_variables)

   !
   !-Accumulate Fluxes at Boundary Nodes
   !
# if defined (MULTIPROCESSOR)
   DO IFABM=1,size(model%interior_state_variables)
       IF(PAR)CALL NODE_MATCH(0,NBN,BN_MLT,BN_LOC,BNC,MT,KB,MYID,NPROCS,XFLUX(:,:,IFABM),XFLUX_ADV(:,:,IFABM))
   END DO
# endif

   DO IFABM=1,size(model%interior_state_variables)
       DO K=1,KBM1
           IF(IOBCN > 0) THEN
               DO I=1,IOBCN
                   I1=I_OBC_N(I)
                   XFLUX_OBCB(I,K,IFABM)=XFLUX_ADV(I1,K,IFABM)
               END DO
           END IF
       END DO
   END DO

   DO IFABM=1,size(model%interior_state_variables)

       !--------------------------------------------------------------------
       !   The central difference scheme in vertical advection
       !--------------------------------------------------------------------
       DO I=1,M
#    if defined (WET_DRY)
           IF(ISWETN(I)*ISWETNT(I) == 1) THEN
#    endif

#    if defined (LIMITER_VER_ADV)
           S_TEMP(0)  = -FABM_ALL(I,1, IFABM)
           S_TEMP(KB) = -FABM_ALL(I,KBM1, IFABM)
           SL_H(0)    = DZ(I,1)
           SL_H(KB)   = DZ(I,KBM1)
           DO K=1, KBM1
               S_TEMP(K) = FABM_ALL(I,K, IFABM)
               SL_H(K)   = DZ(I,K)
           ENDDO

           DO K=2, KBM1
               CONV_S(K) = WTS(I,K)*(S_TEMP(K)+S_TEMP(K-1))*0.5_SP
               SL_U = 2.0_SP*(S_TEMP(K)-S_TEMP(K+1))/(SL_H(K)+SL_H(K+1))
               SL_F = 2.0_SP*(S_TEMP(K-2)-S_TEMP(K-1))/(SL_H(K-2)+SL_H(K-1))
               DISS_S(K) = 0.5_SP*ABS(WTS(I,K))*(S_TEMP(K-1)-S_TEMP(K)-0.5_SP*LIMLED2(SL_U,SL_F,2.0_SP)*(SL_H(K-1)+SL_H(K)))
           ENDDO
           CONV_S(1)  = 0.0_SP
           DISS_S(1)  = 0.0_SP
           CONV_S(KB) = 0.0_SP
           DISS_S(KB) = 0.0_SP
#       endif

               DO K=1, KBM1

#          if defined (LIMITER_VER_ADV)
                   TEMP = CONV_S(K)-CONV_S(K+1)+DISS_S(K+1)-DISS_S(K)
#          endif

#          if !defined (LIMITER_VER_ADV)
                   IF(K == 1) THEN
                       TEMP=-WTS(I,K+1)*(FABM_ALL(I,K,IFABM)*DZ(I,K+1)+FABM_ALL(I,K+1,IFABM)*DZ(I,K))/   &
                           (DZ(I,K)+DZ(I,K+1))
                   ELSE IF(K == KBM1) THEN
                       TEMP= WTS(I,K)*(FABM_ALL(I,K,IFABM)*DZ(I,K-1)+FABM_ALL(I,K-1,IFABM)*DZ(I,K))/(DZ(I,K)+DZ(I,K-1))
                   ELSE
                       TEMP= WTS(I,K)*(FABM_ALL(I,K,IFABM)*DZ(I,K-1)+FABM_ALL(I,K-1,IFABM)*DZ(I,K))/(DZ(I,K)+DZ(I,K-1))-&
                           WTS(I,K+1)*(FABM_ALL(I,K,IFABM)*DZ(I,K+1)+FABM_ALL(I,K+1,IFABM)*DZ(I,K))/(DZ(I,K)+DZ(I,K+1))
                   END IF
#          endif

                   IF(ISONB(I) == 2) THEN
                       !         XFLUX(I,K,IFABM)=TEMP*ART1(I)/DZ(I,K)
                       XFLUX(I,K,IFABM)=TEMP*ART1(I)
                   ELSE
                       !         XFLUX(I,K,IFABM)=XFLUX(I,K,IFABM)+TEMP*ART1(I)/DZ(I,K)
                       XFLUX(I,K,IFABM)=XFLUX(I,K,IFABM)+TEMP*ART1(I)
#          if defined (THIN_DAM)
                   IF(IS_DAM(I)==1.AND.K<=KDAM(I))THEN
                       IF(N_DAM_MATCH(I,1)==1)THEN
                           XFLUX(N_DAM_MATCH(I,2),K,IFABM) = XFLUX(N_DAM_MATCH(I,2),K,IFABM)+TEMP*ART1(I)
                       END IF
                       IF(N_DAM_MATCH(I,1)==2)THEN
                           XFLUX(N_DAM_MATCH(I,2),K,IFABM) = XFLUX(N_DAM_MATCH(I,2),K,IFABM)+TEMP*ART1(I)
                           XFLUX(N_DAM_MATCH(I,3),K,IFABM) = XFLUX(N_DAM_MATCH(I,3),K,IFABM)+TEMP*ART1(I)
                       END IF
                       IF(N_DAM_MATCH(I,1)==3)THEN
                           XFLUX(N_DAM_MATCH(I,2),K,IFABM) = XFLUX(N_DAM_MATCH(I,2),K,IFABM)+TEMP*ART1(I)
                           XFLUX(N_DAM_MATCH(I,3),K,IFABM) = XFLUX(N_DAM_MATCH(I,3),K,IFABM)+TEMP*ART1(I)
                           XFLUX(N_DAM_MATCH(I,4),K,IFABM) = XFLUX(N_DAM_MATCH(I,4),K,IFABM)+TEMP*ART1(I)
                       END IF
                   END IF
#          endif

                   END IF
               ENDDO
#    if defined (WET_DRY)
           END IF
#    endif
       END DO  !! SIGMA LOOP

       !
       !--Set Boundary Conditions-For Fresh Water Flux--------------------------------!
       !
       ! Check if variable will dilute
       IF (.NOT. model%interior_state_variables(IFABM)%no_river_dilution) THEN
           IF(RIVER_TS_SETTING == 'calculated') THEN
               IF(RIVER_INFLOW_LOCATION == 'node') THEN
                   IF(NUMQBC > 0) THEN
                       DO J=1,NUMQBC
                           JJ=INODEQ(J)
                           STPOINT=FABMDIS(J,IFABM) ! river concentration in original units mass/vol
                           DO K=1,KBM1
                               !             XFLUX(JJ,K,IFABM)=XFLUX(JJ,K,IFABM) - QDIS(J)*VQDIST(J,K)*STPOINT/DZ(JJ,K)
                               XFLUX(JJ,K,IFABM)=XFLUX(JJ,K,IFABM) - QDIS(J)*VQDIST(J,K)*STPOINT
                           END DO
                       END DO
                   END IF
               ELSE IF(RIVER_INFLOW_LOCATION == 'edge') THEN
                   IF(NUMQBC > 0) THEN
                       DO J=1,NUMQBC
                           J1=N_ICELLQ(J,1)
                           J2=N_ICELLQ(J,2)
                           STPOINT=FABMDIS(J,IFABM) !! river concentration in original units M/L3
                           DO K=1,KBM1
                               XFLUX(J1,K,IFABM)=XFLUX(J1,K,IFABM)-QDIS(J)*RDISQ(J,1)*VQDIST(J,K)*STPOINT
                               XFLUX(J2,K,IFABM)=XFLUX(J2,K,IFABM)-QDIS(J)*RDISQ(J,2)*VQDIST(J,K)*STPOINT
                           END DO
                       END DO
                   END IF
               END IF
           END IF
       ELSE
           ! we don't want this variable to be diluted with 0 concentration
           IF (DBG_SET(DBG_VRB)) WRITE (IPT,*) "FABM Variable "//TRIM(model%interior_state_variables(IFABM)%name)//" is not in river forcing file and will not be diluted"
       END IF
       ! lwang added for groundwater calculated method
       !
       !--Set Boundary Conditions-For ground water
       !Flux--------------------------------!
       ! Uncomment when added to namelist
       !RJT  IF(GROUNDWATER_FABM_SETTING == 'calculated') THEN
       !RJT      DO J=1,M
       !RJT         IF (BFWDIS(J) /= 0.0_SP) THEN
       !RJT            IF (BFWFABM(J,IFABM)>0.0_SP) THEN
       !RJT               STPOINT=BFWFABM(J,IFABM)
       !RJT                DO K=1,KBM1
       !RJT                    XFLUX(J,K,IFABM)=XFLUX(J,K,IFABM)-BFWDIS(J)*STPOINT*BFWFRAC(K) ! 1029
       !RJT                END DO
       !RJT!LWANG            ELSE
       !RJT!LWANG               STPOINT=FABM_ALL(J,KBM1,IFABM)
       !RJT!LWANG               XFLUX(J,KBM1,IFABM)=XFLUX(J,KBM1,IFABM)-BFWDIS(J)*STPOINT
       !RJT            END IF
       !RJT         END IF
       !RJT      END DO
       !RJT  END IF
       ! lwang
       !---------------------------------------------------------------------

       !--Update Variables------------------------------------------------------------!
       ! This is really the only difference between RK and standard
#  if defined (WET_DRY)
       DO I=1,M
           IF(ISWETN(I)*ISWETNT(I) == 1 )THEN
               DO K=1,KBM1
#      if !defined (THIN_DAM)
                   FABM_F(I,K,IFABM)=(FABM_ALL(I,K,IFABM)-XFLUX(I,K,IFABM)/ART1(I)*(DTI/(DT(I)*DZ(I,K))))*(DT(I)/DTFA(I))
#      else
                   IF(IS_DAM(I)==1.AND.K<=KDAM(I))THEN
                       FABM_F(I,K,IFABM)=(FABM_FB1(I,K,IFABM)-XFLUX(I,K,IFABM)/(ART1(I)&
                       &+SUM(ART1(N_DAM_MATCH(I,2:1+N_DAM_MATCH(I,1)))))*(DTI/(DT(I)*DZ(I,K))))*(DT(I)/DTFA(I))
                   ELSE
                       FABM_F(I,K,IFABM)=(FABM_FB1(I,K,IFABM)-XFLUX(I,K,IFABM)/ART1(I)*(DTI/(DT(I)*DZ(I,K))))*(DT(I)/DTFA(I))
                   END IF
#      endif

               END DO

           ELSE ! IF NOT WET S STAYS THE SAME
               DO K=1,KBM1
                   FABM_F(I,K,IFABM)=FABM_FB1(I,K,IFABM)
               END DO
           END IF
       END DO

# else
       ! WET-DRY

       DO I=1,M
           DO K=1,KBM1
#      if !defined (THIN_DAM)
               FABM_F(I,K,IFABM)=(FABM_FB1(I,K,IFABM)-XFLUX(I,K,IFABM)/ART1(I)*(DTI/(DT(I)*DZ(I,K))))*(DT(I)/DTFA(I))
#      else
               IF(IS_DAM(I)==1.AND.K<=KDAM(I))THEN
                   FABM_F(I,K,IFABM)=(FABM_FB1(I,K,IFABM)-XFLUX(I,K,IFABM)/(ART1(I)&
                   &+SUM(ART1(N_DAM_MATCH(I,2:1+N_DAM_MATCH(I,1)))))*(DTI/(DT(I)*DZ(I,K))))*(DT(I)/DTFA(I))
               ELSE
                   FABM_F(I,K,IFABM)=(FABM_FB1(I,K,IFABM)-XFLUX(I,K,IFABM)/ART1(I)*(DTI/(DT(I)*DZ(I,K))))*(DT(I)/DTFA(I))
               END IF
#      endif
           END DO

       END DO

# endif
       ! WET-DRY

   END DO !do IFABM=1,size(model%interior_state_variables)

   IF(DBG_SET(DBG_SBR)) WRITE(IPT,*)"End: FABM_ADV_RK"
END SUBROUTINE FABM_ADV_RK




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
  USE MOD_SPHERICAL  !syshi 20191013
  USE MOD_NORTHPOLE  !syshi 20191013
  USE MOD_NESTING
# if defined (SEMI_IMPLICIT)
  USE MOD_SEMI_IMPLICIT
# endif

# if defined (TVD)
  USE MOD_TVD
# endif

  IMPLICIT NONE
  REAL(SP), DIMENSION(0:MT,KB,size(model%interior_state_variables))  :: XFLUX,XFLUX_ADV 
  REAL(SP), DIMENSION(M)           :: PUPX,PUPY,PVPX,PVPY  
  REAL(SP), DIMENSION(M)           :: PFPX,PFPY,PFPXD,PFPYD,VISCOFF
  REAL(SP), DIMENSION(3*(NT),KBM1) :: DTIJ 
  REAL(SP), DIMENSION(3*(NT),KBM1) :: UVN
  REAL(SP) :: FFD,FF1   !,X11,Y11,X22,Y22,X33,Y33,TMP1,TMP2,XI,YI
  REAL(SP) :: FIJ1,FIJ2,UN ! removed DXA,DYA,DXB,DYB
  REAL(SP) :: TXX,TYY,FXX,FYY,VISCOF,EXFLUX,TEMP,STPOINT
  REAL(SP) :: FACT,FM1
  INTEGER  :: I,I1,I2,IA,IB,J,J1,J2,K,JTMP,JJ,IFABM
  REAL(SP) :: WQM1MIN, WQM1MAX, WQM2MIN, WQM2MAX

# if defined (TVD)
  REAL(SP), DIMENSION(1:M)             :: dstA,dstB
  REAL(SP), DIMENSION(1:NCV)           :: XUA,XUB,YUA,YUB
  REAL(SP) :: LX,LY,minA,minB,FAB_upstreamA,FAB_upstreamB,A_limiter
  REAL(SP) :: B_limiter,FAB12_A,FAB12_B,smoothrA,smoothrB
# endif

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
  REAL(SP), DIMENSION(0:MT,KB)     :: FABM_FRESH    !! for source term which also bring mass volume
!lwang for MPDATA 0316
  REAL(SP), DIMENSION(0:MT,KB)     :: OFFS    !! Offset to WQM field to avoid values less than zero.
!lwang
  INTEGER ITERA, NTERA
#  endif
   REAL(SP) :: FABM_MEAN




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

!# if defined (SPHERICAL)
!    CALL FATAL_ERROR("SPHERICAL OPTION NOT IMPLEMENTED IN FABM_ADV")
!# endif
# if defined (THIN_DAM)
    CALL FATAL_ERROR("THIN_DAM OPTION NOT IMPLEMENTED IN FABM_ADV")
# endif
# if !defined(MPDATA)
!#   if defined (SEMI_IMPLICIT)
!      CALL FATAL_ERROR("SEMI_IMPLICIT OPTION NOT IMPLEMENTED IN FABM_ADV")
!#   endif
#   if defined (NH)
      CALL FATAL_ERROR("NH OPTION NOT IMPLEMENTED IN FABM_ADV")
#   endif
#   if defined (LIMITER_VER_ADV)
      CALL FATAL_ERROR("LIMITER_VER_ADV OPTION NOT IMPLEMENTED IN FABM_ADV")
#   endif
# endif

!lwang for MPDATA 0316
# if defined (MPDATA)
  OFFS = 5.0_SP
# endif
!lwang
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
       ! USE U,V
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

 DO IFABM=1,size(model%interior_state_variables)
!lwang for MPDATA 0316
#  if defined (MPDATA)
   ! Adding offset to avoid less-than-zero problems
     FABM_ALL(:,:,IFABM) = FABM_ALL(:,:,IFABM)+OFFS(:,:)
#  endif
!lwang
  DO K=1,KBM1
     PFPX  = 0.0_SP
     PFPY  = 0.0_SP
     PFPXD = 0.0_SP
     PFPYD = 0.0_SP
     FABM_MEAN = SUM(FABM_ALL(1:M,K,IFABM))/M
     DO I=1,M
        DO J=1,NTSN(I)-1
           I1=NBSN(I,J)
           I2=NBSN(I,J+1)

#    if defined (WET_DRY)
         IF(ISWETN(I1) == 0 .AND. ISWETN(I2) == 1)THEN
          FFD=0.5_SP*((FABM_ALL(I,K,IFABM)-FABM_MEAN)+(FABM_ALL(I2,K,IFABM)-FABM_MEAN))
          FF1=0.5_SP*(FABM_ALL(I,K,IFABM)+FABM_ALL(I2,K,IFABM))
	 ELSE IF(ISWETN(I1) == 1 .AND. ISWETN(I2) == 0)THEN
          FFD=0.5_SP*((FABM_ALL(I1,K,IFABM)-FABM_MEAN)+(FABM_ALL(I,K,IFABM)-FABM_MEAN))
          FF1=0.5_SP*(FABM_ALL(I1,K,IFABM)+FABM_ALL(I,K,IFABM))
	 ELSE IF(ISWETN(I1) == 0 .AND. ISWETN(I2) == 0)THEN
          FFD=FABM_ALL(I,K,IFABM)-FABM_MEAN
          FF1=FABM_ALL(I,K,IFABM)
	 ELSE
          FFD=0.5_SP*((FABM_ALL(I1,K,IFABM)-FABM_MEAN)+(FABM_ALL(I2,K,IFABM)-FABM_MEAN))
          FF1=0.5_SP*(FABM_ALL(I1,K,IFABM)+FABM_ALL(I2,K,IFABM))
	 END IF 
#    else	 
           FFD=0.5_SP*((FABM_ALL(I1,K,IFABM)-FABM_MEAN)+(FABM_ALL(I2,K,IFABM)-FABM_MEAN))
           FF1=0.5_SP*(FABM_ALL(I1,K,IFABM)+FABM_ALL(I2,K,IFABM))
#    endif	 
	 

           
           PFPX(I)=PFPX(I)+FF1*DLTYTRIE(i,j)
           PFPY(I)=PFPY(I)+FF1*DLTXTRIE(i,j)
           PFPXD(I)=PFPXD(I)+FFD*DLTYTRIE(i,j)
           PFPYD(I)=PFPYD(I)+FFD*DLTXTRIE(i,j)
           

        END DO ! J- Loop

# if !defined (THIN_DAM)
        PFPX(I)=PFPX(I)/ART2(I)
        PFPY(I)=PFPY(I)/ART2(I)
        PFPXD(I)=PFPXD(I)/ART2(I)
        PFPYD(I)=PFPYD(I)/ART2(I)
# endif

     END DO ! M loop

     IF(K == KBM1)THEN
        DO I=1,M
           PFABMPXB(I,IFABM) = PFPX(I)
           PFABMPYB(I,IFABM) = PFPY(I)
        END DO
     END IF

     DO I = 1,M
        VISCOFF(I)=VISCOFH(I,K)
     END DO

     IF(K == KBM1) THEN
       AH_BOTTOM(1:M) = (FACT*VISCOFF(1:M) + FM1)*NN_HVC(1:M)
! !********************Jadon**************************************
! --- This Could be useful in large domain implementations ----
!     DO I = 1,M
!       AH_BOTTOM(i) = (FACT*VISCOFF(i) + FM1) * NN_HVC(i)
!       if (h(i)>800.0) then
!       elseif (h(i)<200.0) then
!         AH_BOTTOM(i)=AH_BOTTOM(i)*0.05
!       else
!         AH_BOTTOM(i)=AH_BOTTOM(i)*(1-(800.0-h(i))/600.0*0.95) 
!      endif
!     END DO
!********************************************************************
     END IF


     DO I=1,NCV_I
        IA=NIEC(I,1)
        IB=NIEC(I,2)


# if !defined(TVD)
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
# else

! ------------------------------------------------------------------------------------------ !
!                                 Drawing the TVD scheme
! ------------------------------------------------------------------------------------------ !
!     If A is upstream of B
       FAB_upstreamA=FABM_ALL(Anear_node(I),K,IFABM)+PFPX(Anear_node(I))*XUAdist(I)+PFPY(Anear_node(I))*YUAdist(I)
!     If B is upstream of A
       FAB_upstreamB=FABM_ALL(Bnear_node(I),K,IFABM)+PFPX(Bnear_node(I))*XUBdist(I)+PFPY(Bnear_node(I))*YUBdist(I)                

!    The smoothness-parameter
! -----------------------------------
!     Fortran gets grumpy if the values turn infinite, which can be solved with a if-loop
       IF (FABM_ALL(IA,K,IFABM).EQ.FABM_ALL(IB,K,IFABM)) THEN
          smoothrA    = 0.0_SP
          smoothrB    = 0.0_SP
         ELSE IF (FABM_ALL(IA,K,IFABM).LT.1.E-10.AND.FABM_ALL(IB,K,IFABM).LT.1.E-10) THEN
          smoothrA    = 0.0_SP
          smoothrB    = 0.0_SP
         ELSE
!         FAB_upstreamA is the "far upstream node", FABM_ALL(IA,K) is upstream of FABM_ALL(IB,K)
          smoothrA    = (FABM_ALL(IA,K,IFABM)-FAB_upstreamA)/(FABM_ALL(IB,K,IFABM)-FABM_ALL(IA,K,IFABM))
          smoothrB    = (FABM_ALL(IB,K,IFABM)-FAB_upstreamB)/(FABM_ALL(IA,K,IFABM)-FABM_ALL(IB,K,IFABM))
       END IF

!     The Flux-limiter 
!      (superbee)
       A_limiter = MAX(0.0_SP, MIN(1.0_SP, 2.0_SP*smoothrA), MIN(2.0_SP, smoothrA))
       B_limiter = MAX(0.0_SP, MIN(1.0_SP, 2.0_SP*smoothrB), MIN(2.0_SP, smoothrB))

!      (minmod)
!       A_limiter = MAX(0.0_SP,MIN(1.0_SP,smoothrA))
!       B_limiter = MAX(0.0_SP,MIN(1.0_SP,smoothrB))

!      (To check if the upwind scheme is working)
!       A_limiter = 0.0_SP;
!       B_limiter = 0.0_SP;

!     Calculating the tracer-concentration at the cell-wall in-between IA and IB
       FAB12_A     = FABM_ALL(IA,K,IFABM)+0.5_SP*A_LIMITER*(FABM_ALL(IB,K,IFABM)-FABM_ALL(IA,K,IFABM))
       FAB12_B     = FABM_ALL(IB,K,IFABM)+0.5_SP*B_LIMITER*(FABM_ALL(IA,K,IFABM)-FABM_ALL(IB,K,IFABM))

! TVD done.
# endif

        UN=UVN(I,K)
#      if defined (SEMI_IMPLICIT)
       UN1=UVN1(I,K)
#      endif

        !VISCOF=HORCON*(FACT*(VISCOFF(IA)+VISCOFF(IB))*0.5_SP + FM1)
        
        ! David moved HPRNU and added HVC
       VISCOF=(FACT*0.5_SP*(VISCOFF(IA)*NN_HVC(IA)+VISCOFF(IB)*NN_HVC(IB)) + FM1*0.5_SP*(NN_HVC(IA)+NN_HVC(IB)))
!*******************************Jadon*************************************
! ---- Could be useful in large implementations with shelf breaks -----------------
!       VISCOF=(FACT*0.5_SP*(VISCOFF(IA)*NN_HVC(IA)+VISCOFF(IB)*NN_HVC(IB)) + FM1*0.5_SP*(NN_HVC(IA)+NN_HVC(IB)))
!     if (min(h(ia),h(ib))>800.0) then
!     elseif (min(h(ia),h(ib))<200.0) then
!       VISCOF=VISCOF*0.05
!     else
!       VISCOF=VISCOF*(1-(800.0-min(h(ia),h(ib)))/600.0*0.95) 
!     endif
!*****************************************************************************
        TXX=0.5_SP*(PFPXD(IA)+PFPXD(IB))*VISCOF
        TYY=0.5_SP*(PFPYD(IA)+PFPYD(IB))*VISCOF

        FXX=-DTIJ(I,K)*TXX*DLTYE(I)
        FYY= DTIJ(I,K)*TYY*DLTXE(I)

#       if !defined (SEMI_IMPLICIT)
#       if !defined (TVD)
        EXFLUX=-UN*DTIJ(I,K)* &
             ((1.0_SP+SIGN(1.0_SP,UN))*FIJ2+(1.0_SP-SIGN(1.0_SP,UN))*FIJ1)*0.5_SP+FXX+FYY
#       else
        EXFLUX=-UN*DTIJ(I,K)* &
             ((1.0_SP+SIGN(1.0_SP,UN))*FAB12_B+(1.0_SP-SIGN(1.0_SP,UN))*FAB12_A)*0.5_SP+FXX+FYY
#       endif
#       else
        EXFLUX=-UN*DTIJ(I,K)* &
           ((1.0_SP+SIGN(1.0_SP,UN))*FIJ2+(1.0_SP-SIGN(1.0_SP,UN))*FIJ1)*0.5_SP
        EXFLUX=(1.0_SP-IFCETA)*EXFLUX+IFCETA*(-UN1*DTIJ1(I,K)*((1.0_SP+SIGN(1.0_SP,UN1))*FIJ2+(1.0_SP-SIGN(1.0_SP,UN1))*FIJ1)*0.5_SP)+FXX+FYY
#       endif

        XFLUX(IA,K,IFABM)=XFLUX(IA,K,IFABM)+EXFLUX
        XFLUX(IB,K,IFABM)=XFLUX(IB,K,IFABM)-EXFLUX

        XFLUX_ADV(IA,K,IFABM)=XFLUX_ADV(IA,K,IFABM)+(EXFLUX-FXX-FYY)
        XFLUX_ADV(IB,K,IFABM)=XFLUX_ADV(IB,K,IFABM)-(EXFLUX-FXX-FYY)

     END DO ! I, NCV_I loop

#    if defined (SPHERICAL)
#    if !defined (SEMI_IMPLICIT)
       CALL ADV_S_XY(XFLUX(:,:,IFABM),XFLUX_ADV(:,:,IFABM),PFPX,PFPY,PFPXD,PFPYD,VISCOFF,K,0.0_SP)
#    else
       CALL ADV_S_XY(XFLUX(:,:,IFABM),XFLUX_ADV(:,:,IFABM),PFPX,PFPY,PFPXD,PFPYD,VISCOFF,K,IFCETA)
#    endif
#    endif

  END DO !!SIGMA LOOP K, KBM1
 END DO !to size(model%interior_state_variables)

  !
  !-Accumulate Fluxes at Boundary Nodes
  !
# if defined (MULTIPROCESSOR)
 DO IFABM=1,size(model%interior_state_variables)
  IF(PAR)CALL NODE_MATCH(0,NBN,BN_MLT,BN_LOC,BNC,MT,KB,MYID,NPROCS,XFLUX(:,:,IFABM),XFLUX_ADV(:,:,IFABM))
 END DO
# endif

 DO IFABM=1,size(model%interior_state_variables)
  DO K=1,KBM1
     IF(IOBCN > 0) THEN
        DO I=1,IOBCN
           I1=I_OBC_N(I)
           XFLUX_OBCB(I,K,IFABM)=XFLUX_ADV(I1,K,IFABM)
        END DO
     END IF
  END DO
 END DO

 DO IFABM=1,size(model%interior_state_variables)

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


  IF (.NOT. model%interior_state_variables(IFABM)%no_river_dilution) THEN
      IF(RIVER_TS_SETTING == 'calculated') THEN
         IF(RIVER_INFLOW_LOCATION == 'node') THEN
            IF(NUMQBC > 0) THEN
               DO J=1,NUMQBC
                  JJ=INODEQ(J)
                  STPOINT=FABMDIS(J,IFABM)+OFFS(1,1)
                  DO K=1,KBM1
                     FABM_FRESH(JJ,K)=FABMDIS(J,IFABM) ! Not sure this is correct
                     XFLUX(JJ,K,IFABM)=XFLUX(JJ,K,IFABM) - QDIS(J)*VQDIST(J,K)*STPOINT
                  END DO
               END DO
            END IF
         ELSE IF(RIVER_INFLOW_LOCATION == 'edge') THEN
            IF(NUMQBC > 0) THEN
               DO J=1,NUMQBC
                  J1=N_ICELLQ(J,1)
                  J2=N_ICELLQ(J,2)
                  STPOINT=FABMDIS(J,IFABM)+OFFS(1,1)!!ASK LIU SHOULD THIS BE STPOINT1(J1)/STPOINT2(J2)
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
    ELSE
        ! we don't want this variable to be diluted with 0 concentration
        IF (DBG_SET(DBG_VRB)) WRITE (IPT,*) "FABM Variable "//TRIM(model%interior_state_variables(IFABM)%name)//" is not in river forcing file and will not be diluted"
    END IF
 !


  ! The horizontal term of advection is neglected here for Open boundary locations
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

         ! Total (horizontal + vertical) flux to the cell
           ZZZFLUX(I,K) = TEMP*(DTI/DT(I))/DZ(I,K) + XFLUX(I,K,IFABM)/ART1(I)*(DTI/DT(I))/DZ(I,K) 
         ! Updated variable without limiter minus current variable
           XXXX = ZZZFLUX(I,K)*DT(I)/DTFA(I)+FABM_ALL(I,K,IFABM)-FABM_ALL(I,K,IFABM)*DT(I)/DTFA(I) 

!lwang for MPDATA 0318
           ! Added by Akvaplan 2018 to avoid single precision-crash
         IF((ABS(XXXX).LT.ABS(WQMMAX-FABM_ALL(I,K,IFABM))).AND.(XXXX.LT.0.0_SP)) THEN
            WQM_SF(I,K) = FABM_ALL(I,K,IFABM)-XXXX
         ELSE IF((ABS(XXXX).LT.ABS(WQMMIN-FABM_ALL(I,K,IFABM))).AND.(XXXX.GT.0.0_SP)) THEN 
            WQM_SF(I,K) = FABM_ALL(I,K,IFABM)-XXXX
         ELSE IF(XXXX.EQ.0) THEN
            WQM_SF(I,K) = FABM_ALL(I,K,IFABM)
         ELSE
!lwang
           BETA(I,K)=0.5*(1.-SIGN(1._SP,XXXX)) * (WQMMAX-FABM_ALL(I,K,IFABM))/(ABS(XXXX)+1.E-10) &
                +0.5*(1.-SIGN(1._SP,-XXXX)) * (FABM_ALL(I,K,IFABM)-WQMMIN)/(ABS(XXXX)+1.E-10)

           WQM_SF(I,K)=FABM_ALL(I,K,IFABM)-MIN(1._SP,BETA(I,K))*XXXX

         END IF
#    if defined (WET_DRY)
        END IF
#    endif
     END DO !! M LOOP
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
              WWWS(I,K)=0._SP 
           END IF
        END DO
     END DO
     DO I=1,M
        WWWS(I,1)=0._SP
     END DO

     DO I=1,M
        WQMMAX = MAXVAL(FABM_ALL(NBSN(I,1:NTSN(I)),1,IFABM))
        WQMMIN = MINVAL(FABM_ALL(NBSN(I,1:NTSN(I)),1,IFABM))
        WQMMAX = MAX(WQMMAX,FABM_ALL(I,2,IFABM),FABM_ALL(I,1,IFABM),FABM_FRESH(I,1)) ! What if I is a river node?
        WQMMIN = MIN(WQMMIN,FABM_ALL(I,2,IFABM),FABM_ALL(I,1,IFABM),FABM_FRESH(I,1))

        TEMP=0.5*((WWWS(I,2)+ABS(WWWS(I,2)))*WQM_S(I,2))*(DTI/DTFA(I))/DZ(I,1)
        BETAIN(I,1)=(WQMMAX-WQM_S(I,1))/(TEMP+1.E-10)

        TEMP=0.5*((WWWS(I,1)+ABS(WWWS(I,1)))*WQM_S(I,1)-        &
             (WWWS(I,2)-ABS(WWWS(I,2)))*WQM_S(I,1))*(DTI/DTFA(I))/DZ(I,1)
        BETAOUT(I,1)=(WQM_S(I,1)-WQMMIN)/(TEMP+1.E-10)

        WWWSF(I,1)=0.5*MIN(1.,BETAOUT(I,1))*(WWWS(I,1)+ABS(WWWS(I,1))) + &
             0.5*MIN(1.,BETAIN(I,1))*(WWWS(I,1)-ABS(WWWS(I,1)))
     END DO

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
!lwang for MPDATA 0316
  FABM_ALL(:,:,IFABM) = FABM_ALL(:,:,IFABM)-OFFS(:,:)
  WQM_SF = WQM_SF-OFFS
  ! RJT Should we consider FABM_MEAN here too?
!lwang
#  endif 
! MPDATA IF


# if ! defined(MPDATA)
# if defined (ONE_D_MODEL)
    XFLUX = 0.0_SP
# endif


  !--------------------------------------------------------------------
  !   The central difference scheme in vertical advection
  !--------------------------------------------------------------------
  DO I=1,M
#    if defined (WET_DRY)
     IF(ISWETN(I)*ISWETNT(I) == 1) THEN
#    endif

#       if defined (NH) || defined (LIMITER_VER_ADV)

#       endif

        DO K=1, KBM1

#          if defined (NH) || defined (LIMITER_VER_ADV)
#          endif

#          if !defined (NH) && !defined (LIMITER_VER_ADV)
           IF(K == 1) THEN
              TEMP=-WTS(I,K+1)*(FABM_ALL(I,K,IFABM)*DZ(I,K+1)+FABM_ALL(I,K+1,IFABM)*DZ(I,K))/   &
                   (DZ(I,K)+DZ(I,K+1))
           ELSE IF(K == KBM1) THEN
              TEMP= WTS(I,K)*(FABM_ALL(I,K,IFABM)*DZ(I,K-1)+FABM_ALL(I,K-1,IFABM)*DZ(I,K))/(DZ(I,K)+DZ(I,K-1))
           ELSE
              TEMP= WTS(I,K)*(FABM_ALL(I,K,IFABM)*DZ(I,K-1)+FABM_ALL(I,K-1,IFABM)*DZ(I,K))/(DZ(I,K)+DZ(I,K-1))-&
                   WTS(I,K+1)*(FABM_ALL(I,K,IFABM)*DZ(I,K+1)+FABM_ALL(I,K+1,IFABM)*DZ(I,K))/(DZ(I,K)+DZ(I,K+1))
           END IF
#          endif

           IF(ISONB(I) == 2) THEN
              !         XFLUX(I,K,IFABM)=TEMP*ART1(I)/DZ(I,K)
              XFLUX(I,K,IFABM)=TEMP*ART1(I)
           ELSE
              !         XFLUX(I,K,IFABM)=XFLUX(I,K,IFABM)+TEMP*ART1(I)/DZ(I,K)
              XFLUX(I,K,IFABM)=XFLUX(I,K,IFABM)+TEMP*ART1(I)
#          if defined (THIN_DAM)
#          endif

           END IF
        ENDDO
#    if defined (WET_DRY)
     END IF
#    endif
  END DO  !! SIGMA LOOP

  !
  !--Set Boundary Conditions-For Fresh Water Flux--------------------------------!
  !
  ! Check if variable will dilute
  IF (.NOT. model%interior_state_variables(IFABM)%no_river_dilution) THEN
      IF(RIVER_TS_SETTING == 'calculated') THEN
         IF(RIVER_INFLOW_LOCATION == 'node') THEN
            IF(NUMQBC > 0) THEN
               DO J=1,NUMQBC
                  JJ=INODEQ(J)
                  STPOINT=FABMDIS(J,IFABM) ! river concentration in original units mass/vol
                  DO K=1,KBM1
                     !             XFLUX(JJ,K,IFABM)=XFLUX(JJ,K,IFABM) - QDIS(J)*VQDIST(J,K)*STPOINT/DZ(JJ,K)
                     XFLUX(JJ,K,IFABM)=XFLUX(JJ,K,IFABM) - QDIS(J)*VQDIST(J,K)*STPOINT
                  END DO
               END DO
            END IF
         ELSE IF(RIVER_INFLOW_LOCATION == 'edge') THEN
            IF(NUMQBC > 0) THEN
               DO J=1,NUMQBC
                  J1=N_ICELLQ(J,1)
                  J2=N_ICELLQ(J,2)
                  STPOINT=FABMDIS(J,IFABM) !! river concentration in original units M/L3
                  DO K=1,KBM1
                     !             XFLUX(J1,K,IFABM)=XFLUX(J1,K,IFABM)-   &
                     !                         QDIS(J)*RDISQ(J,1)*VQDIST(J,K)*STPOINT/DZ1(J1,K)
                     !             XFLUX(J2,K,IFABM)=XFLUX(J2,K,IFABM)-   &
                     !                         QDIS(J)*RDISQ(J,2)*VQDIST(J,K)*STPOINT/DZ1(J2,K)
                     XFLUX(J1,K,IFABM)=XFLUX(J1,K,IFABM)-QDIS(J)*RDISQ(J,1)*VQDIST(J,K)*STPOINT
                     XFLUX(J2,K,IFABM)=XFLUX(J2,K,IFABM)-QDIS(J)*RDISQ(J,2)*VQDIST(J,K)*STPOINT
                  END DO
               END DO
            END IF
         END IF
      END IF
  ELSE
    ! we don't want this variable to be diluted with 0 concentration
    IF (DBG_SET(DBG_VRB)) WRITE (IPT,*) "FABM Variable "//TRIM(model%interior_state_variables(IFABM)%name)//" is not in river forcing file and will not be diluted"
  END IF
! lwang added for groundwater calculated method
  !
  !--Set Boundary Conditions-For ground water
  !Flux--------------------------------!
  ! Uncomment when added to namelist
!RJT  IF(GROUNDWATER_FABM_SETTING == 'calculated') THEN
!RJT      DO J=1,M
!RJT         IF (BFWDIS(J) /= 0.0_SP) THEN     
!RJT            IF (BFWFABM(J,IFABM)>0.0_SP) THEN
!RJT               STPOINT=BFWFABM(J,IFABM)
!RJT                DO K=1,KBM1
!RJT                    XFLUX(J,K,IFABM)=XFLUX(J,K,IFABM)-BFWDIS(J)*STPOINT*BFWFRAC(K) ! 1029
!RJT                END DO
!RJT!LWANG            ELSE 
!RJT!LWANG               STPOINT=FABM_ALL(J,KBM1,IFABM)
!RJT!LWANG               XFLUX(J,KBM1,IFABM)=XFLUX(J,KBM1,IFABM)-BFWDIS(J)*STPOINT
!RJT            END IF
!RJT         END IF
!RJT      END DO
!RJT  END IF
! lwang
!---------------------------------------------------------------------
# endif 
! End of ! defined(MPDATA)

  !--Update Variables------------------------------------------------------------!
  !
#  if defined (WET_DRY)
  DO I=1,M
     IF(ISWETN(I)*ISWETNT(I) == 1 )THEN
        DO K=1,KBM1
#    if !defined (MPDATA)      
! why is the line below commented out? Surely we need to update FABM_F if not using MPDATA? RJT2021
! K. Lettmann, 2022, Aug: set the comment sign again for the line below
!           FABM_F(I,K,IFABM)=(FABM_ALL(I,K,IFABM)-XFLUX(I,K,IFABM)/ART1(I)*(DTI/(DT(I)*DZ(I,K))))*(DT(I)/DTFA(I))
#      if !defined (SEMI_IMPLICIT)

#      if !defined (THIN_DAM)
           FABM_F(I,K,IFABM)=(FABM_ALL(I,K,IFABM)-XFLUX(I,K,IFABM)/ART1(I)*(DTI/(DT(I)*DZ(I,K))))*(DT(I)/DTFA(I))
#      else
#      endif

#      else  
           FABM_F(I,K,IFABM)=(TSC_FABM_ALL(I,K,IFABM)-RK_FABM(STAGE)*XFLUX(I,K,IFABM)/ART1(I)*(DTI/(DT(I)*DZ(I,K))))*(DT(I)/DTFA(I)) !syshi 20191013
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
           FABM_F(I,K,IFABM)=TSC_FABM_ALL(I,K,IFABM) !syshi 20191013
#      endif
        END DO
     END IF
  END DO

# else 
! WET-DRY

  DO I=1,M
     DO K=1,KBM1
#    if !defined (MPDATA)  
! why is the line below commented out? Surely we need to update FABM_F if not using MPDATA? RJT2021   
        FABM_F(I,K,IFABM)=(FABM_ALL(I,K,IFABM)-XFLUX(I,K,IFABM)/ART1(I)*(DTI/(DT(I)*DZ(I,K))))*(DT(I)/DTFA(I))
#      if !defined (SEMI_IMPLICIT)
! nothing happens if not SEMI-IMPLICIT? 
#      if !defined (THIN_DAM)
        FABM_F(I,K,IFABM)=(FABM_ALL(I,K,IFABM)-XFLUX(I,K,IFABM)/ART1(I)*(DTI/(DT(I)*DZ(I,K))))*(DT(I)/DTFA(I))
#      else
#      endif

#      else
! SEMI-IMPLICIT 
        FABM_F(I,K,IFABM)=(TSC_FABM_ALL(I,K,IFABM)-RK_FABM(STAGE)*XFLUX(I,K,IFABM)/ART1(I)*(DTI/(DT(I)*DZ(I,K))))*(DT(I)/DTFA(I)) !syshi 20191013
#      endif
#    else
! IF MPDATA
        FABM_F(I,K,IFABM)=WQM_SF(I,K)
#    endif              
     END DO

  END DO

# endif 
! WET-DRY

  END DO !do IFABM=1,size(model%interior_state_variables)

  IF(DBG_SET(DBG_SBR)) WRITE(IPT,*)"End: FABM_ADV"
END SUBROUTINE FABM_ADV

! FABM_ADV_VEC
!==============================================================================|

  SUBROUTINE FABM_BCOND_PREPARE()
    USE MOD_OBCS

    INTEGER :: I,J

    IF(IOBCN > 0) THEN
      DO I=1,IOBCN
        ! Discard the impact of advection and horizontal diffusion, as the reference boundary state
        ! is determined by water column simulation (sources, sinking, vertical mixing) only.
        ! In the case of outflow over the boundary, the impact of advection will be added by FABM_BCOND.
        J=I_OBC_N(I)
        FABM_F(J,1:KBM1,:) = FABM_ALL(J,1:KBM1,:)
      END DO
    END IF
  END SUBROUTINE FABM_BCOND_PREPARE

!==============================================================================!
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
!DIR$ ATTRIBUTES NOINLINE :: FABM_BCOND

  IF(DBG_SET(DBG_SBR)) WRITE(IPT,*)"Start: FABM_BCOND :"

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
                 DO IFABM=1,size(model%interior_state_variables)
                  IF (.NOT. model%interior_state_variables(IFABM)%no_river_dilution) THEN
                     FABM_F(J11,K,IFABM) = FABMDIS(I,IFABM)
                  END IF
                 END DO
               END DO
             END DO
           ELSE IF(RIVER_INFLOW_LOCATION == 'edge') THEN
             DO I=1,NUMQBC
               J11=N_ICELLQ(I,1)
               J22=N_ICELLQ(I,2)
               DO K=1,KBM1
                 DO IFABM=1,size(model%interior_state_variables)
                   IF (.NOT. model%interior_state_variables(IFABM)%no_river_dilution) THEN
                     FABM_F(J11,K,IFABM)=FABMDIS(I,IFABM)
                     FABM_F(J22,K,IFABM)=FABMDIS(I,IFABM)
                   END IF
                 END DO
               END DO
             END DO
           END IF ! IF RIVER_INFLOW_LOCATION
         END IF ! If NUMQBC > 0
      END IF ! If RIVER_TS == specified

   ! lwang added for fabm point source
   ! Update concentrations at point source nodes.
!   IF(GROUNDWATER_ON) THEN
!      IF (GROUNDWATER_FABM_SETTING == 'specified') THEN
!         DO I=1,MT
!            IF (BFWDIS(I) /= 0.0_SP) THEN
!               DO IFABM=1,size(model%state_variables)
!                  IF (BFWFABM(I,IFABM)>0) THEN
!                     FABM_F(I,KBM1,IFABM)=BFWFABM(I,IFABM)
!                  END IF
!               END DO
!            END IF
!         END DO
!      END IF
!   END IF
   ! lwang

   ! Update concentrations at open boundary nodes, if nudging to boundary state is active.
   IF(OBC_FABM_NUDGING) CALL UPDATE_OBC_FABM(IntTime,FABM_OBC)

   IF(IOBCN > 0) THEN
!
!  SET CONDITIONS ON OUTER BOUNDARY
! Variables presence is a function of variable presence in NETCDF file
! different handling is only needed when tracers come into the domain
     RAMP_BIO = TANH(FLOAT(IINT)/FLOAT(IRAMP+1))
     DO IFABM=1,size(model%interior_state_variables)
       DO I=1,IOBCN
         J=I_OBC_N(I)
         IF(UARD_OBCN(I) > 0.0_SP) THEN
           ! Outward flow over open boundary. Imposed boundary condition has no effect.
           J1=NEXT_OBC(I)

           ! Depth-average mass at current boundary node and its interior neighbor (T2D, T2D_NEXT),
           ! as well as mass flux over the boundary (XFLUX2D).
           ! Note that the affected state FABM_F at the boundary has been modified by FABM_BCOND_PREPARE
           ! to include only biogeochemical sources, residual vertical movement [e.g., sinking], and vertical mixing!
           ! Therefore, while T2D_NEXT is the vertical average of the neighboring field including advection,
           ! T2D is the vertical average of the boundary field excluding advection. The impact of advection is to
           ! be accounted for here.
           T2D=0.0_SP
           T2D_NEXT=0.0_SP
           XFLUX2D=0.0_SP
           DO K=1,KBM1
            T2D=T2D+FABM_F(J,K,IFABM)*DZ(J,K)
!           T2D_NEXT=T2D_NEXT+FABM_F(J1,K,IFABM)*DZ(J1,K) ! RJT ORIGINAL similar
!           to mod_bio_3D.F
            T2D_NEXT=T2D_NEXT+FABM_F(J1,K,IFABM)*DZ(J,K)  ! for Mass preserving we need DZ at boundary
            XFLUX2D=XFLUX2D+XFLUX_OBCB(I,K,IFABM)           !XFLUX_OBCB is set in ADVECTION routine
           END DO

           ! Add flux across open boundary to existing flux for this node [which presumably did not yet
           ! include the flux over the boundary itself]
           TMP=XFLUX2D+T2D*UARD_OBCN(I)

           ! Forecast depth-averaged tracer at the boundary based on previous value and flux TMP.
!lwang
!lwang           T2D_OBC=(T2D*DT(J)-TMP*DTI/ART1(J))/D(J)
                 T2D_OBC=(T2D*DT(J)-TMP*DTI/ART1(J))/DTFA(J)
!lwang

           ! For the new depth-explicit value at the boundary, combine the forecast depth-averaged value
           ! with the differences from the the depth-averaged value as forecast for the neighbouring node.
           ! Only do this if the tracer does not equal zero across the water column (otherwise the result
           ! is NaN).
           IF (T2D_NEXT /= 0) THEN
            DO K=1,KBM1
!              FABM_F(J,K,IFABM)=T2D_OBC+(FABM_F(J1,K,IFABM)-T2D_NEXT)  !
!              Original code similar to mod_bio_3D.F
             ! Mass Preserving implementation
             FABM_F(J,K,IFABM) = FABM_F(J1,K,IFABM)*(T2D_OBC / T2D_NEXT) ! proportionality per layer. T2 have concentrations units
             ! Other alternatives 
!             FABM_F(J,K,IFABM) = FABM_F(J1,K,IFABM) ! See bcon_scal_OBC.F for this crazyness
            END DO
           ELSE
            FABM_F(J, 1:KBM1,IFABM) = T2D_OBC
           END IF

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
           ! Inward flux over open boundary. Optionally nudge boundary state towards prescribed values.
           ! Note that the affected boundary state FABM_F has been modified by FABM_BCOND_PREPARE
           ! to include only biogeochemical sources, residual vertical movement [e.g., sinking], and vertical mixing!
           ! It specifically excludes advection and horizontal diffusion - effectively we run water column simulations at the boundary.
           IF(FABM_OBC_PRESCRIBED(IFABM)) THEN
             ! This variable has a prescribed boundary value. Nudge current state towards it.
             DO K=1,KBM1
               FABM_F(J,K,IFABM) = FABM_F(J,K,IFABM) - OBC_FABM_NUDGING_TIMESCALE*RAMP_BIO*(FABM_F(J,K,IFABM) - FABM_OBC(I,K,IFABM))
             END DO
           END IF
         END IF
       END DO
     END DO !!OUTER LOOP OVER FABM VARIABLES IFABM=1,size(model%interior_state_variables)

   END IF

!
!--SET BOUNDARY CONDITIONS-----------------------------------------------------|
!
       FABM_ALL(0,:,:)=ZERO ! don't know what this is doing here
   IF(DBG_SET(DBG_SBR)) WRITE(IPT,*)"End: FABM_BCOND :"

   RETURN

  END SUBROUTINE FABM_BCOND
!==============================================================================!

  SUBROUTINE fvcom_driver_fatal_error(self,location,message)
     CLASS (type_fvcom_driver), INTENT(INOUT) :: self
     CHARACTER(LEN=*),          INTENT(IN)    :: location,message
    
     WRITE (IPT,*) 'FABM '//TRIM(location)//': '//TRIM(message)
     CALL FATAL_ERROR('FABM '//TRIM(location)//': '//TRIM(message))
  END SUBROUTINE

  SUBROUTINE fvcom_driver_log_message(self,message)
     CLASS (type_fvcom_driver), INTENT(INOUT) :: self
     CHARACTER(LEN=*),          INTENT(IN)    :: message
     WRITE (IPT,'("MYID=",I0,": ",A)') MYID,TRIM(message)
  END SUBROUTINE

  FUNCTION fvcom_driver_describe_location(self,location) RESULT(string)
     CLASS (type_fvcom_driver), INTENT(IN) :: self
     INTEGER,                   INTENT(IN) :: location(2)

     CHARACTER(LEN=256) :: string

     INTEGER :: I,K,IFABM

     I = location(1)
     K = location(2)
     WRITE (string,'("I=",I0,",K=",I0," (LON=",G0.6,",LAT=",G0.6,",Z=",G0.4," m,DZ=",G0.4," m, H=",G0.4," m, WET =",I0,", TOTAL_DEPTH =",G0.4," m)")') &
            NGID(I),K,LON(I),LAT(I),-ZZ(I,K)*D(I),DZ(I,K)*D(I),H(I),ISWETN(I),D(I)
     IF (PRINT_STATE) THEN 

       WRITE (IPT,'("  ",A,": ",G12.5)') 'TEMPERATURE',T1(I,K)
       WRITE (IPT,'("  ",A,": ",G12.5)') 'SALINITY',S1(I,K)
       WRITE (IPT,'("  ",A,": ",G12.5)') 'DENSITY',DENSITY(I,K)
       WRITE (IPT,'("  ",A,": ",G12.5)') 'CELL_THICKNESS',CELL_THICKNESS(I,K)
       WRITE (IPT,'("  ",A,": ",G12.5)') 'PRESSURE',PRESSURE(I,K)
       WRITE (IPT,'("  ",A,": ",G12.5)') 'BOTTOM_STRESS',BOTTOM_STRESS(I)
       WRITE (IPT,'("  ",A,": ",G12.5)') 'WIND_SPEED',WIND_SPEED(I)
       WRITE (IPT,'("  ",A,": ",G12.5)') 'surface_downwelling_shortwave_flux',SWRAD_WATTS(I)
       WRITE (IPT,'("  ",A,": ",G12.5)') 'LATITUDE',LAT(I)
       WRITE (IPT,'("  ",A,": ",G12.5)') 'LONGITUDE',LON(I)
      IF (COMPUTE_DAYS_SINCE_YEAR_START) THEN
                WRITE (IPT,'("  ",A,": ",G12.5)') 'number_of_days_since_start_of_the_year', DAYS_SINCE_YEAR_START
      END IF


       DO IFABM=1,size(model%interior_state_variables)
         WRITE (IPT,'("  ",A,": ",G12.5)') TRIM(model%interior_state_variables(IFABM)%name),FABM_F(I,K,IFABM)
       END DO
 
       DO IFABM=1,size(model%interior_state_variables)
         WRITE (IPT,'("  ",A,": ",G12.5)') TRIM(model%interior_state_variables(IFABM)%name),FABM_ALL(I,K,IFABM)
       END DO
     
       DO IFABM=1,size(model%bottom_state_variables)
         WRITE (IPT,'("  ",A,": ",G12.5)') TRIM(model%bottom_state_variables(IFABM)%name),FABM_ALL_BT(I,IFABM)
       END DO
     END IF
     
  END FUNCTION

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
!DIR$ ATTRIBUTES NOINLINE :: FCT_FABM

  IF(DBG_SET(DBG_SBR)) WRITE(IPT,*)"Start: fct_FABM"

  fabm: DO IFABM=1,size(model%interior_state_variables)
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
                (FABM_ALL(I,K,IFABM)*DZ(I,K+1)+FABM_ALL(I,K+1,IFABM)*DZ(I,K))/           &
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

SUBROUTINE INITIALIZE_FABM_INPUTS()
   CHARACTER(LEN=attribute_length) :: VARIABLE_NAME
   CHARACTER(LEN=80) FILE, NC_VARIABLE_NAME
   NAMELIST /NML_FABM_INPUT/ VARIABLE_NAME, FILE, NC_VARIABLE_NAME
   INTEGER :: ios, status, ntimes
   INTEGER, PARAMETER :: FABM_INPUT_UNIT = 900
   LOGICAL :: FOUND
   TYPE(NCFILE), POINTER :: NCF_TMP
   TYPE(NCDIM),  POINTER :: DIM
   TYPE(NCVAR),  POINTER :: VAR
   TYPE(TIME) :: TIMETEST
   REAL(SP), POINTER :: STORAGE_VEC(:)
   REAL(SP), POINTER :: STORAGE_ARR(:,:)
   TYPE (TYPE_FABM_INPUT), POINTER :: FABM_INPUT
   CHARACTER(LEN=*),PARAMETER :: NMLPATH = 'fabm_input.nml'

   INQUIRE(FILE=NMLPATH,EXIST=FOUND)
   IF (.NOT.FOUND) RETURN
   CALL FOPEN(FABM_INPUT_UNIT,NMLPATH,'cfr')

   DO
      ! Initialize namelist variables with default value.
      VARIABLE_NAME = ''
      NC_VARIABLE_NAME = ''
      FILE = ''

      ! Read namelist
      READ(UNIT=FABM_INPUT_UNIT, NML=NML_FABM_INPUT,IOSTAT=ios)
      IF (IOS>0) CALL FATAL_ERROR("ERROR READING "//TRIM(NMLPATH))
      if (IOS<0) exit

      IF (VARIABLE_NAME=='') CALL FATAL_ERROR(TRIM(nmlpath)//": VARIABLE_NAME MUST BE PROVIDED")
      IF (FILE=='') CALL FATAL_ERROR(TRIM(nmlpath)//": VARIABLE_NAME MUST BE PROVIDED")
      IF (NC_VARIABLE_NAME=='') NC_VARIABLE_NAME = get_safe_name(VARIABLE_NAME)

      ALLOCATE(FABM_INPUT)
      FABM_INPUT%FABM_ID_HZ = model%get_horizontal_variable_id(VARIABLE_NAME)
      IF (.NOT. model%is_variable_used(FABM_INPUT%FABM_ID_HZ))THEN
         FABM_INPUT%FABM_ID = model%get_interior_variable_id(VARIABLE_NAME)
         IF (.NOT. model%is_variable_used(FABM_INPUT%FABM_ID))THEN
            CALL FATAL_ERROR &
               & ("VARIABLE "//TRIM(VARIABLE_NAME)//" REFERENCED IN "//TRIM(nmlpath)//" NOT FOUND IN FABM MODEL")
         ELSE
            FABM_INPUT%TWOD=.FALSE.
         ENDIF
      ELSE
         FABM_INPUT%TWOD=.TRUE.
      ENDIF

      ! Test extension of specified data file.
      if (INDEX(FILE,".nc",.true.)/=LEN_TRIM(FILE)-2)&
         & CALL WARNING("Name of forcing file for FABM variable "//trim(VARIABLE_NAME)//" does not end in .nc", &
         & TRIM(FILE))

      ! Compose full path and initialize NetCDF file object (will use existing open file if available).
      FABM_INPUT%PATH = TRIM(INPUT_DIR)//TRIM(FILE)
      CALL NC_INIT(FABM_INPUT%FILE,FABM_INPUT%PATH)
      If(.not. FABM_INPUT%FILE%OPEN) then
         Call NC_OPEN(FABM_INPUT%FILE)
         CALL NC_LOAD(FABM_INPUT%FILE)
         ! MAKE A TEMPORARY POINTER TO ADD THE FILE TO THE LIST ("ADD" NULLIFIES THE FILE POINTER IT RECEIVES)
         NCF_TMP => FABM_INPUT%FILE
         FILEHEAD => ADD(FILEHEAD,NCF_TMP)
      end if

      ! Get time dimension
      DIM => FIND_UNLIMITED(FABM_INPUT%FILE,FOUND)
      IF(.not. FOUND) CALL FATAL_ERROR &
         & ("IN FILE OBJECT WITH FORCING DATA FOR FABM VARIABLE "//TRIM(VARIABLE_NAME),&
         & "FILE NAME: "//TRIM(FILE),&
         &"COULD NOT FIND THE UNLIMITED DIMENSION")

      NTIMES = DIM%DIM

      ! CHECK THE FILE TIME AND COMPARE WITH MODEL RUN TIME
      TIMETEST = get_file_time(FABM_INPUT%FILE,1)
      IF(TIMETEST > STARTTIME) CALL FATAL_ERROR &
         & ("IN FILE OBJECT WITH FORCING DATA FOR FABM VARIABLE "//TRIM(VARIABLE_NAME),&
         & "FILE NAME: "//TRIM(FILE),&
         &"THE MODEL RUN STARTS BEFORE THE FORCING TIME SERIES")

      TIMETEST = get_file_time(FABM_INPUT%FILE,ntimes)
      IF(TIMETEST < ENDTIME) CALL FATAL_ERROR &
         & ("IN FILE OBJECT WITH FORCING DATA FOR FABM VARIABLE "//TRIM(VARIABLE_NAME),&
         & "FILE NAME: "//TRIM(FILE),&
         &"THE MODEL RUN ENDS AFTER THE FORCING TIME SERIES")

      DIM => FIND_DIM(FABM_INPUT%FILE,'node',FOUND)
      IF(.NOT.FOUND) CALL FATAL_ERROR &
         & ("IN FILE OBJECT WITH FORCING DATA FOR FABM VARIABLE "//TRIM(VARIABLE_NAME),&
         & "FILE NAME: "//TRIM(FILE),&
         & "COULD NOT FIND DIMENSION 'node'")

      if (mgl /= dim%dim) CALL FATAL_ERROR &
         & ("IN FILE OBJECT WITH FORCING DATA FOR FABM VARIABLE "//TRIM(VARIABLE_NAME),&
         & "FILE NAME: "//TRIM(FILE),&
         & "LENGTH OF 'node' DIMENSION DOES NOT MATCH NUMBER OF NODES IN FVCOM GRID")

      VAR => FIND_VAR(FABM_INPUT%FILE,TRIM(NC_VARIABLE_NAME),FOUND)
      IF(.NOT.FOUND) CALL FATAL_ERROR &
         & ("IN FILE OBJECT WITH FORCING DATA FOR FABM VARIABLE "//TRIM(VARIABLE_NAME),&
         & "FILE NAME: "//TRIM(FILE),&
         & "COULD NOT FIND VARIABLE '"//TRIM(NC_VARIABLE_NAME)//"'")

      IF(FABM_INPUT%TWOD)THEN

         ! MAKE SPACE FOR THE DATA FROM THE FILE
         ALLOCATE(STORAGE_VEC(0:MT), stat = status)
         IF(STATUS /= 0) CALL FATAL_ERROR("FAILED TO ALLOCATE MEMORY FOR READING FABM FORCING VARIABLE "//TRIM(VARIABLE_NAME))
         FABM_INPUT%N => reference_var(var)
         CALL NC_CONNECT_PVAR(FABM_INPUT%N,STORAGE_VEC)
         NULLIFY(STORAGE_VEC)

         ALLOCATE(STORAGE_VEC(0:MT), stat = status)
         IF(STATUS /= 0) CALL FATAL_ERROR("FAILED TO ALLOCATE MEMORY FOR READING FABM FORCING VARIABLE "//TRIM(VARIABLE_NAME))
         FABM_INPUT%P => reference_var(var)
         CALL NC_CONNECT_PVAR(FABM_INPUT%P,STORAGE_VEC)
         NULLIFY(STORAGE_VEC)

         ALLOCATE(FABM_INPUT%DATA_HZ(0:MT))
         FABM_INPUT%DATA_HZ = 0.0_SP
         CALL model%link_horizontal_data(FABM_INPUT%FABM_ID_HZ,FABM_INPUT%DATA_HZ(1:M),source=data_source_user)
      ELSE
         ! MAKE SPACE FOR THE DATA FROM THE FILE
         ALLOCATE(STORAGE_ARR(0:MT,KB), stat = status)
         IF(STATUS /= 0) CALL FATAL_ERROR("FAILED TO ALLOCATE MEMORY FOR READING FABM FORCING VARIABLE "//TRIM(VARIABLE_NAME))
         FABM_INPUT%N => reference_var(var)
         CALL NC_CONNECT_PVAR(FABM_INPUT%N,STORAGE_ARR)
         NULLIFY(STORAGE_ARR)

         ALLOCATE(STORAGE_ARR(0:MT,KB), stat = status)
         IF(STATUS /= 0) CALL FATAL_ERROR("FAILED TO ALLOCATE MEMORY FOR READING FABM FORCING VARIABLE "//TRIM(VARIABLE_NAME))
         FABM_INPUT%P => reference_var(var)
         CALL NC_CONNECT_PVAR(FABM_INPUT%P,STORAGE_ARR)
         NULLIFY(STORAGE_ARR)

         ALLOCATE(FABM_INPUT%DATA(0:MT,KB))
         FABM_INPUT%DATA = 0.0_SP
         CALL model%link_interior_data(FABM_INPUT%FABM_ID,FABM_INPUT%DATA(1:M,1:KB),source=data_source_user)
      ENDIF

      ! Prepend to list of inputs
      FABM_INPUT%NEXT => FIRST_FABM_INPUT
      FIRST_FABM_INPUT => FABM_INPUT
   END DO
END SUBROUTINE INITIALIZE_FABM_INPUTS

SUBROUTINE UPDATE_FABM_INPUTS(NOW)
   TYPE(TIME), INTENT(IN) :: NOW

   TYPE (TYPE_FABM_INPUT), POINTER :: FABM_INPUT
   REAL(SP),               POINTER :: VNP(:), VPP(:)
   REAL(SP),               POINTER :: VNP_3D(:,:), VPP_3D(:,:)
   TYPE(NCFTIME),          POINTER :: FTM
   INTEGER                         :: STATUS

   FABM_INPUT => FIRST_FABM_INPUT
   DO WHILE (ASSOCIATED(FABM_INPUT))
      FTM => FABM_INPUT%FILE%FTIME

      CALL UPDATE_VAR_BRACKET(FABM_INPUT%FILE,FABM_INPUT%P,FABM_INPUT%N,NOW,STATUS)
      IF (STATUS /= 0) THEN
         CALL FATAL_ERROR("COULD NOT UPDATE TIME BRACKET FOR "//TRIM(FABM_INPUT%PATH)//": BOUNDS EXCEEDED?")
      end if

      IF(FABM_INPUT%TWOD)THEN
         CALL NC_POINT_VAR(FABM_INPUT%N,VNP)
         CALL NC_POINT_VAR(FABM_INPUT%P,VPP)
         FABM_INPUT%DATA_HZ = FTM%NEXT_WGHT * VNP + FTM%PREV_WGHT * VPP
      ELSE
         CALL NC_POINT_VAR(FABM_INPUT%N,VNP_3D)
         CALL NC_POINT_VAR(FABM_INPUT%P,VPP_3D)
         FABM_INPUT%DATA = FTM%NEXT_WGHT * VNP_3D + FTM%PREV_WGHT * VPP_3D
      ENDIF

      FABM_INPUT => FABM_INPUT%NEXT
   END DO
END SUBROUTINE UPDATE_FABM_INPUTS

SUBROUTINE CONFIGURE_OUTPUT()
   USE yaml_types
   USE yaml,yaml_parse=>parse,yaml_error_length=>error_length
   USE fabm_types,only: output_none, output_instantaneous

   CHARACTER(LEN=*),PARAMETER       :: PATH = 'fabm_output.yaml'
   LOGICAL                          :: FOUND
   CLASS (type_node),       POINTER :: node
   INTEGER, PARAMETER               :: yaml_unit = 901
   CHARACTER(LEN=yaml_error_length) :: yaml_error
   TYPE (type_error),       POINTER :: config_error
   CLASS (type_dictionary), POINTER :: variablemap
   TYPE (type_key_value_pair),POINTER :: pair
   LOGICAL                          :: IN_OUTPUT
   LOGICAL                          :: success

   INQUIRE(FILE=PATH,EXIST=FOUND)
   IF (.NOT.FOUND) RETURN

   WRITE (IPT,*) 'Reading FABM output configuration from '//PATH

   node => yaml_parse(PATH,yaml_unit,yaml_error)
   IF (yaml_error/='') CALL FATAL_ERROR(TRIM(yaml_error))
   IF (.not.ASSOCIATED(node)) CALL FATAL_ERROR('No configuration information found in '//PATH//'.')

   ! Disable output for all variables by default
   model%interior_state_variables(:)%output = output_none
   model%bottom_state_variables(:)%output = output_none
   model%surface_state_variables(:)%output = output_none
   model%interior_diagnostic_variables(:)%save = .false.
   model%horizontal_diagnostic_variables(:)%save = .false.

   SELECT TYPE (node)
   CLASS IS (type_dictionary)
      variablemap => node%get_dictionary('variables',required=.false.,error=config_error)
      if (associated(config_error)) CALL FATAL_ERROR(PATH//': '//TRIM(config_error%message))
      pair => variablemap%first
      DO WHILE (ASSOCIATED(pair))
         SELECT TYPE (value=>pair%value)
         CLASS IS (type_scalar)
            IN_OUTPUT = value%to_logical(.TRUE.,success)
            IF (.not.success) CALL FATAL_ERROR(PATH//': '//trim(value%path)//' is set to "'//trim(value%string)//'", which cannot be interpreted as a Boolean value.')
            CALL SELECT_FOR_OUTPUT(TRIM(pair%key),IN_OUTPUT)
         CLASS IS (type_null)
            CALL FATAL_ERROR(PATH//': '//TRIM(value%path)//' must be set to a Boolean value, not to null.')
         CLASS IS (type_dictionary)
            CALL FATAL_ERROR(PATH//': '//TRIM(value%path)//' must be set to a Boolean value, not to a dictionary.')
         END SELECT
         pair => pair%next
      END DO
   CLASS IS (type_node)
      CALL FATAL_ERROR(PATH//' should must a dictionary with key: value pairs at the root level, not a single value. Are you missing a trailing colon?')
   END SELECT

CONTAINS

   SUBROUTINE SELECT_FOR_OUTPUT(NAME,IN_OUTPUT)
      CHARACTER(LEN=*),INTENT(IN) :: NAME
      LOGICAL,         INTENT(IN) :: IN_OUTPUT

      CHARACTER(LEN=LEN(NAME)) :: NAME_
      LOGICAL :: FOUND
      INTEGER :: IFABM
      INTEGER :: IASTERISK
      INTEGER :: ISTOP
      INTEGER :: OUTPUT_FLAG

      IASTERISK = INDEX(NAME,'*')
      IF (IASTERISK/=0) THEN
         IF (IASTERISK/=LEN_TRIM(NAME)) CALL FATAL_ERROR(PATH//': invalid expression "'//TRIM(NAME)//'". The * symbol can only appear as last character.')
         ISTOP = IASTERISK-1
         NAME_ = NAME(1:ISTOP)
      ELSE
         ISTOP = attribute_length
         NAME_ = NAME
      END IF

      OUTPUT_FLAG = output_instantaneous
      IF (.NOT.IN_OUTPUT) OUTPUT_FLAG = output_none

      FOUND = .FALSE.
      DO IFABM=1,SIZE(model%interior_state_variables)
         IF (model%interior_state_variables(IFABM)%name(1:ISTOP)==NAME_) then
            model%interior_state_variables(IFABM)%output = OUTPUT_FLAG
!            WRITE (IPT,*) '- '//TRIM(model%interior_state_variables(IFABM)%name)//' =',IN_OUTPUT
            FOUND = .TRUE.
         END IF
      END DO
      DO IFABM=1,SIZE(model%bottom_state_variables)
         IF (model%bottom_state_variables(IFABM)%name(1:ISTOP)==NAME_) then
            model%bottom_state_variables(IFABM)%output = OUTPUT_FLAG
!            WRITE (IPT,*) '- '//TRIM(model%bottom_state_variables(IFABM)%name)//' =',IN_OUTPUT
            FOUND = .TRUE.
         END IF
      END DO
      DO IFABM=1,SIZE(model%surface_state_variables)
         IF (model%surface_state_variables(IFABM)%name(1:ISTOP)==NAME_) then
            model%surface_state_variables(IFABM)%output = OUTPUT_FLAG
!            WRITE (IPT,*) '- '//TRIM(model%surface_state_variables(IFABM)%name)//' =',IN_OUTPUT
            FOUND = .TRUE.
         END IF
      END DO
      DO IFABM=1,SIZE(model%interior_diagnostic_variables)
         IF (model%interior_diagnostic_variables(IFABM)%output/=output_none .and. model%interior_diagnostic_variables(IFABM)%name(1:ISTOP)==NAME_) then
            model%interior_diagnostic_variables(IFABM)%save = IN_OUTPUT
!            WRITE (IPT,*) '- '//TRIM(model%interior_diagnostic_variables(IFABM)%name)//' =',IN_OUTPUT
            FOUND = .TRUE.
         END IF
      END DO
      DO IFABM=1,SIZE(model%horizontal_diagnostic_variables)
         IF (model%horizontal_diagnostic_variables(IFABM)%output/=output_none .and. model%horizontal_diagnostic_variables(IFABM)%name(1:ISTOP)==NAME_) then
            model%horizontal_diagnostic_variables(IFABM)%save = IN_OUTPUT
!            WRITE (IPT,*) '- '//TRIM(model%horizontal_diagnostic_variables(IFABM)%name)//' =',IN_OUTPUT
            FOUND = .TRUE.
         END IF
      END DO
      IF (.NOT.FOUND) CALL FATAL_ERROR(PATH//': FABM variable with name "'//TRIM(NAME)//'" not found.')
   END SUBROUTINE

END SUBROUTINE

# endif

END MODULE MOD_FABM_3D
