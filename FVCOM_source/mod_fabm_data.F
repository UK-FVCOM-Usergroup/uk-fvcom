MODULE MOD_FABM_DATA
# if defined(FABM)

  USE fabm
  USE MOD_PREC

  IMPLICIT NONE

  PRIVATE

  TYPE (type_model), PUBLIC :: model

  REAL(SP), ALLOCATABLE, PUBLIC, TARGET ::  FABM_ALL(:,:,:)    ! 3D FABM state variables
  REAL(SP), ALLOCATABLE, PUBLIC, TARGET ::  FABM_ALL_BT(:,:)   ! 2D FABM bottom-attached state variables
  REAL(SP), ALLOCATABLE, PUBLIC, TARGET ::  FABM_ALL_SF(:,:)   ! 2D FABM surface-attached state variables
  REAL(DP), ALLOCATABLE, PUBLIC, TARGET ::  FABM_TOTALS(:)     ! Domain integrals of conserved quantities
  CHARACTER(LEN=80), PUBLIC :: STARTUP_FABM_TYPE  ! Initialization method ("constant" for spatially homogenous, "set values" for read from restart)
  LOGICAL, PUBLIC :: FABM_DIAG_OUT
#   if defined (OFFLINE_FABM)
  CHARACTER(LEN=80), PUBLIC :: OFFLINE_FABM_FILE  ! File with physics forcing for offline biogeochemistry simulation
#   endif
# endif

END MODULE