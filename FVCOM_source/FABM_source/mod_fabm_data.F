MODULE MOD_FABM_DATA

  USE fabm
  USE MOD_PREC

  IMPLICIT NONE

  PRIVATE

  TYPE (type_model), PUBLIC :: model

  REAL(SP), ALLOCATABLE, PUBLIC, TARGET ::  FABM_ALL(:,:,:)    ! 3D FABM state variables
  REAL(SP), ALLOCATABLE, PUBLIC, TARGET ::  FABM_ALL_BT(:,:)   ! 2D FABM bottom-attached state variables
  REAL(SP), ALLOCATABLE, PUBLIC, TARGET ::  FABM_ALL_SF(:,:)   ! 2D FABM surface-attached state variables
  REAL(SP), ALLOCATABLE, PUBLIC :: FABM_MEAN(:,:,:)            ! MEAN VARIABLES

END MODULE