!/===========================================================================/
! Copyright (c) 2007, The University of Massachusetts Dartmouth 
! Produced at the School of Marine Science & Technology 
! Marine Ecosystem Dynamics Modeling group
! All rights reserved.
!
! FVCOM has been developed by the joint UMASSD-WHOI research team. For 
! details of authorship and attribution of credit please see the FVCOM
! technical manual or contact the MEDM group.
!
! 
! This file is part of FVCOM. For details, see http://fvcom.smast.umassd.edu 
! The full copyright notice is contained in the file COPYRIGHT located in the 
! root directory of the FVCOM code. This original header must be maintained
! in all distributed versions.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
! AND ANY EXPRESS OR  IMPLIED WARRANTIES, INCLUDING,  BUT NOT  LIMITED TO,
! THE IMPLIED WARRANTIES OF MERCHANTABILITY AND  FITNESS FOR A PARTICULAR
! PURPOSE ARE DISCLAIMED.  
!
!/---------------------------------------------------------------------------/
! CVS VERSION INFORMATION
! $Id$
! $Name$
! $Revision$
!/===========================================================================/

Module Mod_Nesting
  use all_vars
  use mod_utils
  use mod_ncdio
  use mod_par
  use mod_obcs, only: IOBCN, I_OBC_N, NADJC_OBC, ADJC_OBC, NXOBC, NYOBC
  use sinter
  use mod_nesting_type4, only: nest4_relax_weight, nest4_relax_shear, &
       & nest4_project_transport, nest4_transport_target, &
       & nest4_edge_normal, nest4_characteristic_pair, nest4_match_edge_normals
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  implicit none

  SAVE

  ! For data IO
  TYPE NEST_DATA
     INTEGER :: IDX =0
     INTEGER :: RNG(2) =0
     INTEGER :: NBLK=0

     REAL(SP), POINTER :: UA_BLK(:,:) => NULL(),VA_BLK(:,:) => NULL(),EL_BLK(:,:) => NULL()
     REAL(SP), POINTER :: U_BLK(:,:,:) => NULL(),V_BLK(:,:,:) => NULL()
     REAL(SP), POINTER :: S1_BLK(:,:,:) => NULL(),T1_BLK(:,:,:) => NULL()
     REAL(SP), POINTER :: HYW_BLK(:,:,:) => NULL()
     TYPE(TIME), POINTER :: TIME_BLK(:) => NULL()

     REAL(SP), POINTER :: WCELL_BLK(:,:) => NULL(),WNODE_BLK(:,:) => NULL()

# if defined (FABM)
      REAL(SP), POINTER :: FABM_ALL_TMP(:,:,:) => NULL()
! I think we also need a 4D FABM variable for NCNEST
      REAL(SP), POINTER :: FABM_ALL_BLK(:,:,:,:)  => NULL()     
# endif
  END TYPE NEST_DATA

# if defined (WAVE_CURRENT_INTERACTION)
  TYPE NEST_DATA_WAVE
     INTEGER :: IDX =0
     INTEGER :: RNG(2) =0
     INTEGER :: NBLK=0

     REAL(SP), POINTER :: HSC1_BLK(:,:) => NULL(),TPEAK_BLK(:,:) => NULL(),DIRDEG1_BLK(:,:) => NULL()
     REAL(SP), POINTER :: AC2_BLK(:,:,:,:) => NULL()
     TYPE(TIME), POINTER :: TIME_BLK(:) => NULL()
  END TYPE NEST_DATA_WAVE
# endif


  ! GRID AND DATA FOR RUNNING AS SUBDOMAIN
  TYPE(GRID), POINTER :: NESTING_GRID => NULL()
  TYPE(NEST_DATA), POINTER :: NESTING_DATA => NULL()
  TYPE(NCFILE), POINTER :: NESTING_FILE => NULL()

# if defined (WAVE_CURRENT_INTERACTION)
  TYPE(GRID), POINTER :: NESTING_GRID_WAVE => NULL()
  TYPE(NEST_DATA_WAVE), POINTER :: NESTING_DATA_WAVE => NULL()
  TYPE(NCFILE), POINTER :: NESTING_FILE_WAVE => NULL()
# endif

  !========================================================
  ! Used in NESTING CODE
  ! This memory holds the data for each variable at the current time.
  ! This is where the data that is updated is stored in arrays which
  ! are the size of the nesting boundary, and indexed to the FVCOM domain. 
  REAL(SP), ALLOCATABLE :: UA_NEST(:),VA_NEST(:),EL_NEST(:)
  REAL(SP), ALLOCATABLE :: U_NEST(:,:),V_NEST(:,:)
  REAL(SP), ALLOCATABLE :: S1_NEST(:,:),T1_NEST(:,:)
  REAL(SP), ALLOCATABLE :: HYW_NEST(:,:)

  REAL(SP), ALLOCATABLE :: U_NEST_L(:,:),V_NEST_L(:,:)
  REAL(SP), ALLOCATABLE :: S1_NEST_L(:,:),T1_NEST_L(:,:)
  REAL(SP), ALLOCATABLE :: HYW_NEST_L(:,:)
  
  REAL(SP), ALLOCATABLE, TARGET :: ZZ_L(:,:),ZZ1_L(:,:),H_L(:),H1_L(:)

  REAL(SP), ALLOCATABLE :: WCELL_NEST(:),WNODE_NEST(:)
! songhu
# if defined (FABM)
!  REAL(SP), ALLOCATABLE :: FABM_ALL_BLK(:,:,:,:)
  REAL(SP), ALLOCATABLE :: FABM_ALL_NEST(:,:,:)
# endif
# if defined (WAVE_CURRENT_INTERACTION)
  REAL(SP), ALLOCATABLE :: HSC1_NEST(:),TPEAK_NEST(:),DIRDEG1_NEST(:)
  REAL(SP), ALLOCATABLE :: AC2_NEST(:,:,:)
# endif
  ! 
  TYPE(NCVAR), POINTER :: VAR_UA => NULL()
  TYPE(NCVAR), POINTER :: VAR_VA => NULL()
  TYPE(NCVAR), POINTER :: VAR_EL => NULL()
  TYPE(NCVAR), POINTER :: VAR_U => NULL()
  TYPE(NCVAR), POINTER :: VAR_V => NULL()
  TYPE(NCVAR), POINTER :: VAR_S1 => NULL()
  TYPE(NCVAR), POINTER :: VAR_T1 => NULL()
  TYPE(NCVAR), POINTER :: VAR_HYW => NULL()
  TYPE(NCVAR), POINTER :: VAR_TIME1 => NULL()
  TYPE(NCVAR), POINTER :: VAR_TIME2 => NULL()

  TYPE(NCVAR), POINTER :: VAR_WCELL => NULL()
  TYPE(NCVAR), POINTER :: VAR_WNODE => NULL()

# if defined (WAVE_CURRENT_INTERACTION)
  TYPE(NCVAR), POINTER :: VAR_HSC1 => NULL()
  TYPE(NCVAR), POINTER :: VAR_TPEAK => NULL()
  TYPE(NCVAR), POINTER :: VAR_DIRDEG1 => NULL()
  TYPE(NCVAR), POINTER :: VAR_AC2 => NULL()
# endif
  !========================================================


  ! GRID AND DATA FOR OUTPUT
  INTEGER :: NCNEST_NUM
  CHARACTER(LEN=80), ALLOCATABLE :: NCNEST_FNAMES(:)
  TYPE(GRID), POINTER :: NCNEST_GRIDS(:) => NULL()
  TYPE(NEST_DATA), POINTER :: NCNEST_DATA(:) => NULL()

# if defined (WAVE_CURRENT_INTERACTION)
  INTEGER :: NCNEST_NUM_WAVE
  CHARACTER(LEN=80), ALLOCATABLE :: NCNEST_FNAMES_WAVE(:)
  TYPE(GRID), POINTER :: NCNEST_GRIDS_WAVE(:) => NULL()
  TYPE(NEST_DATA_WAVE), POINTER :: NCNEST_DATA_WAVE(:) => NULL()
# endif
  
  LOGICAL, PRIVATE :: NEED_INIT_NEST = .TRUE.
  ! Optional characteristic OBC: geometry is cached after the mesh is set up.
  LOGICAL, PRIVATE :: N4_CHAR_READY = .FALSE.
  INTEGER, PRIVATE :: N4_NCELL = 0, N4_NNODE = 0
  INTEGER, ALLOCATABLE, PRIVATE :: N4_CELLMAP(:),N4_NODEMAP(:),N4_NEDGE(:)
  INTEGER, ALLOCATABLE, PRIVATE :: N4_CELLIDS(:),N4_NODEIDS(:)
  INTEGER, ALLOCATABLE, PRIVATE :: N4_N1(:,:),N4_N2(:,:),N4_INNER(:,:)
  REAL(SP), ALLOCATABLE, PRIVATE :: N4_NX(:,:),N4_NY(:,:),N4_LENGTH(:,:)
  REAL(SP), ALLOCATABLE, PRIVATE :: N4_UN_TARGET(:,:)
  REAL(SP), ALLOCATABLE, PRIVATE :: N4_ETA_SUM(:),N4_ETA_LENGTH(:)

  TYPE(TIME) :: INTERVAL_TIME_NCNEST, TIME_INTERVAL
# if defined (WAVE_CURRENT_INTERACTION)
  TYPE(TIME) :: INTERVAL_TIME_NCNEST_WAVE, TIME_INTERVAL_WAVE
# endif

  !--Parameters in NameList NML_NESTING
  !TYPE OF NESTING:
  !  1:  DIRECT NESTING     -- The input is the direct NCNEST output of 
  !                            large domain FVCOM model
  !  2:  INDIRECT NESTING   -- Same as 1, but the input file is the subtidal
  !                            values from the NCNEST output of large domain
  !                            FVCOM model plus the Forman tidal analysis 
  !                            elevations and velocities
  !  3:  RELAXATION NESTING -- Direct per-step state blending (legacy)
  !  4:  PHYSICAL-TIMESCALE NESTING: hard OBC elevation/T/S,
  !      interior T/S and shear relaxation, conservative barotropic correction.
  LOGICAL NESTING_ON                   !!TRUE IF OUTPUT RESART FILES
  CHARACTER(LEN=80) NESTING_TYPE       !!TYPE OF NESTING: 1, 2, 3, 4
  LOGICAL NESTING_CHARACTERISTIC_OBC   !!Experimental type-4 outward-wave OBC
  INTEGER :: NESTING_BLOCKSIZE         !!SIZE OF DATA BLOCKS IN FILE
  CHARACTER(LEN=80) NESTING_FILE_NAME  !!NAME OF RESTART FILE
  REAL(SP) :: NESTING_TAU_TS           !!T/S relaxation timescale [seconds]
  REAL(SP) :: NESTING_TAU_SHEAR        !!3-D shear relaxation timescale [seconds]
  REAL(SP) :: NESTING_TAU_ETA          !!Barotropic SSH e-folding time [seconds]
  REAL(SP) :: NESTING_TAU_TRANSPORT    !!Interior transport e-folding time [seconds]
  REAL(SP) :: NESTING_TAU_TRANSPORT_NORMAL     !!OBC normal transport e-folding time [s]
  REAL(SP) :: NESTING_TAU_TRANSPORT_TANGENTIAL !!OBC tangential transport e-folding time [s]
  REAL(SP) :: NESTING_MAX_CORRECTION_SPEED  !!Abort if |delta Q| / depth exceeds threshold [m/s]
# if defined (FABM)
  LOGICAL FABM_NESTING_ON
!  LOGICAL FABM_NCNEST_ON
# endif 

  NAMELIST /NML_NESTING/     &
       & NESTING_ON,         &
# if defined (FABM)
       & FABM_NESTING_ON,    &
# endif
       & NESTING_TYPE,       &
       & NESTING_CHARACTERISTIC_OBC, &
       & NESTING_BLOCKSIZE,  &
       & NESTING_FILE_NAME,  &
       & NESTING_TAU_TS,        &
       & NESTING_TAU_SHEAR,     &
       & NESTING_TAU_ETA,       &
       & NESTING_TAU_TRANSPORT, &
       & NESTING_TAU_TRANSPORT_NORMAL, &
       & NESTING_TAU_TRANSPORT_TANGENTIAL, &
       & NESTING_MAX_CORRECTION_SPEED


  !--Parameters in NameList NML_NCNEST
  LOGICAL NCNEST_ON
  INTEGER :: NCNEST_BLOCKSIZE
  CHARACTER(LEN=160) NCNEST_NODE_FILES
  CHARACTER(LEN=80) NCNEST_OUT_INTERVAL

  NAMELIST /NML_NCNEST/     &
       & NCNEST_ON,         &
 !      & FABM_NCNEST_ON     & ! this is probably not needed.
       & NCNEST_BLOCKSIZE,  &
       & NCNEST_NODE_FILES, &
       & NCNEST_OUT_INTERVAL
  
# if defined (WAVE_CURRENT_INTERACTION)
  !--Parameters in NameList NML_NESTING_WAVE
  LOGICAL NESTING_ON_WAVE                   !!TRUE IF OUTPUT RESART FILES
  CHARACTER(LEN=80) NESTING_TYPE_WAVE       !!TYPE OF WAVE NEST INPUT: 'wave parameters' or 'spectral density'
  INTEGER :: NESTING_BLOCKSIZE_WAVE         !!SIZE OF DATA BLOCKS IN FILE
  CHARACTER(LEN=80) NESTING_FILE_NAME_WAVE  !!NAME OF RESTART FILE

  NAMELIST /NML_NESTING_WAVE/     &
       & NESTING_ON_WAVE,         &
       & NESTING_TYPE_WAVE,       &
       & NESTING_BLOCKSIZE_WAVE,  &
       & NESTING_FILE_NAME_WAVE


  LOGICAL NCNEST_ON_WAVE
  CHARACTER(LEN=80) NCNEST_TYPE_WAVE       !!TYPE OF WAVE NEST OUTPUT: 'wave parameters' or 'spectral density'
  INTEGER :: NCNEST_BLOCKSIZE_WAVE
  CHARACTER(LEN=160) NCNEST_NODE_FILES_WAVE
  CHARACTER(LEN=80) NCNEST_OUT_INTERVAL_WAVE

  NAMELIST /NML_NCNEST_WAVE/     &
       & NCNEST_ON_WAVE,         &
       & NCNEST_TYPE_WAVE,       &
       & NCNEST_BLOCKSIZE_WAVE,  &
       & NCNEST_NODE_FILES_WAVE, &
       & NCNEST_OUT_INTERVAL_WAVE
# endif  

  INTEGER :: KB_L
  INTEGER :: KBM1_L

CONTAINS
  !==============================================================================!
  !
  !==============================================================================!
  SUBROUTINE NAME_LIST_INITIALIZE_NEST
    USE CONTROL

    IMPLICIT NONE

    !--Parameters in NameList NML_NESTING
    NESTING_ON = .FALSE.
# if defined (FABM)
    FABM_NESTING_ON = .FALSE.
# endif
    NESTING_TYPE = "'1' or '2' or '3' or '4'"
    NESTING_CHARACTERISTIC_OBC = .FALSE.
    NESTING_BLOCKSIZE = -1
    NESTING_FILE_NAME = trim(casename)//"_nesting.nc"
    NESTING_TAU_TS = 43200.0_SP
    NESTING_TAU_SHEAR = 10800.0_SP
    NESTING_TAU_ETA = 21600.0_SP
    NESTING_TAU_TRANSPORT = 7200.0_SP
    ! -1 means inherit NESTING_TAU_TRANSPORT from the namelist.
    NESTING_TAU_TRANSPORT_NORMAL = -1.0_SP
    NESTING_TAU_TRANSPORT_TANGENTIAL = -1.0_SP
    NESTING_MAX_CORRECTION_SPEED = 5.0_SP

    !--Parameters in NameList NML_NCNEST
    NCNEST_ON = .FALSE.
 !   # if defined (FABM)
 !   FABM_NCNEST_ON = .FALSE.
 !   # endif
    NCNEST_BLOCKSIZE = -1
    NCNEST_NODE_FILES = "none"
    NCNEST_OUT_INTERVAL = "A length of time: 'seconds= ','days= ', or 'cycles= '"

#   if defined (WAVE_CURRENT_INTERACTION)
    !--Parameters in NameList NML_NESTING_WAVE
    NESTING_ON_WAVE = .FALSE.
    NESTING_TYPE_WAVE = "'wave parameters' or 'spectral density'"
    NESTING_BLOCKSIZE_WAVE = -1
    NESTING_FILE_NAME_WAVE = trim(casename)//"_nesting_wave.nc"


    !--Parameters in NameList NML_NCNEST_WAVE
    NCNEST_ON_WAVE = .FALSE.
    NCNEST_TYPE_WAVE = "'wave parameters' or 'spectral density'"
    NCNEST_BLOCKSIZE_WAVE = -1
    NCNEST_NODE_FILES_WAVE = "none"
    NCNEST_OUT_INTERVAL_WAVE = "A length of time: 'seconds= ','days= ', or 'cycles= '"
#   endif

    KB_L   = 0
    KBM1_L = 0

    RETURN
  END SUBROUTINE NAME_LIST_INITIALIZE_NEST
  !==============================================================================!
  !  
  !==============================================================================!  
  SUBROUTINE NAME_LIST_PRINT_NEST
    USE CONTROL

    IMPLICIT NONE

    WRITE(UNIT=IPT,NML=NML_NCNEST)

    WRITE(UNIT=IPT,NML=NML_NESTING)

#   if defined (WAVE_CURRENT_INTERACTION)
    WRITE(UNIT=IPT,NML=NML_NCNEST_WAVE)

    WRITE(UNIT=IPT,NML=NML_NESTING_WAVE)
#   endif

    RETURN
  END SUBROUTINE NAME_LIST_PRINT_NEST
  !==============================================================================!
  !
  !==============================================================================!
  SUBROUTINE NAME_LIST_READ_NEST
    USE MOD_UTILS
    USE CONTROL
    USE MOD_SET_TIME, ONLY : GET_OUTPUT_FILE_INTERVAL

    IMPLICIT NONE

    INTEGER :: ios,I
    CHARACTER(LEN=120) :: FNAME

    IF(DBG_SET(dbg_sbr)) write(IPT,*) "Subroutine Begins: name_list_read_nest;"

    ios = 0
    FNAME = "./"//trim(casename)//"_run.nml"
    IF(DBG_SET(dbg_io)) write(IPT,*) "Get_nestpar: File: ",trim(FNAME)

    CALL FOPEN(NMLUNIT,trim(FNAME),'cfr')

    !READ NAME LIST FILE

    !READ NESTING FLAG
    READ(UNIT=NMLUNIT, NML=NML_NESTING,IOSTAT=ios)  
    IF(ios /= 0)THEN
       IF(DBG_SET(dbg_log)) write(UNIT=IPT,NML=NML_NESTING)
       CALL FATAL_ERROR("Can Not Read NameList NML_NESTING from file: "//trim(FNAME))
    END IF

    if(DBG_SET(dbg_scl)) &
         & write(IPT,*) "Read_Name_List:NML_NESTING"

    if(DBG_SET(dbg_scl)) &
         & write(UNIT=IPT,NML=NML_NESTING)

    REWIND(NMLUNIT)

    !READ NESTING FLAG
    READ(UNIT=NMLUNIT, NML=NML_NCNEST,IOSTAT=ios)  
    IF(ios /= 0)THEN
       IF(DBG_SET(dbg_log)) write(UNIT=IPT,NML=NML_NCNEST)
       CALL FATAL_ERROR("Can Not Read NameList NML_NCNEST from file: "//trim(FNAME))
    END IF

    if(DBG_SET(dbg_scl)) &
         & write(IPT,*) "Read_Name_List: NML_NCNEST"

    if(DBG_SET(dbg_scl)) &
         & write(UNIT=IPT,NML=NML_NCNEST)


#   if defined (WAVE_CURRENT_INTERACTION)
    REWIND(NMLUNIT)

    !READ NESTING FLAG
    READ(UNIT=NMLUNIT, NML=NML_NESTING_WAVE,IOSTAT=ios)  
    IF(ios /= 0)THEN
       IF(DBG_SET(dbg_log)) write(UNIT=IPT,NML=NML_NESTING_WAVE)
       CALL FATAL_ERROR("Can Not Read NameList NML_NESTING_WAVE from file: "//trim(FNAME))
    END IF

    if(DBG_SET(dbg_scl)) &
         & write(IPT,*) "Read_Name_List:NML_NESTING_WAVE"

    if(DBG_SET(dbg_scl)) &
         & write(UNIT=IPT,NML=NML_NESTING_WAVE)

    REWIND(NMLUNIT)

    !READ NESTING FLAG
    READ(UNIT=NMLUNIT, NML=NML_NCNEST_WAVE,IOSTAT=ios)  
    IF(ios /= 0)THEN
       IF(DBG_SET(dbg_log)) write(UNIT=IPT,NML=NML_NCNEST_WAVE)
       CALL FATAL_ERROR("Can Not Read NameList NML_NCNEST_WAVE from file: "//trim(FNAME))
    END IF

    if(DBG_SET(dbg_scl)) &
         & write(IPT,*) "Read_Name_List: NML_NCNEST_WAVE"

    if(DBG_SET(dbg_scl)) &
         & write(UNIT=IPT,NML=NML_NCNEST_WAVE)
#   endif

    CLOSE(NMLUNIT)

    ! DO SOME BASIC CHECKING ON THE NESTING NAMELIST
    IF(NESTING_ON) THEN
       IF(NESTING_CHARACTERISTIC_OBC .AND. TRIM(NESTING_TYPE)/='4') &
            & CALL FATAL_ERROR('CHARACTERISTIC OBC REQUIRES NESTING_TYPE=4')

       IF( NESTING_BLOCKSIZE < 2) CALL FATAL_ERROR &
            & ("THE NESTING_BLOCKSIZE IS LESS THAN TWO IN THE NESTING NAME LIST")

       IF(LEN_TRIM(NESTING_FILE_NAME) == 0 ) CALL FATAL_ERROR &
            & ("THE NESTING FILE NAME IS EMPTY IN THE NAME LIST")

       IF(TRIM(NESTING_TYPE) == '4') THEN
          IF(NESTING_CHARACTERISTIC_OBC .AND. DBG_SET(DBG_LOG)) &
               & WRITE(IPT,*) 'TYPE 4: CHARACTERISTIC OBC (EXPERIMENTAL)'
          IF(NESTING_TAU_TRANSPORT_NORMAL == -1.0_SP) &
               & NESTING_TAU_TRANSPORT_NORMAL = NESTING_TAU_TRANSPORT
          IF(NESTING_TAU_TRANSPORT_TANGENTIAL == -1.0_SP) &
               & NESTING_TAU_TRANSPORT_TANGENTIAL = NESTING_TAU_TRANSPORT
          IF(.NOT. IEEE_IS_FINITE(NESTING_TAU_TRANSPORT_NORMAL) .OR. &
               & NESTING_TAU_TRANSPORT_NORMAL <= 0.0_SP) &
               & CALL FATAL_ERROR('TYPE 4: NESTING_TAU_TRANSPORT_NORMAL MUST BE POSITIVE')
          IF(.NOT. IEEE_IS_FINITE(NESTING_TAU_TRANSPORT_TANGENTIAL) .OR. &
               & NESTING_TAU_TRANSPORT_TANGENTIAL <= 0.0_SP) &
               & CALL FATAL_ERROR('TYPE 4: NESTING_TAU_TRANSPORT_TANGENTIAL MUST BE POSITIVE')
          IF(.NOT. IEEE_IS_FINITE(NESTING_TAU_TS) .OR. NESTING_TAU_TS <= 0.0_SP) &
               CALL FATAL_ERROR("TYPE 4: NESTING_TAU_TS MUST BE POSITIVE")
          IF(.NOT. IEEE_IS_FINITE(NESTING_TAU_SHEAR) .OR. NESTING_TAU_SHEAR <= 0.0_SP) &
               CALL FATAL_ERROR("TYPE 4: NESTING_TAU_SHEAR MUST BE POSITIVE")
          IF(.NOT. IEEE_IS_FINITE(NESTING_TAU_ETA) .OR. NESTING_TAU_ETA <= 0.0_SP) &
               CALL FATAL_ERROR("TYPE 4: NESTING_TAU_ETA MUST BE POSITIVE")
          IF(.NOT. IEEE_IS_FINITE(NESTING_TAU_TRANSPORT) .OR. NESTING_TAU_TRANSPORT <= 0.0_SP) &
               CALL FATAL_ERROR("TYPE 4: NESTING_TAU_TRANSPORT MUST BE POSITIVE")
          IF(.NOT. IEEE_IS_FINITE(NESTING_MAX_CORRECTION_SPEED) .OR. &
               & NESTING_MAX_CORRECTION_SPEED <= 0.0_SP) &
               CALL FATAL_ERROR('TYPE 4: NESTING_MAX_CORRECTION_SPEED MUST BE POSITIVE')
# if defined (SEMI_IMPLICIT) || defined (NH) || defined (WET_DRY) || defined (MEAN_FLOW) || defined (TWO_D_MODEL) || defined (ONE_D_MODEL) || defined (ISOMIP)
          CALL FATAL_ERROR("TYPE 4: UNSUPPORTED BUILD CONFIGURATION")
# endif
# if defined (FABM)
          IF(FABM_NESTING_ON) CALL FATAL_ERROR("TYPE 4: FABM NESTING NOT SUPPORTED")
# endif
          IF(DBG_SET(DBG_LOG)) WRITE(IPT,*) &
               'TYPE 4: CONSERVATIVE BAROTROPIC PROJECTION (EXPERIMENTAL).'
# if defined (PLBC)
          CALL FATAL_ERROR('TYPE 4: PLBC NOT SUPPORTED IN PROJECTION')
# endif
# if defined (ICESHELF)
          CALL FATAL_ERROR('TYPE 4: ICE SHELF NOT VALIDATED')
# endif
       END IF

    END IF

    IF(NCNEST_ON) THEN

       IF( NCNEST_BLOCKSIZE < 2) CALL FATAL_ERROR &
            & ("THE NCNEST_BLOCKSIZE IS LESS THAN TWO IN THE NCNEST NAME LIST")

       IF(NCNEST_NODE_FILES == 'none' .or. len_trim(NCNEST_NODE_FILES)==0)CALL FATAL_ERROR &
            & ("THE NCNEST_NODE_FILES VARIABLE IS EMPTY IN THE NCNEST NAME LIST")

       CALL GET_OUTPUT_FILE_INTERVAL(TRIM(NCNEST_OUT_INTERVAL),INTERVAL_TIME_NCNEST)

    END IF

#   if defined (WAVE_CURRENT_INTERACTION)
    ! DO SOME BASIC CHECKING ON THE NESTING NAMELIST
    IF(NESTING_ON_WAVE) THEN

       IF( TRIM(NESTING_TYPE_WAVE) /= 'wave parameters' .and. TRIM(NESTING_TYPE_WAVE) /= 'spectral density')   &
         CALL FATAL_ERROR("THE NESTING_TYPE_WAVE SHOULD BE 'wave parameters' OR 'spectral density'")

       IF( NESTING_BLOCKSIZE_WAVE < 2) CALL FATAL_ERROR &
            & ("THE NESTING_BLOCKSIZE_WAVE IS LESS THAN TWO IN THE WAVE NESTING NAME LIST")

       IF(LEN_TRIM(NESTING_FILE_NAME_WAVE) == 0 ) CALL FATAL_ERROR &
            & ("THE WAVE NESTING FILE NAME IS EMPTY IN THE NAME LIST")
#   if !defined (WAVE_ONLY)
       IF(NESTING_BLOCKSIZE /= NESTING_BLOCKSIZE_WAVE)CALL FATAL_ERROR(              &
         "NESTING_BLOCKSIZE /= NESTING_BLOCKSIZE_WAVE", &
         "Please use NESTING_BLOCKSIZE == NESTING_BLOCKSIZE_WAVE for this case.")

#   endif	    

    END IF

    IF(NCNEST_ON_WAVE) THEN

       IF( TRIM(NCNEST_TYPE_WAVE) /= 'wave parameters' .and. TRIM(NCNEST_TYPE_WAVE) /= 'spectral density')   &
         CALL FATAL_ERROR("THE NCNEST_TYPE_WAVE SHOULD BE 'wave parameters' OR 'spectral density'")

       IF( NCNEST_BLOCKSIZE_WAVE < 2) CALL FATAL_ERROR &
            & ("THE NCNEST_BLOCKSIZE_WAVE IS LESS THAN TWO IN THE WAVE NCNEST NAME LIST")

       IF(NCNEST_NODE_FILES_WAVE == 'none' .or. len_trim(NCNEST_NODE_FILES_WAVE)==0)CALL FATAL_ERROR &
            & ("THE NCNEST_NODE_FILES_WAVE VARIABLE IS EMPTY IN THE NCNEST NAME LIST")

#   if !defined (WAVE_ONLY)
       INTERVAL_TIME_NCNEST_WAVE = INTERVAL_TIME_NCNEST
#   else
       CALL GET_OUTPUT_FILE_INTERVAL(TRIM(NCNEST_OUT_INTERVAL_WAVE),INTERVAL_TIME_NCNEST_WAVE)
#   endif	    
    END IF
#   endif
    CLOSE(NMLUNIT)

    if(DBG_SET(dbg_sbr)) &
         & write(IPT,*) "Subroutine Ends: name_list_read_nest;"    

  END SUBROUTINE NAME_LIST_READ_NEST

  SUBROUTINE OPEN_NESTING_FILE
    IMPLICIT NONE
    TYPE(NCFILE), POINTER :: NCF
    integer :: charnum
    logical :: back=.true.
    character(len=160) :: pathnfile
        ! LOAD NESTING NETCDF FILE
    IF (NESTING_ON) THEN

       ! TEST FILE NAME
       charnum = index (NESTING_FILE_NAME,".nc",back)
       if (charnum /= len_trim(NESTING_FILE_NAME)-2)&
            & CALL WARNING("Nesting File does not end in .nc", &
            & trim(NESTING_FILE_NAME))
       
       ! INITIALIZE TYPE TO HOLD FILE METADATA
       pathnfile= trim(INPUT_DIR)//trim(NESTING_FILE_NAME)
       CALL  NC_INIT(NCF,pathnfile)
       
       ! OPEN THE FILE AND LOAD METADATA
       if(.not. NCF%OPEN) then
          Call NC_OPEN(NCF)
          CALL NC_LOAD(NCF)
          FILEHEAD => ADD(FILEHEAD,NCF)
       end if
       
    END IF

  END SUBROUTINE OPEN_NESTING_FILE

# if defined (WAVE_CURRENT_INTERACTION)
  SUBROUTINE OPEN_NESTING_FILE_WAVE
    IMPLICIT NONE
    TYPE(NCFILE), POINTER :: NCF1
    integer :: charnum
    logical :: back=.true.
    character(len=160) :: pathnfile
        ! LOAD NESTING NETCDF FILE
    IF (NESTING_ON_WAVE) THEN

       ! TEST FILE NAME
       charnum = index (NESTING_FILE_NAME_WAVE,".nc",back)
       if (charnum /= len_trim(NESTING_FILE_NAME_WAVE)-2)&
            & CALL WARNING("Nesting File of Wave does not end in .nc", &
            & trim(NESTING_FILE_NAME_WAVE))
       
       ! INITIALIZE TYPE TO HOLD FILE METADATA
       pathnfile= trim(INPUT_DIR)//trim(NESTING_FILE_NAME_WAVE)
       CALL  NC_INIT(NCF1,pathnfile)
       
       ! OPEN THE FILE AND LOAD METADATA
       if(.not. NCF1%OPEN) then
          Call NC_OPEN(NCF1)
          CALL NC_LOAD(NCF1)
          FILEHEAD => ADD(FILEHEAD,NCF1)
       end if
       
    END IF

  END SUBROUTINE OPEN_NESTING_FILE_WAVE
# endif

# if defined (MULTIPROCESSOR)
  
  SUBROUTINE SETUP_NEST_DOMAIN
    IMPLICIT NONE
    
    LOGICAL :: FOUND
    ! NESTING VARIABLES
    TYPE(GRID), POINTER :: GLOBAL_NG
    character(len=160) :: pathnfile
    
    ! NCNEST VARIABLES
    INTEGER :: STATUS,I
    INTEGER, POINTER :: NID(:)
    
    if(DBG_SET(dbg_sbr))  write(IPT,*) "START Setup_nest_domain"
    
    if(DBG_SET(dbg_log))  write(IPT,*) "! "
    
   if (dbg_set(dbg_LOG)) write(IPT,*) "!           SETTING UP NESTING IO"

    IF(NESTING_ON) THEN
       
       NESTING_FILE => FIND_FILE(FILEHEAD,NESTING_FILE_NAME,FOUND)
       IF(.NOT. FOUND) CALL FATAL_ERROR&
            &("SETUP_NEST_DOMAIN: CAN NOT FIND NESTING FILE IN OBJECT LIST",&
            & "FILE NAME:"//TRIM(NESTING_FILE_NAME))


       ALLOCATE(GLOBAL_NG,STAT=status)
       IF(STATUS /=0) CALL FATAL_ERROR("COULD NOT ALLOCATE GLOBAL_NG!")
       
       ! READ THE GLOBAL NESTING GRID INTO A TEMPORARY CONTAINER
       IF(.NOT. IOPROC) CALL LOAD_GRID_TYPE(NESTING_FILE,GLOBAL_NG)
       
       KB_L = GLOBAL_NG%KB
!
!       IF(GLOBAL_NG%KB /= KB) CALL FATAL_ERROR&
!            &("Be not ashamed of mistakes and thus make them crimes.")
!       ! YOUR NESTING FILE DOES NOT HAVE THE SAME NUMBER OF LEVELS AS
!       ! YOUR MODEL
       
       KBM1_L = GLOBAL_NG%KBM1
!
!       IF(GLOBAL_NG%KBM1 /= KBM1) CALL FATAL_ERROR&
!            &("He who will not economize will have to agonize.")
!       ! YOUR NESTING FILE DOES NOT HAVE THE SAME NUMBER OF LAYERS AS
!       ! YOUR MODEL

       ALLOCATE(NESTING_GRID,STAT=status)
       IF(STATUS /=0) CALL FATAL_ERROR("COULD NOT ALLOCATE NESTING_GRID!")
       
       ! DECOMPOSE THE GLOBAL NESTING GRID TO THE LOCAL MAP
       CALL GENMAP_NESTING(GLOBAL_NG, NESTING_GRID )
       
       ! DUMP THE GLOBAL DATA
       CALL KILL_GRID(GLOBAL_NG)
       
       if(DBG_SET(dbg_log)) write(IPT,*) "! SET NESTING SUBDOMAIN"
       if(DBG_SET(dbg_log))  write(IPT,*) "! DIMENSIONS: MGL =",NESTING_GRID%MGL
       if(DBG_SET(dbg_log))  write(IPT,*) "! DIMENSIONS: NGL =",NESTING_GRID%NGL


       ALLOCATE(H_L(0:NESTING_GRID%MT));          H_L   = 0.0_SP
       ALLOCATE(H1_L(0:NESTING_GRID%NT));         H1_L  = 0.0_SP
       ALLOCATE(ZZ_L(0:NESTING_GRID%MT,KBM1_L));  ZZ_L  = 0.0_SP
       ALLOCATE(ZZ1_L(0:NESTING_GRID%NT,KBM1_L)); ZZ1_L = 0.0_SP
       
       H_L   = NESTING_GRID%H
       H1_L  = NESTING_GRID%H1
       ZZ_L  = NESTING_GRID%ZZ
       ZZ1_L = NESTING_GRID%ZZ1
       
    END IF
    
#   if defined (WAVE_CURRENT_INTERACTION)
    IF(NESTING_ON_WAVE) THEN
       
       NESTING_FILE_WAVE => FIND_FILE(FILEHEAD,NESTING_FILE_NAME_WAVE,FOUND)
       IF(.NOT. FOUND) CALL FATAL_ERROR&
            &("SETUP_NEST_DOMAIN_WAVE: CAN NOT FIND WAVE NESTING FILE IN OBJECT LIST",&
            & "FILE NAME:"//TRIM(NESTING_FILE_NAME_WAVE))


       ALLOCATE(GLOBAL_NG,STAT=status)
       IF(STATUS /=0) CALL FATAL_ERROR("COULD NOT ALLOCATE GLOBAL_NG!")
       
       ! READ THE GLOBAL NESTING GRID INTO A TEMPORARY CONTAINER
       IF(.NOT. IOPROC) CALL LOAD_GRID_TYPE(NESTING_FILE_WAVE,GLOBAL_NG)
       
!       IF(GLOBAL_NG%KB /= KB) CALL FATAL_ERROR&
!            &("Be not ashamed of mistakes and thus make them crimes.")
!       ! YOUR NESTING FILE DOES NOT HAVE THE SAME NUMBER OF LEVELS AS
!       ! YOUR MODEL
       
!       IF(GLOBAL_NG%KBM1 /= KBM1) CALL FATAL_ERROR&
!            &("He who will not economize will have to agonize.")
!       ! YOUR NESTING FILE DOES NOT HAVE THE SAME NUMBER OF LAYERS AS
!       ! YOUR MODEL

       ALLOCATE(NESTING_GRID_WAVE,STAT=status)
       IF(STATUS /=0) CALL FATAL_ERROR("COULD NOT ALLOCATE NESTING_GRID_WAVE!")
       
       ! DECOMPOSE THE GLOBAL NESTING GRID TO THE LOCAL MAP
       CALL GENMAP_NESTING(GLOBAL_NG, NESTING_GRID_WAVE )
       
       ! DUMP THE GLOBAL DATA
       CALL KILL_GRID(GLOBAL_NG)
       
       if(DBG_SET(dbg_log)) write(IPT,*) "! SET WAVE NESTING SUBDOMAIN"
       if(DBG_SET(dbg_log))  write(IPT,*) "! DIMENSIONS: MGL =",NESTING_GRID_WAVE%MGL
       if(DBG_SET(dbg_log))  write(IPT,*) "! DIMENSIONS: NGL =",NESTING_GRID_WAVE%NGL

    END IF
#   endif    
    
    
    IF (NCNEST_ON) THEN
       
       CALL SPLIT_STRING(NCNEST_NODE_FILES,",",NCNEST_FNAMES)
       NCNEST_NUM = SIZE(NCNEST_FNAMES)
       
       ALLOCATE(NCNEST_GRIDS(NCNEST_NUM),STAT=status)
       IF(STATUS /=0) CALL FATAL_ERROR("COULD NOT ALLOCATE NCNEST_GRIDS!")
       
       if(DBG_SET(dbg_log))  write(IPT,*) "! READING NCNEST FILES:",NCNEST_NUM
       
       DO I = 1, NCNEST_NUM
          
          if(DBG_SET(dbg_log))  write(IPT,*) "! READING NCNEST FILE:"//TRIM(NCNEST_FNAMES(I))
          
          ! NID IS ALLOCATED AND DEALLOCATED INTERNALLY!
          CALL LOAD_NESTING_NODE_FILE(NCNEST_FNAMES(I),NID)
          
          CALL GENMAP_NCNEST(NID,NCNEST_GRIDS(I))

          if(DBG_SET(dbg_log))  write(IPT,*) "! SET NCNEST DOMAIN:"
          if(DBG_SET(dbg_log))  write(IPT,*) "! DIMENSIONS: MGL =",NCNEST_GRIDS(I)%MGL
          if(DBG_SET(dbg_log))  write(IPT,*) "! DIMENSIONS: NGL =",NCNEST_GRIDS(I)%NGL

       END DO
       

    END IF
    
#   if defined (WAVE_CURRENT_INTERACTION)
    IF (NCNEST_ON_WAVE) THEN
       
       CALL SPLIT_STRING(NCNEST_NODE_FILES_WAVE,",",NCNEST_FNAMES_WAVE)
       NCNEST_NUM_WAVE = SIZE(NCNEST_FNAMES_WAVE)
       
       ALLOCATE(NCNEST_GRIDS_WAVE(NCNEST_NUM_WAVE),STAT=status)
       IF(STATUS /=0) CALL FATAL_ERROR("COULD NOT ALLOCATE NCNEST_GRIDS_WAVE!")
       
       if(DBG_SET(dbg_log))  write(IPT,*) "! READING WAVE NCNEST FILES:",NCNEST_NUM_WAVE
       
       DO I = 1, NCNEST_NUM_WAVE
          
          if(DBG_SET(dbg_log))  write(IPT,*) "! READING WAVE NCNEST FILE:"//TRIM(NCNEST_FNAMES_WAVE(I))
          
          ! NID IS ALLOCATED AND DEALLOCATED INTERNALLY!
          CALL LOAD_NESTING_NODE_FILE(NCNEST_FNAMES_WAVE(I),NID)
          
          CALL GENMAP_NCNEST(NID,NCNEST_GRIDS_WAVE(I))

          if(DBG_SET(dbg_log))  write(IPT,*) "! SET WAVE NCNEST DOMAIN:"
          if(DBG_SET(dbg_log))  write(IPT,*) "! DIMENSIONS: MGL =",NCNEST_GRIDS_WAVE(I)%MGL
          if(DBG_SET(dbg_log))  write(IPT,*) "! DIMENSIONS: NGL =",NCNEST_GRIDS_WAVE(I)%NGL

       END DO
       

    END IF
#   endif    
    
    IF(.NOT. IOPROC)THEN
       CALL ALLOC_NEST ! TEST FOR NCNEST,NESTING INSIDE...
       CALL SETUP_NEST ! SETUP THE GRID FILE AND DATA IO FOR NESTING
#    if defined (WAVE_CURRENT_INTERACTION)
       CALL ALLOC_NEST_WAVE ! TEST FOR NCNEST,NESTING INSIDE...
       CALL SETUP_NEST_WAVE ! SETUP THE GRID FILE AND DATA IO FOR NESTING
#    endif
    END IF


    if(DBG_SET(dbg_sbr))  write(IPT,*) "END Setup_nest_domain"
  END SUBROUTINE SETUP_NEST_DOMAIN
  
  !============================================================================!
  !   
  !============================================================================!   
  SUBROUTINE LOAD_NESTING_NODE_FILE(FNAME,nodes)
    USE CONTROL
    IMPLICIT NONE
    CHARACTER(len=*), INTENT(IN) :: FNAME
    INTEGER, INTENT(OUT), POINTER :: nodes(:)

    INTEGER :: nnode
    INTEGER, POINTER :: ns(:)

    INTEGER :: I,J,n1,n2,n3
    CHARACTER(LEN=80) :: PATHNFILE, TEMP,TEMP2
    INTEGER :: ISCAN,IOS,sender,IERR

    IF(MSR)THEN

       !============================================
       !
       !  READ THE NESTING NODES
       !
       !============================================

       PATHNFILE = TRIM(INPUT_DIR)//TRIM(FNAME)
       CALL FOPEN(NESTUNIT,trim(pathnfile),'cfr')

       ISCAN = SCAN_FILE(NESTUNIT,"Node_Nest Number", ISCAL = nnode)
       IF(ISCAN /= 0) then
          write(temp,'(I2)') ISCAN
          call fatal_error('Improper formatting of NESTING NODE FILE: ISCAN ERROR&
               &# '//trim(temp),&
               & 'The header must contain: "Node_Nest Number = "', &
               & 'Followed by an integer number of Nesting Nodes')
       END IF


       REWIND NESTUNIT

       DO WHILE(.TRUE.)
          READ(NESTUNIT,*,IOSTAT=IOS,END=199)N1,N2,N3
          if (IOS == 0) then
             BackSpace NESTUNIT
             exit
          end if

          CYCLE

199       Call FATAL_ERROR('Improper formatting of NESTING NODE FILE:',&
               &'Reached end of file with out finding NESTING data?',&
               &'FORMAT: NEST# SUBDOMAIN# LARGEDOMAIN# (ALL INTEGERS)')

       END DO
       
       ALLOCATE(nodes(nnode))

       I = 0
       DO WHILE(.TRUE.)

          READ(NESTUNIT,*,IOSTAT=IOS) N1,N2,N3
          IF(IOS < 0) exit

          I = I + 1
          IF(I > NNODE) CALL FATAL_ERROR('Number of rows of data in the NESTING NODE file &
               &exceeds the stated number of nodes in the header ?')


          nodes(i) = N2
         

       END DO

       CLOSE(NESTUNIT)



       ! CHECK TO MAKE SURE VALUES ARE REASONABLE!
       IF( 1 > MINVAL(NODES)) THEN
          write(temp,'(I8)') MINLOC(NODES)
          write(temp2,'(I8)') MGL
          CALL FATAL_ERROR('NESTING NODE NUMBER'//trim(temp)//&
               & ' IS NOT IN THE GLOBAL DOMAIN',&
               & 'CHECK INPUT FILE NESTING NODES ARE 1 <= '//trim(temp2))
       END IF

       IF(  MAXVAL(NODES) > MGL) THEN
          write(temp,'(I8)') MAXLOC(NODES)
          write(temp2,'(I8)') MGL
          CALL FATAL_ERROR('NESTING NODE NUMBER'//trim(temp)//&
               & ' IS NOT IN THE GLOBAL DOMAIN',&
               & 'CHECK INPUT FILE NESTING NODES ARE 1 <= '//trim(temp2))
       END IF

    END IF

    ! BROADCAST TO OTHER PROCS
    
    SENDER = MSRID -1 ! SEND FROM MASTER
    CALL MPI_BCAST(NNODE,1,MPI_INTEGER,SENDER,MPI_COMM_WORLD,IERR)
    
    IF( .NOT. MSR) THEN
       
       ALLOCATE(NODES(NNODE))
       
    END IF
    
    
    CALL MPI_BCAST(NODES,NNODE,MPI_INTEGER,SENDER,MPI_COMM_WORLD,IERR)


    RETURN
  END SUBROUTINE LOAD_NESTING_NODE_FILE
!==============================================================================|
SUBROUTINE GENMAP_NCNEST(NID,NG)
!==============================================================================|
!
! CREATE A GLOBAL TO LOCAL MAP FOR DOMAIN NESTING OUTPUT
! USES DATA READ INTO: NID - THE NESTING BOUNDARY NODE STRING
!
!
!                     
! Creates:             MAP LINK LIST ENTRY FOR IO
!                      THE GRID FOR THE NESTING OUTPUT
!                          The transform from the local nesting list to the local domain
!
!==============================================================================|
  USE MOD_PAR
  USE MOD_TYPES
  USE LIMS
  USE CONTROL
  USE ALL_VARS
  IMPLICIT NONE

  TYPE(GRID), INTENT(OUT) :: NG !The Local Grid, The Local Nesting Grid
  INTEGER,INTENT(INOUT), POINTER :: NID(:)

  integer :: SENDER,RECVER, ierr, I, J,NCNT, NSZE, I1, status,CNT,CNT_L,K,lb,ub

  INTEGER, POINTER :: TEMP1(:),TEMP2(:)

  INTEGER, POINTER :: ELMS(:), ELMS_GL(:)

  TYPE(MAP), POINTER, DIMENSION(:) :: E_MAP,N_MAP

  if (dbg_set(dbg_sbr)) &
       & write(IPT,*) "START: GENMAP_NCNEST"

  IF(.not.ASSOCIATED(NID)) CALL FATAL_ERROR&
       &('Called GENMAP_NCNEST, but NID is not associated!')

  ! FIND THE NESTING ELEMENTS
  IF(.not. IOPROC) THEN
     ALLOCATE(ELMS(0:NT)); ELMS=0 ! TEMPORARY STORAGE
     CNT=0
     DO I = 1,NT
        IF(ANY(NID==NGID_X(NV(I,1))))THEN ! FIND ANY CELL THAT HAS
           ! A LISTED NODE
           IF(ANY(NID==NGID_X(NV(I,2))) .and. ANY(NID==NGID_X(NV(I,3))))THEN
              ! IF ALL THE NODES IN THAT CELL ARE LISTED...
              CNT = CNT+1
              ELMS(CNT) = I
           END IF
        END IF
     END DO
     
     
     ! MUST COUNT ELEMENTS
     CNT_L=0
     DO I=1,CNT
        IF(ELMS(I)<=N) CNT_L = CNT_L+1
     END DO
  ELSE
     CNT_L = 0
     CNT = 0
  END IF
  
  ! SET DIMENSIONS...
  NG%MGL = ubound(NID,1) ! nodes are easy
   
  CALL MPI_ALLREDUCE(CNT_L,NG%NGL,1,MPI_INTEGER,MPI_SUM,MPI_COMM_WORLD,IERR)
  
  IF(NG%NGL < 1) CALL FATAL_ERROR&
       &("GENMAP_NCNEST: THERE IS A PROBLEM WITH YOUR NESTING FILE",&
       & "THERE ARE NO ELEMENTS SELCTED BASED ON THE NODES YOU SPECIFIED!")

  ! LEVELS AND LAYERS ARE EASY...
  NG%KB = KB
  NG%KBM1 = KBM1
  
# if defined (WAVE_CURRENT_INTERACTION)
  NG%MSC = MSC
  NG%MDC = MDC
# endif


!  WRITE(IPT,*) "MYID,LCL,LXL,GL",MYID,CNT_L,CNT,NG%NGL

  IF(.not. IOPROC) THEN
     
     ! BUILD ONE GLOBAL LIST OF NESTING ELEMENTS
     ALLOCATE(ELMS_GL(NG%NGL))
     K = 1
     DO I = 1, NPROCS
        
        ! SEND THE NUMBER OF ELEMENTS YOU HAVE
        SENDER = I -1
        IF(MYID == I)THEN
           I1 = CNT_L
        END IF
        CALL MPI_BCAST(I1,1,MPI_INTEGER,SENDER,MPI_FVCOM_GROUP,IERR)
        
        ! COLLECT THE GLOBAL IDS
        IF(I1 > 0)THEN
           ALLOCATE(TEMP1(I1))
           IF(MYID == I)THEN
              CNT_L =0
              DO J =1,CNT
                 IF(ELMS(J)<=N) THEN
                    CNT_L = CNT_L+1
                    TEMP1(CNT_L)=EGID_X(ELMS(J))
                 END IF
              END DO
!              WRITE(IPT,*) "ELMS_LCL",TEMP1
              
           END IF
           
           
           CALL MPI_BCAST(TEMP1,I1,MPI_INTEGER,SENDER,MPI_FVCOM_GROUP,IERR)
           
           ELMS_GL(K:(K+I1-1))=TEMP1
           DEALLOCATE(TEMP1)
           
           K = K+I1
        END IF
        
     END DO
     
!     WRITE(IPT,*) "ELMS_GL",ELMS_GL
     
     DEALLOCATE(ELMS)
!============================================
! Make a list of the local Nesting nodes
!============================================
     
     !!SET UP LOCAL NESTING NODES
     ALLOCATE(TEMP1(0:NG%MGL));      TEMP1=0
     ALLOCATE(TEMP2(0:NG%MGL));      TEMP2=0
     
     NCNT = 0
     DO I=1,NG%MGL
        I1 = NLID( NID(I) )
        IF(I1 /= 0)THEN
           NCNT = NCNT + 1
           TEMP1(NCNT) = I1
           TEMP2(NCNT) = I
        END IF
     END DO
     
     ! SET LOCAL NUMBER OF BOUNDARY NODES
     NG%M = NCNT

     ! SET GLOBAL TO LOCAL MAP FOR THIS DOMAIN
     ALLOCATE(NG%NGID(0:NG%M),stat=status)
     if(status /= 0) call fatal_error("GENMAP_NCNEST: can not allocate:NG%NGID")

     ALLOCATE(NG%NLID(0:NG%M),stat=status)
     if(status /= 0) call fatal_error("GENMAP_NCNEST: can not allocate:NG%NLID")

     NG%NLID = TEMP1(0:NCNT)
     NG%NGID = TEMP2(0:NCNT)

     DEALLOCATE(TEMP1)
     DEALLOCATE(TEMP2)

     !!SET UP LOCAL+HALO NESTING NODES
     ALLOCATE(TEMP1(0:NG%MGL));      TEMP1=0
     ALLOCATE(TEMP2(0:NG%MGL));      TEMP2=0
     
     NCNT = 0
     DO I=1,NG%MGL
        I1 = NLID_X( NID(I) )
        IF(I1  > M)THEN
           NCNT = NCNT + 1
           TEMP1(NCNT) = I1
           TEMP2(NCNT) = I
        END IF
     END DO
     
     ! SET LOCAL NUMBER OF BOUNDARY NODES
     NG%MT = NG%M + NCNT

     ! SET GLOBAL TO LOCAL MAP FOR THIS DOMAIN
     ALLOCATE(NG%NGID_X(0:NG%MT),stat=status)
     if(status /= 0) call fatal_error("GENMAP_NCNEST: can not allocate:NG%NGID")

     ALLOCATE(NG%NLID_X(0:NG%MT),stat=status)
     if(status /= 0) call fatal_error("GENMAP_NCNEST: can not allocate:NG%NLID")

     NG%NLID_X(0:NG%M) = NG%NLID

     NG%NGID_X(0:NG%M) = NG%NGID

     lb = NG%M+1
     ub = NG%MT
     NG%NLID_X(lb:ub) = TEMP1(1:NCNT)

     NG%NGID_X(lb:ub) = TEMP2(1:NCNT)

     DEALLOCATE(TEMP1)
     DEALLOCATE(TEMP2)
     DEALLOCATE(NID)

!============================================
! Make a list of the local Nesting elements
!============================================

     !!SET UP LOCAL NESTING ELEMENTS
     ALLOCATE(TEMP1(0:NG%NGL));      TEMP1=0
     ALLOCATE(TEMP2(0:NG%NGL));      TEMP2=0

     NCNT = 0
     DO I=1,NG%NGL
        I1 =  ELID(ELMS_GL(I)) 
        IF(I1 /= 0)THEN
           NCNT = NCNT + 1
           TEMP1(NCNT) = I1
           TEMP2(NCNT) = I
        END IF
     END DO
     
     ! SET LOCAL NUMBER OF BOUNDARY NODES
     NG%N = NCNT

     ! SET GLOBAL TO LOCAL MAP FOR THIS DOMAIN

     ALLOCATE(NG%EGID(0:NG%N),stat=status)
     if(status /= 0) call fatal_error("GENMAP_NCNEST: can not allocate:NG%EGID")

     ALLOCATE(NG%ELID(0:NG%N),stat=status)
     if(status /= 0) call fatal_error("GENMAP_NCNEST: can not allocate:NG%ELID")

     NG%ELID = TEMP1(0:NCNT)
     NG%EGID = TEMP2(0:NCNT)

     DEALLOCATE(TEMP1)
     DEALLOCATE(TEMP2)



     !!SET UP LOCAL+HALO NESTING ELEMENTS
     ALLOCATE(TEMP1(0:NG%NGL));      TEMP1=0
     ALLOCATE(TEMP2(0:NG%NGL));      TEMP2=0

     NCNT = 0
     DO I=1,NG%NGL
        I1 =  ELID_X(ELMS_GL(I)) 
        IF(I1 > N)THEN
           NCNT = NCNT + 1
           TEMP1(NCNT) = I1
           TEMP2(NCNT) = I
        END IF
     END DO
     
     ! SET LOCAL NUMBER OF BOUNDARY NODES
     NG%NT = NG%N + NCNT

     ! SET GLOBAL TO LOCAL MAP FOR THIS DOMAIN

     ALLOCATE(NG%EGID_X(0:NG%NT),stat=status)
     if(status /= 0) call fatal_error("GENMAP_NCNEST: can not allocate:NG%EGID")

     ALLOCATE(NG%ELID_X(0:NG%NT),stat=status)
     if(status /= 0) call fatal_error("GENMAP_NCNEST: can not allocate:NG%ELID")

     NG%ELID_X(0:NG%N) = NG%ELID

     NG%EGID_X(0:NG%N) = NG%EGID

     lb = NG%N+1
     ub = NG%NT
     NG%ELID_X(lb:ub) = TEMP1(1:NCNT)

     NG%EGID_X(lb:ub) = TEMP2(1:NCNT)
     
     DEALLOCATE(TEMP1)
     DEALLOCATE(TEMP2)
     DEALLOCATE(ELMS_GL)

  END IF

  !==============================================================================|
  !   SET UP ELEMENT MAPPING FOR GLOBAL 2 LOCAL TRANSFER OF BC'S                 | 
  !   BOUNDARY MAP :: BCMAP(NPROCS)                                              |
  !     BCMAP(1-->NPROCS)%NSIZE  :: NUMBER OF BOUNDARY NODES IN EACH DOM         |
  !     BCMAP(1-->NPROCS)%LOC_2_GL(NSIZE) :: LOCAL TO GLOBAL MAPPING IN EACH DOM |
  !==============================================================================|

  ! SET UP TRANSFER FROM GLOBAL NESTING BOUNDARY TO LOCAL NESTING
  ! BOUNDARY - NOT DIRECTLY TO THE LOCAL MESH

  ! ELEMENTS:

  ! ADD THE MAP TO THE LOCAL DOMAIN AND THE ONE2ONE MAP
  E_MAP => MAKE_MAP(MYID,NPROCS,NG%NGL,NT,NG%EGID_X,NG%ELID_X)
!  CALL PRINT_MAP(E_MAP,"GLOBAL 2 GRID")
  CALL ADD_MAP2LIST(INTERNAL_MAPS,E_MAP)
  NULLIFY(E_MAP)

  E_MAP => MAKE_MAP(MYID,NPROCS,NG%NGL,NG%NT,NG%EGID_X)
!  CALL PRINT_MAP(E_MAP,"GLOBAL 2 DATA")
  CALL ADD_MAP2LIST(INTERNAL_MAPS,E_MAP)
  NULLIFY(E_MAP)

  E_MAP => MAKE_MAP(MYID,NPROCS,NG%NGL,N,NG%EGID,NG%ELID)
!  CALL PRINT_MAP(E_MAP,"GLOBAL 2 GRID")
  CALL ADD_MAP2LIST(INTERNAL_MAPS,E_MAP)
  NULLIFY(E_MAP)

  E_MAP => MAKE_MAP(MYID,NPROCS,NG%NGL,NG%N,NG%EGID)
!  CALL PRINT_MAP(E_MAP,"GLOBAL 2 DATA")
  CALL ADD_MAP2LIST(INTERNAL_MAPS,E_MAP)
  NULLIFY(E_MAP)



  ! NODES
  ! ADD THE MAP TO THE LOCAL DOMAIN AND THE ONE2ONE MAP
  N_MAP => MAKE_MAP(MYID,NPROCS,NG%MGL,MT,NG%NGID_X,NG%NLID_X)
  CALL ADD_MAP2LIST(INTERNAL_MAPS,N_MAP)
  NULLIFY(N_MAP)

  N_MAP => MAKE_MAP(MYID,NPROCS,NG%MGL,NG%MT,NG%NGID_X)
  CALL ADD_MAP2LIST(INTERNAL_MAPS,N_MAP)
  NULLIFY(N_MAP)

  N_MAP => MAKE_MAP(MYID,NPROCS,NG%MGL,M,NG%NGID,NG%NLID)
  CALL ADD_MAP2LIST(INTERNAL_MAPS,N_MAP)
  NULLIFY(N_MAP)

  N_MAP => MAKE_MAP(MYID,NPROCS,NG%MGL,NG%M,NG%NGID)
  CALL ADD_MAP2LIST(INTERNAL_MAPS,N_MAP)
  NULLIFY(N_MAP)

  ! TIME

# if defined (WAVE_CURRENT_INTERACTION)
# if defined (WAVE_ONLY)
  allocate(temp1(NCNEST_BLOCKSIZE_WAVE))
  DO I=1,NCNEST_BLOCKSIZE_WAVE
     TEMP1(I)=I
  END DO

  N_MAP => MAKE_MAP(MYID,NPROCS,NCNEST_BLOCKSIZE_WAVE,NCNEST_BLOCKSIZE_WAVE,TEMP1)
# else
  IF(NCNEST_BLOCKSIZE /= NCNEST_BLOCKSIZE_WAVE)CALL FATAL_ERROR(              &
     "NCNEST_BLOCKSIZE /= NCNEST_BLOCKSIZE_WAVE", &
     "Please use NCNEST_BLOCKSIZE == NCNEST_BLOCKSIZE_WAVE for this case.")

  allocate(temp1(NCNEST_BLOCKSIZE))
  DO I=1,NCNEST_BLOCKSIZE
     TEMP1(I)=I
  END DO

  N_MAP => MAKE_MAP(MYID,NPROCS,NCNEST_BLOCKSIZE,NCNEST_BLOCKSIZE,TEMP1)
# endif
# else
  allocate(temp1(NCNEST_BLOCKSIZE))
  DO I=1,NCNEST_BLOCKSIZE
     TEMP1(I)=I
  END DO

  N_MAP => MAKE_MAP(MYID,NPROCS,NCNEST_BLOCKSIZE,NCNEST_BLOCKSIZE,TEMP1)
# endif
!  CALL PRINT_MAP(N_MAP,"TIME MAP")

  CALL ADD_MAP2LIST(INTERNAL_MAPS,N_MAP)
  NULLIFY(N_MAP)
  DEALLOCATE(TEMP1)


  if (dbg_set(dbg_sbr)) &
       & write(IPT,*) "END: GENMAP_NCNEST"   

  RETURN
END SUBROUTINE GENMAP_NCNEST

!==============================================================================|
!==============================================================================|
!==============================================================================|
!==============================================================================|
SUBROUTINE GENMAP_NESTING(NG,LG)
!==============================================================================|
!
! CREATE A GLOBAL TO LOCAL MAP FOR DOMAIN NESTING OUTPUT
! USES DATA READ INTO: 
!                     
! Creates:             MAP LINK LIST ENTRY FOR IO
!                      
!
!==============================================================================|
!  USE MOD_NESTING
  USE MOD_PAR
  USE LIMS
  USE CONTROL
  USE ALL_VARS
  IMPLICIT NONE
  TYPE(GRID) :: NG,LG !The Local Grid, The Nesting Grid, The local Nesting Grid

   integer :: SENDER,RECVER, ierr, I, NCNT, NSZE, I1, status,J

  INTEGER, POINTER :: EID(:),NID(:)

  INTEGER, POINTER :: TEMP1(:),TEMP2(:)

  TYPE(MAP), POINTER :: E_MAP(:),N_MAP(:)

  REAL(SP),POINTER :: E_max(:), N_max(:)

  REAL(SP),POINTER :: RBUF(:)
  
  REAL(SP),POINTER :: DIST(:)

  REAL(SP) :: DMAX
  
  REAL(SP),POINTER :: TEMP3(:),TEMP4(:,:)

  if (dbg_set(dbg_SBR)) &
       & write(IPT,*) "START: GENMAP_NESTING"


  LG%MGL = NG%MGL
  LG%NGL = NG%NGL
  LG%KB = NG%KB
  LG%KBM1 = NG%KBM1
!
!  LG%KB = KB
!  LG%KBM1 = KBM1

  IF(.not. IOPROC) THEN !ONLY THE FVCOM GROUP PROCS CAN DO THIS
     
     !MATCH BASED ON GRID LOCATIONS!
     ! THIS WILL FIND THE LOCAL NODE AND ELEMENT NUMBERS
     ALLOCATE(EID(0:NG%NGL)); EID=0
     ALLOCATE(DIST(0:NT)); DIST = 0.0_SP
     ALLOCATE(E_max(0:NG%NGL)); E_max = 0.0_SP
     DO I = 1,NG%NGL

#  if !defined (SPHERICAL) 
        DIST = abs(XMC-NG%XMC(I)) + abs(YMC-NG%YMC(I))

        J = MINLOC(DIST(1:NT),1)
        
        EID(I)=J
        
        E_max(I) = SQRT((XMC(J)-NG%XMC(I))**2 + (YMC(J)-NG%YMC(I))**2)

# else

        ! THIS IS GOING TO BE SLOW!!!!
        DO J = 1,NT
           CALL ARC(LONC(J),LATC(J),NG%LONC(I),NG%LATC(I),DIST(J))
        END DO
        J = MINLOC(DIST(1:NT),1)
        EID(I)=J
        E_max(I) = DIST(J)
# endif
        
     END DO
     DEALLOCATE(DIST)
     
     ALLOCATE(NID(0:NG%MGL)); NID=0
     ALLOCATE(DIST(0:MT)); DIST = 0.0_SP
     ALLOCATE(N_max(0:NG%MGL));  N_max = 0.0_SP
     DO I = 1,NG%MGL

#  if !defined (SPHERICAL) 
        DIST = abs(XM-NG%XM(I)) + abs(YM-NG%YM(I))
        J = MINLOC(DIST(1:MT),1)
        

        NID(I)=J
        
        N_max(I) = SQRT((XM(J)-NG%XM(I))**2 + (YM(J)-NG%YM(I))**2)

# else

        ! THIS IS GOING TO BE SLOW!!!!
        DO J = 1,MT
           CALL ARC(LON(J),LAT(J),NG%LON(I),NG%LAT(I),DIST(J))
        END DO
        J = MINLOC(DIST(1:MT),1)
        NID(I)=J
        N_max(I) = DIST(J)

# endif
     END DO
     DEALLOCATE(DIST)
     
     !COLLECT NEAREST INFO TO ALL PROCESSORS AND KEEP ONLY YOUR OWN
     
     
     ! CELLS
     ALLOCATE(RBUF(0:NG%NGL))
     CALL MPI_ALLREDUCE(E_max,RBUF,NG%NGL+1,MPI_F,MPI_MIN,MPI_FVCOM_GROUP,IERR)
     
     ! WHERE ANOTHER PROCESSOR HAS A CLOSER VALUE - SET EID = 0
     WHERE(E_max - RBUF /=0.0_SP )
        EID = 0
     END WHERE
     
     E_max = RBUF
     DEALLOCATE(RBUF)
     
     !NODES
     ALLOCATE(RBUF(0:NG%MGL))
     CALL MPI_ALLREDUCE(N_max,RBUF,NG%MGL+1,MPI_F,MPI_MIN,MPI_FVCOM_GROUP,IERR)
     

     ! WHERE ANOTHER PROCESSOR HAS A CLOSER VALUE - SET EID = 0
     WHERE(N_max - RBUF /=0.0_SP )
        NID = 0
     END WHERE
     
     N_max = RBUF
     DEALLOCATE(RBUF)
          
     ! REPORT ANY DISCREPANCY IN GRID LOCATIONS
     Dmax= max(maxval(E_max),maxval(N_max))

     if (dbg_set(dbg_log)) THEN
        IF(Dmax .GT. 0.0_SP )THEN
           write(IPT,*) "! NESTING GRID MATCH ERROR:"
           write(IPT,*) "! Largest Cell Error:",maxval(E_max)
           write(IPT,*) "! Largest Node Error:",maxval(N_max)
 !           write(IPT,*) "! Global Element indices: EGID_X =",EID

        ELSE
           write(IPT,*) "! NESTING GRID MATCH IS EXACT!"
        end if
     END if
     DEALLOCATE(E_MAX,N_MAX)
  
     
!============================================
! Make a list of the local Nesting nodes
!============================================


     ALLOCATE(TEMP1(0:LG%MGL));      TEMP1=0
     ALLOCATE(TEMP2(0:LG%MGL));      TEMP2=0
     ALLOCATE(TEMP3(0:LG%MGL));         TEMP3 = 0.0_SP
     ALLOCATE(TEMP4(0:LG%MGL,LG%KBM1)); TEMP4 = 0.0_SP
     
     NCNT = 0
     !!SET UP LOCAL NESTING NODES
     DO I=1,LG%MGL

        IF(NID(I) /= 0)THEN
           NCNT = NCNT + 1
           TEMP1(NCNT) = NID(I)
           TEMP2(NCNT) = I
	   TEMP3(NCNT)   = NG%H(I)
	   TEMP4(NCNT,:) = NG%ZZ(I,:)
        END IF
     END DO
     
     ! SET LOCAL NUMBER OF NESTING NODES
     LG%MT = NCNT
     LG%M = NCNT

     ! SET GLOBAL TO LOCAL MAP FOR THIS DOMAIN
     ALLOCATE(LG%NGID_X(0:LG%MT),stat=status)
     if(status /= 0) call fatal_error("GENMAP_NESTING: can not allocate:LG%NGID")

     ALLOCATE(LG%NLID_X(0:LG%MT),stat=status)
     if(status /= 0) call fatal_error("GENMAP_NESTING: can not allocate:LG%NLID")

     LG%NLID_X = TEMP1(0:NCNT)

     LG%NGID_X = TEMP2(0:NCNT)
     
     DEALLOCATE(TEMP1)
     DEALLOCATE(TEMP2)

     ALLOCATE(LG%H(0:LG%MT),stat=status)
     if(status /= 0) call fatal_error("GENMAP_NESTING: can not allocate:LG%H")

     ALLOCATE(LG%ZZ(0:LG%MT,LG%KBM1),stat=status)
     if(status /= 0) call fatal_error("GENMAP_NESTING: can not allocate:LG%ZZ")
     
     LG%H = TEMP3(0:NCNT)
     LG%ZZ = TEMP4(0:NCNT,1:KBM1)

     DEALLOCATE(TEMP3)
     DEALLOCATE(TEMP4)

!============================================
! Make a list of the local Nesting elements
!============================================

     ALLOCATE(TEMP1(0:LG%NGL));      TEMP1=0
     ALLOCATE(TEMP2(0:LG%NGL));      TEMP2=0
     ALLOCATE(TEMP3(0:LG%NGL));         TEMP3=0.0_SP
     ALLOCATE(TEMP4(0:LG%NGL,LG%KBM1)); TEMP4=0.0_SP
     
     NCNT = 0
     !!SET UP LOCAL OPEN BOUNDARY NODES
     DO I=1,LG%NGL

        IF(EID(I) /= 0)THEN
           NCNT = NCNT + 1
           TEMP1(NCNT) = EID(I)
           TEMP2(NCNT) = I
           TEMP3(NCNT)   = NG%H1(I)
	   TEMP4(NCNT,:) = NG%ZZ1(I,:)
        END IF
     END DO
     
     ! SET LOCAL NUMBER OF NESTING ELEMENTS
     LG%NT = NCNT
     LG%N = NCNT

     ! SET GLOBAL TO LOCAL MAP FOR THIS DOMAIN
     ALLOCATE(LG%EGID_X(0:LG%NT),stat=status)
     if(status /= 0) call fatal_error("GENMAP_NESTING: can not allocate:LG%NGID")

     ALLOCATE(LG%ELID_X(0:LG%NT),stat=status)
     if(status /= 0) call fatal_error("GENMAP_NESTING: can not allocate:LG%NLID")

     LG%ELID_X = TEMP1(0:NCNT)

     LG%EGID_X = TEMP2(0:NCNT)
     
     DEALLOCATE(TEMP1)
     DEALLOCATE(TEMP2)

     ALLOCATE(LG%H1(0:LG%NT),stat=status)
     if(status /= 0) call fatal_error("GENMAP_NESTING: can not allocate:LG%H1")

     ALLOCATE(LG%ZZ1(0:LG%NT,LG%KBM1),stat=status)
     if(status /= 0) call fatal_error("GENMAP_NESTING: can not allocate:LG%ZZ1")
     
     LG%H1 = TEMP3(0:NCNT)
     LG%ZZ1 = TEMP4(0:NCNT,1:KBM1)

     DEALLOCATE(TEMP3)
     DEALLOCATE(TEMP4)


  END IF ! END IF IOPROC


  !==============================================================================|
  !   SET UP ELEMENT MAPPING FOR GLOBAL 2 LOCAL TRANSFER OF BC'S                 | 
  !   BOUNDARY MAP :: BCMAP(NPROCS)                                              |
  !     BCMAP(1-->NPROCS)%NSIZE  :: NUMBER OF BOUNDARY NODES IN EACH DOM         |
  !     BCMAP(1-->NPROCS)%LOC_2_GL(NSIZE) :: LOCAL TO GLOBAL MAPPING IN EACH DOM |
  !==============================================================================|

  ! ELEMENTS:
  E_MAP => MAKE_MAP(MYID,NPROCS,LG%NGL,LG%NT,LG%EGID_X)
  CALL ADD_MAP2LIST(INTERNAL_MAPS,E_MAP)
  NULLIFY(E_MAP)

  

  ! NODES
  N_MAP => MAKE_MAP(MYID,NPROCS,LG%MGL,LG%MT,LG%NGID_X)
  CALL ADD_MAP2LIST(INTERNAL_MAPS,N_MAP)
  NULLIFY(N_MAP)

! DO NOT DEALLOCATE MAP!!!

  if (dbg_set(dbg_sbr)) &
       & write(IPT,*) "END: GENMAP_NESTING"   
  RETURN
END SUBROUTINE GENMAP_NESTING
  !============================================================================!
  !
  !============================================================================!   
  SUBROUTINE ALLOC_NEST ! CALL IN ALLOCATE ALL - USE THE MEM_COUNT!
    IMPLICIT NONE
    TYPE(GRID), POINTER :: G
    TYPE(NEST_DATA), POINTER :: D
    INTEGER :: STATUS,I
    INTEGER :: NDB
    !==============================================================================!
# if !defined (DOUBLE_PRECISION)
    NDB = 1       !!GWC BASE THIS ON KIND
# else
    NDB = 2
# endif

    IF(DBG_SET(DBG_SBR))WRITE(IPT,*) "START ALLOC_NEST" 

    IF(NESTING_ON) THEN

       G => NESTING_GRID

       ! ALLOCATE SPACE FOR INTERPOLATED DATA
       ALLOCATE(UA_NEST(0:G%NT),stat=status)
       if(status /= 0) call fatal_error("ALLOC_NEST: can not allocate:UA_NEST")

       ALLOCATE(VA_NEST(0:G%NT),stat=status)
       if(status /= 0) call fatal_error("ALLOC_NEST: can not allocate:VA_NEST")

       ALLOCATE(EL_NEST(0:G%MT),stat=status)
       if(status /= 0) call fatal_error("ALLOC_NEST: can not allocate:EL_NEST")

       ALLOCATE(U_NEST(0:G%NT,KB),stat=status)
       if(status /= 0) call fatal_error("ALLOC_NEST: can not allocate:U_NEST")

       ALLOCATE(V_NEST(0:G%NT,KB),stat=status)
       if(status /= 0) call fatal_error("ALLOC_NEST: can not allocate:V_NEST")

       ALLOCATE(S1_NEST(0:G%MT,KB),stat=status)
       if(status /= 0) call fatal_error("ALLOC_NEST: can not allocate:S1_NEST")

       ALLOCATE(T1_NEST(0:G%MT,KB),stat=status)
       if(status /= 0) call fatal_error("ALLOC_NEST: can not allocate:T1_NEST")

       ALLOCATE(HYW_NEST(0:G%MT,KB),stat=status)
       if(status /= 0) call fatal_error("ALLOC_NEST: can not allocate:HYW_NEST")

       ALLOCATE(WCELL_NEST(0:G%NT),stat=status)
       if(status /= 0) call fatal_error("ALLOC_NEST: can not allocate:WCELL_NEST")
      
       ALLOCATE(WNODE_NEST(0:G%MT),stat=status)
       if(status /= 0) call fatal_error("ALLOC_NEST: can not allocate:WNODE_NEST")

       MEMCNT = MEMCNT + G%NT * (2 + 2*KB)* NDB + G%MT * (1 + 3*KB)* NDB
# if defined (FABM)
       IF(FABM_NESTING_ON) THEN
          ALLOCATE(FABM_ALL_NEST(0:G%MT,KB,IFABMadv),stat=status) !RJT restricting nesting actions to advectable variables only
!          ALLOCATE(FABM_ALL_NEST(0:G%MT,KB,size(model%interior_state_variables)),stat=status)
          if(status /= 0) call fatal_error("ALLOC_NEST: can not allocate:FABM_ALL_NEST")
!          ALLOCATE(FABM_ALL_BLK(0:G%MT,KB,NESTING_BLOCKSIZE,size(model%interior_state_variables)),stat=status)
!          if(status /= 0) call fatal_error("ALLOC_NEST: can not allocate:FABM_ALL_BLK")
       END IF

       MEMCNT = MEMCNT + G%MT * KB * IFABMadv
# endif

      ! ALLOCATE SPACE FOR INTERPOLATED DATA
       ALLOCATE(U_NEST_L(0:G%NT,KB_L),stat=status)
       if(status /= 0) call fatal_error("ALLOC_NEST: can not allocate:U_NEST_L")

       ALLOCATE(V_NEST_L(0:G%NT,KB_L),stat=status)
       if(status /= 0) call fatal_error("ALLOC_NEST: can not allocate:V_NEST_L")

       ALLOCATE(S1_NEST_L(0:G%MT,KB_L),stat=status)
       if(status /= 0) call fatal_error("ALLOC_NEST: can not allocate:S1_NEST_L")

       ALLOCATE(T1_NEST_L(0:G%MT,KB_L),stat=status)
       if(status /= 0) call fatal_error("ALLOC_NEST: can not allocate:T1_NEST_L")

       ALLOCATE(HYW_NEST_L(0:G%MT,KB_L),stat=status)
       if(status /= 0) call fatal_error("ALLOC_NEST: can not allocate:HYW_NEST_L")

       MEMCNT = MEMCNT + G%NT * 2*KB_L * NDB + G%MT * 3*KB_L * NDB

       

       ! ALLOCATE SPACE FOR DATA BLOCK IO FILE
       
       ALLOCATE(NESTING_DATA,STAT=status)
       IF(STATUS /=0) CALL FATAL_ERROR("COULD NOT ALLOCATE NESTING_DATA!")

       D => NESTING_DATA

       D%NBLK = NESTING_BLOCKSIZE

       ! ALLOCATE SPACE FOR DATA BLOCK IO FILE
       ALLOCATE(D%UA_BLK(0:G%NT,NESTING_BLOCKSIZE),stat=status)
       if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:UA_BLK")
       
       ALLOCATE(D%VA_BLK(0:G%NT,NESTING_BLOCKSIZE),stat=status)
       if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:VA_BLK")
       
       ALLOCATE(D%EL_BLK(0:G%MT,NESTING_BLOCKSIZE),stat=status)
       if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:EL_BLK")
       
       ALLOCATE(D%U_BLK(0:G%NT,KB_L,NESTING_BLOCKSIZE),stat=status)
       if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:U_BLK")
       
       ALLOCATE(D%V_BLK(0:G%NT,KB_L,NESTING_BLOCKSIZE),stat=status)
       if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:V_BLK")
       
       ALLOCATE(D%S1_BLK(0:G%MT,KB_L,NESTING_BLOCKSIZE),stat=status)
       if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:S1_BLK")
       
       ALLOCATE(D%T1_BLK(0:G%MT,KB_L,NESTING_BLOCKSIZE),stat=status)
       if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:T1_BLK")

       ALLOCATE(D%HYW_BLK(0:G%MT,KB_L,NESTING_BLOCKSIZE),stat=status)
       if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:HYW_BLK")

# if defined (FABM)
       IF(FABM_NESTING_ON) THEN
          ALLOCATE(D%FABM_ALL_BLK(0:G%MT,KB_L,NESTING_BLOCKSIZE,IFABMadv),stat=status) !RJT restricting nesting actions to advectable variables only
          if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:D%FABM_ALL_BLK")
         ALLOCATE(D%FABM_ALL_TMP(0:G%MT,KB_L,NESTING_BLOCKSIZE),stat=status)
         if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:FABM_ALL_TMP")
       END IF
# endif
       
       ALLOCATE(D%TIME_BLK(NESTING_BLOCKSIZE),stat=status)
       if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:TIME_BLK")

       ALLOCATE(D%WCELL_BLK(0:G%NT,NESTING_BLOCKSIZE),stat=status)
       if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:WCELL_BLK")

       ALLOCATE(D%WNODE_BLK(0:G%MT,NESTING_BLOCKSIZE),stat=status)
       if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:WNODE_BLK")

       MEMCNT = MEMCNT + G%NT * (2 + 2*KB_L)* NDB * NESTING_BLOCKSIZE &
            & +  G%MT * (1 + KB + 2*KB_L)* NDB * NESTING_BLOCKSIZE

    END IF

    IF(NCNEST_ON) THEN
       

       ALLOCATE(NCNEST_DATA(NCNEST_NUM),STAT=status)
       IF(STATUS /=0) CALL FATAL_ERROR("COULD NOT ALLOCATE NCNEST_DATA!")
       
       

       DO I = 1, NCNEST_NUM


         G=> NCNEST_GRIDS(I)
         D => NCNEST_DATA(I)

         D%NBLK = NCNEST_BLOCKSIZE

         ! ALLOCATE SPACE FOR DATA BLOCK IO FILE
         ALLOCATE(D%UA_BLK(0:G%N,NCNEST_BLOCKSIZE),stat=status)
         if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:UA_BLK")
         
         ALLOCATE(D%VA_BLK(0:G%N,NCNEST_BLOCKSIZE),stat=status)
         if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:VA_BLK")
         
         ALLOCATE(D%EL_BLK(0:G%M,NCNEST_BLOCKSIZE),stat=status)
         if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:EL_BLK")
         
         ALLOCATE(D%U_BLK(0:G%N,KB,NCNEST_BLOCKSIZE),stat=status)
         if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:U_BLK")
         
         ALLOCATE(D%V_BLK(0:G%N,KB,NCNEST_BLOCKSIZE),stat=status)
         if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:V_BLK")
         
         ALLOCATE(D%S1_BLK(0:G%M,KB,NCNEST_BLOCKSIZE),stat=status)
         if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:S1_BLK")
         
         ALLOCATE(D%T1_BLK(0:G%M,KB,NCNEST_BLOCKSIZE),stat=status)
         if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:T1_BLK")
                  
         ALLOCATE(D%HYW_BLK(0:G%M,KB,NCNEST_BLOCKSIZE),stat=status)
         if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:HYW_BLK")
         
         ALLOCATE(D%TIME_BLK(NCNEST_BLOCKSIZE),stat=status)
         if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:TIME_BLK")

         ALLOCATE(D%WCELL_BLK(0:G%N,NCNEST_BLOCKSIZE),stat=status)
         if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:WCELL_BLK")
         
         ALLOCATE(D%WNODE_BLK(0:G%M,NCNEST_BLOCKSIZE),stat=status)
         if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:WNODE_BLK")
# if defined (FABM)
!       IF(FABM_NCNEST_ON) THEN ! active as long as NCNEST is on
          ALLOCATE(D%FABM_ALL_BLK(0:G%M,KB,NCNEST_BLOCKSIZE,IFABMadv),stat=status)! RJT Use only advectable variables
          if(status /= 0) call fatal_error("ALLOC_NEST: can not allocate:D%FABM_ALL_BLK")
         MEMCNT = MEMCNT + G%M * KB * IFABMadv * NCNEST_BLOCKSIZE
!       END IF
# endif
         MEMCNT = MEMCNT + G%N * (2 + 2*KB)* NDB * NCNEST_BLOCKSIZE &
              & +  G%M * (1 + 3*KB)* NDB * NCNEST_BLOCKSIZE
         
      END DO

   END IF

    IF(DBG_SET(DBG_SBR))WRITE(IPT,*) "END ALLOC_NEST" 
   
   RETURN
 END SUBROUTINE ALLOC_NEST

  !==================================================================================!
  !
  !==================================================================================!
  Subroutine SETUP_NEST
    USE MOD_SETUP
    IMPLICIT NONE

    TYPE(NCDIM), POINTER :: DIM
    TYPE(NCVAR), POINTER :: VAR
    TYPE(NCATT), POINTER :: ATT
    LOGICAL :: FOUND

    TYPE(TIME):: TIMETEST

    ! ARRAY TO HOLD TIME DATA
    INTEGER, POINTER :: INT_PNT(:)
    
    INTEGER :: I,status
# if defined (FABM)
    INTEGER :: IFABM
# endif
    
    IF(DBG_SET(DBG_SBR))WRITE(IPT,*) "START SETUP_NEST" 

    IF(NCNEST_ON)THEN

       ! SET UP THE GRID INDEX FOR OUTPUT
       DO I = 1, NCNEST_NUM
          CALL SET_SUBDOMAIN_GRID(NCNEST_GRIDS(I))
       END DO

       CALL SETUP_NCNEST_FILE
       

    END IF

    IF(NESTING_ON) THEN

       !=======================
       ! SANITY CHECK
       !=======================
       DIM => FIND_UNLIMITED(NESTING_FILE,FOUND)
       IF(.not. FOUND) CALL FATAL_ERROR &
            & ("IN NESTING FILE OBJECT",&
            & "FILE NAME: "//TRIM(NESTING_FILE%FNAME),&
            &"COULD NOT FIND THE UNLIMITED DIMENSION")

       TIMETEST = get_file_time(NESTING_FILE,1)

       IF(StartTime < TIMETEST) THEN
          
          CALL PRINT_TIME(TIMETEST,IPT,"NESTING FILE STARTS")
          CALL PRINT_TIME(STARTTIME,IPT,"STARTTIME")
          
          CALL FATAL_ERROR&
               &("THE MODEL START TIME DOES NOT MATCH THE NESTING FILE START TIME")
       END IF
       
       ! TEST THE END TIME
       TIMETEST = get_file_time(NESTING_FILE,DIM%DIM)
       IF(TIMETEST < EndTime) CALL WARNING &
            &("MODEL END TIME EXCEEDS THE NESTING FILE END TIME",&
            & "THIS WILL CAUSE AN ERROR IF THE SUBDOMAIN MODEL OUTPACES",&
            & "THE LARGE DOMAIN MODEL AND NO NEW NESTIND DATA IS AVAILABLE!")
       
       !!========================================
       !! CONNECT MEMORY AND READ FIRST TIME STEP
       !!=======================================

       ! FIND CORRECT TIME
       CALL UPDATE_FILE_BRACKET(NESTING_FILE,StartTime,status)

       IF(NESTING_FILE%FTIME%NEXT_IO > StartTime) THEN
          NESTING_DATA%RNG(1)=NESTING_FILE%FTIME%PREV_STKCNT
          NESTING_DATA%RNG(2)=NESTING_FILE%FTIME%PREV_STKCNT+NESTING_BLOCKSIZE-1
       ELSE
          NESTING_DATA%RNG(1)=NESTING_FILE%FTIME%NEXT_STKCNT
          NESTING_DATA%RNG(2)=NESTING_FILE%FTIME%NEXT_STKCNT+NESTING_BLOCKSIZE-1
       END IF


       VAR => FIND_VAR(NESTING_FILE,"zeta",FOUND)
       IF(.not. FOUND) CALL FATAL_ERROR &
            & ("IN NESTING FILE OBJECT NO 'zeta' FOUND?")
       CALL NC_CONNECT_PVAR(VAR, NESTING_DATA%EL_BLK)
       CALL NC_READ_VAR(VAR,STKRNG=NESTING_DATA%RNG)
       VAR_EL => VAR

       VAR => FIND_VAR(NESTING_FILE,"ua",FOUND)
       IF(.not. FOUND) CALL FATAL_ERROR &
            & ("IN NESTING FILE OBJECT NO 'ua' FOUND?")
       CALL NC_CONNECT_PVAR(VAR, NESTING_DATA%UA_BLK)
       CALL NC_READ_VAR(VAR,STKRNG=NESTING_DATA%RNG)
       VAR_UA => VAR
       
       VAR => FIND_VAR(NESTING_FILE,"va",FOUND)
       IF(.not. FOUND) CALL FATAL_ERROR &
            & ("IN NESTING FILE OBJECT NO 'va' FOUND?")
       CALL NC_CONNECT_PVAR(VAR, NESTING_DATA%VA_BLK)
       CALL NC_READ_VAR(VAR,STKRNG=NESTING_DATA%RNG)
       VAR_VA=> VAR

       VAR => FIND_VAR(NESTING_FILE,"u",FOUND)
       IF(.not. FOUND) CALL FATAL_ERROR &
            & ("IN NESTING FILE OBJECT NO 'u' FOUND?")
       CALL NC_CONNECT_PVAR(VAR, NESTING_DATA%U_BLK)
       CALL NC_READ_VAR(VAR,STKRNG=NESTING_DATA%RNG)
       VAR_U=> VAR

       VAR => FIND_VAR(NESTING_FILE,"v",FOUND)
       IF(.not. FOUND) CALL FATAL_ERROR &
            & ("IN NESTING FILE OBJECT NO 'v' FOUND?")
       CALL NC_CONNECT_PVAR(VAR, NESTING_DATA%V_BLK)
       CALL NC_READ_VAR(VAR,STKRNG=NESTING_DATA%RNG)
       VAR_V=> VAR

       VAR => FIND_VAR(NESTING_FILE,"temp",FOUND)
       IF(.not. FOUND) CALL FATAL_ERROR &
            & ("IN NESTING FILE OBJECT NO 'temp' FOUND?")
       CALL NC_CONNECT_PVAR(VAR, NESTING_DATA%T1_BLK)
       CALL NC_READ_VAR(VAR,STKRNG=NESTING_DATA%RNG)
       VAR_T1=> VAR

       VAR => FIND_VAR(NESTING_FILE,"salinity",FOUND)
       IF(.not. FOUND) CALL FATAL_ERROR &
            & ("IN NESTING FILE OBJECT NO 'salinity' FOUND?")
       CALL NC_CONNECT_PVAR(VAR, NESTING_DATA%S1_BLK)
       CALL NC_READ_VAR(VAR,STKRNG=NESTING_DATA%RNG)
       VAR_S1=> VAR

       VAR => FIND_VAR(NESTING_FILE,"hyw",FOUND)
       IF(.not. FOUND) CALL FATAL_ERROR &
            & ("IN NESTING FILE OBJECT NO 'hyw' FOUND?")
       CALL NC_CONNECT_PVAR(VAR, NESTING_DATA%HYW_BLK)
       CALL NC_READ_VAR(VAR,STKRNG=NESTING_DATA%RNG)
       VAR_HYW=> VAR

# if defined (FABM)
       IF(FABM_NESTING_ON) THEN
         IFABMnest = 0 ! Initialise count of nesting variables present in file
         IFABMnest_idx = IFABMadv_idx
       DO IFABM = 1,IFABMadv
!         WRITE (6,*) 'Index ', IFABM,IFABMadv_idx(IFABM), 'Name ',TRIM(model%interior_state_variables(IFABMadv_idx(IFABM))%name)
         VAR => FIND_VAR(NESTING_FILE,TRIM(model%interior_state_variables(IFABMadv_idx(IFABM))%name),FOUND) ! RJT looping only through advectable variables
! If the variable is in the file
         IF(FOUND) THEN
! increase counter
            IFABMnest = IFABMnest + 1
! populate variable mapping index
            IFABMnest_idx(IFABMnest) = IFABMadv_idx(IFABM)
            CALL NC_CONNECT_PVAR(VAR, NESTING_DATA%FABM_ALL_TMP)
            CALL NC_READ_VAR(VAR,STKRNG=NESTING_DATA%RNG)
            NESTING_DATA%FABM_ALL_BLK(:,:,:,IFABMnest)=NESTING_DATA%FABM_ALL_TMP
! Report variable to log file
            WRITE(IPT,*) "Found Variable ",TRIM(model%interior_state_variables(IFABMadv_idx(IFABM))%name) , " in nesting file, with nesting index ", IFABMnest
         END IF
       END DO
       END IF
# endif

! songhu
       IF(TRIM(NESTING_TYPE) == '3' .OR. TRIM(NESTING_TYPE) == '4')THEN

       VAR => FIND_VAR(NESTING_FILE,"weight_cell",FOUND)
       IF(.not. FOUND) CALL FATAL_ERROR &
            & ("IN NESTING FILE OBJECT NO 'weight_cell' FOUND?")
       CALL NC_CONNECT_PVAR(VAR, NESTING_DATA%WCELL_BLK)
       CALL NC_READ_VAR(VAR,STKRNG=NESTING_DATA%RNG)
       VAR_WCELL => VAR
       VAR => FIND_VAR(NESTING_FILE,"weight_node",FOUND)
       IF(.not. FOUND) CALL FATAL_ERROR &
            & ("IN NESTING FILE OBJECT NO 'weight_node' FOUND?")
       CALL NC_CONNECT_PVAR(VAR, NESTING_DATA%WNODE_BLK)
       CALL NC_READ_VAR(VAR,STKRNG=NESTING_DATA%RNG)
       VAR_WNODE => VAR
       
       END IF

       ALLOCATE(INT_PNT(NESTING_BLOCKSIZE))
       VAR => FIND_VAR(NESTING_FILE,"Itime",FOUND)
       IF(.not. FOUND) CALL FATAL_ERROR &
            & ("IN NESTING FILE OBJECT NO 'Itime' FOUND?")
       CALL NC_CONNECT_PVAR(VAR, INT_PNT)
       CALL NC_READ_VAR(VAR,STKRNG=NESTING_DATA%RNG)
       VAR_TIME1=> VAR

       NULLIFY(INT_PNT)
       ALLOCATE(INT_PNT(NESTING_BLOCKSIZE))
       VAR => FIND_VAR(NESTING_FILE,"Itime2",FOUND)
       IF(.not. FOUND) CALL FATAL_ERROR &
            & ("IN NESTING FILE OBJECT NO 'Itime2' FOUND?")
       CALL NC_CONNECT_PVAR(VAR, INT_PNT)
       CALL NC_READ_VAR(VAR,STKRNG=NESTING_DATA%RNG)
       VAR_TIME2=> VAR

    END IF
    IF(DBG_SET(DBG_SBR))WRITE(IPT,*) "END SETUP_NEST" 


  END Subroutine SETUP_NEST
  !==================================================================================!
  !
  !==================================================================================!
  FUNCTION NESTING_FILE_OBJECT(D) RESULT(NCF)
    USE MOD_CLOCK
# if defined (FABM)
 USE MOD_FABM_DATA
!   USE fabm_types, ONLY: output_none
# endif 
    IMPLICIT NONE
    TYPE(NEST_DATA),POINTER::D

    INTEGER :: status,II
    TYPE(NCFILE), POINTER :: NCF
    TYPE(NCVAR),  POINTER :: VAR
    TYPE(NCATT),  POINTER :: ATT
# if defined (FABM)
    REAL(SP), POINTER :: pdata3(:,:,:)
    TYPE(NCFILE), POINTER :: NCF_FABM 
# endif 


    IF(DBG_SET(DBG_SBR))WRITE(IPT,*) "START NESTING_FILE_OBJECT" 

    IF(IOPROC)THEN

       ALLOCATE(D%EL_BLK(Dim_node%dim,NCNEST_BLOCKSIZE),STAT=STATUS) 
       IF (STATUS /=0 )  &
            & CALL FATAL_ERROR("COULD NOT ALLOCATE MEMORY ON IO PROC FOR OUTPUT DATA:EL_BLK")
       D%EL_BLK = 0.0_SP

       ALLOCATE(D%UA_BLK(Dim_nele%dim,NCNEST_BLOCKSIZE),STAT=STATUS) 
       IF (STATUS /=0 )  &
            & CALL FATAL_ERROR("COULD NOT ALLOCATE MEMORY ON IO PROC FOR OUTPUT DATA:UA_BLK")
       D%UA_BLK = 0.0_SP

       ALLOCATE(D%VA_BLK(Dim_nele%dim,NCNEST_BLOCKSIZE),STAT=STATUS) 
       IF (STATUS /=0 )  &
            & CALL FATAL_ERROR("COULD NOT ALLOCATE MEMORY ON IO PROC FOR OUTPUT DATA:VA_BLK")
       D%VA_BLK = 0.0_SP

       ALLOCATE(D%U_BLK(Dim_nele%dim,KB,NCNEST_BLOCKSIZE),STAT=STATUS) 
       IF (STATUS /=0 )  &
            & CALL FATAL_ERROR("COULD NOT ALLOCATE MEMORY ON IO PROC FOR OUTPUT DATA:U_BLK")
       D%U_BLK = 0.0_SP

       ALLOCATE(D%V_BLK(Dim_nele%dim,KB,NCNEST_BLOCKSIZE),STAT=STATUS) 
       IF (STATUS /=0 )  &
            & CALL FATAL_ERROR("COULD NOT ALLOCATE MEMORY ON IO PROC FOR OUTPUT DATA:V_BLK")
       D%V_BLK = 0.0_SP

       ALLOCATE(D%S1_BLK(Dim_node%dim,KB,NCNEST_BLOCKSIZE),STAT=STATUS) 
       IF (STATUS /=0 )  &
            & CALL FATAL_ERROR("COULD NOT ALLOCATE MEMORY ON IO PROC FOR OUTPUT DATA:S1_BLK")
       D%S1_BLK = 0.0_SP

       ALLOCATE(D%T1_BLK(Dim_node%dim,KB,NCNEST_BLOCKSIZE),STAT=STATUS) 
       IF (STATUS /=0 )  &
            & CALL FATAL_ERROR("COULD NOT ALLOCATE MEMORY ON IO PROC FOR OUTPUT DATA:T1_BLK")
       D%T1_BLK = 0.0_SP

       ALLOCATE(D%HYW_BLK(Dim_node%dim,KB,NCNEST_BLOCKSIZE),STAT=STATUS) 
       IF (STATUS /=0 )  &
            & CALL FATAL_ERROR("COULD NOT ALLOCATE MEMORY ON IO PROC FOR OUTPUT DATA:HYW_BLK")
       D%HYW_BLK = 0.0_SP

       ALLOCATE(D%WCELL_BLK(Dim_nele%dim,NCNEST_BLOCKSIZE),STAT=STATUS)
       IF (STATUS /=0 )  &
            & CALL FATAL_ERROR("COULD NOT ALLOCATE MEMORY ON IO PROC FOR OUTPUT DATA:WCELL_BLK")
       D%WCELL_BLK = 0.0_SP
       ALLOCATE(D%WNODE_BLK(Dim_node%dim,NCNEST_BLOCKSIZE),STAT=STATUS)
       IF (STATUS /=0 )  &
            & CALL FATAL_ERROR("COULD NOT ALLOCATE MEMORY ON IO PROC FOR OUTPUT DATA:WNODE_BLK")
       D%WNODE_BLK = 0.0_SP
# if defined (FABM)
!       IF(FABM_NCNEST_ON) THEN
! this needs a variable with size IFABM... 
         ALLOCATE(D%FABM_ALL_BLK(Dim_node%dim,KB,NCNEST_BLOCKSIZE,IFABMadv),STAT=STATUS)
         IF (STATUS /=0 )  &
            & CALL FATAL_ERROR("COULD NOT ALLOCATE MEMORY ON IO PROC FOR OUTPUT DATA: D%FABM_ALL_BLK")
         D%FABM_ALL_BLK = 0.0_SP
!        END IF
# endif
    END IF

    NCF => NEW_FILE()

    ! nest zeta
    VAR  => NC_MAKE_PVAR(name='zeta',&
         & values=D%EL_BLK, DIM1= Dim_node, DIM2 = DIM_TIME)

    ATT  => NC_MAKE_ATT(name='long_name',values='Water Surface Elevation') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='units',values='meters') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='positive',values='up') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='standard_name',values='sea_surface_elevation') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='grid',values='SSH_Mesh') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='coordinates',values=CoordVar) 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='type',values='data') 
    VAR  => ADD(VAR,ATT)

    NCF  => ADD(NCF,VAR)

    ! UA_BLK
    VAR  => NC_MAKE_PVAR(name='ua',&
         & values=D%UA_BLK, DIM1= Dim_nele, DIM2= DIM_TIME)

    ATT  => NC_MAKE_ATT(name='long_name',values='Vertically Averaged x-velocity') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='units',values='meters s-1') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='grid',values='fvcom_grid') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='type',values='data') 
    VAR  => ADD(VAR,ATT)

    NCF  => ADD(NCF,VAR)


    ! VA_BLK
    VAR  => NC_MAKE_PVAR(name='va',&
         & values=D%VA_BLK, DIM1= Dim_nele, DIM2= DIM_TIME)

    ATT  => NC_MAKE_ATT(name='long_name',values='Vertically Averaged y-velocity') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='units',values='meters s-1') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='grid',values='fvcom_grid') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='type',values='data') 
    VAR  => ADD(VAR,ATT)

    NCF  => ADD(NCF,VAR)

    ! U_BLK
    VAR  => NC_MAKE_PVAR(name='u', values=D%U_BLK, DIM1= Dim_nele,&
         & DIM2= DIM_siglay, DIM3= DIM_TIME)
    ATT  => NC_MAKE_ATT(name='long_name',values='Eastward Water Velocity') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='units',values='meters s-1') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='grid',values='fvcom_grid') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='type',values='data') 
    VAR  => ADD(VAR,ATT)

    NCF  => ADD(NCF,VAR)

    ! V_BLK
    VAR  => NC_MAKE_PVAR(name='v', values=D%V_BLK, DIM1= Dim_nele, &
         & DIM2= DIM_siglay, DIM3 = DIM_TIME)

    ATT  => NC_MAKE_ATT(name='long_name',values='Northward Water Velocity') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='units',values='meters s-1') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='grid',values='fvcom_grid') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='type',values='data') 
    VAR  => ADD(VAR,ATT)

    NCF  => ADD(NCF,VAR)

    ! T_BLK
    VAR  => NC_MAKE_PVAR(name='temp',&
         & values=D%T1_BLK, DIM1= Dim_node, DIM2= DIM_siglay, DIM3 = Dim_time)

    ATT  => NC_MAKE_ATT(name='long_name',values='temperature') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='standard_name',values='sea_water_temperature') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='units',values='degrees_C') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='grid',values='fvcom_grid') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='coordinates',values=CoordVar) 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='type',values='data') 
    VAR  => ADD(VAR,ATT)    

    NCF  => ADD(NCF,VAR)


    ! S_BLK
    VAR  => NC_MAKE_PVAR(name='salinity',&
         & values=D%S1_BLK, DIM1= Dim_node, DIM2= DIM_siglay, DIM3 = Dim_time)

    ATT  => NC_MAKE_ATT(name='long_name',values='salinity') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='standard_name',values='sea_water_salinity') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='units',values='1e-3') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='grid',values='fvcom_grid') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='coordinates',values=CoordVar) 
    VAR  => ADD(VAR,ATT)    

    ATT  => NC_MAKE_ATT(name='type',values='data') 
    VAR  => ADD(VAR,ATT)

    NCF  => ADD(NCF,VAR)

    ! HYW_BLK
    VAR  => NC_MAKE_PVAR(name='hyw',&
         & values=D%HYW_BLK, DIM1= Dim_node, DIM2= DIM_siglev, DIM3 = Dim_time)

    ATT  => NC_MAKE_ATT(name='long_name',values='hydro static vertical&
         & velocity') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='units',values='m/s') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='grid',values='fvcom_grid') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='coordinates',values=CoordVar) 
    VAR  => ADD(VAR,ATT)    

    ATT  => NC_MAKE_ATT(name='type',values='data') 
    VAR  => ADD(VAR,ATT)

    NCF  => ADD(NCF,VAR)
! Can I do this??
#   if defined (FABM)
   DO II=1,IFABMadv
       pdata3 => D%FABM_ALL_BLK(1:,:,:,II)
       VAR => ADD_FABM_VARIABLE(model%interior_state_variables(IFABMadv_idx(II)),pdata3=pdata3)
       NCF  => ADD(NCF,VAR)
   write(ipt,*) "Done adding variable ,",trim(model%interior_state_variables(II)%name)
   END DO
#   endif




    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END NESTING_FILE_OBJECT"

  END FUNCTION NESTING_FILE_OBJECT
  !==================================================================================!
  !
  !===========================================================================!  
  SUBROUTINE ARCHIVE_NEST
    USE MOD_SET_TIME, ONLY : GET_OUTPUT_FILE_INTERVAL
    IMPLICIT NONE
    INTEGER :: STATUS,I
    TYPE(NEST_DATA), POINTER :: D
    TYPE(GRID), POINTER :: G
    TYPE(TIME) :: INTERVAL_TIME

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "START ARCHIVE_NEST"

    
    ! SETUP THE OUTPUT IF THIS HAS NOT BEEN DONE YET
    if(NEED_INIT_NEST) then
       IF(USE_MPI_IO_MODE) THEN
          CALL MPI_IO_SYNCHRONIZE(INITNEST_CODE)
       ELSE
          CALL CALL_FUNC(INITNEST_CODE,status)
          IF (status/=0) call fatal_error("ARCHIVE:: Bad INITNEST_CODE",&
               & "Could not retrieve valid function pointer?")
       END IF
       
       NEED_INIT_NEST = .FALSE.
       NCNEST_CMD_DUMP = .FALSE.
    end if

    ! EXIT EARLY IF WE ARE ONLY SETTING UP THE NESTING INPUT
    IF(.not. NCNEST_ON) RETURN

    IF(MOD((EndTime - StartTime),INTERVAL_TIME_NCNEST*NCNEST_BLOCKSIZE) /= ZeroTime) &
          CALL FATAL_ERROR("The simulation time (EndTime-StartTime) should be divided", &
	    " by INTERVAL_TIME_NCNEST*NCNEST_BLOCKSIZE")
    CALL GET_OUTPUT_FILE_INTERVAL(TRIM(RST_OUT_INTERVAL),INTERVAL_TIME)
    IF(MOD(INTERVAL_TIME,INTERVAL_TIME_NCNEST*NCNEST_BLOCKSIZE) /= ZeroTime) &
          CALL FATAL_ERROR("The restart interval (RST_OUT_INTERVAL) should be divided", &
            " by INTERVAL_TIME_NCNEST*NCNEST_BLOCKSIZE")

    ! STORE DATA AT TIME INTERVAL TIME_INTERVAL
    IF(TIME_INTERVAL <= IntTime)THEN
     DO I =1,NCNEST_NUM
      D=>NCNEST_DATA(I)
      G=>NCNEST_GRIDS(I)
      CALL ASSIGN2BLOCK(D,G)
     END DO
     TIME_INTERVAL = IntTime + INTERVAL_TIME_NCNEST
    
    ! WRITE THE FILE IF IT IS TIME
    IF(NCNEST_DATA(1)%IDX == NCNEST_BLOCKSIZE .or.    &
       IntTime >= EndTime .or. NCNEST_CMD_DUMP) then
       
       IF(USE_MPI_IO_MODE) THEN
          CALL MPI_IO_SYNCHRONIZE(NESTING_CODE)
       ELSE
          CALL CALL_FUNC(NESTING_CODE,status)
          IF (status/=0) call fatal_error("ARCHIVE:: Bad NESTING_CODE",&
               & "Could not retrieve valid function pointer?")
       END IF

       DO I =1,NCNEST_NUM
          D=>NCNEST_DATA(I)
          CALL RESET_BLOCK(D)
       END DO
       
       
    END IF
    END IF

!    ! WRITE THE FILE AT THE END OF RUN, OR IF THE RESTART FILE JUST GOT DUMPED!
!    IF(IntTime >= EndTime .or. NCNEST_CMD_DUMP) then
       
!      DO I =1,NCNEST_NUM
!        D=>NCNEST_DATA(I)
!        G=>NCNEST_GRIDS(I)
!        CALL ASSIGN2BLOCK(D,G)
!      END DO
      
!      IF(USE_MPI_IO_MODE) THEN
!        CALL MPI_IO_SYNCHRONIZE(NESTING_CODE)
!      ELSE
!        CALL CALL_FUNC(NESTING_CODE,status)
!        IF (status/=0) call fatal_error("ARCHIVE:: Bad NESTING_CODE",&
!            & "Could not retrieve valid function pointer?")
!      END IF

!      DO I =1,NCNEST_NUM
!        D=>NCNEST_DATA(I)
!        CALL RESET_BLOCK(D)
!      END DO
       
!    END IF

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END ARCHIVE_NEST"
  END SUBROUTINE ARCHIVE_NEST
  !==========================================================================!
  !
  !========================================================================!
  SUBROUTINE DUMP_NCNEST_FILE
    IMPLICIT NONE
    INTEGER ::I,J,IERR
    LOGICAL :: FOUND
    TYPE(NCFILE), POINTER :: NCF
    TYPE(NCVAR), POINTER :: VAR1,VAR2

    INTEGER :: CNT,SENDER
    TYPE(MAP), POINTER :: GMAP(:)
    INTEGER, POINTER :: TEMP1(:),TEMP2(:)
    TYPE(NEST_DATA), POINTER :: D
    REAL(DP) :: INTERVAL
    
    TEMP1 => NULL()
    TEMP2 => NULL()
    IF(DBG_SET(DBG_SBR))WRITE(IPT,*) "START DUMP_NCNEST_FILE" 


    DO I =1,NCNEST_NUM

       IF(USE_MPI_IO_MODE) THEN
          SENDER = MSRID-1
          CALL MPI_BCAST(NCNEST_DATA(I)%IDX,1,MPI_INTEGER,SENDER,MPI_COMM_WORLD,IERR)
       END IF

       ! MAKE MAP FOR SPECIAL END OF RUN CASE IF NEEDED
       IF(NCNEST_DATA(I)%IDX /=NCNEST_BLOCKSIZE) THEN
          
          ! ADJUST THE STACK RANGE
          NCNEST_DATA(I)%RNG(2) = NCNEST_DATA(I)%RNG(1)+NCNEST_DATA(I)%IDX-1


          IF(.NOT. ASSOCIATED(TEMP1))THEN
             allocate(temp1(NCNEST_DATA(I)%IDX))
             DO J=1,NCNEST_DATA(I)%IDX
                TEMP1(J)=J
             END DO

             ALLOCATE(TEMP2(NPROCS)); TEMP2= NCNEST_DATA(I)%IDX

             GMAP => FIND_MAP(INTERNAL_MAPS,NCNEST_DATA(I)%IDX,TEMP2,FOUND)

             IF(.NOT. FOUND) THEN
             IF(DBG_SET(DBG_LOG))WRITE(IPT,*) &
                  & "! CREATE SPECIAL MAP FOR TIME DATA IN NESTING FILE: map size", NCNEST_DATA(I)%IDX
                CALL ADD_MAP2LIST(INTERNAL_MAPS,MAKE_MAP(MYID,NPROCS,NCNEST_DATA(I)%IDX,NCNEST_BLOCKSIZE,TEMP1))
             END IF

             NULLIFY(GMAP)
             DEALLOCATE(TEMP2)

          ELSE 
             ! SINCE TEMP1 STAYS ALLOCATED, IT IS A RECORD OF THE
             ! OTHER PROCESSORS VALUES FOR IDX - CHECK IT!
             IF(SIZE(TEMP1) /= NCNEST_DATA(I)%IDX) call fatal_error&
                  &("DUMP_NCNEST_FILE: ERROR WHILE DUMPING DATA:",&
                  & "THE DATA INDEX SHOULD BE EQUAL FOR ALL FILES?")

          END IF
       end IF



       
       NCF => FIND_FILE(FILEHEAD,NCNEST_FNAMES(I),FOUND)
       IF(.NOT. FOUND) CALL FATAL_ERROR&
            &("DUMP_NCNEST_FILE: COULD NOT FIND FILE IN LIST:"//TRIM(NCNEST_FNAMES(I)))
       

       ! UPDATE THE TIME VARIABLES IN THE NCF
       IF(.NOT.IOPROC) THEN
          
          ! TAKE CARE OF END OF MODEL CASE
          CNT=NCNEST_DATA(I)%IDX

          VAR1 => FIND_VAR(NCF,"Itime",FOUND)
          IF(FOUND) THEN
             VAR2 => FIND_VAR(NCF,"Itime2",FOUND)
             IF (.NOT.FOUND) THEN
                CALL WARNING&
                     & ("DUMP_NCNEST_FILE: FOUND ONLY PART OF INTEGER TIME VARIABLE IN OUT PUT FILE!")
             ELSE
                
                ! USE TRICK WITH POINTERS TO UPDATE THE DATA IN THE TIME VARIABLE
                DO J=1,NCNEST_BLOCKSIZE
                   VAR1%SCL_INT => VAR1%VEC_INT(J)
                   VAR2%SCL_INT => VAR2%VEC_INT(J)
                   IF (J <= CNT) THEN
                      CALL UPDATE_ITIME_NEST(VAR1,VAR2,NCNEST_DATA(I)%TIME_BLK(J))
                   ELSE
                      VAR1%SCL_INT = 0
                      VAR2%SCL_INT = 0
                   END IF
                END DO
             END IF
          END IF
          
          
          VAR1 => FIND_VAR(NCF,"time",FOUND)
          IF(FOUND) THEN
             DO J=1,NCNEST_BLOCKSIZE
# if defined (DOUBLE_PRECISION)
                VAR1%SCL_DBL => VAR1%VEC_DBL(J)
# else
                VAR1%SCL_FLT => VAR1%VEC_FLT(J)
# endif		
                IF (J <= CNT) THEN
                   CALL UPDATE_FLOAT_TIME_NEST(VAR1,NCNEST_DATA(I)%TIME_BLK(J))
                ELSE
# if defined (DOUBLE_PRECISION)
                   VAR1%SCL_DBL = 0
# else
                   VAR1%SCL_FLT = 0
# endif		
                END IF
             END DO
          END IF
          
          
          VAR1 => FIND_VAR(NCF,"iint",FOUND)
          IF(FOUND) THEN
             VAR1%VEC_INT = 0
	     INTERVAL = SECONDS(INTERVAL_TIME_NCNEST)
# if !defined(SEMI_IMPLICIT)
             INTSTEP_SECONDS = EXTSTEP_SECONDS * ISPLIT
# endif
             DO J=1,CNT
               VAR1%VEC_INT(J) = IINT-(CNT-J)*INTERVAL/INTSTEP_SECONDS
             END DO
          END IF
       END IF


       D=>NCNEST_DATA(I)

       IF(DBG_SET(DBG_IO) .and. I==1)THEN
          WRITE(IPT,*) "IDX=",D%IDX,NCNEST_BLOCKSIZE
          WRITE(IPT,*) "RNG=",D%RNG
          
       END IF


       CALL NC_WRITE_FILE(NCF,STKRNG=D%RNG)
       
       D%RNG(1)=D%RNG(1)+D%IDX
       D%RNG(2)=D%RNG(1)+NCNEST_BLOCKSIZE-1
       

    END DO


    IF(ASSOCIATED(TEMP1)) DEALLOCATE(TEMP1)

    IF(DBG_SET(DBG_SBR))WRITE(IPT,*) "END DUMP_NCNEST_FILE" 

  END SUBROUTINE DUMP_NCNEST_FILE

# if defined (WAVE_CURRENT_INTERACTION)
  !==========================================================================!
  !
  !========================================================================!
  SUBROUTINE DUMP_NCNEST_FILE_WAVE
    IMPLICIT NONE
    INTEGER ::I,J,IERR
    LOGICAL :: FOUND
    TYPE(NCFILE), POINTER :: NCF1
    TYPE(NCVAR), POINTER :: VAR1,VAR2

    INTEGER :: CNT,SENDER
    TYPE(MAP), POINTER :: GMAP(:)
    INTEGER, POINTER :: TEMP1(:),TEMP2(:)
    TYPE(NEST_DATA_WAVE), POINTER :: D
    REAL(DP) :: INTERVAL_WAVE

    IF(DBG_SET(DBG_SBR))WRITE(IPT,*) "START DUMP_NCNEST_FILE_WAVE" 


    DO I =1,NCNEST_NUM_WAVE

       IF(USE_MPI_IO_MODE) THEN
          SENDER = MSRID-1
          CALL MPI_BCAST(NCNEST_DATA_WAVE(I)%IDX,1,MPI_INTEGER,SENDER,MPI_COMM_WORLD,IERR)
       END IF

       ! MAKE MAP FOR SPECIAL END OF RUN CASE IF NEEDED
       IF(NCNEST_DATA_WAVE(I)%IDX /=NCNEST_BLOCKSIZE_WAVE) THEN
          
          ! ADJUST THE STACK RANGE
          NCNEST_DATA_WAVE(I)%RNG(2) = NCNEST_DATA_WAVE(I)%RNG(1)+NCNEST_DATA_WAVE(I)%IDX-1


          IF(.NOT. ASSOCIATED(TEMP1))THEN
             allocate(temp1(NCNEST_DATA_WAVE(I)%IDX))
             DO J=1,NCNEST_DATA_WAVE(I)%IDX
                TEMP1(J)=J
             END DO

             ALLOCATE(TEMP2(NPROCS)); TEMP2= NCNEST_DATA_WAVE(I)%IDX

             GMAP => FIND_MAP(INTERNAL_MAPS,NCNEST_DATA_WAVE(I)%IDX,TEMP2,FOUND)

             IF(.NOT. FOUND) THEN
             IF(DBG_SET(DBG_LOG))WRITE(IPT,*) &
                  & "! CREATE SPECIAL MAP FOR TIME DATA IN WAVE NESTING FILE: map size", NCNEST_DATA_WAVE(I)%IDX
                CALL ADD_MAP2LIST(INTERNAL_MAPS,MAKE_MAP(MYID,NPROCS,NCNEST_DATA_WAVE(I)%IDX,NCNEST_BLOCKSIZE_WAVE,TEMP1))
             END IF

             NULLIFY(GMAP)
             DEALLOCATE(TEMP2)

          ELSE 
             ! SINCE TEMP1 STAYS ALLOCATED, IT IS A RECORD OF THE
             ! OTHER PROCESSORS VALUES FOR IDX - CHECK IT!
             IF(SIZE(TEMP1) /= NCNEST_DATA_WAVE(I)%IDX) call fatal_error&
                  &("DUMP_NCNEST_FILE_WAVE: ERROR WHILE DUMPING DATA:",&
                  & "THE DATA INDEX SHOULD BE EQUAL FOR ALL FILES?")

          END IF
       end IF



       
       NCF1 => FIND_FILE(FILEHEAD,NCNEST_FNAMES_WAVE(I),FOUND)
       IF(.NOT. FOUND) CALL FATAL_ERROR&
            &("DUMP_NCNEST_FILE_WAVE: COULD NOT FIND FILE IN LIST:"//TRIM(NCNEST_FNAMES_WAVE(I)))
       

       ! UPDATE THE TIME VARIABLES IN THE NCF
       IF(.NOT.IOPROC) THEN
          
          ! TAKE CARE OF END OF MODEL CASE
          CNT=NCNEST_DATA_WAVE(I)%IDX

          VAR1 => FIND_VAR(NCF1,"Itime",FOUND)
          IF(FOUND) THEN
             VAR2 => FIND_VAR(NCF1,"Itime2",FOUND)
             IF (.NOT.FOUND) THEN
                CALL WARNING&
                     & ("DUMP_NCNEST_FILE_WAVE: FOUND ONLY PART OF INTEGER TIME VARIABLE IN OUT PUT FILE!")
             ELSE
                
                ! USE TRICK WITH POINTERS TO UPDATE THE DATA IN THE TIME VARIABLE
                DO J=1,NCNEST_BLOCKSIZE_WAVE
                   VAR1%SCL_INT => VAR1%VEC_INT(J)
                   VAR2%SCL_INT => VAR2%VEC_INT(J)
                   IF (J <= CNT) THEN
                      CALL UPDATE_ITIME_NEST(VAR1,VAR2,NCNEST_DATA_WAVE(I)%TIME_BLK(J))
                   ELSE
                      VAR1%SCL_INT = 0
                      VAR2%SCL_INT = 0
                   END IF
                END DO
             END IF
          END IF
          
          
          VAR1 => FIND_VAR(NCF1,"time",FOUND)
          IF(FOUND) THEN
             DO J=1,NCNEST_BLOCKSIZE_WAVE
# if defined (DOUBLE_PRECISION)
                VAR1%SCL_DBL => VAR1%VEC_DBL(J)
# else
                VAR1%SCL_FLT => VAR1%VEC_FLT(J)
# endif		
                IF (J <= CNT) THEN
                   CALL UPDATE_FLOAT_TIME_NEST(VAR1,NCNEST_DATA_WAVE(I)%TIME_BLK(J))
                ELSE
# if defined (DOUBLE_PRECISION)
                   VAR1%SCL_DBL = 0
# else
                   VAR1%SCL_FLT = 0
# endif		
                END IF
             END DO
          END IF
          
          
          VAR1 => FIND_VAR(NCF1,"iint",FOUND)
          IF(FOUND) THEN
             VAR1%VEC_INT = 0
	     INTERVAL_WAVE = SECONDS(INTERVAL_TIME_NCNEST_WAVE)
# if !defined(SEMI_IMPLICIT)
             INTSTEP_SECONDS = EXTSTEP_SECONDS * ISPLIT
# endif
             DO J=1,CNT
                VAR1%VEC_INT(J) = IINT -(CNT-J)*INTERVAL_WAVE/INTSTEP_SECONDS
             END DO
          END IF
       END IF


       D=>NCNEST_DATA_WAVE(I)

       IF(DBG_SET(DBG_IO) .and. I==1)THEN
          WRITE(IPT,*) "IDX=",D%IDX,NCNEST_BLOCKSIZE_WAVE
          WRITE(IPT,*) "RNG=",D%RNG
          
       END IF


       CALL NC_WRITE_FILE(NCF1,STKRNG=D%RNG)
       
       D%RNG(1)=D%RNG(1)+D%IDX
       D%RNG(2)=D%RNG(1)+NCNEST_BLOCKSIZE_WAVE-1
       

    END DO


    IF(ASSOCIATED(TEMP1)) DEALLOCATE(TEMP1)

    IF(DBG_SET(DBG_SBR))WRITE(IPT,*) "END DUMP_NCNEST_FILE_WAVE" 

  END SUBROUTINE DUMP_NCNEST_FILE_WAVE
# endif  
  !========================================================================!
  !
  !========================================================================!  
  SUBROUTINE SETUP_NCNEST_FILE
    USE CONTROL
    IMPLICIT NONE

    INTEGER :: I,status
    LOGICAL :: FOUND
    CHARACTER(LEN=80) :: FILE,PATH,EXT

    TYPE(NCFILE), POINTER :: NCF,NCF2
    TYPE(NCVAR),  POINTER :: NCF3

    TYPE(GRID), POINTER :: G
    TYPE(NEST_DATA), POINTER :: D

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "START SETUP_NCNEST_FILE"

    IF(IOPROC) THEN
       ALLOCATE(NCNEST_DATA(NCNEST_NUM),STAT=status)
       IF(STATUS /=0) CALL FATAL_ERROR("IOPROC COULD NOT ALLOCATE NCNEST_DATA!")
    END IF

    DO I = 1,NCNEST_NUM
       

       G => NCNEST_GRIDS(I)
       D => NCNEST_DATA(I)

       
       ! ADD DO LOOP HERE OVER NUMBER OF OUTPUT FILES
       
       ! WHAT ABOUT SETTING UP THE DIMENSIONS?
       
       ! ALLOCATE THE NEW FILE OBJECT
       NCF => NEW_FILE()
       
       CALL PATH_SPLIT(NCNEST_FNAMES(I),PATH,FILE,EXT)
       
       NCF%FNAME = trim(OUTPUT_DIR)//trim(FILE)//".nc"
       
       ! CHANGE NAME IN LIST TO NETCDF NAME
       NCNEST_FNAMES(I) = NCF%FNAME

       CALL DEFINE_DIMENSIONS(G)

       NCF2 => GRID_FILE_OBJECT(G)
       NCF => ADD(NCF,NCF2)
!!$       NCF => ADD(NCF,GRID_FILE_OBJECT(G) )
       ! JUST MODIFY THE DIMENSION RETURNED - FVCOM WILL PICK THE
       ! CORRECT MAP AUTOMATICALLY!


       ! IINT
       NCF3  => IINT_OBJECT(DIM=Dim_time,SIZE=NCNEST_BLOCKSIZE)
       NCF  => ADD(NCF,NCF3)
!!$       NCF  => ADD(NCF,IINT_OBJECT(DIM=Dim_time,SIZE=NCNEST_BLOCKSIZE))
       
       ! time
       NCF3  => FLOAT_TIME_OBJECT_NEST(USE_MJD=use_real_world_time,DIM=DIM_TIME,SIZE=NCNEST_BLOCKSIZE)
       NCF  => ADD(NCF, NCF3)
!!$       NCF  => ADD(NCF, FLOAT_TIME_OBJECT(USE_MJD=use_real_world_time,DIM=DIM_TIME,SIZE=NCNEST_BLOCKSIZE))
       
       !ITIME
       NCF3 => ITIME_OBJECT_NEST(Use_MJD=use_real_world_time,DIM=DIM_TIME,SIZE=NCNEST_BLOCKSIZE)
       NCF => ADD(NCF,NCF3)
!!$       NCF => ADD(NCF,ITIME_OBJECT(Use_MJD=use_real_world_time,DIM=DIM_TIME,SIZE=NCNEST_BLOCKSIZE))

       NCF3 => ITIME2_OBJECT(Use_MJD=use_real_world_time,DIM=DIM_TIME,SIZE=NCNEST_BLOCKSIZE)
       NCF => ADD(NCF,NCF3)
!!$       NCF => ADD(NCF,ITIME2_OBJECT(Use_MJD=use_real_world_time,DIM=DIM_TIME,SIZE=NCNEST_BLOCKSIZE))

       ! NESTING DATA
       NCF2 => NESTING_FILE_OBJECT(D)
       NCF => ADD(NCF,NCF2)
!!$       NCF => ADD(NCF,NESTING_FILE_OBJECT(D) )
       
       NCF%FTIME => NEW_FTIME()
       
       IF (STARTUP_TYPE /= "crashrestart") THEN
          CALL NC_WRITE_FILE(NCF)
          NCF%FTIME%NEXT_STKCNT=1
          D%RNG(1)=1
          D%RNG(2)=NCNEST_BLOCKSIZE
          
       ELSE IF (NCNEST_ON) THEN
          NCF%CONNECTED = .FALSE.
          CALL NC_WRITE_FILE(NCF)
          NCF%FTIME%NEXT_STKCNT=1
          D%RNG(1)=1
          D%RNG(2)=NCNEST_BLOCKSIZE

       ELSE
          Call Set_File_Stack(NCF,STARTTIME,IMDTI)
          NCF%CONNECTED = .TRUE.
          NCF%WRITABLE = .TRUE.

          D%RNG(1)=NCF%FTIME%NEXT_STKCNT
          D%RNG(2)=NCF%FTIME%NEXT_STKCNT + NCNEST_BLOCKSIZE-1

          ! SUBTRACT ONE BECAUSE IT WILL STORE THE FIRST DATA NOW
          ! OTHERWISE IT WILL CREATE A REPEAT ENTRY IN THE FILE
          D%RNG = D%RNG -1

       END IF

       IF(DBG_SET(DBG_IO) .and. I==1)THEN
          WRITE(IPT,*) "SETUP IDX=",D%IDX,NCNEST_BLOCKSIZE
          WRITE(IPT,*) "SETUP RNG=",D%RNG
          
       END IF

       FILEHEAD => ADD(FILEHEAD,NCF)

       CALL KILL_DIMENSIONS

    END DO


       IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END SETUP_NCNEST_FILE"
     END SUBROUTINE SETUP_NCNEST_FILE
  !========================================================================!
  !
  !========================================================================!
  SUBROUTINE ASSIGN2BLOCK(D,G)
#if defined (FABM)
    USE MOD_FABM_DATA
#endif
    IMPLICIT NONE
    INTEGER :: II
    TYPE(NEST_DATA) :: D
    TYPE(GRID) :: G

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "START ASSIGN2BLOCK"

    D%IDX = D%IDX+1

    D%UA_BLK(:,D%IDX) = UA(G%ELID(:))
    D%VA_BLK(:,D%IDX) = VA(G%ELID(:))
    D%EL_BLK(:,D%IDX) = EL(G%NLID(:))
    D%U_BLK(:,:,D%IDX) = U(G%ELID(:),:)
    D%V_BLK(:,:,D%IDX) = V(G%ELID(:),:)
    D%S1_BLK(:,:,D%IDX) = S1(G%NLID(:),:)
    D%T1_BLK(:,:,D%IDX) = T1(G%NLID(:),:)
# if defined (FABM)
   DO II=1,IFABMadv ! RJT only for advectable variables size(model%interior_state_variables)
    D%FABM_ALL_BLK(:,:,D%IDX,II)=FABM_ALL(G%NLID(:),1:KB,IFABMadv_idx(II))
   END DO
# endif
# if defined (NH)
    D%HYW_BLK(:,:,D%IDX) = CALC_HYW(G)
# else
    D%HYW_BLK(:,:,D%IDX) = 0.0_SP
# endif
    D%TIME_BLK(D%IDX) = INTTIME

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END ASSIGN2BLOCK"

  END SUBROUTINE ASSIGN2BLOCK
  !========================================================================!
  FUNCTION CALC_HYW(G) RESULT(HYW)
    IMPLICIT NONE
    TYPE(GRID) :: G

    REAL(SP) :: HYW(0:G%M,G%KB)

    INTEGER :: I, J,K,II,J1,J2,J3
    REAL(SP) :: DDDX,DDDY,WW1,U_TMP,V_TMP,DEDY,DEDX,ETF1AA

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "START CALC_HYW"
    
    DO I=1, G%M

       DO J=1, NTVE(G%NLID(I))
          II = NBVE(G%NLID(I),J)
          J1=NV(II,1)
          J2=NV(II,2)
          J3=NV(II,3)

          DEDX=AWX(II,1)*EL(J1)+AWX(II,2)*EL(J2)+AWX(II,3)*EL(J3)
          DEDY=AWY(II,1)*EL(J1)+AWY(II,2)*EL(J2)+AWY(II,3)*EL(J3)
          ETF1AA=ONE_THIRD*(EL(NV(II,1))+EL(NV(II,2))+EL(NV(II,3)))
          DO K=1, KB

             IF(K==1) THEN
                U_TMP = U(II,1)
                V_TMP = V(II,1)
             ELSE IF(K==KB) THEN
                U_TMP = U(II,KBM1)
                V_TMP = V(II,KBM1)
             ELSE
                U_TMP = (U(II,K-1)*DZ1(II,K)+U(II,K)*DZ1(II,K-1))/(DZ1(II,K-1)+DZ1(II,K))
                V_TMP = (V(II,K-1)*DZ1(II,K)+V(II,K)*DZ1(II,K-1))/(DZ1(II,K-1)+DZ1(II,K))
             ENDIF

             DDDX=AWX(II,1) * D(J1)*Z(J1,K)+AWX(II,2) * D(J2)*Z(J2,K)+AWX(II,3)*D(J3)*Z(J3,K)
             DDDY=AWY(II,1) * D(J1)*Z(J1,K)+AWY(II,2) * D(J2)*Z(J2,K)+AWY(II,3)*D(J3)*Z(J3,K)

             WW1=W(II,K)+U_TMP*(DDDX+DEDX)+V_TMP*(DDDY+DEDY)+(Z1(II,K)+1.)*(ETF1AA-ET1(II))/DTI

             HYW(I,K) = HYW(I,K) + WW1*ART(NBVE(G%NLID(I),J))
          ENDDO
       ENDDO

       DO K=1, KB
          HYW(I,K) = HYW(I,K)/ART2(G%NLID(I))
       ENDDO

    ENDDO

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END CALC_HYW"

  END FUNCTION CALC_HYW
  !========================================================================!
  SUBROUTINE RESET_BLOCK(D)
    IMPLICIT NONE
    INTEGER :: II
    TYPE(NEST_DATA) :: D

    D%IDX=0

    D%UA_BLK =  -99.0_SP
    D%VA_BLK =  -99.0_SP
    D%EL_BLK =  -99.0_SP
    D%U_BLK  =  -99.0_SP
    D%V_BLK  =  -99.0_SP
    D%S1_BLK =  -99.0_SP
    D%T1_BLK =  -99.0_SP
    D%HYW_BLK=  -99.0_SP
    D%TIME_BLK =  zerotime
# if defined (FABM)
   DO II=1,IFABMadv !size(model%interior_state_variables)
    D%FABM_ALL_BLK=-99.0_SP
   END DO
# endif


  END SUBROUTINE RESET_BLOCK
  !========================================================================!  
  !
  !========================================================================!    
# if defined (FABM)
  SUBROUTINE SET_VAR(NOW,UA,VA,EL,U,V,S1,T1,HYW,FABM_ALL, &
       & ZERO_2D_NODES,ZERO_2D_NODES_OBC,ZERO_3D_NODES,ZERO_3D_NODES_OBC,&
       & ZERO_2D_CELLS,ZERO_2D_CELLS_OBC,ZERO_3D_CELLS,ZERO_3D_CELLS_OBC)
# else
  SUBROUTINE SET_VAR(NOW,UA,VA,EL,U,V,S1,T1,HYW, &
       & ZERO_2D_NODES,ZERO_2D_NODES_OBC,ZERO_3D_NODES,ZERO_3D_NODES_OBC,&
       & ZERO_2D_CELLS,ZERO_2D_CELLS_OBC,ZERO_3D_CELLS,ZERO_3D_CELLS_OBC)

# endif 
    IMPLICIT NONE
    TYPE(TIME), INTENT(IN) :: NOW    
    REAL(SP), ALLOCATABLE, OPTIONAL :: UA(:),VA(:),EL(:)
    REAL(SP), ALLOCATABLE, OPTIONAL :: U(:,:),V(:,:),S1(:,:),T1(:,:),HYW(:,:)
# if defined (FABM)
    REAL(SP), ALLOCATABLE, OPTIONAL :: FABM_ALL(:,:,:)
# endif
    REAL(SP), ALLOCATABLE, OPTIONAL :: ZERO_3D_NODES(:,:),ZERO_3D_NODES_OBC(:,:)
    REAL(SP), ALLOCATABLE, OPTIONAL :: ZERO_2D_NODES(:),ZERO_2D_NODES_OBC(:)

    REAL(SP), ALLOCATABLE, OPTIONAL :: ZERO_3D_CELLS(:,:),ZERO_3D_CELLS_OBC(:,:)
    REAL(SP), ALLOCATABLE, OPTIONAL :: ZERO_2D_CELLS(:),ZERO_2D_CELLS_OBC(:)

! To help with SINTER INTERPOLATION
    REAL(SP), DIMENSION(KBM1)   :: PHY_Z   !Depth(m) in every standary Z levels 
    REAL(SP), DIMENSION(KBM1)   :: VARZ    !Variable in standary Z levels 
    REAL(SP), DIMENSION(KBM1_L)  :: ZM      !Depth (m) in every sigma levels for giving node
    REAL(SP), DIMENSION(KBM1_L)  :: VARS    !Variable AT SIGMA LEVELS
    REAL(SP), DIMENSION(KBM1)    :: NEST4_SHEAR_NEW
    REAL(SP)                    :: NEST4_W

 
    REAL(DP)     :: denom, numer
    REAL(DP)     :: prev_w, next_w
    INTEGER      :: STEP,PREV,NEXT,I
    INTEGER      :: I_TMP
# if defined (FABM)
    INTEGER      :: IFABM
# endif

    TYPE(TIME) :: TDIFF

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "START SET_VAR (NESTING)"

    ! SEE IF WE NEED NEW DATA
    IF ( NESTING_DATA%TIME_BLK(1) > NOW .OR. &
          NESTING_DATA%TIME_BLK(NESTING_DATA%NBLK) < NOW) CALL UPDATE_BLOCK(NOW)

    ! FIND WHICH INDEX IN THE BLOCK BRACKETS NOW
    DO STEP = 1,NESTING_DATA%NBLK
       IF(NOW < NESTING_DATA%TIME_BLK(STEP)) EXIT
    END DO
    ! MAKE SURE NO LARGER THAN BLOCK SIZE (FOR == CASE!)
    STEP = MIN(NESTING_DATA%NBLK,STEP)


!    CALL PRINT_TIME(NOW,IPT,"Current time")
!    CALL PRINT_TIME(NESTING_DATA%TIME_BLK(STEP),IPT,"STEP")


    PREV = STEP -1
    NEXT = STEP 

    ! GET THE INTERPOLATION WEIGHTS

    ! THE DIFFERENCE BETWEEN THE CURRENT TIME IN THE MODEL AND THE PREVIOUS DATA
    NUMER = SECONDS(NOW - NESTING_DATA%TIME_BLK(PREV))
    DENOM = SECONDS(NESTING_DATA%TIME_BLK(NEXT) - NESTING_DATA%TIME_BLK(PREV))


    ! TAKE THE RATIO IN DOUBLE PRECISION AND CONVERT IF MODEL IS NOT DOUBLE
    NEXT_W = NUMER/DENOM
    PREV_W = 1.0_DP - NEXT_W

    IF(DBG_SET(DBG_SBRIO))THEN
       write(ipt,*) "========== NESTING SET_VAR_2D ==================="
       CALL PRINT_TIME(NOW,IPT,"Current time")
       CALL PRINT_TIME(NESTING_DATA%TIME_BLK(PREV),IPT,"PREVIOUS STEP")
       CALL PRINT_TIME(NESTING_DATA%TIME_BLK(NEXT),IPT,"NEXT STEP")
       write(ipt,*) "PREV/NEXT, PWGHT/NWGHT",PREV,NEXT,", ",PREV_W,NEXT_W
       write(ipt,*) "================================================="
    END IF

!!$    ! GET THE DATA
!!$    DAT = NEXT_W * BLK_DAT(:,:,NEXT_I) + PREV_W * BLK_DAT(:,:,PREV_I)

    ! Type 4 treats the 2-D velocity components together in the conservative
    ! barotropic correction, not through independent legacy SET_VAR blending.
    IF(TRIM(NESTING_TYPE) == '4') THEN
       IF(PRESENT(UA) .OR. PRESENT(VA)) RETURN
    END IF

    IF(PRESENT(UA)) THEN
       UA_NEST =  NESTING_DATA%UA_BLK(:,PREV) * PREV_W +&
                & NESTING_DATA%UA_BLK(:,NEXT) * NEXT_W


       WCELL_NEST = 1.0_SP
       IF(TRIM(NESTING_TYPE) == '3') WCELL_NEST =  NESTING_DATA%WCELL_BLK(:,PREV) * PREV_W +&
                & NESTING_DATA%WCELL_BLK(:,NEXT) * NEXT_W


       DO I = 1, NESTING_GRID%NT
         I_TMP = NESTING_GRID%ELID_X(I)
         UA(I_TMP) =  UA_NEST(I) * WCELL_NEST(I) + UA(I_TMP) * (1_SP -  WCELL_NEST(I))  
       END DO

    ELSEIF(PRESENT(VA)) THEN
       VA_NEST =  NESTING_DATA%VA_BLK(:,PREV) * PREV_W +&
                & NESTING_DATA%VA_BLK(:,NEXT) * NEXT_W

       WCELL_NEST = 1.0_SP
       IF(TRIM(NESTING_TYPE) == '3') WCELL_NEST =  NESTING_DATA%WCELL_BLK(:,PREV) * PREV_W +&
                & NESTING_DATA%WCELL_BLK(:,NEXT) * NEXT_W


       DO I = 1, NESTING_GRID%NT
         I_TMP = NESTING_GRID%ELID_X(I)
         VA(I_TMP) =  VA_NEST(I) * WCELL_NEST(I) + VA(I_TMP) * (1_SP -  WCELL_NEST(I)) 
       END DO

    ELSEIF(PRESENT(EL)) THEN
       EL_NEST =  NESTING_DATA%EL_BLK(:,PREV) * PREV_W +&
                & NESTING_DATA%EL_BLK(:,NEXT) * NEXT_W

       WNODE_NEST = 1.0_SP
       IF(TRIM(NESTING_TYPE) == '3') WNODE_NEST =  NESTING_DATA%WNODE_BLK(:,PREV) * PREV_W +&
                & NESTING_DATA%WNODE_BLK(:,NEXT) * NEXT_W
    
       DO I = 1, NESTING_GRID%MT
         I_TMP = NESTING_GRID%NLID_X(I)
         IF(TRIM(NESTING_TYPE) == '4') THEN
            ! Clamp OBC elevation after RK-stage scaling; interior elevation
            ! is relaxed together with transport in NEST4_CORRECT_BAROTROPIC.
            IF(ISONB(I_TMP) == 2) EL(I_TMP) = EL_NEST(I)
         ELSE
            EL(I_TMP) = EL_NEST(I) * WNODE_NEST(I) + EL(I_TMP) * (1_SP - WNODE_NEST(I))
         END IF
       END DO

    ELSEIF(PRESENT(U)) THEN
       U_NEST_L =  NESTING_DATA%U_BLK(:,:,PREV) * PREV_W +&
                & NESTING_DATA%U_BLK(:,:,NEXT) * NEXT_W
		
       WCELL_NEST = 1.0_SP
       IF(TRIM(NESTING_TYPE) == '3' .OR. TRIM(NESTING_TYPE) == '4') &
            WCELL_NEST = NESTING_DATA%WCELL_BLK(:,PREV) * PREV_W + &
            & NESTING_DATA%WCELL_BLK(:,NEXT) * NEXT_W

       DO I = 1, NESTING_GRID%NT
         I_TMP = NESTING_GRID%ELID_X(I)
         ZM(1:KBM1_L) = ZZ1_L(I,1:KBM1_L)
         PHY_Z(1:KBM1) = ZZ1(I_TMP,1:KBM1)
         VARS(1:KBM1_L) = U_NEST_L(I,1:KBM1_L)
         VARZ(1:KBM1) = U_NEST(I,1:KBM1)
         CALL SINTER_EXTRP_NONE(ZM,VARS,PHY_Z,VARZ,KBM1_L,KBM1)
         IF(TRIM(NESTING_TYPE) == '4') THEN
            NEST4_W = NEST4_CELL_WEIGHT(I,DTI,NESTING_TAU_SHEAR)
            CALL NEST4_RELAX_SHEAR(U(I_TMP,1:KBM1),VARZ(1:KBM1), &
                 & DZ1(I_TMP,1:KBM1),NEST4_W,NEST4_SHEAR_NEW)
            U(I_TMP,1:KBM1) = NEST4_SHEAR_NEW
         ELSE
            U(I_TMP,1:KBM1) = VARZ(1:KBM1) * WCELL_NEST(I) &
                 & + U(I_TMP,1:KBM1) * (1_SP - WCELL_NEST(I))
         END IF
!         CALL SINTER_EXTRP_NONE(ZZ1_L(I,1:KBM1_L),U_NEST_L(I,1:KBM1_L),ZZ1(I_TMP,1:KBM1),U_NEST(I,1:KBM1),KBM1_L,KBM1)
!         U(I_TMP,:) =  U_NEST(I,:) * WCELL_NEST(I) + U(I_TMP,:) * (1_SP -  WCELL_NEST(I))
       END DO

    ELSEIF(PRESENT(V)) THEN
       V_NEST_L =  NESTING_DATA%V_BLK(:,:,PREV) * PREV_W +&
                & NESTING_DATA%V_BLK(:,:,NEXT) * NEXT_W

       WCELL_NEST = 1.0_SP
       IF(TRIM(NESTING_TYPE) == '3' .OR. TRIM(NESTING_TYPE) == '4') &
            WCELL_NEST = NESTING_DATA%WCELL_BLK(:,PREV) * PREV_W + &
            & NESTING_DATA%WCELL_BLK(:,NEXT) * NEXT_W

       DO I = 1, NESTING_GRID%NT
         I_TMP = NESTING_GRID%ELID_X(I)
         ZM(1:KBM1_L) = ZZ1_L(I,1:KBM1_L)
         PHY_Z(1:KBM1) = ZZ1(I_TMP,1:KBM1)
         VARS(1:KBM1_L) = V_NEST_L(I,1:KBM1_L)
         VARZ(1:KBM1) = V_NEST(I,1:KBM1)
         CALL SINTER_EXTRP_NONE(ZM,VARS,PHY_Z,VARZ,KBM1_L,KBM1)
         IF(TRIM(NESTING_TYPE) == '4') THEN
            NEST4_W = NEST4_CELL_WEIGHT(I,DTI,NESTING_TAU_SHEAR)
            CALL NEST4_RELAX_SHEAR(V(I_TMP,1:KBM1),VARZ(1:KBM1), &
                 & DZ1(I_TMP,1:KBM1),NEST4_W,NEST4_SHEAR_NEW)
            V(I_TMP,1:KBM1) = NEST4_SHEAR_NEW
         ELSE
            V(I_TMP,1:KBM1) = VARZ(1:KBM1) * WCELL_NEST(I) &
                 & + V(I_TMP,1:KBM1) * (1_SP - WCELL_NEST(I))
         END IF
!         CALL SINTER_EXTRP_NONE(ZZ1_L(I,1:KBM1_L),V_NEST_L(I,1:KBM1_L),ZZ1(I_TMP,1:KBM1),V_NEST(I,1:KBM1),KBM1_L,KBM1)
!         V(I_TMP,:) =  V_NEST(I,:)  * WCELL_NEST(I) + V(I_TMP,:) * (1_SP -  WCELL_NEST(I))
       END DO

    ELSEIF(PRESENT(S1)) THEN
       S1_NEST_L =  NESTING_DATA%S1_BLK(:,:,PREV) * PREV_W +&
                & NESTING_DATA%S1_BLK(:,:,NEXT) * NEXT_W

       WNODE_NEST = 1.0_SP
       IF(TRIM(NESTING_TYPE) == '3' .OR. TRIM(NESTING_TYPE) == '4') &
            WNODE_NEST = NESTING_DATA%WNODE_BLK(:,PREV) * PREV_W + &
            & NESTING_DATA%WNODE_BLK(:,NEXT) * NEXT_W

       DO I = 1, NESTING_GRID%MT
         I_TMP = NESTING_GRID%NLID_X(I)
         ZM(1:KBM1_L) = ZZ_L(I,1:KBM1_L)
         PHY_Z(1:KBM1) = ZZ(I_TMP,1:KBM1)
         VARS(1:KBM1_L) = S1_NEST_L(I,1:KBM1_L)
         VARZ(1:KBM1) = S1_NEST(I,1:KBM1)
         CALL SINTER_EXTRP_NONE(ZM,VARS,PHY_Z,VARZ,KBM1_L,KBM1)
         IF(TRIM(NESTING_TYPE) == '4') THEN
            NEST4_W = NEST4_NODE_WEIGHT(I,DTI,NESTING_TAU_TS)
            S1(I_TMP,1:KBM1) = VARZ(1:KBM1)*NEST4_W &
                 & + S1(I_TMP,1:KBM1)*(1.0_SP-NEST4_W)
         ELSE
            S1(I_TMP,1:KBM1) = VARZ(1:KBM1)*WNODE_NEST(I) &
                 & + S1(I_TMP,1:KBM1)*(1.0_SP-WNODE_NEST(I))
         END IF
!         CALL SINTER_EXTRP_NONE(ZZ_L(I,1:KBM1_L),S1_NEST_L(I,1:KBM1_L),ZZ(I_TMP,1:KBM1),S1_NEST(I,1:KBM1),KBM1_L,KBM1)
!         S1(I_TMP,:) =  S1_NEST(I,:) * WNODE_NEST(I) + S1(I_TMP,:) * (1_SP - WNODE_NEST(I))
       END DO

    ELSEIF(PRESENT(T1)) THEN
       T1_NEST_L =  NESTING_DATA%T1_BLK(:,:,PREV) * PREV_W +&
                & NESTING_DATA%T1_BLK(:,:,NEXT) * NEXT_W

       WNODE_NEST = 1.0_SP
       IF(TRIM(NESTING_TYPE) == '3' .OR. TRIM(NESTING_TYPE) == '4') &
            WNODE_NEST = NESTING_DATA%WNODE_BLK(:,PREV) * PREV_W + &
            & NESTING_DATA%WNODE_BLK(:,NEXT) * NEXT_W

       DO I = 1, NESTING_GRID%MT
         I_TMP = NESTING_GRID%NLID_X(I)
         ZM(1:KBM1_L) = ZZ_L(I,1:KBM1_L)
         PHY_Z(1:KBM1) = ZZ(I_TMP,1:KBM1)
         VARS(1:KBM1_L) = T1_NEST_L(I,1:KBM1_L)
         VARZ(1:KBM1) = T1_NEST(I,1:KBM1)
         CALL SINTER_EXTRP_NONE(ZM,VARS,PHY_Z,VARZ,KBM1_L,KBM1)
         IF(TRIM(NESTING_TYPE) == '4') THEN
            NEST4_W = NEST4_NODE_WEIGHT(I,DTI,NESTING_TAU_TS)
            T1(I_TMP,1:KBM1) = VARZ(1:KBM1)*NEST4_W &
                 & + T1(I_TMP,1:KBM1)*(1.0_SP-NEST4_W)
         ELSE
            T1(I_TMP,1:KBM1) = VARZ(1:KBM1)*WNODE_NEST(I) &
                 & + T1(I_TMP,1:KBM1)*(1.0_SP-WNODE_NEST(I))
         END IF
!         CALL SINTER_EXTRP_NONE(ZZ_L(I,1:KBM1_L),T1_NEST_L(I,1:KBM1_L),ZZ(I_TMP,1:KBM1),T1_NEST(I,1:KBM1),KBM1_L,KBM1)
!         T1(I_TMP,:) =  T1_NEST(I,:) * WNODE_NEST(I) + T1(I_TMP,:) * (1_SP - WNODE_NEST(I))
       END DO

# if defined (FABM)
    ELSEIF(PRESENT(FABM_ALL)) THEN
    IF(FABM_NESTING_ON) THEN
    DO IFABM = 1,IFABMnest !IFABMadv !size(model%state_variables)
       FABM_ALL_NEST(:,:,IFABM) =   NESTING_DATA%FABM_ALL_BLK(:,:,PREV,IFABM) * PREV_W +&
                &  NESTING_DATA%FABM_ALL_BLK(:,:,NEXT,IFABM) * NEXT_W

       WNODE_NEST = 1.0_SP
       IF(TRIM(NESTING_TYPE) == '3') WNODE_NEST =  NESTING_DATA%WNODE_BLK(:,PREV) * PREV_W +&
                & NESTING_DATA%WNODE_BLK(:,NEXT) * NEXT_W

       DO I = 1, NESTING_GRID%MT
          I_TMP = NESTING_GRID%NLID_X(I)
          FABM_ALL(I_TMP,:,IFABMnest_idx(IFABM)) =  FABM_ALL_NEST(I,:,IFABM) * WNODE_NEST(I) + FABM_ALL(I_TMP,:,IFABMnest_idx(IFABM)) * (1_SP - WNODE_NEST(I))
          FABM_ALL(I_TMP,:,IFABMnest_idx(IFABM)) =  FABM_ALL(I_TMP,:,IFABMnest_idx(IFABM)) * RAMP
       END DO
    END DO
    END IF

# endif


! FOR NON HYDRO STATIC MODEL
    ELSEIF(PRESENT(HYW)) THEN
       HYW_NEST = NESTING_DATA%HYW_BLK(:,:,PREV) * PREV_W +&
		& NESTING_DATA%HYW_BLK(:,:,NEXT) * NEXT_W

       WNODE_NEST = 1.0_SP
       IF(TRIM(NESTING_TYPE) == '3') WNODE_NEST =  NESTING_DATA%WNODE_BLK(:,PREV) * PREV_W +&
                & NESTING_DATA%WNODE_BLK(:,NEXT) * NEXT_W

       DO I = 1, NESTING_GRID%MT
         I_TMP = NESTING_GRID%NLID_X(I)
!         ZM(1:KBM1_L) = ZZ_L(I,1:KBM1_L)
!         PHY_Z(1:KBM1) = ZZ(I_TMP,1:KBM1)
!         VARS(1:KBM1_L) = HYW_NEST_L(I,1:KBM1_L)
!         VARZ(1:KBM1) = HYW_NEST(I,1:KBM1)
!         CALL SINTER_EXTRP_NONE(ZM,VARS,PHY_Z,VARZ,KBM1_L,KBM1)
!         HYW(I_TMP,1:KBM1) =  VARZ(1:KBM1) * WNODE_NEST(I) + HYW(I_TMP,1:KBM1) * (1_SP -  WNODE_NEST(I))
         CALL SINTER_EXTRP_NONE(ZZ_L(I,1:KB_L),HYW_NEST_L(I,1:KB_L),ZZ(I_TMP,1:KB),HYW_NEST(I,1:KB),KB_L,KB)
         HYW(I_TMP,:) =  HYW_NEST(I,:) * WNODE_NEST(I) + HYW(I_TMP,:) * (1_SP - WNODE_NEST(I))
       END DO


! OPTIONAL ARGUMENTS TO HELP ZERO OUT THE ARRAY IF NEEDED
    ELSEIF(PRESENT(ZERO_2D_NODES)) THEN
       DO I = 1, NESTING_GRID%MT
          ZERO_2D_NODES(NESTING_GRID%NLID_X(I)) =  0.0_SP
       END DO

    ELSEIF(PRESENT(ZERO_2D_NODES_OBC)) THEN
       DO I = 1, NESTING_GRID%MT
          IF((ISONB(NESTING_GRID%NLID_X(I)))==2) THEN
             ZERO_2D_NODES(NESTING_GRID%NLID_X(I)) =  0.0_SP
          END IF
       END DO

    ELSEIF(PRESENT(ZERO_3D_NODES)) THEN
       DO I = 1, NESTING_GRID%MT
          ZERO_3D_NODES(NESTING_GRID%NLID_X(I),:) =  0.0_SP
       END DO


    ELSEIF(PRESENT(ZERO_3D_NODES_OBC)) THEN
       DO I = 1, NESTING_GRID%MT
          IF((ISONB(NESTING_GRID%NLID_X(I)))==2) THEN
             ZERO_3D_NODES(NESTING_GRID%NLID_X(I),:) =  0.0_SP
          END IF
       END DO


    ELSEIF(PRESENT(ZERO_2D_CELLS)) THEN
       DO I = 1, NESTING_GRID%NT
          ZERO_2D_CELLS(NESTING_GRID%ELID_X(I)) =  0.0_SP
       END DO

    ELSEIF(PRESENT(ZERO_2D_CELLS_OBC)) THEN
       DO I = 1, NESTING_GRID%NT
          IF((ISBCE(NESTING_GRID%ELID_X(I)))==2) THEN
             ZERO_2D_CELLS(NESTING_GRID%ELID_X(I)) =  0.0_SP
          END IF
       END DO

    ELSEIF(PRESENT(ZERO_3D_CELLS)) THEN
       DO I = 1, NESTING_GRID%NT
          ZERO_3D_CELLS(NESTING_GRID%ELID_X(I),:) =  0.0_SP
       END DO


    ELSEIF(PRESENT(ZERO_3D_CELLS_OBC)) THEN
       DO I = 1, NESTING_GRID%NT
          IF((ISBCE(NESTING_GRID%ELID_X(I)))==2) THEN
             ZERO_3D_CELLS(NESTING_GRID%ELID_X(I),:) =  0.0_SP
          END IF
       END DO

    END IF



    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END SET_NOW (NESTING)"
    
  END SUBROUTINE SET_VAR

  REAL(SP) FUNCTION NEST4_NODE_WEIGHT(I,DT,TAU) RESULT(W)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: I
    REAL(SP), INTENT(IN) :: DT,TAU
    INTEGER :: J
    REAL(SP) :: ALPHA_NEST
    J = NESTING_GRID%NLID_X(I)
    ALPHA_NEST = WNODE_NEST(I)
    IF(.NOT. IEEE_IS_FINITE(ALPHA_NEST) .OR. ALPHA_NEST < 0.0_SP &
         & .OR. ALPHA_NEST > 1.0_SP) &
         CALL FATAL_ERROR("TYPE 4: INVALID weight_node, MUST BE FINITE IN [0,1]")
    ! Land and OBC have different priorities: no nudging at land nodes;
    ! hard clamping at OBC nodes, regardless of alpha.
    IF(ISONB(J) == 1) THEN
       W = 0.0_SP
    ELSE
       W = NEST4_RELAX_WEIGHT(ALPHA_NEST,DT,TAU,ISONB(J) == 2)
    END IF
  END FUNCTION NEST4_NODE_WEIGHT

  REAL(SP) FUNCTION NEST4_CELL_WEIGHT(I,DT,TAU) RESULT(W)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: I
    REAL(SP), INTENT(IN) :: DT,TAU
    INTEGER :: J
    REAL(SP) :: ALPHA_NEST
    J = NESTING_GRID%ELID_X(I)
    ALPHA_NEST = WCELL_NEST(I)
    IF(.NOT. IEEE_IS_FINITE(ALPHA_NEST) .OR. ALPHA_NEST < 0.0_SP &
         & .OR. ALPHA_NEST > 1.0_SP) &
         CALL FATAL_ERROR("TYPE 4: INVALID weight_cell, MUST BE FINITE IN [0,1]")
    ! Never destroy the no-normal-flow land boundary projection.
    IF(ISBCE(J) == 1 .OR. ISBCE(J) == 3) THEN
       W = 0.0_SP
    ELSE
       W = NEST4_RELAX_WEIGHT(ALPHA_NEST,DT,TAU,ISBCE(J) == 2)
    END IF
  END FUNCTION NEST4_CELL_WEIGHT

  ! First-use initialization: cache actual OBC edge geometry, not nodal normals.
  ! NBE(J,E)==0 marks a mesh-exterior edge opposite vertex E. Check BOTH
  ! endpoints are OBC nodes to exclude land edges and MPI partition edges.
  SUBROUTINE NEST4_CHAR_INIT
# if defined (SPHERICAL)
    USE MOD_SPHERICAL, ONLY : ARCX, ARCY
# endif
    IMPLICIT NONE
    INTEGER :: I,J,E,N1,N2,NE
    REAL(DP) :: DX,DY,IX,IY
    IF(N4_CHAR_READY) RETURN
    ALLOCATE(N4_CELLMAP(0:NT),N4_NODEMAP(0:MT),N4_NEDGE(0:NT), &
         & N4_CELLIDS(NT),N4_NODEIDS(MT), &
         & N4_N1(0:NT,2),N4_N2(0:NT,2),N4_INNER(0:NT,2), &
         & N4_NX(0:NT,2),N4_NY(0:NT,2),N4_LENGTH(0:NT,2), &
         & N4_UN_TARGET(0:NT,2),N4_ETA_SUM(0:MT),N4_ETA_LENGTH(0:MT))
    N4_CELLMAP=0; N4_NODEMAP=0; N4_NEDGE=0
    N4_N1=0; N4_N2=0; N4_INNER=0
    N4_NX=0.0_SP; N4_NY=0.0_SP; N4_LENGTH=0.0_SP
    N4_UN_TARGET=0.0_SP
    N4_ETA_SUM=0.0_SP; N4_ETA_LENGTH=0.0_SP
    N4_NCELL=0; N4_NNODE=0
    DO I=1,NESTING_GRID%MT
       J=NESTING_GRID%NLID_X(I)
       N4_NODEMAP(J)=I
    END DO
    DO I=1,NESTING_GRID%NT
       J=NESTING_GRID%ELID_X(I)
       N4_CELLMAP(J)=I
    END DO
    DO J=1,NT
       IF(ISBCE(J)/=2) CYCLE
       IF(N4_CELLMAP(J)==0) &
            & CALL FATAL_ERROR('TYPE 4 CHAR: OBC CELL MISSING FROM NESTING GRID')
       DO E=1,3
          IF(NBE(J,E)/=0) CYCLE
          N1=NV(J,MOD(E,3)+1)
          N2=NV(J,MOD(E+1,3)+1)
          IF(ISONB(N1)/=2 .OR. ISONB(N2)/=2) CYCLE
          NE=N4_NEDGE(J)+1
          IF(NE>2) CALL FATAL_ERROR('TYPE 4 CHAR: THREE OPEN EDGES IN ONE CELL')
          IF(N4_NODEMAP(N1)==0 .OR. N4_NODEMAP(N2)==0) &
               & CALL FATAL_ERROR('TYPE 4 CHAR: OBC NODE MISSING FROM NESTING GRID')
          N4_NEDGE(J)=NE
          N4_N1(J,NE)=N1
          N4_N2(J,NE)=N2
          N4_INNER(J,NE)=NV(J,E)
# if defined (SPHERICAL)
          CALL ARCX(REAL(VX(N1),DP),REAL(VY(N1),DP), &
               & REAL(VX(N2),DP),REAL(VY(N2),DP),DX)
          CALL ARCY(REAL(VX(N1),DP),REAL(VY(N1),DP), &
               & REAL(VX(N2),DP),REAL(VY(N2),DP),DY)
          CALL ARCX(REAL(VX(N1),DP),REAL(VY(N1),DP), &
               & REAL(XC(J),DP),REAL(YC(J),DP),IX)
          CALL ARCY(REAL(VX(N1),DP),REAL(VY(N1),DP), &
               & REAL(XC(J),DP),REAL(YC(J),DP),IY)
# else
          DX=REAL(VX(N2),DP)-REAL(VX(N1),DP)
          DY=REAL(VY(N2),DP)-REAL(VY(N1),DP)
          IX=REAL(XC(J),DP)-REAL(VX(N1),DP)
          IY=REAL(YC(J),DP)-REAL(VY(N1),DP)
# endif
          CALL NEST4_EDGE_NORMAL(REAL(DX,SP),REAL(DY,SP),REAL(IX,SP), &
               & REAL(IY,SP),N4_NX(J,NE),N4_NY(J,NE),N4_LENGTH(J,NE))
          IF(N4_LENGTH(J,NE)<=0.0_SP .OR. &
               & .NOT. IEEE_IS_FINITE(N4_LENGTH(J,NE))) &
               & CALL FATAL_ERROR('TYPE 4 CHAR: DEGENERATE OPEN EDGE')
          IF(N4_ETA_LENGTH(N1)==0.0_SP) THEN
             N4_NNODE=N4_NNODE+1
             N4_NODEIDS(N4_NNODE)=N1
          END IF
          IF(N4_ETA_LENGTH(N2)==0.0_SP) THEN
             N4_NNODE=N4_NNODE+1
             N4_NODEIDS(N4_NNODE)=N2
          END IF
          N4_ETA_LENGTH(N1)=N4_ETA_LENGTH(N1)+N4_LENGTH(J,NE)
          N4_ETA_LENGTH(N2)=N4_ETA_LENGTH(N2)+N4_LENGTH(J,NE)
       END DO
       IF(N4_NEDGE(J)==0) CALL FATAL_ERROR('TYPE 4 CHAR: OBC CELL HAS NO OPEN EDGE')
       N4_NCELL=N4_NCELL+1
       N4_CELLIDS(N4_NCELL)=J
    END DO
    DO I=1,IOBCN
       J=I_OBC_N(I)
       IF(N4_ETA_LENGTH(J)<=0.0_SP) &
            & CALL FATAL_ERROR('TYPE 4 CHAR: LOCAL OBC NODE HAS NO OPEN EDGE')
    END DO
    N4_CHAR_READY=.TRUE.
  END SUBROUTINE NEST4_CHAR_INIT

  ! Called after the RK elevation boundary scaling, before N2E2D/FLUX_OBN.
  ! Incoming characteristic comes from time-interpolated parent data;
  ! outgoing characteristic is estimated from the adjacent FVCOM cell and
  ! its interior vertex (ELRK1 fallback at a two-edge corner).
  SUBROUTINE NEST4_CHAR_STAGE_EL(NOW)
    IMPLICIT NONE
    TYPE(TIME), INTENT(IN) :: NOW
    INTEGER :: I,J,E,N1,N2,K,PREV,NEXT,IDX,STEP,IC
    REAL(DP) :: NUMER,DENOM,PW,NW
    REAL(SP) :: ETA_EXT,ETA_INT,U_EXT,U_INT,DEPTH,G,ETA_BC,UN_BC,L
    CALL NEST4_CHAR_INIT
    IF(NESTING_DATA%TIME_BLK(1)>NOW .OR. &
         & NESTING_DATA%TIME_BLK(NESTING_DATA%NBLK)<NOW) CALL UPDATE_BLOCK(NOW)
    DO STEP=2,NESTING_DATA%NBLK
       IF(NOW<=NESTING_DATA%TIME_BLK(STEP)) EXIT
    END DO
    STEP=MIN(STEP,NESTING_DATA%NBLK)
    PREV=STEP-1; NEXT=STEP
    NUMER=SECONDS(NOW-NESTING_DATA%TIME_BLK(PREV))
    DENOM=SECONDS(NESTING_DATA%TIME_BLK(NEXT)-NESTING_DATA%TIME_BLK(PREV))
    IF(DENOM<=0.0_DP .OR. NUMER<0.0_DP .OR. NUMER>DENOM) &
         & CALL FATAL_ERROR('TYPE 4 CHAR: INVALID PARENT TIME BRACKET')
    NW=NUMER/DENOM; PW=1.0_DP-NW
    EL_NEST=NESTING_DATA%EL_BLK(:,PREV)*PW+NESTING_DATA%EL_BLK(:,NEXT)*NW
    UA_NEST=NESTING_DATA%UA_BLK(:,PREV)*PW+NESTING_DATA%UA_BLK(:,NEXT)*NW
    VA_NEST=NESTING_DATA%VA_BLK(:,PREV)*PW+NESTING_DATA%VA_BLK(:,NEXT)*NW
    DO I=1,N4_NNODE
       N4_ETA_SUM(N4_NODEIDS(I))=0.0_SP
    END DO
    DO IC=1,N4_NCELL
       J=N4_CELLIDS(IC)
       IDX=N4_CELLMAP(J)
       DO E=1,N4_NEDGE(J)
          N1=N4_N1(J,E); N2=N4_N2(J,E)
          ETA_EXT=0.5_SP*(EL_NEST(N4_NODEMAP(N1))+EL_NEST(N4_NODEMAP(N2)))
          U_EXT=UA_NEST(IDX)*N4_NX(J,E)+VA_NEST(IDX)*N4_NY(J,E)
          U_INT=UA(J)*N4_NX(J,E)+VA(J)*N4_NY(J,E)
          K=N4_INNER(J,E)
          IF(ISONB(K)==2) THEN
             ETA_INT=ELRK1(J)
          ELSE
             ETA_INT=ELF(K)
          END IF
          DEPTH=0.5_SP*(H(N1)+H(N2))+ETA_EXT
          IF(DEPTH<=MIN_DEPTH) &
               & CALL FATAL_ERROR('TYPE 4 CHAR: INVALID EDGE WATER DEPTH')
          G=0.5_SP*(GRAV_N(N1)+GRAV_N(N2))
          IF(G<=0.0_SP) CALL FATAL_ERROR('TYPE 4 CHAR: INVALID GRAVITY')
          CALL NEST4_CHARACTERISTIC_PAIR(U_INT,ETA_INT,U_EXT,ETA_EXT,G,DEPTH, &
               & UN_BC,ETA_BC)
          IF(.NOT. IEEE_IS_FINITE(ETA_BC) .OR. .NOT. IEEE_IS_FINITE(UN_BC)) &
               & CALL FATAL_ERROR('TYPE 4 CHAR: NONFINITE BOUNDARY STATE')
          N4_UN_TARGET(J,E)=UN_BC
          L=N4_LENGTH(J,E)
          N4_ETA_SUM(N1)=N4_ETA_SUM(N1)+L*ETA_BC
          N4_ETA_SUM(N2)=N4_ETA_SUM(N2)+L*ETA_BC
       END DO
    END DO
    ! The existing AEXCHANGE immediately after this routine updates halos.
    DO I=1,IOBCN
       J=I_OBC_N(I)
       ELF(J)=N4_ETA_SUM(J)/N4_ETA_LENGTH(J)
       IF(H(J)+ELF(J)<=MIN_DEPTH) &
            & CALL FATAL_ERROR('TYPE 4 CHAR: OBC ELEVATION WOULD DRY NODE')
    END DO
  END SUBROUTINE NEST4_CHAR_STAGE_EL

  ! Called after the regular velocity BCOND, before the RK state update.
  ! Applying to UAF/VAF (not UA/VA) ensures the next stage sees the modified
  ! boundary velocity and the interior momentum solver is left unchanged.
  SUBROUTINE NEST4_CHAR_STAGE_UV
    IMPLICIT NONE
    INTEGER :: J,INFO,IC
    REAL(SP) :: U_NEW,V_NEW
    DO IC=1,N4_NCELL
       J=N4_CELLIDS(IC)
       CALL NEST4_MATCH_EDGE_NORMALS(UAF(J),VAF(J),N4_NEDGE(J), &
            & N4_NX(J,1:2),N4_NY(J,1:2),N4_UN_TARGET(J,1:2), &
            & U_NEW,V_NEW,INFO)
       IF(INFO/=0 .OR. .NOT. IEEE_IS_FINITE(U_NEW) .OR. &
            & .NOT. IEEE_IS_FINITE(V_NEW)) &
            & CALL FATAL_ERROR('TYPE 4 CHAR: INVALID B-GRID EDGE VELOCITY')
       IF(SQRT((U_NEW-UAF(J))**2+(V_NEW-VAF(J))**2) > &
            & NESTING_MAX_CORRECTION_SPEED) &
            & CALL FATAL_ERROR('TYPE 4 CHAR: OBC VELOCITY CORRECTION TOO LARGE')
       UAF(J)=U_NEW
       VAF(J)=V_NEW
    END DO
  END SUBROUTINE NEST4_CHAR_STAGE_UV

  ! Conservative operator-split transport/SSH update, once per internal DTI.
  ! Never mutate FVCOM state until the projection converges and has been checked.
  SUBROUTINE NEST4_CORRECT_BAROTROPIC(NOW)
# if defined (SPHERICAL)
    USE MOD_NORTHPOLE, ONLY : NODE_NORTHPOLE
# endif
    IMPLICIT NONE
    TYPE(TIME), INTENT(IN) :: NOW
    INTEGER :: STEP,PREV,NEXT,I,J,INFO,ITER
    REAL(DP) :: NUMER,DENOM,PW,NW,NET_VOLUME_CHANGE
    REAL(SP) :: W,RELRES,DEPTH_NEW,WN,WT,DNORM
    REAL(SP), ALLOCATABLE :: DA(:),QSX(:),QSY(:),MOB(:),DQX(:),DQY(:)
    REAL(SP), ALLOCATABLE :: NEW_EL(:),NEW_EL1(:),NEW_D1(:)
    REAL(SP), ALLOCATABLE :: OBC_NX(:),OBC_NY(:)
    INTEGER :: INODE,ICELL

    IF(TRIM(NESTING_TYPE)/='4') RETURN
# if defined (SPHERICAL)
    ! EXTEL_EDGE_XY replaces the polar stencil, so standard B cannot be used.
    IF(NODE_NORTHPOLE/=0) CALL FATAL_ERROR('TYPE 4: NORTH POLE GRID NOT SUPPORTED')
# endif
    IF(NESTING_DATA%TIME_BLK(1)>NOW .OR. &
         & NESTING_DATA%TIME_BLK(NESTING_DATA%NBLK)<NOW) CALL UPDATE_BLOCK(NOW)
    DO STEP=2,NESTING_DATA%NBLK
       IF(NOW <= NESTING_DATA%TIME_BLK(STEP)) EXIT
    END DO
    STEP=MIN(STEP,NESTING_DATA%NBLK)
    PREV=STEP-1; NEXT=STEP
    NUMER=SECONDS(NOW-NESTING_DATA%TIME_BLK(PREV))
    DENOM=SECONDS(NESTING_DATA%TIME_BLK(NEXT)-NESTING_DATA%TIME_BLK(PREV))
    IF(DENOM<=0.0_DP .OR. NUMER<0.0_DP .OR. NUMER>DENOM) &
         & CALL FATAL_ERROR('TYPE 4: BAD TIME BRACKET IN BAROTROPIC CORRECTION')
    NW=NUMER/DENOM; PW=1.0_DP-NW
    EL_NEST=NESTING_DATA%EL_BLK(:,PREV)*PW+NESTING_DATA%EL_BLK(:,NEXT)*NW
    UA_NEST=NESTING_DATA%UA_BLK(:,PREV)*PW+NESTING_DATA%UA_BLK(:,NEXT)*NW
    VA_NEST=NESTING_DATA%VA_BLK(:,PREV)*PW+NESTING_DATA%VA_BLK(:,NEXT)*NW
    WNODE_NEST=NESTING_DATA%WNODE_BLK(:,PREV)*PW+NESTING_DATA%WNODE_BLK(:,NEXT)*NW
    WCELL_NEST=NESTING_DATA%WCELL_BLK(:,PREV)*PW+NESTING_DATA%WCELL_BLK(:,NEXT)*NW

    ALLOCATE(DA(0:MT),QSX(0:NT),QSY(0:NT),MOB(0:NT), &
         & DQX(0:NT),DQY(0:NT),NEW_EL(0:MT),NEW_EL1(0:NT),NEW_D1(0:NT))
    DA=0.0_SP; QSX=0.0_SP; QSY=0.0_SP; MOB=0.0_SP
    ! Use FVCOM's inward OBC node normals and adjacent OBC-element mapping.
    ! Sum node normals first, then normalize per element. Sign of the normal
    ! is immaterial to the two-component relaxation tensor.
    ALLOCATE(OBC_NX(0:NT),OBC_NY(0:NT))
    OBC_NX=0.0_SP; OBC_NY=0.0_SP
    DO INODE=1,IOBCN
       DO ICELL=1,NADJC_OBC(INODE)
          J=ADJC_OBC(INODE,ICELL)
          IF(J<1 .OR. J>NT) CYCLE
          IF(ISBCE(J)/=2) CYCLE
          OBC_NX(J)=OBC_NX(J)+NXOBC(INODE)
          OBC_NY(J)=OBC_NY(J)+NYOBC(INODE)
       END DO
    END DO
# if defined (MULTIPROCESSOR)
    IF(PAR) CALL AEXCHANGE(EC,MYID,NPROCS,OBC_NX,OBC_NY)
# endif

    ! Nodal sea-level target. OBC nodes are *never* changed by this correction:
    ! they were already clamped during every RK stage in EXTERNAL_STEP.
    DO I=1,NESTING_GRID%MT
       J=NESTING_GRID%NLID_X(I)
       IF(ISONB(J)/=0) CYCLE
       W=NEST4_NODE_WEIGHT(I,DTI,NESTING_TAU_ETA)
       DA(J)=W*(EL_NEST(I)-EL(J))
    END DO
    DO I=1,NESTING_GRID%NT
       J=NESTING_GRID%ELID_X(I)
       IF(ISBCE(J)==1 .OR. ISBCE(J)==3) CYCLE
       ! The characteristic RK update already set boundary-cell normal
       ! transport. Do not overwrite it with the post-step projection.
       IF(NESTING_CHARACTERISTIC_OBC .AND. ISBCE(J)==2) CYCLE
       IF(.NOT. IEEE_IS_FINITE(WCELL_NEST(I)) .OR. &
            & WCELL_NEST(I)<0.0_SP .OR. WCELL_NEST(I)>1.0_SP) &
            & CALL FATAL_ERROR('TYPE 4: INVALID weight_cell FOR BAROTROPIC PROJECTION')
       IF(ISBCE(J)==2) THEN
          ! Separate physical e-folding times at the open boundary. Unlike
          ! the hard OBC tracer/shear weight, these are NOT direct overwrites.
          DNORM=SQRT(OBC_NX(J)**2+OBC_NY(J)**2)
          IF(DNORM<=1.0E-6_SP .OR. .NOT. IEEE_IS_FINITE(DNORM)) &
               & CALL FATAL_ERROR('TYPE 4: OBC CELL NORMAL MISSING OR AMBIGUOUS')
          WN=NEST4_RELAX_WEIGHT(1.0_SP,DTI,NESTING_TAU_TRANSPORT_NORMAL,.FALSE.)
          WT=NEST4_RELAX_WEIGHT(1.0_SP,DTI,NESTING_TAU_TRANSPORT_TANGENTIAL,.FALSE.)
          CALL NEST4_TRANSPORT_TARGET(UA_NEST(I)-UA(J),VA_NEST(I)-VA(J), &
               & D1(J),OBC_NX(J)/DNORM,OBC_NY(J)/DNORM,WN,WT,QSX(J),QSY(J))
       ELSEIF(COUNT(ISONB(NV(J,1:3))==2)>0 .AND. &
            & NESTING_TAU_TRANSPORT_NORMAL==NESTING_TAU_TRANSPORT_TANGENTIAL) THEN
          ! Diagnostic OBC vertex band: treat one-OBC-node cells like OBC
          ! edge cells when normal and tangential relaxation times are equal.
          ! With WN==WT the target is isotropic and requires no edge normal.
          W=NEST4_RELAX_WEIGHT(1.0_SP,DTI,NESTING_TAU_TRANSPORT_NORMAL,.FALSE.)
          QSX(J)=W*D1(J)*(UA_NEST(I)-UA(J))
          QSY(J)=W*D1(J)*(VA_NEST(I)-VA(J))
       ELSE
          ! Existing isotropic relaxation inside the nesting zone is unchanged.
          W=NEST4_CELL_WEIGHT(I,DTI,NESTING_TAU_TRANSPORT)
          QSX(J)=W*D1(J)*(UA_NEST(I)-UA(J))
          QSY(J)=W*D1(J)*(VA_NEST(I)-VA(J))
       END IF
       ! A small non-zero mobility permits correction near the inner margin.
       ! Outside the nesting map MOB=0, so these velocities remain untouched.
       IF(WCELL_NEST(I)>0.0_SP .OR. ISBCE(J)==2 .OR. &
            & (COUNT(ISONB(NV(J,1:3))==2)>0 .AND. &
            & NESTING_TAU_TRANSPORT_NORMAL==NESTING_TAU_TRANSPORT_TANGENTIAL)) &
            & MOB(J)=MAX(WCELL_NEST(I),0.01_SP)
    END DO

    CALL NEST4_PROJECT_TRANSPORT(M,MT,NT,NCV,NTRG,NIEC,DLTXE,DLTYE, &
         & ART1,ISONB,MOB,DA,QSX,QSY,DTI,DQX,DQY,INFO,ITER,RELRES, &
         & NEST4_EXCHANGE_NODE,NEST4_EXCHANGE_CELL,NEST4_SUM_DP)
    IF(INFO/=0) THEN
       WRITE(IPT,*) 'TYPE 4: BAROTROPIC PROJECTION FAILED: ',INFO,ITER,RELRES
       CALL FATAL_ERROR('TYPE 4: CONSERVATIVE PROJECTION DID NOT CONVERGE')
    END IF

    ! Never cap a converged correction: clipping would violate continuity.
    ! Reject the entire step instead if a user-configured safety bound is hit.
    IF(NEST4_MAX_DP(REAL(MAXVAL(SQRT(DQX(1:N)**2+DQY(1:N)**2) / &
         & MAX(D1(1:N),MIN_DEPTH)),DP)) > REAL(NESTING_MAX_CORRECTION_SPEED,DP)) &
         & CALL FATAL_ERROR('TYPE 4: REQUIRED TRANSPORT CORRECTION EXCEEDS SPEED LIMIT')
    NET_VOLUME_CHANGE=NEST4_SUM_DP(SUM(REAL(ART1(1:M),DP)*REAL(DA(1:M),DP)))
    IF(DBG_SET(DBG_SCL) .AND. MOD(IINT,100)==0) &
         & WRITE(IPT,*) 'TYPE 4 D_VOLUME [m3] = ',NET_VOLUME_CHANGE
    NEW_EL=EL+DA
    IF(MINVAL(H(1:M)+NEW_EL(1:M))<=MIN_DEPTH) &
         & CALL FATAL_ERROR('TYPE 4: PROJECTION WOULD CREATE DRY/INVALID DEPTH')
    ! New element elevation must use the original FVCOM nodal-to-element operator.
    ! N2E2D is applied only after a converged projection.
    EL=NEW_EL
# if defined (MULTIPROCESSOR)
    IF(PAR) CALL AEXCHANGE(NC,MYID,NPROCS,EL)
# endif
    ELF=EL
    CALL N2E2D(EL,EL1)
    ELF1=EL1
    D=H+EL
    NEW_D1=H1+EL1
    DO I=1,NT
       ! Apply the projected *transport* (m2/s), not a velocity blend.
       ! D1 in the numerator is the pre-projection depth.
       DEPTH_NEW=NEW_D1(I)
       IF(DEPTH_NEW<=MIN_DEPTH) CALL FATAL_ERROR('TYPE 4: BAD ELEMENT DEPTH')
       UA(I)=(D1(I)*UA(I)+DQX(I))/DEPTH_NEW
       VA(I)=(D1(I)*VA(I)+DQY(I))/DEPTH_NEW
    END DO
    D1=NEW_D1
    UAF=UA; VAF=VA
    DTFA=D
    ! UARD/VARD are stored as sums over ISPLIT; ADJUST2D3D(2) divides by ISPLIT.
    ! The operator-split correction must be reflected in the target 3D transport.
    UARD=UARD+REAL(ISPLIT,SP)*DQX
    VARD=VARD+REAL(ISPLIT,SP)*DQY
# if defined (MULTIPROCESSOR)
    IF(PAR) CALL AEXCHANGE(EC,MYID,NPROCS,UA,VA,D1)
# endif
    IF(DBG_SET(DBG_SCL) .AND. MOD(IINT,100)==0) &
         & WRITE(IPT,*) 'TYPE 4 PROJECTION: ITER, RELRES = ',ITER,RELRES
  END SUBROUTINE NEST4_CORRECT_BAROTROPIC

  ! Communication adapters keep the projection kernel independent of MPI/FVCOM.
  SUBROUTINE NEST4_EXCHANGE_NODE(F)
    REAL(SP), ALLOCATABLE, INTENT(INOUT) :: F(:)
# if defined (MULTIPROCESSOR)
    IF(PAR) CALL AEXCHANGE(NC,MYID,NPROCS,F)
# endif
  END SUBROUTINE NEST4_EXCHANGE_NODE

  SUBROUTINE NEST4_EXCHANGE_CELL(F)
    REAL(SP), ALLOCATABLE, INTENT(INOUT) :: F(:)
# if defined (MULTIPROCESSOR)
    IF(PAR) CALL AEXCHANGE(EC,MYID,NPROCS,F)
# endif
  END SUBROUTINE NEST4_EXCHANGE_CELL

  REAL(DP) FUNCTION NEST4_MAX_DP(X) RESULT(Y)
    REAL(DP), INTENT(IN) :: X
# if defined (MULTIPROCESSOR)
    INTEGER :: IERR
    IF(PAR) THEN
       CALL MPI_ALLREDUCE(X,Y,1,MPI_DOUBLE_PRECISION,MPI_MAX,MPI_FVCOM_GROUP,IERR)
    ELSE
       Y=X
    END IF
# else
    Y=X
# endif
  END FUNCTION NEST4_MAX_DP

  REAL(DP) FUNCTION NEST4_SUM_DP(X) RESULT(Y)
    REAL(DP), INTENT(IN) :: X
# if defined (MULTIPROCESSOR)
    INTEGER :: IERR
    IF(PAR) THEN
       CALL MPI_ALLREDUCE(X,Y,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_FVCOM_GROUP,IERR)
    ELSE
       Y=X
    END IF
# else
    Y=X
# endif
  END FUNCTION NEST4_SUM_DP

  SUBROUTINE UPDATE_BLOCK(NOW)
    IMPLICIT NONE
    TYPE(TIME) :: NOW

    TYPE(NCDIM), POINTER :: DIM
# if defined (FABM)
    TYPE(NCVAR), POINTER :: VAR
# endif
    LOGICAL :: FOUND
    TYPE(TIME) :: TIMETEST
    INTEGER :: J,STATUS,mysize
# if defined (FABM)
    INTEGER :: IFABM
# endif

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "START UPDATE_BLOCK (NESTING)"

    ! FIRST - refresh the file in case it is being written concurrently
    CALL NC_REFRESH(NESTING_FILE)

    ! FIGURE OUT WHICH TIMES TO LOAD
    CALL UPDATE_FILE_BRACKET(NESTING_FILE,NOW,status)
    IF(STATUS > 0) THEN
       CALL FATAL_ERROR&
            &("UPDATE_BLOCK(NESTING): THE CURRENT TIME DOES NOT EXIST IN THE NESTING FILE:",&
            & "MODEL TIME IS PAST END OF FILE")
    ELSEIF(STATUS < 0) THEN
       CALL FATAL_ERROR&
            &("UPDATE_BLOCK(NESTING): THE CURRENT TIME DOES NOT EXIST IN THE NESTING FILE:",&
            & "MODEL TIME IS BEFORE START OF FILE")
    END IF


    ! DEAL WITH THE CASE WHERE NOW == NEXT_IO or IS INBETWEEN...
    IF(NESTING_FILE%FTIME%NEXT_IO > NOW) THEN
       NESTING_DATA%RNG(1)=NESTING_FILE%FTIME%PREV_STKCNT
       NESTING_DATA%RNG(2)=NESTING_FILE%FTIME%PREV_STKCNT+NESTING_BLOCKSIZE-1
    ELSE
       NESTING_DATA%RNG(1)=NESTING_FILE%FTIME%NEXT_STKCNT
       NESTING_DATA%RNG(2)=NESTING_FILE%FTIME%NEXT_STKCNT+NESTING_BLOCKSIZE-1
    END IF


!    write(ipt,*) NESTING_DATA%RNG
!    CALL PRINT_TIME(NOW,IPT,"Current time")
!    CALL PRINT_FTIME(NESTING_FILE%FTIME)
!    CALL PSHUTDOWN


    DIM => FIND_UNLIMITED(NESTING_FILE,FOUND)
    IF(.NOT. FOUND) CALL FATAL_ERROR&
         &("****COULD NOT FIND UNLIM DIM IN NESTING FILE?")

    ! DEAL WITH END OF RUN AND FILE
    IF(NESTING_DATA%RNG(2) > DIM%DIM) THEN
       ! DO NOT EXCEED THE END OF FILE
       NESTING_DATA%RNG(2) = DIM%DIM


       ! IF THIS IS NOT THE END OF THE RUN, MAKE IT AN ERROR
       TIMETEST = get_file_time(NESTING_FILE,DIM%DIM)

       IF (TIMETEST < EndTime)CALL FATAL_ERROR&
            &("BLK_STACK EXCEEDS FILE SIZE",&
            & "IF YOU ARE RUNNING THE SMALL AND LARGE DOMAIN AT THE SAME TIME",&
            & "THIS MEANS THE SMALL DOMAIN IS GETTING AHEAD OF THE LARGE DOMAIN!",&
            & "WAIT A FEW MINUTES AND RUN CRASH RESTART WITH FEWER PROCESSORS")

    END IF

    ! GET THE ACTUAL BLOCK SIZE OF THE DATA TO LOAD
    MYSIZE =  NESTING_DATA%RNG(2) - NESTING_DATA%RNG(1) +1

    CALL NC_READ_VAR(VAR_EL,STKRNG=NESTING_DATA%RNG)
    CALL NC_READ_VAR(VAR_UA,STKRNG=NESTING_DATA%RNG)
    CALL NC_READ_VAR(VAR_VA,STKRNG=NESTING_DATA%RNG)
    CALL NC_READ_VAR(VAR_U,STKRNG=NESTING_DATA%RNG)
    CALL NC_READ_VAR(VAR_V,STKRNG=NESTING_DATA%RNG)
    CALL NC_READ_VAR(VAR_T1,STKRNG=NESTING_DATA%RNG)
    CALL NC_READ_VAR(VAR_S1,STKRNG=NESTING_DATA%RNG)
    CALL NC_READ_VAR(VAR_HYW,STKRNG=NESTING_DATA%RNG)
# if defined (FABM)
       IF(FABM_NESTING_ON) THEN
       DO IFABM = 1,IFABMnest
         VAR => FIND_VAR(NESTING_FILE,TRIM(model%interior_state_variables(IFABMnest_idx(IFABM))%name),FOUND)! RJT looping only through advectable variables
         IF(.not. FOUND) CALL FATAL_ERROR("IN NESTING FILE OBJECT NO "// &
            & TRIM(model%interior_state_variables(IFABMnest_idx(IFABM))%name)//"  FOUND?")
!         CALL NC_CONNECT_PVAR(VAR, NESTING_DATA%FABM_ALL_BLK(:,:,:,IFABM))
!         CALL NC_READ_VAR(VAR,STKRNG=NESTING_DATA%RNG)
         CALL NC_CONNECT_PVAR(VAR, NESTING_DATA%FABM_ALL_TMP)
         CALL NC_READ_VAR(VAR,STKRNG=NESTING_DATA%RNG)
         NESTING_DATA%FABM_ALL_BLK(:,:,:,IFABM)=NESTING_DATA%FABM_ALL_TMP
       END DO
       END IF
# endif

    IF(TRIM(NESTING_TYPE) == '3' .OR. TRIM(NESTING_TYPE) == '4') &
         CALL NC_READ_VAR(VAR_WCELL,STKRNG=NESTING_DATA%RNG)
    IF(TRIM(NESTING_TYPE) == '3' .OR. TRIM(NESTING_TYPE) == '4') &
         CALL NC_READ_VAR(VAR_WNODE,STKRNG=NESTING_DATA%RNG)

    ! TIME IS A PROBLEM IS THE BLOCK SIZE DOES NOT MATCH THE
    ! ALLOCATED SIZE
    IF (MYSIZE /= NESTING_BLOCKSIZE) THEN
       DEALLOCATE(VAR_TIME1%VEC_INT)
       DEALLOCATE(VAR_TIME2%VEC_INT)

       ALLOCATE(VAR_TIME1%VEC_INT(MYSIZE))
       ALLOCATE(VAR_TIME2%VEC_INT(MYSIZE))

       CALL NC_READ_VAR(VAR_TIME1,STKRNG=NESTING_DATA%RNG)
       CALL NC_READ_VAR(VAR_TIME2,STKRNG=NESTING_DATA%RNG)

    ELSE
       CALL NC_READ_VAR(VAR_TIME1,STKRNG=NESTING_DATA%RNG)
       CALL NC_READ_VAR(VAR_TIME2,STKRNG=NESTING_DATA%RNG)
    END IF
        
    ! UPDATE THE DATA IN THE TIME BLOCK
    DO J=1,MYSIZE
       NESTING_DATA%TIME_BLK(J) = NCITIME(VAR_TIME1%VEC_INT(J),VAR_TIME2%VEC_INT(J))
    END DO

    ! RESET ALLOCATION and SET THE END OF THE DATA TO A LATER TIME...
    IF (MYSIZE /= NESTING_BLOCKSIZE) THEN
       
       J = MYSIZE +1
       NESTING_DATA%TIME_BLK(j:NESTING_BLOCKSIZE) = &
            & NESTING_DATA%TIME_BLK(MYSIZE) + days2time(1000)


       DEALLOCATE(VAR_TIME1%VEC_INT)
       DEALLOCATE(VAR_TIME2%VEC_INT)

       ALLOCATE(VAR_TIME1%VEC_INT(NESTING_BLOCKSIZE))
       ALLOCATE(VAR_TIME2%VEC_INT(NESTING_BLOCKSIZE))
    END IF


    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END UPDATE_BLOCK (NESTING)"

  END SUBROUTINE UPDATE_BLOCK
  !===========================================================================!
  !
  !============================================================================!   
# if defined (WAVE_CURRENT_INTERACTION)
  SUBROUTINE ALLOC_NEST_WAVE ! CALL IN ALLOCATE ALL - USE THE MEM_COUNT!
    IMPLICIT NONE
    TYPE(GRID), POINTER :: G
    TYPE(NEST_DATA_WAVE), POINTER :: D
    INTEGER :: STATUS,I
    INTEGER :: NDB
    !==============================================================================!
# if !defined (DOUBLE_PRECISION)
    NDB = 1       !!GWC BASE THIS ON KIND
# else
    NDB = 2
# endif

    IF(DBG_SET(DBG_SBR))WRITE(IPT,*) "START ALLOC_NEST_WAVE" 

    IF(NESTING_ON_WAVE) THEN

       G => NESTING_GRID_WAVE
       
       IF(TRIM(NESTING_TYPE_WAVE) == 'wave parameters')THEN

       ! ALLOCATE SPACE FOR INTERPOLATED DATA
       ALLOCATE(HSC1_NEST(0:G%MT),stat=status)
       if(status /= 0) call fatal_error("ALLOC_NEST: can not allocate:HSC1_NEST")

       ALLOCATE(TPEAK_NEST(0:G%MT),stat=status)
       if(status /= 0) call fatal_error("ALLOC_NEST: can not allocate:TPEAK_NEST")

       ALLOCATE(DIRDEG1_NEST(0:G%MT),stat=status)
       if(status /= 0) call fatal_error("ALLOC_NEST: can not allocate:DIRDEG1_NEST")

       MEMCNT = MEMCNT + G%MT * 3 * NDB

       ELSE IF(TRIM(NESTING_TYPE_WAVE) == 'spectral density')THEN

       ! ALLOCATE SPACE FOR INTERPOLATED DATA
       ALLOCATE(AC2_NEST(0:G%MT,MSC,MDC),stat=status)
       if(status /= 0) call fatal_error("ALLOC_NEST: can not allocate:AC2_NEST")

       MEMCNT = MEMCNT + G%MT * MDC * MSC * NDB
       
       ELSE
         CALL FATAL_ERROR("THE PARAMETER NESTING_TYPE_WAVE NOT CORRECT.")
       END IF

       

       ! ALLOCATE SPACE FOR DATA BLOCK IO FILE
       
       ALLOCATE(NESTING_DATA_WAVE,STAT=status)
       IF(STATUS /=0) CALL FATAL_ERROR("COULD NOT ALLOCATE NESTING_DATA_WAVE!")

       D => NESTING_DATA_WAVE

       D%NBLK = NESTING_BLOCKSIZE_WAVE

       IF(TRIM(NESTING_TYPE_WAVE) == 'wave parameters')THEN

       ! ALLOCATE SPACE FOR DATA BLOCK IO FILE
       ALLOCATE(D%HSC1_BLK(0:G%MT,NESTING_BLOCKSIZE_WAVE),stat=status)
       if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:HSC1_BLK")
       
       ALLOCATE(D%TPEAK_BLK(0:G%MT,NESTING_BLOCKSIZE_WAVE),stat=status)
       if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:TPEAK_BLK")
       
       ALLOCATE(D%DIRDEG1_BLK(0:G%MT,NESTING_BLOCKSIZE_WAVE),stat=status)
       if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:DIRDEG1_BLK")
       
       MEMCNT = MEMCNT + G%MT * 3 * NDB * NESTING_BLOCKSIZE_WAVE

       ELSE IF(TRIM(NESTING_TYPE_WAVE) == 'spectral density')THEN
       
       ! ALLOCATE SPACE FOR DATA BLOCK IO FILE
       ALLOCATE(D%AC2_BLK(0:G%MT,MSC,MDC,NESTING_BLOCKSIZE_WAVE),stat=status)
       if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:AC2_BLK")
       
       MEMCNT = MEMCNT + G%MT * MDC * MSC * NDB * NESTING_BLOCKSIZE_WAVE
       
       ELSE
         CALL FATAL_ERROR("THE PARAMETER NESTING_TYPE_WAVE NOT CORRECT.")
       END IF

       ALLOCATE(D%TIME_BLK(NESTING_BLOCKSIZE_WAVE),stat=status)
       if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:TIME_BLK")

       MEMCNT = MEMCNT + NDB * NESTING_BLOCKSIZE_WAVE

    END IF

    IF(NCNEST_ON_WAVE) THEN
       

       ALLOCATE(NCNEST_DATA_WAVE(NCNEST_NUM_WAVE),STAT=status)
       IF(STATUS /=0) CALL FATAL_ERROR("COULD NOT ALLOCATE NCNEST_DATA_WAVE!")
       
       DO I = 1, NCNEST_NUM_WAVE

         G=> NCNEST_GRIDS_WAVE(I)
         D => NCNEST_DATA_WAVE(I)

         D%NBLK = NCNEST_BLOCKSIZE_WAVE

         IF(TRIM(NCNEST_TYPE_WAVE) == 'wave parameters')THEN

         ! ALLOCATE SPACE FOR DATA BLOCK IO FILE
         ALLOCATE(D%HSC1_BLK(0:G%M,NCNEST_BLOCKSIZE_WAVE),stat=status)
         if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:HSC1_BLK")
         
         ALLOCATE(D%TPEAK_BLK(0:G%M,NCNEST_BLOCKSIZE_WAVE),stat=status)
         if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:TPEAK_BLK")
         
         ALLOCATE(D%DIRDEG1_BLK(0:G%M,NCNEST_BLOCKSIZE_WAVE),stat=status)
         if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:DIRDEG1_BLK")
         
         MEMCNT = MEMCNT + G%M * 3 * NDB * NCNEST_BLOCKSIZE_WAVE
         
         ELSE IF(TRIM(NCNEST_TYPE_WAVE) == 'spectral density')THEN
	 
         ! ALLOCATE SPACE FOR DATA BLOCK IO FILE
         ALLOCATE(D%AC2_BLK(0:G%M,MSC,MDC,NCNEST_BLOCKSIZE_WAVE),stat=status)
         if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:AC2_BLK")
         
         MEMCNT = MEMCNT + G%M * MDC * MSC * NDB * NCNEST_BLOCKSIZE_WAVE
         
	 ELSE
           CALL FATAL_ERROR("THE PARAMETER NCNEST_TYPE_WAVE NOT CORRECT.")
	 END IF

         ALLOCATE(D%TIME_BLK(NCNEST_BLOCKSIZE_WAVE),stat=status)
         if(status /= 0) call fatal_error("ALLOC_BLK: can not allocate:TIME_BLK")

         MEMCNT = MEMCNT + NDB * NESTING_BLOCKSIZE_WAVE

      END DO

   END IF

    IF(DBG_SET(DBG_SBR))WRITE(IPT,*) "END ALLOC_NEST_WAVE" 
   
   RETURN
 END SUBROUTINE ALLOC_NEST_WAVE
  !==================================================================================!
  !
  !==================================================================================!
  Subroutine SETUP_NEST_WAVE
    USE MOD_SETUP
    IMPLICIT NONE

    TYPE(NCDIM), POINTER :: DIM
    TYPE(NCVAR), POINTER :: VAR
    TYPE(NCATT), POINTER :: ATT
    LOGICAL :: FOUND

    TYPE(TIME):: TIMETEST

    ! ARRAY TO HOLD TIME DATA
    INTEGER, POINTER :: INT_PNT(:)
    
    INTEGER :: I,status
    
    IF(DBG_SET(DBG_SBR))WRITE(IPT,*) "START SETUP_NEST_WAVE" 

    IF(NCNEST_ON_WAVE)THEN

       ! SET UP THE GRID INDEX FOR OUTPUT
       DO I = 1, NCNEST_NUM_WAVE
          CALL SET_SUBDOMAIN_GRID(NCNEST_GRIDS_WAVE(I))
       END DO

       CALL SETUP_NCNEST_FILE_WAVE
       

    END IF

    IF(NESTING_ON_WAVE) THEN

       !=======================
       ! SANITY CHECK
       !=======================
       DIM => FIND_UNLIMITED(NESTING_FILE_WAVE,FOUND)
       IF(.not. FOUND) CALL FATAL_ERROR &
            & ("IN WAVE NESTING FILE OBJECT",&
            & "FILE NAME: "//TRIM(NESTING_FILE_WAVE%FNAME),&
            &"COULD NOT FIND THE UNLIMITED DIMENSION")

       TIMETEST = get_file_time(NESTING_FILE_WAVE,1)

       IF(StartTime < TIMETEST) THEN
          
          CALL PRINT_TIME(TIMETEST,IPT,"WAVE NESTING FILE STARTS")
          CALL PRINT_TIME(STARTTIME,IPT,"STARTTIME")
          
          CALL FATAL_ERROR&
               &("THE MODEL START TIME DOES NOT MATCH THE WAVE NESTING FILE START TIME")
       END IF
       
       ! TEST THE END TIME
       TIMETEST = get_file_time(NESTING_FILE_WAVE,DIM%DIM)
       IF(TIMETEST < EndTime) CALL WARNING &
            &("MODEL END TIME EXCEEDS THE WAVE NESTING FILE END TIME",&
            & "THIS WILL CAUSE AN ERROR IF THE SUBDOMAIN MODEL OUTPACES",&
            & "THE LARGE DOMAIN MODEL AND NO NEW NESTIND DATA IS AVAILABLE!")
       
       !!========================================
       !! CONNECT MEMORY AND READ FIRST TIME STEP
       !!=======================================

       ! FIND CORRECT TIME
       CALL UPDATE_FILE_BRACKET(NESTING_FILE_WAVE,StartTime,status)

       IF(NESTING_FILE_WAVE%FTIME%NEXT_IO > StartTime) THEN
          NESTING_DATA_WAVE%RNG(1)=NESTING_FILE_WAVE%FTIME%PREV_STKCNT
          NESTING_DATA_WAVE%RNG(2)=NESTING_FILE_WAVE%FTIME%PREV_STKCNT+NESTING_BLOCKSIZE_WAVE-1
       ELSE
          NESTING_DATA_WAVE%RNG(1)=NESTING_FILE_WAVE%FTIME%NEXT_STKCNT
          NESTING_DATA_WAVE%RNG(2)=NESTING_FILE_WAVE%FTIME%NEXT_STKCNT+NESTING_BLOCKSIZE_WAVE-1
       END IF


       IF(TRIM(NESTING_TYPE_WAVE) == 'wave parameters')THEN

       VAR => FIND_VAR(NESTING_FILE_WAVE,"hs",FOUND)
       IF(.not. FOUND) CALL FATAL_ERROR &
            & ("IN WAVE NESTING FILE OBJECT NO 'hs' FOUND?")
       CALL NC_CONNECT_PVAR(VAR, NESTING_DATA_WAVE%HSC1_BLK)
       CALL NC_READ_VAR(VAR,STKRNG=NESTING_DATA_WAVE%RNG)
       VAR_HSC1 => VAR

       VAR => FIND_VAR(NESTING_FILE_WAVE,"tpeak",FOUND)
       IF(.not. FOUND) CALL FATAL_ERROR &
            & ("IN WAVE NESTING FILE OBJECT NO 'tpeak' FOUND?")
       CALL NC_CONNECT_PVAR(VAR, NESTING_DATA_WAVE%TPEAK_BLK)
       CALL NC_READ_VAR(VAR,STKRNG=NESTING_DATA_WAVE%RNG)
       VAR_TPEAK => VAR
       
       VAR => FIND_VAR(NESTING_FILE_WAVE,"wdir",FOUND)
       IF(.not. FOUND) CALL FATAL_ERROR &
            & ("IN WAVE NESTING FILE OBJECT NO 'wdir' FOUND?")
       CALL NC_CONNECT_PVAR(VAR, NESTING_DATA_WAVE%DIRDEG1_BLK)
       CALL NC_READ_VAR(VAR,STKRNG=NESTING_DATA_WAVE%RNG)
       VAR_DIRDEG1=> VAR

       ELSE IF(TRIM(NESTING_TYPE_WAVE) == 'spectral density')THEN

       VAR => FIND_VAR(NESTING_FILE_WAVE,"ac2",FOUND)
       IF(.not. FOUND) CALL FATAL_ERROR &
            & ("IN WAVE NESTING FILE OBJECT NO 'ac2' FOUND?")
       CALL NC_CONNECT_PVAR(VAR, NESTING_DATA_WAVE%AC2_BLK)
       CALL NC_READ_VAR(VAR,STKRNG=NESTING_DATA_WAVE%RNG)
       VAR_AC2 => VAR

       ELSE
         CALL FATAL_ERROR("THE PARAMETER NESTING_TYPE_WAVE NOT CORRECT.")
       END IF
       
       ALLOCATE(INT_PNT(NESTING_BLOCKSIZE_WAVE))
       VAR => FIND_VAR(NESTING_FILE_WAVE,"Itime",FOUND)
       IF(.not. FOUND) CALL FATAL_ERROR &
            & ("IN WAVE NESTING FILE OBJECT NO 'Itime' FOUND?")
       CALL NC_CONNECT_PVAR(VAR, INT_PNT)
       CALL NC_READ_VAR(VAR,STKRNG=NESTING_DATA_WAVE%RNG)
       VAR_TIME1=> VAR

       NULLIFY(INT_PNT)
       ALLOCATE(INT_PNT(NESTING_BLOCKSIZE_WAVE))
       VAR => FIND_VAR(NESTING_FILE_WAVE,"Itime2",FOUND)
       IF(.not. FOUND) CALL FATAL_ERROR &
            & ("IN WAVE NESTING FILE OBJECT NO 'Itime2' FOUND?")
       CALL NC_CONNECT_PVAR(VAR, INT_PNT)
       CALL NC_READ_VAR(VAR,STKRNG=NESTING_DATA_WAVE%RNG)
       VAR_TIME2=> VAR

    END IF
    IF(DBG_SET(DBG_SBR))WRITE(IPT,*) "END SETUP_NEST_WAVE" 


  END Subroutine SETUP_NEST_WAVE
  !==================================================================================!
  !
  !========================================================================!  
  SUBROUTINE SETUP_NCNEST_FILE_WAVE
    USE CONTROL
    IMPLICIT NONE

    INTEGER :: I,status
    LOGICAL :: FOUND
    CHARACTER(LEN=80) :: FILE,PATH,EXT

    TYPE(NCFILE), POINTER :: NCF1,NCF2
    TYPE(NCVAR),  POINTER :: NCF3

    TYPE(GRID), POINTER :: G
    TYPE(NEST_DATA_WAVE), POINTER :: D

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "START SETUP_NCNEST_FILE_WAVE"

    IF(IOPROC) THEN
       ALLOCATE(NCNEST_DATA_WAVE(NCNEST_NUM_WAVE),STAT=status)
       IF(STATUS /=0) CALL FATAL_ERROR("IOPROC COULD NOT ALLOCATE NCNEST_DATA_WAVE!")
    END IF

    DO I = 1,NCNEST_NUM_WAVE
       

       G => NCNEST_GRIDS_WAVE(I)
       D => NCNEST_DATA_WAVE(I)

       
       ! ADD DO LOOP HERE OVER NUMBER OF OUTPUT FILES
       
       ! WHAT ABOUT SETTING UP THE DIMENSIONS?
       
       ! ALLOCATE THE NEW FILE OBJECT
       NCF1 => NEW_FILE()
       
       CALL PATH_SPLIT(NCNEST_FNAMES_WAVE(I),PATH,FILE,EXT)
       
       NCF1%FNAME = trim(OUTPUT_DIR)//trim(FILE)//".nc"
       
       ! CHANGE NAME IN LIST TO NETCDF NAME
       NCNEST_FNAMES_WAVE(I) = NCF1%FNAME

       CALL DEFINE_DIMENSIONS(G)

       NCF2 => GRID_FILE_OBJECT(G)
       NCF1 => ADD(NCF1,NCF2)
!!$       NCF1 => ADD(NCF1,GRID_FILE_OBJECT(G) )
       ! JUST MODIFY THE DIMENSION RETURNED - FVCOM WILL PICK THE
       ! CORRECT MAP AUTOMATICALLY!


        ! IINT
        NCF3  => IINT_OBJECT(DIM=Dim_time,SIZE=NCNEST_BLOCKSIZE_WAVE)
        NCF1  => ADD(NCF1,NCF3)
!!$        NCF1  => ADD(NCF1,IINT_OBJECT(DIM=Dim_time,SIZE=NCNEST_BLOCKSIZE_WAVE))
       
        ! time
        NCF3  => FLOAT_TIME_OBJECT_NEST(USE_MJD=use_real_world_time,DIM=DIM_TIME,SIZE=NCNEST_BLOCKSIZE_WAVE)
        NCF1  => ADD(NCF1, NCF3)
!!$        NCF1  => ADD(NCF1, FLOAT_TIME_OBJECT(USE_MJD=use_real_world_time,DIM=DIM_TIME,SIZE=NCNEST_BLOCKSIZE_WAVE))
       
        !ITIME
        NCF3 => ITIME_OBJECT_NEST(Use_MJD=use_real_world_time,DIM=DIM_TIME,SIZE=NCNEST_BLOCKSIZE_WAVE)
        NCF1 => ADD(NCF1,NCF3)
!!$        NCF1 => ADD(NCF1,ITIME_OBJECT(Use_MJD=use_real_world_time,DIM=DIM_TIME,SIZE=NCNEST_BLOCKSIZE_WAVE))

        NCF3 => ITIME2_OBJECT(Use_MJD=use_real_world_time,DIM=DIM_TIME,SIZE=NCNEST_BLOCKSIZE_WAVE)
        NCF1 => ADD(NCF1,NCF3)
!!$        NCF1 => ADD(NCF1,ITIME2_OBJECT(Use_MJD=use_real_world_time,DIM=DIM_TIME,SIZE=NCNEST_BLOCKSIZE_WAVE))

       ! NESTING WAVE DATA
       NCF2 => NESTING_FILE_OBJECT_WAVE(D)
       NCF1 => ADD(NCF1,NCF2)
!!$       NCF1 => ADD(NCF1,NESTING_FILE_OBJECT_WAVE(D) )
       
       NCF1%FTIME => NEW_FTIME()
       
       IF (STARTUP_TYPE /= "crashrestart") THEN
          CALL NC_WRITE_FILE(NCF1)
          NCF1%FTIME%NEXT_STKCNT=1
          D%RNG(1)=1
          D%RNG(2)=NCNEST_BLOCKSIZE_WAVE
          
       ELSE
          Call Set_File_Stack(NCF1,STARTTIME,IMDTI)
          NCF1%CONNECTED = .TRUE.
          NCF1%WRITABLE = .TRUE.

          D%RNG(1)=NCF1%FTIME%NEXT_STKCNT
          D%RNG(2)=NCF1%FTIME%NEXT_STKCNT + NCNEST_BLOCKSIZE_WAVE-1

          ! SUBTRACT ONE BECAUSE IT WILL STORE THE FIRST DATA NOW
          ! OTHERWISE IT WILL CREATE A REPEAT ENTRY IN THE FILE
          D%RNG = D%RNG -1

       END IF

       IF(DBG_SET(DBG_IO) .and. I==1)THEN
          WRITE(IPT,*) "SETUP IDX=",D%IDX,NCNEST_BLOCKSIZE_WAVE
          WRITE(IPT,*) "SETUP RNG=",D%RNG
          
       END IF

       FILEHEAD => ADD(FILEHEAD,NCF1)

       CALL KILL_DIMENSIONS

    END DO


       IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END SETUP_NCNEST_FILE_WAVE"
     END SUBROUTINE SETUP_NCNEST_FILE_WAVE
  !========================================================================!
  !
  !==================================================================================!
  FUNCTION NESTING_FILE_OBJECT_WAVE(D) RESULT(NCF)
    USE MOD_CLOCK
    IMPLICIT NONE
    TYPE(NEST_DATA_WAVE),POINTER::D

    INTEGER :: status
    TYPE(NCFILE), POINTER :: NCF
    TYPE(NCVAR),  POINTER :: VAR
    TYPE(NCATT),  POINTER :: ATT

    IF(DBG_SET(DBG_SBR))WRITE(IPT,*) "START NESTING_FILE_OBJECT_WAVE" 

    IF(IOPROC)THEN

      IF(TRIM(NCNEST_TYPE_WAVE) == 'wave parameters')THEN

       ALLOCATE(D%HSC1_BLK(Dim_node%dim,NCNEST_BLOCKSIZE_WAVE),STAT=STATUS) 
       IF (STATUS /=0 )  &
            & CALL FATAL_ERROR("COULD NOT ALLOCATE MEMORY ON IO PROC FOR OUTPUT DATA:HSC1_BLK")
       D%HSC1_BLK = 0.0_SP

       ALLOCATE(D%TPEAK_BLK(Dim_node%dim,NCNEST_BLOCKSIZE_WAVE),STAT=STATUS) 
       IF (STATUS /=0 )  &
            & CALL FATAL_ERROR("COULD NOT ALLOCATE MEMORY ON IO PROC FOR OUTPUT DATA:TPEAK_BLK")
       D%TPEAK_BLK = 0.0_SP

       ALLOCATE(D%DIRDEG1_BLK(Dim_node%dim,NCNEST_BLOCKSIZE_WAVE),STAT=STATUS) 
       IF (STATUS /=0 )  &
            & CALL FATAL_ERROR("COULD NOT ALLOCATE MEMORY ON IO PROC FOR OUTPUT DATA:DIRDEG1_BLK")
       D%DIRDEG1_BLK = 0.0_SP

      ELSE IF(TRIM(NCNEST_TYPE_WAVE) == 'spectral density')THEN

       ALLOCATE(D%AC2_BLK(Dim_node%dim,MSC,MDC,NCNEST_BLOCKSIZE_WAVE),STAT=STATUS) 
       IF (STATUS /=0 )  &
            & CALL FATAL_ERROR("COULD NOT ALLOCATE MEMORY ON IO PROC FOR OUTPUT DATA:AC2_BLK")
       D%AC2_BLK = 0.0_SP

      ELSE
        CALL FATAL_ERROR("THE PARAMETER NCNEST_TYPE_WAVE SHOULD BE 'wave parameters' or 'spectral density'")
      END IF
      
    END IF


    NCF => NEW_FILE()

    IF(TRIM(NCNEST_TYPE_WAVE) == 'wave parameters')THEN

    ! nest hs
    VAR  => NC_MAKE_PVAR(name='hs',&
         & values=D%HSC1_BLK, DIM1= Dim_node, DIM2 = DIM_TIME)

    ATT  => NC_MAKE_ATT(name='long_name',values='Significant Wave Height') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='units',values='meters') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='grid',values='fvcom_grid') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='type',values='data') 
    VAR  => ADD(VAR,ATT)

    NCF  => ADD(NCF,VAR)

    ! TPEAK_BLK
    VAR  => NC_MAKE_PVAR(name='tpeak',&
         & values=D%TPEAK_BLK, DIM1= Dim_node, DIM2= DIM_TIME)

    ATT  => NC_MAKE_ATT(name='long_name',values='Relative Peak Period') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='units',values='s') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='grid',values='fvcom_grid') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='type',values='data') 
    VAR  => ADD(VAR,ATT)

    NCF  => ADD(NCF,VAR)


    ! DIRDEG1_BLK
    VAR  => NC_MAKE_PVAR(name='wdir',&
         & values=D%DIRDEG1_BLK, DIM1= Dim_node, DIM2= DIM_TIME)

    ATT  => NC_MAKE_ATT(name='long_name',values='Wave Direction') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='units',values='degree') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='grid',values='fvcom_grid') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='type',values='data') 
    VAR  => ADD(VAR,ATT)

    NCF  => ADD(NCF,VAR)

    ELSE IF(TRIM(NCNEST_TYPE_WAVE) == 'spectral density')THEN

    ! nest AC2
    VAR  => NC_MAKE_PVAR(name='ac2',&
         & values=D%AC2_BLK, DIM1= Dim_node, DIM2 = Dim_MSC, DIM3 = DIM_MDC, DIM4 = DIM_TIME)

    ATT  => NC_MAKE_ATT(name='long_name',values='Wave Spectral Density') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='units',values='m^2/hz^2') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='grid',values='fvcom_grid') 
    VAR  => ADD(VAR,ATT)

    ATT  => NC_MAKE_ATT(name='type',values='data') 
    VAR  => ADD(VAR,ATT)

    NCF  => ADD(NCF,VAR)

    ELSE
      CALL FATAL_ERROR("THE PARAMETER NCNEST_TYPE_WAVE SHOULD BE 'wave parameters' or 'spectral density'")    
    END IF

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END NESTING_FILE_OBJECT_WAVE"

  END FUNCTION NESTING_FILE_OBJECT_WAVE
  !==================================================================================!
  !
  !===========================================================================!  
  SUBROUTINE ARCHIVE_NEST_WAVE
    USE MOD_SET_TIME, ONLY : GET_OUTPUT_FILE_INTERVAL
    IMPLICIT NONE
    INTEGER :: STATUS,I
    TYPE(NEST_DATA_WAVE), POINTER :: D
    TYPE(GRID), POINTER :: G
    TYPE(TIME) :: INTERVAL_TIME

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "START ARCHIVE_NEST_WAVE"

    
    ! SETUP THE OUTPUT IF THIS HAS NOT BEEN DONE YET
    if(NEED_INIT_NEST) then
       IF(USE_MPI_IO_MODE) THEN
          CALL MPI_IO_SYNCHRONIZE(INITNEST_CODE)
       ELSE
          CALL CALL_FUNC(INITNEST_CODE,status)
          IF (status/=0) call fatal_error("ARCHIVE:: Bad INITNEST_CODE_WAVE",&
               & "Could not retrieve valid function pointer?")
       END IF
       
       NEED_INIT_NEST = .FALSE.
       NCNEST_CMD_DUMP = .FALSE.
    end if

    ! EXIT EARLY IF WE ARE ONLY SETTING UP THE NESTING INPUT
    IF(.not. NCNEST_ON_WAVE) RETURN

    IF(MOD((EndTime - StartTime),INTERVAL_TIME_NCNEST_WAVE*NCNEST_BLOCKSIZE_WAVE) /= ZeroTime) &
          CALL FATAL_ERROR("The simulation time (EndTime-StartTime) should be divided", &
	    " by INTERVAL_TIME_NCNEST_WAVE*NCNEST_BLOCKSIZE_WAVE")
    CALL GET_OUTPUT_FILE_INTERVAL(TRIM(RST_OUT_INTERVAL),INTERVAL_TIME)
    IF(MOD(INTERVAL_TIME,INTERVAL_TIME_NCNEST_WAVE*NCNEST_BLOCKSIZE_WAVE) /= ZeroTime) &
          CALL FATAL_ERROR("The restart interval (RST_OUT_INTERVAL) should be divided", &
            " by INTERVAL_TIME_NCNEST_WAVE*NCNEST_BLOCKSIZE_WAVE")

    ! STORE DATA EVERYTIME
    IF(TIME_INTERVAL_WAVE <= IntTime)THEN
     DO I =1,NCNEST_NUM_WAVE
       D=>NCNEST_DATA_WAVE(I)
       G=>NCNEST_GRIDS_WAVE(I)
       CALL ASSIGN2BLOCK_WAVE(D,G)
     END DO
     TIME_INTERVAL_WAVE = IntTime + INTERVAL_TIME_NCNEST_WAVE
    
    ! WRITE THE FILE IF IT IS TIME
    IF(NCNEST_DATA_WAVE(1)%IDX == NCNEST_BLOCKSIZE_WAVE .or.   &
       IntTime >= EndTime .or. NCNEST_CMD_DUMP) then
       
       IF(USE_MPI_IO_MODE) THEN
          CALL MPI_IO_SYNCHRONIZE(NESTING_CODE_WAVE)
       ELSE
          CALL CALL_FUNC(NESTING_CODE_WAVE,status)
          IF (status/=0) call fatal_error("ARCHIVE:: Bad NESTING_CODE (WAVE)",&
               & "Could not retrieve valid function pointer?")
       END IF

       DO I =1,NCNEST_NUM_WAVE
          D=>NCNEST_DATA_WAVE(I)
          CALL RESET_BLOCK_WAVE(D)
       END DO
       
     END IF  
    END IF

!    ! WRITE THE FILE AT THE END OF RUN, OR IF THE RESTART FILE JUST GOT DUMPED!
!    IF(IntTime >= EndTime .or. NCNEST_CMD_DUMP) then
       
!      DO I =1,NCNEST_NUM_WAVE
!        D=>NCNEST_DATA_WAVE(I)
!        G=>NCNEST_GRIDS_WAVE(I)
!        CALL ASSIGN2BLOCK_WAVE(D,G)
!      END DO
      
!      IF(USE_MPI_IO_MODE) THEN
!        CALL MPI_IO_SYNCHRONIZE(NESTING_CODE_WAVE)
!      ELSE
!        CALL CALL_FUNC(NESTING_CODE_WAVE,status)
!        IF (status/=0) call fatal_error("ARCHIVE:: Bad NESTING_CODE (WAVE)",&
!            & "Could not retrieve valid function pointer?")
!      END IF

!      DO I =1,NCNEST_NUM_WAVE
!        D=>NCNEST_DATA_WAVE(I)
!        CALL RESET_BLOCK_WAVE(D)
!      END DO
       
!    END IF
    
    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END ARCHIVE_NEST_WAVE"
  END SUBROUTINE ARCHIVE_NEST_WAVE
  !========================================================================!
  !
  !========================================================================!
  SUBROUTINE ASSIGN2BLOCK_WAVE(D,G)
    IMPLICIT NONE

    TYPE(NEST_DATA_WAVE) :: D
    TYPE(GRID) :: G
    INTEGER :: II,ID,IS
    REAL(SP), ALLOCATABLE :: AC2_TT(:)

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "START ASSIGN2BLOCK_WAVE"

    D%IDX = D%IDX+1

    IF(TRIM(NCNEST_TYPE_WAVE) == 'wave parameters')THEN

    D%HSC1_BLK(:,D%IDX)    = HSC1(G%NLID(:))
    D%TPEAK_BLK(:,D%IDX)   = TPEAK(G%NLID(:))
    D%DIRDEG1_BLK(:,D%IDX) = DIRDEG1(G%NLID(:))
    D%TIME_BLK(D%IDX)      = INTTIME

    ELSE IF(TRIM(NCNEST_TYPE_WAVE) == 'spectral density')THEN
    
    DO ID=1,MDC
    DO IS=1,MSC
      ALLOCATE(AC2_TT(0:MT)) 
      AC2_TT(:) = AC2(ID,IS,:)
      D%AC2_BLK(:,IS,ID,D%IDX)    = AC2_TT(G%NLID(:))
      DEALLOCATE(AC2_TT)
    END DO  
    END DO
    D%TIME_BLK(D%IDX)      = INTTIME

    ELSE
      CALL FATAL_ERROR("THE PARAMETER NCNEST_TYPE_WAVE SHOULD BE 'wave parameters' or 'spectral density'")
    END IF
    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END ASSIGN2BLOCK_WAVE"

  END SUBROUTINE ASSIGN2BLOCK_WAVE
  !========================================================================!
  !
  !========================================================================!
  SUBROUTINE RESET_BLOCK_WAVE(D)
    IMPLICIT NONE

    TYPE(NEST_DATA_WAVE) :: D

    D%IDX=0

    IF(TRIM(NCNEST_TYPE_WAVE) == 'wave parameters')THEN

    D%HSC1_BLK    =  -99.0_SP
    D%TPEAK_BLK   =  -99.0_SP
    D%DIRDEG1_BLK =  -99.0_SP
    D%TIME_BLK    =  zerotime

    ELSE IF(TRIM(NCNEST_TYPE_WAVE) == 'spectral density')THEN

    D%AC2_BLK    =  -99.0_SP
    D%TIME_BLK    =  zerotime
    
    ELSE
      CALL FATAL_ERROR("THE PARAMETER NCNEST_TYPE_WAVE SHOULD BE 'wave parameters' or 'spectral density'")
    END IF

  END SUBROUTINE RESET_BLOCK_WAVE
  !========================================================================!
  !
  !========================================================================!
  SUBROUTINE UPDATE_BLOCK_WAVE(NOW)
    IMPLICIT NONE
    TYPE(TIME) :: NOW

    TYPE(NCDIM), POINTER :: DIM
    LOGICAL :: FOUND
    TYPE(TIME) :: TIMETEST
    INTEGER :: J,STATUS,mysize

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "START UPDATE_BLOCK_WAVE (NESTING)"

    ! FIRST - refresh the file in case it is being written concurrently
    CALL NC_REFRESH(NESTING_FILE_WAVE)

    ! FIGURE OUT WHICH TIMES TO LOAD
    CALL UPDATE_FILE_BRACKET(NESTING_FILE_WAVE,NOW,status)
    IF(STATUS > 0) THEN
       CALL FATAL_ERROR&
            &("UPDATE_BLOCK_WAVE(NESTING): THE CURRENT TIME DOES NOT EXIST IN THE NESTING FILE:",&
            & "MODEL TIME IS PAST END OF FILE")
    ELSEIF(STATUS < 0) THEN
       CALL FATAL_ERROR&
            &("UPDATE_BLOCK_WAVE(NESTING): THE CURRENT TIME DOES NOT EXIST IN THE NESTING FILE:",&
            & "MODEL TIME IS BEFORE START OF FILE")
    END IF


    ! DEAL WITH THE CASE WHERE NOW == NEXT_IO or IS INBETWEEN...
    IF(NESTING_FILE_WAVE%FTIME%NEXT_IO > NOW) THEN
       NESTING_DATA_WAVE%RNG(1)=NESTING_FILE_WAVE%FTIME%PREV_STKCNT
       NESTING_DATA_WAVE%RNG(2)=NESTING_FILE_WAVE%FTIME%PREV_STKCNT+NESTING_BLOCKSIZE_WAVE-1
    ELSE
       NESTING_DATA_WAVE%RNG(1)=NESTING_FILE_WAVE%FTIME%NEXT_STKCNT
       NESTING_DATA_WAVE%RNG(2)=NESTING_FILE_WAVE%FTIME%NEXT_STKCNT+NESTING_BLOCKSIZE_WAVE-1
    END IF


!    write(ipt,*) NESTING_DATA%RNG
!    CALL PRINT_TIME(NOW,IPT,"Current time")
!    CALL PRINT_FTIME(NESTING_FILE%FTIME)
!    CALL PSHUTDOWN


    DIM => FIND_UNLIMITED(NESTING_FILE_WAVE,FOUND)
    IF(.NOT. FOUND) CALL FATAL_ERROR&
         &("****COULD NOT FIND UNLIM DIM IN NESTING FILE?")

    ! DEAL WITH END OF RUN AND FILE
    IF(NESTING_DATA_WAVE%RNG(2) > DIM%DIM) THEN
       ! DO NOT EXCEED THE END OF FILE
       NESTING_DATA_WAVE%RNG(2) = DIM%DIM


       ! IF THIS IS NOT THE END OF THE RUN, MAKE IT AN ERROR
       TIMETEST = get_file_time(NESTING_FILE_WAVE,DIM%DIM)

       IF (TIMETEST < EndTime)CALL FATAL_ERROR&
            &("BLK_STACK EXCEEDS FILE SIZE",&
            & "IF YOU ARE RUNNING THE SMALL AND LARGE DOMAIN AT THE SAME TIME",&
            & "THIS MEANS THE SMALL DOMAIN IS GETTING AHEAD OF THE LARGE DOMAIN!",&
            & "WAIT A FEW MINUTES AND RUN CRASH RESTART WITH FEWER PROCESSORS")

    END IF

    ! GET THE ACTUAL BLOCK SIZE OF THE DATA TO LOAD
    MYSIZE =  NESTING_DATA_WAVE%RNG(2) - NESTING_DATA_WAVE%RNG(1) +1

    IF(TRIM(NESTING_TYPE_WAVE) == 'wave parameters')THEN

    CALL NC_READ_VAR(VAR_HSC1,STKRNG=NESTING_DATA_WAVE%RNG)
    CALL NC_READ_VAR(VAR_TPEAK,STKRNG=NESTING_DATA_WAVE%RNG)
    CALL NC_READ_VAR(VAR_DIRDEG1,STKRNG=NESTING_DATA_WAVE%RNG)

    ELSE IF(TRIM(NESTING_TYPE_WAVE) == 'spectral density')THEN

    CALL NC_READ_VAR(VAR_AC2,STKRNG=NESTING_DATA_WAVE%RNG)

    ELSE
      CALL FATAL_ERROR("THE PARAMETER NESTING_TYPE_WAVE SHOULD BE 'wave parameters' or 'spectral density'")
    END IF
    
    ! TIME IS A PROBLEM IS THE BLOCK SIZE DOES NOT MATCH THE
    ! ALLOCATED SIZE
    IF (MYSIZE /= NESTING_BLOCKSIZE_WAVE) THEN
       DEALLOCATE(VAR_TIME1%VEC_INT)
       DEALLOCATE(VAR_TIME2%VEC_INT)

       ALLOCATE(VAR_TIME1%VEC_INT(MYSIZE))
       ALLOCATE(VAR_TIME2%VEC_INT(MYSIZE))

       CALL NC_READ_VAR(VAR_TIME1,STKRNG=NESTING_DATA_WAVE%RNG)
       CALL NC_READ_VAR(VAR_TIME2,STKRNG=NESTING_DATA_WAVE%RNG)

    ELSE
       CALL NC_READ_VAR(VAR_TIME1,STKRNG=NESTING_DATA_WAVE%RNG)
       CALL NC_READ_VAR(VAR_TIME2,STKRNG=NESTING_DATA_WAVE%RNG)
    END IF
        
    ! UPDATE THE DATA IN THE TIME BLOCK
    DO J=1,MYSIZE
       NESTING_DATA_WAVE%TIME_BLK(J) = NCITIME(VAR_TIME1%VEC_INT(J),VAR_TIME2%VEC_INT(J))
    END DO

    ! RESET ALLOCATION and SET THE END OF THE DATA TO A LATER TIME...
    IF (MYSIZE /= NESTING_BLOCKSIZE_WAVE) THEN
       
       J = MYSIZE +1
       NESTING_DATA_WAVE%TIME_BLK(j:NESTING_BLOCKSIZE_WAVE) = &
            & NESTING_DATA_WAVE%TIME_BLK(MYSIZE) + days2time(1000)


       DEALLOCATE(VAR_TIME1%VEC_INT)
       DEALLOCATE(VAR_TIME2%VEC_INT)

       ALLOCATE(VAR_TIME1%VEC_INT(NESTING_BLOCKSIZE_WAVE))
       ALLOCATE(VAR_TIME2%VEC_INT(NESTING_BLOCKSIZE_WAVE))
    END IF


    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END UPDATE_BLOCK_WAVE (NESTING)"

  END SUBROUTINE UPDATE_BLOCK_WAVE
  !========================================================================!  
  !
  !========================================================================!    
  SUBROUTINE SET_VAR_WAVE(NOW,HSC1,TPEAK,DIRDEG1,                              &
       & ZERO_2D_NODES,ZERO_2D_NODES_OBC,ZERO_3D_NODES,ZERO_3D_NODES_OBC,&
       & ZERO_2D_CELLS,ZERO_2D_CELLS_OBC,ZERO_3D_CELLS,ZERO_3D_CELLS_OBC)

    IMPLICIT NONE
    TYPE(TIME), INTENT(IN) :: NOW    
    REAL(SP), ALLOCATABLE, OPTIONAL :: HSC1(:),TPEAK(:),DIRDEG1(:) 
    REAL(SP), ALLOCATABLE, OPTIONAL :: ZERO_3D_NODES(:,:),ZERO_3D_NODES_OBC(:,:)
    REAL(SP), ALLOCATABLE, OPTIONAL :: ZERO_2D_NODES(:),ZERO_2D_NODES_OBC(:)

    REAL(SP), ALLOCATABLE, OPTIONAL :: ZERO_3D_CELLS(:,:),ZERO_3D_CELLS_OBC(:,:)
    REAL(SP), ALLOCATABLE, OPTIONAL :: ZERO_2D_CELLS(:),ZERO_2D_CELLS_OBC(:)


    REAL(DP)     :: denom, numer
    REAL(DP)     :: prev_w, next_w
    INTEGER      :: STEP,PREV,NEXT,I
    
    REAL(SP), ALLOCATABLE :: X0(:),X1(:),X2(:),Y0(:),Y1(:),Y2(:),ANGLE(:)

    TYPE(TIME) :: TDIFF
    TYPE(GRID), POINTER :: G

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "START SET_VAR_WAVE (NESTING)"

    ! SEE IF WE NEED NEW DATA
    IF ( NESTING_DATA_WAVE%TIME_BLK(1) > NOW .OR. &
          NESTING_DATA_WAVE%TIME_BLK(NESTING_DATA_WAVE%NBLK) < NOW) CALL UPDATE_BLOCK_WAVE(NOW)

    ! FIND WHICH INDEX IN THE BLOCK BRACKETS NOW
    DO STEP = 1,NESTING_DATA_WAVE%NBLK
       IF(NOW < NESTING_DATA_WAVE%TIME_BLK(STEP)) EXIT
    END DO
    ! MAKE SURE NO LARGER THAN BLOCK SIZE (FOR == CASE!)
    STEP = MIN(NESTING_DATA_WAVE%NBLK,STEP)


!    CALL PRINT_TIME(NOW,IPT,"Current time")
!    CALL PRINT_TIME(NESTING_DATA%TIME_BLK(STEP),IPT,"STEP")


    PREV = STEP -1
    NEXT = STEP 

    ! GET THE INTERPOLATION WEIGHTS

    ! THE DIFFERENCE BETWEEN THE CURRENT TIME IN THE MODEL AND THE PREVIOUS DATA
    NUMER = SECONDS(NOW - NESTING_DATA_WAVE%TIME_BLK(PREV))
    DENOM = SECONDS(NESTING_DATA_WAVE%TIME_BLK(NEXT) - NESTING_DATA_WAVE%TIME_BLK(PREV))
    IF(ABS(DENOM) < 1.0E-7) DENOM = 1.0E-7

    ! TAKE THE RATIO IN DOUBLE PRECISION AND CONVERT IF MODEL IS NOT DOUBLE
    NEXT_W = NUMER/DENOM
    PREV_W = 1.0_DP - NEXT_W

    IF(DBG_SET(DBG_SBRIO))THEN
       write(ipt,*) "========== NESTING SET_VAR_2D ==================="
       CALL PRINT_TIME(NOW,IPT,"Current time")
       CALL PRINT_TIME(NESTING_DATA_WAVE%TIME_BLK(PREV),IPT,"PREVIOUS STEP")
       CALL PRINT_TIME(NESTING_DATA_WAVE%TIME_BLK(NEXT),IPT,"NEXT STEP")
       write(ipt,*) "PREV/NEXT, PWGHT/NWGHT",PREV,NEXT,", ",PREV_W,NEXT_W
       write(ipt,*) "================================================="
    END IF

!!$    ! GET THE DATA
!!$    DAT = NEXT_W * BLK_DAT(:,:,NEXT_I) + PREV_W * BLK_DAT(:,:,PREV_I)
    IF(PRESENT(HSC1)) THEN
       HSC1_NEST(:) =  NESTING_DATA_WAVE%HSC1_BLK(:,PREV) * PREV_W + &
                    NESTING_DATA_WAVE%HSC1_BLK(:,NEXT) * NEXT_W

       DO I = 1, NESTING_GRID_WAVE%MT
          HSC1(NESTING_GRID_WAVE%NLID_X(I)) =  HSC1_NEST(I)
       END DO

    ELSEIF(PRESENT(TPEAK)) THEN
       TPEAK_NEST =  NESTING_DATA_WAVE%TPEAK_BLK(:,PREV) * PREV_W +&
                & NESTING_DATA_WAVE%TPEAK_BLK(:,NEXT) * NEXT_W

       DO I = 1, NESTING_GRID_WAVE%MT
          TPEAK(NESTING_GRID_WAVE%NLID_X(I)) =  TPEAK_NEST(I)
       END DO

    ELSEIF(PRESENT(DIRDEG1)) THEN
       G => NESTING_GRID_WAVE
       ALLOCATE(X1(0:G%MT))
       ALLOCATE(X2(0:G%MT))
       ALLOCATE(Y1(0:G%MT))
       ALLOCATE(Y2(0:G%MT))
       ALLOCATE(X0(0:G%MT))
       ALLOCATE(Y0(0:G%MT))
       ALLOCATE(ANGLE(0:G%MT))
       X1 = COS(NESTING_DATA_WAVE%DIRDEG1_BLK(:,PREV)*DEG2RAD)
       X2 = COS(NESTING_DATA_WAVE%DIRDEG1_BLK(:,NEXT)*DEG2RAD)
       Y1 = SIN(NESTING_DATA_WAVE%DIRDEG1_BLK(:,PREV)*DEG2RAD)  
       Y2 = SIN(NESTING_DATA_WAVE%DIRDEG1_BLK(:,NEXT)*DEG2RAD)  
       X0 = X1*PREV_W + X2*NEXT_W
       Y0 = Y1*PREV_W + Y2*NEXT_W
       ANGLE = ATAN2(Y0,X0)
       WHERE(ANGLE < 0.) ANGLE = ANGLE + PI*2.0
       DIRDEG1_NEST = ANGLE*180.0/PI          
       
!       DIRDEG1_NEST =  NESTING_DATA_WAVE%DIRDEG1_BLK(:,PREV) * PREV_W +&
!                & NESTING_DATA_WAVE%DIRDEG1_BLK(:,NEXT) * NEXT_W

       DO I = 1, NESTING_GRID_WAVE%MT
          DIRDEG1(NESTING_GRID_WAVE%NLID_X(I)) =  DIRDEG1_NEST(I)
       END DO
       
       DEALLOCATE(X1,X2,Y1,Y2,X0,Y0,ANGLE)

    END IF



    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END SET_VAR_WAVE (NESTING)"
    
  END SUBROUTINE SET_VAR_WAVE
  !========================================================================!    
  !
  !========================================================================!    
  SUBROUTINE SET_VAR_WAVE_AC2(NOW,AC2)

    IMPLICIT NONE
    TYPE(TIME), INTENT(IN) :: NOW    
    REAL(SP), ALLOCATABLE, OPTIONAL :: AC2(:,:,:) 

    REAL(DP)     :: denom, numer
    REAL(DP)     :: prev_w, next_w
    INTEGER      :: STEP,PREV,NEXT,I,ID,IS
    
    REAL(SP), ALLOCATABLE :: X0(:),X1(:),X2(:),Y0(:),Y1(:),Y2(:),ANGLE(:)

    TYPE(TIME) :: TDIFF
    TYPE(GRID), POINTER :: G

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "START SET_VAR_WAVE (NESTING)"

    ! SEE IF WE NEED NEW DATA
    IF ( NESTING_DATA_WAVE%TIME_BLK(1) > NOW .OR. &
          NESTING_DATA_WAVE%TIME_BLK(NESTING_DATA_WAVE%NBLK) < NOW) CALL UPDATE_BLOCK_WAVE(NOW)

    ! FIND WHICH INDEX IN THE BLOCK BRACKETS NOW
    DO STEP = 1,NESTING_DATA_WAVE%NBLK
       IF(NOW < NESTING_DATA_WAVE%TIME_BLK(STEP)) EXIT
    END DO
    ! MAKE SURE NO LARGER THAN BLOCK SIZE (FOR == CASE!)
    STEP = MIN(NESTING_DATA_WAVE%NBLK,STEP)


!    CALL PRINT_TIME(NOW,IPT,"Current time")
!    CALL PRINT_TIME(NESTING_DATA%TIME_BLK(STEP),IPT,"STEP")


    PREV = STEP -1
    NEXT = STEP 

    ! GET THE INTERPOLATION WEIGHTS

    ! THE DIFFERENCE BETWEEN THE CURRENT TIME IN THE MODEL AND THE PREVIOUS DATA
    NUMER = SECONDS(NOW - NESTING_DATA_WAVE%TIME_BLK(PREV))
    DENOM = SECONDS(NESTING_DATA_WAVE%TIME_BLK(NEXT) - NESTING_DATA_WAVE%TIME_BLK(PREV))
    IF(ABS(DENOM) < 1.0E-7) DENOM = 1.0E-7

    ! TAKE THE RATIO IN DOUBLE PRECISION AND CONVERT IF MODEL IS NOT DOUBLE
    NEXT_W = NUMER/DENOM
    PREV_W = 1.0_DP - NEXT_W

    IF(DBG_SET(DBG_SBRIO))THEN
       write(ipt,*) "========== NESTING SET_VAR_2D ==================="
       CALL PRINT_TIME(NOW,IPT,"Current time")
       CALL PRINT_TIME(NESTING_DATA_WAVE%TIME_BLK(PREV),IPT,"PREVIOUS STEP")
       CALL PRINT_TIME(NESTING_DATA_WAVE%TIME_BLK(NEXT),IPT,"NEXT STEP")
       write(ipt,*) "PREV/NEXT, PWGHT/NWGHT",PREV,NEXT,", ",PREV_W,NEXT_W
       write(ipt,*) "================================================="
    END IF

!!$    ! GET THE DATA
!!$    DAT = NEXT_W * BLK_DAT(:,:,NEXT_I) + PREV_W * BLK_DAT(:,:,PREV_I)
    IF(PRESENT(AC2)) THEN
       AC2_NEST(:,:,:) =  NESTING_DATA_WAVE%AC2_BLK(:,:,:,PREV) * PREV_W + &
                    NESTING_DATA_WAVE%AC2_BLK(:,:,:,NEXT) * NEXT_W

       DO ID=1,MDC
       DO IS=1,MSC
       DO I = 1, NESTING_GRID_WAVE%MT
          AC2(ID,IS,NESTING_GRID_WAVE%NLID_X(I)) =  AC2_NEST(I,IS,ID)
       END DO
       END DO
       END DO

    END IF

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END SET_VAR_WAVE (NESTING)"
    
  END SUBROUTINE SET_VAR_WAVE_AC2
# endif
# else

  ! DEFINE SOME DUMMY ROUTINES

  SUBROUTINE SET_VAR(NOW,UA,VA,EL,U,V,S1,T1,HYW)
    IMPLICIT NONE
    TYPE(TIME), INTENT(IN) :: NOW    
    REAL(SP), ALLOCATABLE, OPTIONAL :: UA(:),VA(:),EL(:)
    REAL(SP), ALLOCATABLE, OPTIONAL :: U(:,:),V(:,:),S1(:,:),T1(:,:),HYW(:,:)
  END SUBROUTINE SET_VAR

  SUBROUTINE SET_VAR_WAVE(NOW,HSC1,TPEAK,DIRDEG1)
    IMPLICIT NONE
    TYPE(TIME), INTENT(IN) :: NOW    
    REAL(SP), ALLOCATABLE, OPTIONAL :: HSC1(:),TPEAK(:),DIRDEG1(:)   
  END SUBROUTINE SET_VAR_WAVE
  
  SUBROUTINE ARCHIVE_NEST
    IMPLICIT NONE
  END SUBROUTINE ARCHIVE_NEST

  SUBROUTINE ARCHIVE_NEST_WAVE
    IMPLICIT NONE
  END SUBROUTINE ARCHIVE_NEST_WAVE

# endif

!====================================================================
  SUBROUTINE UPDATE_ITIME_NEST(VAR1,VAR2,NOW)
    IMPLICIT NONE
    TYPE(NCVAR),  POINTER :: VAR1
    TYPE(NCVAR),  POINTER :: VAR2
    TYPE(NCATT), POINTER :: ATT
    TYPE(TIME), INTENT(in) :: NOW
    INTEGER, POINTER :: D1,D2
    LOGICAL :: TEST2
    INTEGER :: TEST
    CHARACTER(len=80):: TZONE

    TEST2 = IS_VALID_ITIME(VAR1,VAR2,tzone)
    IF(.not. TEST2) THEN
       CALL PRINT_VAR(VAR1)
       CALL PRINT_VAR(VAR2)
       CALL FATAL_ERROR &
            ("CAN NOT UPDATE TIME FOR INVALID INTEGER TIME VARIABLES")
    END IF

    CALL NC_POINT_VAR(VAR1,D1)

    CALL NC_POINT_VAR(VAR2,D2)

    TEST = TIME2NCITIME_NEST(NOW,D1,D2)

!    if(.not. TEST) call fatal_error("That is bad times man!") 
    if(TEST==0) call fatal_error("That is bad times man!")
    ! THIS SHOULD NEVER HAPPEN?
    
  END SUBROUTINE UPDATE_ITIME_NEST
!====================================================================
!======================================================
   FUNCTION TIME2NCITIME_NEST(MJD,D,MS) RESULT(res)
     implicit none
     INTEGER :: RES
     TYPE(TIME), INTENT(IN) :: MJD
     INTEGER, INTENT(OUT) :: D, MS
     REAL(DP) :: MSEC

     res = -1
     MSEC = dble(MJD%MuSod) / 1000.0_DP
     MS = ANINT(MSEC)

     ! CHECK TO MAKE SURE IT IS NOT TOO LARGE
     IF (ABS(MJD%MJD) .GT. HUGE(D)) THEN
        res =0 
        return
     END IF
     
     D = MJD%MJD

   END FUNCTION TIME2NCITIME_NEST
!======================================================
!====================================================================
  SUBROUTINE UPDATE_FLOAT_TIME_NEST(VAR,NOW)
    IMPLICIT NONE
    TYPE(NCVAR),  POINTER :: VAR
    TYPE(TIME), INTENT(in) :: NOW
    REAL(SP), POINTER :: Data
    LOGICAL :: TEST
    CHARACTER(len=80):: TZONE

    TEST = IS_VALID_FLOAT_DAYS(VAR,TZONE)
    IF(.not. TEST) THEN
       CALL PRINT_VAR(VAR)
       call print_att_list(VAR)
       CALL FATAL_ERROR &
            ("CAN NOT UPDATE TIME FOR INVALID FLOATING POINT TIME VARIABLE")
    END IF

    CALL NC_POINT_VAR(VAR,Data)
    
    Data = DAYS(NOW)

  END SUBROUTINE UPDATE_FLOAT_TIME_NEST
!====================================================================
!====================================================================
 FUNCTION Float_time_OBJECT_NEST(use_mjd,DIM,size) RESULT(VAR)
    IMPLICIT NONE
    TYPE(NCVAR),  POINTER :: VAR
    logical, intent(in) :: use_mjd
    TYPE(NCDIM),  POINTER, OPTIONAL :: DIM
    INTEGER, OPTIONAL :: SIZE
    TYPE(NCATT), POINTER :: ATT
    REAL(SP),pointer :: Data_vec(:)
    REAL(SP),pointer :: Data_scl

    IF(PRESENT(SIZE)) THEN
       ALLOCATE(DATA_vec(SIZE))
    ELSE
       ALLOCATE(DATA_vec(1))
       DATA_scl =>DATA_vec(1)
    END IF

    IF (PRESENT(DIM)) THEN
       VAR  => NC_MAKE_PVAR(name='time', values=Data_vec, DIM1= DIM)
       if(associated(var%vec_flt))then
         VAR%scl_flt => Var%vec_flt(1)
       else
         VAR%scl_dbl => Var%vec_dbl(1)
       endif
    ELSE
       VAR  => NC_MAKE_PVAR(name='time', values=Data_scl)
    END IF
    
    ATT  => NC_MAKE_ATT(name='long_name',values='time')
    VAR  => ADD(VAR,ATT)
    
    IF (use_mjd) THEN
       ATT  => NC_MAKE_ATT(name='units',values=mjd_units)
       VAR  => ADD(VAR,ATT)
       
       ATT  => NC_MAKE_ATT(name='format',values=fmat)
       VAR  => ADD(VAR,ATT)

       ATT  => NC_MAKE_ATT(name='time_zone',values='UTC') 
       VAR  => ADD(VAR,ATT)
       
    ELSE
       ATT  => NC_MAKE_ATT(name='units',values=days_units)
       VAR  => ADD(VAR,ATT)       
       
       ATT  => NC_MAKE_ATT(name='time_zone',values='none') 
       VAR  => ADD(VAR,ATT)
    END IF
    

  END FUNCTION FLOAT_TIME_OBJECT_NEST
!====================================================================
!====================================================================
 FUNCTION ITIME_OBJECT_NEST(use_mjd,DIM,size) RESULT(VAR)
    IMPLICIT NONE
    TYPE(NCVAR),  POINTER :: VAR
    logical, intent(in) :: use_mjd
    TYPE(NCDIM),  POINTER, OPTIONAL :: DIM
    INTEGER, OPTIONAL :: SIZE
    TYPE(NCATT), POINTER :: ATT
    INTEGER,POINTER :: Data_vec(:)
    INTEGER,POINTER :: Data_scl

    IF(PRESENT(SIZE)) THEN
       ALLOCATE(DATA_vec(SIZE))
    ELSE
       ALLOCATE(DATA_vec(1))
       DATA_scl =>DATA_vec(1)
    END IF
    
        ! Itime
    IF (PRESENT(DIM)) THEN
        VAR  => NC_MAKE_PVAR(name='Itime', values=Data_vec, DIM1= DIM)
        VAR%SCL_INT => VAR%VEC_INT(1)
    ELSE
       VAR  => NC_MAKE_PVAR(name='Itime', values=Data_scl)
    END IF

    IF (use_mjd) THEN
       ATT  => NC_MAKE_ATT(name='units',values=mjd_units)
       VAR  => ADD(VAR,ATT)

       ATT  => NC_MAKE_ATT(name='format',values=fmat)
       VAR  => ADD(VAR,ATT)

       ATT  => NC_MAKE_ATT(name='time_zone',values='UTC') 
       VAR  => ADD(VAR,ATT)
    ELSE
       ATT  => NC_MAKE_ATT(name='units',values=days_units)
       VAR  => ADD(VAR,ATT)
              
       ATT  => NC_MAKE_ATT(name='time_zone',values='none') 
       VAR  => ADD(VAR,ATT)
    END IF

  END FUNCTION ITIME_OBJECT_NEST
!====================================================================

END Module Mod_Nesting
