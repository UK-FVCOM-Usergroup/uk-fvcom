!==============================================================================!
! MOD_NID_RELAX: domain-wide relaxation of T, S and FABM interior states to    !
! horizontally uniform, time-varying target profiles (NiD4Ocean, D20).         !
!                                                                              !
! Mirrors the 1D GOTM setup that provides the open-boundary values: GOTM       !
! relaxes T/S (and some nutrients, lower half of the column only) towards      !
! the DFM profiles. Without the same term the FVCOM interior drifts away from  !
! its own boundary and the resulting T front along the OBC goes unstable.      !
!                                                                              !
! Namelist (in <casename>_run.nml; absent = off):                              !
!   &NML_NID_RELAX  NID_RELAX_ON = T, NID_RELAX_FILE = 'nid_relax.nc' /        !
!                                                                              !
! File (INPUT_DIR/NID_RELAX_FILE), netCDF:                                     !
!   dimensions  time, siglay (= KBM1, surface first)                           !
!   time(time)            days since 1858-11-17 00:00:00 (MJD)                 !
!   <name>(time, siglay)  target value; <name> = 'temp', 'salinity' or the     !
!                         FABM interior state name (e.g. N3_n)                 !
!   <name>_tau(siglay)    relaxation time scale (s); >= 1e14 = no relaxation   !
!   global attribute relax_variables = space-separated list of <name>          !
!                                                                              !
! Applied once per internal step, implicitly, on every node (halos included:  !
! the target is horizontally uniform):                                         !
!   F = (F + a*F_obs) / (1 + a),  a = DTI / tau                                !
! F_obs is linearly interpolated in time (held constant outside the record).   !
!==============================================================================!
MODULE MOD_NID_RELAX
  USE MOD_PREC
  USE ALL_VARS
  USE MOD_UTILS
  USE NETCDF
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: NID_RELAX_ON, NAME_LIST_READ_NID_RELAX, NID_RELAX_INIT, NID_RELAX_INDEX, NID_RELAX_APPLY

  LOGICAL :: NID_RELAX_ON = .FALSE.
  CHARACTER(LEN=200) :: NID_RELAX_FILE = 'none'
  NAMELIST /NML_NID_RELAX/ NID_RELAX_ON, NID_RELAX_FILE

  INTEGER :: NREC = 0, NVAR = 0, NLAY = 0
  REAL(DP), ALLOCATABLE :: RTIME(:)          ! (NREC) MJD
  REAL(DP), ALLOCATABLE :: ROBS(:,:,:)       ! (NLAY, NREC, NVAR)
  REAL(DP), ALLOCATABLE :: RTAU(:,:)         ! (NLAY, NVAR) s
  CHARACTER(LEN=64), ALLOCATABLE :: RNAME(:) ! (NVAR)

CONTAINS

  SUBROUTINE NAME_LIST_READ_NID_RELAX
    INTEGER :: IOS
    CHARACTER(LEN=120) :: FNAME
    FNAME = "./"//trim(casename)//"_run.nml"
    CALL FOPEN(NMLUNIT, trim(FNAME), 'cfr')
    READ(UNIT=NMLUNIT, NML=NML_NID_RELAX, IOSTAT=IOS)
    IF (IOS > 0) CALL FATAL_ERROR('Cannot read namelist NML_NID_RELAX from file: '//trim(FNAME))
    IF (IOS < 0) NID_RELAX_ON = .FALSE.     ! namelist absent: off
    CLOSE(NMLUNIT)
    IF (MSR) WRITE(IPT,*) '! NID_RELAX_ON = ', NID_RELAX_ON, '  FILE = ', trim(NID_RELAX_FILE)
  END SUBROUTINE NAME_LIST_READ_NID_RELAX


  SUBROUTINE CHK(STATUS, WHAT)
    INTEGER, INTENT(IN) :: STATUS
    CHARACTER(LEN=*), INTENT(IN) :: WHAT
    IF (STATUS /= NF90_NOERR) CALL FATAL_ERROR('NID_RELAX: '//trim(WHAT), trim(NF90_STRERROR(STATUS)))
  END SUBROUTINE CHK


  SUBROUTINE NID_RELAX_INIT
    ! Every rank reads the (small) file itself.
    INTEGER :: NCID, DID, VID, IV, I0, I1, LL
    CHARACTER(LEN=2000) :: LIST
    CHARACTER(LEN=256) :: PATH
    REAL(DP), ALLOCATABLE :: TMP(:,:)

    IF (.NOT. NID_RELAX_ON) RETURN
    PATH = trim(INPUT_DIR)//trim(NID_RELAX_FILE)
    CALL CHK(NF90_OPEN(trim(PATH), NF90_NOWRITE, NCID), 'open '//trim(PATH))
    CALL CHK(NF90_INQ_DIMID(NCID, 'time', DID), 'dimension time')
    CALL CHK(NF90_INQUIRE_DIMENSION(NCID, DID, LEN=NREC), 'dimension time')
    CALL CHK(NF90_INQ_DIMID(NCID, 'siglay', DID), 'dimension siglay')
    CALL CHK(NF90_INQUIRE_DIMENSION(NCID, DID, LEN=NLAY), 'dimension siglay')
    IF (NLAY /= KBM1) CALL FATAL_ERROR('NID_RELAX: siglay in the file does not match KBM1')
    IF (NREC < 1) CALL FATAL_ERROR('NID_RELAX: no time records')

    LIST = ''
    CALL CHK(NF90_GET_ATT(NCID, NF90_GLOBAL, 'relax_variables', LIST), 'attribute relax_variables')
    ! count and split the names
    NVAR = 0
    LL = len_trim(LIST)
    I0 = 1
    DO WHILE (I0 <= LL)
       DO WHILE (I0 <= LL .AND. LIST(I0:I0) == ' '); I0 = I0 + 1; END DO
       IF (I0 > LL) EXIT
       I1 = I0
       DO WHILE (I1 < LL .AND. LIST(I1+1:I1+1) /= ' '); I1 = I1 + 1; END DO
       NVAR = NVAR + 1
       I0 = I1 + 1
    END DO
    IF (NVAR == 0) CALL FATAL_ERROR('NID_RELAX: relax_variables is empty')
    ALLOCATE(RNAME(NVAR), RTIME(NREC), ROBS(NLAY, NREC, NVAR), RTAU(NLAY, NVAR), TMP(NLAY, NREC))
    IV = 0
    I0 = 1
    DO WHILE (I0 <= LL)
       DO WHILE (I0 <= LL .AND. LIST(I0:I0) == ' '); I0 = I0 + 1; END DO
       IF (I0 > LL) EXIT
       I1 = I0
       DO WHILE (I1 < LL .AND. LIST(I1+1:I1+1) /= ' '); I1 = I1 + 1; END DO
       IV = IV + 1
       RNAME(IV) = LIST(I0:I1)
       I0 = I1 + 1
    END DO

    CALL CHK(NF90_INQ_VARID(NCID, 'time', VID), 'variable time')
    CALL CHK(NF90_GET_VAR(NCID, VID, RTIME), 'read time')
    DO IV = 1, NVAR
       CALL CHK(NF90_INQ_VARID(NCID, trim(RNAME(IV)), VID), 'variable '//trim(RNAME(IV)))
       CALL CHK(NF90_GET_VAR(NCID, VID, TMP), 'read '//trim(RNAME(IV)))
       ROBS(:, :, IV) = TMP
       CALL CHK(NF90_INQ_VARID(NCID, trim(RNAME(IV))//'_tau', VID), 'variable '//trim(RNAME(IV))//'_tau')
       CALL CHK(NF90_GET_VAR(NCID, VID, RTAU(:, IV)), 'read '//trim(RNAME(IV))//'_tau')
       IF (ANY(RTAU(:, IV) <= 0.0_DP)) CALL FATAL_ERROR('NID_RELAX: tau <= 0 for '//trim(RNAME(IV)))
    END DO
    CALL CHK(NF90_CLOSE(NCID), 'close')
    DEALLOCATE(TMP)
    DO IV = 2, NREC
       IF (RTIME(IV) <= RTIME(IV-1)) CALL FATAL_ERROR('NID_RELAX: time not increasing')
    END DO

    IF (MSR) THEN
       WRITE(IPT,*) '! NID_RELAX: ', NREC, ' records, MJD ', RTIME(1), ' .. ', RTIME(NREC)
       DO IV = 1, NVAR
          WRITE(IPT,'(A,A20,A,2ES11.3,A,ES11.3)') ' ! NID_RELAX: ', trim(RNAME(IV)), ' tau min/max (s)', &
               & minval(RTAU(:, IV)), maxval(RTAU(:, IV)), '  first-record mean', sum(ROBS(:, 1, IV)) / NLAY
       END DO
    END IF
  END SUBROUTINE NID_RELAX_INIT


  INTEGER FUNCTION NID_RELAX_INDEX(NAME)
    ! index of NAME in the relaxed variables, 0 if absent
    CHARACTER(LEN=*), INTENT(IN) :: NAME
    INTEGER :: IV
    NID_RELAX_INDEX = 0
    IF (.NOT. NID_RELAX_ON) RETURN
    DO IV = 1, NVAR
       IF (trim(RNAME(IV)) == trim(NAME)) THEN
          NID_RELAX_INDEX = IV
          RETURN
       END IF
    END DO
  END FUNCTION NID_RELAX_INDEX


  SUBROUTINE NID_RELAX_APPLY(IV, F)
    INTEGER, INTENT(IN) :: IV
    REAL(SP), INTENT(INOUT) :: F(0:, :)
    REAL(DP) :: T, W, A, OBS
    INTEGER :: J, K

    IF (IV <= 0) RETURN
    T = DAYS(IntTime)
    IF (T <= RTIME(1)) THEN
       J = 1; W = 0.0_DP
    ELSE IF (T >= RTIME(NREC)) THEN
       J = max(NREC - 1, 1); W = 1.0_DP
       IF (NREC == 1) W = 0.0_DP
    ELSE
       J = 1
       DO WHILE (RTIME(J+1) < T); J = J + 1; END DO
       W = (T - RTIME(J)) / (RTIME(J+1) - RTIME(J))
    END IF
    DO K = 1, NLAY
       IF (RTAU(K, IV) >= 1.0E14_DP) CYCLE
       A = DTI / RTAU(K, IV)
       IF (NREC == 1) THEN
          OBS = ROBS(K, 1, IV)
       ELSE
          OBS = (1.0_DP - W) * ROBS(K, J, IV) + W * ROBS(K, J+1, IV)
       END IF
       F(1:MT, K) = (F(1:MT, K) + A * OBS) / (1.0_DP + A)
    END DO
  END SUBROUTINE NID_RELAX_APPLY

END MODULE MOD_NID_RELAX
