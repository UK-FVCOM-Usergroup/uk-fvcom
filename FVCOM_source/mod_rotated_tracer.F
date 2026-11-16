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
! This file is part of FVCOM. For details, see http://fvcom.smast.umassd.edu
! The full copyright notice is contained in the file COPYRIGHT located in the
! root directory of the FVCOM code. This original header must be maintained
! in all distributed versions.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
! THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
! PURPOSE ARE DISCLAIMED.
!
!/===========================================================================/

#include "rotated_diffusion_config.h"
# if defined (ROTATED_DIFFUSION_FLUX)
MODULE MOD_ROTATED_TRACER
  USE MOD_PREC, ONLY: SP
  USE ALL_VARS, ONLY: M, MT, N, KB, KBM1, NV, VX, VY, DT, DTFA, H, ZZ, &
       DZ, DT1, DZ1, ART1, DTI, VISCOFH, NN_HVC, HORIZONTAL_MIXING_TYPE
  USE MOD_UTILS, ONLY: FATAL_ERROR
  USE MOD_ROTATED_DIFFUSION, ONLY: ROTATED_DIFFUSION_RESIDUAL
# if defined (MULTIPROCESSOR)
  USE ALL_VARS, ONLY: PAR, MYID, NPROCS, MPI_FVCOM_GROUP
  USE MOD_PREC, ONLY: MPI_F
  USE MPI, ONLY: MPI_ALLREDUCE, MPI_INTEGER, MPI_MAX
  USE MOD_PAR, ONLY: NODE_MATCH, AEXCHANGE, NC, BNC, NBN, BN_MLT, BN_LOC
# endif
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: APPLY_ROTATED_TRACER_DIFFUSION

CONTAINS

  ! Add the conservative diffusion mass increment to an already advected
  ! result. OLD and REFERENCE are the same time-level fields that the legacy
  ! horizontal operator used. Diffusion remains separate from MPDATA's local
  ! advection limiter and from WTS, including its sigma-interface transport.
  SUBROUTINE APPLY_ROTATED_TRACER_DIFFUSION(OLD, REFERENCE, RESULT)
    REAL(SP), INTENT(IN) :: OLD(0:MT,KB), REFERENCE(0:MT,KB)
    REAL(SP), INTENT(INOUT) :: RESULT(0:MT,KB)
    INTEGER, ALLOCATABLE :: TRIANGLES(:,:)
    REAL(SP), ALLOCATABLE :: CENTRE_Z(:,:), THICKNESS(:,:), OLD_VOLUME(:,:), NEW_VOLUME(:,:)
    REAL(SP), ALLOCATABLE :: MASS_DELTA(:,:)
    REAL(SP), ALLOCATABLE, TARGET :: ANOMALY(:,:), KH(:,:)
    REAL(SP), ALLOCATABLE :: RESIDUAL(:,:), BOUND(:,:)
    REAL(SP) :: RATE_LOCAL, RATE_GLOBAL, SUBSTEP_DT, MASS_STEP
    REAL(SP), PARAMETER :: EULER_SAFETY = 0.9_SP
    INTEGER, PARAMETER :: MAX_SUBSTEPS = 1000000
    INTEGER :: I, K, STEP, NSTEPS, STATUS_LOCAL, STATUS_KERNEL
# if defined (MULTIPROCESSOR)
    INTEGER :: MPI_STATUS
# endif

    STATUS_LOCAL = 0
    IF (M < 1 .OR. MT < M .OR. N < 0 .OR. KBM1 < 2) STATUS_LOCAL = 1
    IF (.NOT.(DTI >= 0.0_SP .AND. DTI < HUGE(1.0_SP))) STATUS_LOCAL = 2
    CALL CHECK_ROTATED_STATUS(STATUS_LOCAL, &
         'ROTATED_DIFFUSION_FLUX requires positive mesh dimensions, at least two layers and a finite nonnegative DTI')
    IF (DTI == 0.0_SP) RETURN

    ALLOCATE(TRIANGLES(3,N), CENTRE_Z(MT,KBM1), THICKNESS(N,KBM1))
    ALLOCATE(OLD_VOLUME(M,KBM1), NEW_VOLUME(M,KBM1), MASS_DELTA(M,KBM1))
    ! The zero row and unused bottom level retain FVCOM's communication layout.
    ALLOCATE(ANOMALY(0:MT,KB), KH(0:MT,KB))
    ALLOCATE(RESIDUAL(0:MT,KB), BOUND(0:MT,KB))
    TRIANGLES = TRANSPOSE(NV(1:N,1:3))
    ANOMALY = OLD-REFERENCE
    KH = 0.0_SP
    RESIDUAL = 0.0_SP
    BOUND = 0.0_SP
    MASS_DELTA = 0.0_SP

    SELECT CASE (HORIZONTAL_MIXING_TYPE)
    CASE ('closure')
      DO K = 1, KBM1
        KH(1:MT,K) = VISCOFH(1:MT,K)*NN_HVC(1:MT)
      END DO
    CASE ('constant')
      DO K = 1, KBM1
        KH(1:MT,K) = NN_HVC(1:MT)
      END DO
    CASE DEFAULT
      STATUS_LOCAL = 3
    END SELECT
    CALL CHECK_ROTATED_STATUS(STATUS_LOCAL, &
         'ROTATED_DIFFUSION_FLUX: unsupported horizontal mixing type')

# if defined (MULTIPROCESSOR)
    IF (PAR) CALL AEXCHANGE(NC,MYID,NPROCS,ANOMALY,KH)
# endif

    DO K = 1, KBM1
      ! DT-H is the free surface belonging to OLD, whereas EL may have
      ! advanced during the external-mode steps. ZZ is negative downward.
      CENTRE_Z(:,K) = DT(1:MT)-H(1:MT)+DT(1:MT)*ZZ(1:MT,K)
      THICKNESS(:,K) = DT1(1:N)*DZ1(1:N,K)
      OLD_VOLUME(:,K) = ART1(1:M)*DT(1:M)*DZ(1:M,K)
      NEW_VOLUME(:,K) = ART1(1:M)*DTFA(1:M)*DZ(1:M,K)
    END DO
    STATUS_LOCAL = 0
    IF (.NOT.ALL(OLD_VOLUME > 0.0_SP) .OR. &
        .NOT.ALL(OLD_VOLUME < HUGE(1.0_SP))) STATUS_LOCAL = 4
    IF (.NOT.ALL(NEW_VOLUME > 0.0_SP) .OR. &
        .NOT.ALL(NEW_VOLUME < HUGE(1.0_SP))) STATUS_LOCAL = 4
    IF (.NOT.ALL(ART1(1:M) > 0.0_SP) .OR. &
        .NOT.ALL(DT(1:M) > 0.0_SP) .OR. &
        .NOT.ALL(DTFA(1:M) > 0.0_SP) .OR. &
        .NOT.ALL(DZ(1:M,1:KBM1) > 0.0_SP)) STATUS_LOCAL = 4
    IF (.NOT.ALL(ABS(ANOMALY(1:MT,1:KBM1)) < HUGE(1.0_SP))) STATUS_LOCAL = 5
    CALL CHECK_ROTATED_STATUS(STATUS_LOCAL, &
         'ROTATED_DIFFUSION_FLUX: invalid tracer anomaly or nodal control volume')

    CALL ASSEMBLE_RESIDUAL(.TRUE.)
    RATE_LOCAL = MAXVAL(BOUND(1:M,1:KBM1)/OLD_VOLUME)
    RATE_GLOBAL = RATE_LOCAL
# if defined (MULTIPROCESSOR)
    IF (PAR) CALL MPI_ALLREDUCE(RATE_LOCAL,RATE_GLOBAL,1,MPI_F,MPI_MAX,MPI_FVCOM_GROUP,MPI_STATUS)
# endif
    STATUS_LOCAL = 0
    IF (.NOT.(RATE_GLOBAL >= 0.0_SP .AND. RATE_GLOBAL < HUGE(1.0_SP))) STATUS_LOCAL = 6
    CALL CHECK_ROTATED_STATUS(STATUS_LOCAL, &
         'ROTATED_DIFFUSION_FLUX: nonfinite explicit diffusion stability bound')
    IF (RATE_GLOBAL == 0.0_SP) RETURN
    ! Check before multiplying DTI*RATE_GLOBAL or converting it to an integer.
    STATUS_LOCAL = 0
    IF (RATE_GLOBAL > 1.0_SP) THEN
      IF (DTI > REAL(MAX_SUBSTEPS,SP)*EULER_SAFETY/RATE_GLOBAL) STATUS_LOCAL = 9
    ELSE IF (DTI > REAL(MAX_SUBSTEPS,SP)*EULER_SAFETY) THEN
      IF (RATE_GLOBAL > REAL(MAX_SUBSTEPS,SP)*EULER_SAFETY/DTI) STATUS_LOCAL = 9
    END IF
    CALL CHECK_ROTATED_STATUS(STATUS_LOCAL, &
         'ROTATED_DIFFUSION_FLUX requires more than 1000000 diffusion substeps; reduce DTI or inspect the mesh and K')
    NSTEPS = MAX(1,CEILING(DTI*RATE_GLOBAL/EULER_SAFETY))
    IF (NSTEPS > MAX_SUBSTEPS) THEN
      CALL FATAL_ERROR('ROTATED_DIFFUSION_FLUX requires more than 1000000 diffusion substeps', &
           'Reduce DTI or inspect layer thicknesses, slopes and horizontal diffusivity')
      RETURN
    END IF
    SUBSTEP_DT = DTI/REAL(NSTEPS,SP)

    ! Geometry and K are frozen during the split diffusion solve. With the
    ! row-absolute-sum bound above, Euler subcycling is variance dissipative
    ! for the anomaly in OLD_VOLUME. This is not a positivity limiter.
    DO STEP = 1, NSTEPS
      IF (STEP > 1) CALL ASSEMBLE_RESIDUAL(.FALSE.)
      DO K = 1, KBM1
        DO I = 1, M
          MASS_STEP = -SUBSTEP_DT*RESIDUAL(I,K)
          MASS_DELTA(I,K) = MASS_DELTA(I,K)+MASS_STEP
          ANOMALY(I,K) = ANOMALY(I,K)+MASS_STEP/OLD_VOLUME(I,K)
        END DO
      END DO
      STATUS_LOCAL = 0
      IF (.NOT.ALL(ABS(ANOMALY(1:M,1:KBM1)) < HUGE(1.0_SP)) .OR. &
          .NOT.ALL(ABS(MASS_DELTA) < HUGE(1.0_SP))) STATUS_LOCAL = 7
      CALL CHECK_ROTATED_STATUS(STATUS_LOCAL, &
           'ROTATED_DIFFUSION_FLUX: nonfinite tracer during explicit diffusion')
# if defined (MULTIPROCESSOR)
      IF (PAR .AND. STEP < NSTEPS) CALL AEXCHANGE(NC,MYID,NPROCS,ANOMALY)
# endif
    END DO

    DO K = 1, KBM1
      RESULT(1:M,K) = RESULT(1:M,K)+MASS_DELTA(:,K)/NEW_VOLUME(:,K)
    END DO

  CONTAINS

    SUBROUTINE ASSEMBLE_RESIDUAL(ASSEMBLE_BOUND)
      LOGICAL, INTENT(IN) :: ASSEMBLE_BOUND
      RESIDUAL = 0.0_SP
      BOUND = 0.0_SP
      ! NV(1:N,:) contains owned triangles only. Halo triangles would
      ! duplicate transport; NODE_MATCH sums the shared-node contributions.
      CALL ROTATED_DIFFUSION_RESIDUAL(MT,N,KBM1,TRIANGLES,VX(1:MT),VY(1:MT), &
           CENTRE_Z,THICKNESS,KH(1:MT,1:KBM1),ANOMALY(1:MT,1:KBM1), &
           RESIDUAL(1:MT,1:KBM1),BOUND(1:MT,1:KBM1),STATUS_KERNEL)
      CALL CHECK_ROTATED_STATUS(STATUS_KERNEL, &
           'ROTATED_DIFFUSION_FLUX: invalid triangle geometry, diffusivity or sigma layer depths')
# if defined (MULTIPROCESSOR)
      IF (PAR) THEN
        IF (ASSEMBLE_BOUND) THEN
          CALL NODE_MATCH(0,NBN,BN_MLT,BN_LOC,BNC,MT,KB,MYID,NPROCS,RESIDUAL,BOUND)
        ELSE
          CALL NODE_MATCH(0,NBN,BN_MLT,BN_LOC,BNC,MT,KB,MYID,NPROCS,RESIDUAL)
        END IF
      END IF
# endif
      STATUS_LOCAL = 0
      IF (.NOT.ALL(ABS(RESIDUAL(1:M,1:KBM1)) < HUGE(1.0_SP))) STATUS_LOCAL = 8
      IF (ASSEMBLE_BOUND) THEN
        IF (.NOT.ALL(BOUND(1:M,1:KBM1) >= 0.0_SP) .OR. &
            .NOT.ALL(BOUND(1:M,1:KBM1) < HUGE(1.0_SP))) STATUS_LOCAL = 8
      END IF
      CALL CHECK_ROTATED_STATUS(STATUS_LOCAL, &
           'ROTATED_DIFFUSION_FLUX: nonfinite assembled diffusion residual or bound')
    END SUBROUTINE ASSEMBLE_RESIDUAL
  END SUBROUTINE APPLY_ROTATED_TRACER_DIFFUSION

  ! Every rank checks an assembly error before entering its next halo exchange.
  ! A geometry error on one rank must not leave its neighbors blocked in MPI.
  SUBROUTINE CHECK_ROTATED_STATUS(LOCAL_STATUS, MESSAGE)
    INTEGER, INTENT(IN) :: LOCAL_STATUS
    CHARACTER(LEN=*), INTENT(IN) :: MESSAGE
    INTEGER :: GLOBAL_STATUS
# if defined (MULTIPROCESSOR)
    INTEGER :: MPI_STATUS
# endif
    GLOBAL_STATUS = LOCAL_STATUS
# if defined (MULTIPROCESSOR)
    IF (PAR) CALL MPI_ALLREDUCE(LOCAL_STATUS,GLOBAL_STATUS,1,MPI_INTEGER,MPI_MAX,MPI_FVCOM_GROUP,MPI_STATUS)
# endif
    IF (GLOBAL_STATUS /= 0) CALL FATAL_ERROR(MESSAGE)
  END SUBROUTINE CHECK_ROTATED_STATUS
END MODULE MOD_ROTATED_TRACER
# endif
