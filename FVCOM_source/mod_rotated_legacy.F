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
#if defined(ROTATED_DIFFUSION_LEGACY)
! Retain FVCOM's nodal Green-Gauss/dual-face lateral operator. Only its
! diffusion gradient is rotated; advection gradients and WTS are untouched.
MODULE MOD_ROTATED_LEGACY
  USE MOD_PREC, ONLY: SP,DP
  USE ALL_VARS, ONLY: M,MT,KB,KBM1,NTSN,NBSN,DLTXTRIE,DLTYTRIE,ART2, &
       DT,H,ZZ,DZ,ART1,NN_HVC,HORIZONTAL_MIXING_TYPE
  USE MOD_UTILS, ONLY: FATAL_ERROR
#if defined(MULTIPROCESSOR)
  USE ALL_VARS, ONLY: PAR,MPI_FVCOM_GROUP
  USE mpi, ONLY: MPI_ALLREDUCE,MPI_INTEGER,MPI_MAX
#endif
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: PREPARE_LEGACY_ROTATION, ROTATE_LEGACY_GRADIENT
  PUBLIC :: SAVE_LEGACY_COEFFICIENT, ADD_LEGACY_VERTICAL, RESET_LEGACY_ROTATION
  INTEGER, ALLOCATABLE, SAVE :: START(:),NODE1(:),NODE2(:)
#if defined(SPHERICAL)
  REAL(DP), ALLOCATABLE, SAVE :: WX(:),WY(:)
#else
  REAL(SP), ALLOCATABLE, SAVE :: WX(:),WY(:)
#endif
  REAL(SP), ALLOCATABLE, SAVE :: Z(:,:),SX(:,:),SY(:,:),DA(:,:),DOTQ(:,:),KH(:,:)
  INTEGER, SAVE :: SAVED_M=0,SAVED_MT=0,SAVED_KB=0
CONTAINS
  SUBROUTINE CHECK_STATUS(STATUS,MESSAGE)
    INTEGER, INTENT(IN) :: STATUS
    CHARACTER(*), INTENT(IN) :: MESSAGE
    INTEGER :: GLOBAL_STATUS
#if defined(MULTIPROCESSOR)
    INTEGER :: MPI_STATUS
#endif
    GLOBAL_STATUS=STATUS
#if defined(MULTIPROCESSOR)
    IF(PAR) CALL MPI_ALLREDUCE(STATUS,GLOBAL_STATUS,1,MPI_INTEGER,MPI_MAX,MPI_FVCOM_GROUP,MPI_STATUS)
#endif
    IF(GLOBAL_STATUS/=0) CALL FATAL_ERROR(MESSAGE)
  END SUBROUTINE

  SUBROUTINE PREPARE_LEGACY_ROTATION(OLD,REFERENCE)
    REAL(SP), INTENT(IN) :: OLD(0:MT,KB),REFERENCE(0:MT,KB)
    INTEGER :: I,J,P,K,LO,HI,STATUS
    REAL(SP) :: VALUE,SPAN
    STATUS=0
    IF(M<1.OR.MT<M.OR.KBM1<2) STATUS=1
    IF(ALLOCATED(START)) THEN
      IF(M/=SAVED_M.OR.MT/=SAVED_MT.OR.KB/=SAVED_KB) STATUS=1
    END IF
    CALL CHECK_STATUS(STATUS,'ROTATED_DIFFUSION_LEGACY: invalid dimensions or mesh replaced without reset')
    IF(.NOT.ALLOCATED(START)) THEN
      IF(ANY(NTSN(1:M)<2).OR.ANY(NTSN(1:M)>SIZE(NBSN,2)).OR. &
          ANY(NTSN(1:M)-1>SIZE(DLTXTRIE,2)).OR.ANY(NTSN(1:M)-1>SIZE(DLTYTRIE,2)).OR. &
          .NOT.ALL(ART2(1:M)>0.0_SP).OR. &
          .NOT.ALL(ART2(1:M)<HUGE(1.0_SP))) STATUS=1
      CALL CHECK_STATUS(STATUS,'ROTATED_DIFFUSION_LEGACY: invalid fixed nodal patch geometry')
      ALLOCATE(START(M+1))
      START(1)=1
      DO I=1,M
        START(I+1)=START(I)+NTSN(I)-1
      END DO
      ALLOCATE(NODE1(START(M+1)-1),NODE2(START(M+1)-1),WX(START(M+1)-1),WY(START(M+1)-1))
      DO I=1,M
        DO J=1,NTSN(I)-1
          P=START(I)+J-1
          NODE1(P)=NBSN(I,J); NODE2(P)=NBSN(I,J+1)
          WX(P)=DLTYTRIE(I,J)
          WY(P)=DLTXTRIE(I,J)
        END DO
      END DO
      IF(ANY(NODE1<1).OR.ANY(NODE1>MT).OR.ANY(NODE2<1).OR.ANY(NODE2>MT).OR. &
          .NOT.ALL(ABS(WX)<HUGE(1.0_SP)).OR..NOT.ALL(ABS(WY)<HUGE(1.0_SP))) STATUS=1
      CALL CHECK_STATUS(STATUS,'ROTATED_DIFFUSION_LEGACY: invalid fixed patch coefficients or connectivity')
      ALLOCATE(Z(MT,KBM1),SX(M,KBM1),SY(M,KBM1),DA(M,KBM1),DOTQ(M,KBM1),KH(M,KBM1))
      SAVED_M=M; SAVED_MT=MT; SAVED_KB=KB
    END IF
    DO K=1,KBM1
      Z(:,K)=DT(1:MT)-H(1:MT)+DT(1:MT)*ZZ(1:MT,K)
    END DO
    IF(.NOT.ALL(ABS(Z)<HUGE(1.0_SP)).OR..NOT.ALL(Z(:,1:KBM1-1)>Z(:,2:KBM1)).OR. &
       .NOT.ALL(ABS(OLD(1:MT,1:KBM1))<HUGE(1.0_SP)).OR. &
       .NOT.ALL(ABS(REFERENCE(1:MT,1:KBM1))<HUGE(1.0_SP)).OR. &
       .NOT.ALL(ART1(1:M)>0.0_SP).OR..NOT.ALL(DT(1:M)>0.0_SP).OR. &
       .NOT.ALL(DZ(1:M,1:KBM1)>0.0_SP)) STATUS=1
    CALL CHECK_STATUS(STATUS,'ROTATED_DIFFUSION_LEGACY: invalid tracer, volume or sigma depths')
    SX=0.0_SP; SY=0.0_SP; DOTQ=0.0_SP; KH=0.0_SP
    DO K=1,KBM1
      LO=MAX(1,K-1); HI=MIN(KBM1,K+1)
      DO I=1,M
        ! Subtract the centre before averaging: exactly zero for flat layers.
        DO P=START(I),START(I+1)-1
          VALUE=0.5_SP*((Z(NODE1(P),K)-Z(I,K))+(Z(NODE2(P),K)-Z(I,K)))
          SX(I,K)=SX(I,K)+WX(P)*VALUE
          SY(I,K)=SY(I,K)+WY(P)*VALUE
        END DO
        SX(I,K)=SX(I,K)/ART2(I)
        SY(I,K)=SY(I,K)/ART2(I)
        SPAN=Z(I,LO)-Z(I,HI)
        DA(I,K)=((OLD(I,LO)-REFERENCE(I,LO))-(OLD(I,HI)-REFERENCE(I,HI)))/SPAN
      END DO
    END DO
    IF(.NOT.ALL(ABS(SX)<HUGE(1.0_SP)).OR..NOT.ALL(ABS(SY)<HUGE(1.0_SP)).OR. &
       .NOT.ALL(ABS(DA)<HUGE(1.0_SP))) STATUS=1
    CALL CHECK_STATUS(STATUS,'ROTATED_DIFFUSION_LEGACY: nonfinite rotated gradient')
  END SUBROUTINE

  SUBROUTINE ROTATE_LEGACY_GRADIENT(K,GX,GY)
    INTEGER, INTENT(IN) :: K
    REAL(SP), INTENT(INOUT) :: GX(0:MT),GY(0:MT)
    ! Preserve even the floating-point bit patterns of the flat-layer path.
    IF(ALL(SX(:,K)==0.0_SP).AND.ALL(SY(:,K)==0.0_SP)) RETURN
    GX(1:M)=GX(1:M)-SX(:,K)*DA(:,K)
    GY(1:M)=GY(1:M)-SY(:,K)*DA(:,K)
    DOTQ(:,K)=SX(:,K)*GX(1:M)+SY(:,K)*GY(1:M)
  END SUBROUTINE

  SUBROUTINE SAVE_LEGACY_COEFFICIENT(K,VISCOFF)
    INTEGER, INTENT(IN) :: K
    REAL(SP), INTENT(IN) :: VISCOFF(0:MT)
    ! Exactly the nodal coefficient used by the unmodified lateral formula.
    SELECT CASE(HORIZONTAL_MIXING_TYPE)
    CASE('constant')
      KH(:,K)=NN_HVC(1:M)
    CASE('closure')
      KH(:,K)=VISCOFF(1:M)*NN_HVC(1:M)
    CASE DEFAULT
      KH(:,K)=-1.0_SP
    END SELECT
  END SUBROUTINE

  SUBROUTINE ADD_LEGACY_VERTICAL(XFLUX)
    REAL(SP), INTENT(INOUT) :: XFLUX(0:MT,KB)
    INTEGER :: I,K,LO,HI,STATUS
    REAL(SP) :: TRANSPORT,VOLUME
    STATUS=0
    IF(.NOT.ALL(KH>=0.0_SP).OR..NOT.ALL(KH<HUGE(1.0_SP)).OR. &
       .NOT.ALL(ABS(DOTQ)<HUGE(1.0_SP))) STATUS=1
    CALL CHECK_STATUS(STATUS,'ROTATED_DIFFUSION_LEGACY: invalid diffusivity or flux')
    ! Called AFTER horizontal NODE_MATCH. Every local column gets the same
    ! completed correction; summing it across shared ranks would duplicate it.
    DO K=1,KBM1
      LO=MAX(1,K-1); HI=MIN(KBM1,K+1)
      DO I=1,M
        IF(DOTQ(I,K)==0.0_SP) CYCLE
        VOLUME=ART1(I)*DT(I)*DZ(I,K)
        TRANSPORT=-VOLUME*KH(I,K)*DOTQ(I,K)/(Z(I,LO)-Z(I,HI))
        XFLUX(I,LO)=XFLUX(I,LO)+TRANSPORT
        XFLUX(I,HI)=XFLUX(I,HI)-TRANSPORT
      END DO
    END DO
    IF(.NOT.ALL(ABS(XFLUX(1:M,1:KBM1))<HUGE(1.0_SP))) STATUS=1
    CALL CHECK_STATUS(STATUS,'ROTATED_DIFFUSION_LEGACY: nonfinite completed tracer flux')
  END SUBROUTINE

  SUBROUTINE RESET_LEGACY_ROTATION()
    IF(ALLOCATED(START)) THEN
      DEALLOCATE(START,NODE1,NODE2,WX,WY,Z,SX,SY,DA,DOTQ,KH)
    END IF
    SAVED_M=0; SAVED_MT=0; SAVED_KB=0
  END SUBROUTINE
END MODULE MOD_ROTATED_LEGACY
#endif
