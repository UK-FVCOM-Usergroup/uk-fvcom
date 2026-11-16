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

! Geopotential horizontal diffusion on nodal, layered triangular meshes.
!
! The outgoing mass residual is B^T W K B C, with
! B C = G C - (G z) * mean(D_z C).
! G is the piecewise-linear triangle gradient, and D_z is a physical-depth
! secant at each node. Applying the transpose of the SAME corrected gradient
! produces both lateral transport and compensating sigma-interface transport.
! Consequently sum(R)=0, C^T R >= 0 and B(a+b*z)=0, to roundoff, for K>=0.
!
! This module deliberately has no dependency on model state or MPI. Assemble
! owned triangles only; the caller must sum shared-node residuals and bounds.
MODULE MOD_ROTATED_DIFFUSION
  USE MOD_PREC, ONLY: SP
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: ROTATED_DIFFUSION_RESIDUAL

CONTAINS

  SUBROUTINE ROTATED_DIFFUSION_RESIDUAL(NNODE, NELEM, NLAY, TRIANGLES, X, Y, &
       Z, THICKNESS, KH, ANOMALY, RESIDUAL, BOUND, IERR, LATERAL, INTERFACE_FLUX)
    INTEGER, INTENT(IN) :: NNODE, NELEM, NLAY
    INTEGER, INTENT(IN) :: TRIANGLES(3,NELEM)
    REAL(SP), INTENT(IN) :: X(NNODE), Y(NNODE), Z(NNODE,NLAY)
    REAL(SP), INTENT(IN) :: THICKNESS(NELEM,NLAY)
    REAL(SP), INTENT(IN) :: KH(NNODE,NLAY), ANOMALY(NNODE,NLAY)
    REAL(SP), INTENT(OUT) :: RESIDUAL(NNODE,NLAY), BOUND(NNODE,NLAY)
    INTEGER, INTENT(OUT) :: IERR
    REAL(SP), OPTIONAL, INTENT(OUT) :: LATERAL(NNODE,NLAY)
    REAL(SP), OPTIONAL, INTENT(OUT) :: INTERFACE_FLUX(NNODE,NLAY+1)

    INTEGER :: E, K, A, I, LO, HI, J
    INTEGER :: NODES(3)
    REAL(SP) :: GX(3), GY(3), BX(3,3), BY(3,3), SPAN(3)
    REAL(SP) :: AREA, SX, SY, DZC, QX, QY, WEIGHT, VFLUX, LX, LY
    REAL(SP) :: SUMBX, SUMBY, DET, DX21, DY21, DX31, DY31, SCALE

    ! R has units tracer*volume/time. No volume division or timestep occurs
    ! here. BOUND is an upper bound on sum_j abs((B^T W K B)_ij), assembled
    ! without cancellation. It supports an explicit-Euler energy bound after
    ! the caller sums partitions and divides by nodal control-volume volumes.
    RESIDUAL = 0.0_SP
    BOUND = 0.0_SP
    IF (PRESENT(LATERAL)) LATERAL = 0.0_SP
    IF (PRESENT(INTERFACE_FLUX)) INTERFACE_FLUX = 0.0_SP
    IERR = 0

    IF (NNODE < 1 .OR. NELEM < 0 .OR. NLAY < 2) THEN
      IERR = 1
      RETURN
    END IF
    IF (ANY(TRIANGLES < 1) .OR. ANY(TRIANGLES > NNODE)) THEN
      IERR = 2
      RETURN
    END IF
    IF (.NOT.ALL(ABS(X) < HUGE(1.0_SP)) .OR. &
        .NOT.ALL(ABS(Y) < HUGE(1.0_SP))) THEN
      IERR = 3
      RETURN
    END IF
    IF (.NOT.ALL(THICKNESS > 0.0_SP) .OR. &
        .NOT.ALL(THICKNESS < HUGE(1.0_SP)) .OR. &
        .NOT.ALL(KH >= 0.0_SP) .OR. .NOT.ALL(KH < HUGE(1.0_SP))) THEN
      IERR = 4
      RETURN
    END IF
    IF (.NOT.ALL(ABS(Z) < HUGE(1.0_SP)) .OR. &
        .NOT.ALL(Z(:,1:NLAY-1) > Z(:,2:NLAY))) THEN
      IERR = 5
      RETURN
    END IF

    ! Validate geometry before assembly, leaving outputs zero on any error.
    DO E = 1, NELEM
      NODES = TRIANGLES(:,E)
      DX21 = X(NODES(2))-X(NODES(1))
      DY21 = Y(NODES(2))-Y(NODES(1))
      DX31 = X(NODES(3))-X(NODES(1))
      DY31 = Y(NODES(3))-Y(NODES(1))
      DET = DX21*DY31-DX31*DY21
      SCALE = MAX(DX21*DX21+DY21*DY21, DX31*DX31+DY31*DY31)
      IF (.NOT.(ABS(DET) > EPSILON(1.0_SP)*SCALE)) THEN
        IERR = 3
        RETURN
      END IF
    END DO

    DO E = 1, NELEM
      NODES = TRIANGLES(:,E)
      DX21 = X(NODES(2))-X(NODES(1))
      DY21 = Y(NODES(2))-Y(NODES(1))
      DX31 = X(NODES(3))-X(NODES(1))
      DY31 = Y(NODES(3))-Y(NODES(1))
      DET = DX21*DY31-DX31*DY21
      AREA = 0.5_SP*ABS(DET)
      GX(2) = DY31/DET
      GX(3) = -DY21/DET
      GX(1) = -GX(2)-GX(3)
      GY(2) = -DX31/DET
      GY(3) = DX21/DET
      GY(1) = -GY(2)-GY(3)

      DO K = 1, NLAY
        LO = MAX(1,K-1)
        HI = MIN(NLAY,K+1)
        ! Use differences relative to vertex 1 to avoid large-depth or
        ! reference-offset cancellation in the horizontal reconstruction.
        SX = GX(2)*(Z(NODES(2),K)-Z(NODES(1),K)) &
           + GX(3)*(Z(NODES(3),K)-Z(NODES(1),K))
        SY = GY(2)*(Z(NODES(2),K)-Z(NODES(1),K)) &
           + GY(3)*(Z(NODES(3),K)-Z(NODES(1),K))
        LX = GX(2)*(ANOMALY(NODES(2),K)-ANOMALY(NODES(1),K)) &
           + GX(3)*(ANOMALY(NODES(3),K)-ANOMALY(NODES(1),K))
        LY = GY(2)*(ANOMALY(NODES(2),K)-ANOMALY(NODES(1),K)) &
           + GY(3)*(ANOMALY(NODES(3),K)-ANOMALY(NODES(1),K))
        DZC = 0.0_SP
        DO A = 1, 3
          I = NODES(A)
          SPAN(A) = Z(I,LO)-Z(I,HI)
          DZC = DZC+(ANOMALY(I,LO)-ANOMALY(I,HI))/SPAN(A)/3.0_SP
        END DO
        QX = LX-SX*DZC
        QY = LY-SY*DZC
        WEIGHT = AREA*THICKNESS(E,K)*SUM(KH(NODES,K))/3.0_SP

        ! These raw sparse coefficients allow duplicate entries at end
        ! layers (LO=K or HI=K). Their absolute-sum bound is conservative.
        BX(:,1) = GX
        BY(:,1) = GY
        BX(:,2) = -SX/(3.0_SP*SPAN)
        BY(:,2) = -SY/(3.0_SP*SPAN)
        BX(:,3) = -BX(:,2)
        BY(:,3) = -BY(:,2)
        SUMBX = SUM(ABS(BX))
        SUMBY = SUM(ABS(BY))

        DO A = 1, 3
          I = NODES(A)
          LX = WEIGHT*GX(A)*QX+WEIGHT*GY(A)*QY
          RESIDUAL(I,K) = RESIDUAL(I,K)+LX
          IF (PRESENT(LATERAL)) LATERAL(I,K) = LATERAL(I,K)+LX

          ! Transpose of the vertical correction, from the corrected Q.
          ! The same transport crosses every interface between LO and HI,
          ! so it is added to one layer and removed from the other.
          VFLUX = -WEIGHT*(SX*QX+SY*QY)/(3.0_SP*SPAN(A))
          RESIDUAL(I,LO) = RESIDUAL(I,LO)+VFLUX
          RESIDUAL(I,HI) = RESIDUAL(I,HI)-VFLUX
          IF (PRESENT(INTERFACE_FLUX)) THEN
            DO J = LO+1, HI
              INTERFACE_FLUX(I,J) = INTERFACE_FLUX(I,J)+VFLUX
            END DO
          END IF

          BOUND(I,K) = BOUND(I,K)+WEIGHT*(ABS(BX(A,1))*SUMBX+ABS(BY(A,1))*SUMBY)
          BOUND(I,LO) = BOUND(I,LO)+WEIGHT*(ABS(BX(A,2))*SUMBX+ABS(BY(A,2))*SUMBY)
          BOUND(I,HI) = BOUND(I,HI)+WEIGHT*(ABS(BX(A,3))*SUMBX+ABS(BY(A,3))*SUMBY)
        END DO
      END DO
    END DO
  END SUBROUTINE ROTATED_DIFFUSION_RESIDUAL
END MODULE MOD_ROTATED_DIFFUSION
