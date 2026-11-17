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
  USE MOD_PREC, ONLY: SP, DP
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: ROTATED_DIFFUSION_RESIDUAL, ROTATED_DIFFUSION_GEOMETRY
  PUBLIC :: PREPARE_ROTATED_DIFFUSION_GEOMETRY

  ! Immutable horizontal mesh data, reusable across tracers and time steps.
  TYPE ROTATED_DIFFUSION_GEOMETRY
    PRIVATE
    INTEGER :: NNODE = 0, NELEM = -1
    LOGICAL :: READY = .FALSE.
    REAL(SP), ALLOCATABLE :: GX(:,:), GY(:,:), AREA(:)
  END TYPE ROTATED_DIFFUSION_GEOMETRY

CONTAINS

  SUBROUTINE ROTATED_DIFFUSION_RESIDUAL(NNODE, NELEM, NLAY, TRIANGLES, X, Y, &
       Z, THICKNESS, KH, ANOMALY, RESIDUAL, BOUND, IERR, LATERAL, INTERFACE_FLUX, SPHERICAL_RADIUS, GEOMETRY)
    INTEGER, INTENT(IN) :: NNODE, NELEM, NLAY
    INTEGER, INTENT(IN) :: TRIANGLES(3,NELEM)
    REAL(SP), INTENT(IN) :: X(NNODE), Y(NNODE), Z(NNODE,NLAY)
    REAL(SP), INTENT(IN) :: THICKNESS(NELEM,NLAY)
    REAL(SP), INTENT(IN) :: KH(NNODE,NLAY), ANOMALY(NNODE,NLAY)
    REAL(SP), INTENT(OUT) :: RESIDUAL(NNODE,NLAY), BOUND(NNODE,NLAY)
    INTEGER, INTENT(OUT) :: IERR
    REAL(SP), OPTIONAL, INTENT(OUT) :: LATERAL(NNODE,NLAY)
    REAL(SP), OPTIONAL, INTENT(OUT) :: INTERFACE_FLUX(NNODE,NLAY+1)
    ! Present: X/Y are longitude/latitude in degrees, radius is in metres.
    ! Absent: retain the original Cartesian metre-coordinate calculation.
    REAL(SP), OPTIONAL, INTENT(IN) :: SPHERICAL_RADIUS

    ! The caller must rebuild GEOMETRY if coordinates/connectivity change.
    TYPE(ROTATED_DIFFUSION_GEOMETRY), OPTIONAL, TARGET, INTENT(IN) :: GEOMETRY
    TYPE(ROTATED_DIFFUSION_GEOMETRY), TARGET :: LOCAL_GEOMETRY
    TYPE(ROTATED_DIFFUSION_GEOMETRY), POINTER :: GEO

    INTEGER :: E, K, A, I, LO, HI, J
    INTEGER :: NODES(3)
    REAL(SP) :: GX(3), GY(3), BX(3,3), BY(3,3), SPAN(3)
    REAL(SP) :: AREA, SX, SY, DZC, QX, QY, WEIGHT, VFLUX, LX, LY
    REAL(SP) :: SUMBX, SUMBY

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

    IF (PRESENT(GEOMETRY)) THEN
      IF (.NOT.GEOMETRY%READY .OR. GEOMETRY%NNODE /= NNODE .OR. GEOMETRY%NELEM /= NELEM) THEN
        IERR = 7
        RETURN
      END IF
      GEO => GEOMETRY
    ELSE
      ! Standalone callers retain full validation without managing a cache.
      CALL PREPARE_ROTATED_DIFFUSION_GEOMETRY(NNODE,NELEM,TRIANGLES,X,Y, &
           LOCAL_GEOMETRY,IERR,SPHERICAL_RADIUS)
      IF (IERR /= 0) RETURN
      GEO => LOCAL_GEOMETRY
    END IF

    DO E = 1, NELEM
      NODES = TRIANGLES(:,E)
      GX = GEO%GX(:,E)
      GY = GEO%GY(:,E)
      AREA = GEO%AREA(E)

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

  ! INTENT(OUT) discards any previous cache, including after failed rebuilds.
  SUBROUTINE PREPARE_ROTATED_DIFFUSION_GEOMETRY(NNODE,NELEM,TRIANGLES,X,Y,GEOMETRY,IERR,SPHERICAL_RADIUS)
    INTEGER, INTENT(IN) :: NNODE, NELEM, TRIANGLES(3,NELEM)
    REAL(SP), INTENT(IN) :: X(NNODE), Y(NNODE)
    TYPE(ROTATED_DIFFUSION_GEOMETRY), INTENT(OUT) :: GEOMETRY
    INTEGER, INTENT(OUT) :: IERR
    REAL(SP), OPTIONAL, INTENT(IN) :: SPHERICAL_RADIUS
    INTEGER :: E, NODES(3)
    REAL(SP) :: DX21, DY21, DX31, DY31, DET, SCALE

    IERR = 0
    IF (NNODE < 1 .OR. NELEM < 0) THEN
      IERR = 1
      RETURN
    END IF
    IF (ANY(TRIANGLES < 1) .OR. ANY(TRIANGLES > NNODE)) THEN
      IERR = 2
      RETURN
    END IF
    IF (.NOT.ALL(ABS(X) < HUGE(1.0_SP)) .OR. .NOT.ALL(ABS(Y) < HUGE(1.0_SP))) THEN
      IERR = 3
      RETURN
    END IF
    IF (PRESENT(SPHERICAL_RADIUS)) THEN
      IF (.NOT.(SPHERICAL_RADIUS > 0.0_SP .AND. SPHERICAL_RADIUS < HUGE(1.0_SP)) .OR. &
          .NOT.ALL(ABS(Y) <= 90.0_SP)) THEN
        IERR = 6
        RETURN
      END IF
    END IF
    ALLOCATE(GEOMETRY%GX(3,NELEM),GEOMETRY%GY(3,NELEM),GEOMETRY%AREA(NELEM))
    DO E = 1, NELEM
      NODES = TRIANGLES(:,E)
      IF (PRESENT(SPHERICAL_RADIUS)) THEN
        CALL SPHERICAL_TRIANGLE(X(NODES),Y(NODES),SPHERICAL_RADIUS, &
             GEOMETRY%GX(:,E),GEOMETRY%GY(:,E),GEOMETRY%AREA(E),IERR)
        IF (IERR /= 0) RETURN
      ELSE
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
        GEOMETRY%AREA(E) = 0.5_SP*ABS(DET)
        GEOMETRY%GX(2,E) = DY31/DET
        GEOMETRY%GX(3,E) = -DY21/DET
        GEOMETRY%GX(1,E) = -GEOMETRY%GX(2,E)-GEOMETRY%GX(3,E)
        GEOMETRY%GY(2,E) = -DX31/DET
        GEOMETRY%GY(3,E) = DX21/DET
        GEOMETRY%GY(1,E) = -GEOMETRY%GY(2,E)-GEOMETRY%GY(3,E)
      END IF
    END DO
    GEOMETRY%NNODE = NNODE
    GEOMETRY%NELEM = NELEM
    GEOMETRY%READY = .TRUE.
  END SUBROUTINE PREPARE_ROTATED_DIFFUSION_GEOMETRY

  ! Local P1 reconstruction in an orthonormal centroid tangent frame, with
  ! the true spherical triangle area for integration. This is a consistent
  ! small-element surface discretization, rather than an exact integration
  ! of curved-element basis functions. Its metric error vanishes on mesh
  ! refinement. The frame uses a projected edge, so longitude seams and
  ! either geographic pole introduce no coordinate singularity.
  SUBROUTINE SPHERICAL_TRIANGLE(LON,LAT,RADIUS,GX,GY,AREA,IERR)
    REAL(SP), INTENT(IN) :: LON(3), LAT(3), RADIUS
    REAL(SP), INTENT(OUT) :: GX(3), GY(3), AREA
    INTEGER, INTENT(OUT) :: IERR
    REAL(DP) :: P(3,3), CENTRE(3), T1(3), T2(3), EDGE21(3), EDGE31(3)
    REAL(DP) :: LAMBDA, PHI, DEG, NORM_CENTRE, NORM_EDGE, R
    REAL(DP) :: DX21, DY21, DX31, DY31, DET, SCALE, SOLID_ANGLE, AREA_DP, DENOM
    INTEGER :: A

    IERR = 0
    GX = 0.0_SP
    GY = 0.0_SP
    AREA = 0.0_SP
    DEG = ACOS(-1.0_DP)/180.0_DP
    R = REAL(RADIUS,DP)
    DO A = 1, 3
      LAMBDA = MODULO(REAL(LON(A),DP),360.0_DP)*DEG
      PHI = REAL(LAT(A),DP)*DEG
      P(:,A) = (/ COS(PHI)*COS(LAMBDA), COS(PHI)*SIN(LAMBDA), SIN(PHI) /)
    END DO
    CENTRE = SUM(P,DIM=2)
    NORM_CENTRE = SQRT(DOT_PRODUCT(CENTRE,CENTRE))
    IF (.NOT.(NORM_CENTRE > 64.0_DP*EPSILON(1.0_DP))) THEN
      IERR = 6
      RETURN
    END IF
    CENTRE = CENTRE/NORM_CENTRE
    ! Only local triangles fitting inside this open hemisphere are valid.
    DO A = 1, 3
      IF (.NOT.(DOT_PRODUCT(CENTRE,P(:,A)) > 64.0_DP*EPSILON(1.0_DP))) THEN
        IERR = 6
        RETURN
      END IF
    END DO
    EDGE21 = P(:,2)-P(:,1)
    EDGE31 = P(:,3)-P(:,1)
    T1 = EDGE21-CENTRE*DOT_PRODUCT(CENTRE,EDGE21)
    NORM_EDGE = SQRT(DOT_PRODUCT(T1,T1))
    IF (.NOT.(NORM_EDGE > 0.0_DP)) THEN
      IERR = 3
      RETURN
    END IF
    T1 = T1/NORM_EDGE
    T2 = CROSS_PRODUCT(CENTRE,T1)
    DX21 = R*DOT_PRODUCT(EDGE21,T1)
    DY21 = R*DOT_PRODUCT(EDGE21,T2)
    DX31 = R*DOT_PRODUCT(EDGE31,T1)
    DY31 = R*DOT_PRODUCT(EDGE31,T2)
    DET = DX21*DY31-DX31*DY21
    SCALE = MAX(DX21*DX21+DY21*DY21,DX31*DX31+DY31*DY31)
    IF (.NOT.(ABS(DET) > REAL(EPSILON(1.0_SP),DP)*SCALE)) THEN
      IERR = 3
      RETURN
    END IF
    DENOM = 1.0_DP+DOT_PRODUCT(P(:,1),P(:,2))+ &
         DOT_PRODUCT(P(:,2),P(:,3))+DOT_PRODUCT(P(:,3),P(:,1))
    SOLID_ANGLE = 2.0_DP*ATAN2(ABS(DOT_PRODUCT(P(:,1),CROSS_PRODUCT(EDGE21,EDGE31))),DENOM)
    AREA_DP = R*R*SOLID_ANGLE
    IF (.NOT.(AREA_DP >= REAL(TINY(1.0_SP),DP) .AND. &
        AREA_DP < REAL(HUGE(1.0_SP),DP)) .OR. &
        .NOT.(MAX(ABS(DY31/DET),ABS(DY21/DET),ABS(DX31/DET),ABS(DX21/DET)) &
        < REAL(HUGE(1.0_SP),DP)/2.0_DP)) THEN
      IERR = 6
      RETURN
    END IF
    AREA = REAL(AREA_DP,SP)
    GX(2) = REAL(DY31/DET,SP)
    GX(3) = REAL(-DY21/DET,SP)
    GX(1) = -GX(2)-GX(3)
    GY(2) = REAL(-DX31/DET,SP)
    GY(3) = REAL(DX21/DET,SP)
    GY(1) = -GY(2)-GY(3)
  END SUBROUTINE SPHERICAL_TRIANGLE

  PURE FUNCTION CROSS_PRODUCT(A,B) RESULT(C)
    REAL(DP), INTENT(IN) :: A(3), B(3)
    REAL(DP) :: C(3)
    C = (/ A(2)*B(3)-A(3)*B(2), A(3)*B(1)-A(1)*B(3), A(1)*B(2)-A(2)*B(1) /)
  END FUNCTION CROSS_PRODUCT
END MODULE MOD_ROTATED_DIFFUSION
