! Numerically independent kernels for the first phase of FVCOM type-4 nesting.
! This module deliberately does not modify the external-mode volume transport.
MODULE MOD_NESTING_TYPE4
  USE MOD_PREC, ONLY : SP, DP
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: NEST4_RELAX_WEIGHT, NEST4_RELAX_SHEAR, NEST4_PROJECT_TRANSPORT

CONTAINS

  PURE REAL(SP) FUNCTION NEST4_RELAX_WEIGHT(ALPHA_NEST, DT, TAU, ON_OBC) RESULT(W)
    REAL(SP), INTENT(IN) :: ALPHA_NEST, DT, TAU
    LOGICAL, INTENT(IN) :: ON_OBC

    ! OBC is a hard Dirichlet boundary, not an artificially short timescale.
    IF (ON_OBC) THEN
       W = 1.0_SP
    ELSE IF (ALPHA_NEST <= 0.0_SP .OR. DT <= 0.0_SP) THEN
       W = 0.0_SP
    ELSE
       ! Input validation (ALPHA_NEST in [0,1], TAU>0) is done by MOD_NESTING.
       W = REAL(1.0_DP - EXP(-REAL(ALPHA_NEST,DP)*REAL(DT,DP)/REAL(TAU,DP)),SP)
    END IF
  END FUNCTION NEST4_RELAX_WEIGHT

  PURE SUBROUTINE NEST4_RELAX_SHEAR(MODEL, PARENT, DZ, W, RESULT)
    REAL(SP), INTENT(IN) :: MODEL(:), PARENT(:), DZ(:), W
    REAL(SP), INTENT(OUT) :: RESULT(:)
    REAL(DP) :: DZSUM, MODEL_MEAN, PARENT_MEAN, NEW_MEAN

    ! Preserve the input model's depth mean (already corrected by ADJUST2D3D).
    ! Only the zero-depth-mean vertical structure is relaxed to the parent.
    IF (W <= 0.0_SP) THEN
       RESULT = MODEL
       RETURN
    END IF
    DZSUM = SUM(REAL(DZ,DP))
    IF (DZSUM <= 0.0_DP) THEN
       RESULT = MODEL
       RETURN
    END IF
    MODEL_MEAN  = SUM(REAL(MODEL,DP)*REAL(DZ,DP))/DZSUM
    PARENT_MEAN = SUM(REAL(PARENT,DP)*REAL(DZ,DP))/DZSUM
    RESULT = REAL(MODEL_MEAN,SP) + (1.0_SP-W)*(MODEL-REAL(MODEL_MEAN,SP)) &
         & + W*(PARENT-REAL(PARENT_MEAN,SP))

    ! Remove the small residual caused by SP rounding; keeps 2D/3D consistent.
    NEW_MEAN = SUM(REAL(RESULT,DP)*REAL(DZ,DP))/DZSUM
    RESULT = RESULT + REAL(MODEL_MEAN-NEW_MEAN,SP)
  END SUBROUTINE NEST4_RELAX_SHEAR


  ! Minimum-magnitude correction to element-integrated transports.
  ! The operator B is FVCOM's unmodified EXTEL_EDGE CV half-edge divergence:
  ! F_e = -qy_x * DLTYE + qy_y * DLTXE,  B(q)(ia)-=F, B(q)(ib)+=F.
  ! All constraints are on owned NON-OBC nodes. OBC nodes take up the residual
  ! as a real open-boundary volume exchange, not as an interior source.
  SUBROUTINE NEST4_PROJECT_TRANSPORT(M,MT,NT,NCV,TRI,ENDS,DX,DY, &
       & AREA, BTYPE, MOBILITY, DELTA_ETA, QX_SOFT,QY_SOFT,DT, &
       & DQX,DQY,INFO,ITER,RELRES,EXCHANGE_NODE,EXCHANGE_CELL,GLOBAL_SUM)
    INTEGER, INTENT(IN) :: M,MT,NT,NCV
    INTEGER, INTENT(IN) :: TRI(:),ENDS(:,:),BTYPE(0:MT)
    REAL(SP), INTENT(IN) :: DX(:),DY(:),AREA(0:MT),MOBILITY(0:NT)
    REAL(SP), INTENT(IN) :: DELTA_ETA(0:MT), QX_SOFT(0:NT),QY_SOFT(0:NT),DT
    REAL(SP), INTENT(OUT) :: DQX(0:NT),DQY(0:NT),RELRES
    INTEGER, INTENT(OUT) :: INFO,ITER
    INTERFACE
       SUBROUTINE EXCHANGE_NODE(F)
         IMPORT SP
         REAL(SP), ALLOCATABLE, INTENT(INOUT) :: F(:)
       END SUBROUTINE
       SUBROUTINE EXCHANGE_CELL(F)
         IMPORT SP
         REAL(SP), ALLOCATABLE, INTENT(INOUT) :: F(:)
       END SUBROUTINE
       FUNCTION GLOBAL_SUM(X) RESULT(Y)
         IMPORT DP
         REAL(DP), INTENT(IN) :: X
         REAL(DP) :: Y
       END FUNCTION
    END INTERFACE
    REAL(SP), ALLOCATABLE :: PHI(:),RES(:),DIR(:),Z(:),AP(:),RHS(:),DIAG(:)
    REAL(SP), ALLOCATABLE :: AX(:),AY(:),DIV(:)
    REAL(DP) :: RHSNORM2,RHO,RHO_NEW,PDOT,STEP,BETA,NORM2
    INTEGER :: E,IA,IB,C,I,UNREACHABLE
    INTEGER, PARAMETER :: MAXITER = 350
    REAL(DP), PARAMETER :: TOL = 5.0E-6_DP

    INFO = 0; ITER = 0; RELRES = 0.0_SP
    DQX = 0.0_SP; DQY = 0.0_SP
    IF (DT <= 0.0_SP) THEN
       INFO = 4; RETURN
    END IF
    ALLOCATE(PHI(0:MT),RES(0:MT),DIR(0:MT),Z(0:MT),AP(0:MT), &
         & RHS(0:MT),DIAG(0:MT),AX(0:NT),AY(0:NT),DIV(0:MT))
    CALL APPLY_B(QX_SOFT,QY_SOFT,DIV)
    RHS=0.0_SP; DIAG=0.0_SP
    DO I=1,M
       IF(BTYPE(I) /= 2) RHS(I) = -AREA(I)*DELTA_ETA(I)/DT-DIV(I)
    END DO

    ! Jacobi approximation from per-half-edge contributions. A is B W B^T.
    DO E=1,NCV
       C=TRI(E); IA=ENDS(E,1); IB=ENDS(E,2)
       IF (MOBILITY(C) <= 0.0_SP) CYCLE
       DIAG(IA)=DIAG(IA)+MOBILITY(C)*(DX(E)**2+DY(E)**2)
       DIAG(IB)=DIAG(IB)+MOBILITY(C)*(DX(E)**2+DY(E)**2)
    END DO
    RHSNORM2=GLOBAL_SUM(SUM(REAL(RHS(1:M),DP)**2))
    IF(RHSNORM2 <= 1.0E-60_DP) THEN
       DQX=QX_SOFT; DQY=QY_SOFT
       RETURN
    END IF
    UNREACHABLE=0
    DO I=1,M
       IF(BTYPE(I) /= 2 .AND. DIAG(I) <= 0.0_SP .AND. ABS(RHS(I)) > 0.0_SP) &
            & UNREACHABLE=1
    END DO
    ! All ranks must see the same failure before any further collective.
    IF(GLOBAL_SUM(REAL(UNREACHABLE,DP))>0.0_DP) THEN
       INFO=3; RETURN
    END IF

    PHI=0.0_SP; RES=RHS; Z=0.0_SP; DIR=0.0_SP
    DO I=1,M
       IF(BTYPE(I) /= 2 .AND. DIAG(I)>0.0_SP) Z(I)=RES(I)/DIAG(I)
    END DO
    DIR=Z
    RHO=GLOBAL_SUM(SUM(REAL(RES(1:M),DP)*REAL(Z(1:M),DP)))
    IF(RHO <= 0.0_DP) THEN
       INFO=2; RETURN
    END IF

    DO ITER=1,MAXITER
       CALL APPLY_A(DIR,AP)
       PDOT=GLOBAL_SUM(SUM(REAL(DIR(1:M),DP)*REAL(AP(1:M),DP)))
       IF(PDOT <= 0.0_DP) THEN
          INFO=2; EXIT
       END IF
       STEP=RHO/PDOT
       DO I=1,M
          IF(BTYPE(I) == 2) CYCLE
          PHI(I)=PHI(I)+REAL(STEP,SP)*DIR(I)
          RES(I)=RES(I)-REAL(STEP,SP)*AP(I)
       END DO
       NORM2=GLOBAL_SUM(SUM(REAL(RES(1:M),DP)**2))
       RELRES=REAL(SQRT(NORM2/RHSNORM2),SP)
       IF(RELRES <= TOL) EXIT
       Z=0.0_SP
       DO I=1,M
          IF(BTYPE(I) /= 2 .AND. DIAG(I)>0.0_SP) Z(I)=RES(I)/DIAG(I)
       END DO
       RHO_NEW=GLOBAL_SUM(SUM(REAL(RES(1:M),DP)*REAL(Z(1:M),DP)))
       IF(RHO_NEW<=0.0_DP) THEN
          INFO=2; EXIT
       END IF
       BETA=RHO_NEW/RHO
       DIR=Z+REAL(BETA,SP)*DIR
       RHO=RHO_NEW
    END DO
    IF(INFO /= 0) RETURN
    IF(RELRES>TOL) THEN
       INFO=1; RETURN
    END IF

    CALL EXCHANGE_NODE(PHI)
    CALL APPLY_BT(PHI,AX,AY)
    AX=QX_SOFT+MOBILITY*AX
    AY=QY_SOFT+MOBILITY*AY
    CALL EXCHANGE_CELL(AX)
    CALL EXCHANGE_CELL(AY)
    DQX=AX
    DQY=AY
    ! Final residual check for the actual returned transport and state change.
    CALL APPLY_B(DQX,DQY,DIV)
    NORM2=0.0_DP
    DO I=1,M
       IF(BTYPE(I) /= 2) NORM2=NORM2+REAL(DIV(I)+AREA(I)*DELTA_ETA(I)/DT,DP)**2
    END DO
    NORM2=GLOBAL_SUM(NORM2)
    RELRES=REAL(SQRT(NORM2/RHSNORM2),SP)
    IF(RELRES > 2.0E-5_SP) INFO=5

  CONTAINS
    SUBROUTINE APPLY_B(QX,QY,OUT)
      REAL(SP), INTENT(IN) :: QX(0:NT),QY(0:NT)
      REAL(SP), INTENT(OUT) :: OUT(0:MT)
      REAL(SP) :: FLUX
      INTEGER :: J,A,B,K
      OUT=0.0_SP
      DO J=1,NCV
         K=TRI(J); A=ENDS(J,1); B=ENDS(J,2)
         FLUX=-QX(K)*DY(J)+QY(K)*DX(J)
         OUT(A)=OUT(A)-FLUX
         OUT(B)=OUT(B)+FLUX
      END DO
    END SUBROUTINE APPLY_B

    SUBROUTINE APPLY_BT(POT,QX,QY)
      REAL(SP), INTENT(IN) :: POT(0:MT)
      REAL(SP), INTENT(OUT) :: QX(0:NT),QY(0:NT)
      INTEGER :: J,K,A,B
      REAL(SP) :: DIFF
      QX=0.0_SP; QY=0.0_SP
      DO J=1,NCV
         K=TRI(J); A=ENDS(J,1); B=ENDS(J,2)
         DIFF=POT(B)-POT(A)
         QX(K)=QX(K)-DY(J)*DIFF
         QY(K)=QY(K)+DX(J)*DIFF
      END DO
    END SUBROUTINE APPLY_BT

    SUBROUTINE APPLY_A(POT,OUT)
      REAL(SP), ALLOCATABLE, INTENT(INOUT) :: POT(:)
      REAL(SP), INTENT(OUT) :: OUT(0:MT)
      CALL EXCHANGE_NODE(POT)
      CALL APPLY_BT(POT,AX,AY)
      AX=AX*MOBILITY; AY=AY*MOBILITY
      CALL EXCHANGE_CELL(AX)
      CALL EXCHANGE_CELL(AY)
      CALL APPLY_B(AX,AY,OUT)
      DO I=1,MT
         IF(BTYPE(I) == 2) OUT(I)=0.0_SP
      END DO
    END SUBROUTINE APPLY_A
  END SUBROUTINE NEST4_PROJECT_TRANSPORT

END MODULE MOD_NESTING_TYPE4
