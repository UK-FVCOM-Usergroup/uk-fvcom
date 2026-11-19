! ROMS PJ_GRADPQ4 vertical reconstruction and pressure moments, adapted
! to FVCOM triangular prisms. This is not the structured ROMS horizontal
! FC limiter: horizontal forces here use a common-z pressure field and
! its closed-surface integral. All pressure work uses double precision.
!
! Vertical reconstruction adapted from myroms/roms, ROMS/Nonlinear/
! prsgrd44.h, blob 1b4fa760511320eaded3984367676f8d9782e627.
! Copyright (c) 2002-2026 Regional Ocean Modeling System (ROMS)
! Licensed under The MIT/X License
!
! Permission is hereby granted, free of charge, to any person obtaining
! a copy of this software and associated documentation files (the
! "Software"), to deal in the Software without restriction, including
! without limitation the rights to use, copy, modify, merge, publish,
! distribute, sublicense, and/or sell copies of the Software, and to
! permit persons to whom the Software is furnished to do so, subject to
! the following conditions:
!
! The above copyright notice and this permission notice shall be
! included in all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
! EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
! MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
! NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
! BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
! ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
! CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.

MODULE MOD_PJ44
  USE MOD_PREC, ONLY: DP
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_IS_FINITE
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: PJ44_PROFILE, PJ44_BUILD, PJ44_VALUE, PJ44_PRISM, PJ44_EDGE
  TYPE PJ44_PROFILE
    INTEGER :: N=0
    REAL(DP), ALLOCATABLE :: Z(:), R(:), D(:), P(:), S(:), MOM(:)
    REAL(DP), ALLOCATABLE :: PC(:,:), SC(:,:)
  END TYPE
  REAL(DP), PARAMETER :: EPS=1.0E-8_DP
CONTAINS

  ! Bottom-up indexing, density in kg/m3, z in metres. As in ROMS44,
  ! supplied layer densities are interpreted as layer means.
  SUBROUTINE PJ44_BUILD(Z,Q,C,STATUS)
    REAL(DP), INTENT(IN) :: Z(0:), Q(:)
    TYPE(PJ44_PROFILE), INTENT(INOUT) :: C
    INTEGER, INTENT(OUT) :: STATUS
    REAL(DP) :: H(SIZE(Q)), FC(SIZE(Q)), ALEFT(SIZE(Q)), ARIGHT(SIZE(Q))
    REAL(DP) :: DL(SIZE(Q)), DR(SIZE(Q)), R1(0:SIZE(Q)), A(0:4)
    REAL(DP) :: DELTAL,DELTAR,CF,CL,CR,HDD,RR,AMPL,F,G,V
    INTEGER :: N,K,L
    STATUS=1
    N=SIZE(Q)
    IF(N<1 .OR. SIZE(Z)/=N+1) RETURN
    IF(.NOT.ALL(IEEE_IS_FINITE(Z)) .OR. .NOT.ALL(IEEE_IS_FINITE(Q))) RETURN
    H=Z(1:N)-Z(0:N-1)
    IF(ANY(H<=0.0_DP)) RETURN
    IF(C%N/=N) THEN
      IF(ALLOCATED(C%Z)) DEALLOCATE(C%Z,C%R,C%D,C%P,C%S,C%MOM,C%PC,C%SC)
      ALLOCATE(C%Z(0:N),C%R(0:N),C%D(0:N),C%P(0:N),C%S(0:N), &
               C%MOM(N),C%PC(0:5,N),C%SC(0:6,N))
      C%N=N
    END IF
    C%Z=Z
    IF(N==1) THEN
      C%R=Q(1)
      C%D=0.0_DP
    ELSE IF(N==2) THEN
      ! Conservative linear fallback; ROMS44 needs at least 3 layers.
      CF=2.0_DP*(Q(2)-Q(1))/(H(1)+H(2))
      C%D=CF
      C%R(0)=Q(1)-0.5_DP*H(1)*CF
      C%R(1)=Q(1)+0.5_DP*H(1)*CF
      C%R(2)=Q(2)+0.5_DP*H(2)*CF
    ELSE
      DO K=N-1,1,-1
        FC(K)=1.0_DP/(H(K+1)+H(K))
        C%R(K)=FC(K)*(Q(K+1)*H(K)+Q(K)*H(K+1))
        C%D(K)=FC(K)*(Q(K+1)-Q(K))
      END DO
      DO K=2,N-1
        DELTAR=H(K)*C%D(K)
        DELTAL=H(K)*C%D(K-1)
        IF(DELTAR*DELTAL<0.0_DP) THEN
          DELTAR=0.0_DP
          DELTAL=0.0_DP
        END IF
        CF=H(K-1)+2.0_DP*H(K)+H(K+1)
        CR=CF*C%D(K)
        CL=CF*C%D(K-1)
        IF(ABS(DELTAR)>ABS(CL)) DELTAR=CL
        IF(ABS(DELTAL)>ABS(CR)) DELTAL=CR
        CF=(DELTAR-DELTAL)/(H(K-1)+H(K)+H(K+1))
        DELTAR=DELTAR-CF*H(K+1)
        DELTAL=DELTAL+CF*H(K-1)
        ARIGHT(K)=Q(K)+DELTAR
        ALEFT(K)=Q(K)-DELTAL
        DR(K)=(2.0_DP*DELTAR-DELTAL)**2
        DL(K)=(2.0_DP*DELTAL-DELTAR)**2
      END DO
      ALEFT(N)=ARIGHT(N-1)
      ARIGHT(N)=2.0_DP*Q(N)-ALEFT(N)
      DR(N)=(2.0_DP*ARIGHT(N)+ALEFT(N)-3.0_DP*Q(N))**2
      DL(N)=(3.0_DP*Q(N)-2.0_DP*ALEFT(N)-ARIGHT(N))**2
      ARIGHT(1)=ALEFT(2)
      ALEFT(1)=2.0_DP*Q(1)-ARIGHT(1)
      DR(1)=(2.0_DP*ARIGHT(1)+ALEFT(1)-3.0_DP*Q(1))**2
      DL(1)=(3.0_DP*Q(1)-2.0_DP*ALEFT(1)-ARIGHT(1))**2
      DO K=1,N-1
        DELTAL=MAX(DL(K),EPS)
        DELTAR=MAX(DR(K+1),EPS)
        R1(K)=(DELTAR*ARIGHT(K)+DELTAL*ALEFT(K+1))/(DELTAR+DELTAL)
      END DO
      R1(N)=2.0_DP*Q(N)-R1(N-1)
      R1(0)=2.0_DP*Q(1)-R1(1)
      DO K=1,N
        DELTAR=R1(K)-Q(K)
        DELTAL=Q(K)-R1(K-1)
        CF=DELTAR*DELTAL
        IF(CF>EPS) THEN
          CF=(DELTAR+DELTAL)/CF
        ELSE
          CF=0.0_DP
        END IF
        CL=CF*DELTAL
        CR=CF*DELTAR
        IF(CL>3.0_DP) THEN
          CL=CL*DELTAL
          CR=0.0_DP
        ELSE IF(CR>3.0_DP) THEN
          CL=0.0_DP
          CR=CR*DELTAR
        ELSE
          CL=4.0_DP*DELTAL-2.0_DP*DELTAR
          CR=4.0_DP*DELTAR-2.0_DP*DELTAL
        END IF
        DR(K)=CR/H(K)
        DL(K)=CL/H(K)
      END DO
      DO K=N-1,1,-1
        C%D(K)=FC(K)*(H(K+1)*DL(K+1)+H(K)*DR(K))
        CR=8.0_DP*(DR(K)+2.0_DP*DL(K))
        CL=8.0_DP*(DL(K+1)+2.0_DP*DR(K+1))
        IF(ABS(C%D(K))>ABS(CR)) C%D(K)=CR
        IF(ABS(C%D(K))>ABS(CL)) C%D(K)=CL
        IF((DL(K+1)-DR(K))*(Q(K+1)-Q(K))>0.0_DP) THEN
          HDD=H(K)*(C%D(K)-DR(K))
          RR=Q(K)-R1(K-1)
        ELSE
          HDD=H(K+1)*(DL(K+1)-C%D(K))
          RR=R1(K+1)-Q(K+1)
        END IF
        RR=ABS(RR)
        AMPL=0.2_DP*HDD*RR
        HDD=ABS(HDD)
        CF=RR*RR+0.0763636363636363636_DP*HDD*(RR+0.004329004329004329_DP*HDD)
        IF(CF>EPS) THEN
          AMPL=AMPL*(RR+0.0363636363636363636_DP*HDD)/CF
        ELSE
          AMPL=0.0_DP
        END IF
        C%R(K)=R1(K)+AMPL
      END DO
      C%R(0)=2.0_DP*Q(1)-C%R(1)
      C%R(N)=2.0_DP*Q(N)-C%R(N-1)
      C%D(0)=C%D(1)
      C%D(N)=C%D(N-1)
    END IF
    C%P(N)=0.0_DP
    C%S(N)=0.0_DP
    DO K=N,1,-1
      C%P(K-1)=C%P(K)+H(K)*Q(K)
      C%MOM(K)=0.5_DP*H(K)*(C%P(K)+C%P(K-1)+0.2_DP*H(K)* &
        (C%R(K)-C%R(K-1)-H(K)*(C%D(K)+C%D(K-1))/12.0_DP))
      C%S(K-1)=C%S(K)+C%MOM(K)
      A(0)=C%R(K)
      A(1)=-H(K)*C%D(K)
      F=C%R(K-1)-A(0)-A(1)
      G=-H(K)*C%D(K-1)-A(1)
      V=Q(K)-A(0)-0.5_DP*A(1)
      A(2)=-12.0_DP*F+1.5_DP*G+30.0_DP*V
      A(3)=28.0_DP*F-4.0_DP*G-60.0_DP*V
      A(4)=-15.0_DP*F+2.5_DP*G+30.0_DP*V
      C%PC(0,K)=C%P(K)
      C%SC(0,K)=C%S(K)
      C%SC(1,K)=H(K)*C%P(K)
      DO L=0,4
        C%PC(L+1,K)=H(K)*A(L)/REAL(L+1,DP)
        C%SC(L+2,K)=H(K)**2*A(L)/REAL((L+1)*(L+2),DP)
      END DO
    END DO
    IF(.NOT.ALL(IEEE_IS_FINITE(C%PC)) .OR. .NOT.ALL(IEEE_IS_FINITE(C%SC))) RETURN
    STATUS=0
  END SUBROUTINE PJ44_BUILD

  ! P=int_z^eta rho dz; S=int_z^eta P dz. Endpoint continuation is LINEAR
  ! in density, not a quartic polynomial extrapolated below the bottom.
  REAL(DP) FUNCTION PJ44_VALUE(C,Z,MOMENT) RESULT(V)
    TYPE(PJ44_PROFILE), INTENT(IN) :: C
    REAL(DP), INTENT(IN) :: Z
    LOGICAL, INTENT(IN) :: MOMENT
    INTEGER :: LO,HI,K,L,E
    REAL(DP) :: DZ,T
    IF(Z<C%Z(0) .OR. Z>C%Z(C%N)) THEN
      E=0
      IF(Z>C%Z(C%N)) E=C%N
      DZ=Z-C%Z(E)
      IF(MOMENT) THEN
        V=C%S(E)-C%P(E)*DZ+0.5_DP*C%R(E)*DZ**2+C%D(E)*DZ**3/6.0_DP
      ELSE
        V=C%P(E)-C%R(E)*DZ-0.5_DP*C%D(E)*DZ**2
      END IF
      RETURN
    END IF
    LO=1
    HI=C%N
    DO WHILE(LO<HI)
      K=(LO+HI)/2
      IF(Z<C%Z(K)) THEN
        HI=K
      ELSE
        LO=K+1
      END IF
    END DO
    K=LO
    T=(C%Z(K)-Z)/(C%Z(K)-C%Z(K-1))
    IF(MOMENT) THEN
      V=C%SC(6,K)
      DO L=5,0,-1
        V=V*T+C%SC(L,K)
      END DO
    ELSE
      V=C%PC(5,K)
      DO L=4,0,-1
        V=V*T+C%PC(L,K)
      END DO
    END IF
  END FUNCTION PJ44_VALUE

  ! Exact piecewise-polynomial line integral with weight (1-s)^A*s^B.
  REAL(DP) FUNCTION LINE(C,Z0,Z1,MOMENT,A,B) RESULT(V)
    TYPE(PJ44_PROFILE), INTENT(IN) :: C
    REAL(DP), INTENT(IN) :: Z0,Z1
    LOGICAL, INTENT(IN) :: MOMENT
    INTEGER, INTENT(IN) :: A,B
    REAL(DP), PARAMETER :: X(4)=[0.069431844202973712_DP,0.33000947820757187_DP, &
                                0.66999052179242813_DP,0.93056815579702629_DP]
    REAL(DP), PARAMETER :: W(4)=[0.17392742256872693_DP,0.32607257743127307_DP, &
                                0.32607257743127307_DP,0.17392742256872693_DP]
    REAL(DP) :: CUTS(C%N+3), DZ,T,L,R
    INTEGER :: N,K,J,FIRST,LAST,STEP
    V=0.0_DP
    DZ=Z1-Z0
    N=1
    CUTS(1)=0.0_DP
    IF(DZ/=0.0_DP) THEN
      FIRST=0; LAST=C%N; STEP=1
      IF(DZ<0.0_DP) THEN
        FIRST=C%N; LAST=0; STEP=-1
      END IF
      DO K=FIRST,LAST,STEP
        T=(C%Z(K)-Z0)/DZ
        IF(T>0.0_DP .AND. T<1.0_DP) THEN
          N=N+1
          CUTS(N)=T
        END IF
      END DO
    END IF
    N=N+1
    CUTS(N)=1.0_DP
    DO K=1,N-1
      L=CUTS(K); R=CUTS(K+1)
      DO J=1,4
        T=L+(R-L)*X(J)
        V=V+(R-L)*W(J)*(1.0_DP-T)**A*T**B*PJ44_VALUE(C,Z0+DZ*T,MOMENT)
      END DO
    END DO
  END FUNCTION LINE

  SUBROUTINE PJ44_EDGE(C1,C2,V)
    TYPE(PJ44_PROFILE), INTENT(IN) :: C1,C2
    REAL(DP), INTENT(OUT) :: V(C1%N)
    REAL(DP) :: J(0:C1%N),B1,B2,E1,E2
    INTEGER :: K
    E1=C1%Z(C1%N); E2=C2%Z(C2%N)
    B1=LINE(C1,E1,E2,.FALSE.,2,0)+LINE(C2,E1,E2,.FALSE.,1,1)
    B2=LINE(C1,E1,E2,.FALSE.,1,1)+LINE(C2,E1,E2,.FALSE.,0,2)
    DO K=0,C1%N
      J(K)=LINE(C1,C1%Z(K),C2%Z(K),.TRUE.,1,0)+ &
           LINE(C2,C1%Z(K),C2%Z(K),.TRUE.,0,1)
    END DO
    DO K=1,C1%N
      V(K)=J(K-1)-J(K)-(C1%Z(K)-C1%Z(K-1))*B1-(C2%Z(K)-C2%Z(K-1))*B2
    END DO
  END SUBROUTINE PJ44_EDGE

  ! Clip a barycentric polygon by an affine height bound.
  SUBROUTINE CLIP(P,N,Z,BOUND,ABOVE)
    REAL(DP), INTENT(INOUT) :: P(3,8)
    INTEGER, INTENT(INOUT) :: N
    REAL(DP), INTENT(IN) :: Z(3),BOUND
    LOGICAL, INTENT(IN) :: ABOVE
    REAL(DP) :: OUT(3,8),A(3),B(3),FA,FB,SGN
    INTEGER :: I,J,NO
    IF(N==0) RETURN
    SGN=1.0_DP
    IF(.NOT.ABOVE) SGN=-1.0_DP
    NO=0
    DO I=1,N
      J=MOD(I,N)+1
      A=P(:,I); B=P(:,J)
      FA=SGN*(DOT_PRODUCT(A,Z)-BOUND)
      FB=SGN*(DOT_PRODUCT(B,Z)-BOUND)
      IF(FA>=0.0_DP) THEN
        NO=NO+1; OUT(:,NO)=A
      END IF
      IF((FA<0.0_DP .AND. FB>0.0_DP) .OR. (FA>0.0_DP .AND. FB<0.0_DP)) THEN
        NO=NO+1; OUT(:,NO)=A+(B-A)*(FA/(FA-FB))
      END IF
    END DO
    N=NO
    IF(N>0) P(:,1:N)=OUT(:,1:N)
  END SUBROUTINE CLIP

  ! Degree-six positive quadrature, exact after splitting at profile knots.
  REAL(DP) FUNCTION CAP(C,SOURCE,Z) RESULT(V)
    TYPE(PJ44_PROFILE), INTENT(IN) :: C
    INTEGER, INTENT(IN) :: SOURCE
    REAL(DP), INTENT(IN) :: Z(3)
    REAL(DP), PARAMETER :: A=.501426509658179_DP,B=.249286745170910_DP
    REAL(DP), PARAMETER :: D=.873821971016996_DP,E=.063089014491502_DP
    REAL(DP), PARAMETER :: F=.053145049844816_DP,G=.310352451033785_DP,H=.636502499121399_DP
    REAL(DP), PARAMETER :: Q(3,12)=RESHAPE([A,B,B,B,A,B,B,B,A, D,E,E,E,D,E,E,E,D, &
                                      F,G,H,F,H,G,G,F,H,G,H,F,H,F,G,H,G,F],[3,12])
    REAL(DP), PARAMETER :: W(12)=[.116786275726379_DP,.116786275726379_DP,.116786275726379_DP, &
      .050844906370207_DP,.050844906370207_DP,.050844906370207_DP, &
      .082851075618374_DP,.082851075618374_DP,.082851075618374_DP, &
      .082851075618374_DP,.082851075618374_DP,.082851075618374_DP]
    REAL(DP) :: P(3,8),LAMBDA(3),DET,ZMIN,ZMAX
    INTEGER :: K,N,I,J
    V=0.0_DP
    ZMIN=MINVAL(Z); ZMAX=MAXVAL(Z)
    DO K=0,C%N+1
      IF(K>0) THEN
        IF(ZMAX<C%Z(K-1)) CYCLE
      END IF
      IF(K<=C%N) THEN
        IF(ZMIN>C%Z(K)) CYCLE
      END IF
      ! If all heights coincide with an interface, count exactly one strip.
      IF(ZMIN==ZMAX) THEN
        IF(K==0) THEN
          IF(ZMIN>=C%Z(0)) CYCLE
        ELSE IF(K<=C%N) THEN
          IF(ZMIN<C%Z(K-1) .OR. ZMIN>=C%Z(K)) CYCLE
        ELSE
          IF(ZMIN<C%Z(C%N)) CYCLE
        END IF
      END IF
      P=0.0_DP
      P(1,1)=1.0_DP; P(2,2)=1.0_DP; P(3,3)=1.0_DP
      N=3
      IF(K>0) CALL CLIP(P,N,Z,C%Z(K-1),.TRUE.)
      IF(K<=C%N) CALL CLIP(P,N,Z,C%Z(K),.FALSE.)
      DO I=2,N-1
        DET=ABS((P(2,I)-P(2,1))*(P(3,I+1)-P(3,1))-(P(3,I)-P(3,1))*(P(2,I+1)-P(2,1)))
        DO J=1,12
          LAMBDA=Q(1,J)*P(:,1)+Q(2,J)*P(:,I)+Q(3,J)*P(:,I+1)
          V=V+DET*W(J)*LAMBDA(SOURCE)*PJ44_VALUE(C,DOT_PRODUCT(LAMBDA,Z),.FALSE.)
        END DO
      END DO
    END DO
  END FUNCTION CAP

  ! Output is +int_V grad_h(P) dV, without g/reference-density scaling.
  ! FVCOM subtracts this quantity in momentum. Both vertex orders work.
  SUBROUTINE PJ44_PRISM(XY,C,FORCE,STATUS)
    REAL(DP), INTENT(IN) :: XY(2,3)
    TYPE(PJ44_PROFILE), INTENT(IN) :: C(3)
    REAL(DP), INTENT(OUT) :: FORCE(2,C(1)%N)
    INTEGER, INTENT(OUT) :: STATUS
    REAL(DP) :: GRAD(2,3),TWICE_AREA,AREA,JFACE(0:C(1)%N),Z(3)
    REAL(DP) :: EDGE(C(1)%N),NORMAL(2),GLO(2),GHI(2)
    INTEGER :: I,J,K,L,A,B
    FORCE=0.0_DP
    STATUS=1
    IF(C(2)%N/=C(1)%N .OR. C(3)%N/=C(1)%N) RETURN
    TWICE_AREA=(XY(1,2)-XY(1,1))*(XY(2,3)-XY(2,1))-(XY(2,2)-XY(2,1))*(XY(1,3)-XY(1,1))
    IF(.NOT.IEEE_IS_FINITE(TWICE_AREA)) RETURN
    IF(ABS(TWICE_AREA)<=TINY(1.0_DP)) RETURN
    AREA=0.5_DP*ABS(TWICE_AREA)
    DO I=1,3
      J=MOD(I,3)+1; K=MOD(I+1,3)+1
      GRAD(:,I)=[XY(2,J)-XY(2,K),XY(1,K)-XY(1,J)]/TWICE_AREA
    END DO
    DO K=0,C(1)%N
      DO I=1,3
        Z(I)=C(I)%Z(K)
      END DO
      JFACE(K)=0.0_DP
      DO I=1,3
        JFACE(K)=JFACE(K)+AREA*CAP(C(I),I,Z)
      END DO
    END DO
    ! Subtract the same surface pressure B(x) from the whole column.
    ! This enforces P(x,eta(x))=0 without duplicating FVCOM's g*grad(eta).
    JFACE=JFACE-JFACE(C(1)%N)
    DO I=1,3
      J=MOD(I,3)+1
      A=I; B=J
      ! Canonical edge order makes the scalar identical in neighbour cells.
      IF(XY(1,A)>XY(1,B) .OR. (XY(1,A)==XY(1,B) .AND. XY(2,A)>XY(2,B))) THEN
        A=J; B=I
      END IF
      CALL PJ44_EDGE(C(A),C(B),EDGE)
      NORMAL=[XY(2,J)-XY(2,I),XY(1,I)-XY(1,J)]*SIGN(1.0_DP,TWICE_AREA)
      DO K=1,C(1)%N
        FORCE(:,K)=FORCE(:,K)+NORMAL*EDGE(K)
      END DO
    END DO
    DO K=1,C(1)%N
      GLO=0.0_DP; GHI=0.0_DP
      DO I=1,3
        GLO=GLO+GRAD(:,I)*C(I)%Z(K-1)
        GHI=GHI+GRAD(:,I)*C(I)%Z(K)
      END DO
      FORCE(:,K)=FORCE(:,K)+GLO*JFACE(K-1)-GHI*JFACE(K)
    END DO
    IF(.NOT.ALL(IEEE_IS_FINITE(FORCE))) RETURN
    STATUS=0
  END SUBROUTINE PJ44_PRISM
END MODULE MOD_PJ44

SUBROUTINE BAROPG_PJ44
  USE ALL_VARS, ONLY: SP,DP,N,KB,KBM1,NV,VX,VY,ET,DT,Z,RHO1,RMEAN1, &
    ART,DT1,DZ1,GRAV_E,RAMP,DRHOX,DRHOY
#if defined (ICE_EMBEDDING)
  USE ALL_VARS, ONLY: QTHICE
#endif
  USE MOD_WD
  USE MOD_UTILS, ONLY: FATAL_ERROR
  USE MOD_PJ44
  IMPLICIT NONE
  TYPE(PJ44_PROFILE) :: C(3)
  REAL(DP) :: HEIGHT(0:KBM1),Q(KBM1),FORCE(2,KBM1),XY(2,3),VOLUME,MODEL_VOLUME,AREA
  INTEGER :: I,J,K,L,NODE,STATUS
#if defined (ICESHELF)
  CALL FATAL_ERROR('sigma levels pj44 does not support ICESHELF')
  RETURN
#endif
#if defined (SPHERICAL)
  CALL FATAL_ERROR('sigma levels pj44 currently requires a Cartesian mesh')
  RETURN
#endif
  DRHOX=0.0_SP
  DRHOY=0.0_SP
  DO I=1,N
#if defined (WET_DRY)
    IF(ISWETCT(I)*ISWETC(I)/=1) CYCLE
#endif
    DO J=1,3
      NODE=NV(I,J)
      XY(:,J)=[REAL(VX(NODE),DP)-REAL(VX(NV(I,1)),DP), &
                REAL(VY(NODE),DP)-REAL(VY(NV(I,1)),DP)]
      DO K=0,KBM1
        HEIGHT(K)=REAL(ET(NODE),DP)+REAL(DT(NODE),DP)*REAL(Z(NODE,KB-K),DP)
#if defined (ICE_EMBEDDING)
        HEIGHT(K)=HEIGHT(K)-REAL(QTHICE(NODE),DP)
#endif
      END DO
      DO K=1,KBM1
        L=KB-K
        Q(K)=1000.0_DP*(REAL(RHO1(NODE,L),DP)-REAL(RMEAN1(NODE,L),DP))
      END DO
      CALL PJ44_BUILD(HEIGHT,Q,C(J),STATUS)
      IF(STATUS/=0) CALL FATAL_ERROR('Invalid wet density column in BAROPG_PJ44')
    END DO
    AREA=0.5_DP*ABS(XY(1,2)*XY(2,3)-XY(2,2)*XY(1,3))
    IF(ABS(AREA-REAL(ART(I),DP))>2.0E-5_DP*MAX(AREA,REAL(ART(I),DP))) &
      CALL FATAL_ERROR('PJ44 requires Cartesian coordinates consistent with ART')
    ! Reject geometry inconsistent with FVCOM's momentum control volumes.
    DO K=1,KBM1
      VOLUME=REAL(ART(I),DP)*SUM([C(1)%Z(K)-C(1)%Z(K-1), &
          C(2)%Z(K)-C(2)%Z(K-1),C(3)%Z(K)-C(3)%Z(K-1)])/3.0_DP
      MODEL_VOLUME=REAL(ART(I),DP)*REAL(DT1(I),DP)*REAL(DZ1(I,KB-K),DP)
      IF(ABS(VOLUME-MODEL_VOLUME)>2.0E-5_DP*MAX(ABS(VOLUME),ABS(MODEL_VOLUME))) &
        CALL FATAL_ERROR('PJ44 requires consistent nodal and element layer volumes')
    END DO
    CALL PJ44_PRISM(XY,C,FORCE,STATUS)
    IF(STATUS/=0) CALL FATAL_ERROR('Invalid prism or pressure force in BAROPG_PJ44')
    DO K=1,KBM1
      L=KB-K
      DRHOX(I,L)=REAL(FORCE(1,K)*REAL(GRAV_E(I),DP)*0.001_DP*REAL(RAMP,DP),SP)
      DRHOY(I,L)=REAL(FORCE(2,K)*REAL(GRAV_E(I),DP)*0.001_DP*REAL(RAMP,DP),SP)
    END DO
  END DO
END SUBROUTINE BAROPG_PJ44
