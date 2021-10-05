!==============================================================================|
!  SET UP LOCAL PHYSICAL DOMAIN (CONNECTIVITY/MESH)                            |
!==============================================================================|

      SUBROUTINE PDOMDEC

!==============================================================================!
      USE ALL_VARS
      USE MOD_RCA
      USE MOD_TYPES
#  if defined (MULTIPROCESSOR)
      USE MOD_PAR  
#  endif
      IMPLICIT NONE
!      INCLUDE 'RCACM'
      INTEGER I,EGL,K

!==============================================================================|
!  GENERATE LOCAL NODE CONNECTIVITY (NV) FROM GLOBAL NODE CONNECTIVITY (NVG)   |
!  USING LOCAL TO GLOBAL MAPPING FOR INTERIOR ELEMENTS (EGID)                  |
!  AND LOCAL TO GLOBAL MAPPING FOR HALO ELEMENTS (HE_LST)                      |
!==============================================================================|

      ALLOCATE(NV(0:NT,4));    NV = 0
      IF(SERIAL) NV = NVG

#  if defined (MULTIPROCESSOR)
      IF(PAR)THEN
       DO I=1,N
        EGL = EGID(I)
        NV(I,1:4) = NLID(NVG(EGID(I),1:4))
       END DO
       DO I=1,NHE
        EGL = HE_LST(I)
        NV(I+N,1:4) = NLID_X(NVG(EGL,1:4))
       END DO
      END IF
#  endif

!==============================================================================|
!   SET UP LOCAL MESH (HORIZONTAL COORDINATES)                                 |
!==============================================================================|

!--------------CALCULATE GLOBAL MINIMUMS AND MAXIMUMS--------------------------!

      VXMIN = MINVAL(XG(1:NXY)) ; VXMAX = MAXVAL(XG(1:NXY))
      VYMIN = MINVAL(YG(1:NXY)) ; VYMAX = MAXVAL(YG(1:NXY))

!--------------SHIFT GRID TO UPPER RIGHT CARTESIAN-----------------------------!

      XG = XG - VXMIN
      YG = YG - VYMIN
      XG(0) = 0.0_SP ; YG(0) = 0.0_SP

!--------------CALCULATE GLOBAL ELEMENT CENTER GRID COORDINATES----------------!

      ALLOCATE(XCG(0:EXY),YCG(0:EXY)) ; XCG = 0.0_SP ; YCG = 0.0_SP
      DO I=1,EXY   
       XCG(I)  = (XG(NVG(I,1)) + XG(NVG(I,2)) + XG(NVG(I,3)))/3.0_SP
       YCG(I)  = (YG(NVG(I,1)) + YG(NVG(I,2)) + YG(NVG(I,3)))/3.0_SP
      END DO
      XCG(0) = 0.0_SP ; YCG(0) = 0.0_SP

!--------------TRANSFORM TO LOCAL DOMAINS IF PARALLEL--------------------------!

      ALLOCATE(VX(0:MT),VY(0:MT));     VX = 0.0;    VY = 0.0
      IF(SERIAL)THEN
       VX = XG
       VY = YG
      END IF

#  if defined (MULTIPROCESSOR)
      IF(PAR)THEN
       DO I=1,M
        VX(I) = XG(NGID(I))
        VY(I) = YG(NGID(I))
       END DO

       DO I=1,NHN
         VX(I+M) = XG(HN_LST(I))
         VY(I+M) = YG(HN_LST(I))
       END DO
      END IF
# endif

!==============================================================================|
!   SET UP LOCAL MESH (BATHYMETRIC DEPTH)                                      |
!==============================================================================|

!--------------TRANSFORM TO LOCAL DOMAINS IF PARALLEL--------------------------!

      ALLOCATE(H(0:MT));      H = 0.0
      IF(SERIAL) H = HG

#  if defined (MULTIPROCESSOR)
      IF(PAR)THEN
       DO I=1,M
        H(I)   = HG(NGID(I))
       END DO
       DO I=1,NHN
        H(I+M) = HG(HN_LST(I))
       END DO
      END IF
#  endif

!--------------CALCULATE EXTREMUMS---------------------------------------------!

      HMAX = MAXVAL(ABS(HG(1:NXY)))
      HMIN = MINVAL(HG(1:NXY))

!==============================================================================|
!   COMPUTE FACE CENTER VALUES FOR GRID, DEPTH, AND CORIOLIS PARAMETER         |
!==============================================================================|

      ALLOCATE(XC(0:NT),YC(0:NT));  XC = 0.0;  YC = 0.0
      ALLOCATE(HC(0:NT));           HC = 0.0
      DO I=1,NT
!       XC(I)  = SUM(VX(NV(I,1:3)))/3.0
       XC(I)  = (VX(NV(I,1)) + VX(NV(I,2)) + VX(NV(I,3)))/3.0_SP
       YC(I)  = (VY(NV(I,1)) + VY(NV(I,2)) + VY(NV(I,3)))/3.0_SP
!       YC(I)  = SUM(VY(NV(I,1:3)))/3.0
!JQI       HC(I)  = SUM( H(NV(I,1:3)))/3.0_SP
       HC(I)  = (H(NV(I,1)) + H(NV(I,2)) + H(NV(I,3)))/3.0_SP
      END DO

!==============================================================================|
!   COMPUTE DERIVATIVE AND INTRA-SIGMA LEVELS                                  |
!==============================================================================|

!--------------TRANSFORM TO LOCAL DOMAINS IF PARALLEL--------------------------!

      ALLOCATE(Z(0:MT,KB));     Z = 0.0
      IF(SERIAL) Z = ZG

#  if defined (MULTIPROCESSOR)
      IF(PAR)THEN
       DO I=1,M
        Z(I,:)   = ZG(NGID(I),:)
       END DO
       DO I=1,NHN
        Z(I+M,:) = ZG(HN_LST(I),:)
       END DO
      END IF
#  endif

!--Compute derivative and intra-sigma levels
      ALLOCATE(ZZ(0:MT,KB),DZ(0:MT,KB));  ZZ = 0.0;  DZ = 0.0
      DO K=1,KBM1
       ZZ(:,K)=0.5*(Z(:,K)+Z(:,K+1))
       DZ(:,K)=Z(:,K)-Z(:,K+1)
      END DO
      ZZ(:,KB)=2.0*ZZ(:,KBM1)-ZZ(:,KBM2)

      ALLOCATE(DZZ(0:MT,KB));  DZZ = 0.0
      DO K=1,KBM2
       DZZ(:,K)=ZZ(:,K)-ZZ(:,K+1)
      END DO
      DZZ(:,KB-1)=0.0
      DZ(:,KB)=0.0
      
      DZ(0,:) = DZ(1,:)

      ALLOCATE(DZ1(0:EXY,NZ+1))

      DZ1(1:NT,:) = (DZ(NV(1:NT,1),:)+DZ(NV(1:NT,2),:)+DZ(NV(1:NT,3),:))/3.0

      RETURN
      END SUBROUTINE PDOMDEC
!==============================================================================|
