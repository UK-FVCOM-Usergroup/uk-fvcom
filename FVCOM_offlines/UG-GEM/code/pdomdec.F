!==============================================================================|
!  SET UP LOCAL PHYSICAL DOMAIN (CONNECTIVITY/MESH)                            |
!==============================================================================|

   SUBROUTINE PDOMDEC

!==============================================================================!
   USE ALL_VARS
#  if defined (MULTIPROCESSOR)
   USE MOD_PAR  
#  endif
#  if defined (SPHERICAL)
   USE MOD_SPHERICAL
#  endif
   IMPLICIT NONE
   INTEGER I,EGL,J,IERR,I1,I2,K
#  if defined (SPHERICAL)
   INTEGER ITMP
   REAL(DP) VX1,VY1,VX2,VY2,VX3,VY3,EVX12,EVX13,EVX23,&
            EVY12,EVY13,EVY23,EVXY,VX12,VY12,VX23,VY23,VX31,VY31,&
	    X1_DP,Y1_DP,X2_DP,Y2_DP,DTMP_DP
#  endif

!==============================================================================|
!  GENERATE LOCAL NODE CONNECTIVITY (NV) FROM GLOBAL NODE CONNECTIVITY (NVG)   |
!  USING LOCAL TO GLOBAL MAPPING FOR INTERIOR ELEMENTS (EGID)                  |
!  AND LOCAL TO GLOBAL MAPPING FOR HALO ELEMENTS (HE_LST)                      |
!==============================================================================|

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

#  if defined (SPHERICAL)
   VXMIN = 0.0_SP ; VXMAX = MAXVAL(XG(1:MGL))
   VYMIN = 0.0_SP ; VYMAX = MAXVAL(YG(1:MGL))
#  else
   VXMIN = MINVAL(XG(1:MGL)) ; VXMAX = MAXVAL(XG(1:MGL))
   VYMIN = MINVAL(YG(1:MGL)) ; VYMAX = MAXVAL(YG(1:MGL))
#  endif

!--------------SHIFT GRID TO UPPER RIGHT CARTESIAN-----------------------------!

   XG = XG - VXMIN
   YG = YG - VYMIN
   XG(0) = 0.0_SP ; YG(0) = 0.0_SP

!--------------CALCULATE GLOBAL ELEMENT CENTER GRID COORDINATES----------------!

   ALLOCATE(XCG(0:NGL),YCG(0:NGL)) ; XCG = 0.0_SP ; YCG = 0.0_SP
   DO I=1,NGL   
     XCG(I)  = (XG(NVG(I,1)) + XG(NVG(I,2)) + XG(NVG(I,3)))/3.0_SP
     YCG(I)  = (YG(NVG(I,1)) + YG(NVG(I,2)) + YG(NVG(I,3)))/3.0_SP
   END DO
# if defined (SPHERICAL)
   DO I=1,NGL
     VX1=XG(NVG(I,1))
     VY1=YG(NVG(I,1))
     VX2=XG(NVG(I,2))
     VY2=YG(NVG(I,2))
     VX3=XG(NVG(I,3))
     VY3=YG(NVG(I,3))

     DO K=1,1000000

       EVX12=VX2-VX1
       EVX13=VX3-VX1
       EVX23=VX3-VX2

       IF(EVX12 >  180.0_SP)THEN
         EVX12 = -360.0_SP+EVX12
       ELSE IF(EVX12 < -180.0_SP)THEN
         EVX12 =  360.0_SP+EVX12
       END IF
       IF(EVX13 >  180.0_SP)THEN
	 EVX13 = -360.0_SP+EVX13
       ELSE IF(EVX13 < -180.0_SP)THEN
	 EVX13 =  360.0_SP+EVX13
       END IF
       IF(EVX23 >  180.0_SP)THEN
         EVX23 = -360.0_SP+EVX23
       ELSE IF(EVX23 < -180.0_SP)THEN
         EVX23 =  360.0_SP+EVX23
       END IF

       EVX12=ABS(EVX12)
       EVX13=ABS(EVX13)
       EVX23=ABS(EVX23)

       EVY12=ABS(VY2-VY1)
       EVY13=ABS(VY3-VY1)
       EVY23=ABS(VY3-VY2)

       EVXY=1.E-10_SP

       IF((EVX12 < EVXY) .AND.(EVX13 < EVXY) .AND. (EVX23 < EVXY) &
          .AND.(EVY12 < EVXY) .AND. (EVY13 < EVXY)                &
          .AND.(EVY23 < EVXY))THEN
         XCG(I)=VX1
         YCG(I)=VY1
         GOTO 57
       ELSE
         CALL ARCC(VX1,VY1,VX2,VY2,VX12,VY12)
         CALL ARCC(VX2,VY2,VX3,VY3,VX23,VY23)
         CALL ARCC(VX3,VY3,VX1,VY1,VX31,VY31)

         VX1=VX12
         VY1=VY12
         VX2=VX23
         VY2=VY23
         VX3=VX31
         VY3=VY31
       END IF
     END DO
57   CONTINUE
   END DO
# endif

   XCG(0) = 0.0_SP ; YCG(0) = 0.0_SP


!--------------TRANSFORM TO LOCAL DOMAINS IF PARALLEL--------------------------!

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
#  endif

!==============================================================================|
!   SET UP LOCAL MESH (BATHYMETRIC DEPTH)                                      |
!==============================================================================|

!--------------TRANSFORM TO LOCAL DOMAINS IF PARALLEL--------------------------!

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

!==============================================================================|
!   COMPUTE FACE CENTER VALUES FOR GRID, DEPTH, AND CORIOLIS PARAMETER         |
!==============================================================================|

#  if defined (SPHERICAL)
   IF(SERIAL) XC = XCG; YC = YCG

#  if defined (MULTIPROCESSOR)
   IF(PAR)THEN
     DO I=1,N
       XC(I) = XCG(EGID(I))
       YC(I) = YCG(EGID(I))
     ENDDO
     DO I=1,NHE
       XC(I+N) = XCG(HE_LST(I))
       YC(I+N) = YCG(HE_LST(I))
     END DO
   END IF
#  endif
   DO I=1,NT
     H1(I)  = SUM( H(NV(I,1:3)))/3.0_SP
   END DO
# else
   DO I=1,NT
     XC(I)  = (VX(NV(I,1)) + VX(NV(I,2)) + VX(NV(I,3)))/3.0_SP
     YC(I)  = (VY(NV(I,1)) + VY(NV(I,2)) + VY(NV(I,3)))/3.0_SP
     H1(I)  = SUM( H(NV(I,1:3)))/3.0_SP
   END DO
#  endif

!==============================================================================|
!   COMPUTE DERIVATIVE AND INTRA-SIGMA LEVELS                                  |
!==============================================================================|

!--------------TRANSFORM TO LOCAL DOMAINS IF PARALLEL--------------------------!

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
   DO K=1,KBM1
     ZZ(:,K)=0.5*(Z(:,K)+Z(:,K+1))
     DZ(:,K)=Z(:,K)-Z(:,K+1)
   END DO
   ZZ(:,KB)=2.0*ZZ(:,KBM1)-ZZ(:,KBM2)

   DO K=1,KBM2
     DZZ(:,K)=ZZ(:,K)-ZZ(:,K+1)
   END DO
   DZZ(:,KB-1)=0.0
   DZ(:,KB)=0.0
      
   DZ(0,:) = DZ(1,:)

   DZ1(1:NT,:) = (DZ(NV(1:NT,1),:)+DZ(NV(1:NT,2),:)+DZ(NV(1:NT,3),:))/3.0

   RETURN
   END SUBROUTINE PDOMDEC
!==============================================================================|
