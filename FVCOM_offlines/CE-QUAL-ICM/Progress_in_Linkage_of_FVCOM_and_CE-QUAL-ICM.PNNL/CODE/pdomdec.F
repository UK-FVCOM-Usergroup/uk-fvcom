!==============================================================================|
!  SET UP LOCAL PHYSICAL DOMAIN (CONNECTIVITY/MESH)                            |
!==============================================================================|

   SUBROUTINE PDOMDEC

!==============================================================================!
   USE ALL_VARS
#  if defined (MULTIPROCESSOR)
   USE MOD_PAR  
#  endif
   USE SIZES

   IMPLICIT NONE
   INTEGER I,EGL,J,IERR,I1,I2  

!==============================================================================|
!  GENERATE LOCAL NODE CONNECTIVITY (NV) FROM GLOBAL NODE CONNECTIVITY (NVG)   |
!  USING LOCAL TO GLOBAL MAPPING FOR INTERIOR ELEMENTS (EGID)                  |
!  AND LOCAL TO GLOBAL MAPPING FOR HALO ELEMENTS (HE_LST)                      |
!==============================================================================|
   ALLOCATE(NV(0:NTLOC,4));    NV = 0  !!NODE NUMBERING FOR ELEMENTS
   IF(SERIAL) NV = NVG

#  if defined (MULTIPROCESSOR)
   IF(PAR)THEN
     DO I=1,NLOC
       EGL = EGID(I)
       NV(I,1:4) = NLID(NVG(EGID(I),1:4))
     END DO
     DO I=1,NHE
       EGL = HE_LST(I)
       NV(I+NLOC,1:4) = NLID_X(NVG(EGL,1:4))
     END DO
   END IF
#  endif

!==============================================================================|
!   SET UP LOCAL MESH (HORIZONTAL COORDINATES)                                 |
!==============================================================================|

!--------------CALCULATE GLOBAL MINIMUMS AND MAXIMUMS--------------------------!

   VXMIN = MINVAL(XG(1:MGL)) ; VXMAX = MAXVAL(XG(1:MGL))
   VYMIN = MINVAL(YG(1:MGL)) ; VYMAX = MAXVAL(YG(1:MGL))

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

   XCG(0) = 0.0_SP ; YCG(0) = 0.0_SP


!--------------TRANSFORM TO LOCAL DOMAINS IF PARALLEL--------------------------!

   ALLOCATE(VX(0:MTLOC));        VX   = ZERO   !!X-COORD AT GRID POINT   
   ALLOCATE(VY(0:MTLOC));        VY   = ZERO   !!X-COORD AT GRID POINT   
   IF(SERIAL)THEN
     VX = XG
     VY = YG
   END IF

#  if defined (MULTIPROCESSOR)
   IF(PAR)THEN
     DO I=1,MLOC
       VX(I) = XG(NGID(I))
       VY(I) = YG(NGID(I))
     END DO

     DO I=1,NHN
       VX(I+MLOC) = XG(HN_LST(I))
       VY(I+MLOC) = YG(HN_LST(I))
     END DO
   END IF
# endif

!==============================================================================|
!   SET UP LOCAL MESH (BATHYMETRIC DEPTH)                                      |
!==============================================================================|

!--------------TRANSFORM TO LOCAL DOMAINS IF PARALLEL--------------------------!
   ALLOCATE(H(0:MTLOC));       H = ZERO       !!BATHYMETRIC DEPTH
   IF(SERIAL) H = HG

#  if defined (MULTIPROCESSOR)
   IF(PAR)THEN
     DO I=1,MLOC
       H(I)   = HG(NGID(I))
     END DO
     DO I=1,NHN
       H(I+MLOC) = HG(HN_LST(I))
     END DO
   END IF
#  endif

!--------------CALCULATE EXTREMUMS---------------------------------------------!

   HMAX = MAXVAL(ABS(HG(1:MGL)))
   HMIN = MINVAL(HG(1:MGL))

!==============================================================================|
!   COMPUTE FACE CENTER VALUES FOR GRID, DEPTH, AND CORIOLIS PARAMETER         |
!==============================================================================|

   ALLOCATE(XC(0:NTLOC))       ;XC   = ZERO   !!X-COORD AT FACE CENTER
   ALLOCATE(YC(0:NTLOC))       ;YC   = ZERO   !!X-COORD AT FACE CENTER
   ALLOCATE(H1(0:NTLOC))       ;H1   = ZERO   !!BATHYMETRIC DEPTH
   DO I=1,NTLOC
     XC(I)  = (VX(NV(I,1)) + VX(NV(I,2)) + VX(NV(I,3)))/3.0_SP
     YC(I)  = (VY(NV(I,1)) + VY(NV(I,2)) + VY(NV(I,3)))/3.0_SP
     H1(I)  = SUM( H(NV(I,1:3)))/3.0_SP
   END DO

   RETURN
   END SUBROUTINE PDOMDEC
!==============================================================================|
