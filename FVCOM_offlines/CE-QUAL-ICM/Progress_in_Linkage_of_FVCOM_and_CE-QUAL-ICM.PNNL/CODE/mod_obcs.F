MODULE MOD_OBCS
   USE MOD_PREC
   IMPLICIT NONE
   SAVE

!--Open Boundary Types, Lists, Pointers
   INTEGER               :: IOBCN_GL         !!GLOBAL NUMBER OF OPEN BOUNDARY NODES
   INTEGER               :: IOBCN            !!LOCAL NUMBER OF OPEN BOUNDARY NODES
   INTEGER,  ALLOCATABLE :: I_OBC_GL(:)      !!GLOBAL ID OF OPEN BOUNDARY NODES
   INTEGER,  ALLOCATABLE :: I_OBC_N(:)       !!OPEN BOUNDARY NODE LIST
   INTEGER,  ALLOCATABLE :: NEXT_OBC(:)      !!INTERIOR NEIGHBOR OF OPEN BOUNDARY NODE
   INTEGER,  ALLOCATABLE :: NEXT_OBC2(:)     !!INTERIOR NEIGHBOR OF NEXT_OBC
   INTEGER,  ALLOCATABLE :: TYPE_OBC(:)      !!OUTER BOUNDARY NODE TYPE (FOR SURFACE ELEVATION)
   INTEGER,  ALLOCATABLE :: TYPE_OBC_GL(:)   !!OUTER BOUNDARY NODE TYPE (FOR SURFACE ELEVATION)
   INTEGER               :: IBCN(5)          !!NUMBER OF EACH TYPE OF OBN IN LOCAL  DOM
   INTEGER               :: IBCN_GL(5)       !!NUMBER OF EACH TYPE OF OBN IN GLOBAL DOM
   INTEGER,  ALLOCATABLE :: OBC_LST(:,:)     !!MAPPING OF OPEN BOUNDARY ARRAYS TO EACH TYPE
   INTEGER,  ALLOCATABLE :: NADJN_OBC(:)     !!NUMBER OF ADJACENT OPEN BOUNDARY NODES TO OBN
   INTEGER,  ALLOCATABLE :: ADJN_OBC(:,:)    !!ADJACENT OBN's of OBN
   INTEGER,  ALLOCATABLE :: NADJC_OBC(:)     !!NUMBER OF ADJACENT OPEN BOUNDARY CELLS TO OBN
   INTEGER,  ALLOCATABLE :: ADJC_OBC(:,:)    !!ADJACENT OPEN BOUNDARY CELLS

!--Open Boundary Metrics
   INTEGER,  ALLOCATABLE :: NFLUXF_OBC(:)    !!NUMBER OF FLUX SEGMENTS TO OBN
   REAL, ALLOCATABLE :: FLUXF_OBC(:,:)   !!FLUX FRACTION ON EACH SIDE OF OBN
   REAL, ALLOCATABLE :: NXOBC(:)         !!INWARD POINTING X-NORMAL OF OBN
   REAL, ALLOCATABLE :: NYOBC(:)         !!INWARD POINTING Y-NORMAL OF OBN
   REAL, ALLOCATABLE :: DLTN_OBC(:)      !!DISTANCE BETWEEN NEXT_OBC AND OBN NORMAL TO BOUNDARY
! tykim
   REAL, ALLOCATABLE :: NUT_OBC(:,:)     !!NUTRIENT AT OPEN BOUNDARY
   REAL, ALLOCATABLE :: NUT_OBC_GL(:,:)  !!DISTANCE BETWEEN NEXT_OBC AND OBN NORMAL TO BOUNDARY

   CONTAINS


!==========================================================================|
   SUBROUTINE ALLOC_OBC_DATA

!--------------------------------------------------------------------------|
!  ALLOCATE AND INITIALIZE SURFACE ELEVATION ARRAYS FOR                    |
!  TIME STEPS N-1 AND N-2                                                  |
!--------------------------------------------------------------------------|

   USE ALL_VARS
   IMPLICIT NONE


   RETURN
   END SUBROUTINE ALLOC_OBC_DATA
!==========================================================================|

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

!==============================================================================|
   SUBROUTINE SEPARATE_OBC

!------------------------------------------------------------------------------|
! Accumulate separately the amounts of nodes for 11 types of open boundaries   |
!------------------------------------------------------------------------------|
   USE ALL_VARS
   IMPLICIT NONE

   INTEGER :: I,I1,I2,I3,I4,I5,II,J

   IBCN = 0
   IBCN_GL = 0

   DO I = 1, IOBCN_GL
     IF(TYPE_OBC_GL(I) == 1 .OR. TYPE_OBC_GL(I) == 2)  IBCN_GL(1) = IBCN_GL(1) + 1
     IF(TYPE_OBC_GL(I) == 3 .OR. TYPE_OBC_GL(I) == 4)  IBCN_GL(2) = IBCN_GL(2) + 1
     IF(TYPE_OBC_GL(I) == 5 .OR. TYPE_OBC_GL(I) == 6)  IBCN_GL(3) = IBCN_GL(3) + 1
     IF(TYPE_OBC_GL(I) == 7 .OR. TYPE_OBC_GL(I) == 8)  IBCN_GL(4) = IBCN_GL(4) + 1
     IF(TYPE_OBC_GL(I) == 9 .OR. TYPE_OBC_GL(I) == 10) IBCN_GL(5) = IBCN_GL(5) + 1
   END DO

   DO I = 1, IOBCN
     IF(TYPE_OBC(I) == 1 .OR. TYPE_OBC(I) == 2)  IBCN(1) = IBCN(1) + 1
     IF(TYPE_OBC(I) == 3 .OR. TYPE_OBC(I) == 4)  IBCN(2) = IBCN(2) + 1
     IF(TYPE_OBC(I) == 5 .OR. TYPE_OBC(I) == 6)  IBCN(3) = IBCN(3) + 1
     IF(TYPE_OBC(I) == 7 .OR. TYPE_OBC(I) == 8)  IBCN(4) = IBCN(4) + 1
     IF(TYPE_OBC(I) == 9 .OR. TYPE_OBC(I) == 10) IBCN(5) = IBCN(5) + 1
   END DO

   I1 = 0
   I2 = 0
   I3 = 0
   I4 = 0
   I5 = 0


   ALLOCATE(OBC_LST(5,MAXVAL(IBCN))) ; OBC_LST = 0

   DO I=1,IOBCN
     IF(TYPE_OBC(I) == 1 .OR. TYPE_OBC(I) == 2)THEN
       I1 = I1 + 1
       OBC_LST(1,I1) = I
     ELSE IF(TYPE_OBC(I) == 3 .OR. TYPE_OBC(I) == 4)THEN
       I2 = I2 + 1
       OBC_LST(2,I2) = I
     ELSE IF(TYPE_OBC(I) == 5 .OR. TYPE_OBC(I) == 6)THEN
       I3 = I3 + 1
       OBC_LST(3,I3) = I
     ELSE IF(TYPE_OBC(I) == 7 .OR. TYPE_OBC(I) == 8)THEN
       I4 = I4 + 1
       OBC_LST(4,I4) = I
     ELSE IF(TYPE_OBC(I) == 9 .OR. TYPE_OBC(I) == 10)THEN
       I5 = I5 + 1
       OBC_LST(5,I5) = I
     END IF
   END DO

   RETURN
   END SUBROUTINE SEPARATE_OBC
!==============================================================================|

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

!==============================================================================|
   SUBROUTINE SETUP_OBC

!------------------------------------------------------------------------------!
   USE ALL_VARS
# if defined (MULTIPROCESSOR)
   USE MOD_PAR  
# endif
   IMPLICIT NONE

   REAL(SP) :: DXC,DYC,DXN,DYN,CROSS,E1,E2,DOTMAX,DOT,DX,DY,DXN_TMP,DYN_TMP
   INTEGER  :: I,J,JJ,INODE,JNODE,I1,I2,IC,N1,N2,N3
   LOGICAL  :: DEBUG

   REAL(SP), ALLOCATABLE :: NXOBC_TMP(:),NYOBC_TMP(:)

!------------------------------------------------------------------------------!

!--Determine Adjacent Open Boundary Points-------------------------------------!

   ALLOCATE(NADJN_OBC(IOBCN))  ; NADJN_OBC = 0
   ALLOCATE(ADJN_OBC(IOBCN,2)) ; ADJN_OBC = 0

   ALLOCATE(NEXT_OBC(IOBCN))           ;NEXT_OBC   = 0
   ALLOCATE(NEXT_OBC2(IOBCN))          ;NEXT_OBC2  = 0

!   CALL ALLOC_OBC_DATA

   DO I=1,IOBCN
     INODE = I_OBC_N(I)
     DO J=1,NTSN(INODE)-1
       JNODE = NBSN(INODE,J)
       IF(ISONB(JNODE) == 2 .AND. INODE /= JNODE)THEN
         NADJN_OBC(I) = NADJN_OBC(I) + 1
         ADJN_OBC(I,NADJN_OBC(I)) = JNODE
       END IF
     END DO
   END DO


   DO I=1,IOBCN
     IF(NADJN_OBC(I) == 0)THEN
       WRITE(*,*)'NO ADJACENT NODE FOUND FOR BOUNDARY NODE',I
       WRITE(*,*)'IN PROCESSOR',MYID
       CALL PSTOP
     END IF
   END DO

!--Determine Adjacent Cells-(Nonlinear Only)-----------------------------------!
!--Simultaneously Determine INWARD Pointing Normal NXOBC,NYOBC                 !

   ALLOCATE(NADJC_OBC(IOBCN))  ; NADJC_OBC = 0
   ALLOCATE(ADJC_OBC(IOBCN,2)) ; ADJC_OBC = 0
   ALLOCATE(NXOBC(IOBCN)) ; NXOBC = 0
   ALLOCATE(NYOBC(IOBCN)) ; NYOBC = 0
   ALLOCATE(NXOBC_TMP(IOBCN)) ; NXOBC_TMP = 0
   ALLOCATE(NYOBC_TMP(IOBCN)) ; NYOBC_TMP = 0

   DO I=1,IOBCN
     I1 = I_OBC_N(I)

     !!Mark First Cell on Boundary Edge Adjacent to Node I
     I2 = ADJN_OBC(I,1)
     DO J = 1, NTVE(I1)
       IC = NBVE(I1,J)
       N1 = NV(IC,1) ; N2 = NV(IC,2) ; N3 = NV(IC,3)
       IF( N1-I2 == 0 .OR. N2-I2 == 0 .OR. N3-I2 == 0)THEN
         DXN = VX(I2)-VX(I1) ; DYN = VY(I2)-VY(I1)
         DXC = XC(IC)-VX(I1) ; DYC = YC(IC)-VY(I1)

         CROSS = SIGN(1.0_SP,DXC*DYN - DYC*DXN)
         NXOBC_TMP(I) =  CROSS*DYN/SQRT(DXN**2 +DYN**2)
         NYOBC_TMP(I) = -CROSS*DXN/SQRT(DXN**2 +DYN**2)
         NXOBC(I) = NXOBC_TMP(I)
         NYOBC(I) = NYOBC_TMP(I)
           NADJC_OBC(I) = NADJC_OBC(I) + 1
           ADJC_OBC(I,NADJC_OBC(I)) = IC
       END IF
     END DO

     IF(NADJN_OBC(I) > 1)THEN
     I2 = ADJN_OBC(I,2)
     DO J = 1, NTVE(I1)
       IC = NBVE(I1,J)
       N1 = NV(IC,1) ; N2 = NV(IC,2) ; N3 = NV(IC,3)
       IF( N1-I2 == 0 .OR. N2-I2 == 0 .OR. N3-I2 == 0)THEN
         DXN = VX(I2)-VX(I1) ; DYN = VY(I2)-VY(I1)
         DXC = XC(IC)-VX(I1) ; DYC = YC(IC)-VY(I1)

         CROSS = SIGN(1.0_SP,DXC*DYN - DYC*DXN)
         NXOBC_TMP(I) = NXOBC_TMP(I) + CROSS*DYN/SQRT(DXN**2 +DYN**2)
         NYOBC_TMP(I) = NYOBC_TMP(I) - CROSS*DXN/SQRT(DXN**2 +DYN**2)
         NXOBC(I) = NXOBC_TMP(I)/SQRT(NXOBC_TMP(I)**2 + NYOBC_TMP(I)**2)
         NYOBC(I) = NYOBC_TMP(I)/SQRT(NXOBC_TMP(I)**2 + NYOBC_TMP(I)**2)

           NADJC_OBC(I) = NADJC_OBC(I) + 1
           ADJC_OBC(I,NADJC_OBC(I)) = IC
       END IF
     END DO
     END IF
   END DO

   DEALLOCATE(NXOBC_TMP,NYOBC_TMP)

!--Determine 1st Layer Neighbor for Open Boundary Points-----------------------!
!--Node Chosen is Node That is Connected to OBC Node and is Oriented           !
!--Most Normal to the Boundary.  It is not Necessarily the Closest Node        !
!--Determine also DLTN_OBC, the normal component of the distance between       !
!--Next_obc and the open boundary node                                         !

   DO I=1,IOBCN
     DOTMAX =  -1.0
     INODE = I_OBC_N(I)
     DO J=1,NTSN(INODE)-1
       JNODE = NBSN(INODE,J)
       IF(ISONB(JNODE) /= 2 .AND. INODE /= JNODE)THEN
         DXN_TMP = VX(JNODE)-VX(INODE)
         DYN_TMP = VY(JNODE)-VY(INODE)

         DXN = DXN_TMP/SQRT(DXN_TMP**2 + DYN_TMP**2)
         DYN = DYN_TMP/SQRT(DXN_TMP**2 + DYN_TMP**2)
         DOT = DXN*NXOBC(I) + DYN*NYOBC(I)
         IF(DOT > DOTMAX)THEN
           DOTMAX = DOT
           NEXT_OBC(I) = JNODE
         END IF
       END IF
     END DO
   END DO

!--Determine DLTN_OBC----------------------------------------------------------!
   ALLOCATE(DLTN_OBC(IOBCN))
   DO I=1,IOBCN
     I1 = I_OBC_N(I)
     I2 = NEXT_OBC(I)

     DX = VX(I2)-VX(I1)
     DY = VY(I2)-VY(I1)
     DLTN_OBC(I) = ABS(DX*NXOBC(I) + DY*NYOBC(I))
   END DO

   RETURN
   END SUBROUTINE SETUP_OBC
!==============================================================================|
END MODULE MOD_OBCS
