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

!==============================================================================|
!     CALCULATE THE BAROCLINIC PRESSURE GRADIENT IN SIGMA COORDINATES          |
!==============================================================================|

   SUBROUTINE BAROPG 

!==============================================================================|
   USE ALL_VARS
   USE MOD_SPHERICAL
   USE MOD_NORTHPOLE
   USE MOD_WD
   USE MOD_UTILS, ONLY: FATAL_ERROR
   USE MOD_PAR, ONLY: NGID_X
# if defined (MULTIPROCESSOR)
   USE MOD_PAR, ONLY: AEXCHANGE, NC
# endif
   
# if defined (ICESHELF)
    USE MOD_ISF
# endif


   IMPLICIT NONE
   REAL(SP) :: RIJK(0:N,3,KBM1), DRIJK1(0:N,3,KBM1), DRIJK2(0:N,KBM1)
   REAL(SP) :: LOCAL_RHO(3,KBM1), LOCAL_RHO_C(KBM1)
   REAL(DP) :: LOCAL_Z(3,KBM1), LOCAL_RAW(3,KBM1), LOCAL_REF(3,KBM1)
   REAL(DP) :: LOCAL_TOP(3), LOCAL_BOTTOM(3)
   INTEGER :: PATCH_NODES(3+6*MX_NBR_ELEM), PATCH_COUNT, P, NODE_ID
   REAL(DP) :: PATCH_Z(3+6*MX_NBR_ELEM,KBM1), PATCH_RHO(3+6*MX_NBR_ELEM,KBM1)
   REAL(DP) :: PATCH_TOP(3+6*MX_NBR_ELEM), PATCH_BOTTOM(3+6*MX_NBR_ELEM)
   REAL(SP), ALLOCATABLE, TARGET :: REFERENCE_RHO(:,:)
   LOGICAL :: USE_LOCAL_REFERENCE, USE_NEIGHBOR_MEAN
   INTEGER :: LOCAL_STATUS
   REAL(SP) :: TEMP,DIJ,DRHO1,DRHO2
   INTEGER  :: I,K,J,J1,J2,IJK
#  if defined (SPHERICAL)
   REAL(SP) :: XTMP,XTMP1
#  endif

#  if defined (ICESHELF)                                
   REAL (SP) :: DRIJK3(0:N,KBM1)               !  DRIJK3 is the density difference in the triangle center
   REAL (SP) :: DRHO3,ZISFIJ                   !  DRHO3 is the extra term related to ICESHELF, ZISFIJ is the ice draft along the triangle edge
   REAL (SP) :: DRHO4,R1IJ                     !  DRHO4 is the extra term related to iceshelf pressure term                                    
                                               !   R1IJ  is the top layer density at each  triangle edge
   REAL (SP) ::  TMPZI(0:MT), TMPZI1(0:NT)     ! TMPZI TMPZI1 are temporary iceshelf drafts on nodes and cells respectively
#  endif
!==============================================================================|

   IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "Start: baropg.F"
   
   ! USE RAMP CALCULATED IN 'internal_step.F'

!----------SUBTRACT MEAN DENSITY TO MINIMIZE ROUNDOFF ERROR--------------------!

   USE_NEIGHBOR_MEAN = BAROCLINIC_PRESSURE_GRADIENT == 'sigma levels neighborhood mean'
   USE_LOCAL_REFERENCE = USE_NEIGHBOR_MEAN .OR. &
     BAROCLINIC_PRESSURE_GRADIENT == 'sigma levels deep reference'
   IF(USE_NEIGHBOR_MEAN .AND. PAR) THEN
     ! Refresh a PRIVATE density copy for halo donors. No global field is written.
     ALLOCATE(REFERENCE_RHO(0:MT,KB))
     REFERENCE_RHO=RHO1
# if defined (MULTIPROCESSOR)
     CALL AEXCHANGE(NC,MYID,NPROCS,REFERENCE_RHO)
# endif
   END IF
# if defined (ICESHELF)
   IF(USE_LOCAL_REFERENCE) CALL FATAL_ERROR('Local reference is not implemented for ICESHELF')
# endif
   IF(.NOT.USE_LOCAL_REFERENCE) THEN
     RHO1(:,1:KBM1) = RHO1(:,1:KBM1) - RMEAN1(:,1:KBM1)
     RHO = RHO - RMEAN
   END IF

!----------INITIALIZE ARRAYS---------------------------------------------------!

   DRHOX      = 0.0_SP
   DRHOY      = 0.0_SP
   IF(.NOT.USE_LOCAL_REFERENCE) THEN
     RMEAN(0,:) = 0.0_SP
     RHO(0,:)   = 0.0_SP
   END IF
   RIJK       = 0.0_SP
   DRIJK1     = 0.0_SP
   DRIJK2     = 0.0_SP
# if defined (ICESHELF)
   DRIJK3     = 0.0_SP
   TMPZI      = 0.0_SP
   TMPZI1     = 0.0_SP
# endif
 

!----------CALCULATE AVERAGE DENSITY ON EACH EDGE------------------------------!

   IF(USE_LOCAL_REFERENCE) THEN
     DO I=1,N
# if defined (WET_DRY)
       IF(ISWETCT(I)*ISWETC(I)/=1) CYCLE
       IF(ALL(H(NV(I,:))<=STATIC_SSH_ADJ)) CYCLE
# endif
       DO J=1,3
         IJK=NV(I,J)
         ! Same physical depth convention as the existing RHO_PMEAN.
         LOCAL_Z(J,:)=REAL(ZZ(IJK,1:KBM1),DP)*REAL(D(IJK),DP)+REAL(EL(IJK),DP)
         LOCAL_TOP(J)=REAL(EL(IJK),DP)
         LOCAL_BOTTOM(J)=-REAL(H(IJK),DP)
# if defined (ICE_EMBEDDING)
         LOCAL_Z(J,:)=LOCAL_Z(J,:)-REAL(QTHICE(IJK),DP)
         LOCAL_TOP(J)=LOCAL_TOP(J)-REAL(QTHICE(IJK),DP)
# endif
         LOCAL_RAW(J,:)=REAL(RHO1(IJK,1:KBM1),DP)
       END DO
       IF(USE_NEIGHBOR_MEAN) THEN
         CALL REFERENCE_NEIGHBORS(I,PATCH_NODES,PATCH_COUNT,LOCAL_STATUS)
         IF(LOCAL_STATUS/=0) CALL FATAL_ERROR('Incomplete topology for neighborhood reference')
         DO P=1,PATCH_COUNT
           NODE_ID=PATCH_NODES(P)
           PATCH_BOTTOM(P)=-REAL(H(NODE_ID),DP)
           PATCH_TOP(P)=REAL(EL(NODE_ID),DP)
# if defined (ICE_EMBEDDING)
           PATCH_TOP(P)=PATCH_TOP(P)-REAL(QTHICE(NODE_ID),DP)
# endif
# if defined (WET_DRY)
           IF(WETTING_DRYING_ON .AND. ISWETN(NODE_ID)==0) PATCH_TOP(P)=PATCH_BOTTOM(P)
# endif
           PATCH_Z(P,:)=0.0_DP
           PATCH_RHO(P,:)=0.0_DP
           IF(PATCH_TOP(P)<=PATCH_BOTTOM(P)) CYCLE
           PATCH_Z(P,:)=REAL(ZZ(NODE_ID,1:KBM1),DP)*REAL(D(NODE_ID),DP)+PATCH_TOP(P)
           IF(PAR) THEN
             PATCH_RHO(P,:)=REAL(REFERENCE_RHO(NODE_ID,1:KBM1),DP)
           ELSE
             PATCH_RHO(P,:)=REAL(RHO1(NODE_ID,1:KBM1),DP)
           END IF
         END DO
         CALL NEIGHBOR_RMEAN(KBM1,PATCH_Z(1:PATCH_COUNT,:),PATCH_RHO(1:PATCH_COUNT,:), &
           PATCH_TOP(1:PATCH_COUNT),PATCH_BOTTOM(1:PATCH_COUNT),LOCAL_Z,LOCAL_REF,LOCAL_STATUS)
       ELSE
         CALL TRIANGLE_DEEP_REFERENCE(KBM1,LOCAL_Z,LOCAL_RAW,LOCAL_TOP,LOCAL_BOTTOM,LOCAL_REF,LOCAL_STATUS)
       END IF
       IF(LOCAL_STATUS==2 .AND. USE_NEIGHBOR_MEAN) THEN
         ! No wet donor covers a target (e.g. shoreline): retain original reference.
         DO J=1,3
           NODE_ID=NV(I,J)
           LOCAL_RHO(J,:)=RHO1(NODE_ID,1:KBM1)-RMEAN1(NODE_ID,1:KBM1)
         END DO
         LOCAL_RHO_C=RHO(I,1:KBM1)-RMEAN(I,1:KBM1)
       ELSE
         IF(LOCAL_STATUS/=0) CALL FATAL_ERROR('Invalid wet column in local reference')
         LOCAL_RHO=REAL(LOCAL_RAW,SP)-REAL(LOCAL_REF,SP)
         LOCAL_RHO_C=RHO(I,1:KBM1)-ONE_THIRD*(REAL(LOCAL_REF(1,:),SP) &
           +REAL(LOCAL_REF(2,:),SP)+REAL(LOCAL_REF(3,:),SP))
       END IF
       DO K=1,KBM1
         DO J=1,3
           J1=J+1-INT((J+1)/4)*3
           J2=J+2-INT((J+2)/4)*3
           RIJK(I,J,K)=0.5_SP*(LOCAL_RHO(J1,K)+LOCAL_RHO(J2,K))
         END DO
       END DO
       DO K=2,KBM1
         DRIJK2(I,K)=0.5_SP*(ZZ1(I,K-1)+ZZ1(I,K))*(LOCAL_RHO_C(K)-LOCAL_RHO_C(K-1))
         DRIJK2(I,K)=DRIJK2(I,K-1)+DRIJK2(I,K)
       END DO
     END DO
   ELSE
   DO K=1,KBM1
     DO I=1,N
       DO J=1,3
         J1=J+1-INT((J+1)/4)*3
         J2=J+2-INT((J+2)/4)*3
         RIJK(I,J,K)  = 0.5_SP*(RHO1(NV(I,J1),K)+RHO1(NV(I,J2),K))
       END DO
     END DO
   END DO
   END IF

   DO I=1,N
     DO J=1,3
       DRIJK1(I,J,1)=RIJK(I,J,1)*(-ZZ1(I,1))
       DO K=2,KBM1
         DRIJK1(I,J,K)=0.5_SP*(RIJK(I,J,K-1)+RIJK(I,J,K))*(ZZ1(I,K-1)-ZZ1(I,K))
         DRIJK1(I,J,K)=DRIJK1(I,J,K)+DRIJK1(I,J,K-1)
       END DO
     END DO
   END DO

   IF(.NOT.USE_LOCAL_REFERENCE) THEN
   DO I=1,N
     DRIJK2(I,1)=0.0_SP
     DO K=2,KBM1
       DRIJK2(I,K)=0.5_SP*(ZZ1(I,K-1)+ZZ1(I,K))*(RHO(I,K)-RHO(I,K-1)) 
       DRIJK2(I,K)=DRIJK2(I,K-1)+DRIJK2(I,K)
     END DO
   END DO
   END IF
   
# if defined (ICESHELF)
 DO K=1,KBM1
     DO I=1,N
           DRIJK3(I,K)  = RHO(I,1)-RHO(I,K)
     END DO
 END DO
! replace zisf with h+el-d to avoid computing pressure gradient with absolute
! ice draft on dry nodes
  TMPZI  = H  +  EL - D 
  TMPZI1 = H1 + EL1 - D1
# endif
   
   
   DO I = 1, N
#  if defined (WET_DRY)
    IF(ISWETCT(I)*ISWETC(I) == 1 .AND. &
      (H(NV(I,1)) > STATIC_SSH_ADJ .OR. H(NV(I,2)) > STATIC_SSH_ADJ .OR. H(NV(I,3)) > STATIC_SSH_ADJ))THEN
#  endif
     DO K=1,KBM1
        DO J = 1, 3
          J1=J+1-INT((J+1)/4)*3
          J2=J+2-INT((J+2)/4)*3
          IJK=NBE(I,J)
          DIJ=0.5_SP*(DT(NV(I,J1))+DT(NV(I,J2)))
#        if defined (ICESHELF)
           ZISFIJ=0.5_SP*(TMPZI(NV(I,J1))+TMPZI(NV(I,J2)))
           R1IJ  =0.5_SP*(RHO1(NV(I,J1),1)+RHO1(NV(I,J2),1))
#         endif
#    if defined (SPHERICAL)
          DRHO1=-DELTUY(I,J)*DRIJK1(I,J,K)*DT1(I)
          DRHO2=-DELTUY(I,J)*DIJ*DRIJK2(I,K)
#         if defined (ICESHELF)
           DRHO3=-DELTUY(I,J)*DRIJK3(I,K)*ZISFIJ
           DRHO4=TMPZI1(I)*(-DELTUY(I,J))*(R1IJ-(DRDZ*1e-3)*ZISFIJ)
#         endif
#    else
          DRHO1=(VY(NV(I,J1))-VY(NV(I,J2)))*DRIJK1(I,J,K)*DT1(I)
          DRHO2=(VY(NV(I,J1))-VY(NV(I,J2)))*DIJ*DRIJK2(I,K)
#       if defined (ICESHELF)
         DRHO3=(VY(NV(I,J1))-VY(NV(I,J2)))*DRIJK3(I,K)*ZISFIJ
         DRHO4=TMPZI1(I)*(VY(NV(I,J1))-VY(NV(I,J2)))*(R1IJ-(DRDZ*1e-3)*ZISFIJ)
#       endif	
#    endif

#  if defined (ICESHELF)
      DRHOX(I,K)=DRHOX(I,K)+DRHO1+DRHO2+DRHO3+DRHO4	 
#  else
      DRHOX(I,K)=DRHOX(I,K)+DRHO1+DRHO2
#  endif 
         

#    if defined (SPHERICAL)
          XTMP  = VX(NV(I,J2))*TPI-VX(NV(I,J1))*TPI
          XTMP1 = VX(NV(I,J2))-VX(NV(I,J1))
          IF(XTMP1 >  180.0_SP)THEN
            XTMP = -360.0_SP*TPI+XTMP
          ELSE IF(XTMP1 < -180.0_SP)THEN
            XTMP =  360.0_SP*TPI+XTMP
          END IF  

          DRHO1=XTMP*COS(DEG2RAD*YC(I))*DRIJK1(I,J,K)*DT1(I)
          DRHO2=XTMP*COS(DEG2RAD*YC(I))*DIJ*DRIJK2(I,K)
#       if defined (ICESHELF)	
         DRHO3=XTMP*COS(DEG2RAD*YC(I))*DRIJK3(I,K)*ZISFIJ
         DRHO4=TMPZI1(I)*XTMP*COS(DEG2RAD*YC(I))*(R1IJ-(DRDZ*1e-3)*ZISFIJ)           
#       endif 	  

#    else
	      DRHO1=(VX(NV(I,J2))-VX(NV(I,J1)))*DRIJK1(I,J,K)*DT1(I)
          DRHO2=(VX(NV(I,J2))-VX(NV(I,J1)))*DIJ*DRIJK2(I,K)
#        if defined (ICESHELF)	
          DRHO3=(VX(NV(I,J2))-VX(NV(I,J1)))*DRIJK3(I,K)*ZISFIJ
          DRHO4=TMPZI1(I)*(VX(NV(I,J2))-VX(NV(I,J1)))*(R1IJ-(DRDZ*1e-3)*ZISFIJ)
#        endif 	  
		  
#        endif

#  if defined (ICESHELF) 		 
          DRHOY(I,K)=DRHOY(I,K)+DRHO1+DRHO2+DRHO3+DRHO4
#  else 
          DRHOY(I,K)=DRHOY(I,K)+DRHO1+DRHO2
#  endif 
      END DO
     END DO
#  if defined (WET_DRY)
    END IF
#  endif
   END DO
   
! for north pole   

#  if defined (SPHERICAL)
#  if defined (ICESHELF)
!    CALL BAROPG_XY_ISF(DRIJK1,DRIJK2)
#  else
     CALL BAROPG_XY(DRIJK1,DRIJK2)
#  endif   
#  endif 

!----------MULTIPLY BY GRAVITY AND ELEMENT DEPTH-------------------------------!

   DO K=1,KBM1
     DRHOX(:,K)=DRHOX(:,K)*DT1(:)*DZ1(:,K)*GRAV_E(:)*RAMP
     DRHOY(:,K)=DRHOY(:,K)*DT1(:)*DZ1(:,K)*GRAV_E(:)*RAMP
   END DO

!----------ADD MEAN DENSITY BACK ON--------------------------------------------!

   IF(.NOT.USE_LOCAL_REFERENCE) THEN
     RHO1 = RHO1 + RMEAN1
     RHO  = RHO  + RMEAN
   END IF

   IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "End: baropg.F"

   RETURN
   CONTAINS

   PURE SUBROUTINE REFERENCE_NEIGHBORS(CELL,NODES,NCOUNT,STATUS)
     ! Union of all nodes in triangles incident on any target-triangle vertex.
     ! genmap includes this full vertex star in the normal MPI halo.
     INTEGER, INTENT(IN) :: CELL
     INTEGER, INTENT(OUT) :: NODES(:),NCOUNT,STATUS
     INTEGER :: VERTEX,SEED,K,ELEMENT,J,ID,A,B,KEY,OTHER
     STATUS=1
     NCOUNT=3
     NODES=0
     NODES(1:3)=NV(CELL,:)
     DO VERTEX=1,3
       SEED=NV(CELL,VERTEX)
       IF(SEED<1 .OR. SEED>UBOUND(NBVE,1)) RETURN
       IF(NTVE(SEED)<1 .OR. NTVE(SEED)>SIZE(NBVE,2)) RETURN
       DO K=1,NTVE(SEED)
         ELEMENT=NBVE(SEED,K)
         IF(ELEMENT<1 .OR. ELEMENT>UBOUND(NV,1)) RETURN
         DO J=1,3
           ID=NV(ELEMENT,J)
           IF(ID<1 .OR. ID>MT) RETURN
           IF(ANY(NODES(1:NCOUNT)==ID)) CYCLE
           IF(NCOUNT==SIZE(NODES)) RETURN
           NCOUNT=NCOUNT+1
           NODES(NCOUNT)=ID
         END DO
       END DO
     END DO
     ! Canonical global-node order keeps summation independent of MPI numbering.
     DO A=2,NCOUNT
       ID=NODES(A)
       KEY=ID
       IF(PAR) KEY=NGID_X(ID)
       B=A-1
       DO WHILE(B>=1)
         OTHER=NODES(B)
         IF(PAR) OTHER=NGID_X(OTHER)
         IF(OTHER<=KEY) EXIT
         NODES(B+1)=NODES(B)
         B=B-1
       END DO
       NODES(B+1)=ID
     END DO
     STATUS=0
   END SUBROUTINE REFERENCE_NEIGHBORS

   PURE SUBROUTINE NEIGHBOR_RMEAN(NLAY,Z,R,TOP,BOTTOM,QUERY_Z,REF,STATUS)
     ! Equal-weight mean at each physical z; each unique wet node counts once.
     ! End values are constant only between sample centres and that node's
     ! water boundary. A donor never contributes below its physical bottom.
     USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_IS_FINITE
     INTEGER, INTENT(IN) :: NLAY
     REAL(DP), INTENT(IN) :: Z(:,:),R(:,:),TOP(:),BOTTOM(:),QUERY_Z(3,NLAY)
     REAL(DP), INTENT(OUT) :: REF(3,NLAY)
     INTEGER, INTENT(OUT) :: STATUS
     INTEGER :: COUNT(3,NLAY),DONOR,NODE,K,LOWER
     REAL(DP) :: QUERY,VALUE,WEIGHT
     STATUS=1
     REF=0.0_DP
     COUNT=0
     IF(NLAY<1 .OR. SIZE(Z,1)<1) RETURN
     IF(.NOT.ALL(IEEE_IS_FINITE(QUERY_Z))) RETURN
     IF(NLAY>1) THEN
       IF(ANY(QUERY_Z(:,1:NLAY-1)<=QUERY_Z(:,2:NLAY))) RETURN
     END IF
     DO DONOR=1,SIZE(Z,1)
       IF(.NOT.IEEE_IS_FINITE(TOP(DONOR)) .OR. .NOT.IEEE_IS_FINITE(BOTTOM(DONOR))) RETURN
       IF(TOP(DONOR)<=BOTTOM(DONOR)) CYCLE
       IF(.NOT.ALL(IEEE_IS_FINITE(Z(DONOR,:))) .OR. .NOT.ALL(IEEE_IS_FINITE(R(DONOR,:)))) RETURN
       IF(Z(DONOR,1)>TOP(DONOR) .OR. Z(DONOR,NLAY)<BOTTOM(DONOR)) RETURN
       IF(NLAY>1) THEN
         IF(ANY(Z(DONOR,1:NLAY-1)<=Z(DONOR,2:NLAY))) RETURN
       END IF
       DO NODE=1,3
         LOWER=1
         DO K=1,NLAY
           QUERY=QUERY_Z(NODE,K)
           IF(QUERY>TOP(DONOR) .OR. QUERY<BOTTOM(DONOR)) CYCLE
           IF(QUERY>=Z(DONOR,1)) THEN
             VALUE=R(DONOR,1)
           ELSE IF(QUERY<=Z(DONOR,NLAY)) THEN
             VALUE=R(DONOR,NLAY)
           ELSE
             DO WHILE(LOWER<NLAY-1)
               IF(QUERY>=Z(DONOR,LOWER+1)) EXIT
               LOWER=LOWER+1
             END DO
             WEIGHT=(Z(DONOR,LOWER)-QUERY)/(Z(DONOR,LOWER)-Z(DONOR,LOWER+1))
             VALUE=R(DONOR,LOWER)+WEIGHT*(R(DONOR,LOWER+1)-R(DONOR,LOWER))
           END IF
           REF(NODE,K)=REF(NODE,K)+VALUE
           COUNT(NODE,K)=COUNT(NODE,K)+1
         END DO
       END DO
     END DO
     IF(ANY(COUNT==0)) THEN
       STATUS=2
       RETURN
     END IF
     REF=REF/REAL(COUNT,DP)
     STATUS=0
   END SUBROUTINE NEIGHBOR_RMEAN

   PURE SUBROUTINE TRIANGLE_DEEP_REFERENCE(NLAY,Z,R,TOP,BOTTOM,REF,STATUS)
     ! One common physical-z reference: the triangle node with deepest seabed.
     ! Linear interpolation; constant endpoint values as SINTER_EXTRP_NONE.
     ! No query is below the chosen donor's physical bottom.
     USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_IS_FINITE
     INTEGER, INTENT(IN) :: NLAY
     REAL(DP), INTENT(IN) :: Z(3,NLAY),R(3,NLAY),TOP(3),BOTTOM(3)
     REAL(DP), INTENT(OUT) :: REF(3,NLAY)
     INTEGER, INTENT(OUT) :: STATUS
     INTEGER :: DONOR,NODE,K,LOWER
     REAL(DP) :: QUERY,WEIGHT
     STATUS=1
     REF=0.0_DP
     IF(NLAY<1) RETURN
     IF(.NOT.ALL(IEEE_IS_FINITE(Z)) .OR. .NOT.ALL(IEEE_IS_FINITE(R))) RETURN
     IF(.NOT.ALL(IEEE_IS_FINITE(TOP)) .OR. .NOT.ALL(IEEE_IS_FINITE(BOTTOM))) RETURN
     IF(ANY(TOP<=BOTTOM)) RETURN
     IF(ANY(Z(:,1)>TOP) .OR. ANY(Z(:,NLAY)<BOTTOM)) RETURN
     IF(NLAY>1) THEN
       IF(ANY(Z(:,1:NLAY-1)<=Z(:,2:NLAY))) RETURN
     END IF
     DONOR=MINLOC(BOTTOM,DIM=1) ! Fixed bathymetry; ties use first NV entry.
     IF(ANY(Z<BOTTOM(DONOR))) RETURN
     DO NODE=1,3
       LOWER=1
       DO K=1,NLAY
         QUERY=Z(NODE,K)
         IF(QUERY>=Z(DONOR,1)) THEN
           REF(NODE,K)=R(DONOR,1)
         ELSE IF(QUERY<=Z(DONOR,NLAY)) THEN
           REF(NODE,K)=R(DONOR,NLAY)
         ELSE
           DO WHILE(LOWER<NLAY-1)
             IF(QUERY>=Z(DONOR,LOWER+1)) EXIT
             LOWER=LOWER+1
           END DO
           WEIGHT=(Z(DONOR,LOWER)-QUERY)/(Z(DONOR,LOWER)-Z(DONOR,LOWER+1))
           REF(NODE,K)=R(DONOR,LOWER)+WEIGHT*(R(DONOR,LOWER+1)-R(DONOR,LOWER))
         END IF
       END DO
     END DO
     STATUS=0
   END SUBROUTINE TRIANGLE_DEEP_REFERENCE
   END SUBROUTINE BAROPG
!==============================================================================|
