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

   SUBROUTINE BAROPG_SHEAR_CORE

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
   REAL(SP) :: LOCAL_Z(3,KBM1), LOCAL_RHO(3,KBM1), LOCAL_RHO_C(KBM1)
   REAL(DP) :: REFERENCE_Z(3,KBM1), REFERENCE_RAW(3,KBM1), REFERENCE_RHO_AT_Z(3,KBM1)
   REAL(DP) :: REFERENCE_TOP(3), REFERENCE_BOTTOM(3)
   INTEGER :: PATCH_NODES(3+6*MX_NBR_ELEM), PATCH_COUNT, P, NODE_ID
   REAL(DP) :: PATCH_Z(3+6*MX_NBR_ELEM,KBM1), PATCH_RHO(3+6*MX_NBR_ELEM,KBM1)
   REAL(DP) :: PATCH_TOP(3+6*MX_NBR_ELEM), PATCH_BOTTOM(3+6*MX_NBR_ELEM)
   REAL(SP), ALLOCATABLE, TARGET :: REFERENCE_RHO(:,:)
   ! Snapshot native-depth products plus initial surface/density: fixed physical-z profiles.
   REAL(DP), ALLOCATABLE, SAVE :: FROZEN_ZD(:,:), FROZEN_TOP(:), FROZEN_BOTTOM(:), FROZEN_EL(:)
   REAL(SP), ALLOCATABLE, SAVE :: FROZEN_RHO(:,:)
   LOGICAL, SAVE :: FROZEN_PAR=.FALSE.
# if defined (ICE_EMBEDDING)
   REAL(DP), ALLOCATABLE, SAVE :: FROZEN_ICE(:)
# endif
   LOGICAL :: USE_LOCAL_REFERENCE, USE_NEIGHBOR_MEAN, USE_FROZEN_REFERENCE, NEED_DONOR_RHO
   INTEGER :: LOCAL_STATUS
   REAL(SP) :: TEMP,DIJ,DRHO1,DRHO2
   INTEGER  :: I,K,J,J1,J2,IJK
   LOGICAL  :: USE_LOCAL_MEAN, USE_LOCAL_Z_MEAN, APPLY_V2, USE_LOCAL_CUBIC
   ! Zero selects the merged native layer depths; positive values select a uniform grid.
   INTEGER, PARAMETER :: LOCAL_Z_GRID=0
   REAL(SP) :: LOCAL_TOP(3), LOCAL_BOTTOM(3), LOCAL_Z_REFERENCE(3,KBM1)
   INTEGER :: LOCAL_Z_STATUS
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

   USE_FROZEN_REFERENCE = BAROCLINIC_PRESSURE_GRADIENT == 'sigma levels deep reference frozen' .OR. &
     BAROCLINIC_PRESSURE_GRADIENT == 'sigma levels neighborhood mean frozen'
   USE_NEIGHBOR_MEAN = BAROCLINIC_PRESSURE_GRADIENT == 'sigma levels neighborhood mean' .OR. &
     BAROCLINIC_PRESSURE_GRADIENT == 'sigma levels neighborhood mean frozen'
   USE_LOCAL_REFERENCE = USE_NEIGHBOR_MEAN .OR. USE_FROZEN_REFERENCE .OR. &
     BAROCLINIC_PRESSURE_GRADIENT == 'sigma levels deep reference'
   NEED_DONOR_RHO = (USE_NEIGHBOR_MEAN .AND. .NOT.USE_FROZEN_REFERENCE) .OR. &
     (USE_FROZEN_REFERENCE .AND. .NOT.ALLOCATED(FROZEN_RHO))
   IF(NEED_DONOR_RHO .AND. PAR) THEN
     ! Refresh a PRIVATE density copy for halo donors. No global field is written.
     ALLOCATE(REFERENCE_RHO(0:MT,KB))
     REFERENCE_RHO=RHO1
# if defined (MULTIPROCESSOR)
     CALL AEXCHANGE(NC,MYID,NPROCS,REFERENCE_RHO)
# endif
   END IF
   IF(USE_FROZEN_REFERENCE) CALL CAPTURE_LOCAL_REFERENCE

   APPLY_V2 = BAROCLINIC_PRESSURE_GRADIENT == 'sigma levels local mean' .OR. &
     BAROCLINIC_PRESSURE_GRADIENT == 'sigma levels local z mean v2'
   USE_LOCAL_Z_MEAN = BAROCLINIC_PRESSURE_GRADIENT == 'sigma levels local z mean' .OR. &
     BAROCLINIC_PRESSURE_GRADIENT == 'sigma levels local z mean v2'
   USE_LOCAL_CUBIC = BAROCLINIC_PRESSURE_GRADIENT == 'sigma levels local cubic'
   USE_LOCAL_MEAN = USE_LOCAL_Z_MEAN .OR. APPLY_V2 .OR. USE_LOCAL_CUBIC .OR. USE_LOCAL_REFERENCE
#  if defined (ICESHELF)
   IF(USE_LOCAL_REFERENCE) CALL FATAL_ERROR('Local reference is not implemented for ICESHELF')
   ! Ice-draft pressure terms need their own consistent reference treatment.
   IF(USE_LOCAL_MEAN) CALL FATAL_ERROR( &
     'sigma levels local mean is not yet supported with ICESHELF')
#  endif
   IF(.NOT. USE_LOCAL_MEAN)THEN
     RHO1(:,1:KBM1) = RHO1(:,1:KBM1) - RMEAN1(:,1:KBM1)
     RHO = RHO - RMEAN
   END IF

!----------INITIALIZE ARRAYS---------------------------------------------------!

   DRHOX      = 0.0_SP
   DRHOY      = 0.0_SP
   IF(.NOT. USE_LOCAL_MEAN)THEN
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

   IF(USE_LOCAL_MEAN)THEN
     DO I=1,N
#      if defined (WET_DRY)
       ! Match the pressure-force mask before accessing possibly dry columns.
       IF(ISWETCT(I)*ISWETC(I) /= 1)CYCLE
       IF(ALL(H(NV(I,:)) <= STATIC_SSH_ADJ))CYCLE
#      endif
       IF(USE_LOCAL_REFERENCE)THEN
         DO J=1,3
           IJK=NV(I,J)
           ! Same physical depth convention as the existing RHO_PMEAN.
           REFERENCE_Z(J,:)=REAL(ZZ(IJK,1:KBM1),DP)*REAL(D(IJK),DP)+REAL(EL(IJK),DP)
           REFERENCE_TOP(J)=REAL(EL(IJK),DP)
           REFERENCE_BOTTOM(J)=-REAL(H(IJK),DP)
# if defined (ICE_EMBEDDING)
           REFERENCE_Z(J,:)=REFERENCE_Z(J,:)-REAL(QTHICE(IJK),DP)
           REFERENCE_TOP(J)=REFERENCE_TOP(J)-REAL(QTHICE(IJK),DP)
# endif
           REFERENCE_RAW(J,:)=REAL(RHO1(IJK,1:KBM1),DP)
         END DO
         IF(USE_NEIGHBOR_MEAN) THEN
           CALL REFERENCE_NEIGHBORS(I,PATCH_NODES,PATCH_COUNT,LOCAL_STATUS)
           IF(LOCAL_STATUS/=0) CALL FATAL_ERROR('Incomplete topology for neighborhood reference')
           DO P=1,PATCH_COUNT
             NODE_ID=PATCH_NODES(P)
             IF(USE_FROZEN_REFERENCE) THEN
               PATCH_BOTTOM(P)=FROZEN_BOTTOM(NODE_ID)
               PATCH_TOP(P)=FROZEN_TOP(NODE_ID)
               PATCH_Z(P,:)=0.0_DP
               PATCH_RHO(P,:)=0.0_DP
               IF(PATCH_TOP(P)<=PATCH_BOTTOM(P)) CYCLE
               PATCH_Z(P,:)=FROZEN_ZD(NODE_ID,:)+PATCH_TOP(P)
               PATCH_RHO(P,:)=REAL(FROZEN_RHO(NODE_ID,:),DP)
               CYCLE
             END IF
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
           IF(USE_FROZEN_REFERENCE) THEN
             ! Extend the entire frozen mean constantly above its initial surface.
             CALL NEIGHBOR_RMEAN(KBM1,PATCH_Z(1:PATCH_COUNT,:),PATCH_RHO(1:PATCH_COUNT,:), &
               PATCH_TOP(1:PATCH_COUNT),PATCH_BOTTOM(1:PATCH_COUNT),REFERENCE_Z,REFERENCE_RHO_AT_Z,LOCAL_STATUS, &
               TOP_CAP=MAXVAL(PATCH_TOP(1:PATCH_COUNT), &
                 MASK=PATCH_TOP(1:PATCH_COUNT)>PATCH_BOTTOM(1:PATCH_COUNT)))
           ELSE
           CALL NEIGHBOR_RMEAN(KBM1,PATCH_Z(1:PATCH_COUNT,:),PATCH_RHO(1:PATCH_COUNT,:), &
             PATCH_TOP(1:PATCH_COUNT),PATCH_BOTTOM(1:PATCH_COUNT),REFERENCE_Z,REFERENCE_RHO_AT_Z,LOCAL_STATUS)
           END IF
         ELSE IF(USE_FROZEN_REFERENCE) THEN
           DO J=1,3
             NODE_ID=NV(I,J)
             PATCH_Z(J,:)=FROZEN_ZD(NODE_ID,:)+FROZEN_EL(NODE_ID)
# if defined (ICE_EMBEDDING)
             PATCH_Z(J,:)=PATCH_Z(J,:)-FROZEN_ICE(NODE_ID)
# endif
             PATCH_RHO(J,:)=REAL(FROZEN_RHO(NODE_ID,:),DP)
             PATCH_TOP(J)=FROZEN_TOP(NODE_ID)
             PATCH_BOTTOM(J)=FROZEN_BOTTOM(NODE_ID)
           END DO
           CALL TRIANGLE_DEEP_REFERENCE(KBM1,PATCH_Z(1:3,:),PATCH_RHO(1:3,:),PATCH_TOP(1:3),PATCH_BOTTOM(1:3), &
             REFERENCE_RHO_AT_Z,LOCAL_STATUS,QUERY_Z=REFERENCE_Z)
         ELSE
           CALL TRIANGLE_DEEP_REFERENCE(KBM1,REFERENCE_Z,REFERENCE_RAW,REFERENCE_TOP,REFERENCE_BOTTOM, &
             REFERENCE_RHO_AT_Z,LOCAL_STATUS)
         END IF
         IF(LOCAL_STATUS==2 .AND. USE_NEIGHBOR_MEAN) THEN
           IF(USE_FROZEN_REFERENCE) CALL FATAL_ERROR('Frozen reference has no initial wet donor at target depth')
           ! No wet donor covers a target (e.g. shoreline): retain original reference.
           DO J=1,3
             NODE_ID=NV(I,J)
             LOCAL_RHO(J,:)=RHO1(NODE_ID,1:KBM1)-RMEAN1(NODE_ID,1:KBM1)
           END DO
           LOCAL_RHO_C=RHO(I,1:KBM1)-RMEAN(I,1:KBM1)
         ELSE
           IF(LOCAL_STATUS/=0) CALL FATAL_ERROR('Invalid wet column in local reference')
           LOCAL_RHO=REAL(REFERENCE_RAW,SP)-REAL(REFERENCE_RHO_AT_Z,SP)
           LOCAL_RHO_C=RHO(I,1:KBM1)-ONE_THIRD*(REAL(REFERENCE_RHO_AT_Z(1,:),SP) &
             +REAL(REFERENCE_RHO_AT_Z(2,:),SP)+REAL(REFERENCE_RHO_AT_Z(3,:),SP))
         END IF
       ELSE IF(USE_LOCAL_CUBIC)THEN
         DO J=1,3
           IJK=NV(I,J)
           LOCAL_Z(J,:)=ZZ(IJK,1:KBM1)*DT(IJK)+ET(IJK)
#          if defined (ICE_EMBEDDING)
           LOCAL_Z(J,:)=LOCAL_Z(J,:)-QTHICE(IJK)
#          endif
           LOCAL_RHO(J,:)=RHO1(IJK,1:KBM1)
         END DO
         CALL TRIANGLE_CUBIC_RESIDUAL(KBM1,LOCAL_Z,LOCAL_RHO,LOCAL_Z_STATUS)
         IF(LOCAL_Z_STATUS /= 0)THEN
           DO J=1,3
             IJK=NV(I,J)
             LOCAL_RHO(J,:)=(RHO1(IJK,1:KBM1)-RHO1(NV(I,1),1)) &
               -(RMEAN1(IJK,1:KBM1)-RMEAN1(NV(I,1),1))
           END DO
         END IF
       ELSE IF(USE_LOCAL_Z_MEAN)THEN
         DO J=1,3
           IJK=NV(I,J)
           ! Same reference coordinates and physical mask as RHO_PMEAN.
           LOCAL_Z(J,:)=ZZ(IJK,1:KBM1)*D(IJK)+EL(IJK)
           LOCAL_TOP(J)=EL(IJK)
           LOCAL_BOTTOM(J)=-H(IJK)
#          if defined (ICE_EMBEDDING)
           LOCAL_Z(J,:)=LOCAL_Z(J,:)-QTHICE(IJK)
           LOCAL_TOP(J)=LOCAL_TOP(J)-QTHICE(IJK)
#          endif
           LOCAL_RHO(J,:)=RHO1(IJK,1:KBM1)
         END DO
         CALL BUILD_Z_REFERENCE(3,KBM1,LOCAL_Z_GRID, &
           LOCAL_Z,LOCAL_RHO,LOCAL_TOP,LOCAL_BOTTOM,LOCAL_Z_REFERENCE,LOCAL_Z_STATUS)
         IF(LOCAL_Z_STATUS /= 0)THEN
           ! Invalid columns or uncovered z levels: use the domain reference.
           DO J=1,3
             LOCAL_Z_REFERENCE(J,:)=RMEAN1(NV(I,J),1:KBM1)
           END DO
         END IF
         DO J=1,3
           IJK=NV(I,J)
           LOCAL_RHO(J,:)=(RHO1(IJK,1:KBM1)-RHO1(NV(I,1),1)) &
             -(LOCAL_Z_REFERENCE(J,:)-LOCAL_Z_REFERENCE(1,1))
           ! The v2 curvature correction keeps its original pressure coordinates.
           LOCAL_Z(J,:)=ZZ(IJK,1:KBM1)*DT(IJK)+ET(IJK)
#          if defined (ICE_EMBEDDING)
           LOCAL_Z(J,:)=LOCAL_Z(J,:)-QTHICE(IJK)
#          endif
         END DO
       ELSE
       DO J=1,3
         IJK=NV(I,J)
         LOCAL_Z(J,:) = ZZ(IJK,1:KBM1)*DT(IJK)+ET(IJK)
#        if defined (ICE_EMBEDDING)
         LOCAL_Z(J,:) = LOCAL_Z(J,:)-QTHICE(IJK)
#        endif
         ! Work on anomalies relative to the existing domain reference.
         ! Subtract a common anchor first to avoid a large offset in either field.
         LOCAL_RHO(J,:) = (RHO1(IJK,1:KBM1)-RHO1(NV(I,1),1)) &
                       - (RMEAN1(IJK,1:KBM1)-RMEAN1(NV(I,1),1))
       END DO

       ! Version 2: a local curvature correction to the domain reference.
       ! No profile is extrapolated beyond its sampled depths.
       END IF
       IF(.NOT.USE_LOCAL_REFERENCE)THEN
         IF(APPLY_V2)CALL REMOVE_TRIANGLE_MEAN(LOCAL_Z,LOCAL_RHO)
         LOCAL_RHO_C = ONE_THIRD*(LOCAL_RHO(1,:)+LOCAL_RHO(2,:)+LOCAL_RHO(3,:))
       END IF
       DO K=1,KBM1
         DO J=1,3
           J1=J+1-INT((J+1)/4)*3
           J2=J+2-INT((J+2)/4)*3
           RIJK(I,J,K)=0.5_SP*(LOCAL_RHO(J1,K)+LOCAL_RHO(J2,K))
         END DO
       END DO

       ! Use the same residuals in the vertical/metric part of the Jacobian.
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

   IF(.NOT. USE_LOCAL_MEAN)THEN
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

   IF(.NOT. USE_LOCAL_MEAN)THEN
     RHO1 = RHO1 + RMEAN1
     RHO  = RHO  + RMEAN
   END IF

   IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "End: baropg.F"

   RETURN
   CONTAINS

   SUBROUTINE CAPTURE_LOCAL_REFERENCE
     INTEGER :: NODE
     IF(ALLOCATED(FROZEN_RHO)) THEN
       IF(SIZE(FROZEN_RHO,1)/=MT+1 .OR. SIZE(FROZEN_RHO,2)/=KBM1 .OR. (FROZEN_PAR.NEQV.PAR)) &
         CALL FATAL_ERROR('Frozen reference grid or execution mode changed')
       RETURN
     END IF
     ALLOCATE(FROZEN_RHO(0:MT,KBM1),FROZEN_ZD(0:MT,KBM1))
     ALLOCATE(FROZEN_TOP(0:MT),FROZEN_BOTTOM(0:MT),FROZEN_EL(0:MT))
# if defined (ICE_EMBEDDING)
     ALLOCATE(FROZEN_ICE(0:MT))
     FROZEN_ICE=REAL(QTHICE,DP)
# endif
     IF(PAR) THEN
       FROZEN_RHO=REFERENCE_RHO(:,1:KBM1)
     ELSE
       FROZEN_RHO=RHO1(:,1:KBM1)
     END IF
     FROZEN_EL=REAL(EL,DP)
     FROZEN_TOP=FROZEN_EL
     FROZEN_BOTTOM=-REAL(H,DP)
# if defined (ICE_EMBEDDING)
     FROZEN_TOP=FROZEN_TOP-FROZEN_ICE
# endif
     FROZEN_ZD=0.0_DP
     DO NODE=1,MT
# if defined (WET_DRY)
       IF(WETTING_DRYING_ON .AND. ISWETN(NODE)==0) FROZEN_TOP(NODE)=FROZEN_BOTTOM(NODE)
# endif
       IF(FROZEN_TOP(NODE)<=FROZEN_BOTTOM(NODE)) CYCLE
       FROZEN_ZD(NODE,:)=REAL(ZZ(NODE,1:KBM1),DP)*REAL(D(NODE),DP)
     END DO
     FROZEN_PAR=PAR
     IF(SERIAL .OR. MYID==1) WRITE(IPT,*) 'BAROPG: local reference frozen at first call (physical z)'
   END SUBROUTINE CAPTURE_LOCAL_REFERENCE

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

   PURE SUBROUTINE NEIGHBOR_RMEAN(NLAY,Z,R,TOP,BOTTOM,QUERY_Z,REF,STATUS,TOP_CAP)
     ! Equal-weight mean at each physical z; each unique wet node counts once.
     ! End values are constant only between sample centres and that node's
     ! water boundary. A donor never contributes below its physical bottom.
     USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_IS_FINITE
     INTEGER, INTENT(IN) :: NLAY
     REAL(DP), INTENT(IN) :: Z(:,:),R(:,:),TOP(:),BOTTOM(:),QUERY_Z(3,NLAY)
     REAL(DP), INTENT(OUT) :: REF(3,NLAY)
     INTEGER, INTENT(OUT) :: STATUS
     REAL(DP), OPTIONAL, INTENT(IN) :: TOP_CAP
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
           IF(PRESENT(TOP_CAP)) QUERY=MIN(QUERY,TOP_CAP)
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

   PURE SUBROUTINE TRIANGLE_DEEP_REFERENCE(NLAY,Z,R,TOP,BOTTOM,REF,STATUS,QUERY_Z)
     ! One common physical-z reference: the triangle node with deepest seabed.
     ! Linear interpolation; constant endpoint values as SINTER_EXTRP_NONE.
     ! No query is below the chosen donor's physical bottom.
     USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_IS_FINITE
     INTEGER, INTENT(IN) :: NLAY
     REAL(DP), INTENT(IN) :: Z(:,:),R(:,:),TOP(3),BOTTOM(3)
     REAL(DP), INTENT(OUT) :: REF(3,NLAY)
     INTEGER, INTENT(OUT) :: STATUS
     REAL(DP), OPTIONAL, INTENT(IN) :: QUERY_Z(3,NLAY)
     INTEGER :: DONOR,NODE,K,LOWER
     REAL(DP) :: QUERY,WEIGHT
     STATUS=1
     REF=0.0_DP
     IF(NLAY<1) RETURN
     IF(SIZE(Z,1)/=3 .OR. SIZE(Z,2)/=NLAY .OR. SIZE(R,1)/=3 .OR. SIZE(R,2)/=NLAY) RETURN
     IF(.NOT.ALL(IEEE_IS_FINITE(Z)) .OR. .NOT.ALL(IEEE_IS_FINITE(R))) RETURN
     IF(.NOT.ALL(IEEE_IS_FINITE(TOP)) .OR. .NOT.ALL(IEEE_IS_FINITE(BOTTOM))) RETURN
     IF(ANY(TOP<=BOTTOM)) RETURN
     IF(ANY(Z(:,1)>TOP) .OR. ANY(Z(:,NLAY)<BOTTOM)) RETURN
     IF(NLAY>1) THEN
       IF(ANY(Z(:,1:NLAY-1)<=Z(:,2:NLAY))) RETURN
     END IF
     DONOR=MINLOC(BOTTOM,DIM=1) ! Fixed bathymetry; ties use first NV entry.
     IF(PRESENT(QUERY_Z)) THEN
       IF(.NOT.ALL(IEEE_IS_FINITE(QUERY_Z))) RETURN
       IF(ANY(QUERY_Z<BOTTOM(DONOR))) RETURN
       IF(NLAY>1) THEN
         IF(ANY(QUERY_Z(:,1:NLAY-1)<=QUERY_Z(:,2:NLAY))) RETURN
       END IF
     ELSE
       IF(ANY(Z<BOTTOM(DONOR))) RETURN
     END IF
     DO NODE=1,3
       LOWER=1
       DO K=1,NLAY
         QUERY=Z(NODE,K)
         IF(PRESENT(QUERY_Z)) QUERY=QUERY_Z(NODE,K)
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


   PURE SUBROUTINE REMOVE_TRIANGLE_MEAN(Z,R)
     ! Mean curvature of the three domain-anomaly profiles in physical z.
     ! Each profile's endpoint chord is removed before averaging. Its
     ! curvature correction is zero outside its own sampled water depths.
     REAL(SP), INTENT(IN)    :: Z(3,KBM1)
     REAL(SP), INTENT(INOUT) :: R(3,KBM1)
     REAL(SP) :: RESULT(3,KBM1), SPAN, DZ_PROFILE, WEIGHT, FRACTION, CURVATURE
     REAL(SP) :: COMMON_SPAN, COVERAGE
     INTEGER  :: NODE, SOURCE, LEVEL, LOWER

     IF(KBM1 < 3)RETURN
     IF(ANY(Z(:,1:KBM1-1) <= Z(:,2:KBM1)))RETURN
     COMMON_SPAN=MINVAL(Z(:,1))-MAXVAL(Z(:,KBM1))
     IF(COMMON_SPAN <= 0.0_SP)RETURN
     ! Fade continuously to the domain method as sampled overlap disappears.
     COVERAGE=MIN(1.0_SP,COMMON_SPAN/MINVAL(Z(:,1)-Z(:,KBM1)))
     RESULT=R
     DO SOURCE=1,3
       SPAN=Z(SOURCE,1)-Z(SOURCE,KBM1)
       DO NODE=1,3
         LOWER=1
         DO LEVEL=1,KBM1
           IF(Z(NODE,LEVEL) >= Z(SOURCE,1) .OR. Z(NODE,LEVEL) <= Z(SOURCE,KBM1))CYCLE
           DO WHILE(LOWER < KBM1-1)
             IF(Z(NODE,LEVEL) >= Z(SOURCE,LOWER+1))EXIT
             LOWER=LOWER+1
           END DO
           DZ_PROFILE=Z(SOURCE,LOWER)-Z(SOURCE,LOWER+1)
           WEIGHT=MIN(1.0_SP,MAX(0.0_SP,(Z(SOURCE,LOWER)-Z(NODE,LEVEL))/DZ_PROFILE))
           FRACTION=(Z(SOURCE,1)-Z(NODE,LEVEL))/SPAN
           CURVATURE=(R(SOURCE,LOWER)-R(SOURCE,1)) &
             + WEIGHT*(R(SOURCE,LOWER+1)-R(SOURCE,LOWER)) &
             - FRACTION*(R(SOURCE,KBM1)-R(SOURCE,1))
           RESULT(NODE,LEVEL)=RESULT(NODE,LEVEL)-ONE_THIRD*COVERAGE*CURVATURE
         END DO
       END DO
     END DO
     R=RESULT
   END SUBROUTINE REMOVE_TRIANGLE_MEAN

   PURE SUBROUTINE TRIANGLE_CUBIC_RESIDUAL(NLAY,Z,R,STATUS)
     ! ROMS-inspired reconstruction; FVCOM pressure quadrature is unchanged.
     ! prsgrd32.h: harmonic slopes for BOTH rho(s) and z(s), then invert z(s).
     ! https://github.com/myroms/roms/blob/develop/ROMS/Nonlinear/prsgrd32.h
     ! Shchepetkin & McWilliams (2003), doi:10.1029/2001JC001047.
     !
     ! Copyright (c) 2002-2026 Regional Ocean Modeling System (ROMS)
     ! Permission is hereby granted, free of charge, to any person obtaining a
     ! copy of this software and associated documentation files (the Software),
     ! to deal in the Software without restriction, including without limitation
     ! the rights to use, copy, modify, merge, publish, distribute, sublicense,
     ! and/or sell copies of the Software, and to permit persons to whom the
     ! Software is furnished to do so, subject to the following conditions:
     ! The above copyright notice and this permission notice shall be included
     ! in all copies or substantial portions of the Software.
     ! THE SOFTWARE IS PROVIDED AS IS, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
     ! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
     ! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
     ! THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
     ! OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
     ! ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
     ! OTHER DEALINGS IN THE SOFTWARE.
     USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_IS_FINITE
     INTEGER, INTENT(IN) :: NLAY
     REAL(SP), INTENT(IN) :: Z(3,NLAY)
     REAL(SP), INTENT(INOUT) :: R(3,NLAY)
     INTEGER, INTENT(OUT) :: STATUS
     REAL(DP) :: ZD(NLAY,3),RD(NLAY,3),DZ(NLAY,3),DR(NLAY,3)
     REAL(DP) :: OFFSET(2,3),SLOPE(2,3),WIDTH(2,3),DELTA(3)
     REAL(DP) :: B,DB,V,DV,Q,U,SIGN_Z,MEAN_DELTA
     REAL(SP) :: RESULT(3,NLAY)
     INTEGER :: J,K,NODE,DONOR,ENDPT,LEV,LOWER(3),IDX

     STATUS=1
     IF(NLAY < 2)RETURN
     IF(.NOT.ALL(IEEE_IS_FINITE(Z)) .OR. .NOT.ALL(IEEE_IS_FINITE(R)))RETURN
     IF(ANY(Z(:,1:NLAY-1) <= Z(:,2:NLAY)))RETURN
     ZD=TRANSPOSE(REAL(Z,DP))
     RD=TRANSPOSE(REAL(R,DP))-REAL(R(1,1),DP)
     DO J=1,3
       CALL ROMS_CUBIC_SLOPES(NLAY,ZD(:,J),RD(:,J),DZ(:,J),DR(:,J))
     END DO
     ! The deepest sampled column covers every bottom query. Its continuation
     ! supplies the deep stratification; shallow-column offsets join it C1.
     DONOR=MINLOC(ZD(NLAY,:),DIM=1)
     DO J=1,3
       DO ENDPT=1,2
         LEV=1
         IF(ENDPT == 2)LEV=NLAY
         IDX=1
         CALL ROMS_CUBIC_EVAL(NLAY,ZD(:,DONOR),RD(:,DONOR),DZ(:,DONOR), &
           DR(:,DONOR),ZD(LEV,J),IDX,B,DB)
         OFFSET(ENDPT,J)=RD(LEV,J)-B
         SLOPE(ENDPT,J)=DR(LEV,J)/DZ(LEV,J)-DB
       END DO
       WIDTH(1,J)=ZD(1,J)-ZD(2,J)
       WIDTH(2,J)=ZD(NLAY-1,J)-ZD(NLAY,J)
     END DO
     DO NODE=1,3
       LOWER=1
       DO K=1,NLAY
         Q=ZD(K,NODE)
         CALL ROMS_CUBIC_EVAL(NLAY,ZD(:,DONOR),RD(:,DONOR),DZ(:,DONOR), &
           DR(:,DONOR),Q,LOWER(DONOR),B,DB)
         DELTA=0.0_DP
         DO J=1,3
           IF(J == DONOR)CYCLE
           IF(J == NODE)THEN
             DELTA(J)=RD(K,J)-B
           ELSE IF(Q <= ZD(1,J) .AND. Q >= ZD(NLAY,J))THEN
             CALL ROMS_CUBIC_EVAL(NLAY,ZD(:,J),RD(:,J),DZ(:,J),DR(:,J), &
               Q,LOWER(J),V,DV)
             DELTA(J)=V-B
           ELSE
             ENDPT=1
             LEV=1
             SIGN_Z=1.0_DP
             IF(Q < ZD(NLAY,J))THEN
               ENDPT=2
               LEV=NLAY
               SIGN_Z=-1.0_DP
             END IF
             U=MIN(1.0_DP,SIGN_Z*(Q-ZD(LEV,J))/WIDTH(ENDPT,J))
             ! Bounded cubic continuation of the DIFFERENCE from the donor.
             ! Its derivative tends to zero over one native layer spacing.
             DELTA(J)=OFFSET(ENDPT,J)+SIGN_Z*SLOPE(ENDPT,J)*WIDTH(ENDPT,J) &
               *U*(1.0_DP-U+U*U/3.0_DP)
           END IF
         END DO
         MEAN_DELTA=SUM(DELTA)/3.0_DP
         RESULT(NODE,K)=REAL(DELTA(NODE)-MEAN_DELTA,SP)
       END DO
     END DO
     R=RESULT
     STATUS=0
   END SUBROUTINE TRIANGLE_CUBIC_RESIDUAL

   PURE SUBROUTINE ROMS_CUBIC_SLOPES(N,Z,R,DZ,DR)
     INTEGER, INTENT(IN) :: N
     REAL(DP), INTENT(IN) :: Z(N),R(N)
     REAL(DP), INTENT(OUT) :: DZ(N),DR(N)
     INTEGER :: K
     DZ(1:N-1)=Z(2:N)-Z(1:N-1)
     DR(1:N-1)=R(2:N)-R(1:N-1)
     DZ(N)=DZ(N-1)
     DR(N)=DR(N-1)
     DO K=N-1,2,-1
       DZ(K)=2.0_DP*DZ(K)*DZ(K-1)/(DZ(K)+DZ(K-1))
       ! Sign limiting avoids a dimensional density threshold: FVCOM stores
       ! density anomalies scaled by 1e-3 relative to ROMS rho in kg/m3.
       IF(DR(K)*DR(K-1) > 0.0_DP)THEN
         DR(K)=2.0_DP*DR(K)*DR(K-1)/(DR(K)+DR(K-1))
       ELSE
         DR(K)=0.0_DP
       END IF
     END DO
   END SUBROUTINE ROMS_CUBIC_SLOPES

   PURE SUBROUTINE ROMS_CUBIC_EVAL(N,Z,R,DZ,DR,Q,LOWER,V,SLOPE)
     INTEGER, INTENT(IN) :: N
     REAL(DP), INTENT(IN) :: Z(N),R(N),DZ(N),DR(N),Q
     INTEGER, INTENT(INOUT) :: LOWER
     REAL(DP), INTENT(OUT) :: V,SLOPE
     REAL(DP) :: AZ,BZ,AR,BR,T,LO,HI,F,DF,TNEW,DQ,TOL
     INTEGER :: K,IT
     ! Only the donor uses this tangent outside its own sampled interval.
     ! It covers all deep targets; its top continuation is normally <1 layer.
     IF(Q >= Z(1))THEN
       SLOPE=DR(1)/DZ(1)
       V=R(1)+(Q-Z(1))*SLOPE
       RETURN
     ELSE IF(Q <= Z(N))THEN
       SLOPE=DR(N)/DZ(N)
       V=R(N)+(Q-Z(N))*SLOPE
       RETURN
     END IF
     LOWER=MAX(1,MIN(N-1,LOWER))
     DO WHILE(LOWER > 1)
       IF(Q <= Z(LOWER))EXIT
       LOWER=LOWER-1
     END DO
     DO WHILE(LOWER < N-1)
       IF(Q >= Z(LOWER+1))EXIT
       LOWER=LOWER+1
     END DO
     K=LOWER
     AZ=2.0_DP*(Z(K)-Z(K+1))+DZ(K)+DZ(K+1)
     BZ=3.0_DP*(Z(K+1)-Z(K))-2.0_DP*DZ(K)-DZ(K+1)
     DQ=Q-Z(K)
     T=DQ/(Z(K+1)-Z(K))
     LO=0.0_DP
     HI=1.0_DP
     TOL=16.0_DP*EPSILON(1.0_DP)*MAX(1.0_DP,ABS(Z(K+1)-Z(K)))
     DO IT=1,52
       F=T*(DZ(K)+T*(BZ+T*AZ))-DQ
       IF(ABS(F) <= TOL)EXIT
       IF(F > 0.0_DP)THEN
         LO=T
       ELSE
         HI=T
       END IF
       DF=DZ(K)+T*(2.0_DP*BZ+3.0_DP*T*AZ)
       TNEW=T-F/DF
       IF(TNEW <= LO .OR. TNEW >= HI)TNEW=0.5_DP*(LO+HI)
       T=TNEW
     END DO
     AR=2.0_DP*(R(K)-R(K+1))+DR(K)+DR(K+1)
     BR=3.0_DP*(R(K+1)-R(K))-2.0_DP*DR(K)-DR(K+1)
     V=R(K)+T*(DR(K)+T*(BR+T*AR))
     SLOPE=(DR(K)+T*(2.0_DP*BR+3.0_DP*T*AR)) &
       /(DZ(K)+T*(2.0_DP*BZ+3.0_DP*T*AZ))
   END SUBROUTINE ROMS_CUBIC_EVAL

   SUBROUTINE BUILD_Z_REFERENCE(NCOL,NLAY,NZ,Z,R,TOP,BOTTOM,REFERENCE,STATUS, &
       LEVELS,MEAN_Z,COUNTS,N_USED)
     ! Serial RHO_PMEAN algorithm on the supplied set of columns. Production
     ! supplies three triangle corners; tests also supply the whole domain.
     INTEGER, INTENT(IN) :: NCOL,NLAY,NZ
     REAL(SP), INTENT(IN) :: Z(NCOL,NLAY),R(NCOL,NLAY),TOP(NCOL),BOTTOM(NCOL)
     REAL(SP), INTENT(OUT) :: REFERENCE(NCOL,NLAY)
     INTEGER, INTENT(OUT) :: STATUS
     REAL(SP), OPTIONAL, INTENT(OUT) :: LEVELS(:),MEAN_Z(:),COUNTS(:)
     INTEGER, OPTIONAL, INTENT(OUT) :: N_USED
     REAL(SP) :: PHY_Z(MAX(NZ,NCOL*(NLAY+2))),RHOZ(MAX(NZ,NCOL*(NLAY+2)))
     REAL(SP) :: RHOA(MAX(NZ,NCOL*(NLAY+2))),FCOUNT(MAX(NZ,NCOL*(NLAY+2)))
     REAL(SP) :: NEXT_Z(NCOL),VALUE
     INTEGER :: POSITION(NCOL),NGRID
     REAL(SP) :: ZM(NLAY),RHOS(NLAY),Z_TOP,Z_BOTTOM,DELTZ
     INTEGER :: COL,LEV

     STATUS=1
     REFERENCE=0.0_SP
     IF(PRESENT(LEVELS))LEVELS=0.0_SP
     IF(PRESENT(MEAN_Z))MEAN_Z=0.0_SP
     IF(PRESENT(COUNTS))COUNTS=0.0_SP
     IF(PRESENT(N_USED))N_USED=0
     IF(NCOL < 1 .OR. NLAY < 2 .OR. (NZ /= 0 .AND. NZ < 2))RETURN
     IF(ANY(TOP <= BOTTOM))RETURN
     IF(ANY(Z(:,1:NLAY-1) <= Z(:,2:NLAY)))RETURN
     IF(ANY(Z(:,1) > TOP) .OR. ANY(Z(:,NLAY) < BOTTOM))RETURN
     IF(NZ == 0)THEN
       ! Merge sorted native depths and physical boundaries, removing exact
       ! duplicates. Every back-interpolation target is itself a grid point.
       POSITION=0
       NGRID=0
       DO WHILE(ANY(POSITION <= NLAY+1))
         DO COL=1,NCOL
           IF(POSITION(COL) == 0)THEN
             NEXT_Z(COL)=TOP(COL)
           ELSE IF(POSITION(COL) <= NLAY)THEN
             NEXT_Z(COL)=Z(COL,POSITION(COL))
           ELSE IF(POSITION(COL) == NLAY+1)THEN
             NEXT_Z(COL)=BOTTOM(COL)
           ELSE
             NEXT_Z(COL)=-HUGE(1.0_SP)
           END IF
         END DO
         VALUE=MAXVAL(NEXT_Z)
         IF(NGRID == 0)THEN
           NGRID=1
           PHY_Z(NGRID)=VALUE
         ELSE IF(VALUE < PHY_Z(NGRID))THEN
           NGRID=NGRID+1
           PHY_Z(NGRID)=VALUE
         END IF
         WHERE(NEXT_Z == VALUE)POSITION=POSITION+1
       END DO
     ELSE
       ! Uniform grid follows the serial RHO_PMEAN construction exactly.
       NGRID=NZ
       Z_TOP=MAXVAL(TOP)
       Z_BOTTOM=MAXVAL(-BOTTOM)
       Z_BOTTOM=Z_BOTTOM+SPACING(Z_BOTTOM)
       IF(Z_TOP+Z_BOTTOM <= 0.0_SP)RETURN
       DELTZ=(Z_BOTTOM+Z_TOP)/REAL(NGRID-1,SP)
       DO LEV=1,NGRID
         PHY_Z(LEV)=Z_TOP-REAL(LEV-1,SP)*DELTZ
       END DO
       PHY_Z(1)=NEAREST(Z_TOP,-1.0_SP)
       PHY_Z(NGRID)=NEAREST(-Z_BOTTOM,1.0_SP)
     END IF
     IF(ANY(PHY_Z(1:NGRID-1) <= PHY_Z(2:NGRID)))RETURN
     IF(PRESENT(LEVELS))THEN
       IF(SIZE(LEVELS) < NGRID)RETURN
       LEVELS(1:NGRID)=PHY_Z(1:NGRID)
     END IF
     IF(PRESENT(MEAN_Z))THEN
       IF(SIZE(MEAN_Z) < NGRID)RETURN
     END IF
     IF(PRESENT(COUNTS))THEN
       IF(SIZE(COUNTS) < NGRID)RETURN
     END IF
     IF(PRESENT(N_USED))N_USED=NGRID
     RHOA=0.0_SP
     FCOUNT=0.0_SP
     DO COL=1,NCOL
       ZM=Z(COL,:)
       RHOS=R(COL,:)
       CALL INTERPOLATE_Z_FAST(ZM,RHOS,PHY_Z(1:NGRID),RHOZ(1:NGRID),NLAY,NGRID)
       DO LEV=1,NGRID
         IF(BOTTOM(COL) <= PHY_Z(LEV) .AND. TOP(COL) >= PHY_Z(LEV))THEN
           FCOUNT(LEV)=FCOUNT(LEV)+1.0_SP
           RHOA(LEV)=RHOA(LEV)+RHOZ(LEV)
         END IF
       END DO
     END DO
     IF(PRESENT(COUNTS))COUNTS(1:NGRID)=FCOUNT(1:NGRID)
     IF(ANY(FCOUNT(1:NGRID) < 1.0_SP))THEN
       STATUS=2
       RETURN
     END IF
     RHOA(1:NGRID)=RHOA(1:NGRID)/FCOUNT(1:NGRID)
     IF(PRESENT(MEAN_Z))MEAN_Z(1:NGRID)=RHOA(1:NGRID)
     DO COL=1,NCOL
       ZM=Z(COL,:)
       CALL INTERPOLATE_Z_FAST(PHY_Z(1:NGRID),RHOA(1:NGRID),ZM,RHOS,NGRID,NLAY)
       REFERENCE(COL,:)=RHOS
     END DO
     STATUS=0
   END SUBROUTINE BUILD_Z_REFERENCE

   PURE SUBROUTINE INTERPOLATE_Z_FAST(X,A,Y,B,NSOURCE,NTARGET)
     ! SINTER_EXTRP_NONE values and arithmetic, for strictly descending input
     ! and descending queries. A single interval walk costs O(NSOURCE+NTARGET).
     INTEGER, INTENT(IN) :: NSOURCE,NTARGET
     REAL(SP), INTENT(IN) :: X(NSOURCE),A(NSOURCE),Y(NTARGET)
     REAL(SP), INTENT(OUT) :: B(NTARGET)
     INTEGER :: TARGET,LOWER
     LOWER=1
     DO TARGET=1,NTARGET
       IF(Y(TARGET) > X(1))THEN
         B(TARGET)=A(1)
       ELSE IF(Y(TARGET) < X(NSOURCE))THEN
         B(TARGET)=A(NSOURCE)
       ELSE
         DO WHILE(LOWER < NSOURCE-1)
           IF(Y(TARGET) > X(LOWER+1))EXIT
           LOWER=LOWER+1
         END DO
         B(TARGET)=A(LOWER)-(A(LOWER)-A(LOWER+1)) &
           *(X(LOWER)-Y(TARGET))/(X(LOWER)-X(LOWER+1))
       END IF
     END DO
   END SUBROUTINE INTERPOLATE_Z_FAST

   END SUBROUTINE BAROPG_SHEAR_CORE
!==============================================================================|

SUBROUTINE BAROPG
  USE ALL_VARS, ONLY: SP,N,MT,NT,KB,KBM1,RHO1,RHO,RMEAN1,RMEAN, &
    BAROCLINIC_PRESSURE_GRADIENT,DZ1,DRHOX,DRHOY
  IMPLICIT NONE
  REAL(SP), ALLOCATABLE :: RN(:,:,:), RC(:,:,:)
  REAL(SP) :: BX(N),BY(N),SX,SY,SW
  INTEGER :: I,K

  IF (BAROCLINIC_PRESSURE_GRADIENT /= 'sigma levels v2 shear') THEN
    CALL BAROPG_SHEAR_CORE
    RETURN
  END IF

  ! Both forces use identical density, reference and geometry.
  ALLOCATE(RN(0:MT,KB,2),RC(0:NT,KB,2))
  RN(:,:,1)=RHO1
  RN(:,:,2)=RMEAN1
  RC(:,:,1)=RHO
  RC(:,:,2)=RMEAN
  BAROCLINIC_PRESSURE_GRADIENT='sigma levels'
  CALL BAROPG_SHEAR_CORE
  DO I=1,N
    BX(I)=SUM(DRHOX(I,1:KBM1))
    BY(I)=SUM(DRHOY(I,1:KBM1))
  END DO

  RHO1=RN(:,:,1)
  RMEAN1=RN(:,:,2)
  RHO=RC(:,:,1)
  RMEAN=RC(:,:,2)
  BAROCLINIC_PRESSURE_GRADIENT='sigma levels local mean'
  CALL BAROPG_SHEAR_CORE
  BAROCLINIC_PRESSURE_GRADIENT='sigma levels v2 shear'
  RHO1=RN(:,:,1)
  RMEAN1=RN(:,:,2)
  RHO=RC(:,:,1)
  RMEAN=RC(:,:,2)
  DEALLOCATE(RN,RC)

  ! DRHO already contains DZ1. Project only the added column force.
  DO I=1,N
    SX=SUM(DRHOX(I,1:KBM1))-BX(I)
    SY=SUM(DRHOY(I,1:KBM1))-BY(I)
    IF (SX == 0.0_SP .AND. SY == 0.0_SP) CYCLE
    SW=SUM(DZ1(I,1:KBM1))
    IF (SW <= 0.0_SP) CYCLE
    SX=SX/SW
    SY=SY/SW
    DO K=1,KBM1
      DRHOX(I,K)=DRHOX(I,K)-DZ1(I,K)*SX
      DRHOY(I,K)=DRHOY(I,K)-DZ1(I,K)*SY
    END DO
  END DO
END SUBROUTINE BAROPG
