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
   
# if defined (ICESHELF)
    USE MOD_ISF
# endif


   IMPLICIT NONE
   REAL(SP) :: RIJK(0:N,3,KBM1), DRIJK1(0:N,3,KBM1), DRIJK2(0:N,KBM1)
   REAL(SP) :: LOCAL_RHO(3,KBM1), LOCAL_RHO_C(KBM1)
   REAL(DP) :: LOCAL_Z(3,KBM1), LOCAL_RAW(3,KBM1), LOCAL_REF(3,KBM1)
   REAL(DP) :: LOCAL_TOP(3), LOCAL_BOTTOM(3)
   LOGICAL :: USE_DEEP_REFERENCE
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

   USE_DEEP_REFERENCE = BAROCLINIC_PRESSURE_GRADIENT == 'sigma levels deep reference'
# if defined (ICESHELF)
   IF(USE_DEEP_REFERENCE) CALL FATAL_ERROR('Deep reference is not implemented for ICESHELF')
# endif
   IF(.NOT.USE_DEEP_REFERENCE) THEN
     RHO1(:,1:KBM1) = RHO1(:,1:KBM1) - RMEAN1(:,1:KBM1)
     RHO = RHO - RMEAN
   END IF

!----------INITIALIZE ARRAYS---------------------------------------------------!

   DRHOX      = 0.0_SP
   DRHOY      = 0.0_SP
   IF(.NOT.USE_DEEP_REFERENCE) THEN
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

   IF(USE_DEEP_REFERENCE) THEN
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
       CALL TRIANGLE_DEEP_REFERENCE(KBM1,LOCAL_Z,LOCAL_RAW,LOCAL_TOP,LOCAL_BOTTOM,LOCAL_REF,LOCAL_STATUS)
       IF(LOCAL_STATUS/=0) CALL FATAL_ERROR('Invalid wet column in deep reference')
       LOCAL_RHO=REAL(LOCAL_RAW,SP)-REAL(LOCAL_REF,SP)
       LOCAL_RHO_C=RHO(I,1:KBM1)-ONE_THIRD*(REAL(LOCAL_REF(1,:),SP) &
         +REAL(LOCAL_REF(2,:),SP)+REAL(LOCAL_REF(3,:),SP))
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

   IF(.NOT.USE_DEEP_REFERENCE) THEN
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

   IF(.NOT.USE_DEEP_REFERENCE) THEN
     RHO1 = RHO1 + RMEAN1
     RHO  = RHO  + RMEAN
   END IF

   IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "End: baropg.F"

   RETURN
   CONTAINS

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
