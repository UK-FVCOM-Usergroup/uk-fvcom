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
   
# if defined (ICESHELF)
    USE MOD_ISF
    USE MOD_UTILS, ONLY: FATAL_ERROR
# endif


   IMPLICIT NONE
   REAL(SP) :: RIJK(0:N,3,KBM1), DRIJK1(0:N,3,KBM1), DRIJK2(0:N,KBM1)
   REAL(SP) :: LOCAL_Z(3,KBM1), LOCAL_RHO(3,KBM1), LOCAL_RHO_C(KBM1)
   REAL(SP) :: TEMP,DIJ,DRHO1,DRHO2
   INTEGER  :: I,K,J,J1,J2,IJK
   LOGICAL  :: USE_LOCAL_MEAN
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

   USE_LOCAL_MEAN = BAROCLINIC_PRESSURE_GRADIENT == 'sigma levels local mean'
#  if defined (ICESHELF)
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
       DO J=1,3
         IJK=NV(I,J)
         LOCAL_Z(J,:) = ZZ(IJK,1:KBM1)*DT(IJK)+ET(IJK)
#        if defined (ICE_EMBEDDING)
         LOCAL_Z(J,:) = LOCAL_Z(J,:)-QTHICE(IJK)
#        endif
         LOCAL_RHO(J,:) = RHO1(IJK,1:KBM1)
       END DO

       ! One reference rho_ref(z) for this triangle, evaluated at each node's
       ! physical layer depths. Shared nodes have different residuals in
       ! different triangles; the model density arrays are not changed.
       CALL REMOVE_TRIANGLE_MEAN(LOCAL_Z,LOCAL_RHO)
       LOCAL_RHO_C = ONE_THIRD*(LOCAL_RHO(1,:)+LOCAL_RHO(2,:)+LOCAL_RHO(3,:))
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

   PURE SUBROUTINE REMOVE_TRIANGLE_MEAN(Z,R)
     ! Define a single physical-depth reference as the arithmetic mean of
     ! three piecewise-linear nodal profiles, with linear end extrapolation.
     ! Evaluating this function at each nodal depth is equivalent to forming
     ! it on the union of all three sets of z levels and interpolating back.
     ! A sigma-index mean is not a horizontally uniform physical-depth reference.
     REAL(SP), INTENT(IN)    :: Z(3,KBM1)
     REAL(SP), INTENT(INOUT) :: R(3,KBM1)
     REAL(SP) :: RESIDUAL(3,KBM1), DZ_PROFILE, WEIGHT
     INTEGER  :: NODE, SOURCE, LEVEL, LOWER

     RESIDUAL=0.0_SP
     DO SOURCE=1,3
       DO NODE=1,3
         IF(NODE == SOURCE)CYCLE
         LOWER=1
         DO LEVEL=1,KBM1
           IF(KBM1 == 1)THEN
             RESIDUAL(NODE,LEVEL)=RESIDUAL(NODE,LEVEL)+R(NODE,LEVEL)-R(SOURCE,1)
           ELSE
             ! Both depth arrays descend, so this walk costs O(KBM1).
             DO WHILE(LOWER < KBM1-1)
               IF(Z(NODE,LEVEL) >= Z(SOURCE,LOWER+1))EXIT
               LOWER=LOWER+1
             END DO
             DZ_PROFILE=Z(SOURCE,LOWER)-Z(SOURCE,LOWER+1)
             WEIGHT=0.0_SP
             IF(DZ_PROFILE > 0.0_SP)WEIGHT=(Z(SOURCE,LOWER)-Z(NODE,LEVEL))/DZ_PROFILE
             ! Accumulate differences, avoiding an extra large mean followed
             ! by subtraction. The source node's own contribution is zero.
             RESIDUAL(NODE,LEVEL)=RESIDUAL(NODE,LEVEL) &
               + (R(NODE,LEVEL)-R(SOURCE,LOWER)) &
               - WEIGHT*(R(SOURCE,LOWER+1)-R(SOURCE,LOWER))
           END IF
         END DO
       END DO
     END DO
     R=ONE_THIRD*RESIDUAL
   END SUBROUTINE REMOVE_TRIANGLE_MEAN

   END SUBROUTINE BAROPG
!==============================================================================|
