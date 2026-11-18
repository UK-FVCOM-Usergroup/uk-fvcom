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
   
# if defined (ICESHELF)
    USE MOD_ISF
    USE MOD_UTILS, ONLY: FATAL_ERROR
# endif


   IMPLICIT NONE
   REAL(SP) :: RIJK(0:N,3,KBM1), DRIJK1(0:N,3,KBM1), DRIJK2(0:N,KBM1)
   REAL(SP) :: LOCAL_Z(3,KBM1), LOCAL_RHO(3,KBM1), LOCAL_RHO_C(KBM1)
   REAL(SP) :: TEMP,DIJ,DRHO1,DRHO2
   INTEGER  :: I,K,J,J1,J2,IJK
   LOGICAL  :: USE_LOCAL_MEAN, USE_LOCAL_Z_MEAN, APPLY_V2
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

   APPLY_V2 = BAROCLINIC_PRESSURE_GRADIENT == 'sigma levels local mean' .OR. &
     BAROCLINIC_PRESSURE_GRADIENT == 'sigma levels local z mean v2'
   USE_LOCAL_Z_MEAN = BAROCLINIC_PRESSURE_GRADIENT == 'sigma levels local z mean' .OR. &
     BAROCLINIC_PRESSURE_GRADIENT == 'sigma levels local z mean v2'
   USE_LOCAL_MEAN = USE_LOCAL_Z_MEAN .OR. APPLY_V2
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
       IF(USE_LOCAL_Z_MEAN)THEN
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
       IF(APPLY_V2)CALL REMOVE_TRIANGLE_MEAN(LOCAL_Z,LOCAL_RHO)
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
