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
! A SIMPLE SEA ICE MODULE FOR NORWEGIAN COAST
!/===========================================================================/

MODULE MOD_ICENORKYST
# if defined (ICENORKYST)   
   USE ALL_VARS
   USE MOD_PAR
   USE MOD_PREC
   IMPLICIT NONE
   SAVE

 
!=======================================================

!=========================================================
! DEFINE VARIABLES USED GLOBALLY BY ICENORKYST  MODULE
  
!========================================================
   REAL (SP), DIMENSION(:),ALLOCATABLE, SAVE, TARGET :: FRZMLT,SSS,SST,TF,FHNET,FRESH
  !-------SET CONSTANT-------------------------------------
    REAL(DP),PARAMETER :: depressT  = 0.054_DP  !! Tf:brine salinity ratio(C/ppt)
    REAL(DP),PARAMETER :: Lfresh = 3.34E5_DP !! Latent heat of fusion of (J/kg)
    REAL(DP),PARAMETER :: cp_ocn = 4218._DP    !! specific heat of ocn(J/kg/K)
    REAL(DP),PARAMETER :: hmix =10.0_DP        !! constant mixed layer depth (m)
    REAL(DP),PARAMETER :: rhow=1026.0_DP       !! density of seawater (kg/m^3)
    REAL(DP),PARAMETER :: albocn=0.5_DP        !! albedo
    REAL(DP),PARAMETER :: DRAGW = 0.0055_SP*rhow !! drag coefficient for water on ice
    REAL(DP),PARAMETER :: SPRO=cp_ocn*rhow !!

!========================================================================

 CONTAINS

SUBROUTINE ICENORKYST_INIT_0
!------------------ALLOCATE VARIABLES
   ALLOCATE(SSS(0:MT))    ;  SSS   =ZERO  ! salinity in the ocean(ppt)
   ALLOCATE(SST(0:MT))    ;  SST    =ZERO  ! temperature in the ocean (C)
   ALLOCATE(FRZMLT(0:MT))   ;  FRZMLT   =ZERO  ! freezing/melting potential
   ALLOCATE(FHNET(0:MT))  ;   FHNET =ZERO ! heat loss due to freezing/melting
   ALLOCATE(FRESH(0:MT))  ;  FRESH  =ZERO ! freshwater flux
   ALLOCATE(TF(0:MT))    ;   TF =ZERO   ! freezing point
 END SUBROUTINE ICENORKYST_INIT_0
!=================================================================
!==========================================================================
 
SUBROUTINE ICE_NORKYST
  
  USE ALL_VARS
   USE MOD_PAR
   USE MOD_PREC
  IMPLICIT NONE


  
   INTEGER :: I,II,K
!-------SET CONSTANT-------------------------------------
  !  REAL(DP),PARAMETER :: depressT  = 0.054_DP  !! Tf:brine salinity ratio(C/ppt)
  !  REAL(DP),PARAMETER :: Lfresh = 3.34E5_DP !! Latent heat of fusion of (J/kg)
  !  REAL(DP),PARAMETER :: cp_ocn = 4218._DP    !! specific heat of ocn(J/kg/K)
  !  REAL(DP),PARAMETER :: hmix =10.0_DP        !! constant mixed layer depth (m)
  !  REAL(DP),PARAMETER :: rhow=1026.0_DP       !! density of seawater (kg/m^3)
  !  REAL(DP),PARAMETER :: albocn=0.5_DP        !! albedo
  !  REAL(DP),PARAMETER :: DRAGW = 0.0055_SP*rhow !! drag coefficient for water on ice
  !  REAL(DP),PARAMETER :: SPRO=cp_ocn*rhow !!


! initialize variables
  SST=0.0_sp
  SSS=0.0_sp
  FRZMLT=0.0_sp
  FHNET=0.0_sp  
  TF=0.0_sp

! assign values

  SST = T1(:,1);
  SSS = S1(:,1);
  Tf =-depressT*SSS ! freezing temp
  WHERE(Tf<-1.85_sp) Tf=-1.85_sp

!compute potential to freeze or melt ice

DO I =1,M
  
  FRZMLT(I) = (SST(I)-Tf(I))*cp_ocn*hmix/dti
  FRZMLT(I) = max(min(FRZMLT(I),1.0_sp),-1.0_sp)
 
IF (ZICE(I)>0 .and. FRZMLT(I)>0 ) THEN   ! ice can melt

  FRESH(I) = FRZMLT(I)/Lfresh    ! fresh water flux (m/s), positive to ocean
  FHNET(I) = FRZMLT(I)*rhow/SPRO      ! should be (-)*(-), to match the sigh of WTSURF
  QPREC(I)=QPREC(I)+FRESH(I)     ! positive melting

ELSEIF (FRZMLT(I)<0)  THEN            ! ice can grow

 FRESH(I) = FRZMLT(I)/Lfresh    ! fresh water flux (m/s), positive to ocean
  FHNET(I) = FRZMLT(I)*rhow/SPRO      ! should be (-)*(-), to match the sigh of WTSURF
  QPREC(I)=QPREC(I)+FRESH(I)     ! positive melting
ELSE

FRZMLT(I)=0
FRESH(I)=0
FHNET(I)=0
END IF

END DO


  ZICE=ZICE-FRESH*dti            ! ice thickness


#  if defined (MULTIPROCESSOR)
     IF(PAR)CALL NODE_MATCH(1,NBN,BN_MLT,BN_LOC,BNC,MT,1,MYID,NPROCS,ZICE)
#  endif

 CALL N2E2D(ZICE,ZICE1)



# if defined(MULTIPROCESSOR)
  IF(PAR)THEN
    CALL AEXCHANGE(NC,MYID,NPROCS,ZICE)
    CALL AEXCHANGE(EC,MYID,NPROCS,ZICE1)
  END IF
# endif

WHERE(ZICE>0.05_SP)    SWRAD=(1-albocn)*SWRAD

WHERE(ZICE<=0.01_SP) 
              WTSURF=WTSURF+FHNET
ELSEWHERE(ZICE>0.01_SP.and.ZICE<=0.1_SP)
              WTSURF=WTSURF*0.75+FHNET
ELSEWHERE(ZICE>0.1_SP.and.ZICE<=0.2_SP)
              WTSURF=WTSURF*0.4375+FHNET
ELSEWHERE(ZICE>0.2_SP.and.ZICE<=0.4_SP)
              WTSURF=WTSURF*0.25+FHNET
ELSEWHERE(ZICE>0.4_SP.and.ZICE<=0.8_SP)
             WTSURF=WTSURF*0.125+FHNET

ELSEWHERE(ZICE>0.8_SP)
             WTSURF=FHNET
            
ENDWHERE



# if defined (MULTIPROCESSOR)
       IF(PAR) CALL NODE_MATCH(1,NBN,BN_MLT,BN_LOC,BNC,MT,1,MYID,NPROCS,WTSURF,SWRAD)
       IF(PAR) CALL AEXCHANGE(NC,MYID,NPROCS,WTSURF,SWRAD)
# endif

 DO I=1,M
         IF(T1(I,1) .LT. Tf(I)) THEN
           T1(I,1) = Tf(I)
         END IF

         DO K=2,KBM1
         IF(T1(I,K) .LT. Tf(I)) THEN
           T1(I,K) = Tf(I)
         END IF
         END DO

  END DO
# if defined (MULTIPROCESSOR)
       IF(PAR) CALL AEXCHANGE(NC,MYID,NPROCS,T1)
# endif

      CALL N2E3D(T1,T)                               !Shift to Elements
!

# if defined (MULTIPROCESSOR)
       if(PAR) CALL NODE_MATCH(1,NBN,BN_MLT,BN_LOC,BNC,MT,1,MYID,NPROCS,QPREC)
       IF(PAR) CALL AEXCHANGE(NC,MYID,NPROCS,QPREC)

#  if !defined (SEMI_IMPLICIT)
       QPREC2=QPREC
       if(PAR) CALL NODE_MATCH(1,NBN,BN_MLT,BN_LOC,BNC,MT,1,MYID,NPROCS,QPREC2)
       IF(PAR) CALL AEXCHANGE(NC,MYID,NPROCS,QPREC2)
#  endif
# endif

END SUBROUTINE ICE_NORKYST
# endif

!==================================================

END MODULE  MOD_ICENORKYST



