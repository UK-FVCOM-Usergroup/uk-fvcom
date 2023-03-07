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

MODULE MOD_ISF
# if defined (ICESHELF)   
   USE ALL_VARS
   USE MOD_PREC
   USE MOD_PAR
   USE SINTER
   IMPLICIT NONE
   SAVE
    
		 
 !-----variables controlling ice shelf mask----------------!
   INTEGER , ALLOCATABLE :: ISISFN(:)           !!  ICESHELF MASK FOR NODE,
   INTEGER , ALLOCATABLE :: ISISFC(:)           !!  ICESHELF MASK FOR CELL
!-----variables used globally by iceshelf module   
   REAL (SP), DIMENSION(:),ALLOCATABLE, SAVE, TARGET ::  S_w, T_w, U2_w, U2,&
                           T_b, S_b, P_b, MLTFRZ,  NHF_isf,NSF_isf
             
!==============================================================================

 CONTAINS  !----------------------------------------------------------------------------------| 
           ! ISF_INIT_0                  : Initialize global variables in iceshelf module     |	
           ! SET_ISF_INI                 : Initialize arrays for iceshelf module              |
           !                               for coldstart                                      |
           ! ISF_JUDGE                   : Determine if nodes/elements are under iceshelf     |
           !                             : cavity or not                                      |
           ! ISF_COUPLING                : Determine fresh/heat flux at the ice/ocean boundary|
           !                             : and meltrate                                       |
		   

!==============================================================================|
!===========================================================================
   SUBROUTINE ISF_INIT_0
!-- for thermodyamics component

! from ocean

   ALLOCATE(S_w(0:MT))    ;  S_w   =ZERO  ! salinity in the ocean(ppt)
   ALLOCATE(T_w(0:MT))    ;  T_w    =ZERO  ! temperature in the ocean (C)
   ALLOCATE(U2_w(0:MT))   ;  U2_w   =ZERO  ! square of ocean velocity on nodes(m^2/s^2)
                                         ! NOTE, USE E2N2D to convert 
   ALLOCATE(U2(0:NT))    ;  U2 = ZERO
! in the ice-shelf interface

  ALLOCATE(T_b(0:MT)) ;  T_b  =ZERO    ! temperature at the ice-shelf interface,
                                      !also the freezing temperature(C)
  ALLOCATE(S_b(0:MT)) ;  S_b  =ZERO    ! salinity at the ice-shelf interface
  ALLOCATE(P_b(0:MT)) ;  P_b  =ZERO    ! pressure at the ice-shelf interface
  ALLOCATE(MLTFRZ(0:MT)); MLTFRZ  =ZERO  ! freezing/melting potential for ice shelf ()

! to the ocean
 
   ALLOCATE(NHF_isf(0:MT)); NHF_isf=ZERO    ! net heat flux due to melting to the ocean
   ALLOCATE(NSF_isf(0:MT)); NSF_isf=ZERO    ! net salt flux due to melting to the ocean
!==============================================================================|
 END SUBROUTINE ISF_INIT_0
!==============================================================================|


!=============================================================================|


   SUBROUTINE SET_ISF_INI 
!------------------------------------------------------------------------------|
!  INITIALIZE ARRAYS USED FOR ICE SHELF  TREATMENT FOR COLD START                                |
!------------------------------------------------------------------------------|

   USE ALL_VARS
   USE MOD_PAR
   IMPLICIT NONE
   INTEGER :: I
   REAL(SP) :: DTMP
   ALLOCATE(ISISFN(0:MT)); ISISFN = ZERO !
   ALLOCATE(ISISFC(0:NT)); ISISFC = ZERO !


   IF (DBG_SET(DBG_SBR)) WRITE(IPT,*) "START: SET_ISF_INI"

   IF(STARTUP_TYPE == STARTUP_TYPE_COLDSTART) THEN

 !--Determine If Node Points Are Iceshelf Based on ZISF Threshold---------------!
!
   ISISFN(:) = 0
   ISISFC(:) = 0
   
   WHERE(ZISF>0.0_SP) ISISFN=1

# if defined (FAKEICEDRAFT)
   WHERE(ZISF<1.01_SP) ISISFN = -1
# endif
 

!
!--Determine if Cells are Iceshelf  Based on ZISF !Threshold---------------------!
!
   DO I = 1, N
     DTMP =  MIN(ZISF(NV(I,1)),ZISF(NV(I,2)),ZISF(NV(I,3)))
     IF(DTMP > 0.0_SP )    ISISFC(I) = 1
# if defined (FAKEICEDRAFT)
     IF (DTMP<1.01_SP)    ISISFC(I)  = -1  
# endif
   END DO
   
  ENDIF ! coldstart

#    if defined (MULTIPROCESSOR)
       IF(PAR)CALL AEXCHANGE(EC,MYID,NPROCS,ISISFC)
      IF(PAR)CALL AEXCHANGE(NC,MYID,NPROCS,ISISFN)
#    endif


   IF (DBG_SET(DBG_SBR)) WRITE(IPT,*) "END: SET_ISF_INI"
   RETURN
   END SUBROUTINE SET_ISF_INI

!==============================================================================|
!==============================================================================|

   SUBROUTINE ISF_JUDGE

!------------------------------------------------------------------------------|
!  DETERMINE IF NODES/ELEMENTS ARE ICESHELF OR NOT                                  |
!------------------------------------------------------------------------------|

   USE MOD_PREC
   USE ALL_VARS
   USE MOD_PAR
   IMPLICIT NONE
   REAL(SP) :: DTMP
   INTEGER  :: I
   
   IF (DBG_SET(DBG_SBR)) WRITE(IPT,*) "START: ISF_JUDGE"
!
!--Determine If Node Points Are Iceshelf Based on Atmospheric Pressure Threshold---------------!
!
    !--Determine If Node Points Are Iceshelf Based on ZISF
    !Threshold---------------!
!
   ISISFN(:) = 0
   ISISFC(:) = 0

   WHERE(ZISF>0.0_SP) ISISFN=1

# if defined (FAKEICEDRAFT)
   WHERE(ZISF<1.01_SP) ISISFN = -1
# endif

!
!--Determine if Cells are Iceshelf  Based on ZISF
!!Threshold---------------------!
!
   DO I = 1, N
     DTMP =  MIN(ZISF(NV(I,1)),ZISF(NV(I,2)),ZISF(NV(I,3)))
     IF( DTMP >0.0_SP)    ISISFC(I) = 1     
# if defined (FAKEICEDRAFT)
     IF (DTMP<1.01_SP)    ISISFC(I)  = -1
# endif

   END DO

#    if defined (MULTIPROCESSOR)
      IF(PAR)CALL AEXCHANGE(EC,MYID,NPROCS,ISISFC)
      IF(PAR)CALL AEXCHANGE(NC,MYID,NPROCS,ISISFN)
#    endif
 

   IF (DBG_SET(DBG_SBR)) WRITE(IPT,*) "END: ISF_JUDGE"
   RETURN
   END SUBROUTINE ISF_JUDGE


!==============================================================================|
!==============================================================================|

  SUBROUTINE ISF_COUPLING
!------------------------------------------------------------------------------|
!          DETERMINE MELT RATE FOR ICESHELF, FRESHWATER AND HEAT FLUX TO OCEAN
!------------------------------------------------------------------------------|
  
! read input data from ocean

   USE MOD_PREC
   USE ALL_VARS
   USE MOD_PAR

  USE MOD_WD, ONLY: ISWETN

 IMPLICIT NONE
    
    INTEGER :: I,K,J
    REAL(SP)           :: COEF_a,SPCP,SPRO,TEMP1,TEMP2,TEMP3, GAMA_S
    REAL(SP),DIMENSION(:), ALLOCATABLE, TARGET:: COEF_b, COEF_c,U2,DTMP
!-------SET CONSTANT-------------------------------------
    REAL(DP),PARAMETER :: C_w  = 3974_DP  !! Specific heat capacity of seawater(J/kg/C)
    REAL(DP),PARAMETER :: L_isf = 3.34E5_DP !! Latent heat of fusion of (J/kg)
    REAL(DP),PARAMETER :: LAMDA1 = -0.0573_DP !! Liquidus slope (C/PSU)
    REAL(DP),PARAMETER :: LAMDA2 = 0.0832_DP  !! Liquidus inercept (C/degree)
    REAL(DP),PARAMETER :: LAMDA3 =-7.53E-8_DP   !!Liquidus pressure coefficient(C/pa) 
    REAL(DP),PARAMETER :: U_tide =0.01_DP  !! RMS velocity associated with tides
    REAL(DP),PARAMETER :: CD_top =2.5E-3_DP !! TOP drag coefficient
    REAL(DP),PARAMETER :: RHO_sw= 1.028E3_DP !! Reference sea water density
    REAL(DP),PARAMETER :: RHO_fw= 1.0E3_DP !! Reference density of fresh water

!------ALLOCATE VARIABLES----------------------
    ALLOCATE(COEF_b(0:MT));   COEF_b=ZERO  ! coefficient for resolve S_b
    ALLOCATE(COEF_C(0:MT));  COEF_c=ZERO   ! coefficient for resolve S_b
    ALLOCATE(U2(0:NT));       U2=ZERO   ! 
    
 
! initialize the variables
  GAMA_S=GAMA_T/35.0_DP
  S_w(:)  = 0.0_SP
  T_w(:)  = 0.0_SP
  U2(:)   = 0.0_SP  ! velocity magnitude square  of surface layer (or far field characterics) on cell
  U2_w(:)  = 0.0_SP ! U2 on node
  T_b(:)  = 0.0_SP  ! potential temperature at the interface of ice and ocean
  S_b(:)  = 0.0_SP
  P_b(:)  = 0.0_SP
  M_w(:)  = 0.0_SP
  MLTFRZ(:)  = 0.0_SP
  COEF_b(:)  =0.0_SP
  COEF_c(:) =0.0_SP
  NHF_isf(:) =0.0_SP
  NSF_isf(:) =0.0_SP 
 
! assign values

  S_w(:) = S1(:,1)
  T_w(:) = T1(:,1)
  T_b(:) = T1(:,1)
  S_b(:) = S1(:,1)  

 DO I = 1,N
 U2(I) = U(I,1)**2+V(I,1)**2
 END DO

# if defined (MULTIPROCESSOR)
   IF(PAR) CALL AEXCHANGE(EC, MYID,NPROCS,U2)
# endif 

 CALL E2N2D(U2,U2_W)

# if defined (MULTIPROCESSOR)
   IF(PAR) CALL AEXCHANGE(NC, MYID,NPROCS,U2_W)
   IF(PAR) CALL AEXCHANGE(NC, MYID,NPROCS,S_w,S_b)
   IF(PAR) CALL AEXCHANGE(NC, MYID,NPROCS,T_w,T_b)
# endif


!diagnose Sb, Tb, nhet_isf, melt-freeze potential
  
 SPCP=4.2174E3_SP
 SPRO=SPCP*RHO_sw
 
 
  P_b(:)=GRAV*ZISF(:)*((RHO1(:,1)*1000+1000)-0.5_SP*DRDZ*ZISF(:))

   
 COEF_a=C_w*GAMA_T*LAMDA1

  DO I = 1,M

    IF(ABS(ISISFN(I))==1) THEN
!! calculate the freezing temperature and melting rate

   COEF_b(I)=C_w*GAMA_T*(LAMDA2+LAMDA3*P_b(I)-T_w(I))-L_isf*GAMA_S
   COEF_c(I)=L_isf*GAMA_S*S_w(I)
   S_b(I)=(-COEF_b(I)-SQRT(COEF_b(I)*COEF_b(I)-4*COEF_a*COEF_c(I)))/COEF_a/2
   T_b(I)=LAMDA1*S_b(I)+LAMDA2+LAMDA3*P_b(I)
   MLTFRZ(I)=C_w*SQRT(CD_top*(U2_W(I)+U_tide*U_tide))*GAMA_T*(T_w(I)-T_b(I))*RHO_sw
   M_w(I)=MLTFRZ(I)/(L_isf*RHO_fw)  ! melt rate 

# if defined(WET_DRY)
  IF(ISWETN(I) == 0) M_w(I) = 0.0_sp
# endif
 ! heat flux
  
   NHF_isf(I)=(MLTFRZ(I)+RHO_fw*C_w*M_w(I)*(T_w(I)-T_b(I)))/SPRO      ! dividing SPRO to match the WTSURF, WTSURF is changed sign before vdif_ts is called
   NSF_isf(I)=-(RHO_sw*SQRT(CD_top*(U2_W(I)+U_tide*U_tide))*GAMA_S+RHO_fw*M_w(I))*(S_w(I)-S_b(I)) ! ocean losing salt when melting
   NSF_isf(I)=NSF_isf(I)*0.001_SP     ! convert to kg/m^2/s
 
  ENDIF

 END DO


# if defined (MULTIPROCESSOR)
       IF(PAR) CALL NODE_MATCH(1,NBN,BN_MLT,BN_LOC,BNC,MT,1,MYID,NPROCS,NHF_isf,NSF_isf)
       IF(PAR) CALL AEXCHANGE(NC,MYID,NPROCS,NHF_isf,NSF_isf)
       IF(PAR) CALL NODE_MATCH(1,NBN,BN_MLT,BN_LOC,BNC,MT,1,MYID,NPROCS,M_w)
       IF(PAR) CALL AEXCHANGE(NC,MYID,NPROCS,M_w)

# endif

 
!!======================================================
!!======================================================
 END SUBROUTINE ISF_COUPLING

!==============================================================================|
!==============================================================================|
# endif
END MODULE  MOD_ISF
