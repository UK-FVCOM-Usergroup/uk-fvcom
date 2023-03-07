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
! THIS MODULE is developed by QIN ZHOU (APN) & TORE HATTERMANN (NPI)
!/---------------------------------------------------------------------------/
! CVS VERSION INFORMATION
! $Id$
! $Name$
! $Revision$
!/===========================================================================/

MODULE MOD_ICENUDGE
# if defined (ICENUDGE)   
   USE ALL_VARS
   USE MOD_PREC
   USE MOD_PAR
# if defined (ICESHELF)
   USE MOD_ISF
# endif 
   IMPLICIT NONE
   SAVE
 
    !-------SET CONSTANTS-------------------------------------
    REAL(DP),PARAMETER :: cp_ocn  = 4218._DP  !! Specific heat capacity of ocean(J/kg/C)
    REAL(DP),PARAMETER :: c_h   = 0.006_DP  !! heat transfer coefficient 
    REAL(DP),PARAMETER :: Lfresh = 3.34E5_DP !! Latent heat of melting of fresh ice (J/kg)
    REAL(DP),PARAMETER :: ustar_min =0.005_DP  !! Minimum friction velocity for ocean heat flux (m/s)
    REAL(DP),PARAMETER :: rhoi = 917.0_DP !!  Density of ice (kg/m^3)
    REAL(DP),PARAMETER :: ISALT_ref  = 10.0_DP !!  Refrence salinity for new ice
    REAL(DP),PARAMETER :: rhow = 1026.0_DP !!  Density of seawater (kg/m^3)
    REAL(DP),PARAMETER :: rhofresh = 1000.0_DP !! Density of fresh water (kg/m^3)
    REAL(DP),PARAMETER :: depressT = 0.054_DP !! Tf:brine salinity ratio (C/ppt)
    REAL(DP),PARAMETER :: SPCP = 4.2173E3_DP !! Specific heat capacity, keep the same value as in bcond 
    ! drag force
    REAL(DP),PARAMETER ::  Cw = 0.0055_DP !! ocean-ice neutral drag coefficient
    REAL(SP), PARAMETER :: COSW = 1.0_SP  !cos(ocean turning angle) !turning angle = 0.
    REAL(SP), PARAMETER :: SINW = 0.0_SP  !sin(ocean turning angle) !turning angle = 0.
    ! shortwave
    REAL(SP), PARAMETER :: i0vis     = 0.70_DP   ! fraction of penetrating solar rad (visible)
    REAL(SP), PARAMETER :: albicev   = 0.78_DP   ! visible ice albedo for h > ahmax
    REAL(SP), PARAMETER :: ahmax     = 0.3_DP    ! thickness above which ice albedo constant (m)
    REAL(SP), PARAMETER :: c4i       = 4.0_DP
    REAL(SP), PARAMETER :: c1i       = 1.0_DP
    REAL(SP), PARAMETER :: p001      = 0.001_DP
    REAL(SP), PARAMETER :: albocn    = 0.06_DP   !ocean albedo
    REAL(SP), PARAMETER :: kappav    = 1.4_DP   ! vis extnctn coef in ice, wvlngth<700nm (1/m)
	! conductive heat 
    REAL(SP), PARAMETER :: K0        = 2.03_DP   !  conductivity of fresh ice, w/m/C
    REAL(SP), PARAMETER :: K_min     = 0.1_DP    !  minimum ice thermal conductivity
    REAL(SP), PARAMETER :: beta      = 0.13_DP   !  empirical constant for compute ice thermal conductivity, w/m/psu
  
  CONTAINS
    
  SUBROUTINE ICENUDGE_STRESS
!------------------------------------------------------------------------------|
!  COMPUTE ICE-OCEAN STRESS
!------------------------------------------------------------------------------|

   USE MOD_PREC
   USE ALL_VARS
   USE MOD_PAR
   
   INTEGER :: I,K,J
   REAL(SP):: vrel ! relative ice-ocean velocity 
   REAL (SP), DIMENSION(:),ALLOCATABLE, TARGET :: ustar_tmp


! in the ice-shelf interface
    ALLOCATE(ustar_tmp(0:NT));ustar_tmp = ZERO  ! friction velocity on cells(m/s)
! Initialize 
   STROCNX(:) = 0.0_SP
   STROCNY(:) = 0.0_SP
   USTAR(:)   = 0.0_SP
   
! compute ocean-ice stress

   CALL N2E2D(AICE,AICEU)

#   if defined (MULTIPROCESSOR)
        IF(PAR) CALL AEXCHANGE(EC,MYID,NPROCS,AICEU)
#   endif

  DO I = 1,NT
# if defined (ICESHELF)
    IF (ABS(ISISFC(I)==1) AICEU(I) = 0.0_SP
# endif 
     IF(AICEU(I)>=1.0E-5_SP)  THEN	 
       vrel = rhow*Cw*sqrt((U(I,1) - UICE(I))**2 + (V(I,1) - VICE(I))**2)  ! m/s
       ! turning angle is oppsite in the northen and southern hemisphere
       STROCNX(I) =  vrel*((U(I,1) - UICE(I))*COSW - (V(I,1) - VICE(I))*SINW*SIGN(1.0_SP,COR(I)))   
       STROCNY(I) =  vrel*((V(I,1) - VICE(I))*COSW + (U(I,1) - UICE(I))*SINW*SIGN(1.0_SP,COR(I)))
       ustar_tmp(I) = sqrt(sqrt(STROCNX(I)**2+STROCNY(I)**2)/rhow)
     ENDIF
  END DO

#   if defined (MULTIPROCESSOR)
        IF(PAR) CALL AEXCHANGE(EC,MYID,NPROCS,STROCNX,STROCNY)
        IF(PAR) CALL AEXCHANGE(EC,MYID,NPROCS,ustar_tmp)
#   endif

    CALL E2N2D(ustar_tmp,USTAR)

#   if defined (MULTIPROCESSOR)
        IF(PAR) CALL NODE_MATCH(1,NBN,BN_MLT,BN_LOC,BNC,MT,1,MYID,NPROCS,USTAR)
        IF(PAR) CALL AEXCHANGE(NC,MYID,NPROCS,USTAR)
#   endif
    
  RETURN
  END SUBROUTINE ICENUDGE_STRESS

  SUBROUTINE ICENUDGE_THERMODYN1
!------------------------------------------------------------------------------|
!  COMPUTE LATENT HEAT FLUX, HEAT CONTENT FLUX & VIRTUAL SALT FLUX  DUE 
!  TO BASAL MELTING, UPDATE SURFACE SALINITY, COMPUTE CONDUCTIVE HEAT FLUX& 
!  PENETRATING SHORTWAVE HEAT FLUX, MODIFY THE NET HEAT FLUX WTSURF & SWRAD 
!  ACCORDINGLY
!    
!------------------------------------------------------------------------------|
  
! read input data from ocean

   USE MOD_PREC
   USE ALL_VARS
   USE MOD_PAR

    
    INTEGER :: I,K,J
    REAL(SP):: fh,dhdt,hw,Du,SPRO,Ki,tmpT,Tf,diffT ! temporal variable for albedo
    REAL (SP), DIMENSION(:),ALLOCATABLE, TARGET :: Fbot,Fct,Fc,albv,transbot


! in the ice-shelf interface  
    ALLOCATE(Fbot(0:MT));    Fbot = ZERO  ! heat flux for basal melting 
    ALLOCATE(Fct(0:MT));     Fct  = ZERO  ! heat flux for melting water heat content
    ALLOCATE(Fc(0:MT));      Fc   = ZERO  ! conductive heat flux 
    ALLOCATE(transbot(0:MT)); transbot = ZERO  !  sw transmitted to the ice bottom
    ALLOCATE(albv(0:MT));   albv = ZERO  ! albedo for visiable components absorbed in the ice interior or transmitted to the ocean. 
   
! initialize the variable
  FVSALT1(:) = 0.0_SP
  S_TMP(:) = 0.0_SP
  FSWTHRU(:) = 0.0_SP
  FHEAT(:)  = 0.0_SP
  T_f(:)    = 0.0_SP

!  compute the surface freezing temperature 
    T_AIR = T_AIR - 273.16_SP       ! from K to ^oC
    S_TMP = S1(:,1)	
    T_f    = -depressT*S_TMP 
   !  compute basal melting
    SPRO = SPCP*rhow  
    DO I = 1,M
# if defined (ICESHELF)
      IF (ABS(ISISFN(I))==1) AICE(I) = 0.0_SP
# endif
     IF(AICE(I)>=1.0E-5_SP)  THEN
				 
         ! SW penetrating thru ice into ocean, only visible components (28% direct and 24% diffuse) absorbed 
          ! in the ice interior or transmitted to the ocean & assume frsnow = 0
          fh = MIN(ATAN(HICE(i)*c4i)/ATAN(ahmax*c4i),c1i)
          albv(I)  = albicev*fh  
          transbot(I) = EXP(-kappav*HICE(I))
          FSWTHRU(I) = SWRAD(I)*0.52*(1-albv(I))*i0vis*transbot(I)
        
          !  Conductive heat flux
          tmpT = 0.5_SP*(T_AIR(I)+T_f(I))   ! sea ice temperature
	  !Ki   = MAX(K0+beta*ISALT(I)/tmpT,K_min)
	   Ki   = K0 !MAX(K0+beta*ISALT(I)/tmpT,K_min)
          Fc(I) = Ki*(T_AIR(I)-T_f(I))/HICE(I)
	
         ! basal melting latent heat flux
          
	  diffT   = (TF1(I,1)-T_f(I))
	  IF (diffT > 0.0_SP) THEN                ! only consider the melting
            Fbot(I) = -rhow*cp_ocn*c_h*MAX(USTAR(I),ustar_min)*diffT  !  <0 melting, ocean loses heat
            dhdt    = -Fbot(I)/(rhoi*Lfresh)              ! dhdt > 0, melting
            Du      = D(I)*DZ(I,1)
            hw      =  rhoi*dhdt*DTI/rhow
		  
          ! virtual salt flux and fresh water flux, ocean gets water & salt when dhdt<0 melting
            FVSALT1(I) = (-1)*(Du/(Du+hw))*rhoi*dhdt*(S1(I,1)-ISALT(I))        ! g/m^2/s
            Fct(I)     = (-1)*cp_ocn*(Du/(Du+hw))*rhoi*dhdt*(TF1(I,1)-T_f(I))       ! w/m^2
            S_TMP(I)   = S1(I,1) + (FVSALT1(I)*DTI)/(rhow*Du)
	  END IF	  		  
          ! sum the latent and sensbile heat flux

	  
             FHEAT(I) = Fc(I)+Fbot(I)+Fct(I)+FSWTHRU(I)
		  
	   ! convert the virtual salt flux from g to kg
	      FVSALT1(I)  = FVSALT1(I)*p001

    		  
          !===============================================
          ! add the net heat flux and sw flux,! WTSURF & SWRAD is multiplied with (-1) 
          ! before it is called by vdif_ts,  that is why we times (-1)* here
          WTSURF(I) = (1-AICE(I))*WTSURF(I)+(-1)*AICE(I)*FHEAT(I)/SPRO 
          SWRAD(I)  = (1-AICE(I))*SWRAD(I) +(-1)*AICE(I)*FSWTHRU(I)/SPRO

        ENDIF

   END DO

# if defined (MULTIPROCESSOR)
       IF(PAR) CALL NODE_MATCH(1,NBN,BN_MLT,BN_LOC,BNC,MT,1,MYID,NPROCS,WTSURF,SWRAD)
       IF(PAR) CALL AEXCHANGE(NC,MYID,NPROCS,WTSURF,SWRAD)
       IF(PAR) CALL NODE_MATCH(1,NBN,BN_MLT,BN_LOC,BNC,MT,1,MYID,NPROCS,FVSALT1,S_TMP)
       IF(PAR) CALL AEXCHANGE(NC,MYID,NPROCS,FVSALT1,S_TMP)
 !      IF(PAR) CALL NODE_MATCH(1,NBN,BN_MLT,BN_LOC,BNC,MT,1,MYID,NPROCS,S_TMP)
 !     IF(PAR) CALL AEXCHANGE(NC,MYID,NPROCS,S_TMP)

       IF(PAR) CALL NODE_MATCH(1,NBN,BN_MLT,BN_LOC,BNC,MT,1,MYID,NPROCS,FHEAT,FSWTHRU)
       IF(PAR) CALL AEXCHANGE(NC,MYID,NPROCS,FHEAT,FSWTHRU)
# endif

 RETURN
 END SUBROUTINE ICENUDGE_THERMODYN1
 
 SUBROUTINE  ICENUDGE_THERMODYN2
 !------------------------------------------------------------------------------|
!  COMPUTE freezing potential from the updated upper layer temperature,
!------------------------------------------------------------------------------|
  
! read input data from ocean

   USE MOD_PREC
   USE ALL_VARS
   USE MOD_PAR

    
   INTEGER :: I,K,J
   REAL(SP):: frzmlt,dhdt,hw,Du,Tf! temporal variable for albedo

! initialize the variable
   FVSALT2(:) = 0.0_SP    
  
!  compute freezing potential, apply to both ice-covered and ice-free cells

    DO I = 1,M
        Du   = D(I)*DZ(I,1)
        Tf   = -depressT*S_TMP(I)
        frzmlt = rhow*cp_ocn*(Tf-T1(I,1))*Du/DTI  !  >0 freezing

#  if defined (ICESHELF)
        IF(ABS(ISISFN(I)I==1)  frzmlt = 0.0_SP
#  endif
 
        IF (frzmlt >0.0_SP) THEN  
            dhdt   = -frzmlt/(rhoi*Lfresh)              ! dhdt < 0, freezing
            hw      =  rhoi*dhdt*DTI/rhow
		  
          ! virtual salt flux and fresh water flux, ocean gets water & salt when dhdt<0 melting
            FVSALT2(I) = (-1)*(Du/(Du+hw))*rhoi*dhdt*(S_TMP(I)-ISALT_ref)        ! g/m^2/s
		  
          ! convert the virtual salt flux from g to kg
             FVSALT2(I)  = FVSALT2(I)*p001
          ! set the tmperature back to the surface temperature point
         
            DO K = 1, KBM1
             IF (T1(I,K) .LT. Tf) THEN
                T1(I,K) = Tf
             END IF

            END DO

 
         END IF
        
    END DO
		
# if defined (MULTIPROCESSOR)
       IF(PAR) CALL NODE_MATCH(1,NBN,BN_MLT,BN_LOC,BNC,MT,1,MYID,NPROCS,FVSALT2)
       IF(PAR) CALL AEXCHANGE(NC,MYID,NPROCS,FVSALT2)
       IF(PAR) CALL AEXCHANGE(NC,MYID,NPROCS,T1)
# endif
  
  CALL N2E3D(T1,T) 
 
 RETURN
 END SUBROUTINE ICENUDGE_THERMODYN2

# endif
END MODULE  MOD_ICENUDGE
