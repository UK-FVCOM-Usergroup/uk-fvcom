! ---------------------------------------------------------------------
! This model estimates the flux of tracer from a point release at the
! bottom. It is based on idealized physics proposed by Morton, Talyer &
! Turner, 1956: "Turbulent gravitational convection" (the MTT model)
      
! The tracer flux at the fluid-source interface is read, the model
! estimates the depth the plume will reach a hydrostatically stable
! configuration based on entrainment of water from the sides.
! A net flux of temperature, salinity and tracer is
! released at the depth predicted by the mtt model.

! Important note!:
! MTT theory assumes that the buoyancy flux and stratification is
! independent of time. However, the stratification changes with 
! with time in FVCOM. By using this plume model we assume that the 
! plume adapts to the changing environment and reaches a stable
! configuration instantaneously. This is not the case in nature, 
! and should be accounted for in the future development of this code.
       
! Copyright (c) Akaplan-niva 2019
! ----------------------------------------------------------------------
! REVISION HISTORY:
! Original authors: Håvard Espenes     (hes@akvaplan.niva.no)
!                   Qin Zhou           (qin@akvaplan.niva.no)
!                   Ole Anders Nøst    (oan@akvaplan.niva.no)

! Ps. To use this module, you must activate both "PLUME" and
!     "PLUME_CALCULATED" when you compile the code.

MODULE MOD_PLUME
      USE MOD_PREC
      IMPLICIT NONE

CONTAINS

      SUBROUTINE UPDATE_PLUMES_CALCULATED(V0,W0,F0,PTEMP,PSALT,PTRACER,INS,Inode,IFABM, &
                                          &FLUX,TMPT,SLT,TRACER,gridinfo,MAXsigma,HYDsigma)
! --------------------------------------------------------------------------------------
!     This routine reads the volume-, momentum- and buoyancyflux, as well as the
!     initial concentration of salt, tracer and temperature to give an estimate of
!     where the plume will stop, the flux of matter to that designated sigma layer
!     and the flux of matter from the layers where the plume entrains water from the
!     surroundings.

!     Variables:
!     ----------------
!     INPUT
!     V0:           Source momentum flux
!     W0:           Source volume flux
!     F0:           Source buoyancy flux
!     PTEMP:        Initial plume temperature
!     PSALT:        Initial plume salinity
!     PTRACER:      Initial plume concentration
!     Inode:        Node where the plume was released
!     IFABM:        Id of FABM variable
!     INS:          Input sigma layer

!     OUTPUT:
!     T:            Temperature flux caused by the plume
!     S:            Salinity flux caused by the plume
!     F:            Tracer flux caused by the plume
!     gridinfo:     The nodes influenced by the plume, as well as sigma layers
! ------------------------------------------------------------------------------------

      USE ALL_VARS
      USE MOD_UTILS
      IMPLICIT NONE
      REAL(SP), INTENT(INOUT)              ::                        V0, W0, F0
      REAL(SP), INTENT(IN   )              ::             PTEMP, PSALT, PTRACER
      REAL(SP), INTENT(  OUT), ALLOCATABLE ::  TMPT(:),SLT(:),TRACER(:),FLUX(:)
      INTEGER,  INTENT(  OUT), ALLOCATABLE ::                       gridinfo(:)
      INTEGER,  INTENT(IN   )              ::                   INS,IFABM,Inode
      INTEGER,  INTENT(  OUT)              ::                MAXsigma, HYDsigma
      REAL(SP), ALLOCATABLE                ::    MYDENS(:), DEPTH(:), nyfreq(:)
      REAL(SP)                             ::         entcoef, zhyd, ztop, area
      REAL(SP)                             ::                            radius

!     Constants and ICs
      entcoef        = 0.07_SP  ! The exact value of this parameter is disputed
      DEPTH          = -H(Inode)*DZ(Inode,:)
      MYDENS         = RHO1(Inode,:)

      CALL BUOYFREQ(MYDENS, DEPTH, nyfreq)
      CALL INTVERT(W0,V0,F0,PTEMP,PTRACER,PSALT,entcoef,INS,nyfreq,DEPTH, &
                   MAXsigma,HYDsigma,IFABM,Inode,FLUX,TMPT,SLT,TRACER)  

      radius = W0/V0
      area   = 2*3.14159*radius**2
      END SUBROUTINE UPDATE_PLUMES_CALCULATED
       
      SUBROUTINE BUOYFREQ(DENS,DEPTH,nyfreq)
      USE ALL_VARS
      IMPLICIT NONE
      REAL(SP), ALLOCATABLE, INTENT(IN ) ::          DENS(:), DEPTH(:)
      REAL(SP), ALLOCATABLE, INTENT(OUT) ::                  nyfreq(:)
      REAL(SP)                           ::     Rup, Rdown, g, rhoref
      INTEGER                            ::                         K
      
      g      = 9.81_SP
      rhoref = 1024.0_SP
      allocate(nyfreq(1:SIZE(DENS)))
      
      DO K = 1,KBM1
         IF (K == 1) THEN
            Rup   = DENS(K)
            Rdown = (DENS(K)*DEPTH(K+1)+DENS(K+1)*DEPTH(K))/(DEPTH(K)+DEPTH(K+1))
         ELSE IF (K == KBM1) THEN
            Rup   = (DENS(K)*DEPTH(K-1)+DENS(K-1)*DEPTH(K))/(DEPTH(K)+DEPTH(K-1))
            Rdown = DENS(K)
         ELSE
            Rup   = (DENS(K)*DEPTH(K-1)+DENS(K-1)*DEPTH(K))/(DEPTH(K)+DEPTH(K-1))
            Rdown = (DENS(K)*DEPTH(K+1)+DENS(K+1)*DEPTH(K))/(DEPTH(K)+DEPTH(K+1))
         END IF
         
         nyfreq(K)     = -g/rhoref * ((Rup-Rdown)/(DEPTH(K)))
      
         IF (nyfreq(K).LT.0.0_SP) THEN
!           If the water column is not hydrostatically stable
            nyfreq(K) = 0.0_SP
         END IF
         nyfreq(K)     = SQRT(nyfreq(K))
      END DO
      END SUBROUTINE BUOYFREQ

      SUBROUTINE INTVERT(W0,V0,F0,PLUME_TEMP,PLUME_TRACER,PLUME_SALT,entcoef,INS, &
                         &nyfreq,DEPTH,MAXsigma,HYDsigma,IFABM,Inode,FLUX,TMPT,SLT,TRACER)
      USE ALL_VARS
      USE MOD_FABM_DATA
      IMPLICIT NONE
      REAL(SP), INTENT(INOUT)                    ::                          W0, V0, F0
      REAL(SP), INTENT(IN   )                    ::   PLUME_TEMP, PLUME_TRACER, entcoef
      REAL(SP), INTENT(IN   )                    ::                          PLUME_SALT
      REAL(SP), INTENT(IN   ), ALLOCATABLE       ::                 nyfreq(:), DEPTH(:)
      INTEGER,  INTENT(IN   )                    ::                   INS, IFABM, Inode
      INTEGER,  INTENT(  OUT)                    ::                  MAXsigma, HYDsigma
      REAL(SP), INTENT(  OUT), DIMENSION(0:KBM1) ::                TMPT,SLT,TRACER,FLUX
      REAL(SP), DIMENSION(0:KBM1)                ::          TEMPFLUX,SALTFLUX,FABMFLUX
      REAL(SP)                                   ::       radius, vertspeed, Wn, Vn, Fn
      INTEGER                                    ::                                   K
!     Integrate the equations vertically (from the bottom to the top), and solve the 
!     equations to get the plume parameters.

      K           = INS    ! INPUT sigma layer
      FLUX(K)     = 0.0_SP

      radius      = W0/V0
      vertspeed   = V0**2/W0

!     Her må jeg legge inn en "IF (F0 > 0) THEN" greie, og integrere i motsatt retning om det viser seg
!     at plumen synker.
      TEMPFLUX(K) = V0*PLUME_TEMP   + entcoef*2*pi*radius*vertspeed*(DEPTH(K))*T1(Inode,K)
      SALTFLUX(K) = V0*PLUME_SALT   + entcoef*2*pi*radius*vertspeed*(DEPTH(K))*S1(Inode,K)
      FABMFLUX(K) = V0*PLUME_TRACER + entcoef*2*pi*radius*vertspeed*(DEPTH(K))*FABM_ALL(Inode,K,IFABM)
     
      DO WHILE (.TRUE.)
         K  = K-1
         IF (K == 0) THEN
            EXIT
         ENDIF
         IF (K.GT.SIZE(nyfreq)) THEN
            EXIT
         END IF
            
         Wn = W0    + 2*entcoef*V0*DEPTH(K)
         IF (ISNAN(Wn)) THEN
            MAXsigma = K
            EXIT
         END IF
         
         IF ((F0.GT.0).AND.(Fn.LE.0)) THEN
            HYDsigma = K
         END IF
         Vn          = V0**4 + 2.0_SP*F0*W0*0.5_SP*DEPTH(K)
         Fn          = F0    - nyfreq(K)**2

!     Update the plume S, T and FABMtracer due to entrained water
         radius      = Wn/Vn
         vertspeed   = Vn**2/Wn

!     Finding the flux upwards from given sigma layer 
         SALTFLUX(K) = SALTFLUX(K+1) + entcoef*2*pi*radius*vertspeed*DEPTH(K)*S1(Inode,K)
         TEMPFLUX(K) = TEMPFLUX(K+1) + entcoef*2*pi*radius*vertspeed*DEPTH(K)*T1(Inode,K)
         FABMFLUX(K) = FABMFLUX(K+1) + entcoef*2*pi*radius*vertspeed*DEPTH(K)*FABM_ALL(Inode,K,IFABM)
         FLUX(K)     = entcoef*2*pi*radius*vertspeed*(DEPTH(K)-DEPTH(K+1))

!     We update the plume parameters if the plume is still rising
         V0          = Vn**(0.25)        ! Momentum flux
         W0          = Wn                ! Volume flux
         F0          = Fn                ! Buoyancy flux
      END DO
      END SUBROUTINE INTVERT
END MODULE MOD_PLUME
