MODULE MOD_FABM_3D

   USE fabm
   USE fabm_config
   USE MOD_PREC
   USE ALL_VARS
   USE MOD_UTILS ! to USE DBG_SET for debugging screen output
#  if defined (WET_DRY)
   USE MOD_WD
#  endif
# if defined (THIN_DAM)
  USE MOD_DAM,only : NODE_DAM1_N,NODE_DAM2_N,NODE_DAM3_N&
                      &,I_NODE_DAM1_N,I_NODE_DAM2_N,I_NODE_DAM3_N,KDAM
# endif

   IMPLICIT NONE

   TYPE (type_model) :: model

   REAL(SP), ALLOCATABLE, TARGET ::  FABM_ALL(:,:,:)    ! 3D FABM STATE VARIABLES
   REAL(SP), ALLOCATABLE, TARGET ::  FABM_DIAG(:,:,:)   ! 3D FABM DIAGNOSTIC VARIABLES
   REAL(SP), ALLOCATABLE, TARGET ::  FABM_ALL_HZ(:,:)   ! 2D FABM SURFACE/BOTTOM-BOUND STATE VARIABLES
   REAL(SP), ALLOCATABLE, TARGET ::  FABM_DIAG_HZ(:,:)  ! 2D FABM HORIZONTAL DIAGNOSTIC VARIABLES
   REAL(SP), ALLOCATABLE ::  FABM_F(:,:,:)          !FORECASTED VARIABLES
   REAL(SP), ALLOCATABLE ::  FABM_MEAN(:,:,:)       !MEAN VARIABLES
   REAL(SP), ALLOCATABLE ::  KM_FABM(:)        ! VERTICAL MIXING COEFFICIENT
   REAL(SP), ALLOCATABLE :: FABM_VAR(:,:)     ! ENSEMBLE VARIABLES FOR MIXING
   REAL(SP), ALLOCATABLE :: XFLUX_OBCB(:,:,:)     ! !OPEN BOUNDARY FLUX



CONTAINS
      
   SUBROUTINE FABM_FVCOM_INITIALIZE()
      CALL fabm_create_model_from_yaml_file(model)
   END SUBROUTINE FABM_FVCOM_INITIALIZE

   SUBROUTINE FABM_INITIAL()
      CALL FABM_ALLOCATE

      ! TODO: initialize all FABM fields
      ! TODO: send pointers to environemntal variables to FABM
      ! TODO: call fabm_set_domain
      ! TODO: call fabm_check_ready
   END SUBROUTINE FABM_INITIAL

   SUBROUTINE FABM_ALLOCATE()
    ALLOCATE(FABM_ALL(0:MT,KB,size(model%state_variables)))    ; FABM_ALL     =  0.001_SP
    ALLOCATE(FABM_ALL_HZ(0:MT,size(model%bottom_state_variables)+&
       & size(model%surface_state_variables)))    ; FABM_ALL_HZ     =  0.001_SP
    ALLOCATE(FABM_F(0:MT,KB,size(model%state_variables)))      ; FABM_F       =  0.001_SP
    ALLOCATE(FABM_MEAN(0:MT,KB,size(model%state_variables)))  ; FABM_MEAN    =  0.001_SP

    ALLOCATE(KM_FABM(KB))      ; KM_FABM       =  0.001_SP
! Potential not needed...
    ALLOCATE(FABM_VAR(KB,size(model%state_variables))) ; FABM_VAR       =  0.001_SP   
    !Following variables appear to be no longer used:
    ALLOCATE(XFLUX_OBCB(0:MT,KB,size(model%state_variables))) ; XFLUX_OBCB  =  0.0_SP
    !ALLOCATE(FABM_MEANN(0:NT,KB,size(model%state_variables)))  ; FABM_MEANN   =  0.001_SP
    !ALLOCATE(FABM_VAR_MEAN(0:MT,KB,size(model%state_variables))) ; FABM_VAR_MEAN   =  0.0_SP
   END SUBROUTINE

SUBROUTINE FABM_3D1D
     IMPLICIT NONE
     SAVE
     INTEGER  :: I, IFABM

!
!     SPCP  = 4.2174E3_SP                        !HEAT SPECIFIC CAPACITY
!     ROSEA = 1.023E3_SP                         !RHO OF SEA WATER
!     SPRO=SPCP*ROSEA
!!     BIO_VAR_MEAN = 0.0_SP
!!---------------------------                    !MAIN LOOP OVER ELEMENTS
!
#    if defined (ONE_D_MODEL)
     DO I=M,M
#    else
     DO I=1,M
#    endif
!       DO K=1,KBM1                              !3D TO 1D FIELD
!         DO J=1,NNN
!            BIO_N(K,J)=BIO_ALL(I,K,J+INN-1)
!         END DO
!         DO J=1,NNP
!            BIO_P(K,J)=BIO_ALL(I,K,J+INP-1)
!         END DO
!         DO J=1,NNZ
!            BIO_Z(K,J)=BIO_ALL(I,K,J+INZ-1)
!         END DO
!         DO J=1,NNM
!            BIO_DOM(K,J)=BIO_ALL(I,K,J+INM-1)
!         END DO
!         DO J=1,NNB
!            BIO_B(K,J)=BIO_ALL(I,K,J+INB-1)
!         END DO
!         DO J=1,NND
!            BIO_D(K,J)=BIO_ALL(I,K,J+IND-1)
!         END DO
!         DELTA_D(K)=DZ(I,K)*D(I)                   !LAYER THICKNESS
!         DELTA_Z(K)=DZZ(I,K)*D(I)                  !DISTANCE BETWEEN LAYERS
!         DEPTH_Z(K)=Z(I,K)*D(I)                    !LAYER CENTER DEPTH
!         IRRAD0=-SWRAD(I)*PARFRAC*SPRO/RAMP      !PAR FRACTION
!         L_NH4N=30._SPP                          !NITRIFICATION USE	 
!         T_BIO(K)=T1(I,K)
!       END DO                                    !K=1,KB
!       T_STEP=DTI
!       CALL ZOOPLANKTON
!       CALL PHYTOPLANKTON
!       CALL BACTERIA
!       CALL DETRITUS
!       CALL DOM
!       CALL NUTRIENT
!        DO K=1,KBM1                                !1D TO 3D FIELD
!          DO J=1,NNN
!             BIO_ALL(I,K,J+INN-1)=BIO_N(K,J)
!          END DO
!          DO J=1,NNP
!             BIO_ALL(I,K,J+INP-1)=BIO_P(K,J)
!          END DO
!          DO J=1,NNZ
!             BIO_ALL(I,K,J+INZ-1)=BIO_Z(K,J)
!          END DO
!          DO J=1,NNM
!             BIO_ALL(I,K,J+INM-1)=BIO_DOM(K,J)
!          END DO
!          DO J=1,NNB
!             BIO_ALL(I,K,J+INB-1)=BIO_B(K,J)
!          END DO
!          DO J=1,NND
!             BIO_ALL(I,K,J+IND-1)=BIO_D(K,J)
!          END DO
!        END DO 
      KM_FABM(:)=KH(I,:)

!      FABM_VAR(1:KB,1:size(model%state_variables))=FABM_ALL(I,1:KB,1:size(model%state_variables))
# Loop here over individual variables for vertical mixing
      DO IFABM=1,size(model%state_variables)

#  if !defined (WET_DRY)
        !       IF (D(I) > 0.0_SP) THEN
#  else
         IF(ISWETN(I) == 1)THEN
#  endif
           CALL FABM_MIXING (FABM_ALL(I,1:KB,IFABM))
# if defined(WET_DRY)
         END IF
# endif
# Thin DAM treatment goes in here.... as all it does is some averaging over nodes affected by the thin dam
# at least for the vertical mixing
      ENDDO #IFABM
      FABM_ALL(I,1:KB,1:size(model%state_variables))=FABM_VAR(1:KB,1:size(model%state_variables))
      END DO !I=1,M 
#    if !defined (ONE_D_MODEL)
#    if defined (MULTIPROCESSOR)
       CALL FABM_EXCHANGE
#    endif
      CALL FABM_ADV
#    if defined (MULTIPROCESSOR)
      CALL FABM_EXCHANGE
#    endif
      CALL FABM_BCOND
      FABM_ALL=FABM_F                                   !UPDATE
#    endif
!    end if defined 1D

      !WHERE (FABM_ALL < 0.001) FABM_ALL=0.001


!      IF(MOD(IINT-1,CDF_INT)==0) CALL BIO_OUT_NETCDF


END SUBROUTINE FABM_3D1D

SUBROUTINE FABM_EXCHANGE
!==============================================================================!
!     PERFORM DATA EXCHANGE FOR the Generalized biological model               |
!==============================================================================!
#if defined (MULTIPROCESSOR)
!     USE ALL_VARS
     USE MOD_PAR
     USE LIMS
     USE CONTROL
     IMPLICIT NONE
     INTEGER :: I3
     REAL(SP),ALLOCATABLE :: FABM_ALL_T(:,:),FABM_MEAN_T(:,:),FABM_F_T(:,:)
     DO I3=1,NTT
      ALLOCATE(FABM_ALL_T(0:MT,KB))
      ALLOCATE(FABM_MEAN_T(0:MT,KB))
      ALLOCATE(FABM_F_T(0:MT,KB))

      FABM_ALL_T(:,:)  = FABM_ALL(:,:,I3)
      FABM_MEAN_T(:,:) = FABM_MEAN(:,:,I3)
      FABM_F_T(:,:)    = FABM_F(:,:,I3)

      IF(PAR) CALL AEXCHANGE(NC,MYID,NPROCS,FABM_ALL_T)
      IF(PAR) CALL AEXCHANGE(NC,MYID,NPROCS,FABM_MEAN_T)
      IF(PAR) CALL AEXCHANGE(NC,MYID,NPROCS,FABM_F_T)

      FABM_ALL(:,:,I3)  = FABM_ALL_T(:,:)  
      FABM_MEAN(:,:,I3) = FABM_MEAN_T(:,:) 
      FABM_F(:,:,I3)    = FABM_F_T(:,:)    

      DEALLOCATE(FABM_ALL_T,FABM_MEAN_T,FABM_F_T)
     END DO 
   RETURN
#endif
  END SUBROUTINE FABM_EXCHANGE

  SUBROUTINE FABM_MIXING(F)
!==============================================================================|
!     vertical mixing adopted from FVCOM                                       !
!==============================================================================|

   USE MOD_1D
   IMPLICIT NONE
   
   INTEGER :: I,K,J,J1,KI, KVM1,KVM2
   REAL(DPP) :: GW,FKH
   REAL(SPP), DIMENSION(KB),INTENT (INOUT)     :: F
!   REAL(SPP), DIMENSION(KBV),INTENT (OUT)     :: FOUT
   REAL(DPP), DIMENSION(KB)     :: FF,VHF,VHPF
   REAL(DPP), DIMENSION(KB)         :: AF,CF 

  IF(DBG_SET(DBG_SBR)) WRITE(IPT,*)"Start: FABM_MIXING :"
!------------------------------------------------------------------------------!
!        the following section solves the equation                             !
!         dti*(kh*f')'-f=-fb                                                   !
!------------------------------------------------------------------------------!
    KVM1=KB-1
    KVM2=KB-2
!    F(1:KBV,1:NTT)=BIO_VAR(1:KBV,1:NTT)

    DO K = 2, KVM1
         FKH=KM_BIO(K)
         AF(K-1)=-T_STEP*FKH/(DELTA_D(K-1)*DELTA_Z(K-1))
         CF(K)=-T_STEP*FKH/(DELTA_D(K)*DELTA_Z(K-1))
    END DO

  !------------------------------------------------------------------------------!
  !   surface bcs;                                                        !
  !------------------------------------------------------------------------------!

       VHF(1) = AF(1) / (AF(1)-1.)
       VHPF(1) = -F(1)(1) / (AF(1)-1.)
  !------------------------------------------------------------------------------!
  !   Interior;                                                        !
  !------------------------------------------------------------------------------!

      DO K = 2, KVM2
         VHPF(K)=1./ (AF(K)+CF(K)*(1.-VHF(K-1))-1.)
         VHF(K) = AF(K) * VHPF(K)
         VHPF(K) = (CF(K)*VHPF(K-1)-DBLE(F(K)) &
                      )*VHPF(K)
      END DO


      DO  K = 1, KVM1
         FF(K) = F(K) 
      END DO

  !------------------------------------------------------------------------------!
  !   Bottom bcs;                                                        !
  !------------------------------------------------------------------------------!


       GW=0.0_SPP
       FF(KVM1) = ((CF(KVM1)*VHPF(KVM2)-FF(KVM1)-GW) &
         /(CF(KVM1)*(1._SP-VHF(KVM2))-1._SPP)) !GW IS GROUND FLUX


       DO  K = 2, KVM1
         KI = KB - K
         FF(KI) = (VHF(KI)*FF(KI+1)+VHPF(KI))
       END DO


!This Do loop can be removed if no negative number control is needed.
       DO K = 1, KVM1
           F(K) = FF(K)
! No negative number control here!
!	   if (BIO_VAR(K,I) < 0.0_SPP) BIO_VAR(K,I)=0.0001	 
       END DO


    RETURN
END SUBROUTINE FABM_MIXING

  SUBROUTINE FABM_ADV()
  END SUBROUTINE

  SUBROUTINE FABM_BCOND()
!==============================================================================|
!   Set Boundary Conditions for FABM                                         |
!==============================================================================|

!------------------------------------------------------------------------------|
   USE ALL_VARS
   USE BCS
   USE MOD_OBCS
   IMPLICIT NONE
   REAL(SP) :: T2D,T2D_NEXT,T2D_OBC,XFLUX2D,TMP,RAMP_BIO
   INTEGER  :: I,J,K,J1,J11,J22,NCON2,IFABM
   REAL(SP) ::WQMMAX,WQMMIN

!   ALLOCATE(WDIS(NUMQBC,ntt))     ;WDIS      = ZERO
!------------------------------------------------------------------------------|

! make use of masks for both river and OBC. Non suplied variables will get diluted (in the case of the rivers) or fixed
! to interior values for OBCs. 
!------------------------------------------------------------------------------|
!!!! River nutrient treatment for FABM
! FABMDIS is initialised to zero so we don't need a mask. Not-read values will be zero
!--SET CONDITIONS FOR FRESH WATER INFLOW---------------------------------------|
!
   IF(RIVER_TS_SETTING == 'specified') THEN
     IF(NUMQBC > 0) THEN
       IF(RIVER_INFLOW_LOCATION == 'node') THEN
         DO I=1,NUMQBC
           J11=INODEQ(I)
           DO K=1,KBM1
             DO IFABM=1,size(model%state_variables)
               FABM_F(J11,K,IFABM) = FABMDIS(I,IFABM)
             END DO
           END DO
         END DO
       ELSE IF(RIVER_INFLOW_LOCATION == 'edge') THEN
         DO I=1,NUMQBC
           J11=N_ICELLQ(I,1)
           J22=N_ICELLQ(I,2)
           DO K=1,KBM1
             DO IFABM=1,size(model%state_variables)
               FABM_F(J11,K,IFABM)=FABMDIS(I,IFABM)
               FABM_F(J22,K,IFABM)=FABMDIS(I,IFABM)
             END DO
           END DO
         END DO
       END IF
     END IF
   END IF

       
   IF(IOBCN > 0) THEN
!
!  SET CONDITIONS ON OUTER BOUNDARY
!
     IF(OBC_BIO_NUDGING) CALL UPDATE_OBC_FABM(IntTime,FABM_OBC)
     RAMP_BIO = TANH(FLOAT(IINT)/FLOAT(IRAMP+1))
     DO IFABM=1,size(model%state_variables)
       DO I=1,IOBCN
         J=I_OBC_N(I)
         J1=NEXT_OBC(I)
         T2D=0.0_SP
         T2D_NEXT=0.0_SP
         XFLUX2D=0.0_SP
         DO K=1,KBM1
           T2D=T2D+FABM_ALL(J,K,IFABM)*DZ(J,K)
           T2D_NEXT=T2D_NEXT+FABM_F(J1,K,IFABM)*DZ(J1,K)
           XFLUX2D=XFLUX2D+XFLUX_OBCB(I,K,IFABM)           !*DZ(K)
         END DO
         IF(UARD_OBCN(I) > 0.0_SP) THEN
           TMP=XFLUX2D+T2D*UARD_OBCN(I)
           T2D_OBC=(T2D*DT(J)-TMP*DTI/ART1(J))/D(J)
           DO K=1,KBM1
            FABM_ALL(J,K,IFABM)=T2D_OBC+(FABM_ALL(J1,K,IFABM)-T2D_NEXT)
           END DO

         DO K=1,KBM1
           WQMMAX = MAXVAL(FABM_ALL(NBSN(J,1:NTSN(J)),K,IFABM))
           WQMMIN = MINVAL(FABM_ALL(NBSN(J,1:NTSN(J)),K,IFABM))
         
           IF(K == 1)THEN
            WQMMAX = MAX(WQMMAX,(FABM_ALL(J,K,IFABM)*DZ(J,K+1)+FABM_ALL(J,K+1,IFABM)*DZ(J,K))/  &
	             (DZ(J,K)+DZ(J,K+1)))
            WQMMIN = MIN(WQMMIN,(FABM_ALL(J,K,IFABM)*DZ(J,K+1)+FABM_ALL(J,K+1,IFABM)*DZ(J,K))/  &
	             (DZ(J,K)+DZ(J,K+1)))
           ELSE IF(K == KBM1)THEN
            WQMMAX = MAX(WQMMAX,(FABM_ALL(J,K,IFABM)*DZ(J,K-1)+FABM_ALL(J,K-1,IFABM)*DZ(J,K))/  &
	             (DZ(J,K)+DZ(J,K-1)))
            WQMMIN = MIN(WQMMIN,(FABM_ALL(J,K,IFABM)*DZ(J,K-1)+FABM_ALL(J,K-1,IFABM)*DZ(J,K))/  &
	             (DZ(J,K)+DZ(J,K-1)))
           ELSE
            WQMMAX = MAX(WQMMAX,(FABM_ALL(J,K,IFABM)*DZ(J,K-1)+FABM_ALL(J,K-1,IFABM)*DZ(J,K))/  &
	             (DZ(J,K)+DZ(J,K-1)), &
                    (FABM_ALL(J,K,IFABM)*DZ(J,K+1)+FABM_ALL(J,K+1,IFABM)*DZ(J,K))/  &
		     (DZ(J,K)+DZ(J,K+1)))
            WQMMIN = MIN(WQMMIN,(FABM_ALL(J,K,IFABM)*DZ(J,K-1)+FABM_ALL(J,K-1,IFABM)*DZ(J,K))/  &
	             (DZ(J,K)+DZ(J,K-1)), &
                    (FABM_ALL(J,K,IFABM)*DZ(J,K+1)+FABM_ALL(J,K+1,IFABM)*DZ(J,K))/  &
		     (DZ(J,K)+DZ(J,K+1)))
           END IF
 
           IF(WQMMIN-FABM_F(J,K,IFABM) > 0.0_SP)FABM_F(J,K,IFABM) = WQMMIN
           IF(FABM_F(J,K,IFABM)-WQMMAX > 0.0_SP)FABM_F(J,K,IFABM) = WQMMAX

         END DO

          ELSE

           IF(OBC_BIO_NUDGING) THEN
             DO K=1,KBM1
               FABM_F(J,K,IFABM) = FABM_ALL(J,K,IFABM) - OBC_FABM_NUDGING_TIMESCALE*RAMP_BIO*(FABM_ALL(J,K,IFABM)&
                      &-BIO_OBC(I,K,IFABM))
             END DO
           ELSE
             DO K=1,KBM1
               FABM_F(J,K,IFABM)=FABM_ALL(J,K,IFABM)
             END DO
           END IF
         END IF
       END DO
     END DO !!OUTER LOOP OVER BIO-VARIABLES IFABM=1,size(model%state_variables)

   END IF

!
!--SET BOUNDARY CONDITIONS-----------------------------------------------------|
!
       FABM_ALL(0,:,:)=ZERO ! don't know what this is doing here
   RETURN

  END SUBROUTINE FABM_BCOND
!==============================================================================!

END MODULE MOD_FABM_3D