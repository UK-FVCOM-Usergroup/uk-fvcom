MODULE MOD_FABM_3D

   USE fabm
   USE fabm_config

   IMPLICIT NONE

   TYPE (type_model) :: model

   REAL(SP), ALLOCATABLE, TARGET ::  FABM_ALL(:,:,:)    ! 3D FABM STATE VARIABLES
   REAL(SP), ALLOCATABLE, TARGET ::  FABM_DIAG(:,:,:)   ! 3D FABM DIAGNOSTIC VARIABLES
   REAL(SP), ALLOCATABLE, TARGET ::  FABM_ALL_HZ(:,:)   ! 2D FABM SURFACE/BOTTOM-BOUND STATE VARIABLES
   REAL(SP), ALLOCATABLE, TARGET ::  FABM_DIAG_HZ(:,:)  ! 2D FABM HORIZONTAL DIAGNOSTIC VARIABLES

   REAL(SP), ALLOCATABLE ::  FABM_F(:,:,:)          !FORECASTED VARIABLES
   REAL(SP), ALLOCATABLE ::  FABM_MEAN(:,:,:)       !MEAN VARIABLES

CONTAINS
      
   SUBROUTINE FABM_INITIALIZE()
      CALL fabm_create_model_from_yaml_file(model)
   END SUBROUTINE FABM_INITIALIZE

   SUBROUTINE FABM_INITIAL()
      CALL FABM_ALLOCATE

      ! TODO: initialize all FABM fields
      ! TODO: send pointers to environemntal variables to FABM
      ! TODO: call fabm_set_domain
      ! TODO: call fabm_check_ready
   END SUBROUTINE FABM_INITIAL

   SUBROUTINE FABM_ALLOCATE()
    ALLOCATE(FABM_ALL(0:MT,KB,size(model%state_variables)))    ; FABM_ALL     =  0.001_SP
    ALLOCATE(FABM_F(0:MT,KB,size(model%state_variables)))      ; FABM_F       =  0.001_SP
    ALLOCATE(FABM_MEAN(0:MT,KB,size(model%state_variables)))  ; FABM_MEAN    =  0.001_SP

    !Following variables appear to be no longer used:
    !ALLOCATE(XFLUX_OBCB(0:MT,KB,size(model%state_variables))) ; XFLUX_OBCB  =  0.0_SP
    !ALLOCATE(FABM_MEANN(0:NT,KB,size(model%state_variables)))  ; FABM_MEANN   =  0.001_SP
    !ALLOCATE(FABM_VAR_MEAN(0:MT,KB,size(model%state_variables))) ; FABM_VAR_MEAN   =  0.0_SP
   END SUBROUTINE

SUBROUTINE FABM_3D1D
     IMPLICIT NONE
     SAVE
!     INTEGER  :: I,J,K,L
!     REAL(SP) :: SPCP,ROSEA,SPRO !,BIO_VAR_MEAN(M,KBM1,NTT)
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
      FABM_VAR(1:KBV,1:size(model%state_variables))=FABM_ALL(I,1:KBV,1:size(model%state_variables))
      CALL FABM_MIXING 
      FABM_ALL(I,1:KBV,1:size(model%state_variables))=FABM_VAR(1:KBV,1:size(model%state_variables))
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

  SUBROUTINE FABM_MIXING()
  END SUBROUTINE

  SUBROUTINE FABM_ADV()
  END SUBROUTINE

  SUBROUTINE FABM_BCOND()
  END SUBROUTINE

END MODULE MOD_FABM_3D