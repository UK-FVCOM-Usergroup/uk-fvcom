!------------------------------------------------------------------------------|
!   THIS MODULE LINKS 1D BIOLOGICAL MODULE TO 3D COMPUTAITION                  |
!------------------------------------------------------------------------------|
MODULE MOD_BIO_3D 

   USE ALL_VARS
   USE BCS
   USE MOD_OBCS
   USE LIMS
   USE CONTROL
#  if defined (MULTIPROCESSOR)
   USE MOD_PAR   
#  endif
#  if defined (SPHERICAL)
   USE MOD_SPHERICAL
#  endif
   USE MOD_TYPES
   USE MOD_UTILS
   USE MOD_1D
   USE MOD_PHYTOPLANKTON !,   ONLY: BIO_P,NNP,INP,IRRAD0,PARFRAC
   USE MOD_ZOOPLANKTON !,     ONLY: BIO_Z,NNZ,INZ
   USE MOD_BACTERIA !,        ONLY: BIO_B,NNB,INB
   USE MOD_DETRITUS !,        ONLY: BIO_D,NND,IND
   USE MOD_DOM !,             ONLY: BIO_DOM,NNM,INM
   USE MOD_NUTRIENT !,        ONLY: BIO_N,NNN,INN
   USE MOD_PARAMETER
   
   IMPLICIT NONE
   SAVE
   REAL(SP), ALLOCATABLE ::  BIO_ALL(:,:,:)        !3D BIO_VARIABLES
   REAL(SP), ALLOCATABLE ::  BIO_F(:,:,:)          !FORECASTED VARIABLES
   REAL(SP), ALLOCATABLE ::  BIO_MEAN(:,:,:)       !MEAN VARIABLES
   REAL(SP), ALLOCATABLE ::  XFLUX_OBC(:,:,:)      !OPEN BOUNDARY FLUX

!JQI   REAL(SP), ALLOCATABLE ::  BIO_MEANN(:,:,:)      !MEAN IN CELLS

!JQI   REAL(SP), ALLOCATABLE :: BIO_VAR_MEAN(:,:,:)
   REAL(SP), ALLOCATABLE :: DWDIS(:,:,:)  !!WATER QUALITY DISCHARGE DATA

!====== TEMPORAL FOR RATE OUTPUT
   REAL(SP), ALLOCATABLE ::  U_PT(:,:)         !3D PRIMARY PRODUCTION
   REAL(SP), ALLOCATABLE ::  G_PT(:,:)         !GRAZING
   REAL(SP), ALLOCATABLE ::  P_DT(:,:)         !PHYTO MORTALITY
   REAL(SP), ALLOCATABLE ::  Z_DT(:,:)         !ZOO MORTALITY
   REAL(SP), ALLOCATABLE ::  REMT(:,:)         !REMINERALIZATION
   REAL(SP), ALLOCATABLE ::  D_SINKT(:,:)      !DETRITUS SINKING FLUX
   REAL(SP), ALLOCATABLE ::  P_SINKT(:,:)      !PHYTO SINKING FLUX

   CONTAINS !------------------------------------------------------------------!
            ! BIO_3D1D             :ADVANCE TMODEL USING GOTM LIBRARIES        !
            ! BIO_ADV              :ADVECTION OF BIOLOGICAL STATE VARIABLES    !
            ! BIO_BCOND            :BOUNDARY CONDITION                         !
            ! BIO_NETCDF_HEADER    :NETCDF HEADER WRITER                       !
            ! BIO_OUT_NETCDF       :NETCDF OUTPUT                              !
            ! BIO_EXCHANGE         :INTERPROCESSOR EXCHANGE                    !
            ! BIO_INITIAL          :INITIALIZATION                             !
            ! BIO_HOT_START        :HOT_START FROM BIO_RESTART.NC              !
            !------------------------------------------------------------------!
   SUBROUTINE BIO_3D1D(CDF_INT)

   IMPLICIT NONE
   SAVE
   
   INTEGER  :: I,J,K,L,N1,index_id,IPOUT,CDF_INT
   REAL(SP) :: SPCP,ROSEA,SPRO !,BIO_VAR_MEAN(M,KBM1,NTT)
   REAL(SP) :: DEPTH_Z(KB),RIEN

   CHARACTER(LEN=3) PP
   SPCP  = 4.2174E3_SP                        !HEAT SPECIFIC CAPACITY
   ROSEA = 1.023E3_SP                         !RHO OF SEA WATER
   SPRO=SPCP*ROSEA
!   BIO_VAR_MEAN = 0.0_SP
!---------------------------                    !MAIN LOOP OVER ELEMENTS

   IF(.NOT. ALLOCATED(DELTA_D)) ALLOCATE(DELTA_D(KBM1))
   IF(.NOT. ALLOCATED(DELTA_Z)) ALLOCATE(DELTA_Z(KBM1))
   IF(.NOT. ALLOCATED(T_BIO))   ALLOCATE(T_BIO(KBM1))
   IF(.NOT. ALLOCATED(KM_BIO))  ALLOCATE(KM_BIO(KBM1))
   IF(.NOT. ALLOCATED(BIO_VAR)) ALLOCATE(BIO_VAR(KBV,NTT))     

#  if defined (ONE_D_MODEL)
   DO I=M,M
#  else
   DO I=1,M
#  endif
    DO K=1,KB                              !3D TO 1D FIELD
     DO J=1,NNN
      BIO_N(K,J)=BIO_ALL(I,K,J+INN-1)
     END DO
     DO J=1,NNP
      BIO_P(K,J)=BIO_ALL(I,K,J+INP-1)
     END DO
     DO J=1,NNZ
      BIO_Z(K,J)=BIO_ALL(I,K,J+INZ-1)
     END DO
     DO J=1,NNM
      BIO_DOM(K,J)=BIO_ALL(I,K,J+INM-1)
     END DO
     DO J=1,NNB
      BIO_B(K,J)=BIO_ALL(I,K,J+INB-1)
     END DO
     DO J=1,NND
      BIO_D(K,J)=BIO_ALL(I,K,J+IND-1)
     END DO
     DELTA_D(K)=DZ(I,K)*D(I)                   !LAYER THICKNESS
     DELTA_Z(K)=DZZ(I,K)*D(I)                  !DISTANCE BETWEEN LAYERS
     DEPTH_Z(K)=Z(I,K)*D(I)                    !LAYER CENTER DEPTH
     IRRAD0=-SWRAD(I)*PARFRAC*SPRO             !PAR FRACTION
     L_NH4N=30._SPP                            !NITRIFICATION USE	 
     T_BIO(K)=T1(I,K)
    END DO                                    !K=1,KB
    T_STEP=DTI
    
!JQIJQI
    index_id = 0
  !  if(ngid(I) == 153 .or. ngid(i) == 5681 .or. ngid(i) == 10244) index_id = 1    
!JQIJQI
    CALL ZOOPLANKTON
    CALL PHYTOPLANKTON(index_id,i,iint)
    CALL BACTERIA
    CALL DETRITUS
    CALL DOM
    CALL NUTRIENT
    DO K=1,KB                                !1D TO 3D FIELD
     DO J=1,NNN
      BIO_ALL(I,K,J+INN-1)=BIO_N(K,J)
     END DO
     DO J=1,NNP
      BIO_ALL(I,K,J+INP-1)=BIO_P(K,J)

     END DO
     DO J=1,NNZ
      BIO_ALL(I,K,J+INZ-1)=BIO_Z(K,J)
     END DO
     DO J=1,NNM
      BIO_ALL(I,K,J+INM-1)=BIO_DOM(K,J)
     END DO
     DO J=1,NNB
      BIO_ALL(I,K,J+INB-1)=BIO_B(K,J)
     END DO
     DO J=1,NND
      BIO_ALL(I,K,J+IND-1)=BIO_D(K,J)
     END DO
    END DO 
   END DO !I=1,M 

!    GO TO 88      !Temporal for nphy, no physical processes
#  if !defined (ONE_D_MODEL)
#  if defined (MULTIPROCESSOR)
   CALL BIO_EXCHANGE
#  endif
   CALL BIO_ADV
#  if defined(MULTIPROCESSOR)
   IF(PAR)THEN
    DO N1=1,NTT
     CALL NODE_MATCH(1,NBN,BN_MLT,BN_LOC,BNC,MT,KB,MYID,NPROCS,BIO_F(:,:,N1))
    END DO
   END IF  
#  endif
   IF(BIO_FCT) CALL FCT_BIO
#  endif

#  if defined (ONE_D_MODEL)
   DO I = M,M
#  else
   DO I = 1,M
#  endif   
    KM_BIO(1:KBV)=KH(I,1:KBV)    !*0.5_SP+UMOL
    BIO_VAR(1:KBV,1:NTT)=BIO_F(I,1:KBV,1:NTT)
    DELTA_D(1:KBV)=DZ(I,1:KBV)*D(I)                   !LAYER THICKNESS
    DELTA_Z(1:KBV)=DZZ(I,1:KBV)*D(I)                  !DISTANCE BETWEEN LAYERS
    DEPTH_Z(1:KBV)=Z(I,1:KBV)*D(I)                    !LAYER CENTER DEPTH
    CALL BIO_MIXING 
    BIO_F(I,1:KBV,1:NTT)=BIO_VAR(1:KBV,1:NTT)

!    BIO_F(I,KBV-1,INN:INN+NNN-1)=BIO_ALL(I,KBV-1,INN:INN+NNN-1)  !Keep nutrient level at the bottom for mass balance
   END DO !I=1,M 

#  if !defined (ONE_D_MODEL)
   CALL BIO_BCOND
!JQIJQI   CALL ADJUST_BIO
   BIO_ALL=BIO_F                                   !UPDATE
#  if defined (MULTIPROCESSOR)
   CALL BIO_EXCHANGE
#  endif
#  endif
!  end if defined 1D
 88  CONTINUE      !Temporal for nphys, no physical processes
   WHERE (BIO_ALL < 0.01) BIO_ALL=0.01
   RETURN
   END SUBROUTINE BIO_3D1D

!=============================================================================!
   SUBROUTINE BIO_ADV  
!=============================================================================!
!                                                                             !
!   This subroutine is used to calculate the horizontal advection and         !
!   and diffusion terms for the state variables of the adjustable biomodel    !
!=============================================================================!

   USE ALL_VARS
   USE BCS
   USE MOD_OBCS
# if defined (MULTIPROCESSOR)
   USE MOD_PAR   
# endif
#  if defined (SPHERICAL)
   USE MOD_SPHERICAL
#  endif
   IMPLICIT NONE

   REAL(SP), DIMENSION(0:MT,KB,NTT) :: XFLUX,RF,XFLUX_ADV
   REAL(SP), DIMENSION(M)           :: PUPX,PUPY,PVPX,PVPY
   REAL(SP), DIMENSION(M)           :: PFPX,PFPY,PFPXD,PFPYD,VISCOFF
   REAL(SP), DIMENSION(3*(NT),KBM1) :: DTIJ
   REAL(SP), DIMENSION(3*(NT),KBM1) :: UVN
   REAL(SP) :: FFD,FF1,X11,Y11,X22,Y22,X33,Y33,TMP1,TMP2,XI,YI
   REAL(SP) :: DXA,DYA,DXB,DYB,FIJ1,FIJ2,UN
   REAL(SP) :: TXX,TYY,FXX,FYY,VISCOF,EXFLUX,TEMP
   REAL(SP) :: FACT,FM1
   REAL(SP) :: TT,TTIME,STPOINT
   INTEGER  :: I,I1,I2,IA,IB,J,J1,J2,JTMP,K,JJ,N1
   REAL(SP) :: WQM1MIN, WQM1MAX, WQM2MIN, WQM2MAX

   REAL(SP), ALLOCATABLE :: WDIS(:,:)     !!FRESH WATER QUALITY AT CURRENT TIME

# if defined (SPHERICAL)
   REAL(SP) :: TY,TXPI,TYPI
   REAL(DP) :: XTMP,XTMP1
   REAL(DP) :: X1_DP,Y1_DP,X2_DP,Y2_DP,XII,YII
   REAL(DP) :: X11_TMP,Y11_TMP,X33_TMP,Y33_TMP
# endif

   REAL(SP)  CONV_S(1:KB), DISS_S(1:KB)
   REAL(SP)  SL_H(0:KB), S_TEMP(0:KB)
   REAL(SP)  SL_U, SL_F

!------------------------------------------------------------------------------
   FACT = 0.0_SP
   FM1  = 1.0_SP
   IF(HORZMIX == 'closure')THEN
     FACT = 1.0_SP
     FM1  = 0.0_SP
   END IF

!
!--Initialize Fluxes-----------------------------------------------------------
!
   XFLUX = 0.0_SP
   XFLUX_ADV = 0.0_SP
!
!--Loop Over Control Volume Sub-Edges And Calculate Normal Velocity------------
!
   DO I=1,NCV
     I1=NTRG(I)
     DO K=1,KBM1
       DTIJ(I,K) = D1(I1)*DZ1(I1,K)
       UVN(I,K)=V(I1,K)*DLTXE(I) - U(I1,K)*DLTYE(I) 
     END DO
   END DO

   TTIME=THOUR

!--Calculate the Advection and Horizontal Diffusion Terms----------------------

   DO N1=1,NTT
    DO K=1,KBM1
     PFPX  = 0.0_SP
     PFPY  = 0.0_SP
     PFPXD = 0.0_SP
     PFPYD = 0.0_SP

     DO I=1,M
      DO J=1,NTSN(I)-1
       I1=NBSN(I,J)
       I2=NBSN(I,J+1)
       FFD=0.5_SP*(BIO_ALL(I1,K,N1)+BIO_ALL(I2,K,N1)          &
           -BIO_MEAN(I1,K,N1)-BIO_MEAN(I2,K,N1))
       FF1=0.5_SP*(BIO_ALL(I1,K,N1)+BIO_ALL(I2,K,N1))
#      if defined (SPHERICAL)
       XTMP  = VX(I2)*TPI-VX(I1)*TPI
       XTMP1 = VX(I2)-VX(I1)
       IF(XTMP1 >  180.0_SP)THEN
        XTMP = -360.0_SP*TPI+XTMP
       ELSE IF(XTMP1 < -180.0_SP)THEN
        XTMP =  360.0_SP*TPI+XTMP
       END IF  
       TXPI=XTMP*COS(DEG2RAD*VY(I))
       TYPI=(VY(I1)-VY(I2))*TPI
       PFPX(I)=PFPX(I)+FF1*TYPI
       PFPY(I)=PFPY(I)+FF1*TXPI
       PFPXD(I)=PFPXD(I)+FFD*TYPI
       PFPYD(I)=PFPYD(I)+FFD*TXPI
#      else
       PFPX(I)=PFPX(I)+FF1*(VY(I1)-VY(I2))
       PFPY(I)=PFPY(I)+FF1*(VX(I2)-VX(I1))
       PFPXD(I)=PFPXD(I)+FFD*(VY(I1)-VY(I2))
       PFPYD(I)=PFPYD(I)+FFD*(VX(I2)-VX(I1))
#      endif
      END DO
      PFPX(I)=PFPX(I)/ART2(I)
      PFPY(I)=PFPY(I)/ART2(I)
      PFPXD(I)=PFPXD(I)/ART2(I)
      PFPYD(I)=PFPYD(I)/ART2(I)
     END DO

     VISCOFF(1:M)=VISCOFH(1:M,K)  !CALCULATED IN viscofh.F

     DO I=1,NCV_I
      IA=NIEC(I,1)
      IB=NIEC(I,2)
      XI=0.5_SP*(XIJE(I,1)+XIJE(I,2))
      YI=0.5_SP*(YIJE(I,1)+YIJE(I,2))
#     if defined (SPHERICAL)
      X1_DP=XIJE(I,1)
      Y1_DP=YIJE(I,1)
      X2_DP=XIJE(I,2)
      Y2_DP=YIJE(I,2)
      CALL ARCC(X2_DP,Y2_DP,X1_DP,Y1_DP,XII,YII)
      XI=XII		
      XTMP  = XI*TPI-VX(IA)*TPI
      XTMP1 = XI-VX(IA)
      IF(XTMP1 >  180.0_SP)THEN
       XTMP = -360.0_SP*TPI+XTMP
      ELSE IF(XTMP1 < -180.0_SP)THEN
       XTMP =  360.0_SP*TPI+XTMP
      END IF	 
      DXA=XTMP*COS(DEG2RAD*VY(IA))   
      DYA=(YI-VY(IA))*TPI
      XTMP  = XI*TPI-VX(IB)*TPI
      XTMP1 = XI-VX(IB)
      IF(XTMP1 >  180.0_SP)THEN
       XTMP = -360.0_SP*TPI+XTMP
      ELSE IF(XTMP1 < -180.0_SP)THEN
       XTMP =  360.0_SP*TPI+XTMP
      END IF	 
      DXB=XTMP*COS(DEG2RAD*VY(IB))  
      DYB=(YI-VY(IB))*TPI
#     else
      DXA=XI-VX(IA)
      DYA=YI-VY(IA)
      DXB=XI-VX(IB)
      DYB=YI-VY(IB)
#     endif
      FIJ1=BIO_ALL(IA,K,N1)+DXA*PFPX(IA)+DYA*PFPY(IA)
      FIJ2=BIO_ALL(IB,K,N1)+DXB*PFPX(IB)+DYB*PFPY(IB)

      WQM1MIN=MINVAL(BIO_ALL(NBSN(IA,1:NTSN(IA)-1),K,N1))
      WQM1MIN=MIN(WQM1MIN, BIO_ALL(IA,K,N1))
      WQM1MAX=MAXVAL(BIO_ALL(NBSN(IA,1:NTSN(IA)-1),K,N1))
      WQM1MAX=MAX(WQM1MAX, BIO_ALL(IA,K,N1))
      WQM2MIN=MINVAL(BIO_ALL(NBSN(IB,1:NTSN(IB)-1),K,N1))
      WQM2MIN=MIN(WQM2MIN, BIO_ALL(IB,K,N1))
      WQM2MAX=MAXVAL(BIO_ALL(NBSN(IB,1:NTSN(IB)-1),K,N1))
      WQM2MAX=MAX(WQM2MAX, BIO_ALL(IB,K,N1))
      IF(FIJ1 < WQM1MIN) FIJ1=WQM1MIN
      IF(FIJ1 > WQM1MAX) FIJ1=WQM1MAX
      IF(FIJ2 < WQM2MIN) FIJ2=WQM2MIN
      IF(FIJ2 > WQM2MAX) FIJ2=WQM2MAX

      UN=UVN(I,K)
  
      VISCOF=HORCON*(FACT*(VISCOFF(IA)+VISCOFF(IB))*0.5_SP + FM1)

      TXX=0.5_SP*(PFPXD(IA)+PFPXD(IB))*VISCOF
      TYY=0.5_SP*(PFPYD(IA)+PFPYD(IB))*VISCOF

      FXX=-DTIJ(I,K)*TXX*DLTYE(I)
      FYY= DTIJ(I,K)*TYY*DLTXE(I)

      EXFLUX=-UN*DTIJ(I,K)*((1.0_SP+SIGN(1.0_SP,UN))*FIJ2+          &
              (1.0_SP-SIGN(1.0_SP,UN))*FIJ1)*0.5_SP+FXX+FYY
 
      XFLUX(IA,K,N1)=XFLUX(IA,K,N1)+EXFLUX
      XFLUX(IB,K,N1)=XFLUX(IB,K,N1)-EXFLUX

      XFLUX_ADV(IA,K,N1)=XFLUX_ADV(IA,K,N1)+(EXFLUX-FXX-FYY)
      XFLUX_ADV(IB,K,N1)=XFLUX_ADV(IB,K,N1)-(EXFLUX-FXX-FYY)

     END DO !to NCV-1

#    if defined (SPHERICAL) && (NORTHPOLE)
     CALL ADV_T_XY(XFLUX(:,:,N1),XFLUX_ADV(:,:,N1),PTPX,PTPY,PTPXD,PTPYD,VISCOFF,K)
#    endif

    END DO !to KBM1
   END DO !to NNT

!
!-Accumulate Fluxes at Boundary Nodes
!
#  if defined (MULTIPROCESSOR)
   DO N1=1,NTT
    IF(PAR)CALL NODE_MATCH(0,NBN,BN_MLT,BN_LOC,BNC,MT,KB,MYID,NPROCS,       &
                           XFLUX(:,:,N1),XFLUX_ADV(:,:,N1))
   END DO
#  endif

   DO N1=1,NTT
    DO K=1,KBM1
     IF(IOBCN > 0) THEN
      DO I=1,IOBCN
       I1=I_OBC_N(I)
       XFLUX_OBC(I,K,N1)=XFLUX_ADV(I1,K,N1)
      END DO
     END IF
    END DO
   END DO

   DO N1=1,NTT
!
!--Calculate the Vertical Terms------------------------------------------------
!
# if !defined (VerSigma)
    DO K=1,KBM1
     DO I=1,M
      IF(K == 1) THEN  
       TEMP=-WTS(I,K+1)*(BIO_ALL(I,K,N1)*DZ(I,K+1)+BIO_ALL(I,K+1,N1)*DZ(I,K))/   &
            (DZ(I,K)+DZ(I,K+1))
      ELSE IF(K == KBM1) THEN
       TEMP=WTS(I,K)*(BIO_ALL(I,K,N1)*DZ(I,K-1)+BIO_ALL(I,K-1,N1)*DZ(I,K))/      &
            (DZ(I,K)+DZ(I,K-1))
      ELSE
       TEMP=WTS(I,K)*(BIO_ALL(I,K,N1)*DZ(I,K-1)+BIO_ALL(I,K-1,N1)*DZ(I,K))/      &
            (DZ(I,K)+DZ(I,K-1))-  &
            WTS(I,K+1)*(BIO_ALL(I,K,N1)*DZ(I,K+1)+BIO_ALL(I,K+1,N1)*DZ(I,K))/    &
	    (DZ(I,K)+DZ(I,K+1))
      END IF

      IF(ISONB(I) == 2) THEN
       XFLUX(I,K,N1)=TEMP*ART1(I)
      ELSE
       XFLUX(I,K,N1)=XFLUX(I,K,N1)+TEMP*ART1(I)
      END IF
     END DO  !i=1,M
    END DO    !k=1,kbm1

# else      
! LAI FORMULA
    DO I=1,M
        S_TEMP(0)  = -BIO_ALL(I,1,N1)
        S_TEMP(KB) = -BIO_ALL(I,KBM1,N1)
        SL_H(0)    = DZ(I,1)
        SL_H(KB)   = DZ(I,KBM1)
        DO K=1, KBM1
          S_TEMP(K) = BIO_ALL(I,K,N1)
          SL_H(K)   = DZ(I,K)
        ENDDO

        DO K=2, KBM1
          CONV_S(K) = WTS(I,K)*(S_TEMP(K)+S_TEMP(K-1))*0.5_SP
          SL_U = 2.0_SP*(S_TEMP(K)-S_TEMP(K+1))/(SL_H(K)+SL_H(K+1))
          SL_F = 2.0_SP*(S_TEMP(K-2)-S_TEMP(K-1))/(SL_H(K-2)+SL_H(K-1))
          DISS_S(K) = 0.5_SP*ABS(WTS(I,K))*(S_TEMP(K-1)-S_TEMP(K)-0.5_SP*LIMLED2(SL_U,SL_F,2.0_SP)*(SL_H(K-1)+SL_H(K)))
        ENDDO
        CONV_S(1)  = 0.0_SP
        DISS_S(1)  = 0.0_SP
        CONV_S(KB) = 0.0_SP
        DISS_S(KB) = 0.0_SP
!
!--Total Fluxes ---------------------------------------------------------------
!
      do k=1,kbm1
       TEMP = CONV_S(K)-CONV_S(K+1)+DISS_S(K+1)-DISS_S(K)

       IF(ISONB(I) == 2) THEN
        XFLUX(I,K,N1)=TEMP*ART1(I)
       ELSE
        XFLUX(I,K,N1)=XFLUX(I,K,N1)+TEMP*ART1(I)
       END IF
      END DO    !k=1,kbm1
    END DO  !i=1,M

# endif

!------------- the salinity flux from ground water ----------------------
   IF(IBFW > 0)THEN
     DO I=1,M
       DO J=1,IBFW
         IF(I == NODE_BFW(J))THEN
           XFLUX(I,KBM1,N1)=XFLUX(I,KBM1,N1)-BFWDIS3(J)*BFWWDIS3(J,N1)
         END IF
       END DO
     END DO
   END IF

!--Update Variables--------------------------------
!

    DO I = 1,M
     DO K = 1, KBM1
      BIO_F(I,K,N1)=(BIO_ALL(I,K,N1)-XFLUX(I,K,N1)/ART1(I)*(DTI/(D(I)*DZ(I,K))))*   &
                    (D(I)/DF(I)) 
     END DO 
    END DO
   END DO !do N1=1,ntt
   BIO_F(:,KB,:)=BIO_ALL(:,KB,:) !FOR BENTHIC PURPOSE

   RETURN
   END SUBROUTINE BIO_ADV

   SUBROUTINE BIO_BCOND
!==============================================================================|
!   Set Boundary Conditions for BioGen                                         |
!==============================================================================|

!------------------------------------------------------------------------------|
   USE ALL_VARS
   USE BCS
   USE MOD_OBCS
#  if defined (BIO_OBC)
   USE MOD_BIO_OBC
#  endif

   IMPLICIT NONE
   REAL(SP) :: BIO2D,BIO2D_NEXT,BIO2D_OBC,XFLUX2D,TMP

#  if defined (BIO_OBC)
   REAL(SP), ALLOCATABLE :: BIO_OBC_TMP(:,:,:)
   INTEGER :: JJ
!   REAL(SP) ::FACT,UFACT
!   INTEGER  ::L1,L2,IERR
#  endif 

   REAL(SP) :: FACT,UFACT
   INTEGER  :: I,J,K,J1,J11,J22,N1,L1,L2,IERR
   REAL(SP), ALLOCATABLE :: WDIS(:,:)     !!RIVER BIOLOGICAL INPUT AT CURRENT TIME
   REAL(SP), ALLOCATABLE :: TTMP(:,:)

   REAL(SP) ::BIOMAX,BIOMIN

!------------------------------------------------------------------------------|
   ALLOCATE(WDIS(NUMQBC,NTT))     ;WDIS      = ZERO

   IF(NUMQBC > 0)THEN
    CALL BRACKET(QBC_TM,THOUR,L1,L2,FACT,UFACT,IERR)
    DO N1 = 1, NTT
     WDIS(:,N1) = UFACT*DWDIS(:,N1,L1) + FACT*DWDIS(:,N1,L2)
    END DO
   END IF
!
!--SET CONDITIONS FOR FRESH WATER INFLOW---------------------------------------|
!
   IF(POINT_ST_TYPE == 'specified') THEN
    IF(NUMQBC > 0) THEN
     IF(INFLOW_TYPE == 'node') THEN
      DO I=1,NUMQBC
       J11=INODEQ(I)
       DO K=1,KBM1
        BIO_F(J11,K,:) = WDIS(I,:)
        DO J22=1,NTSN(J11)    !Temporal for surrounding nodes of rivers
          BIO_F(NBSN(J11,J22),K,:) = WDIS(I,:)
        END DO 
       END DO
      END DO
     ELSE IF(INFLOW_TYPE == 'edge') THEN
      DO I=1,NUMQBC
       J11=N_ICELLQ(I,1)
       J22=N_ICELLQ(I,2)
       DO K=1,KBM1
        BIO_F(J11,K,:) = WDIS(I,:)
        BIO_F(J22,K,:) = WDIS(I,:)
       END DO
      END DO
     END IF
    END IF
   END IF


       
   IF(IOBCN > 0) THEN
#   if defined (BIO_OBC)
    ALLOCATE(BIO_OBC_TMP(1:IOBCN,1:KBM1,1:NTT))
    
    CALL BRACKET(BIO_O_TM,THOUR,L1,L2,FACT,UFACT,IERR)
    IF(IERR==-1)THEN
     BIO_OBC_TMP = 0.0_SP
    ELSE
     BIO_OBC_TMP(1:IOBCN,1:KBM1,1:NTT) = UFACT*BIOOBC(1:IOBCN,1:KBM1,1:NTT,L1)  +&
                                   FACT*BIOOBC(1:IOBCN,1:KBM1,1:NTT,L2)
    END IF
#   endif
!
!  SET CONDITIONS ON OUTER BOUNDARY
!
!    RAMP_TS = TANH(FLOAT(IINT)/FLOAT(IRAMP+1))
    DO N1 = 1,NTT
     ALLOCATE(TTMP(IOBCN,KBM1));  TTMP = 0.0_SP
     DO I=1,IOBCN
      J=I_OBC_N(I)
      J1=NEXT_OBC(I)
      BIO2D=0.0_SP
      BIO2D_NEXT=0.0_SP
      XFLUX2D=0.0_SP
      DO K=1,KBM1
       BIO2D=BIO2D+BIO_F(J,K,N1)*DZ(J,K)
       BIO2D_NEXT=BIO2D_NEXT+BIO_F(J1,K,N1)*DZ(J1,K)
       XFLUX2D=XFLUX2D+XFLUX_OBC(I,K,N1)           
      END DO
  
      IF(UARD_OBCN(I) > 0.0_SP) THEN
       TMP=XFLUX2D+BIO2D*UARD_OBCN(I)
       BIO2D_OBC=(BIO2D*D(J)-TMP*DTI/ART1(J))/DF(J)

       DO K=1,KBM1
        TTMP(I,K) = BIO_F(J1,K,N1) - BIO2D_NEXT
        BIO_F(J,K,N1)=BIO2D_OBC+TTMP(I,K)
       END DO

       DO K=1,KBM1
        BIOMAX = MAXVAL(BIO_ALL(NBSN(J,1:NTSN(J)),K,N1))
        BIOMIN = MINVAL(BIO_ALL(NBSN(J,1:NTSN(J)),K,N1))
         
        IF(K == 1)THEN
         BIOMAX = MAX(BIOMAX,(BIO_ALL(J,K,N1)*DZ(J,K+1)+BIO_ALL(J,K+1,N1)*DZ(J,K))/  &
                  (DZ(J,K)+DZ(J,K+1)))
         BIOMIN = MIN(BIOMIN,(BIO_ALL(J,K,N1)*DZ(J,K+1)+BIO_ALL(J,K+1,N1)*DZ(J,K))/  &
                  (DZ(J,K)+DZ(J,K+1)))
        ELSE IF(K == KBM1)THEN
         BIOMAX = MAX(BIOMAX,(BIO_ALL(J,K,N1)*DZ(J,K-1)+BIO_ALL(J,K-1,N1)*DZ(J,K))/  &
                  (DZ(J,K)+DZ(J,K-1)))
         BIOMIN = MIN(BIOMIN,(BIO_ALL(J,K,N1)*DZ(J,K-1)+BIO_ALL(J,K-1,N1)*DZ(J,K))/  & 
                  (DZ(J,K)+DZ(J,K-1)))
        ELSE
         BIOMAX = MAX(BIOMAX,(BIO_ALL(J,K,N1)*DZ(J,K-1)+BIO_ALL(J,K-1,N1)*DZ(J,K))/  &
                  (DZ(J,K)+DZ(J,K-1)),                             &
                  (BIO_ALL(J,K,N1)*DZ(J,K+1)+BIO_ALL(J,K+1,N1)*DZ(J,K))/           &
                  (DZ(J,K)+DZ(J,K+1)))
         BIOMIN = MIN(BIOMIN,(BIO_ALL(J,K,N1)*DZ(J,K-1)+BIO_ALL(J,K-1,N1)*DZ(J,K))/  &
                  (DZ(J,K)+DZ(J,K-1)),                             &
                  (BIO_ALL(J,K,N1)*DZ(J,K+1)+BIO_ALL(J,K+1,N1)*DZ(J,K))/           &
                  (DZ(J,K)+DZ(J,K+1)))
        END IF
 
        IF(BIOMIN-BIO_F(J,K,N1) > 0.0_SP)BIO_F(J,K,N1) = BIOMIN
        IF(BIO_F(J,K,N1)-BIOMAX > 0.0_SP)BIO_F(J,K,N1) = BIOMAX

!        BIO_F(J,K,N1)=BIO_F(J1,K,N1)    !Temporal, equal to the next interior node

   END DO

      ELSE

       DO K=1,KBM1
!JQI#       if defined (BIO_OBC)
!JQI        IF(IERR.NE.-1)THEN
!JQI         BIO_F(J,K,N1) = BIO_ALL(J,K,N1) - ALPHA_SERIES_OBC*(BIO_ALL(J,K,N1)-BIO_OBC_TMP(I,K,N1))
!JQI        ELSE
!JQI         BIO_F(J,K,N1) = BIO_ALL(J,K,N1)
!JQI        ENDIF 	
!JQI#       else
        BIO_F(J,K,N1) = BIO_ALL(J,K,N1)
!        BIO_F(J,K,N1) = BIO_F(J1,K,N1)  !Temporal equal next node value
!JQI#       endif
       END DO
      END IF
!JQI      if(ngid(j) == 28)write(100,*) BIO_F(J,1,N1),BIO_ALL(J,1,N1),N1
     END DO
     DEALLOCATE(TTMP)
    END DO
    
!#   if defined (BIO_OBC)
!    IF(UARD_OBCN(I) <= 0.0_SP) THEN
!     DO JJ=1,NNN
!      DO I=1,IOBCN
!       DO K=1,KBM1
!        IF(IERR.NE.-1)THEN
!         BIO_F(J,K,JJ+INN-1) = BIO_ALL(J,K,JJ+INN-1) - ALPHA_SERIES_OBC*(BIO_ALL(J,K,JJ+INN-1)    &
!	                      -BIO_OBC_TMP(I,K,JJ+INN-1))
!        ELSE
!         BIO_F(J,K,JJ+INN-1) = BIO_ALL(J,K,JJ+INN-1)
!        ENDIF 	
!       END DO
!      END DO
!     END DO 
!    END IF
!#   endif
    
!#   if defined (BIO_OBC)
!    DEALLOCATE(BIO_OBC_TMP)
!#   endif
 
   END IF

#   if defined (BIO_OBC)
!    IF(UARD_OBCN(I) <= 0.0_SP) THEN
     DO JJ=1,NNN
      DO I=1,IOBCN
           J=I_OBC_N(I)
       DO K=1,KBM1
        IF(IERR.NE.-1)THEN
         BIO_F(J,K,JJ+INN-1) = BIO_ALL(J,K,JJ+INN-1) - ALPHA_SERIES_OBC*(BIO_ALL(J,K,JJ+INN-1)    &
                              -BIO_OBC_TMP(I,K,JJ+INN-1))
       ELSE
         BIO_F(J,K,JJ+INN-1) = BIO_ALL(J,K,JJ+INN-1)
        ENDIF   
       END DO
      END DO
     END DO
!    END IF
#   endif
#   if defined (BIO_OBC)
    IF(IOBCN > 0) THEN
      DEALLOCATE(BIO_OBC_TMP)
    END IF
#   endif

!--SET BOUNDARY CONDITIONS-----------------------------------------------------|
!
   BIO_F(0,1:KBM1,:)=0.0_SP
   
   CALL ADJUST_BIO(WDIS)
   DEALLOCATE(WDIS)

   RETURN
   END SUBROUTINE BIO_BCOND
!==============================================================================|

   SUBROUTINE BIO_EXCHANGE
!==============================================================================!
!   PERFORM DATA EXCHANGE FOR the Generalized biological model                 |
!==============================================================================!
#  if defined (MULTIPROCESSOR)
   USE MOD_PAR
   USE LIMS
   USE CONTROL
   IMPLICIT NONE
   INTEGER :: I3
   DO I3=1,NTT
    IF(PAR) CALL EXCHANGE(NC,MT,KB,MYID,NPROCS,BIO_ALL(:,:,I3))
    IF(PAR) CALL EXCHANGE(NC,MT,KB,MYID,NPROCS,BIO_MEAN(:,:,I3))
    IF(PAR) CALL EXCHANGE(NC,MT,KB,MYID,NPROCS,BIO_F(:,:,I3))
   END DO 
  
   RETURN
#  endif
   END SUBROUTINE BIO_EXCHANGE

   SUBROUTINE BIO_INITIAL
!=============================================================================!
! THS PROGRAM INITIALIZES THE 3D BIOLOGICAL FIELD FOR THE GENERALIZED         !
! BIOLOGICAL MODEL: BIO_ALL(I,K,Nl),N1=1,NTT),AND MEAN VALUES BIO_MEAN        !
! EACH BIOLOGICAL STATE VARIABLE HAS AN INDEPENDENT INITIAL CONDITION FILE    !
! PLACED IN INPDIR. THEY SHOULD BE NAME AS "NUTRIENT_INI_1", "NUTRIENT_INI_2",!
! "PHYTOPLANKTON_INI_1", "ZOOPLANKTON_INI_1", "BACTERIA_INI_1", 'DETRITUS_    !
! INI_1", "DOM_INI_1" AND SO FORTH. THREE TYPES OF INITIAL CONDITIONS WERE    !
! IMPLEMENTED: (1) 'CONSTANT': A SINGLE VALUE; (2) 'LINEAR':WITH AT LEAST TWO !
! PAIRS OF VALUES WITH DEPTH. VARIABLE VALUES WILL BE LINEARLY INTERPOLATED   !
! BETWEEN THE VALUES GIVEN), (3) "DATA": OBSERVATION DATA SHOULD BE INTER-    !
! POLATED ONTO THE GRID POINTS AT STANDARD LEVELS. VARIABLE VALUES WILL BE    !
! INTERPOLATED AT EACH GRID POINT FROM THE DATA. THE TYPE OF INITIAL CONDI-   !
! TION SHOULD BE PUT ON THE FIRST LINE OF EACH INITIAL FILE                   !
!=============================================================================!
   IMPLICIT NONE
   INTEGER :: I,J,K,LL,N_DATA
   CHARACTER(LEN=80) :: ISTR
   CHARACTER(LEN=1)  :: BIO_NUMBER
   CHARACTER(LEN=10) :: INI_TYPE
   REAL(SP), DIMENSION(KBM1)    :: ZM           !GRID DEPTH
   REAL(SP), DIMENSION(500)     :: DEPTH_STD    !STANDARD DEPTH OF DATA
   REAL(SP), DIMENSION(500)     :: DATA_BIO     !STANDARD DATA FOR LINEAR INTERPOLATION
   REAL(SP), DIMENSION(KB)      :: DATA_INT     !INTERPOLDATED VALUES
   REAL(SP), DIMENSION(MGL,KSL) :: TEMPB        !TEMPERAL FOR DATA INPUT
   REAL(SP), DIMENSION(M,KSL)   :: DATA_3D      !3D OBSERVATION DATA
   ALLOCATE(BIO_ALL(0:MT,KB,NTT))    ; BIO_ALL     =  0.001_SP
   ALLOCATE(BIO_F(0:MT,KB,NTT))      ; BIO_F       =  0.001_SP
   ALLOCATE(BIO_MEAN(0:MT,KB,NTT))   ; BIO_MEAN    =  0.001_SP
   ALLOCATE(XFLUX_OBC(0:MT,KB,NTT))  ; XFLUX_OBC   =  0.0_SP
!JQI   ALLOCATE(BIO_MEANN(0:NT,KB,NTT))  ; BIO_MEANN   =  0.001_SP
!JQI   ALLOCATE(BIO_VAR_MEAN(0:MT,KB,NTT)) ; BIO_VAR_MEAN   =  0.0_SP
   ALLOCATE(U_PT(MT,KB))    ; U_PT=0._SP     !3D PRIMARY PRODUCTION
   ALLOCATE(G_PT(MT,KB))    ; G_PT=0._SP     !GRAZING
   ALLOCATE(P_DT(MT,KB))    ; P_DT=0._SP     !PHYTO MORTALITY
   ALLOCATE(Z_DT(MT,KB))    ; Z_DT=0._SP     !ZOO MORTALITY
   ALLOCATE(REMT(MT,KB))    ; REMT=0._SP     !REMINERALIZATION
   ALLOCATE(D_SINKT(MT,KB)) ; D_SINKT=0._SP  !DETRITUS SINKING FLUX
   ALLOCATE(P_SINKT(MT,KB)) ; P_SINKT=0._SP  !PHYTO SINKING FLUX


!*******************************************************************!
! *********     PRINT OUT MODEL SETUP AND PARAMETER VALUES      ****!
!*******************************************************************!
   IF(MSR)THEN
    PRINT*
    PRINT*,'*****************************************************'
    PRINT*,'**  STRUCTURE AND FUNCTION OF THE BIOLOGICAL MODEL **'
    PRINT*,'*****************************************************'

    PRINT*
    PRINT*,'MODEL STRUCTURE        : ',  MODEL_STRUCTURE
    DO I=1,NNN
     PRINT*,'                         ',  NUTRIENT_NAME(I)
    END DO
    DO I=1,NNP
     PRINT*,'                         ',  PHYTO_NAME(I)
    END DO
    DO I=1,NNZ
     PRINT*,'                         ',  ZOO_NAME(I)
    END DO
    DO I=1,NND
     PRINT*,'                         ',  DETRITUS_NAME(I)
    END DO
    DO I=1,NNM
     PRINT*,'                         ', DOM_NAME(I)
    END DO
    DO I=1,NNB
     PRINT*,'                         ', BACTERIA_NAME(I)
    END DO
    WRITE(*,'(A26,A20)')' LIGHT FUNCTION         : ', L_FUNCTION
    WRITE(*,'(A26,A20)')' GRAZING FUNCTION       : ', G_FUNCTION
    PRINT*
    PRINT*,'*********    PHYTOPLANTON PARAMETERS    **************'
    PRINT*
    IF(L_FUNCTION /= 'EXP_LIGHT' .AND. L_FUNCTION /= 'SL62_LIGHT')THEN
     PRINT*,'ALPHA                  : '  , (ALPHAP(I),I=1,NNP)
    END IF
    PRINT*,'L_N COMBINE            : ' , (ALPHA_U(I),I=1,NNP)
    PRINT*,'T FORCING              : ' , (A_TP(I),I=1,NNP)
    PRINT*,'CHL ATTANUATION        : ' , ATANU_C
    PRINT*,'D ATTANUATION          : ', ATANU_D
    PRINT*,'WATER ATTANUATION      : ', ATANU_W
    IF(L_FUNCTION == 'PGH80_LIGHT' .OR. L_FUNCTION == 'V65_LIGHT' .OR. &
       L_FUNCTION == 'BWDC9_LIGHT')THEN       
     PRINT*,'BETAP                  : ', (BETAP(I),I=1,NNP)
    END IF
    PRINT*,'CHL:C                  : ',(CHL2C(I),I=1,NNP)
    PRINT*,'ACTIVE DOM EXUD.       : ',(D_DOM(I),I=1,NNP)
    PRINT*,'PASSIVE DOM EXUD.      : ', (DPDOM(I),I=1,NNP)
    IF(L_FUNCTION == 'SL62_LIGHT' .OR. L_FUNCTION == 'V65_LIGHT' .OR. &
       L_FUNCTION == 'PE78_LIGHT')THEN       
     PRINT*,'OPTIMAL LIGHT          : ',(I_OPT(I),I=1,NNP)
    END IF
    IF(L_FUNCTION == 'MM_LIGHT' .OR. L_FUNCTION == 'LB_LIGHT')THEN 
     PRINT*,'LIGHT HALF SATURATTION : ', (K_LIGHT(I),I=1,NNP)
    END IF
    PRINT*,'MORTALITY              : ', (MPD(I),I=1,NNP)
    PRINT*,'MORTALITY POWER        : ', (M_P(I),I=1,NNP)
    IF(L_FUNCTION == 'LB_LIGHT' .OR. L_FUNCTION == 'V65_LIGHT')THEN 
     PRINT*,'POWER OF LIGHT         : ', (N_P(I),I=1,NNP)
    END IF
    PRINT*,'THRESHOLD              : ', (P_0(I),I=1,NNP)
    PRINT*,'T ON RESPIRATION       : ', RP_T
    PRINT*,'RESPIRATION            : ', (R_P(I),I=1,NNP)
    PRINT*,'OPTIMAL T              : ', (T_OPTP(I),I=1,NNP)
    PRINT*,'MAXIMUM GROWTH         : ', (UMAX(I),I=1,NNP)
    PRINT*,'SINKING VELOCITY       : ', (W_P(I),I=1,NNP)
    PRINT*

    PRINT*,'********    ZOOPLANKTON PARAMETERS   ****************'
    PRINT*
    PRINT*,'ACTIVE RESPIRATION     : ', (ACTIVE_R(I),I=1,NNZ)
    PRINT*,'T FORCING EXPONENTIAL  : ', (A_TZ(I),I=1,NNZ)
    IF(NNB >= 1)THEN
     PRINT*,'EFFICIENCY ON BACTERIA : ',((EFFIB(I,J),I=1,NNB),J=1,NNZ)
    END IF
    IF(NND >= 1)THEN
     PRINT*,'EFFICIENCY ON DETRITUS : ',((EFFID(I,J),I=1,NND),J=1,NNZ)
    END IF
    PRINT*,'EFFICIENCY ON PHYTO    : ',((EFFIP(I,J),I=1,NNP),J=1,NNZ)
    PRINT*,'EFFICIENCY ON ZOO      : ',((EFFIZ(I,J),I=1,NNZ),J=1,NNZ)
    PRINT*,'MAX GRAIZING RATE      : ',(G_MAX(I),I=1,NNZ)
    IF(G_FUNCTION == 'RECTI_G' .OR. G_FUNCTION == 'MM1_G' .AND. &
       G_FUNCTION == 'MM2_G')THEN 
     PRINT*,'HALF SATURATION        : ', (K_ZG(I),I=1,NNZ)
    END IF
    PRINT*,'GRAZING POWER          : ',(M_G(I),I=1,NNZ)
    PRINT*,'MORALITY               : ',(MZD(I),I=1,NNZ)
    PRINT*,'MORTALITY POWER        : ',(M_Z(I),I=1,NNZ)
    IF(G_FUNCTION == 'MM2_G')THEN 
     PRINT*,'GRAZING THRESHOLD      : ',(P_C(I),I=1,NNZ)
    END IF
    IF(NNZ >= 1)THEN
     PRINT*,'RECRUITMENT            : ',(R_RECRUIT(I),I=1,NNZ)
    END IF
    PRINT*,'RESPIRATION            : ',(R_Z(I),I=1,NNZ)
    IF(NNB >= 1)THEN
     PRINT*,'PREFERENCE ON BACTERIA : ',((SIGMA_B(I,J),I=1,NNB),J=1,NNZ)
    END IF
    IF(NND >= 1)THEN
     PRINT*,'PREFERENCE ON DETRITUS : ',((SIGMA_D(I,J),I=1,NND),J=1,NNZ)
    END IF
    PRINT*,'PREFERENCE ON PHYTO    : ',((SIGMA_P(I,J),I=1,NNP),J=1,NNZ)
    PRINT*,'PREFERENCE ON ZOO      : ',((SIGMA_Z(I,J),I=1,NNZ),J=1,NNZ)
    PRINT*,'OPTIMAL T              : ',(T_OPTZ(I),I=1,NNZ)
    PRINT*,'ZOO THRESHOLD          : ',(Z_0(I),I=1,NNZ)
    PRINT*
    PRINT*,'*********    NUTRIENT PARAMETERS    ***********'
    PRINT*
    PRINT*,'HALF-SATURATION',((KSN(I,J),I=1,NNN),J=1,NNP)
    IF(NNB >= 1)THEN
     PRINT*,'ELEMENT RATIO IN BAC.  : ',((N2CB(I,J),I=1,NNN),J=1,NNB)
    END IF
    IF(NND >= 1)THEN
     PRINT*,'ELEMENT RATIO IN D     : ',((N2CD(I,J),I=1,NNN),J=1,NND)
    END IF
    PRINT*,'ELEMENT RATIO IN PHYTO : ',((N2CP(I,J),I=1,NNN),J=1,NNP)
    PRINT*,'ELEMENT RATIO IN ZOO   : ',((N2CZ(I,J),I=1,NNN),J=1,NNZ)
    IF(NNM >= 1)THEN
     PRINT*,'ELEMENT RATIO IN DOM   : ', ((N2CDOM(I,J),I=1,NNN),J=1,NNM)
    END IF
    PRINT*,'THRESHOLD              : ', (N_0(I),I=1,NNN)
    IF(NO3_ON)THEN
     PRINT*,'NITRIFICATION RATE     : ', R_AN
    END IF
    PRINT*
    IF(NNB >= 1)THEN
     PRINT*,'*********  BACTERIA PARAMETERS  ************'
     PRINT*
     PRINT*,'T_FORCING              : ',(A_TB(I),I=1,NNB)
     PRINT*,'THRESHOLD              : ', (B_0(I),I=1,NNB)
     PRINT*,'RATIO OF NH4 VS DON    : ',(DELTA_B(I),I=1,NNB)
     PRINT*,'EFFICIENCY OF DETRITUS : ',((EFFIBD(I,J),I=1,NND),J=1,NNB)
     PRINT*,'EFFICIENCY OF DOM      : ',((EFFIDOM(I,J),I=1,NNM),J=1,NNB)
     PRINT*,'EFFICIENCY OF NUTRIENT : ', ((EFFIN(I,J),I=1,NNN),J=1,NNB)
     PRINT*,'RESPIRATION            : ',(R_B(I),I=1,NNB)
     PRINT*,'PREFERENCE ON DETRITUS : ',((SIGMA_BD(I,J),I=1,NND),J=1,NNB)
     PRINT*,'PREFERENCE ON DOM      : ',((SIGMA_DOM(I,J),I=1,NNM),J=1,NNB)
     PRINT*,'PREFERENCE ON NUTRIENT : ',((SIGMA_N(I,J),I=1,NNN),J=1,NNB)
     PRINT*,'OPTIMAL TEMPERATURE    : ',(T_OPTB(I),I=1,NNB)
     PRINT*,'MAXIMUM GROWTH RATE    : ',(UBMAX(I),I=1,NNB)
     PRINT*
    END IF
    IF(NND >= 1)THEN
     PRINT*, '********  DETRITUS PARAMETERS  *************'
     PRINT*
     IF(NNB >= 1)THEN
      PRINT*,'GRAZING ON B TO D      : ', (((ALPHA_BD(I,J,K),I=1,NND),J=1,NNB),K=1,NNZ)
     END IF
     PRINT*,'AGGREGATION            : ', (ALPHA_DAG(I),I=1,NND)
     PRINT*,'GRAZING LOSS ON D TO D : ', (((ALPHA_DD(I,J,K),I=1,NND),J=1,NND),K=1,NNZ)
     PRINT*,'DISAGGREGATION         : ', (ALPHA_DAG(I),I=1,NND)
     PRINT*,'GRAZING LOSS ON P TO D : ', (((ALPHA_PD(I,J,K),I=1,NND),J=1,NNP),K=1,NNZ)
     PRINT*,'GRAZING LOSS ON Z TO D : ', (((ALPHA_ZD(I,J,K),I=1,NND),J=1,NNZ),K=1,NNZ)
     IF(NNM >= 1)THEN
      PRINT*,'DISSOLUTION            : ', (D_DOM(I),I=1,NND)
     END IF
     PRINT*,'REMINERALIZATION       : ', (D_RN(I),I=1,NND)
     PRINT*,'THRESHOLD              : ', (D_0(I),I=1,NND)
     PRINT*,'P MORTALITY TO DETRITUS: ', ((EPSILON_PD(I,J),I=1,NND),J=1,NNP)
     PRINT*,'Z MORTALITY TO DETRITUS: ', ((EPSILON_ZD(I,J),I=1,NND),J=1,NNZ)
     PRINT*,'SINING VELOCITY        : ', (W_D(I),I=1,NND)
     PRINT*
    END IF
    IF(NNM >= 1)THEN
     PRINT*,'*********     DOM PARAMETERS     *************'
     PRINT*
     PRINT*,'DOM AGEING COEFFICIENT : ', (ALPHA_DOM(I),I=1,NNM)
     PRINT*,'PHYTO EXUDATION        : ', ((ALPHA_PDOM(I,J),I=1,NNM),J=1,NNP)
     PRINT*,'DETRITUS DISSOLUTION   : ', ((ALPHA_DDOM(I,J),I=1,NNM),J=1,NND)
     IF(NNB >= 1)THEN
      PRINT*,'GRAZING LOSS ON B > DOM: ', (((ALPHA_ZBDOM(I,J,K),I=1,NNM),J=1,NNB),K=1,NNZ)
     END IF
     IF(NND >= 1)THEN
      PRINT*,'GRAZING LOSS ON D > DOM: ', (((ALPHA_ZDDOM(I,J,K),I=1,NNM),J=1,NND),K=1,NNZ)
     END IF
     PRINT*,'GRAZING LOSS ON P > DOM: ', (((ALPHA_ZPDOM(I,J,K),I=1,NNM),J=1,NNP),K=1,NNZ)
     PRINT*,'GRAZING LOSS ON Z > DOM: ', (((ALPHA_ZZDOM(I,J,K),I=1,NNM),J=1,NNZ),K=1,NNZ)
     PRINT*,'THRESHOLD              : ', (DOM_0(I),I=1,NNM)
     PRINT*
    END IF
    PRINT*
    PRINT*,'*****************************************************'
    PRINT*,'*********    END OF BIOLOGICAL MODEL     ************'
    PRINT*,'*****************************************************'
   END IF !(MSR)

!*********      NUTRIENT INITIAL CONDITIONS   *************
   DO LL=1,NNN
    WRITE(BIO_NUMBER,'(I1.1)')LL
    OPEN(1,FILE='INPDIR/NUTRIENT_INI_'//TRIM(BIO_NUMBER)//'.dat',STATUS='old')
    READ(1,*)INI_TYPE
    IF(TRIM(INI_TYPE) == 'CONSTANT')THEN
     READ(1,*) DATA_BIO(1)
     DO I=1,M
      DO K=1,KB
       BIO_ALL(I,K,LL+INN-1)=DATA_BIO(1)
      END DO
     END DO
    END IF
    IF(TRIM(INI_TYPE) == 'LINEAR')THEN
     N_DATA=1
 11  READ(1,*,END=12)DEPTH_STD(N_DATA),DATA_BIO(N_DATA)
     N_DATA=N_DATA+1
     GO TO 11
 12  N_DATA=N_DATA-1
     DO I=1,M
      DO K= 1,KBM1
       ZM(K) =ZZ(I,K)*(D(I)+EL(I))
      END DO
      CALL SINTER(DEPTH_STD,DATA_BIO,ZM,DATA_INT,N_DATA,KBM1)
      DO K =1,KBM1
       BIO_ALL(I,K,LL+INN-1) = DATA_INT(K)
      END DO
     END DO
    END IF !LINEAR
    IF(TRIM(INI_TYPE) == 'DATA')THEN
     DO I=1,MGL 
      READ(1,*) (TEMPB(I,K), K=1,KSL)
     END DO

     IF(SERIAL) DATA_3D = TEMPB

#    if defined (MULTIPROCESSOR)
     IF(PAR)THEN
      DO I=1,M
       DATA_3D(I,1:KSL) = TEMPB(NGID(I),1:KSL)
      END DO
     END IF
#    endif
     DO I=1,M
      DO K=1,KSL
       DATA_BIO(K)=DATA_3D(I,K)
      END DO
      DO K= 1,KBM1
       ZM(K) =ZZ(I,K)*D(I)+EL(I)
      END DO
      CALL SINTER(DPTHSL,DATA_BIO,ZM,DATA_INT,KSL,KBM1)
      DO K =1,KBM1
       BIO_ALL(I,K,LL+INN-1) = DATA_INT(K)
      END DO
     END DO
    END IF !DATA
   END DO !L=1,NNN; NUTRIENT INITIALIZATION

!*********      PHYTOPLANKTON INITIAL CONDITIONS   *************
   DO LL=1,NNP
    WRITE(BIO_NUMBER,'(I1.1)')LL
    OPEN(1,FILE='INPDIR/PHYTOPLANKTON_INI_'//TRIM(BIO_NUMBER)//'.dat',STATUS='old')
    READ(1,*)INI_TYPE
    IF(TRIM(INI_TYPE) == 'CONSTANT')THEN
     READ(1,*)DATA_BIO(1)
     DO I=1,M
      DO K=1,KB
       BIO_ALL(I,K,LL+INP-1)=DATA_BIO(1)
      END DO
     END DO
    END IF
    IF(TRIM(INI_TYPE) == 'LINEAR')THEN
     N_DATA=1
 21  READ(1,*,END=22)DEPTH_STD(N_DATA),DATA_BIO(N_DATA)
     N_DATA=N_DATA+1
     GO TO 21
 22  N_DATA=N_DATA-1
     DO I=1,M
      DO K= 1,KBM1
       ZM(K) =ZZ(I,K)*D(I)+EL(I)
      END DO
      CALL SINTER(DEPTH_STD,DATA_BIO,ZM,DATA_INT,N_DATA,KBM1)
      DO K =1,KBM1
       BIO_ALL(I,K,LL+INP-1) = DATA_INT(K)
      END DO
     END DO
    END IF !LINEAR

    IF(TRIM(INI_TYPE) == 'DATA')THEN
     DO I=1,MGL 
      READ(1,*) (TEMPB(I,K), K=1,KSL)
     END DO

     IF(SERIAL) DATA_3D = TEMPB
!     IF(SERIAL) DATA_3D = TEMPB/2.0

#    if defined (MULTIPROCESSOR)
     IF(PAR)THEN
      DO I=1,M
       DATA_3D(I,1:KSL) = TEMPB(NGID(I),1:KSL)
!       DATA_3D(I,1:KSL) = TEMPB(NGID(I),1:KSL)/2.0
      END DO
     END IF
#    endif
          
!JQIJI
     DO I=1,M
      DO K=1,KSL
       IF(DATA_3D(I,K) > DATA_3D(I,1))               &
        DATA_3D(I,K) = DATA_3D(I,1)           !chl always smaller in the deep layers
      END DO
     END DO    
!JQIJI
	    
     DO I=1,M
      DO K=1,KSL
       DATA_BIO(K)=DATA_3D(I,K)
      END DO
      DO K= 1,KBM1
       ZM(K) =ZZ(I,K)*D(I)+EL(I)
      END DO
      CALL SINTER(DPTHSL,DATA_BIO,ZM,DATA_INT,KSL,KBM1)
      DO K =1,KBM1
       BIO_ALL(I,K,LL+INP-1) = DATA_INT(K)
      END DO
     END DO
    END IF !DATA
   END DO !L=1,NNP; PHYTOPLANKTON INITIALIZATION

!*********      ZOOPLANKTON INITIAL CONDITIONS   *************
   DO LL=1,NNZ
    WRITE(BIO_NUMBER,'(I1.1)')LL
    OPEN(1,FILE='INPDIR/ZOOPLANKTON_INI_'//TRIM(BIO_NUMBER)//'.dat',STATUS='old')
    READ(1,*)INI_TYPE
    IF(TRIM(INI_TYPE) == 'CONSTANT')THEN
     READ(1,*)DATA_BIO(1)
     DO I=1,M
      DO K=1,KB
       BIO_ALL(I,K,LL+INZ-1)=DATA_BIO(1)
      END DO
     END DO
    END IF
    IF(TRIM(INI_TYPE) == 'LINEAR')THEN
     N_DATA=1
 31  READ(1,*,END=32)DEPTH_STD(N_DATA),DATA_BIO(N_DATA)
     N_DATA=N_DATA+1
     GO TO 31
 32  N_DATA=N_DATA-1
     DO I=1,M
      DO K= 1,KBM1
       ZM(K) =ZZ(I,K)*D(I)+EL(I)
      END DO
      CALL SINTER(DEPTH_STD,DATA_BIO,ZM,DATA_INT,N_DATA,KBM1)
      DO K =1,KBM1
       BIO_ALL(I,K,LL+INZ-1) = DATA_INT(K)
      END DO
     END DO
    END IF !LINEAR

    IF(TRIM(INI_TYPE) == 'DATA')THEN
     DO I=1,MGL 
      READ(1,*) (TEMPB(I,K), K=1,KSL)
     END DO

     IF(SERIAL) DATA_3D = TEMPB

#    if defined (MULTIPROCESSOR)
     IF(PAR)THEN
      DO I=1,M
       DATA_3D(I,1:KSL) = TEMPB(NGID(I),1:KSL)
      END DO
     END IF
#    endif
     DO I=1,M
      DO K=1,KSL
       DATA_BIO(K)=DATA_3D(I,K)
      END DO
      DO K= 1,KBM1
       ZM(K) =ZZ(I,K)*D(I)+EL(I)
      END DO
      CALL SINTER(DPTHSL,DATA_BIO,ZM,DATA_INT,KSL,KBM1)
      DO K =1,KBM1
       BIO_ALL(I,K,LL+INZ-1) = DATA_INT(K)
      END DO
     END DO
    END IF !DATA
   END DO !L=1,NNZ; ZOOPLANKTON INITIALIZATION


!*********      BACTERIA INITIAL CONDITIONS   *************
   DO LL=1,NNB
    WRITE(BIO_NUMBER,'(I1.1)')LL
    OPEN(1,FILE='INPDIR/BACTERIA_INI_'//TRIM(BIO_NUMBER)//'.dat',STATUS='old')
    READ(1,*)INI_TYPE
    IF(TRIM(INI_TYPE) == 'CONSTANT')THEN
     READ(1,*)DATA_BIO(1)
     DO I=1,M
      DO K=1,KB
       BIO_ALL(I,K,LL+INB-1)=DATA_BIO(1)
      END DO
     END DO
    END IF
    IF(TRIM(INI_TYPE) == 'LINEAR')THEN
     N_DATA=1
 41  READ(1,*,END=42)DEPTH_STD(N_DATA),DATA_BIO(N_DATA)
     N_DATA=N_DATA+1
     GO TO 41
 42  N_DATA=N_DATA-1
     DO I=1,M
      DO K= 1,KBM1
       ZM(K) =ZZ(I,K)*D(I)+EL(I)
      END DO
      CALL SINTER(DEPTH_STD,DATA_BIO,ZM,DATA_INT,N_DATA,KBM1)
      DO K =1,KBM1
       BIO_ALL(I,K,LL+INB-1) = DATA_INT(K)
      END DO
     END DO
    END IF !LINEAR

    IF(TRIM(INI_TYPE) == 'DATA')THEN
     DO I=1,MGL 
      READ(1,*) (TEMPB(I,K), K=1,KSL)
     END DO

     IF(SERIAL) DATA_3D = TEMPB

#    if defined (MULTIPROCESSOR)
     IF(PAR)THEN
      DO I=1,M
       DATA_3D(I,1:KSL) = TEMPB(NGID(I),1:KSL)
      END DO
     END IF
#    endif
     DO I=1,M
      DO K=1,KSL
       DATA_BIO(K)=DATA_3D(I,K)
      END DO
      DO K= 1,KBM1
       ZM(K) =ZZ(I,K)*D(I)+EL(I)
      END DO
      CALL SINTER(DPTHSL,DATA_BIO,ZM,DATA_INT,KSL,KBM1)
      DO K =1,KBM1
       BIO_ALL(I,K,LL+INB-1) = DATA_INT(K)
      END DO
     END DO
    END IF !DATA
   END DO !L=1,NNB; BACTERIA INITIALIZATION


!*********      DOM INITIAL CONDITIONS   *************
   DO LL=1,NNM
    WRITE(BIO_NUMBER,'(I1.1)')LL
    OPEN(1,FILE='INPDIR/DOM_INI_'//TRIM(BIO_NUMBER)//'.dat',STATUS='old')
    READ(1,*)INI_TYPE
    IF(TRIM(INI_TYPE) == 'CONSTANT')THEN
     READ(1,*)DATA_BIO(1)
     DO I=1,M
      DO K=1,KB
       BIO_ALL(I,K,LL+INM-1)=DATA_BIO(1)
      END DO
     END DO
    END IF
    IF(TRIM(INI_TYPE) == 'LINEAR')THEN
     N_DATA=1
 51  READ(1,*,END=52)DEPTH_STD(N_DATA),DATA_BIO(N_DATA)
     N_DATA=N_DATA+1
     GO TO 51
 52  N_DATA=N_DATA-1
     DO I=1,M
      DO K= 1,KBM1
       ZM(K) =ZZ(I,K)*D(I)+EL(I)
      END DO
      CALL SINTER(DEPTH_STD,DATA_BIO,ZM,DATA_INT,N_DATA,KBM1)
      DO K =1,KBM1
       BIO_ALL(I,K,LL+INM-1) = DATA_INT(K)
      END DO
     END DO
    END IF !LINEAR

    IF(TRIM(INI_TYPE) == 'DATA')THEN
     DO I=1,MGL 
      READ(1,*) (TEMPB(I,K), K=1,KSL)
     END DO

     IF(SERIAL) DATA_3D = TEMPB

#    if defined (MULTIPROCESSOR)
     IF(PAR)THEN
      DO I=1,M
       DATA_3D(I,1:KSL) = TEMPB(NGID(I),1:KSL)
      END DO
     END IF
#    endif
     DO I=1,M
      DO K=1,KSL
       DATA_BIO(K)=DATA_3D(I,K)
      END DO
      DO K= 1,KBM1
       ZM(K) =ZZ(I,K)*D(I)+EL(I)
      END DO
      CALL SINTER(DPTHSL,DATA_BIO,ZM,DATA_INT,KSL,KBM1)
      DO K =1,KBM1
       BIO_ALL(I,K,LL+INM-1) = DATA_INT(K)
      END DO
     END DO
    END IF !DATA
   END DO !L=1,NNM; DOM INITIALIZATION

!*********      DETRITUS INITIAL CONDITIONS   *************
   DO LL=1,NND
    WRITE(BIO_NUMBER,'(I1.1)')LL
    OPEN(1,FILE='INPDIR/DETRITUS_INI_'//TRIM(BIO_NUMBER)//'.dat',STATUS='old')
    READ(1,*)INI_TYPE
    IF(TRIM(INI_TYPE) == 'CONSTANT')THEN
     READ(1,*)DATA_BIO(1)
     DO I=1,M
      DO K=1,KB
       BIO_ALL(I,K,LL+IND-1)=DATA_BIO(1)
      END DO
     END DO
    END IF
    IF(TRIM(INI_TYPE) == 'LINEAR')THEN
     N_DATA=1
 61  READ(1,*,END=62)DEPTH_STD(N_DATA),DATA_BIO(N_DATA)
     N_DATA=N_DATA+1
     GO TO 61
 62  N_DATA=N_DATA-1
     DO I=1,M
      DO K= 1,KBM1
       ZM(K) =ZZ(I,K)*D(I)+EL(I)
      END DO
      CALL SINTER(DEPTH_STD,DATA_BIO,ZM,DATA_INT,N_DATA,KBM1)
      DO K =1,KBM1
       BIO_ALL(I,K,LL+IND-1) = DATA_INT(K)
      END DO
     END DO
    END IF !LINEAR

    IF(TRIM(INI_TYPE) == 'DATA')THEN
     DO I=1,MGL 
      READ(1,*) (TEMPB(I,K), K=1,KSL)
     END DO

     IF(SERIAL) DATA_3D = TEMPB

#    if defined (MULTIPROCESSOR)
     IF(PAR)THEN
      DO I=1,M
       DATA_3D(I,1:KSL) = TEMPB(NGID(I),1:KSL)
      END DO
     END IF
#    endif
     DO I=1,M
      DO K=1,KSL
       DATA_BIO(K)=DATA_3D(I,K)
      END DO
      DO K= 1,KBM1
       ZM(K) =ZZ(I,K)*D(I)+EL(I)
      END DO
      CALL SINTER(DPTHSL,DATA_BIO,ZM,DATA_INT,KSL,KBM1)
      DO K =1,KBM1
       BIO_ALL(I,K,LL+IND-1) = DATA_INT(K)
      END DO
     END DO
    END IF !DATA
   END DO !L=1,NND; DETRITUS INITIALIZATION
   WHERE(BIO_ALL < 0.001) BIO_ALL=0.001
!JQI   
!JQI   BIO_MEAN=BIO_ALL   !3D ASSIGNMENT
   BIO_MEAN = 0.001    
!JQI
#  if defined (MULTIPROCESSOR)
   IF(PAR) CALL BIO_EXCHANGE
#  endif

   RETURN
   END SUBROUTINE BIO_INITIAL


   SUBROUTINE BIO_HOT_START
!  THIS SUBROUTINE READS IN RESTART BIOLOGICAL DATA FROM THE NETCDF FILE restart_bio.nc  !
!  IT QUERIES THE DIMENSION AND TAKES THE LAST TIME STEP AS THE RESTART DATA.            !

   USE NETCDF  

   IMPLICIT NONE
   SAVE
   INTEGER  ::  I,J,K,IERR,NC_FID,N_START,VARID,DIMID,DIMS(3),DIMSI(1)
   INTEGER  ::  IINT_TMP
!   REAL(SP) ::  TEMPB(MGL,KBM1)
   REAL(SP) ::  TEMPB(MGL,KB)
   CHARACTER(LEN=20) ::  TEMPNAME,time
   ALLOCATE(BIO_ALL(0:MT,KB,NTT))      ; BIO_ALL      = 0.001_SP
   ALLOCATE(BIO_F(0:MT,KB,NTT))        ; BIO_F        = 0.001_SP
   ALLOCATE(BIO_MEAN(0:MT,KB,NTT))    ; BIO_MEAN     = 0.001_SP
   ALLOCATE(XFLUX_OBC(0:MT,KB,NTT))   ; XFLUX_OBC   = 0.0_SP
!JQI   ALLOCATE(BIO_MEANN(0:NT,KB,NTT))    ; BIO_MEANN    = 0.001_SP
!JQI   ALLOCATE(BIO_VAR_MEAN(0:MT,KB,NTT)) ; BIO_VAR_MEAN = 0.0_SP
!====== TEMPORAL FOR RATE OUTPUT
   ALLOCATE(U_PT(MT,KB))    ; U_PT=0._SP     !3D PRIMARY PRODUCTION
   ALLOCATE(G_PT(MT,KB))    ; G_PT=0._SP     !GRAZING
   ALLOCATE(P_DT(MT,KB))    ; P_DT=0._SP     !PHYTO MORTALITY
   ALLOCATE(Z_DT(MT,KB))    ; Z_DT=0._SP     !ZOO MORTALITY
   ALLOCATE(REMT(MT,KB))    ; REMT=0._SP     !REMINERALIZATION
   ALLOCATE(D_SINKT(MT,KB)) ; D_SINKT=0._SP  !DETRITUS SINKING FLUX
   ALLOCATE(P_SINKT(MT,KB)) ; P_SINKT=0._SP  !PHYTO SINKING FLUX



!*******************   START EXECUTABLE      *******************!
   IERR = NF90_OPEN('restart_bio.nc',NF90_NOWRITE,NC_FID)
   IF(IERR /=NF90_NOERR)THEN
    WRITE(*,*)' ERROR IN OPENNING restart_bio.nc '
    CALL PSTOP
   END IF
   IERR = NF90_INQ_DIMID(NC_FID,'time',DIMID)   !GET time DIMENSION ID
   IF(IERR /=NF90_NOERR)THEN
    WRITE(*,*)' ERROR GETTING time ID: '
    CALL PSTOP
   END IF
   IERR = NF90_INQUIRE_DIMENSION(NC_FID,DIMID,TEMPNAME,N_START)
   IF(IERR /=NF90_NOERR)THEN
    WRITE(*,*)' ERROR GETTING TIME STEPS '
    CALL PSTOP
   END IF
!**************   DETERMINE START POINT
   DIMS(1) = 1
   DIMS(2) = 1
   DIMS(3) = N_START
   DO I=1,NTT
    IERR = nf90_inq_varid(NC_FID,TRIM(BIO_NAME(I,1)),VARID)
    IF(IERR /= NF90_NOERR)THEN
     WRITE(*,*)'ERROR GETTING THE ID OF ',TRIM(BIO_NAME(I,1))
     CALL PSTOP
    END IF

    IERR = nf90_get_var(NC_FID,VARID,TEMPB,START=DIMS)
    IF(IERR /= NF90_NOERR)THEN
     WRITE(*,*)'ERROR GETTING RESTART DATA OF ',TRIM(BIO_NAME(I,1))
     CALL PSTOP
    END IF

!    IF(SERIAL) BIO_ALL(1:MT,1:KBM1,I)=TEMPB(1:MT,1:KBM1)
    IF(SERIAL) BIO_ALL(1:MT,1:KB,I)=TEMPB(1:MT,1:KB)
#   if defined (MULTIPROCESSOR)
    IF(PAR)THEN
     DO J=1,M
!      BIO_ALL(J,1:KBM1,I)=TEMPB(NGID(J),1:KBM1)
      BIO_ALL(J,1:KB,I)=TEMPB(NGID(J),1:KB)
     END DO
    END IF
#   endif
   END DO  !DO I=1,NNT

!JQI
!JQI   BIO_MEAN=BIO_ALL
   BIO_MEAN = 0.001_SP
!JQI   
#  if defined (MULTIPROCESSOR)
   IF (PAR) CALL BIO_EXCHANGE
#  endif
!JQI      CALL BIO_NETCDF_HEADER

   DIMSI(1) = N_START
   IERR = NF90_INQ_VARID(NC_FID,'iint',VARID)
   IF(IERR /= NF90_NOERR)THEN
    WRITE(*,*)'ERROR GETTING THE ID OF iint'
    CALL PSTOP
   END IF

   IERR = NF90_GET_VAR(NC_FID,VARID,IINT_TMP,START=DIMSI)
   IF(IERR /= NF90_NOERR)THEN
    WRITE(*,*)'ERROR GETTING RESTART DATA OF iint'
    CALL PSTOP
   END IF
   IINT = IINT_TMP
!JQI   
   IINT = 0
!JQI   

   IERR=NF90_CLOSE(NC_FID)

   RETURN
   END SUBROUTINE BIO_HOT_START

!==============================================================================|
!   Implement Fresh Water Boundary Condition on Bottom (Groundwater)           |
!==============================================================================|

   SUBROUTINE BCOND_BFW           

!------------------------------------------------------------------------------|
   USE ALL_VARS
   USE BCS
   IMPLICIT NONE
   REAL(SP) :: FACT,UFACT
   INTEGER  :: L1,L2,IERR 
!------------------------------------------------------------------------------|

   IF(IBFW <= 0)RETURN

   CALL BRACKET(BFW_TM,THOUR,L1,L2,FACT,UFACT,IERR)
   
   IF(.NOT. ALLOCATED(BFWDIS3))  ALLOCATE(BFWDIS3(IBFW))
   IF(.NOT. ALLOCATED(BFWWDIS3)) ALLOCATE(BFWWDIS3(IBFW,NTT))

   IF(IERR == -1)THEN
    BFWDIS3 = 0.0_SP
    BFWWDIS3 = 0.0_SP
   ELSE
    BFWDIS3(:)=UFACT*BFWQDIS(:,L1)+FACT*BFWQDIS(:,L2)
    BFWWDIS3(:,:)=UFACT*BFWQWDIS(:,:,L1)+FACT*BFWQWDIS(:,:,L2)
   END IF

   RETURN
   END SUBROUTINE BCOND_BFW
!==============================================================================|


   REAL(SP) FUNCTION LIMLED2(A,B,ALPHA) RESULT(LIM)
   IMPLICIT NONE

   REAL(SP) A,B,ALPHA

   LIM = 0.5_SP*(SIGN(1.,A)+SIGN(1.,B))*MIN(0.5_SP*ABS(A+B),ALPHA*ABS(A),ALPHA*ABS(B))

   END FUNCTION LIMLED2

END MODULE MOD_BIO_3D
