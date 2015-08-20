MODULE MOD_ACTION_EX
      
  IMPLICIT NONE
  SAVE
   
  REAL, ALLOCATABLE :: N31(:,:)
  REAL, ALLOCATABLE :: N32(:,:,:)            

  CONTAINS
!
!==============================================================================|
!
!==========================================================================|
!
  SUBROUTINE ACTION_ALLO
   
  USE ALL_VARS, ONLY : MT
  USE SWCOMM3, ONLY : MDC,MSC
!  USE MOD_USGRID, ONLY : MDC,MSC
  IMPLICIT NONE
   
  ALLOCATE(N31(MDC,MSC))   ;  N31 = 0.0
  ALLOCATE(N32(MDC,MSC,0:MT));  N32 = 0.0
   
  RETURN
  END SUBROUTINE ACTION_ALLO
!
!==========================================================================|
!
!==========================================================================|
!
  SUBROUTINE ACTION_DEALLO
   
  IMPLICIT NONE
   
  DEALLOCATE(N31)
  DEALLOCATE(N32)
   
  RETURN
  END SUBROUTINE ACTION_DEALLO
!
!==========================================================================|
!
!==========================================================================|
!
  SUBROUTINE ALGORITHM_FCT(CAS,IG,DTW,IDCMIN,IDCMAX)

  USE SWCOMM3, ONLY : MDC,MSC
  USE M_GENARR, ONLY : SPCSIG
!  USE MOD_USGRID, ONLY : SPCSIG,MDC,MSC
  USE VARS_WAVE, ONLY : MT,AC2
!  USE ALL_VARS, ONLY : MT,AC2
# if defined(MULTIPROCESSOR)
  USE MOD_PAR
# endif
  IMPLICIT NONE

  INTEGER :: ISS,ID,IG
  REAL :: CASR,CASL,FLUX1,FLUX2,FLUXLP,FLUXLM,FLUXHP,FLUXHM
  REAL :: MIN11,MIN22,MIN33,ADLP,ADLM
  REAL, DIMENSION(MDC,MSC) :: ADP,ADM,NL
  REAL :: CAS(MDC,MSC,10)
  REAL :: DTW,DS
   
  INTEGER, DIMENSION(MSC) :: IDCMIN,IDCMAX
   
  N31 = 0.0
   
  DO ISS = 1,MSC
    IF(ISS == 1)THEN
      DS   = SPCSIG(ISS+1) - SPCSIG(ISS)                              
      DO ID = 1,MDC
        CASR = 0.5*(CAS(ID,ISS,1)+CAS(ID,ISS+1,1))
        FLUX1 = 0.5*(CASR+ABS(CASR))*AC2(ID,ISS,IG)
        FLUX2 = 0.5*(CASR-ABS(CASR))*AC2(ID,ISS+1,IG)
        FLUXLP = FLUX1+FLUX2

        FLUXLM = 0.0
         
        FLUXHP = CASR*0.5*(AC2(ID,ISS,IG)+AC2(ID,ISS+1,IG))
        FLUXHM = 0.0
   
        NL(ID,ISS) = AC2(ID,ISS,IG)-(FLUXLP-FLUXLM)*DTW/DS

        ADP(ID,ISS) = FLUXHP-FLUXLP
        ADM(ID,ISS) = FLUXHM-FLUXLM
      END DO	 
    ELSE IF(ISS == MSC)THEN
      DS   = SPCSIG(ISS) - SPCSIG(ISS-1)                              
      DO ID = 1,MDC
        CASR = CAS(ID,ISS,1)
        FLUX1 = 0.5*(CASR+ABS(CASR))*AC2(ID,ISS,IG)
        FLUX2 = 0.0
        FLUXLP = FLUX1+FLUX2

        CASL = CAS(ID,ISS-1,1)
        FLUX1 = 0.5*(CASL+ABS(CASL))*AC2(ID,ISS-1,IG)
        FLUX2 = 0.5*(CASL-ABS(CASL))*AC2(ID,ISS,IG)
        FLUXLM = FLUX1+FLUX2
         
        FLUXHP = CASR*AC2(ID,ISS,IG)
        FLUXHM = CASL*AC2(ID,ISS-1,IG)
   
        NL(ID,ISS) = AC2(ID,ISS,IG)-(FLUXLP-FLUXLM)*DTW/DS

        ADP(ID,ISS) = FLUXHP-FLUXLP
        ADM(ID,ISS) = FLUXHM-FLUXLM
      END DO	 
    ELSE
      DS   = SPCSIG(ISS) - SPCSIG(ISS-1)                              
      DO ID = 1,MDC
        CASR = 0.5*(CAS(ID,ISS,1)+CAS(ID,ISS+1,1))
        FLUX1 = 0.5*(CASR+ABS(CASR))*AC2(ID,ISS,IG)
        FLUX2 = 0.5*(CASR-ABS(CASR))*AC2(ID,ISS+1,IG)
        FLUXLP = FLUX1+FLUX2

        CASL = 0.5*(CAS(ID,ISS,1)+CAS(ID,ISS-1,1))
	FLUX1 = 0.5*(CASL+ABS(CASL))*AC2(ID,ISS-1,IG)
        FLUX2 = 0.5*(CASL-ABS(CASL))*AC2(ID,ISS,IG)
        FLUXLM = FLUX1+FLUX2
         
        FLUXHP = CASR*0.5*(AC2(ID,ISS,IG)+AC2(ID,ISS+1,IG))
        FLUXHM = CASL*0.5*(AC2(ID,ISS,IG)+AC2(ID,ISS-1,IG))
   
        NL(ID,ISS) = AC2(ID,ISS,IG)-(FLUXLP-FLUXLM)*DTW/DS

        ADP(ID,ISS) = FLUXHP-FLUXLP
        ADM(ID,ISS) = FLUXHM-FLUXLM
      END DO	 
    END IF
  END DO
   
  DO ISS = 1,MSC
    IF(ISS == 1)THEN
      DS = SPCSIG(ISS+1) - SPCSIG(ISS)                              
      DO ID = 1,MDC
        MIN11 = ABS(ADP(ID,ISS))
        MIN22 = SIGN(1.,ADP(ID,ISS))*(NL(ID,ISS+2)-NL(ID,ISS+1))*DS/DTW
        ADLP = MIN(MIN11,MIN22)
        ADLP = MAX(0.,ADLP)
        ADLP = SIGN(1.,ADP(ID,ISS))*ADLP

        MIN11 = ABS(ADM(ID,ISS))
        MIN22 = SIGN(1.,ADM(ID,ISS))*(NL(ID,ISS+1)-NL(ID,ISS))*DS/DTW
        ADLM = MIN(MIN11,MIN22)
        ADLM = MAX(0.,ADLM)
        ADLM = SIGN(1.,ADM(ID,ISS))*ADLM

        N31(ID,ISS) = NL(ID,ISS)-(ADLP-ADLM)*DTW/DS
      END DO
    ELSE IF(ISS == 2)THEN
      DS   = SPCSIG(ISS) - SPCSIG(ISS-1)                              
      DO ID = 1,MDC
        MIN11 = ABS(ADP(ID,ISS))
        MIN22 = SIGN(1.,ADP(ID,ISS))*(NL(ID,ISS+2)-NL(ID,ISS+1))*DS/DTW
        MIN33 = SIGN(1.,ADP(ID,ISS))*(NL(ID,ISS)-NL(ID,ISS-1))*DS/DTW
        ADLP = MIN(MIN11,MIN22,MIN33)
        ADLP = MAX(0.,ADLP)
        ADLP = SIGN(1.,ADP(ID,ISS))*ADLP

        MIN11 = ABS(ADM(ID,ISS))
        MIN22 = SIGN(1.,ADM(ID,ISS))*(NL(ID,ISS+1)-NL(ID,ISS))*DS/DTW
        ADLM = MIN(MIN11,MIN22)
        ADLM = MAX(0.,ADLM)
        ADLM = SIGN(1.,ADM(ID,ISS))*ADLM
    
        N31(ID,ISS) = NL(ID,ISS)-(ADLP-ADLM)*DTW/DS
      END DO
    ELSE IF(ISS == MSC-1)THEN   
      DS   = SPCSIG(ISS) - SPCSIG(ISS-1)                              
      DO ID = 1,MDC
        MIN11 = ABS(ADP(ID,ISS))
        MIN33 = SIGN(1.,ADP(ID,ISS))*(NL(ID,ISS)-NL(ID,ISS-1))*DS/DTW
        ADLP = MIN(MIN11,MIN33)
        ADLP = MAX(0.,ADLP)
        ADLP = SIGN(1.,ADP(ID,ISS))*ADLP

        MIN11 = ABS(ADM(ID,ISS))
        MIN22 = SIGN(1.,ADM(ID,ISS))*(NL(ID,ISS+1)-NL(ID,ISS))*DS/DTW
        MIN33 = SIGN(1.,ADM(ID,ISS))*(NL(ID,ISS-1)-NL(ID,ISS-2))*DS/DTW
        ADLM = MIN(MIN11,MIN22,MIN33)
        ADLM = MAX(0.,ADLM)
        ADLM = SIGN(1.,ADM(ID,ISS))*ADLM
    
        N31(ID,ISS) = NL(ID,ISS)-(ADLP-ADLM)*DTW/DS
      END DO
    ELSE IF(ISS == MSC)THEN
      DS   = SPCSIG(ISS) - SPCSIG(ISS-1)                              
      DO ID = 1,MDC
        MIN11 = ABS(ADP(ID,ISS))
        MIN33 = SIGN(1.,ADP(ID,ISS))*(NL(ID,ISS)-NL(ID,ISS-1))*DS/DTW
        ADLP = MIN(MIN11,MIN33)
        ADLP = MAX(0.,ADLP)
        ADLP = SIGN(1.,ADP(ID,ISS))*ADLP

        MIN11 = ABS(ADM(ID,ISS))
        MIN33 = SIGN(1.,ADM(ID,ISS))*(NL(ID,ISS-1)-NL(ID,ISS-2))*DS/DTW
        ADLM = MIN(MIN11,MIN33)
        ADLM = MAX(0.,ADLM)
        ADLM = SIGN(1.,ADM(ID,ISS))*ADLM
    
        N31(ID,ISS) = NL(ID,ISS)-(ADLP-ADLM)*DTW/DS
      END DO
    ELSE    
      DS   = SPCSIG(ISS) - SPCSIG(ISS-1)                              
      DO ID = 1,MDC
        MIN11 = ABS(ADP(ID,ISS))
        MIN22 = SIGN(1.,ADP(ID,ISS))*(NL(ID,ISS+2)-NL(ID,ISS+1))*DS/DTW
        MIN33 = SIGN(1.,ADP(ID,ISS))*(NL(ID,ISS)-NL(ID,ISS-1))*DS/DTW
        ADLP = MIN(MIN11,MIN22,MIN33)
        ADLP = MAX(0.,ADLP)
        ADLP = SIGN(1.,ADP(ID,ISS))*ADLP

        MIN11 = ABS(ADM(ID,ISS))
        MIN22 = SIGN(1.,ADM(ID,ISS))*(NL(ID,ISS+1)-NL(ID,ISS))*DS/DTW
        MIN33 = SIGN(1.,ADM(ID,ISS))*(NL(ID,ISS-1)-NL(ID,ISS-2))*DS/DTW
        ADLM = MIN(MIN11,MIN22,MIN33)
        ADLM = MAX(0.,ADLM)
        ADLM = SIGN(1.,ADM(ID,ISS))*ADLM
    
        N31(ID,ISS) = NL(ID,ISS)-(ADLP-ADLM)*DTW/DS
      END DO
    END IF  
  END DO    

  RETURN
  END SUBROUTINE ALGORITHM_FCT
!============================================================================|
!============================================================================|
  SUBROUTINE ALGORITHM_CRANK_NICOLSON(CAD,IG,DTW,IDCMIN,IDCMAX,DD)

  USE SWCOMM3, ONLY : MDC,MSC
!  USE MOD_USGRID, ONLY : MDC,MSC
  IMPLICIT NONE
  INTEGER :: ISS,ID,IDM1,IDM2,MDCM,IG,II
  INTEGER :: IDDUM
  INTEGER, DIMENSION(MSC) :: IDCMIN,IDCMAX
  REAL, PARAMETER :: ZETA = 0.5
  REAL :: CAD(:,:,:)
  REAL :: DTW,DD
  REAL :: N32M,N32P,IDP1
   
  REAL,DIMENSION(MDC) :: A,B,C,R,U
   
  DO ISS = 1,MSC
    DO IDDUM = IDCMIN(ISS),IDCMAX(ISS)
      ID = MOD(IDDUM-1+MDC,MDC)+1
      IDP1 = MOD(IDDUM+MDC,MDC)+1
      IDM1 = MOD(IDDUM-2+MDC,MDC)+1
 
      B(ID) = 1.0
      IF(ID == 1)THEN
        A(ID) = 0.0
      ELSE
        A(ID) = -0.5*ZETA*DTW*CAD(IDM1,ISS,1)/DD
      END IF
      IF(ID == MDC)THEN
        C(ID) = 0.0
      ELSE
        C(ID) = 0.5*ZETA*DTW*CAD(IDP1,ISS,1)/DD
      END IF
 
      IF(ID == 1)THEN
        R(ID) = CAD(IDP1,ISS,1)*N31(IDP1,ISS) 
        R(ID) = (1.0-ZETA)*0.5*DTW*R(ID)/DD
        R(ID) = N31(ID,ISS)-R(ID)
      ELSE IF(ID == MDC)THEN
        R(ID) = -CAD(IDM1,ISS,1)*N31(IDM1,ISS)
        R(ID) = (1.0-ZETA)*0.5*DTW*R(ID)/DD
        R(ID) = N31(ID,ISS)-R(ID)       
      ELSE
        R(ID) = CAD(IDP1,ISS,1)*N31(IDP1,ISS)-CAD(IDM1,ISS,1)*N31(IDM1,ISS) 
        R(ID) = (1.0-ZETA)*0.5*DTW*R(ID)/DD
        R(ID) = N31(ID,ISS)-R(ID)     
      END IF

    END DO

    CALL TRIDAG(A,B,C,R,U,MDC)
       
    DO IDDUM = IDCMIN(ISS),IDCMAX(ISS)
      ID = MOD(IDDUM-1+MDC,MDC)+1
      N32(ID,ISS,IG) = U(ID)
    END DO	 
  END DO
   
  RETURN
  END SUBROUTINE ALGORITHM_CRANK_NICOLSON
!==========================================================================|
!
!==================================================================================!
  SUBROUTINE TRIDAG(A,B,C,R,U,N)
  IMPLICIT NONE
  INTEGER  :: N,J
  REAL,DIMENSION(N) :: A,B,C,R,U
  INTEGER, PARAMETER :: NMAX = 500
  REAL BET,GAM(NMAX)
    
  IF(B(1) == 0.)PAUSE 'TRIDAG: REWRITE EQUATIONS'
  BET = B(1)
  U(1) = R(1)/BET
  DO J=2,N
    GAM(J) = C(J-1)/BET
    BET = B(J)-A(J)*GAM(J)
    IF(BET == 0.)PAUSE 'TRIDAG FAILED'
    U(J) = (R(J)-A(J)*U(J-1))/BET
  END DO
  DO J=N-1,1,-1
    U(J) = U(J)-GAM(J+1)*U(J+1)
  END DO
    
  RETURN
  END SUBROUTINE TRIDAG  
!==========================================================================|
!
!==========================================================================|

  SUBROUTINE ADV_N(DTW)                   

!------------------------------------------------------------------------------|

  USE VARS_WAVE
!  USE ALL_VARS
# if defined(MULTIPROCESSOR)
  USE MOD_PAR
# endif
# if defined (SPHERICAL)   
  USE MOD_SPHERICAL
# endif   
  USE SWCOMM3
  USE M_GENARR
# if defined(PLBC)
  USE MOD_PERIODIC_LBC
# endif

!  USE MOD_USGRID

  IMPLICIT NONE
    
  REAL(SP), DIMENSION(0:MT) :: XFLUX
  REAL, DIMENSION(M)   :: PSPX,PSPY
  REAL :: UTMP,VTMP,FF1,X11,Y11,X22,Y22,X33,Y33,TMP1,TMP2,XI,YI
  REAL :: DXA,DYA,DXB,DYB,FIJ1,FIJ2,UN
  REAL :: TXX,TYY,FXX,FYY,VISCOF,EXFLUX,TEMP,STPOINT
  REAL :: FACT,FM1
  INTEGER  :: I,I1,I2,IA,IB,J,J1,J2,K,JTMP,JJ,II
  REAL :: AC1MIN, AC1MAX, AC2MIN, AC2MAX
  INTEGER  :: ID,ISS,IG,IG2 ! LWU IG2 IS FOR PLBC
  REAL :: XIN,YIN,XIC,YIC,CANX,CANY
  REAL :: CAX(MDC,MSC),CAY(MDC,MSC)
  REAL :: DTW,RF(MDC,MSC),DF(MDC,MSC)
  INTEGER :: NTSN_T
  REAL    :: N32_T,XFLUX_T
!  REAL    :: DEP2(MT),AC2LOC(0:MT)   
  REAL(SP),ALLOCATABLE:: DEP2(:),AC2LOC(:)   
  REAL    :: DEPLOC,KWAVELOC,CGLOC,NN,ND,SPCSIGL
# if defined (SPHERICAL)
!!$  REAL(8) :: TY,TXPI,TYPI
  REAL(8) :: XTMP1,XTMP
!!$  REAL(8) :: X1_DP,Y1_DP,X2_DP,Y2_DP,XII,YII
!!$  REAL(8) :: X11_TMP,Y11_TMP,X33_TMP,Y33_TMP,DLTXE_TMP,DLTYE_TMP
  REAL(8) :: DLTXE_TMP,DLTYE_TMP
# endif
  REAL(SP) :: UA_NODE,VA_NODE
  INTEGER  :: CNT
   
!------------------------------------------------------------------------------!
  ALLOCATE(DEP2(MT),AC2LOC(0:MT))  

  DEP2(1:MT) = COMPDA(1:MT,JDP2)
# if defined(MULTIPROCESSOR)
!  IF(PAR) CALL EXCHANGE(NC,MT,1,MYID,NPROCS,DEP2)
  IF(PAR)CALL aexchange(nc,myid,nprocs,dep2)
# endif  

  CAX = 0.0
  CAY = 0.0
    
  DO ISS = 1,MSC
    DO ID = 1,MDC
!
!--Initialize Fluxes-----------------------------------------------------------!
!
      XFLUX = 0.0
      PSPX  = 0.0 
      PSPY  = 0.0 
# if defined(PLBC)
   call replace_N32(N32,ID,ISS)
# endif
!
!--Calculate the Advection and Horizontal Diffusion Terms----------------------!
!
      DO I = 1,M
        DO J=1,NTSN(I)-1
          I1=NBSN(I,J)
          I2=NBSN(I,J+1)
!!$          IF(DEP2(I1) <= DEPMIN .AND. DEP2(I2) > DEPMIN)THEN
!!$           FF1=0.5*(N32(ID,ISS,I)+N32(ID,ISS,I2))
!!$          ELSE IF(DEP2(I1) > DEPMIN .AND. DEP2(I2) <= DEPMIN)THEN
!!$           FF1=0.5*(N32(ID,ISS,I1)+N32(ID,ISS,I))
!!$          ELSE IF(DEP2(I1) <= DEPMIN .AND. DEP2(I2) <= DEPMIN)THEN
!!$           FF1=N32(ID,ISS,I)
!!$          ELSE
!!$           FF1=0.5*(N32(ID,ISS,I1)+N32(ID,ISS,I2))
!!$          END IF
          FF1=0.5*(N32(ID,ISS,I1)+N32(ID,ISS,I2))
!!$#         if defined (SPHERICAL)
!!$          XTMP  = VX(I2)*TPI-VX(I1)*TPI
!!$	  XTMP1 = VX(I2)-VX(I1)
!!$	  IF(XTMP1 >  180.0)THEN
!!$	    XTMP = -360.0*TPI+XTMP
!!$	  ELSE IF(XTMP1 < -180.0)THEN
!!$	    XTMP =  360.0*TPI+XTMP
!!$	  END IF  
!!$          TXPI=XTMP*COS(DEG2RAD*VY(I))
!!$          TYPI=(VY(I1)-VY(I2))*TPI
!!$          PSPX(I)=PSPX(I)+FF1*TYPI
!!$          PSPY(I)=PSPY(I)+FF1*TXPI
!!$#         else
!!$          PSPX(I)=PSPX(I)+FF1*(VY(I1)-VY(I2))
!!$          PSPY(I)=PSPY(I)+FF1*(VX(I2)-VX(I1))
!!$#         endif
          PSPX(I)=PSPX(I)+FF1*DLTYTRIE(I,J)
          PSPY(I)=PSPY(I)+FF1*DLTXTRIE(I,J)
        END DO
        PSPX(I)=PSPX(I)/ART2(I)
        PSPY(I)=PSPY(I)/ART2(I)
      END DO  
!lwu
# if defined (PLBC)
  PSPY=0.0_SP
# endif
      DO I=1,NCV_I
        I1 = NTRG(I)
        IA = NIEC(I,1)
	IB = NIEC(I,2)
!!$	XI = 0.5*(XIJE(I,1)+XIJE(I,2))
!!$	YI = 0.5*(YIJE(I,1)+YIJE(I,2))
!!$	 
!!$#       if defined (SPHERICAL)
!!$        X1_DP=XIJE(I,1)
!!$        Y1_DP=YIJE(I,1)
!!$        X2_DP=XIJE(I,2)
!!$        Y2_DP=YIJE(I,2)
!!$        XII = XCG2(I)
!!$        YII = YCG2(I)
!!$        XI=XII		
!!$        XTMP  = XI*TPI-VX(IA)*TPI
!!$        XTMP1 = XI-VX(IA)
!!$        IF(XTMP1 >  180.0)THEN
!!$          XTMP = -360.0*TPI+XTMP
!!$        ELSE IF(XTMP1 < -180.0)THEN
!!$          XTMP =  360.0*TPI+XTMP
!!$        END IF	 
!!$
!!$        DXA=XTMP*VAL_COS_VY(IA)    
!!$        DYA=(YI-VY(IA))*TPI
!!$        XTMP  = XI*TPI-VX(IB)*TPI
!!$        XTMP1 = XI-VX(IB)
!!$        IF(XTMP1 >  180.0)THEN
!!$          XTMP = -360.0*TPI+XTMP
!!$        ELSE IF(XTMP1 < -180.0)THEN
!!$          XTMP =  360.0*TPI+XTMP
!!$        END IF	 
!!$
!!$        DXB=XTMP*VAL_COS_VY(IB) 
!!$        DYB=(YI-VY(IB))*TPI
!!$#       else
!!$        DXA = XI - VX(IA)
!!$        DYA = YI - VY(IA)
!!$	DXB = XI - VX(IB)
!!$	DYB = YI - VY(IB)
!!$#       endif
!!$	 
!!$        FIJ1=N32(ID,ISS,IA)+DXA*PSPX(IA)+DYA*PSPY(IA)
!!$        FIJ2=N32(ID,ISS,IB)+DXB*PSPX(IB)+DYB*PSPY(IB)

! DEVELOPMENT TESTING - FIRST ORDER WAVE ACTION ADVECTION IN GEOG. SPACE
# if defined (DEVELOP1)
        FIJ1=N32(ID,ISS,IA)
        FIJ2=N32(ID,ISS,IB)
# else
        FIJ1=N32(ID,ISS,IA)+DLTXNCVE(I,1)*PSPX(IA)+DLTYNCVE(I,1)*PSPY(IA)
        FIJ2=N32(ID,ISS,IB)+DLTXNCVE(I,2)*PSPX(IB)+DLTYNCVE(I,2)*PSPY(IB)
# endif
	 
        AC1MIN=MINVAL(N32(ID,ISS,NBSN(IA,1:NTSN(IA)-1)))
        AC1MIN=MIN(AC1MIN, N32(ID,ISS,IA))
        AC1MAX=MAXVAL(N32(ID,ISS,NBSN(IA,1:NTSN(IA)-1)))
        AC1MAX=MAX(AC1MAX, N32(ID,ISS,IA))
        AC2MIN=MINVAL(N32(ID,ISS,NBSN(IB,1:NTSN(IB)-1)))
        AC2MIN=MIN(AC2MIN, N32(ID,ISS,IB))
        AC2MAX=MAXVAL(N32(ID,ISS,NBSN(IB,1:NTSN(IB)-1)))
        AC2MAX=MAX(AC2MAX, N32(ID,ISS,IB))
        IF(FIJ1 < AC1MIN) FIJ1 = AC1MIN
        IF(FIJ1 > AC1MAX) FIJ1 = AC1MAX
        IF(FIJ2 < AC2MIN) FIJ2 = AC2MIN
        IF(FIJ2 > AC2MAX) FIJ2 = AC2MAX

        CALL SWAPAR1(I1,ISS,ID,DEP2(1),KWAVELOC,CGLOC)
	  
#       if !defined (TWO_D_MODEL)
        UTMP = (COMPDA(NV(I1,1),JVX2)+COMPDA(NV(I1,2),JVX2)+        &
	        COMPDA(NV(I1,3),JVX2))/3.0
        VTMP = (COMPDA(NV(I1,1),JVY2)+COMPDA(NV(I1,2),JVY2)+        &
	        COMPDA(NV(I1,3),JVY2))/3.0
#       else
        UTMP=UA(I1)
        VTMP=VA(I1)
#       endif	
	  
        CALL SPROXY(I1     ,ISS         ,ID          ,CANX ,CANY ,   &
	            CGLOC  ,SPCDIR(ID,2),SPCDIR(ID,3),UTMP ,VTMP )
!lwu for no y gradient
        UN = CANY*DLTXE(I) - CANX*DLTYE(I)  
# if defined (PLBC)
        UN =  - CANX*DLTYE(I)
# endif
        EXFLUX=-UN*((1.0+SIGN(1.0,UN))*FIJ2+(1.0-SIGN(1.0,UN))*FIJ1)*0.5
	 
        XFLUX(IA)=XFLUX(IA)+EXFLUX
        XFLUX(IB)=XFLUX(IB)-EXFLUX
      END DO 
	 
      DO I = 1,M
        IF(ISONB_W(I) /= 0)THEN
!#          if !defined(PLBC)
           DEPLOC = DEP2(I)  
!#          else        
!           CALL replace_node_wave(I,IG2)
!           !print*,'I,IG2===',I,IG2
!           DEPLOC = DEP2(IG2)
!#          endif           
          IF(DEPLOC <= DEPMIN)THEN
!         *** depth is negative ***
            KWAVELOC = -1.                                           
            CGLOC   = 0.                                            
          ELSE
!         *** call KSCIP1 to compute KWAVE and CGO ***
            SPCSIGL = SPCSIG(ISS)
            CALL KSCIP1(1,SPCSIGL,DEPLOC,KWAVELOC,CGLOC,NN,ND)                                 
          ENDIF
          CAX(ID,ISS) = CGLOC * SPCDIR(ID,2)
          CAY(ID,ISS) = CGLOC * SPCDIR(ID,3)
!
!         --- adapt the velocities in case of diffraction
!
          IF(IDIFFR == 1 .AND. PDIFFR(3) /= 0.)THEN 
!JQI            CAX(ID,ISS) = CAX(ID,ISS)*DIFPARAM(I)      
!JQI            CAY(ID,ISS) = CAY(ID,ISS)*DIFPARAM(I)      
          END IF
!
!         --- ambient currents added
!
          IF(ICUR == 1)THEN 
#         if !defined (TWO_D_MODEL)
            CAX(ID,ISS) = CAX(ID,ISS) + COMPDA(I,JVX2)
            CAY(ID,ISS) = CAY(ID,ISS) + COMPDA(I,JVY2)
#         else
            UA_NODE = 0.0_SP
	    VA_NODE = 0.0_SP
	    CNT = 0
            DO JJ=1,NTVE(I)
             CNT =CNT + 1
             UA_NODE = UA_NODE + UA(NBVE(I,JJ))
             VA_NODE = VA_NODE + VA(NBVE(I,JJ))
            ENDDO
            UA_NODE = UA_NODE/CNT
            VA_NODE = VA_NODE/CNT
            CAX(ID,ISS) = CAX(ID,ISS) + UA_NODE
            CAY(ID,ISS) = CAY(ID,ISS) + VA_NODE
#         endif	    
          END IF
	    
          CANX = CAX(ID,ISS)
	  CANY = CAY(ID,ISS)
!          write(100,*)I,ID,ISS,CANX,CANY
	 
	  FIJ1 = N32(ID,ISS,I)
	 
          IF(NBSN(I,NTSN(I)-1) > M)THEN
#           if defined (SPHERICAL)
            XTMP1 = VX(I)-VX(NBSN(I,2))
            XTMP = XTMP1*TPI
	    IF(XTMP1 > 180.0)THEN
	      XTMP = -360.0*TPI+XTMP
            ELSE IF(XTMP1 < -180.0)THEN
	      XTMP =  360.0*TPI+XTMP
	    END IF
	    DLTXE_TMP = XTMP*COS(DEG2RAD*VY(I))
            DLTYE_TMP = (VY(I)-VY(NBSN(I,2)))*TPI
	    UN = CANY*DLTXE_TMP-CANX*DLTYE_TMP
#           else

	    UN = CANY*(VX(I)-VX(NBSN(I,2)))      &
	        -CANX*(VY(I)-VY(NBSN(I,2)))
# if defined (PLBC)
            UN =-CANX*(VY(I)-VY(NBSN(I,2)))
# endif
#           endif
          ELSE IF(NBSN(I,2) > M)THEN
#           if defined (SPHERICAL)
            XTMP1 = VX(NBSN(I,NTSN(I)-1))-VX(I)
            XTMP = XTMP1*TPI
	    IF(XTMP1 > 180.0)THEN
	      XTMP = -360.0*TPI+XTMP
	    ELSE IF(XTMP1 < -180.0)THEN
	      XTMP =  360.0*TPI+XTMP
	    END IF
	    DLTXE_TMP = XTMP*COS(DEG2RAD*VY(I))
            DLTYE_TMP = (VY(NBSN(I,NTSN(I)-1))-VY(I))*TPI
	    UN = CANY*DLTXE_TMP-CANX*DLTYE_TMP
#           else
	    UN = CANY*(VX(NBSN(I,NTSN(I)-1))-VX(I))      &
	        -CANX*(VY(NBSN(I,NTSN(I)-1))-VY(I))
# if defined (PLBC)
            UN = -CANX*(VY(NBSN(I,NTSN(I)-1))-VY(I))
# endif
#           endif
          ELSE
#           if defined (SPHERICAL)
            XTMP1 = VX(NBSN(I,NTSN(I)-1))-VX(NBSN(I,2))
            XTMP = XTMP1*TPI
	    IF(XTMP1 > 180.0)THEN
	      XTMP = -360.0*TPI+XTMP
	    ELSE IF(XTMP1 < -180.0)THEN
	      XTMP =  360.0*TPI+XTMP
	    END IF
	    DLTXE_TMP = XTMP*COS(DEG2RAD*VY(I))
            DLTYE_TMP = (VY(NBSN(I,NTSN(I)-1))-VY(NBSN(I,2)))*TPI
	    UN = CANY*DLTXE_TMP-CANX*DLTYE_TMP
#           else
	    UN = CANY*(VX(NBSN(I,NTSN(I)-1))-VX(NBSN(I,2)))      &
	        -CANX*(VY(NBSN(I,NTSN(I)-1))-VY(NBSN(I,2)))
# if defined (PLBC)
            UN =-CANX*(VY(NBSN(I,NTSN(I)-1))-VY(NBSN(I,2)))
# endif
#           endif
          END IF
	     
	  UN = 0.5*UN
	     
          EXFLUX   = MAX(0.0,-UN*FIJ1)
          XFLUX(I) = XFLUX(I)+EXFLUX
        END IF	 
      END DO  
!
!-Accumulate Fluxes at Boundary Nodes
!
#     if defined (MULTIPROCESSOR)
      IF(PAR)CALL NODE_MATCH(0,NBN,BN_MLT,BN_LOC,BNC,MT,1,MYID,NPROCS,XFLUX)
#     endif

      DO I = 1,M
!--Update Action Density ------------------------------------------------------!
!
       IF(DEP2(I) > DEPMIN)THEN
        AC2(ID,ISS,I) = N32(ID,ISS,I)-XFLUX(I)/ART1(I)*DTW
	AC2(ID,ISS,I) = MAX(0.0,AC2(ID,ISS,I))
       ELSE
        AC2(ID,ISS,I) = 0.0_SP
       END IF	
      END DO

# if defined(PLBC)
       CALL replace_ac2(ID,ISS)
# endif

#     if defined (MULTIPROCESSOR)
      IF(PAR)THEN
        AC2LOC = 0.0 
        DO I = 1,MT
          AC2LOC(I) = AC2(ID,ISS,I)
        END DO
   
        CALL NODE_MATCH(1,NBN,BN_MLT,BN_LOC,BNC,MT,1,MYID,NPROCS,AC2LOC)
!        CALL EXCHANGE(NC,MT,1,MYID,NPROCS,AC2LOC)
        CALL AEXCHANGE(NC,MYID,NPROCS,AC2LOC)

        AC2(ID,ISS,:) = 0.0
        DO I = 1,MT
          AC2(ID,ISS,I) = AC2LOC(I)
        END DO
      END IF
#     endif

    END DO
  END DO
   
  RETURN
  END SUBROUTINE ADV_N
!==============================================================================|

END MODULE MOD_ACTION_EX
