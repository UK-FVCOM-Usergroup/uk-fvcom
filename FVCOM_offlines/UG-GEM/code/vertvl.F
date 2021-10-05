!==============================================================================|
!   CALCULATE THE SIGMA COORDINATE VERTICAL VELOCITY FOR THE 3D MODE (omega)   |
!							                       |
!   DETERMINED FROM EQUATION:						       |
!   									       !
!   d/dt(D) + d/dx(uD) + d/dy(uD) = d/sigma(omega)                             !
!==============================================================================|

   SUBROUTINE VERTVL         

!------------------------------------------------------------------------------|
   USE ALL_VARS
   USE BCS
   USE MOD_OBCS
#  if defined (SPHERICAL) && (NORTHPOLE)
   USE MOD_NORTHPOLE
#  endif
#if defined (MULTIPROCESSOR)
   USE MOD_PAR   
#endif
 
   IMPLICIT NONE 
   REAL(SP) :: XFLUX(MT,KBM1),XFLUX2D(0:MT),WBOTTOM(MT)
   REAL(SP) :: DIJ,UIJ,VIJ,UN,EXFLUX,TMP1,DIJ1,UIJ1,VIJ1
   INTEGER  :: I,K,IA,IB,I1 ,J,JJ,J1,J2
!------------------------------------------------------------------------------|

!----------------------INITIALIZE FLUX-----------------------------------------!

   XFLUX   = 0.0_SP
   XFLUX2D = 0.0_SP

!---------ACCUMULATE FLUX BY LOOPING OVER CONTROL VOLUME HALF EDGES------------!

   DO I=1,NCV
    I1  = NTRG(I)
    IA  = NIEC(I,1)
    IB  = NIEC(I,2)
    DIJ = D1(I1)

    UIJ = UA(I1)
    VIJ = VA(I1)
    EXFLUX = DIJ*(-UIJ*DLTYE(I) + VIJ*DLTXE(I))  
    XFLUX2D(IA) = XFLUX2D(IA)-EXFLUX
    XFLUX2D(IB) = XFLUX2D(IB)+EXFLUX
   END DO

!--ADD GROUND WATER TERM-------------------------------------------------------!

   IF(IBFW > 0)THEN
     DO I=1,M
       DO J=1,IBFW
         IF(I == NODE_BFW(J))THEN
	   XFLUX2D(I)=XFLUX2D(I)-BFWDIS3(J)         !*ROFVROS*ART1(I)
         END IF
       END DO
     END DO
   END IF
       	    
!--SAVE ACCUMULATED FLUX ON OPEN BOUNDARY NODES AND ZERO OUT OPEN BOUNDARY FLUX!

   IF(IOBCN > 0) THEN  
     DO I=1,IOBCN
       XFLUX_OBCN(I)=XFLUX2D(I_OBC_N(I))
       XFLUX2D(I_OBC_N(I)) = 0.0_SP
     END DO
   END IF

!-- ZERO OUT OPEN BOUNDARY FLUX!

!   DO I=1,M
!    IF(ISONB(I) == 2) THEN  
!      XFLUX2D(I) = 0.0
!    END IF  
!   END DO

!----------PERFORM UPDATE ON ELF-----------------------------------------------!

   DF  = D - DTI*XFLUX2D/ART1
   ELF = DF - H
   
!----------------------ACCUMULATE FLUX-----------------------------------------!

   DO I=1,NCV
    I1=NTRG(I)
    IA=NIEC(I,1)
    IB=NIEC(I,2)

    DO K=1,KBM1
     DIJ=D1(I1)*DZ1(I1,K)
     UIJ=U(I1,K)
     VIJ=V(I1,K)
     EXFLUX=DIJ*(-UIJ*DLTYE(I)+VIJ*DLTXE(I))
     XFLUX(IA,K)=XFLUX(IA,K)-EXFLUX
     XFLUX(IB,K)=XFLUX(IB,K)+EXFLUX
    END DO
   END DO

#  if defined (SPHERICAL) && (NORTHPOLE)
   CALL VERTVL_EDGE_XY(XFLUX,0.0_SP)
#  endif
   
!-----------------------NULLIFY BOUNDARY FLUX----------------------------------!
   DO I=1,M
    DO K=1,KBM1
     IF(ISONB(I) == 2) XFLUX(I,K)=0.0_SP  
    END DO
   END DO

#  if defined (ONE_D_MODEL)
   XFLUX = 0.0_SP
#  endif    

!---IF NO FRESH WATER INFLOW, OMEGA IS ZERO AT FREE SURFACE AND BOTTOM---------!

   WBOTTOM   = 0.0_SP
   WTS(:,KB) = 0.0_SP
   WTS = 0.0_SP

   IF(IBFW > 0)THEN
    DO I=1,M
     DO J=1,IBFW
      IF(I == NODE_BFW(J))THEN
       WBOTTOM(I)= BFWDIS3(J)/ART1(I)
      END IF
     END DO
    END DO 
   END IF    	   

!--------------------------CALCULATE OMEGA-------------------------------------!

   DO I=1,M
    DO K=1,KBM1
!     WTS(I,K+1)=WTS(I,K)+DZ(I,K)*(XFLUX(I,K)/ART1(I)+(EL(I)-ET(I))/DTI)
     WTS(I,K+1)=WTS(I,K)+XFLUX(I,K)/ART1(I)+DZ(I,K)*(DF(I)-D(I))/DTI
    END DO
   END DO

!--------------------------ADJUST OMEGA----------------------------------------!
! IMPROVES MASS CONSERVATION
   DO I=1,M
    IF(ABS(WTS(I,KB)-WBOTTOM(I)) > 1.0E-8_SP)THEN
     IF(ISONB(I) /= 2)THEN
      TMP1=ELF(I)*FLOAT(KBM1)-(WTS(I,KB)-WBOTTOM(I))*DTI/DZ(I,1)
      TMP1=TMP1/FLOAT(KBM1)
      DF(I)=TMP1+H(I)
      DO K=2,KB
       WTS(I,K)=WTS(I,K)-FLOAT(K-1)/FLOAT(KBM1)*(WTS(I,KB)-WBOTTOM(I))
      END DO
     END IF
    END IF
   END DO

#  if defined (MULTIPROCESSOR)
   IF(PAR) CALL EXCHANGE(NC,MT,KB,MYID,NPROCS,WTS)
#  endif

   RETURN
   END SUBROUTINE VERTVL
!==============================================================================|
