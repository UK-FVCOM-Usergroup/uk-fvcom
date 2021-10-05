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
   USE WQM, ONLY : DLT

   IMPLICIT NONE 
   REAL(SP) :: XFLUX(MTLOC,KBM1)
   REAL(SP) :: DIJ,UIJ,VIJ,UN,EXFLUX,TMP1
   INTEGER  :: I,K,IA,IB,I1 ,J,JJ,J1,J2
!------------------------------------------------------------------------------|

!----------------------INITIALIZE FLUX-----------------------------------------!

   XFLUX = 0.0_SP

!----------------------ACCUMULATE FLUX-----------------------------------------!

   DO I=1,NCV
     I1=NTRG(I)
     IA=NIEC(I,1)
     IB=NIEC(I,2)
     DIJ=DT1(I1)
     DO K=1,KBM1
       UIJ=UU(I1,K)
       VIJ=VV(I1,K)
       EXFLUX=DIJ*(-UIJ*DLTYE(I)+VIJ*DLTXE(I))
       XFLUX(IA,K)=XFLUX(IA,K)-EXFLUX
       XFLUX(IB,K)=XFLUX(IB,K)+EXFLUX
     END DO
   END DO

!-----------------------NULLIFY BOUNDARY FLUX----------------------------------!

      DO I=1,MLOC
        DO K=1,KBM1
          IF(ISONB(I) == 2) XFLUX(I,K)=0.0_SP  
        ENDDO
      ENDDO

!---IF NO FRESH WATER INFLOW, OMEGA IS ZERO AT FREE SURFACE AND BOTTOM---------!

   DO I=1,MLOC
     WTS(I,1)=0.0_SP
     WTS(I,KB)=0.0_SP
   ENDDO


!--------------------------CALCULATE OMEGA-------------------------------------!

   DO I=1,MLOC
     DO K=1,KBM1
       WTS(I,K+1)=WTS(I,K)+DZ(K)*(XFLUX(I,K)/ART1(I)+(EL(I)-ET(I))/DLT)
     END DO
   END DO

!--------------------------ADJUST OMEGA----------------------------------------!
! IMPROVES MASS CONSERVATION

   DO I=1,MLOC
     IF(ABS(WTS(I,KB)) > 1.0E-8_SP)THEN
       IF(ISONB(I) /= 2)THEN
         TMP1=EL(I)*FLOAT(KBM1)-WTS(I,KB)*DLT/DZ(1)
         TMP1=TMP1/FLOAT(KBM1)
         DTFA(I)=TMP1+H(I)
         DO K=2,KB
           WTS(I,K)=WTS(I,K)-FLOAT(K-1)/FLOAT(KBM1)*WTS(I,KB)
         END DO
       END IF	 
     END IF
   END DO

   RETURN
   END SUBROUTINE VERTVL
!==============================================================================|
