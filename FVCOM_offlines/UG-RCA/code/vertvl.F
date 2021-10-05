!==============================================================================|
!   CALCULATE THE SIGMA COORDINATE VERTICAL VELOCITY FOR THE 3D MODE (omega)   |
!							                       |
!   DETERMINED FROM EQUATION:						       |
!   									       !
!   d/dt(D) + d/dx(uD) + d/dy(uD) = d/sigma(omega)                             !
!==============================================================================|

      SUBROUTINE VERTVL(BVOLN)         

!------------------------------------------------------------------------------|
      USE ALL_VARS
      USE MOD_RCA

      IMPLICIT NONE 
!      INCLUDE 'RCACM'
      REAL :: XFLUX(NXY,NZ),XFLUX2D(NXY),WBOTTOM(NXY)
      REAL :: DIJ,UIJ,VIJ,UN,EXFLUX,TMP1,DIJ1,UIJ1,VIJ1
      INTEGER  :: I,K,IA,IB,I1 ,J,JJ,J1,J2
      REAL :: BVOLN(0:MT,NZ),DTK,DF(MT)
!------------------------------------------------------------------------------|

!----------------------INITIALIZE FLUX-----------------------------------------!

      XFLUX = 0.0
      XFLUX2D = 0.0

!---------ACCUMULATE FLUX BY LOOPING OVER CONTROL VOLUME HALF EDGES------------!

   DO I=1,NCV
     I1  = NTRG(I)
     IA  = NIEC(I,1)
     IB  = NIEC(I,2)
     DIJ = D1(I1)

     UIJ = UUA(I1)
     VIJ = VVA(I1)
     EXFLUX = DIJ*(-UIJ*DLTYE(I) + VIJ*DLTXE(I))  
     XFLUX2D(IA) = XFLUX2D(IA)-EXFLUX
     XFLUX2D(IB) = XFLUX2D(IB)+EXFLUX
   END DO

!-- ZERO OUT OPEN BOUNDARY FLUX!

   DO I=1,M
    IF(ISONB(I) == 2) THEN  
      XFLUX2D(I) = 0.0
    END IF  
   END DO

!----------PERFORM UPDATE ON ELF-----------------------------------------------!

   DTK = DT*86400.
   DF = D - DTK*XFLUX2D/ART1
   
!----------------------ACCUMULATE FLUX-----------------------------------------!

      DO I=1,NCV
       I1=NTRG(I)
       IA=NIEC(I,1)
       IB=NIEC(I,2)

       DO K=1,NZ
        DIJ=D1(I1)*DZ1(I1,K)
        UIJ=UU(I1,K)
        VIJ=VV(I1,K)
        EXFLUX=DIJ*(-UIJ*DLTYE(I)+VIJ*DLTXE(I))

        XFLUX(IA,K)=XFLUX(IA,K)-EXFLUX
        XFLUX(IB,K)=XFLUX(IB,K)+EXFLUX
       END DO
      END DO

      DO I=1,M
        DO K=1,KBM1
          IF(ISONB(I) == 2) XFLUX(I,K)=0.0  
        ENDDO
      ENDDO
!---IF NO FRESH WATER INFLOW, OMEGA IS ZERO AT FREE SURFACE ---------!

      WTS(:,1)  = 0.0

!--------------------------CALCULATE OMEGA-------------------------------------!

      DO I=1,M
       DO K=1,NZ
        WTS(I,K+1)=WTS(I,K)+XFLUX(I,K)/ART1(I)+DZ(I,K)*(DF(I)-D(I))/(DT*86400.)
!JQI        WTS(I,K+1)=WTS(I,K)+XFLUX(I,K)/ART1(I)+DZ(I,K)*DETA(I)/DT
!JQI        WTS(I,K+1)=WTS(I,K)+XFLUX(I,K)/ART1(I)+DZ(I,K)*DETA(I)/(DT*86400.)
!JQI        XFLUX(I,K) = XFLUX(I,K)-(WTS(I,K+1)-WTS(I,K))*ART1(I)
!JQI	BVOLN(I,K) = BVOL(I,K)-XFLUX(I,K)*DT*86400.
	BVOLN(I,K) = DF(I)*DZ(I,K)*ART1(I)
       END DO
      END DO

!--------------------------ADJUST OMEGA----------------------------------------!
! IMPROVES MASS CONSERVATION

      DO I=1,MT
       IF(ABS(WTS(I,NZ+1)) > 1.0E-8)THEN
!        write(700,*) time, ixy,wts(i,nz+1)
        IF(ISONB(I) /= 2)THEN
!JQI         TMP1=ETA(I)*FLOAT(NZ)-(WTS(I,NZ+1))*DT/DZ(I,1)
         TMP1=(DF(I)-H(I))*FLOAT(NZ)-(WTS(I,NZ+1))*DT*86400./DZ(I,1)
         TMP1=TMP1/FLOAT(NZ)
         DF(I)=TMP1+H(I)
!JQI         D(I)=TMP1+H(I)
	 	 	 
         do k=1,nz
          BVOLN(I,k)=DF(I)*DZ(I,K)*ART1(I)
         end do 
         DO K=2,NZ+1
           WTS(I,K)=WTS(I,K)-FLOAT(K-1)/FLOAT(NZ)*(WTS(I,NZ+1))
         END DO
        END IF
       END IF
      END DO

!JQI      DO I=1,EXY
!JQI       D1(I) = ONE_THIRD*(D(NV(I,1))+D(NV(I,2))+D(NV(I,3)))
!JQI      END DO
!
!----TRANSFER OMEGA TO FACE CENTER---------------------------------------------!
!
!      DO I=1,EXY
!       DO K=1,KB
!        W(I,K) = ONE_THIRD*(WTS(NV(I,1),K)+WTS(NV(I,2),K)+WTS(NV(I,3),K))
!       END DO
!      END DO

      RETURN
      END SUBROUTINE VERTVL
!==============================================================================|
