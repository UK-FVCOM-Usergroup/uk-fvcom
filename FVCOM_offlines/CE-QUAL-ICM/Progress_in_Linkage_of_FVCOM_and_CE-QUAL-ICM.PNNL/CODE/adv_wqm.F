!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!=============================================================================!
   SUBROUTINE ADV_WQM  

   USE ALL_VARS
   USE WQM, ONLY : NAC,AC,C2,C1,C2F,DLT,DTM,NCP,XYDF
   USE WQM_INIT, ONLY : XYDFU
# if defined (MULTIPROCESSOR)
   USE MOD_PAR   
# endif
   IMPLICIT NONE
   REAL, DIMENSION(0:MTLOC,KB,NCP)  :: XFLUX,RF
   REAL, DIMENSION(MLOC)           :: PUPX,PUPY,PVPX,PVPY
   REAL, DIMENSION(MLOC)           :: PFPX,PFPY,PFPXD,PFPYD,VISCOFF
   REAL, DIMENSION(3*(NTLOC))      :: DTIJ
   REAL, DIMENSION(3*(NTLOC),KBM1) :: UVN
   REAL :: FFD,FF1,X11,Y11,X22,Y22,X33,Y33,TMP1,TMP2,XI,YI
   REAL :: DXA,DYA,DXB,DYB,FIJ1,FIJ2,UN
   REAL :: TXX,TYY,FXX,FYY,VISCOF,EXFLUX,TEMP
   REAL :: FACT,FM1
   REAL :: TT,TTIME,STPOINT,STPOINT1,STPOINT2
   INTEGER  :: I,I1,I2,IA,IB,J,J1,J2,JTMP,K,JJ,JC,II
   REAL :: C21MIN, C21MAX, C22MIN, C22MAX
   REAL :: C2MEAN(MLOC,KBM1,NCP)
!------------------------------------------------------------------------------
   C2MEAN = 0.0

   FACT = 0.0
   FM1  = 1.0
   IF(XYDFU == ' ON')THEN
     FACT = 1.0
     FM1  = 0.0
   END IF

!
!--Initialize Fluxes-----------------------------------------------------------
!
   XFLUX = 0.0

!
!--Loop Over Control Volume Sub-Edges And Calculate Normal Velocity------------
!
!   Print*, 'UU before UVM in control vol', UU(11126,:)      
   DO I=1,NCV
     I1=NTRG(I)
     DTIJ(I)=DT1(I1)
     DO K=1,KBM1
       UVN(I,K)=VV(I1,K)*DLTXE(I) - UU(I1,K)*DLTYE(I)
!       IF(I.eq.11126) Print*, 'UVN', UVN(I,K)
     END DO
   END DO
!
!--Calculate the Advection and Horizontal Diffusion Terms----------------------
!
   DO JC=1,NAC
     II = AC(JC)
     DO K=1,KBM1
       PFPX  = 0.0
       PFPY  = 0.0
       PFPXD = 0.0
       PFPYD = 0.0

       DO I=1,MLOC
         DO J=1,NTSN(I)-1
           I1=NBSN(I,J)
           I2=NBSN(I,J+1)
           FFD=0.5_SP*(C2(I1,K,II)+C2(I2,K,II)-C2MEAN(I1,K,II)-C2MEAN(I2,K,II))
           FF1=0.5_SP*(C2(I1,K,II)+C2(I2,K,II))

           PFPX(I)=PFPX(I)+FF1*(VY(I1)-VY(I2))
           PFPY(I)=PFPY(I)+FF1*(VX(I2)-VX(I1))
           PFPXD(I)=PFPXD(I)+FFD*(VY(I1)-VY(I2))
           PFPYD(I)=PFPYD(I)+FFD*(VX(I2)-VX(I1))
         END DO
         PFPX(I)=PFPX(I)/ART2(I)
         PFPY(I)=PFPY(I)/ART2(I)
         PFPXD(I)=PFPXD(I)/ART2(I)
         PFPYD(I)=PFPYD(I)/ART2(I)
       END DO

       DO I=1,MLOC
         VISCOFF(I)=VISCOFH(I,K)
       END DO

       DO I=1,NCV_I
         IA=NIEC(I,1)
         IB=NIEC(I,2)
         XI=0.5_SP*(XIJE(I,1)+XIJE(I,2))
         YI=0.5_SP*(YIJE(I,1)+YIJE(I,2))
         DXA=XI-VX(IA)
         DYA=YI-VY(IA)
         DXB=XI-VX(IB)
         DYB=YI-VY(IB)
         FIJ1=C2(IA,K,II)+DXA*PFPX(IA)+DYA*PFPY(IA)
         FIJ2=C2(IB,K,II)+DXB*PFPX(IB)+DYB*PFPY(IB)

         C21MIN=MINVAL(C2(NBSN(IA,1:NTSN(IA)-1),K,II))
         C21MIN=MIN(C21MIN, C2(IA,K,II))
         C21MAX=MAXVAL(C2(NBSN(IA,1:NTSN(IA)-1),K,II))
         C21MAX=MAX(C21MAX, C2(IA,K,II))
         C22MIN=MINVAL(C2(NBSN(IB,1:NTSN(IB)-1),K,II))
         C22MIN=MIN(C22MIN, C2(IB,K,II))
         C22MAX=MAXVAL(C2(NBSN(IB,1:NTSN(IB)-1),K,II))
         C22MAX=MAX(C22MAX, C2(IB,K,II))
         IF(FIJ1 < C21MIN) FIJ1=C21MIN
         IF(FIJ1 > C21MAX) FIJ1=C21MAX
         IF(FIJ2 < C22MIN) FIJ2=C22MIN
         IF(FIJ2 > C22MAX) FIJ2=C22MAX
!RGL above sets to min max -- no mass conservation? 
         UN=UVN(I,K)
  
         VISCOF=XYDF*(FACT*(VISCOFF(IA)+VISCOFF(IB))*0.5 + FM1)

         TXX=0.5*(PFPXD(IA)+PFPXD(IB))*VISCOF
         TYY=0.5*(PFPYD(IA)+PFPYD(IB))*VISCOF

         FXX=-DTIJ(I)*TXX*DLTYE(I)
         FYY= DTIJ(I)*TYY*DLTXE(I)

         EXFLUX=-UN*DTIJ(I)*((1.0+SIGN(1.0,UN))*FIJ2+                &
                 (1.0-SIGN(1.0,UN))*FIJ1)*0.5+FXX+FYY
 
         XFLUX(IA,K,II)=XFLUX(IA,K,II)+EXFLUX
         XFLUX(IB,K,II)=XFLUX(IB,K,II)-EXFLUX
       END DO
     END DO
   END DO
 
!
!-Accumulate Fluxes at Boundary Nodes
!
# if defined (MULTIPROCESSOR)
   IF(PAR)THEN
     DO JC=1,NAC
       II=AC(JC)
       CALL NODE_MATCH(0,NBN,BN_MLT,BN_LOC,BNC,MTLOC,KB,MYID,NPROCS,XFLUX(:,:,II))
     END DO  
   END IF  
# endif


   DO JC=1,NAC
     II = AC(JC)

!
!--Calculate the Vertical Terms------------------------------------------------
!
     DO K=1,KBM1
       DO I=1,MLOC
         IF(K == 1) THEN
           TEMP=-WTS(I,K+1)*(C2(I,K,II)*DZ(K+1)+C2(I,K+1,II)*DZ(K))/(DZ(K)+DZ(K+1))
         ELSE IF(K == KBM1) THEN
           TEMP=WTS(I,K)*(C2(I,K,II)*DZ(K-1)+C2(I,K-1,II)*DZ(K))/(DZ(K)+DZ(K-1))
         ELSE
           TEMP=WTS(I,K)*(C2(I,K,II)*DZ(K-1)+C2(I,K-1,II)*DZ(K))/(DZ(K)+DZ(K-1))-  &
                WTS(I,K+1)*(C2(I,K,II)*DZ(K+1)+C2(I,K+1,II)*DZ(K))/(DZ(K)+DZ(K+1))
         END IF

!
!--Total Fluxes ---------------------------------------------------------------
!
         IF(ISONB(I) == 2) THEN
           XFLUX(I,K,II)=TEMP*ART1(I)/DZ(K)
         ELSE
           XFLUX(I,K,II)=XFLUX(I,K,II)+TEMP*ART1(I)/DZ(K)
         END IF
       END DO
     END DO     
!
!--Update Water Quality Variables--------------------------------
!
     DO I = 1,MLOC
         DO K = 1, KBM1
! below is where concentration DTC/DTM is added to state variables
           C1(I,K,II)=(C2(I,K,II)-XFLUX(I,K,II)/ART1(I)*(DLT/DT(I)))*   &
                         (DT(I)/DTFA(I))+DTM(I,K,II)*DLT

!RGL added prints, below
!          IF(II.eq.9.and.I.eq.11126.and.K.eq.1) THEN
!             Print*, 'change in DOC in adv_wqm', DTM(I,K,II)
!             Print*, 'DOC orig in adv_wqm', C2(I,K,II)
!             Print*, 'new doc in adv_wqm', C1(I,K,II)
!             Print*, 'DT(I)', DT(I)
!          ENDIF
!         if(II.eq.27.and.I.eq.6695.and.K.eq.1) then                
!          write(2252,*)C1(I,K,II),C2(I,K,II),xflux(I,K,II),art1(I),&
!          DLT,DT(I),D(I),DTM(I,K,II),DLT
!          write(2252,*)'tykim',XFLUX(I,K,II)/ART1(I)*(DLT/DT(I)),   &
!                C2(I,K,II)-XFLUX(I,K,II)/ART1(I)*(DLT/DT(I))
!         endif
          
!ORI           C1(I,K,II)=(C2(I,K,II)-XFLUX(I,K,II)/ART1(I)*(DLT/DT(I)))*   &
!                         (DT(I)/DTFA(I))+DTM(I,K,II)*DLT
           C2F(I,K,II) = MAX( C1(I,K,II), 0.0 )

         END DO
     END DO

   END DO

   RETURN
   END SUBROUTINE ADV_WQM
!==============================================================================!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
 
