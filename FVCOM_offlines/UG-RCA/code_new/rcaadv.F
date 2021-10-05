      MODULE ALLOCRCAADV
       IMPLICIT NONE
       SAVE
       
       REAL, ALLOCATABLE :: FLXMB(:,:,:)
       REAL, ALLOCATABLE :: FLYMB(:,:,:)
      END MODULE ALLOCRCAADV

      SUBROUTINE RCAADV
      USE ALLOCRCAADV
      USE ALL_VARS
      USE MOD_RCA
      USE ALLOCEUTRO,ONLY : NCRB1,NCRB2,NCRB3
#  if defined (MULTIPROCESSOR)
      USE MOD_PAR
#  endif
!
!        RCAADV  IS THE MASTER ROUTINE USED TO INTEGRATE THE DIFFERENTIAL
!                EQUATIONS OF THE WATER QUALITY MODEL.
!                TIME DERIVATIVE: EXPLICIT - 1ST ORDER EULER
!                SPACE DERIVATIVE: UPWIND WITH SECOND ORDER OF FINITE VOLUME
!                SCHEME IN FVCOM
!
!************************************************************************
!
      IMPLICIT NONE
      SAVE
!      INCLUDE 'RCACM' 
      INTEGER  NEGSEG(99,3)
       REAL T_NO3   !Tian Temporal     
!      REAL   CONCS(0:MT,0:NZ),CSLAVE(0:MT,NZ,NOSYS)              &
      REAL   CONCS(0:MT,NZ+1),CSLAVE(0:MT,NZ,NOSYS)              &       
            ,DM(0:MT,NZ,NOSYS),BVOLN(0:MT,NZ)                    &
!JQI            ,DXBAR1(MT),DYBAR1(MT)                           &
!JQI            ,DXBAR2(MT),DYBAR2(MT)                           &
            ,U(NT,NZ),V(NT,NZ)                               &
!JQI     .      ,D(NXY),U(NXY,NZ),V(NXY,NZ)
!JQI            ,W(MT,NZ)                                        &
!JQI            ,FLUXIN(MT,NZ),FLUXOUT(MT,NZ)                    &
            ,QXFLX(MT,NZ)                                    &
            ,QYFLX(MT,NZ)                                    &
            ,QZFLX(MT,NZ)                                    &
            ,RXFLX(MT,NZ),RYFLX(MT,NZ)                       &
!JQI            ,AL(MT,NZ),AD(MT,NZ),AU(MT,NZ)                   &
!JQI            ,BETA(MT,NZ),GAMMA(MT,NZ)                        &
!JQI            ,MF(MT,NZ)                                       &
            ,SYSMASS(NOSYS+NOKINSYS),SYSLOADS(4,NOSYS)       &
            ,ADDMASS(0:MT,NZ,NOSYS),concs_t(0:mt,nz)
      REAL, DIMENSION(0:MT)        :: PUPX,PUPY,PVPX,PVPY
      REAL, DIMENSION(0:MT)        :: PFPX,PFPY,PFPXD,PFPYD,VISCOFF
      REAL, DIMENSION(3*(NT),NZ)    :: DTIJ
      REAL, DIMENSION(3*(NT),NZ) :: UVN
      REAL :: C21MIN, C21MAX, C22MIN, C22MAX
      REAL :: CARAYMEAN(0:MT,NZ,NOSYS)
      
      INTEGER :: ITRAK,IWRTADDM,IADDMDEBUG,INITMB,INEGS,IMBWK,IWK
      INTEGER :: I,I1,I2,ISTAT,J,IA,IB
      REAL    :: R,DELTBC,DELT,DELTNPS,DELTFL,DELTATM,DELTPS,TOTMAS   &
                ,DTMB,WK,CKMAX,TIMEAVE
      REAL    :: FFD,FF1,XI,YI,DXA,DYA,DXB,DYB,FIJ1,FIJ2,UN,VISCOF
      REAL    :: FACT,FM1,TXX,TYY,FXX,FYY,EXFLUX,TEMP
      REAL, DIMENSION(0:MT,NZ) ::                            &   !Tian Temporal
          DOAVEG,DOMING,DOMAXG,BOTBOD,BOTBODHS,XEMPRC           !Tian Temporal
      REAL    :: BVOL_TMP,DM_TMP	  
      integer :: issource(mt),IERR
!------------------------------------------------------------------------------
      issource = 0
      CARAYMEAN = 0.0
 
!        INITIALIZATION
      INITB=0
      ITRAK=1 
      ITIMESECS=0
      ITIMEWQSECS=0
      TIME=0.0
      NXPRTD=IPRNTDSECS
      NXPRTG=IPRNTGSECS
      IWRTADDM=0
      IADDMDEBUG=0
      INITMB=1
      IF(MASSBAL == 1) THEN
       IF(ISMBSECS == 0) THEN
        NXPRTMB=0
        INITMB=0
       ELSE
        NXPRTMB=ISMBSECS
        INITMB=1
       ENDIF
      ENDIF
      R=1.E-10
      write(out,*) "TZERO =",TZERO 
      IF(TZERO /= 0.) THEN
       if(msr)print*,'before call rca08'
       CALL RCA08
       print*,'after call rca08'
       ITIMESECS = 86400.*TZERO
       ITIMEWQSECS = 86400.*TZERO
       TIME = TZERO
       NXPRTD = ITIMESECS+IPRNTDSECS
       NXPRTG = ITIMESECS+IPRNTGSECS
       IF(MASSBAL == 1)THEN
        IF(ITIMESECS < ISMBSECS) THEN
         NXPRTMB=ISMBSECS
         INITMB=1
        ELSE
         NXPRTMB = ITIMESECS
         INITMB=0
        ENDIF
       ENDIF
      ENDIF

!     INITIALIZE ARRAYS
      CONCS  = 0.0
      QXFLX  = 0.0
      QYFLX  = 0.0
      QZFLX  = 0.0
      RXFLX  = 0.0
      RYFLX  = 0.0

      IF(MASSBAL == 1) THEN
       ALLOCATE(FLXMB(M,NZ,NOSYS+NOKINSYS),STAT=ISTAT)
       ALLOCATE(FLYMB(M,NZ,NOSYS+NOKINSYS),STAT=ISTAT)
       FLXMB = 0.
       FLYMB = 0.

       SYSLOADS = 0.0

       IF(INITMB == 0) THEN
        DO ISYS=1,NOSYS
         SYSMASS(ISYS)=0.0
         DO IZ=1,NZ
          DO IXY=1,MT
           IF(FSM(IXY) == 1.)                              & 
            SYSMASS(ISYS) = SYSMASS(ISYS) +                &
                            BVOL(IXY,IZ)*CARAY(IXY,IZ,ISYS)
          END DO
	 END DO
	END DO  

        DO ISYS=1,NOKINSYS
         SYSMASS(NOSYS+ISYS)=0.0
         DO IZ=1,NZ
          DO IXY=1,MT
           IF(FSM(IXY) == 1.)                              & 
            SYSMASS(NOSYS+ISYS) = SYSMASS(NOSYS+ISYS) +    &
                       BVOL(IXY,IZ)*CKINARRAY(IXY,IZ,ISYS)
          END DO
	 END DO
	END DO  

        WRITE(17)  TIME,SYSMASS,SYSLOADS,FLXMB,FLYMB
        NXPRTMB=NXPRTMB+IPRNTMBSECS
        INITMB=1

       ENDIF
      ENDIF

!$doacross local(isys,iz,ixy) , share(cslave)
      CSLAVE = 0.0

!  SET UP -AVECT- ARRAYS FOR TIME = 0.0 (TZERO)
!JQI      CALL RCAEXP1

!   PRINT INITIAL CONDITIONS
      CALL RCA09
!       IF(SERIAL)THEN
!        WRITE(11)   CARAY(:,:,1)  !Salinity
!        WRITE(11)   CARAY(:,:,2)  !Phyto1
!        WRITE(11)   CARAY(:,:,3)  !Phyto2
!        WRITE(11)   CARAY(:,:,4)  !Phyto3
!        WRITE(11)   CARAY(:,:,6)  !LPOP
!        WRITE(11)   CARAY(:,:,8)  !LDOP
!        WRITE(11)   CARAY(:,:,9)  !PO4
!        WRITE(11)   CARAY(:,:,11)  !LPON
!        WRITE(11)   CARAY(:,:,13)  !LDON
!        WRITE(11)   CARAY(:,:,14)  !NH4
!        WRITE(11)   CARAY(:,:,15)  !NO3
!        WRITE(11)   CARAY(:,:,16)  !BSi
!        WRITE(11)   CARAY(:,:,17)  !SiO3
!        WRITE(11)   CARAY(:,:,19)  !LPOC
!        WRITE(11)   CARAY(:,:,21)  !LDOC
!        WRITE(11)   CARAY(:,:,25)  !A O2
!        WRITE(11)   CARAY(:,:,26)  !DO
!        WRITE(11)   CARAY(:,:,5)   !RPOP
!        WRITE(11)   CARAY(:,:,7)   !RDOP
!        WRITE(11)   CARAY(:,:,10)  !RPON
!        WRITE(11)   CARAY(:,:,12)  !RDON
!        WRITE(11)   CARAY(:,:,18)  !RPOC
!        WRITE(11)   CARAY(:,:,20)  !RDOC
!        WRITE(11)   CARAY(:,:,22)  !ExDOC
!        WRITE(11)   CARAY(:,:,23)  !RePOC
!        WRITE(11)   CARAY(:,:,24)  !ReDOC
!        WRITE(11)   CARAY(:,:,1)   !Sal
!        WRITE(11)   CARAY(:,:,26)  !DO
!       ELSE
! #  if defined (MULTIPROCESSOR)
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,1),1,MT,M,NXY,NZ,NMAP,1,"SAL_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,2),1,MT,M,NXY,NZ,NMAP,1,"PHYT1_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,3),1,MT,M,NXY,NZ,NMAP,1,"PHYT2_ini")       
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,4),1,MT,M,NXY,NZ,NMAP,1,"PHYT3_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,6),1,MT,M,NXY,NZ,NMAP,1,"LPOP_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,8),1,MT,M,NXY,NZ,NMAP,1,"LDOP_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,9),1,MT,M,NXY,NZ,NMAP,1,"DPO4_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,11),1,MT,M,NXY,NZ,NMAP,1,"LPON_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,13),1,MT,M,NXY,NZ,NMAP,1,"LDON_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,14),1,MT,M,NXY,NZ,NMAP,1,"DNH4_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,15),1,MT,M,NXY,NZ,NMAP,1,"NO23_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,16),1,MT,M,NXY,NZ,NMAP,1,"TPSI_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,17),1,MT,M,NXY,NZ,NMAP,1,"DSI_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,19),1,MT,M,NXY,NZ,NMAP,1,"LPOC_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,21),1,MT,M,NXY,NZ,NMAP,1,"LDOC_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,25),1,MT,M,NXY,NZ,NMAP,1,"O2EQ_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,26),1,MT,M,NXY,NZ,NMAP,1,"DO_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,5),1,MT,M,NXY,NZ,NMAP,1,"RPOP_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,7),1,MT,M,NXY,NZ,NMAP,1,"RDOP_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,10),1,MT,M,NXY,NZ,NMAP,1,"RPON_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,12),1,MT,M,NXY,NZ,NMAP,1,"RDON_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,18),1,MT,M,NXY,NZ,NMAP,1,"RPOC_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,20),1,MT,M,NXY,NZ,NMAP,1,"RDOC_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,22),1,MT,M,NXY,NZ,NMAP,1,"ExDOC_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,23),1,MT,M,NXY,NZ,NMAP,1,"RePOC_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,24),1,MT,M,NXY,NZ,NMAP,1,"ReDOC_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,1),1,MT,M,NXY,NZ,NMAP,1,"Sal_ini")
!        CALL PWRITE(11,MYID,NPROCS,CARAY(:,:,26),1,MT,M,NXY,NZ,NMAP,1,"DO_ini")
	
! #  endif	
!      ENDIF !(SERIAL)
      
      IDISK = 3 

!*********************   BEGINNING OF FULL INTEGRATION TIMESTEP LOOP

   30 CONTINUE

!   GET TOTAL MASS PER SYSTEM IF MASS/FLUX BALANCES REQUESTED
      IF(MASSBAL == 1 .AND. ITIMESECS == NXPRTMB) THEN
       DO ISYS=1,NOSYS
        SYSMASS(ISYS)=0.0
        DO IZ=1,NZ
         DO IXY=1,MT
          IF(FSM(IXY) == 1.)                               &
           SYSMASS(ISYS) = SYSMASS(ISYS) +                 &
                           BVOL(IXY,IZ)*CARAY(IXY,IZ,ISYS)
         END DO
	END DO
       END DO	
       DO ISYS=1,NOKINSYS
        SYSMASS(NOSYS+ISYS)=0.0
        DO IZ=1,NZ
         DO IXY=1,MT
          IF(FSM(IXY) == 1.)                               &
           SYSMASS(NOSYS+ISYS) = SYSMASS(NOSYS+ISYS) +     &
                       BVOL(IXY,IZ)*CKINARRAY(IXY,IZ,ISYS)	       
		       
         END DO
	END DO
       END DO	 
      ENDIF  
          

!  IF PIECEWISE LINEAR BOUNDARY CONDITIONS THEN
!  GET CONCENTRATIONS AT CURRENT TIME LEVEL
      IF(IBCPWLOPT == 1) THEN
!$doacross local(isys,deltbc,iz,ixy,i)
       DO ISYS=2,NOSYS  !Salinity uses FVCOM prediction in eutro.F
        IF(NOBC(ISYS) /= 0)THEN
         DELTBC = TIME - NXBCT
         DO I=1,NOBC(ISYS)
          IXY=IBC(1,I,ISYS)
          IZ =IBC(2,I,ISYS)
            CARAY(IXY,IZ,ISYS) = DELTBC*SBC(I,ISYS) + BBC(I,ISYS) 
         END DO
        END IF
       END DO 
      ENDIF
 
!     EVALUATE KINETIC DERIVATIVES
!lz      if(msr)print*,"BEFORE TUNER ",DTWQ,ITIMESECS,ITIMEWQSECS
      IF(DTWQ == 0.0 .OR. ITIMESECS >= ITIMEWQSECS) THEN

        CALL TUNER(DOAVEG)

       IDISK = 0
       ITIMEWQSECS=ITIMEWQSECS+IDTWQSECS
       TIMEWQ=ITIMEWQSECS/86400.
      ENDIF
      INITB = 1
!lz      if(msr)print*,"AFTER TUNER "
 
!     COMPUTE DEPTHS AT TIME LEVEL N
      DELT = TIME - (NXHYDTSECS-IHYDDTSECS)/86400.
!$doacross local(iy,ix) , share(d)
      IF(.NOT. ALLOCATED(D)) ALLOCATE(D(MT))
      IF(.NOT. ALLOCATED(D1)) ALLOCATE(D1(NT))
      D(1:MT) = H(1:MT)+ETA(1:MT)     !+DELT*DETA(1:MT)*86400.
!      D1(1:NT) = HC(1:NT)+ETA1(1:NT)    !+DELT*DETA1(1:NT)*86400.
      D1(1:NT) = (D(NV(1:NT,1))+D(NV(1:NT,2))+D(NV(1:NT,3)))/3.0
!     COMPUTE VOLUMES AT TIME LEVEL N+1
!$doacross local(iz,ixy)
      BVOLN = BVOL + DT*VDER

!     BEGIN INTEGRATION STEP

!     COMPUTE VOLUMES AT TIME LEVEL N+1
!$doacross local(iz,ixy)
!
!--Initialize Fluxes-----------------------------------------------------------
!
      DM = 0.0
!
!--Loop Over Control Volume Sub-Edges And Calculate Normal Velocity------------
!
      DO I=1,NCV
       I1=NTRG(I)
!JQI       DTIJ(I)=D1(I1)
       DO IZ=1,NZ
        DTIJ(I,IZ)=D1(I1)*DZ1(I1,IZ)
        UVN(I,IZ)=VV(I1,IZ)*DLTXE(I) - UU(I1,IZ)*DLTYE(I)
       END DO
      END DO
!
!--Calculate the Advection and Horizontal Diffusion Terms----------------------
!
!     MAIN SYSTEM LOOP

!      DO 400 ISYS=1,NOSYS
      DO 400 ISYS=2,NOSYS   !No more salinity calculation 
!
!      CHECK FOR SYSTEM BYPASS
       IF(SYSBY(ISYS) == 1)   GO TO 400
       INEGS=0
       TOTMAS=0.0
     
! GO TO 888   !Tian Temporal

       DO IZ=1,NZ
        PFPX  = 0.0
        PFPY  = 0.0
        PFPXD = 0.0
        PFPYD = 0.0

        DO I=1,M

         DO J=1,NTSN(I)-1
          I1=NBSN(I,J)
          I2=NBSN(I,J+1)

          FFD=0.5*(CARAY(I1,IZ,ISYS)+CARAY(I2,IZ,ISYS)              &
             -CARAYMEAN(I1,IZ,ISYS)-CARAYMEAN(I2,IZ,ISYS))
          FF1=0.5*(CARAY(I1,IZ,ISYS)+CARAY(I2,IZ,ISYS))
	  
!	  if(isys == 15) write(600,*) ffd,ff1

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

        DO I=1,M
         VISCOFF(I)=VISCOFH(I,IZ)
        END DO

        DO I=1,NCV_I
         IA=NIEC(I,1)
         IB=NIEC(I,2)
         XI=0.5*(XIJE(I,1)+XIJE(I,2))
         YI=0.5*(YIJE(I,1)+YIJE(I,2))
         DXA=XI-VX(IA)
         DYA=YI-VY(IA)
         DXB=XI-VX(IB)
         DYB=YI-VY(IB)
         FIJ1=CARAY(IA,IZ,ISYS)+DXA*PFPX(IA)+DYA*PFPY(IA)
         FIJ2=CARAY(IB,IZ,ISYS)+DXB*PFPX(IB)+DYB*PFPY(IB)

         C21MIN=MINVAL(CARAY(NBSN(IA,1:NTSN(IA)-1),IZ,ISYS))
         C21MIN=MIN(C21MIN, CARAY(IA,IZ,ISYS))
         C21MAX=MAXVAL(CARAY(NBSN(IA,1:NTSN(IA)-1),IZ,ISYS))
         C21MAX=MAX(C21MAX, CARAY(IA,IZ,ISYS))
         C22MIN=MINVAL(CARAY(NBSN(IB,1:NTSN(IB)-1),IZ,ISYS))
         C22MIN=MIN(C22MIN, CARAY(IB,IZ,ISYS))
         C22MAX=MAXVAL(CARAY(NBSN(IB,1:NTSN(IB)-1),IZ,ISYS))
         C22MAX=MAX(C22MAX, CARAY(IB,IZ,ISYS))
         IF(FIJ1 < C21MIN) FIJ1=C21MIN
         IF(FIJ1 > C21MAX) FIJ1=C21MAX
         IF(FIJ2 < C22MIN) FIJ2=C22MIN
         IF(FIJ2 > C22MAX) FIJ2=C22MAX

         UN=UVN(I,IZ)

         FACT = 1.0   !Tian Temporal
	 FM1 = 0.0
         VISCOF=FACT*(VISCOFF(IA)+VISCOFF(IB))*0.5 + FM1

         TXX=0.5*(PFPXD(IA)+PFPXD(IB))*VISCOF
         TYY=0.5*(PFPYD(IA)+PFPYD(IB))*VISCOF
	 
         FXX=-DTIJ(I,IZ)*TXX*DLTYE(I)
         FYY= DTIJ(I,IZ)*TYY*DLTXE(I)

         EXFLUX=-UN*DTIJ(I,IZ)*((1.0+SIGN(1.0,UN))*FIJ2+        & 
                 (1.0-SIGN(1.0,UN))*FIJ1)*0.5+FXX+FYY
  
         DM(IA,IZ,ISYS)=DM(IA,IZ,ISYS)+EXFLUX 
         DM(IB,IZ,ISYS)=DM(IB,IZ,ISYS)-EXFLUX 

	 
        END DO
       END DO 
# if defined (MULTIPROCESSOR)
       IF(PAR)THEN
	CALL NODE_MATCH(0,NBN,BN_MLT,BN_LOC,BNC,MT,NZ,MYID,NPROCS,       &
                        DM(:,:,ISYS))
       END IF
# endif

!
!--Calculate the Vertical Terms------------------------------------------------
!
       DO IZ = 1,NZ
        DO IXY=1,M
!IF(NGID(IXY).eq.3460.and.ISYS.eq.14) write(88,*)'rcaadv 422 ',TIME,IZ,CONCS(IXY,IZ),CARAY(IXY,IZ,ISYS)	
         IF(IZ == 1) THEN
          TEMP=-WTS(IXY,IZ+1)*(CARAY(IXY,IZ,ISYS)*DZ(IXY,IZ+1)+           &   
               CARAY(IXY,IZ+1,ISYS)*DZ(IXY,IZ))/                          &
               (DZ(IXY,IZ)+DZ(IXY,IZ+1))
         ELSE IF(IZ == NZ) THEN
          TEMP=WTS(IXY,IZ)*(CARAY(IXY,IZ,ISYS)*DZ(IXY,IZ-1)+              &
               CARAY(IXY,IZ-1,ISYS)*DZ(IXY,IZ))/                          &
               (DZ(IXY,IZ)+DZ(IXY,IZ-1))
         ELSE
          TEMP=WTS(IXY,IZ)*(CARAY(IXY,IZ,ISYS)*DZ(IXY,IZ-1)+              &
               CARAY(IXY,IZ-1,ISYS)*DZ(IXY,IZ))/                          &
               (DZ(IXY,IZ)+DZ(IXY,IZ-1))-                                 &
               WTS(IXY,IZ+1)*(CARAY(IXY,IZ,ISYS)*DZ(IXY,IZ+1)+            &
               CARAY(IXY,IZ+1,ISYS)*DZ(IXY,IZ))/                          &
               (DZ(IXY,IZ)+DZ(IXY,IZ+1))
         END IF

!
!--Total Fluxes ---------------------------------------------------------------
!
     
         IF(ISONB(IXY) == 2) THEN
          DM(IXY,IZ,ISYS)=TEMP*ART1(IXY)    !/DZ(IXY,IZ)
         ELSE
          DM(IXY,IZ,ISYS)=DM(IXY,IZ,ISYS)+TEMP*ART1(IXY)     !/DZ(IXY,IZ)
         END IF

!if(ISYS.eq.14.and.IZ.eq.30.and.NGID(IXY).eq.3920)write(88,'(A,10E10.2)')'NH4 rcaadv450 ',TIME,  &
!    -86400*DM(IXY,IZ,ISYS)/BVOLN(IXY,IZ), &
!     CDARAY(IXY,IZ,ISYS)/BVOLN(IXY,IZ),CARAY(IXY,IZ,ISYS),BVOLN(IXY,IZ)
!if(ISYS.eq.17.and.IZ.eq.30.and.NGID(IXY).eq.3920)write(87,'(A,10E10.2)')'SiO3 PhysBio ',TIME,  &
!    -86400*DM(IXY,IZ,ISYS)/BVOLN(IXY,IZ), &
!     CDARAY(IXY,IZ,ISYS)/BVOLN(IXY,IZ),CARAY(IXY,IZ,ISYS),BVOLN(IXY,IZ)
	 
        END DO
       END DO
 
 888  CONTINUE   !Tian Temporal
 
      DO IXY = 1,M
        IF(ISYS == 2)THEN
	  if(abs(vx(ixy)-876471.3422) < 1.0 .and. abs(vy(ixy)-(-109186.138)) < 1.0) &
	    write(100+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-865847.4266) < 1.0 .and. abs(vy(ixy)-(-73599.9225)) < 1.0) &
	    write(100+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-856189.8853) < 1.0 .and. abs(vy(ixy)-(-32446.4657)) < 1.0) &
	    write(100+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-852701.7285) < 1.0 .and. abs(vy(ixy)-(-62416.0614)) < 1.0) &
	    write(100+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-853563.4485) < 1.0 .and. abs(vy(ixy)-(-56867.7743)) < 1.0) &
	    write(100+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-862725.7822) < 1.0 .and. abs(vy(ixy)-(-39149.5107)) < 1.0) &
	    write(100+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-889801.2992) < 1.0 .and. abs(vy(ixy)-(-79228.8819)) < 1.0) &
	    write(100+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-842566.8990) < 1.0 .and. abs(vy(ixy)-(-45751.9188)) < 1.0) &
	    write(100+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-852829.0357) < 1.0 .and. abs(vy(ixy)-(-43532.9709)) < 1.0) &
	    write(100+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-855240.4672) < 1.0 .and. abs(vy(ixy)-(-52435.4278)) < 1.0) &
	    write(100+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-849481.8948) < 1.0 .and. abs(vy(ixy)-(-51285.4348)) < 1.0) &
	    write(100+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	END IF
        IF(ISYS == 3)THEN
	  if(abs(vx(ixy)-876471.3422) < 1.0 .and. abs(vy(ixy)-(-109186.138)) < 1.0) &
	    write(200+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-865847.4266) < 1.0 .and. abs(vy(ixy)-(-73599.9225)) < 1.0) &
	    write(200+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-856189.8853) < 1.0 .and. abs(vy(ixy)-(-32446.4657)) < 1.0) &
	    write(200+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-852701.7285) < 1.0 .and. abs(vy(ixy)-(-62416.0614)) < 1.0) &
	    write(200+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-853563.4485) < 1.0 .and. abs(vy(ixy)-(-56867.7743)) < 1.0) &
	    write(200+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-862725.7822) < 1.0 .and. abs(vy(ixy)-(-39149.5107)) < 1.0) &
	    write(200+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-889801.2992) < 1.0 .and. abs(vy(ixy)-(-79228.8819)) < 1.0) &
	    write(200+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-842566.8990) < 1.0 .and. abs(vy(ixy)-(-45751.9188)) < 1.0) &
	    write(200+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-852829.0357) < 1.0 .and. abs(vy(ixy)-(-43532.9709)) < 1.0) &
	    write(200+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-855240.4672) < 1.0 .and. abs(vy(ixy)-(-52435.4278)) < 1.0) &
	    write(200+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-849481.8948) < 1.0 .and. abs(vy(ixy)-(-51285.4348)) < 1.0) &
	    write(200+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	END IF
        IF(ISYS == 4)THEN
	  if(abs(vx(ixy)-876471.3422) < 1.0 .and. abs(vy(ixy)-(-109186.138)) < 1.0) &
	    write(300+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-865847.4266) < 1.0 .and. abs(vy(ixy)-(-73599.9225)) < 1.0) &
	    write(300+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-856189.8853) < 1.0 .and. abs(vy(ixy)-(-32446.4657)) < 1.0) &
	    write(300+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-852701.7285) < 1.0 .and. abs(vy(ixy)-(-62416.0614)) < 1.0) &
	    write(300+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-853563.4485) < 1.0 .and. abs(vy(ixy)-(-56867.7743)) < 1.0) &
	    write(300+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-862725.7822) < 1.0 .and. abs(vy(ixy)-(-39149.5107)) < 1.0) &
	    write(300+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-889801.2992) < 1.0 .and. abs(vy(ixy)-(-79228.8819)) < 1.0) &
	    write(300+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-842566.8990) < 1.0 .and. abs(vy(ixy)-(-45751.9188)) < 1.0) &
	    write(300+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-852829.0357) < 1.0 .and. abs(vy(ixy)-(-43532.9709)) < 1.0) &
	    write(300+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-855240.4672) < 1.0 .and. abs(vy(ixy)-(-52435.4278)) < 1.0) &
	    write(300+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	  if(abs(vx(ixy)-849481.8948) < 1.0 .and. abs(vy(ixy)-(-51285.4348)) < 1.0) &
	    write(300+myid,*) isys,vx(ixy),vy(ixy),DM(ixy,1,isys),DM(ixy,nz,isys)
	END IF
      END DO	

! ADD SOURCES/SINKS DUE TO KINETICS
!$doacross local(ixy,iz)
       
       DM(:,:,ISYS) = -DM(:,:,ISYS) *86400.0        !*DZ(:,:)
       DM(:,:,ISYS) = DM(:,:,ISYS)    + CDARAY(:,:,ISYS) 
      
!if(isys.eq.17)write(88,*),time,CARAY(3564,30,isys),CDARAY(3564,30,ISYS)
! SET MASS BALANCE FLAG FOR LOADS
       IMBWK=0
       IF(MASSBAL == 1 .AND. ITIMESECS >= ISMBSECS) THEN
        IF(IMBDOPT == 0) THEN
         IF(ITIMESECS == NXPRTMB) IMBWK=1
         DTMB=1.
        ELSE
         IMBWK=1
         DTMB=DT
        ENDIF
       ENDIF

! ADD LOADS

       IF(NOPS(ISYS) > 0) THEN
        DELTPS = TIME - NXPST
        DO I=1,NOPS(ISYS) 
         WK = DELTPS*SPS(I,ISYS) + BPS(I,ISYS)
         IF(IMBWK == 1) SYSLOADS(1,ISYS) = SYSLOADS(1,ISYS)+DTMB*WK
         IXY = IPS(1,I,ISYS)
         IWK = IPS(2,I,ISYS)	 
	 ISSOURCE(IXY) = 1
         DO IZ=1,NZ
          DM(IXY,IZ,ISYS) = DM(IXY,IZ,ISYS) + ZFRACPS(IZ,IWK)*WK	 
!JQI2017          DM(IXY,IZ,ISYS) = DM(IXY,IZ,ISYS) + ZFRACPS_GL(IZ,IWK)*WK
	  	  	  
!if(ISYS.eq.14.and.IZ.eq.30.and.NGID(IXY).eq.1666)write(82,'(A,10E10.2)')'NH4 rcaadv538 ',TIME,  &
!     DM(IXY,IZ,ISYS)/BVOLN(IXY,IZ), &
!     ZFRACPS_GL(IZ,IWK),WK,ZFRACPS_GL(IZ,IWK)*WK/BVOLN(IXY,IZ),BVOLN(IXY,IZ)
	  
         ENDDO
	END DO 
       ENDIF

       IF(NONPS(ISYS) > 0) THEN
        DELTNPS = TIME - NXNPST
        DO I=1,NONPS(ISYS)
         WK = DELTNPS*SNPS(I,ISYS) + BNPS(I,ISYS)
         IF(IMBWK == 1) SYSLOADS(2,ISYS) = SYSLOADS(2,ISYS)+DTMB*WK
         IXY = INPS(1,I,ISYS)
         IWK = INPS(2,I,ISYS)
	 ISSOURCE(IXY) = 1
	 
         DO IZ=1,NZ
          DM(IXY,IZ,ISYS) = DM(IXY,IZ,ISYS) + ZFRACNPS(IZ,IWK)*WK
!JQI2017          DM(IXY,IZ,ISYS) = DM(IXY,IZ,ISYS) + ZFRACNPS_GL(IZ,IWK)*WK

!if(ISYS.eq.14.and.IZ.eq.1.and.NGID(IXY).eq.4380)write(83,'(A,10E10.2)')'NH4 rcaadv513 ',TIME,  &
!     DM(IXY,IZ,ISYS)/BVOLN(IXY,IZ), &
!     ZFRACNPS_GL(IZ,IWK),WK,ZFRACNPS_GL(IZ,IWK)*WK/BVOLN(IXY,IZ),BVOLN(IXY,IZ)
     	  
         END DO
	END DO 
       ENDIF

       IF(NOFL(ISYS) > 0) THEN
        DELTFL = TIME - NXFLT
        DO I=1,NOFL(ISYS)
!	   IF (I.LT.61 .and. I.gt.65) THEN    !Tian temporal
           WK = DELTFL*SFL(I,ISYS) + BFL(I,ISYS)
!	   ELSE 
!	   WK = 0
!	   END IF
	 
         IF(IMBWK == 1) SYSLOADS(3,ISYS) = SYSLOADS(3,ISYS)+DTMB*WK
         IXY = IFL(1,I,ISYS)
         IWK = IFL(2,I,ISYS)
	 ISSOURCE(IXY) = 1
         DO IZ=1,NZ
          DM(IXY,IZ,ISYS) = DM(IXY,IZ,ISYS) + ZFRACFL(IZ,IWK)*WK
!JQI2017          DM(IXY,IZ,ISYS) = DM(IXY,IZ,ISYS) + ZFRACFL_GL(IZ,IWK)*WK

!if(ISYS.eq.14.and.IZ.eq.1.and.NGID(IXY).eq.4510)write(81,'(A,10E10.2)')'NH4 rcaadv538 ',TIME,  &
!     DM(IXY,IZ,ISYS)/BVOLN(IXY,IZ), &
!     ZFRACNPS_GL(IZ,IWK),WK,ZFRACNPS_GL(IZ,IWK)*WK/BVOLN(IXY,IZ),BVOLN(IXY,IZ)
  	  
         END DO
	END DO 
       ENDIF

       IF(NOATM(ISYS) > 0) THEN
        DELTATM = TIME - NXATMT
        DO IXY=1,M
         IF(FSM(IXY) > 0.) THEN 

          DM(IXY,1,ISYS) = DM(IXY,1,ISYS)  + (DELTATM*SATM(IXY,ISYS)     &
                        + BATM(IXY,ISYS))*XAZ(IXY)
          IF(IMBWK == 1) SYSLOADS(4,ISYS) = SYSLOADS(4,ISYS) +          &
             DTMB*(DELTATM*SATM(IXY,ISYS)+BATM(IXY,ISYS))*XAZ(IXY)
	 ISSOURCE(IXY) = 1
         END IF
	END DO 
       ENDIF

!  CALCULATE C AT TIME LEVEL N+1
!    PREDICTOR STEP (UPWIND ADVECTION, KINETICS, LOADS)
 
!$doacross local(iz,ixy)
       DO IZ=1,NZ
        DO IXY=1,M
         IF(FSM(IXY) > 0.) THEN
!JQI          CONCS(IXY,IZ) = (BVOL(IXY,IZ)*CARAY(IXY,IZ,ISYS)              &
!JQI                        + DT*DM(IXY,IZ,ISYS))/BVOLN(IXY,IZ)
          BVOL_TMP = BVOL(IXY,IZ)/BVOLN(IXY,IZ)
          DM_TMP = DT*DM(IXY,IZ,ISYS)/BVOLN(IXY,IZ)
          CONCS(IXY,IZ) = BVOL_TMP*CARAY(IXY,IZ,ISYS) + DM_TMP

!IF(NGID(IXY).eq.3460.and.ISYS.eq.14) write(88,*)'rcaadv 561 ',TIME,IZ,CONCS(IXY,IZ),CARAY(IXY,IZ,ISYS)
         END IF
        END DO
       END DO 
         
#  if defined(MULTIPROCESSOR)
       IF(PAR)CALL NODE_MATCH(1,NBN,BN_MLT,BN_LOC,BNC,MT,KB,            &
                              MYID,NPROCS,CONCS)
#  endif

       concs_t(:,:) = caray(:,:,isys)
       CALL FCT_S(concs,concs_t,issource)

      DO IXY = 1,M
        IF(ISYS == 2)THEN
	  if(abs(vx(ixy)-876471.3422) < 1.0 .and. abs(vy(ixy)-(-109186.138)) < 1.0) &
	    write(400+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-865847.4266) < 1.0 .and. abs(vy(ixy)-(-73599.9225)) < 1.0) &
	    write(400+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-856189.8853) < 1.0 .and. abs(vy(ixy)-(-32446.4657)) < 1.0) &
	    write(400+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-852701.7285) < 1.0 .and. abs(vy(ixy)-(-62416.0614)) < 1.0) &
	    write(400+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-853563.4485) < 1.0 .and. abs(vy(ixy)-(-56867.7743)) < 1.0) &
	    write(400+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-862725.7822) < 1.0 .and. abs(vy(ixy)-(-39149.5107)) < 1.0) &
	    write(400+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-889801.2992) < 1.0 .and. abs(vy(ixy)-(-79228.8819)) < 1.0) &
	    write(400+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-842566.8990) < 1.0 .and. abs(vy(ixy)-(-45751.9188)) < 1.0) &
	    write(400+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-852829.0357) < 1.0 .and. abs(vy(ixy)-(-43532.9709)) < 1.0) &
	    write(400+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-855240.4672) < 1.0 .and. abs(vy(ixy)-(-52435.4278)) < 1.0) &
	    write(400+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-849481.8948) < 1.0 .and. abs(vy(ixy)-(-51285.4348)) < 1.0) &
	    write(400+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	END IF
        IF(ISYS == 3)THEN
	  if(abs(vx(ixy)-876471.3422) < 1.0 .and. abs(vy(ixy)-(-109186.138)) < 1.0) &
	    write(500+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-865847.4266) < 1.0 .and. abs(vy(ixy)-(-73599.9225)) < 1.0) &
	    write(500+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-856189.8853) < 1.0 .and. abs(vy(ixy)-(-32446.4657)) < 1.0) &
	    write(500+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-852701.7285) < 1.0 .and. abs(vy(ixy)-(-62416.0614)) < 1.0) &
	    write(500+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-853563.4485) < 1.0 .and. abs(vy(ixy)-(-56867.7743)) < 1.0) &
	    write(500+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-862725.7822) < 1.0 .and. abs(vy(ixy)-(-39149.5107)) < 1.0) &
	    write(500+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-889801.2992) < 1.0 .and. abs(vy(ixy)-(-79228.8819)) < 1.0) &
	    write(500+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-842566.8990) < 1.0 .and. abs(vy(ixy)-(-45751.9188)) < 1.0) &
	    write(500+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-852829.0357) < 1.0 .and. abs(vy(ixy)-(-43532.9709)) < 1.0) &
	    write(500+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-855240.4672) < 1.0 .and. abs(vy(ixy)-(-52435.4278)) < 1.0) &
	    write(500+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-849481.8948) < 1.0 .and. abs(vy(ixy)-(-51285.4348)) < 1.0) &
	    write(500+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	END IF
        IF(ISYS == 4)THEN
	  if(abs(vx(ixy)-876471.3422) < 1.0 .and. abs(vy(ixy)-(-109186.138)) < 1.0) &
	    write(600+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-865847.4266) < 1.0 .and. abs(vy(ixy)-(-73599.9225)) < 1.0) &
	    write(600+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-856189.8853) < 1.0 .and. abs(vy(ixy)-(-32446.4657)) < 1.0) &
	    write(600+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-852701.7285) < 1.0 .and. abs(vy(ixy)-(-62416.0614)) < 1.0) &
	    write(600+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-853563.4485) < 1.0 .and. abs(vy(ixy)-(-56867.7743)) < 1.0) &
	    write(600+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-862725.7822) < 1.0 .and. abs(vy(ixy)-(-39149.5107)) < 1.0) &
	    write(600+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-889801.2992) < 1.0 .and. abs(vy(ixy)-(-79228.8819)) < 1.0) &
	    write(600+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-842566.8990) < 1.0 .and. abs(vy(ixy)-(-45751.9188)) < 1.0) &
	    write(600+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-852829.0357) < 1.0 .and. abs(vy(ixy)-(-43532.9709)) < 1.0) &
	    write(600+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-855240.4672) < 1.0 .and. abs(vy(ixy)-(-52435.4278)) < 1.0) &
	    write(600+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-849481.8948) < 1.0 .and. abs(vy(ixy)-(-51285.4348)) < 1.0) &
	    write(600+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	END IF
      END DO	
      
       CALL VDIF(CONCS)  
       
      DO IXY = 1,M
        IF(ISYS == 2)THEN
	  if(abs(vx(ixy)-876471.3422) < 1.0 .and. abs(vy(ixy)-(-109186.138)) < 1.0) &
	    write(700+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-865847.4266) < 1.0 .and. abs(vy(ixy)-(-73599.9225)) < 1.0) &
	    write(700+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-856189.8853) < 1.0 .and. abs(vy(ixy)-(-32446.4657)) < 1.0) &
	    write(700+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-852701.7285) < 1.0 .and. abs(vy(ixy)-(-62416.0614)) < 1.0) &
	    write(700+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-853563.4485) < 1.0 .and. abs(vy(ixy)-(-56867.7743)) < 1.0) &
	    write(700+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-862725.7822) < 1.0 .and. abs(vy(ixy)-(-39149.5107)) < 1.0) &
	    write(700+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-889801.2992) < 1.0 .and. abs(vy(ixy)-(-79228.8819)) < 1.0) &
	    write(700+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-842566.8990) < 1.0 .and. abs(vy(ixy)-(-45751.9188)) < 1.0) &
	    write(700+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-852829.0357) < 1.0 .and. abs(vy(ixy)-(-43532.9709)) < 1.0) &
	    write(700+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-855240.4672) < 1.0 .and. abs(vy(ixy)-(-52435.4278)) < 1.0) &
	    write(700+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-849481.8948) < 1.0 .and. abs(vy(ixy)-(-51285.4348)) < 1.0) &
	    write(700+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	END IF
        IF(ISYS == 3)THEN
	  if(abs(vx(ixy)-876471.3422) < 1.0 .and. abs(vy(ixy)-(-109186.138)) < 1.0) &
	    write(800+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-865847.4266) < 1.0 .and. abs(vy(ixy)-(-73599.9225)) < 1.0) &
	    write(800+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-856189.8853) < 1.0 .and. abs(vy(ixy)-(-32446.4657)) < 1.0) &
	    write(800+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-852701.7285) < 1.0 .and. abs(vy(ixy)-(-62416.0614)) < 1.0) &
	    write(800+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-853563.4485) < 1.0 .and. abs(vy(ixy)-(-56867.7743)) < 1.0) &
	    write(800+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-862725.7822) < 1.0 .and. abs(vy(ixy)-(-39149.5107)) < 1.0) &
	    write(800+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-889801.2992) < 1.0 .and. abs(vy(ixy)-(-79228.8819)) < 1.0) &
	    write(800+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-842566.8990) < 1.0 .and. abs(vy(ixy)-(-45751.9188)) < 1.0) &
	    write(800+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-852829.0357) < 1.0 .and. abs(vy(ixy)-(-43532.9709)) < 1.0) &
	    write(800+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-855240.4672) < 1.0 .and. abs(vy(ixy)-(-52435.4278)) < 1.0) &
	    write(800+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-849481.8948) < 1.0 .and. abs(vy(ixy)-(-51285.4348)) < 1.0) &
	    write(800+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	END IF
        IF(ISYS == 4)THEN
	  if(abs(vx(ixy)-876471.3422) < 1.0 .and. abs(vy(ixy)-(-109186.138)) < 1.0) &
	    write(900+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-865847.4266) < 1.0 .and. abs(vy(ixy)-(-73599.9225)) < 1.0) &
	    write(900+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-856189.8853) < 1.0 .and. abs(vy(ixy)-(-32446.4657)) < 1.0) &
	    write(900+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-852701.7285) < 1.0 .and. abs(vy(ixy)-(-62416.0614)) < 1.0) &
	    write(900+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-853563.4485) < 1.0 .and. abs(vy(ixy)-(-56867.7743)) < 1.0) &
	    write(900+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-862725.7822) < 1.0 .and. abs(vy(ixy)-(-39149.5107)) < 1.0) &
	    write(900+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-889801.2992) < 1.0 .and. abs(vy(ixy)-(-79228.8819)) < 1.0) &
	    write(900+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-842566.8990) < 1.0 .and. abs(vy(ixy)-(-45751.9188)) < 1.0) &
	    write(900+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-852829.0357) < 1.0 .and. abs(vy(ixy)-(-43532.9709)) < 1.0) &
	    write(900+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-855240.4672) < 1.0 .and. abs(vy(ixy)-(-52435.4278)) < 1.0) &
	    write(900+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	  if(abs(vx(ixy)-849481.8948) < 1.0 .and. abs(vy(ixy)-(-51285.4348)) < 1.0) &
	    write(900+myid,*) isys,vx(ixy),vy(ixy),concs_t(ixy,1),concs_t(ixy,nz)
	END IF
      END DO	

#  if defined (MULTIPROCESSOR)
       IF(PAR)CALL NODE_MATCH(1,NBN,BN_MLT,BN_LOC,BNC,MT,KB,            &
                              MYID,NPROCS,CONCS)			      
!       IF(PAR)CALL EXCHANGE(NC,MT,KB,MYID,NPROCS,CONCS) !Interprocessor Exchange   !
       IF(PAR)CALL EXCHANGE(NC,MT,NZ,MYID,NPROCS,CONCS) !Interprocessor Exchange   !       
      
#  endif

!  BOUNDARY ELEMENTS ARE UNCHANGED

       DO I=1,NOBCALL
        IXY=IBCALL(1,I)
        IZ =IBCALL(2,I)
         CONCS(IXY,IZ)=CARAY(IXY,IZ,ISYS) 
	 IF(ISYS.eq.14) CONCS(IXY,IZ) =CONCS(IXY,IZ) +NCRB1(IXY,IZ)*CARAY(IXY,IZ,2) &
	 + NCRB2(IXY,IZ)*CARAY(IXY,IZ,3) + NCRB3(IXY,IZ)*CARAY(IXY,IZ,4)  !Algal ammonia	 
       END DO 

!     COMPUTE VOLUMES AT TIME LEVEL N+1
!$doacross local(iz,ixy)
!JQI      BVOLN = BVOL + DT*VDER
!***********************************************************************
!        CHECK FOR MASS/FLUX BALANCE COMPUTATIONS
      IF(MASSBAL == 0) GO TO 333
!        INSTANTANEOUS OR AVERAGED
      IF(IMBDOPT == 0 .AND. ITIMESECS /= NXPRTMB) GO TO 333
      IF(ITIMESECS < ISMBSECS) GO TO 333
      DO IZ=1,NZ
       DO IXY=1,M
         FLXMB(IXY,IZ,ISYS)=FLXMB(IXY,IZ,ISYS)+DTMB*QXFLX(IXY,IZ)
         FLYMB(IXY,IZ,ISYS)=FLYMB(IXY,IZ,ISYS)+DTMB*QYFLX(IXY,IZ)
       ENDDO
      ENDDO 

!**********************************************************************
!        CHECK FOR SIGMA-LEVEL CORRECTION OPTION
  333   IF(SLCOPT == 1 .AND.                                            &
          MOD((ITIMESECS-IFIX(86400.*TZERO)),IDTSLCSECS) <= 0.) THEN
          CALL RCAMPROF(CARAY(1,1,ISYS),CSLAVE(1,1,ISYS))
        ENDIF

!        CHECK FOR MASS/FLUX BALANCE COMPUTATIONS
!JQI      IF(MASSBAL.EQ.0) GO TO 280
!        INSTANTANEOUS OR AVERAGED
!JQI      IF(IMBDOPT.EQ.0 .AND. ITIMESECS.NE.NXPRTMB) GO TO 280
!JQI      IF(ITIMESECS.LT.ISMBSECS) GO TO 280
!JQI      DO IZ=1,NZ
!JQI       DO IXY=1,NXY
!JQI         FLXMB(IXY,IZ,ISYS)=FLXMB(IXY,IZ,ISYS)+DTMB*RXFLX(IXY,IZ)
!JQI         FLYMB(IXY,IZ,ISYS)=FLYMB(IXY,IZ,ISYS)+DTMB*RYFLX(IXY,IZ)
!JQI       ENDDO
!JQI      ENDDO
 
!**********************************************************************

!  COMPLETE REST OF INTEGRATION STEP
 
!  BOUNDARY ELEMENTS ARE UNCHANGED
!JQI       DO I=1,NOBCALL
!JQI        IXY=IBCALL(1,I)
!JQI        IZ =IBCALL(2,I)
!JQI        CONCS(IXY,IZ)=CARAY(IXY,IZ,ISYS)
!JQI       END DO 

!        CHECK TO SEE IF NEGATIVE SOLUTIONS PERMITTED 
!$doacross local(iz,iy,ix,is,ie,inegs,totmas) , share(negseg)
      DO IZ=1,NZ
       DO IXY=1,M
!IF(NGID(IXY).EQ.3460.and.ISYS.eq.14) write(88,*)'rcaadv 640 ',time,IZ,CONCS(IXY,IZ),CARAY(IXY,IZ,ISYS)
        IF(FSM(IXY) > 0.) THEN
         IF(NEGSLN == 1) THEN
          CARAY(IXY,IZ,ISYS) = CONCS(IXY,IZ)  
         ELSE
!        IF NOT ... CHECK TO MAKE SURE SOLUTION IS NOT
!                   FORCED THRU ZERO (1.0E-38)
          IF(CONCS(IXY,IZ) >= CMIN(ISYS)) THEN
           CARAY(IXY,IZ,ISYS) = CONCS(IXY,IZ)
          ELSE
!        PREVENT NEG SOLN BY QUARTERING PRESENT CONC AND PRINT WARNING
           CARAY(IXY,IZ,ISYS) = MAX(CARAY(IXY,IZ,ISYS)/4.,CMIN(ISYS))
!$        call m_lock
           INEGS = INEGS+1
           IF(INEGS <= 99) THEN
            NEGSEG(INEGS,1) = IXY
            NEGSEG(INEGS,2) = IZ
           ENDIF
           TOTMAS = TOTMAS + BVOLN(IXY,IZ)*                    &
                    (CARAY(IXY,IZ,ISYS)-CONCS(IXY,IZ))
           ADDMASS(IXY,IZ,ISYS) = ADDMASS(IXY,IZ,ISYS) +          &
                BVOLN(IXY,IZ)*(CARAY(IXY,IZ,ISYS)-CONCS(IXY,IZ))
           IWRTADDM=1
!$        call m_unlock
          ENDIF
         ENDIF
	END IF    
       END DO
      END DO 	
   
#  if defined (MULTIPROCESSOR)
       IF(PAR)CALL NODE_MATCH(1,NBN,BN_MLT,BN_LOC,BNC,MT,NZ,MYID,NPROCS,CARAY(:,:,ISYS))
       IF(PAR)CALL EXCHANGE(NC,MT,NZ,MYID,NPROCS,CARAY(:,:,ISYS))       
#  endif

      IF(IADDMDEBUG == 1) THEN
       IF(INEGS > 0) THEN
!$     call mp_setlock
        CALL RCAMESS(2,0.001*TOTMAS)
        WRITE(OUT,1000)  ((NEGSEG(I,J),J=1,2),I=1,MIN(INEGS,99))
 1000   FORMAT(30X,12I5)
!$     call mp_unsetlock
       ENDIF
      ENDIF
     
       IF(SYSBY(ISYS) /= 1) THEN
        CKMAX = CMAX(ISYS)
        DO IZ=1,NZ
         DO IXY=1,M
          IF(FSM(IXY) > 0.) THEN
           IF(CARAY(IXY,IZ,ISYS) > CMAX(ISYS)) THEN
           !  print*,'SYS ',ISYS,' exceeds max,t,node,z,v,m'
	   !  PRINT*,time,NGID(ixy),iz,CARAY(IXY,IZ,ISYS),CMAX(ISYS)
	    CARAY(IXY,IZ,ISYS) = CMAX(ISYS) 
	   ! CALL PSTOP('rcaadv 691')   !Tian
	   END IF  
	  END IF 
	 END DO
	END DO      
       END IF
       
      ! CALL PRINT_VALS

  400 CONTINUE

!        SEE IF MASS HAS BEEN ADDED TO THE SYSTEM
      IF(IWRTADDM == 1 .AND. MSR) THEN
       OPEN(19,FILE='RCAFADDM',FORM='UNFORMATTED',STATUS='REPLACE')
       WRITE(19)  ADDMASS
       CLOSE(19)
       IWRTADDM=0
      ENDIF

!     FLUX BALANCES FOR MISC KINETIC VARIABLES
      IF(MASSBAL == 0 .OR. NOKINSYS == 0) GO TO 410
!        INSTANTANEOUS OR AVERAGED
      IF(IMBDOPT == 0) THEN 
       IF(ITIMESECS /= NXPRTMB) GO TO 410
       DO 405 ISYS=1,NOKINSYS
        DO 405 IZ=1,NZ
         DO 405 IXY=1,M
!JQI           IF(FSM(IXY) /= 1.) GO TO 405
!JQI            FLXMB(IXY,IZ,NOSYS+ISYS) = 
!JQI     .         (AMAX1(0.,QX(IXY,IZ)*CKINARRAY(IX-1,IY,IZ,ISYS))
!JQI     .        + AMIN1(0.,QX(IX,IY,IZ)*CKINARRAY(IX,IY,IZ,ISYS))
!JQI     .        + RX(IX,IY,IZ)
!JQI     .           *(CKINARRAY(IX-1,IY,IZ,ISYS)-CKINARRAY(IX,IY,IZ,ISYS)))
!JQI            FLYMB(IX,IY,IZ,NOSYS+ISYS) = 
!JQI     .         (AMAX1(0.,QY(IX,IY,IZ)*CKINARRAY(IX,IY-1,IZ,ISYS))
!JQI     .        + AMIN1(0.,QY(IX,IY,IZ)*CKINARRAY(IX,IY,IZ,ISYS))
!JQI     .        + RY(IX,IY,IZ)
!JQI     .           *(CKINARRAY(IX,IY-1,IZ,ISYS)-CKINARRAY(IX,IY,IZ,ISYS)))
  405  CONTINUE
      ELSE
       IF(ITIMESECS < ISMBSECS) GO TO 410
       DO 407 ISYS=1,NOKINSYS
        DO 407 IZ=1,NZ
         DO 407 IXY=1,M
           IF(FSM(IXY) /= 1.) GO TO 407
!JQI            FLXMB(IX,IY,IZ,NOSYS+ISYS) = FLXMB(IX,IY,IZ,NOSYS+ISYS) 
!JQI     .        +DT*(AMAX1(0.,QX(IX,IY,IZ)*CKINARRAY(IX-1,IY,IZ,ISYS))
!JQI     .             + AMIN1(0.,QX(IX,IY,IZ)*CKINARRAY(IX,IY,IZ,ISYS))
!JQI     .             + RX(IX,IY,IZ)
!JQI     .           *(CKINARRAY(IX-1,IY,IZ,ISYS)-CKINARRAY(IX,IY,IZ,ISYS)))
!JQI            FLYMB(IX,IY,IZ,NOSYS+ISYS) = FLYMB(IX,IY,IZ,NOSYS+ISYS) 
!JQI     .        +DT*(AMAX1(0.,QY(IX,IY,IZ)*CKINARRAY(IX,IY-1,IZ,ISYS))
!JQI     .             + AMIN1(0.,QY(IX,IY,IZ)*CKINARRAY(IX,IY,IZ,ISYS))
!JQI     .             + RY(IX,IY,IZ)
!JQI     .           *(CKINARRAY(IX,IY-1,IZ,ISYS)-CKINARRAY(IX,IY,IZ,ISYS)))
  407  CONTINUE
      ENDIF 

!$doacross local(iz,ixy)
!JQI  410 BVOL = BVOLN
  410 DO IZ = 1,NZ
       DO IXY = 1, M
        BVOL(IXY,IZ)=ART1(IXY)*(H(IXY)+ETA(IXY))*DZ(IXY,IZ)+1.E-20
       END DO
      END DO
!  PREPARE FOR NEXT TIME STEP
      ITIMESECS = ITIMESECS + IDTSECS
      TIME = ITIMESECS/86400.

!      print*,'STOP HERE? ',ITIMESECS, NXPRTD, NXPRTG, MASSBAL
!  CHECK TO SEE IF IT IS TIME TO PRINT RESULTS
!  FIRST DETAILED DUMPS
      IF(ITIMESECS < NXPRTD)  GO TO 435
!  DETAILED PRINT TIME
!  SET IDISK FOR DETAILED DUMPS
      IDISK=2
      NXPRTD = NXPRTD + IPRNTDSECS
!  SECOND GLOBAL DUMPS
 435  IF(ITIMESECS < NXPRTG)  GO TO 450
!  GLOBAL PRINT TIME
!  CALL PRINT ROUTINE
#if defined (MULTIPROCESSOR)
     IF(PAR)CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
#endif
      CALL RCA09
      
      IDISK=IDISK+1
      NXPRTG = NXPRTG + IPRNTGSECS

!  NOW PERFORM STABILITY CHECK
     ! DO ISYS=1,NOSYS 
     !  IF(SYSBY(ISYS) /= 1) THEN
     !   CKMAX = CMAX(ISYS)
     !   DO IZ=1,NZ
     !    DO IXY=1,M
     !     IF(FSM(IXY) > 0.) THEN
     !      IF(CARAY(IXY,IZ,ISYS) > CKMAX)   GO TO 500
     !     END IF 
     !	 END DO
     !	END DO      
     !  END IF
     ! END DO

!  MASS/FLUX BALANCE DUMPS
  450 IF(MASSBAL == 0) GO TO 470
      IF(ITIMESECS < NXPRTMB) GO TO 470
       IF(IMBDOPT == 0) THEN
        WRITE(17)  TIME,SYSMASS,SYSLOADS,FLXMB,FLYMB
       ELSE
        TIMEAVE=FLOAT(IPRNTMBSECS)/86400.

        FLXMB=FLXMB/TIMEAVE
        FLYMB=FLYMB/TIMEAVE

        SYSLOADS=SYSLOADS/TIMEAVE

        WRITE(17)  TIME,SYSMASS,SYSLOADS,FLXMB,FLYMB
       ENDIF
!  RESET ARRAYS AND COUNTER
      SYSLOADS = 0.

      FLXMB = 0.0
      FLYMB = 0.0

      NXPRTMB = NXPRTMB + IPRNTMBSECS
      IF(NXPRTMB > IEMBSECS) MASSBAL=0

!        CHECK TO SEE IF NECESSARY TO UPDATE TIME-VARIABLE FUNCTIONS
!        FIRST POINT SOURCE LOADS 
  470 IF(IPSOPT > 1 .AND. TIME >= NXPST)                            &
         CALL RCA10(SPS_GL,BPS_GL,MXWK,NOPS_GL,NXPST,33,IPSPWLOPT,SCALPS)
!        NONPOINT SOURCE LOADS 
      IF(INPSOPT > 1 .AND. TIME >= NXNPST)                          &
         CALL RCA10(SNPS_GL,BNPS_GL,MXWK,NONPS_GL,NXNPST,34,INPSPWLOPT,SCALNPS)
!        FALL-LINE LOADS 
      IF(IFLOPT > 1 .AND. TIME >= NXFLT)                            &
         CALL RCA10(SFL_GL,BFL_GL,MXWK,NOFL_GL,NXFLT,35,IFLPWLOPT,SCALFL)
!        ATMOSPHERIC LOADS 
      IF(IATMOPT > 1 .AND. TIME >= NXATMT)                          &
         CALL RCA10(SATM_GL,BATM_GL,NXY,NOATM_GL,NXATMT,36,IATMPWLOPT,       &
                    SCALATM)
		    
        CALL GLOB2LOC10
     
!        NEXT BOUNDARY CONDITIONS 
!       print*,'after 470 50 ',IBCOPT, TIME, NXBCT
      IF(IBCOPT > 1 .AND. TIME >= NXBCT) CALL RCA11 
!        NEXT TRANSPORT FIELDS
!     IF(ITIMESECS.GE.HYDBRK(ITIMHYD))  THEN
!       print*,'after 470 5 ',IBCOPT, TIME, NXBCT
!       print*,'after 470 6 ',ITIMESECS, NXHYDTSECS
      IF(ITIMESECS >= NXHYDTSECS) THEN
         if(msr)print*,"BEFORE RCA03A"
         CALL RCA03A
         if(msr)print*,"AFTER RCA03A"
!JQI         CALL RCAEXP1
!        ITIMHYD = ITIMHYD+1
      ENDIF
 
!        CHECK FOR END OF SIMULATION...
!           IF NOT GO BACK AND TAKE NEXT INTEGRATION STEP
!       print*,'after 470 7 ',TIME, TEND, NXBCT
! lz      if(msr) print*,'TIME and TEND = ',TIME,TEND
      IF(TIME <= TEND) GO TO 30 
      ITRAK = ITRAK+1 
        if(msr)print*,"ITRAK = ",ITRAK,NSTEP
      IF(ITRAK <= NSTEP) THEN
       IDTSECS=ISTEP(ITRAK)
       DT=ISTEP(ITRAK)/86400.
       TEND = TBRK(ITRAK) 
       IF(DT == 0.0) THEN
        CALL RCAMESS(3,DT)
        IDISK=1
        INITB=2
 !lz       if(msr)print*,"BEFORE TUNER"
        CALL TUNER
 !lz       if(msr)print*,"AFTER TUNER"
        CALL EXIT
       ENDIF
      ELSE
       TEND = 0.0
      ENDIF 

      IF(TEND > 0.) GO TO 30 
!        FINISHED...
      WRITE(OUT,8000)
 8000 FORMAT(///30X,'RCAEXPS FINISHED INTEGRATION'/        &
                30X,'USER DUMPS TO FOLLOW'//)
      IF(MASSBAL == 1) THEN
        DEALLOCATE(FLXMB)
        DEALLOCATE(FLYMB)
      ENDIF
      RETURN

!         STABILITY CRITERIA VIOLATED...ABEND 
  500 CALL RCAMESS(1,CKMAX) 
      WRITE(OUT,9000)  BVOLN(IXY,IZ),DIAG(IXY,IZ)
 9000 FORMAT(10X,'VOLUME =',E13.4,'  ,DIAGONAL =',E13.4)
      IREC = IREC+1 
      IDISK = 1 
      INITB = 2
      if(msr)print*,"BEFORE TUNER 2"
      CALL TUNER
      if(msr)print*,"AFTER TUNER 2"
      RETURN
      END SUBROUTINE RCAADV
