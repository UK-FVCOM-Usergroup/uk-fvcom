!************************************************************************
!** Modified by JLM 2/22/01 to allow multiple boundary files           **
!**                                                                    **
!** added temporal diffusion variation to agree with hw's ch3d         **
!**  modified so that flux area is not dependent upon VFN              **
!**  bwb 1-12-93                                                       **
!**  corrected ratio calculation for non step boundary conditions      **
!**  bwb 1-12-93                                                       **
!**  modified error calculations so that mass balance can be run       **
!**  when sediment model is OFF  bwb 1-12-93                           **
!**  modified hardwired diffusion for Indian River to correspond to    **
!**  Harry Wang's CH3D diffusion scheme.  bwb 1-19-93                  **
!**  inserted Carl's new autostepping stability criteria bwb 1-20-93   **
!**  references to SALINITY_RUN have been removed   MNOEL 1-15-93      **
!**  references to HydroQual and CH3d have been removed now            **
!**  referenced as ASCII and binary    MNOEL 1-25-93                   **
!**  references to point and non-point sources have been replaced with **
!**  Source One and Source Two   MNOEL  1-25-93                        **
!**  Saving of final output for binary initial conditions added        **
!**  MNOEL 2-5-93                                                      **
!**  Removal of Ocean boundary conditions faces from DATA statements   **
!**  read as ASCII from wqm_map file   MNOEL  2-10-93                  **
!**  additional parameters made spacially variable-- note preprocessor **
!**  needed to produce ASCII input file (parameters specified by       **
!**  Cerco)   MNOEL 2-20-93                                            **
!**  Modified Kinetic flux output to be aggregated temporally and to   **
!**  write for every box in a binary output file                       **
!**  Benthos inputs from HydroQual's Oct 97 transmission               **
!**                                                                    **
!**                                                                    **
!**                                                                    **
!**                                                                    **
!**                           CE-QUAL-IC                               **
!**                                                                    **
!**                    An Integrated Compartment                       **
!**                       Water Quality Model                          **
!**                                                                    **
!**                           Version 1.1                              **
!**                          July 28, 1992                             **
!**                                                                    **
!**                           Developed by                             **
!**                                                                    **
!**             Carl F. Cerco      : Water quality scheme              **
!**             Raymond S. Chapman : Numerical solution scheme         **
!**             Thomas M. Cole     : Computer algorithms & coding      **
!**             Hydroqual          : Sediment compartment              **
!**                                                                    **
!**                    Water Quality Modeling Group                    **
!**                    U.S. Army Corps of Engineers                    **
!**                    Waterways Experiment Station                    **
!**                    Vicksburg, Mississippi 39180                    **
!**                                                                    **
!************************************************************************

   PROGRAM  PARWQM   
   USE ALL_VARS   
   USE FILE_INFO
   USE WQM
   USE WQM_INIT
   USE ALGAL
   USE SED
   USE MOD_OBCS
#  if defined (MULTIPROCESSOR)
   USE MOD_PAR
#  endif

   IMPLICIT NONE
   INTEGER :: J, JF, JG, JC, K, L, N
   INTEGER :: I, II, ISF, JP, JNP
   INTEGER :: JS1, JS2, JS3
   CHARACTER(LEN=100) :: TEMPSTR
   
   INTEGER :: I1,I2
   REAL    :: ITT,TTMP1,TTMP2


!************************************************************************
!**                       Program Initialization                       **
!************************************************************************

!==============================================================================!
!   SETUP PARALLEL ENVIRONMENT                                                 !
!==============================================================================!

   SERIAL = .TRUE. 
   PAR    = .FALSE. 
   MSR    = .TRUE.
   MYID   = 1
   NPROCS = 1
#  if defined (MULTIPROCESSOR)
   CALL INIT_MPI_ENV(MYID,NPROCS,SERIAL,PAR,MSR)
#  endif

!--IMPORT CASENAME FROM COMMAND LINE  
   IF(MSR)THEN
     CALL GETARG(1,TEMPSTR)

     IF(LEN_TRIM(TEMPSTR) == 0)THEN
       WRITE(*,*) 'PLEASE PROVIDE CASENAME ON COMMAND LINE'
       WRITE(*,*) 'STOPPING...'
       CALL PSTOP
     END IF
     CASENAME = ADJUSTL(TEMPSTR)
   END IF	

#  if defined (MULTIPROCESSOR)
   CALL MPI_BCAST(CASENAME,80,MPI_CHARACTER,0,MPI_COMM_WORLD,IERR)
#  endif

   ! <ezpp-begin>
   CALL INIT_FILE_INFO()    !  Setup WQM file information

   CALL INPUTS()            !  Initialze WQM

   CALL SETUP_OBC

   T(:,:)    = C2(:,:,1)
   SALT(:,:) = C2(:,:,2)
   SSI(:,:)  = C2(:,:,3)
   B1(:,:)   = C2(:,:,4)
   B2(:,:)   = C2(:,:,5)
   B3(:,:)   = C2(:,:,6)
   SZ(:,:)   = C2(:,:,7)
   LZ(:,:)   = C2(:,:,8)
   LDOC(:,:) = C2(:,:,9)
   RDOC(:,:) = C2(:,:,10)
   LPOC(:,:) = C2(:,:,11)
   RPOC(:,:) = C2(:,:,12)
   NH4(:,:)  = C2(:,:,13)
   NO3(:,:)  = C2(:,:,14)
   UREA(:,:) = C2(:,:,15)
   LDON(:,:) = C2(:,:,16)
   RDON(:,:) = C2(:,:,17)
   LPON(:,:) = C2(:,:,18)
   RPON(:,:) = C2(:,:,19)
   PO4(:,:)  = C2(:,:,20)
   LDOP(:,:) = C2(:,:,21)
   RDOP(:,:) = C2(:,:,22)
   LPOP(:,:) = C2(:,:,23)
   RPOP(:,:) = C2(:,:,24)
   PIP(:,:)  = C2(:,:,25)
   COD(:,:)  = C2(:,:,26)
   DO(:,:)   = C2(:,:,27)
   SU(:,:)   = C2(:,:,28) 
   SA(:,:)   = C2(:,:,29)
   PIB1(:,:) = C2(:,:,30)
   PIB2(:,:) = C2(:,:,31)
   PIB3(:,:) = C2(:,:,32)
 
!************************************************************************
!**                          Begin Simulation                          **
!************************************************************************


   DO WHILE(.NOT.END_RUN)
      ! <ezpp-user name="timestep">

     THOUR = ELTMS/3600.0
     
     IF(MSR)PRINT*,'SIMULATION TIME: ',THOUR,' HOURS',JDAY,'JDAYS'
!*******  time-varying kinetics data
!     Print*, 'calling TVDS'
     IF (JDAY >= NXTVD) CALL TVDS (NXTVD)
!     Print*, 'after TVDS'
!******* Update flows
!     Print*, 'ELTMS', ELTMS
!     Print*, 'NINT(ELTMS)', NINT(ELTMS)
     IF (NINT(ELTMS) >= NXHYD)  THEN
!      Print*, 'calling hydro'
      CALL FLUSH(6)
       CALL HYDRO()
!      Print*, 'after hydro'
      CALL FLUSH(6)
     END IF

!--Time interpolation of physical fields
     UU  = UNC1
     VV  = VNC1
     WTS = WTSNC1
     UARD_OBCN=UARD_OBCNNC1
     XFLUX_OBC=XFLUX_OBCNC1
     DTFA=DTFANC1  
     KH  = KHNC1
     T   = TNC1
     SALT= SNC1
     EL  = ELNC1
     D   = H+EL
!     Print*, 'after interp of phys fields'
!   C2(:,:,1)  = T(:,:)
!   C2(:,:,2)  = SALT(:,:)

!******* Dead sea case
     IF(.NOT. FLOW)THEN
       UU  = 0.0
       VV  = 0.0
       WTS = 0.0
     END IF
     IF(.NOT. Z_DIFFUSION) KH = 0.0  

     ELTMS1 = REAL(ELTMS,8)

!******* Time-variable solar radiation

!--- TIME OF DAY (SECONDS)
     RELTMS = ELTMS
     TODS = MOD(RELTMS,86400.)
     IF (TODS < TTSS) THEN
       I0 = 0.
     ELSE
!--- DAYS SINCE SUNRISE (DAYS)
       DSSR = (TODS-TTSS)/86400.
       I0 = 3.1416*IT*SIN(3.1416*DSSR/FD)/2./FD
       I0 = MAX(I0,0.)
     END IF

!--- HARDWIRE LIGHT HERE
!        I0 = 1.0        
        
!******* Light Attenuation

     IF (SAV_CALC .OR. ALGAE_CALC) THEN
       IF (JDAY >= NXKEI) THEN
         NXKEI = NXKEI + INTKE/24. 
         IF (I0 > 0. .OR. IAVG(1,1) > 0.) CALL LGHT_ATTN
       END IF
     END IF        

!******* SAV Submodel

     SAVENT = -9999
     IF (SAV_CALC) THEN
       CALL SAV_COMP
     END IF

!******* Determine cell quota for this time step

     DO K=1,KBM1
       DO I=1,MLOC
         Q1(I,K) = PIB1(I,K)/(B1(I,K)+1.0E-9)
!         IF (I.eq.25) THEN
!            Print*, 'PIB1', PIB1(I,K)
!            Print*, 'B1', B1(I,K)
!            Print*, 'Q1', Q1(I,K)
!         ENDIF
         Q2(I,K) = PIB2(I,K)/(B2(I,K)+1.0E-9)
         Q3(I,K) = PIB3(I,K)/(B3(I,K)+1.0E-9)
       END DO  
     END DO
        
!******* Sediment kinetics

     SEDENT = -9999
     IF (SEDIMENT_CALC) THEN
       CALL SED_CALC
     ELSE IF (BENTHIC_FLUXES) THEN
       CALL BEN_FLUX
     END IF

!******* Water column kinetics
     ! <ezpp-user name = "wc_kinetics">

     IF (TEMPERATURE_CALC) CALL TEMPER(DTC(1,1,1))
!tykim
        DTC(:,:,1)=0.0
     IF (SOLIDS_CALC)      CALL SOLIDS(DTC(1,1,3),FLUXS(1,1,1))
     IF (ZOO_CALC)         CALL ZOOPL(DTC(1,1,7),DTC(1,1,8))
     IF (ALGAE_CALC)         &
       CALL ALGAE(DTC(1,1,4),DTC(1,1,5),DTC(1,1,6),FLUXS(1,1,2),      &
	          FLUXS(1,1,3),FLUXS(1,1,4))
     IF (CARBON_CALC)      CALL CARBON(DTC(1,1,9), DTC(1,1,10),       &
                                       DTC(1,1,11),DTC(1,1,12),FLUXS(1,1,5))
     IF (NITROGEN_CALC)    CALL NITROG(DTC(1,1,13),DTC(1,1,14),DTC(1,1,16),  &
                                       DTC(1,1,17),DTC(1,1,18),DTC(1,1,19),  &
	                               FLUXS(1,1,6))
     IF (PHOSPHORUS_CALC)    &
       CALL PHOSPH(DTC(1,1,20),DTC(1,1,21),DTC(1,1,22),DTC(1,1,23),    &
                   DTC(1,1,24),DTC(1,1,25),DTC(1,1,30),DTC(1,1,31),    &
	           DTC(1,1,32),FLUXS(1,1,7),FLUXS(1,1,8),FLUXS(1,1,9))
     IF (COD_CALC)         CALL CODMND(DTC(1,1,26))
     IF (OXYGEN_CALC)      CALL OXYGEN(DTC(1,1,27))
     IF (SILICA_CALC)      CALL SILICA(DTC(1,1,28),DTC(1,1,29),FLUXS(1,1,10))

     SFENT = -9999
     IF (SFEEDER) THEN
       DO I=1,NSPECIES
         CALL SUSPFEED(I,DTC(1,1,4), DTC(1,1,5), DTC(1,1,6), DTC(1,1,30), &
                         DTC(1,1,31),DTC(1,1,32),DTC(1,1,11),DTC(1,1,23), &
	                 DTC(1,1,18),DTC(1,1,12),DTC(1,1,24),DTC(1,1,19), &
			 DTC(1,1,3), DTC(1,1,28),DTC(1,1,29),DTC(1,1,20))
       END DO
     END IF
!  Print*, 'after all wc characteristics'
!
!     *** Convert water column change in concentration to change in mass
!
! RGL added print 
!      Print*, 'DTLDOC', DTC(1,25,9)
!      Print*, 'DTRDOC', DTC(1,25,10)
     DO JC=1,NAC
       II=AC(JC)
       DO K=1,KBM1
         DO I=1,MLOC
           DTM(I,K,II) = DTC(I,K,II)       !*V1(I,K)
         END DO  
       END DO
     END DO  

!***** Horizontal diffusion

     IF (XY_DIFFUSION .AND. XYDF < 0.0) CALL VISCOF_H
!
     DO K=1,KBM1
       DO I=1,MLOC
!tykim  commented out below for analytical tests
!         CONLIT(I,K,1) = TE
!         DOS = 14.5532+TE*(0.0054258*TE-0.38217)         
!         CONLIT(I,K,27) = DOS
       END DO  
     END DO

     DO JC=1,NAC
       II=AC(JC)
       DO K=1,KBM1 
         DO I=1,MLOC
           DTM(I,K,II) = DTM(I,K,II)+(QLIT(I,K)*CONLIT(I,K,II))
         END DO
       END DO
     END DO	   
!  Print*, 'after horizontal diffusion'
!     CALL VERTVL
     CALL ADV_WQM
!  Print*, 'after adv_wqm'
!     CALL FCT_NUT
     CALL VDIF_WQM(C2F)
!  Print*, 'after VDIF_WQM'
     CALL EXCHANGE_WQM                              !Interprocessor Exchange   !
!  Print*, 'after exchange_wqm'
     CALL BCOND_WQM                                 !Boundary Conditions       !
!  Print*, 'after bcond_wqm'
     C2 = C2F

!******* Update time related variables

     NIT     = NIT+1
     ELTMS   = ELTMS+DLT
     DLTAV   = ELTMS/NIT
     JDAY    = ELTMS/86400.
!JLM        JDAY    = TMSTRT+ ELTMS/86400.                                  ! JLM 2/27/01
     ELTMJD  = JDAY-TMSTRT
     END_RUN = JDAY >= TMEND
     IF (JDAY >= DLTD(DLTDP+1)) THEN
       DLTDP = DLTDP+1
       IF (.NOT.AUTO_STEPPING) DLT = DLTVAL(DLTDP)
       MXDLT = DLTMAX(DLTDP)
       FNDLT = DLTFTN(DLTDP)
     END IF

!******* Mass balance

     IF (MASS_BALANCE) THEN
!
!         *** Water column constituent mass
!
        DO JC=1,NAC
          II=AC(JC)
          CMASS(II) = 0.0
          DO K=1,KBM1
            DO I=1,MLOC
              CMASS(II) = CMASS(II)+C1(I,K,II)*ART1(I)*D(I)*DZ(K)*0.001
	    END DO  
          END DO
	END DO  

!********* Water column nutrient mass

        WCMN = ANC1*CMASS(4)+ANC2*CMASS(5)+ANC3*CMASS(6)            &
               +ANCSZ*CMASS(7)+ANCLZ*CMASS(8)                       &
               +CMASS(13)+CMASS(14)+CMASS(15)+CMASS(16)             &
               +CMASS(17)+CMASS(18)+CMASS(19)
        WCMP = APCSZ*CMASS(7)+APCLZ*CMASS(8)                        &
               +CMASS(20)+CMASS(21)+CMASS(22)+CMASS(23)             &
               +CMASS(24)+CMASS(25)+CMASS(30)+CMASS(31)             &
               +CMASS(32)
        WCMC = CMASS(4)+CMASS(5)+CMASS(6)+CMASS(7)+CMASS(8)         &
               +CMASS(9)+CMASS(10)+CMASS(11)+CMASS(12)
        WCMS = ASC1*CMASS(4)+ASC2*CMASS(5)+ASC3*CMASS(6)            &
               +CMASS(28)+CMASS(29)

!********* Change in water column nutrient mass
          
        DLWCMN = WCMN-IWCMN
        DLWCMP = WCMP-IWCMP
        DLWCMC = WCMC-IWCMC

!********* Atmospheric loadings

       ATMFLXN = ATMFLXN+(ATMNH4+ATMNO3+ATMLDON+ATMRDON)            &
                 *PRECIP*SFATOT/1000.*DLT
       ATMFLXP = ATMFLXP+(ATMPO4+ATMLDOP+ATMRDOP)                   &
                 *PRECIP*SFATOT/1000.*DLT
       TM1=.001*DLT*PRECIP
       DO SB=1,MLOC
         ATMFLXNB(SB) = ATMFLXNB(SB)+                               &

           (ATMNH4+ATMNO3+ATMLDON+ATMRDON)*ART1(SB)*TM1
         ATMFLXPB(SB) = ATMFLXPB(SB)+                               &

           (ATMPO4+ATMLDOP+ATMRDOP)*ART1(SB)*TM1
       END DO
!
!       *** Benthic transfers
!
       TM1=DLT/86400000.
       DO BB=1,MLOC

         TM2 = PNFWS(BB)*ART1(BB)*TM1
         BENFLXPN      = BENFLXPN      + TM2
         BENFLXPNB(BB) = BENFLXPNB(BB) + TM2

         TM2 = (BENNH4(BB)+BENNO3(BB))*ART1(BB)*TM1
         BENFLXDN      = BENFLXDN      + TM2
         BENFLXDNB(BB) = BENFLXDNB(BB) + TM2

         TM2 = PPFWS(BB)*ART1(BB)*TM1
         BENFLXPP      = BENFLXPP      + TM2
         BENFLXPPB(BB) = BENFLXPPB(BB) + TM2

         TM2 = BENPO4(BB)*ART1(BB)*TM1
         BENFLXDP      = BENFLXDP      + TM2
         BENFLXDPB(BB) = BENFLXDPB(BB) + TM2

         TM2 = PCFWS(BB)*ART1(BB)*TM1
         BENFLXPC      = BENFLXPC      + TM2
         BENFLXPCB(BB) = BENFLXPCB(BB) + TM2
       END DO
!
!       *** Change in nutrient mass due to kinetics
!
       TM1=ANDC*DLT/86400000.
       TM2=DLT/86400000.
       DO K=1,KBM1
	 DO I=1,MLOC
           DTMP1 = DENIT(I,K)*ART1(I)*D(I)*DZ(K)*TM1
           DLWCKMNB(I,K) = DLWCKMNB(I,K)-DTMP1
           DLWCKMN = DLWCKMN-DTMP1
           DTMP1 = (DLALGC(I,K)-RESPC(I,K)-MNLLDOC(I,K)-MNLRDOC(I,K)-DENIT(I,K))   &
                   *ART1(I)*D(I)*DZ(K)*TM2
           DLWCKMCB(I,K) = DLWCKMCB(I,K)+DTMP1
           DLWCKMC = DLWCKMC+DTMP1
         END DO  
       END DO  
!
!       *** Boundary fluxes
!
       DO JCB=1,INFLOW
         F=IFLOWP(JCB)
         BNDN = FLXTNH4(F)+FLXTNO3(F)+FLXTLDON(F)+FLXTRDON(F)         &
                +FLXTLPON(F)                                          &
                +FLXTRPON(F)+ANC1*FLXT1(F)+ANC2*FLXT2(F)+ANC3         &
                *FLXT3(F)+ANCSZ*FLXTSZ(F)+ANCLZ*FLXTLZ(F)
         BNDP = FLXTPO4(F)+FLXTLDOP(F)+FLXTRDOP(F)+FLXTLPOP(F)        &
                +FLXTRPOP(F)                                          &
                +APCSZ*FLXTSZ(F)+APCLZ*FLXTLZ(F)                      &
                +FLXTPIB1(F)+FLXTPIB2(F)+FLXTPIB3(F)
         BNDC = FLXTLDOC(F)+FLXTRDOC(F)+FLXTLPOC(F)+FLXTRPOC(F)       &
                +FLXT1(F)+FLXT2(F)+FLXT3(F)+FLXTSZ(F)+FLXTLZ(F)
         IF (LEFT_FLOWB(F)) THEN
           BNDFLXN = BNDFLXN+BNDN
           BNDFLXP = BNDFLXP+BNDP
           BNDFLXC = BNDFLXC+BNDC
         ELSE
           BNDFLXN = BNDFLXN-BNDN
           BNDFLXP = BNDFLXP-BNDP
           BNDFLXC = BNDFLXC-BNDC
         END IF
       END DO
!
!       *** Water column mass balance % error
!
       DLWCSSMN = BNDFLXN+BENFLXPN+BENFLXDN+S1FLXN+S2FLXN+S3FLXN      &
                  +ATMFLXN+DLWCKMN
       DLWCSSMP = BNDFLXP+BENFLXPP+BENFLXDP+S1FLXP+S2FLXP+S3FLXP      &
                  +ATMFLXP
       DLWCSSMC = BNDFLXC+BENFLXPC+S1FLXC+S2FLXC+S3FLXC+DLWCKMC
          
       ERRWCN   = 0.0
       ERRWCP   = 0.0
       ERRWCC   = 0.0
       IF(DLWCMN /= 0.0) ERRWCN = (DLWCMN-DLWCSSMN)/DLWCMN*100.
       IF(DLWCMP /= 0.0) ERRWCP = (DLWCMP-DLWCSSMP)/DLWCMP*100.
       IF(DLWCMC /= 0.0) ERRWCC = (DLWCMC-DLWCSSMC)/DLWCMC*100.
!
!       *** Change in sediment concentration due to burial and kinetics
!
       DTMP1=DLTS/1.E6
       DTMP2=DLTS/1.E3
       DO BB=1,MLOC

         DFTMP=DTMP1*ART1(BB)
         BURIALFLXNB(BB) = BURIALFLXNB(BB)-BURIALN(BB)*DFTMP
         BURIALFLXPB(BB) = BURIALFLXPB(BB)-BURIALP(BB)*DFTMP
         BURIALFLXCB(BB) = BURIALFLXCB(BB)-BURIALC(BB)*DFTMP
         BURIALFLXN      = BURIALFLXN-BURIALN(BB)*DFTMP
         BURIALFLXP      = BURIALFLXP-BURIALP(BB)*DFTMP
         BURIALFLXC      = BURIALFLXC-BURIALC(BB)*DFTMP

         DFTMP=DTMP2*ART1(BB)
         DLSEDKNB(BB)    = DLSEDKNB(BB)-BENDEN(BB)*DFTMP
         DLSEDKCB(BB)    = DLSEDKCB(BB)-DIAGENC(BB)*DFTMP
         DLSEDKN         = DLSEDKN-BENDEN(BB)*DFTMP
         DLSEDKC         = DLSEDKC-DIAGENC(BB)*DFTMP
       END DO

!********* Sediment mass balance

       DLSEDMN   = SEDMN-ISEDMN
       DLSEDMP   = SEDMP-ISEDMP
       DLSEDMC   = SEDMC-ISEDMC
       DLSEDSSMN = BURIALFLXN+DLSEDKN-BENFLXPN-BENFLXDN
       DLSEDSSMP = BURIALFLXP-BENFLXPP-BENFLXDP
       DLSEDSSMC = BURIALFLXC+DLSEDKC-BENFLXPC
!
!         *** Sediment mass balance % error
!
       IF(SEDIMENT_CALC) THEN 
         ERRSEDN = 0.0
         ERRSEDP = 0.0
         ERRSEDC = 0.0
         IF(DLSEDMN /= 0.0) ERRSEDN = (DLSEDMN-DLSEDSSMN)/DLSEDMN*100.
         IF(DLSEDMP /= 0.0) ERRSEDP = (DLSEDMP-DLSEDSSMP)/DLSEDMP*100.
         IF(DLSEDMC /= 0.0) ERRSEDC = (DLSEDMC-DLSEDSSMC)/DLSEDMC*100.
       END IF
     END IF
    
!
!    *** Oxygen days
!
     IF (OXYGEN_PLOTS) THEN
       TM1=DLT/86400.
       DO J=1,NOINT-1
         DO K=1,KBM1
	   DO I=1,MLOC
             IF (DO(I,K) >= OINT(J) .AND. DO(I,K) < OINT(J+1)) THEN
               DOVDAYS(I,K,J) = DOVDAYS(I,K,J)+TM1*ART1(I)*D(I)*DZ(K)
             END IF
           END DO  
	 END DO
       END DO    
       DO K=1,KBM1
	 DO I=1,MLOC
           IF(DO(I,K) >= OINT(NOINT))                          &
	      DOVDAYS(I,K,J)=DOVDAYS(I,K,J)+TM1*ART1(I)*D(I)*DZ(K)
         END DO  
       END DO  
     END IF

     ! </ezpp-user name = "transport">

     C2  = C2F
     ET  = EL
     DT  = D
 
     DO I=1,NGL
       DT1(I)=DT(NV(I,1))+DT(NV(I,2))+DT(NV(I,3))
       DT1(I)=DT1(I)/3.0
     END DO

     T(:,:)    = C2(:,:,1)
     SALT(:,:) = C2(:,:,2)
     SSI(:,:)  = C2(:,:,3)
     B1(:,:)   = C2(:,:,4)
     B2(:,:)   = C2(:,:,5)
     B3(:,:)   = C2(:,:,6)
     SZ(:,:)   = C2(:,:,7)
     LZ(:,:)   = C2(:,:,8)
     LDOC(:,:) = C2(:,:,9)
     RDOC(:,:) = C2(:,:,10)
     LPOC(:,:) = C2(:,:,11)
     RPOC(:,:) = C2(:,:,12)
     NH4(:,:)  = C2(:,:,13)
     NO3(:,:)  = C2(:,:,14)
     UREA(:,:) = C2(:,:,15)
     LDON(:,:) = C2(:,:,16)
     RDON(:,:) = C2(:,:,17)
     LPON(:,:) = C2(:,:,18)
     RPON(:,:) = C2(:,:,19)
     PO4(:,:)  = C2(:,:,20)
     LDOP(:,:) = C2(:,:,21)
     RDOP(:,:) = C2(:,:,22)
     LPOP(:,:) = C2(:,:,23)
     RPOP(:,:) = C2(:,:,24)
     PIP(:,:)  = C2(:,:,25)
     COD(:,:)  = C2(:,:,26)
     DO(:,:)   = C2(:,:,27)
     SU(:,:)   = C2(:,:,28)
     SA(:,:)   = C2(:,:,29)
     PIB1(:,:) = C2(:,:,30)
     PIB2(:,:) = C2(:,:,31)
     PIB3(:,:) = C2(:,:,32)
       if(AMOD((JDAY*86400),7200.0).eq.0) then
       write(2250,*)JDAY,4,MGL,(C2(I,1,27),I=1,MGL), &
                               (C2(I,1,9),I=1,MGL), &
                               (C2(I,1,4),I=1,MGL), &
                               (C2(I,1,13),I=1,MGL), &
                               (C2(I,1,14),I=1,MGL), &
                               (C2(I,1,20),I=1,MGL), &
                               (C2(I,1,1),I=1,MGL),  &
                               (C2(I,1,2),I=1,MGL)
       endif

!************************************************************************
!**                           Output Results                           **
!************************************************************************

!******* Snapshots

     ! <ezpp-user name = "output_results">

     IF (SNAPSHOTS) THEN
       IF (JDAY >= NXSNP .OR. JDAY >= SNPD(SNPDP+1)) THEN
         IF (JDAY >= SNPD(SNPDP+1)) THEN
           SNPDP = SNPDP+1
           NXSNP = SNPD(SNPDP)
         END IF
         NXSNP = NXSNP+SNPF(SNPDP)
         WRITE (SNP,3000)
         WRITE (SNP,3010) (TITLE(K),K=1,6)
         WRITE (SNP,3020) JDAY,INT(ELTMJD),(ELTMJD-INT(ELTMJD))*24.,  &
                          NIT,DLT,DLTAV
         DO JC=1,NAC
           WRITE (SNP,3030) CNAME(AC(JC)),((C1(I,K,AC(JC)),I=1,MLOC),K=1,KBM1)
         END DO  
         IF (SEDIMENT_CALC) THEN
           WRITE (SNP,3034) SSNAME(1),(CTEMP(BB),BB=1,MLOC)
           DO JG=1,3
             WRITE (SNP,3035) JG,SSNAME(2),(CPOP(BB,JG),BB=1,MLOC)
             WRITE (SNP,3035) JG,SSNAME(3),(CPON(BB,JG),BB=1,MLOC)
             WRITE (SNP,3035) JG,SSNAME(4),(CPOC(BB,JG),BB=1,MLOC)
           END DO		
           WRITE (SNP,3034) SSNAME(5), (CPOS(BB),BB=1,MLOC)
           WRITE (SNP,3034) SSNAME(6), (PO4T2TM1S(BB),BB=1,MLOC)
           WRITE (SNP,3034) SSNAME(7), (NH4T2TM1S(BB),BB=1,MLOC)
           WRITE (SNP,3034) SSNAME(8), (NO3T2TM1S(BB),BB=1,MLOC)
           WRITE (SNP,3034) SSNAME(9), (HST2TM1S(BB),BB=1,MLOC)
           WRITE (SNP,3034) SSNAME(10),(CH4T2TM1S(BB),BB=1,MLOC) 
           WRITE (SNP,3034) SSNAME(11),(CH41TM1S(BB),BB=1,MLOC) 
           WRITE (SNP,3034) SSNAME(12),(SO4T2TM1S(BB),BB=1,MLOC)
           WRITE (SNP,3034) SSNAME(13),(SIT2TM1S(BB),BB=1,MLOC)
           WRITE (SNP,3034) SSNAME(14),(BENSTR1S(BB),BB=1,MLOC)
           WRITE (SNP,3034) SSNAME(15),(BBM(BB),BB=1,MLOC) 
         END IF
         IF (DEPFEED)                                             & 
           WRITE (SNP,3034) SSNAME(16),(DFEEDM1S(BB),BB=1,MLOC)
         IF (SFEEDER) THEN
           DO N=1,NSPECIES 
             WRITE (SNP,3036) N,SSNAME(17),(SFEED(BB,N),BB=1,MLOC) 
           END DO
         END IF

         IF (BENTHIC_FLUXES.AND.BENTHIC_OUTPUT) THEN
           WRITE (BFO,3020) JDAY,INT(ELTMJD),                        &
               (ELTMJD-INT(ELTMJD))*24.,NIT,DLT,DLTAV
           WRITE (BFO,3034) 'DOC Flux', (BENDOC(BB), BB=1,MLOC)
           WRITE (BFO,3034) 'Ammonium Flux', (BENNH4(BB),BB=1,MLOC)
           WRITE (BFO,3034) 'Nitrate Flux', (BENNO3(BB),BB=1,MLOC)
           WRITE (BFO,3034) 'Phosphate Flux', (BENPO4(BB),BB=1,MLOC)
           WRITE (BFO,3034) 'COD Flux', (BENCOD(BB), BB=1,MLOC)
           WRITE (BFO,3034) 'CH4 Gas Flux', (BENCH4G(BB), BB=1,MLOC)
           WRITE (BFO,3034) 'CH4 Aq Flux', (BENCH4A(BB), BB=1,MLOC)
           WRITE (BFO,3034) 'SOD', (BENDO(BB),BB=1,MLOC)
           WRITE (BFO,3034) 'Silica Flux', (BENSA(BB), BB=1,MLOC)
         END IF

       END IF
     END IF

!******* Plots

     IF (PLOTS) THEN
       IF (JDAY >= NXPLT .OR. JDAY >= PLTD(PLTDP+1)) THEN
         IF (JDAY >= PLTD(PLTDP+1)) THEN
           PLTDP = PLTDP+1
           NXPLT = PLTD(PLTDP)
         END IF
         NXPLT = NXPLT+PLTF(PLTDP)
         WRITE (PLT,*) JDAY,(((C2(I,K,AC(JC)),I=1,MLOC),K=1,KBM1),JC=1,NAC)
         WRITE (PLT,*) ((CCHL1(I,K),I=1,MLOC),K=1,KBM1),                       &
	             ((CCHL2(I,K),I=1,MLOC),K=1,KBM1),                       &
                     ((CCHL3(I,K),I=1,MLOC),K=1,KBM1)

         IF (QUALITY_DIAG)  THEN
           WRITE (PLT,*) ((FI1(I,K),I=1,MLOC),K=1,KBM1),               &
	               ((NL1(I,K),I=1,MLOC),K=1,KBM1),               &
                       ((PL1(I,K),I=1,MLOC),K=1,KBM1),               &
                       ((SL1(I,K),I=1,MLOC),K=1,KBM1),               &
                       ((FI2(I,K),I=1,MLOC),K=1,KBM1),               &
	               ((NL2(I,K),I=1,MLOC),K=1,KBM1),               &
	               ((PL2(I,K),I=1,MLOC),K=1,KBM1),               &
                       ((SL2(I,K),I=1,MLOC),K=1,KBM1),               &
                       ((FI3(I,K),I=1,MLOC),K=1,KBM1),               &
		       ((NL3(I,K),I=1,MLOC),K=1,KBM1),               &
		       ((PL3(I,K),I=1,MLOC),K=1,KBM1),               &
                       ((SL3(I,K),I=1,MLOC),K=1,KBM1),               &
                       ((NPP(I,K),I=1,MLOC),K=1,KBM1),               &
		       ((GPP(I,K),I=1,MLOC),K=1,KBM1),               &
		       ((RESP(I,K),I=1,MLOC),K=1,KBM1),              &
                       ((KESS(I,K),I=1,MLOC),K=1,KBM1),              &
		       ((CLSZ(I,K),I=1,MLOC),K=1,KBM1),              &
		       ((CLLZ(I,K),I=1,MLOC),K=1,KBM1),              &
                       (ASRAT(I),I=1,MLOC),(CFIX(I),I=1,MLOC),       &
		       (SNFIX(I),I=1,MLOC),(FIB(I),I=1,MLOC),        &
		       (NLB(I),I=1,MLOC),(PLB(I),I=1,MLOC),          &
		       (NPPB(I),I=1,MLOC)
         END IF

         IF (SEDIMENT_DIAG)  THEN
           WRITE (PLT,*) (BENDOC(I),I=1,MLOC),(BENNH4(I),I=1,MLOC),       &
                       (BENNO3(I),I=1,MLOC), (BENPO4(I),I=1,MLOC),      &
                       (BENCOD(I),I=1,MLOC), (BENCH4G(I),I=1,MLOC),     &
                       (BENCH4A(I),I=1,MLOC),(BENDO(I),I=1,MLOC),       &
                       (BENSA(I),I=1,MLOC),  (SSFWS(I),I=1,MLOC),       &
                       (PCFWS(I),I=1,MLOC),  (PNFWS(I),I=1,MLOC),       &
                       (PPFWS(I),I=1,MLOC),  (PSFWS(I),I=1,MLOC),       &
                       ((CPOC(I,J),I=1,MLOC),J=1,3),                    &
                       ((CPON(I,J),I=1,MLOC),J=1,3),                    &
                       ((CPOP(I,J),I=1,MLOC),J=1,3),                    &
                       (CPIP(I),I=1,MLOC),   (CPOS(I),I=1,MLOC),        &
                       (BBM(I),I=1,MLOC),    (CNH4(I),I=1,MLOC),        &
                       (CNO3(I),I=1,MLOC),   (CHS(I),I=1,MLOC),         &
                       (CSI(I),I=1,MLOC),    (DIAGN(I),I=1,MLOC)
         END IF

       END IF
     END IF
    
!
!       *** Oxygen plots
!
     IF (OXYGEN_PLOTS) THEN
       IF (JDAY >= NXOPL .OR. JDAY >= OPLD(OPLDP+1)) THEN
         IF (JDAY >= OPLD(OPLDP+1)) THEN
           OPLDP = OPLDP+1
           NXOPL = OPLD(OPLDP)
         END IF
         NXOPL = NXOPL+OPLF(OPLDP)
         WRITE (OPL) JDAY,DOVDAYS

         DOVDAYS = 0.

       END IF
     END IF

!******* Mass balance

     IF (MASS_BALANCE) THEN
       IF (JDAY >= NXMBL .OR. JDAY >= MBLD(MBLDP+1)) THEN
         IF (JDAY >= MBLD(MBLDP+1)) THEN
           MBLDP = MBLDP+1
           NXMBL = MBLD(MBLDP)
         END IF
         NXMBL   = NXMBL+MBLF(MBLDP)
         ELTMMBL = JDAY-JDAYMBL

!*********** Calculate flux rates

         DLWCKMNB = DLWCKMNB/ELTMMBL
         DLWCKMCB = DLWCKMCB/ELTMMBL
         S1FLXNB  = S1FLXNB/ELTMMBL
         S1FLXPB  = S1FLXPB/ELTMMBL
         S1FLXCB  = S1FLXCB/ELTMMBL
         S2FLXNB  = S2FLXNB/ELTMMBL
         S2FLXPB  = S2FLXPB/ELTMMBL
         S2FLXCB  = S2FLXCB/ELTMMBL
         S3FLXNB  = S3FLXNB/ELTMMBL
         S3FLXPB  = S3FLXPB/ELTMMBL
         S3FLXCB  = S3FLXCB/ELTMMBL

         DLSEDKNB    = DLSEDKNB/ELTMMBL
         DLSEDKCB    = DLSEDKCB/ELTMMBL
         ATMFLXNB    = ATMFLXNB/ELTMMBL
         ATMFLXPB    = ATMFLXPB/ELTMMBL
         BENFLXPNB   = BENFLXPNB/ELTMMBL
         BENFLXDNB   = BENFLXDNB/ELTMMBL
         BENFLXPPB   = BENFLXPPB/ELTMMBL
         BENFLXDPB   = BENFLXDPB/ELTMMBL
         BENFLXPCB   = BENFLXPCB/ELTMMBL
         BURIALFLXNB = BURIALFLXNB/ELTMMBL
         BURIALFLXPB = BURIALFLXPB/ELTMMBL
         BURIALFLXCB = BURIALFLXCB/ELTMMBL

!*********** Output flux rates

         WRITE (MBL) JDAY
         WRITE (MBL) DLWCKMNB,BENFLXPNB,BENFLXDNB,S1FLXNB,S2FLXNB,   &
                     S3FLXNB,ATMFLXNB
         WRITE (MBL) BENFLXPPB,BENFLXDPB,S1FLXPB,S2FLXPB,S3FLXPB,    &
                     ATMFLXPB
         WRITE (MBL) DLWCKMCB,BENFLXPCB,S1FLXCB,S2FLXCB,S3FLXCB
         WRITE (MBL) DLSEDKNB,BURIALFLXNB
         WRITE (MBL) BURIALFLXPB
         WRITE (MBL) DLSEDKCB,BURIALFLXCB

!*********** Reinitialize mass fluxes

         DLWCKMNB = 0.0
         DLWCKMCB = 0.0
         S1FLXNB  = 0.0
         S1FLXPB  = 0.0
         S1FLXCB  = 0.0
         S2FLXNB  = 0.0
         S2FLXPB  = 0.0
         S2FLXCB  = 0.0
         S3FLXNB  = 0.0
         S3FLXPB  = 0.0
         S3FLXCB  = 0.0

         DLSEDKNB    = 0.0
         DLSEDKCB    = 0.0
         ATMFLXNB    = 0.0
         ATMFLXPB    = 0.0
         BENFLXPNB   = 0.0
         BENFLXDNB   = 0.0
         BENFLXPPB   = 0.0
         BENFLXDPB   = 0.0
         BENFLXPCB   = 0.0
         BURIALFLXNB = 0.0
         BURIALFLXPB = 0.0
         BURIALFLXCB = 0.0

         JDAYMBL = JDAY
       END IF
     END IF

!******* Average plots

     IF (AVERAGE_PLOTS) THEN
       IF (JDAY >= NXAPL .OR. JDAY >= APLTD(APLDP+1)) THEN
         IF (JDAY >= APLTD(APLDP+1)) THEN
           APLDP = APLDP+1
           NXAPL = APLTD(APLDP)
         END IF
         NXAPL  = NXAPL+APLF(APLDP)
         AVGINT = ELTMS-ELTMSPLT

         AC1 = AC1/AVGINT

         WRITE (APL) JDAY,(((AC1(I,K,AC(JC)),                          &
               C1MIN(I,K,AC(JC)),C1MAX(I,K,AC(JC)),I=1,MLOC),K=1,KBM1),JC=1,NAC)

         C1MIN = 1.E10
         C1MAX = 0.0
         AC1   = 0.0

         ACCHL1  = ACCHL1/AVGINT
         ACCHL2  = ACCHL2/AVGINT
         ACCHL3  = ACCHL3/AVGINT

         WRITE (APL) ((ACCHL1(I,K),I=1,MLOC),K=1,KBM1),                         &
                     ((ACCHL2(I,K),I=1,MLOC),K=1,KBM1),                         &
                     ((ACCHL3(I,K),I=1,MLOC),K=1,KBM1)

         ACCHL1  = 0.0
         ACCHL2  = 0.0
         ACCHL3  = 0.0

         IF (QUALITY_DIAG) THEN
           AKE   = AKE/AVGINT 
           AFI1  = AFI1/AVGINT
           ANL1  = ANL1/AVGINT
           APL1  = APL1/AVGINT
           ASL1  = ASL1/AVGINT
           AFI2  = AFI2/AVGINT
           ANL2  = ANL2/AVGINT
           APL2  = APL2/AVGINT
           ASL2  = ASL2/AVGINT
           AFI3  = AFI3/AVGINT
           ANL3  = ANL3/AVGINT
           APL3  = APL3/AVGINT
           ASL3  = ASL3/AVGINT
           ANPP  = ANPP/AVGINT
           AGPP  = AGPP/AVGINT
           ARESP = ARESP/AVGINT
           ACLSZ = ACLSZ/AVGINT
           ACLLZ = ACLLZ/AVGINT

           AASRAT= AASRAT/AVGINT
           ACFIX = ACFIX/AVGINT

           AFIB  = AFIB/AVGINT
           ANLB  = ANLB/AVGINT
           APLB  = APLB/AVGINT
           ANPPB = ANPPB/AVGINT

           WRITE (APL) ((AFI1(I,K),I=1,MLOC),K=1,KBM1),                 &
	               ((ANL1(I,K),I=1,MLOC),K=1,KBM1),                 &
                       ((APL1(I,K),I=1,MLOC),K=1,KBM1),                 &
	               ((ASL1(I,K),I=1,MLOC),K=1,KBM1),                 &
                       ((AFI2(I,K),I=1,MLOC),K=1,KBM1),                 &
		       ((ANL2(I,K),I=1,MLOC),K=1,KBM1),                 &
                       ((APL2(I,K),I=1,MLOC),K=1,KBM1),                 &
		       ((ASL2(I,K),I=1,MLOC),K=1,KBM1),                 &
                       ((AFI3(I,K),I=1,MLOC),K=1,KBM1),                 &
		       ((ANL3(I,K),I=1,MLOC),K=1,KBM1),                 &
                       ((APL3(I,K),I=1,MLOC),K=1,KBM1),                 &
	               ((ASL3(I,K),I=1,MLOC),K=1,KBM1),                 &
                       ((ANPP(I,K),I=1,MLOC),K=1,KBM1),                 &
		       ((AGPP(I,K),I=1,MLOC),K=1,KBM1),                 &
                       ((ARESP(I,K),I=1,MLOC),K=1,KBM1),                &
		       ((AKE(I,K),I=1,MLOC),K=1,KBM1),                  &
                       ((ACLSZ(I,K),I=1,MLOC),K=1,KBM1),                &
		       ((ACLLZ(I,K),I=1,MLOC),K=1,KBM1),                &
                       (AASRAT(I),I=1,MLOC),(AFIB(I),I=1,MLOC),         &
                       (ANLB(I),I=1,MLOC),(APLB(I),I=1,MLOC),           &
                       (ANPPB(I),I=1,MLOC)

           AKE   = 0.0 
           AFI1  = 0.0
           ANL1  = 0.0
           APL1  = 0.0
           ASL1  = 0.0
           AFI2  = 0.0
           ANL2  = 0.0
           APL2  = 0.0
           ASL2  = 0.0
           AFI3  = 0.0
           ANL3  = 0.0
           APL3  = 0.0
           ASL3  = 0.0
           ANPP  = 0.0
           AGPP  = 0.0
           ARESP = 0.0
           ACLSZ = 0.0
           ACLLZ = 0.0

           AASRAT= 0.0
           ACFIX = 0.0

           AFIB  = 0.0
           ANLB  = 0.0
           APLB  = 0.0
           ANPPB = 0.0
         END IF

         IF (SEDIMENT_DIAG) THEN
           ACPOS   = ACPOS/AVGINT
           ACPIP   = ACPIP/AVGINT
           ASSFWS  = ASSFWS/AVGINT
           APCFWS  = APCFWS/AVGINT
           APNFWS  = APNFWS/AVGINT
           APPFWS  = APPFWS/AVGINT
           APSFWS  = APSFWS/AVGINT
           ABENDO  = ABENDO/AVGINT
           ABENSA  = ABENSA/AVGINT
           ABENDOC = ABENDOC/AVGINT
           ABENNH4 = ABENNH4/AVGINT
           ABENNO3 = ABENNO3/AVGINT
           ABENPO4 = ABENPO4/AVGINT
           ABENCOD = ABENCOD/AVGINT
           ABENCH4G = ABENCH4G/AVGINT
           ABENCH4A = ABENCH4A/AVGINT
           ABBM    = ABBM/AVGINT
           ABLITE  = ABLITE/AVGINT
           ADFEED  = ADFEED/AVGINT

           ASFEED  = ASFEED/AVGINT
           ASF_SFGC  = ASF_SFGC/AVGINT
           ASF_RESP  = ASF_RESP/AVGINT
           ASF_PRED  = ASF_PRED/AVGINT
           ASF_RMORT = ASF_RMORT/AVGINT

           ADF_GROW = ADF_GROW/AVGINT
           ADF_RESP = ADF_RESP/AVGINT
           ADF_PRED = ADF_PRED/AVGINT
           ADF_MORT = ADF_MORT/AVGINT
           AJNSF = AJNSF/AVGINT
           AJPSF = AJPSF/AVGINT
           ASODSF = ASODSF/AVGINT
           ASASF = ASASF/AVGINT
           ASUSF = ASUSF/AVGINT
           ASFGCIN = ASFGCIN/AVGINT
           ASFCFEC = ASFCFEC/AVGINT
           ASFCPSF = ASFCPSF/AVGINT
           AFLXCSF = AFLXCSF/AVGINT
           AFLXNSF = AFLXNSF/AVGINT
           AFLXPSF = AFLXPSF/AVGINT
           ARPOCSF = ARPOCSF/AVGINT
           ARPONSF = ARPONSF/AVGINT
           ARPOPSF = ARPOPSF/AVGINT
           ASSISF = ASSISF/AVGINT
           ASSISASF = ASSISASF/AVGINT
           ASSISUSF = ASSISUSF/AVGINT
           ASSIPSF = ASSIPSF/AVGINT

           ACPOC = ACPOC/AVGINT
           ACPON = ACPON/AVGINT
           ACPOP = ACPOP/AVGINT

           WRITE (APL) (ABENDOC(B),B=1,MLOC),(ABENNH4(B),B=1,MLOC),     &
                       (ABENNO3(B),B=1,MLOC), (ABENPO4(B),B=1,MLOC),    &
                       (ABENCOD(B),B=1,MLOC),                           &
                       (ABENCH4G(B),B=1,MLOC), (ABENCH4A(B),B=1,MLOC),  &
                       (ABENDO(B),B=1,MLOC),                            &
                       (ABENSA(B),B=1,MLOC),  (ASSFWS(B),B=1,MLOC),     &
                       (APCFWS(B),B=1,MLOC),  (APNFWS(B),B=1,MLOC),     &
                       (APPFWS(B),B=1,MLOC),  (APSFWS(B),B=1,MLOC),     &
                       ((ACPOC(B,J),B=1,MLOC),J=1,3),                   &
                       ((ACPON(B,J),B=1,MLOC),J=1,3),                   &
                       ((ACPOP(B,J),B=1,MLOC),J=1,3),                   &
                       (ACPIP(B),B=1,MLOC),                             &
                       (ACPOS(B),B=1,MLOC),  (ABBM(B),B=1,MLOC),        &
                       (ABLITE(B),B=1,MLOC),  (ADFEED(B),B=1,MLOC),     &
                       ((ASFEED(B,K),B=1,MLOC),K=1,NSSFP)


           IF (SFEEDER) THEN 
             WRITE (SUD) (AJNSF(B),B=1,MLOC),(AJPSF(B),B=1,MLOC)       &
                    ,(ASODSF(B),B=1,MLOC),(ASASF(B),B=1,MLOC)          &
                    ,(ASUSF(B),B=1,MLOC)                               &
                    ,(ASFGCIN(B),B=1,MLOC),(ASFCFEC(B),B=1,MLOC)       &
                    ,(ASFCPSF(B),B=1,MLOC),(AFLXCSF(B),B=1,MLOC)       &
                    ,(AFLXNSF(B),B=1,MLOC),(AFLXPSF(B),B=1,MLOC)       &
                    ,(ARPOCSF(B),B=1,MLOC),(ARPONSF(B),B=1,MLOC)       &
                    ,(ARPOPSF(B),B=1,MLOC),(ASSISF(B),B=1,MLOC)        &
                    ,(ASSISASF(B),B=1,MLOC),(ASSISUSF(B),B=1,MLOC)     &
                    ,(ASSIPSF(B),B=1,MLOC)                             &
                    ,((ASF_SFGC(B,K),B=1,MLOC),K=1,NSSFP)              &
                    ,((ASF_RESP(B,K),B=1,MLOC),K=1,NSSFP)              &
                    ,((ASF_PRED(B,K),B=1,MLOC),K=1,NSSFP)              &
                    ,((ASF_RMORT(B,K),B=1,MLOC),K=1,NSSFP)             &
                    ,(ADF_GROW(B),B=1,MLOC),(ADF_RESP(B),B=1,MLOC)     &
                    ,(ADF_PRED(B),B=1,MLOC),(ADF_MORT(B),B=1,MLOC)
           END IF

           ACPOS   = 0.0
           ACPIP   = 0.0
           ASSFWS  = 0.0
           APCFWS  = 0.0
           APNFWS  = 0.0
           APPFWS  = 0.0
           APSFWS  = 0.0
           ABENDO  = 0.0
           ABENSA  = 0.0
           ABENDOC = 0.0
           ABENNH4 = 0.0
           ABENNO3 = 0.0
           ABENPO4 = 0.0
           ABENCOD = 0.0
           ABENCH4G = 0.0
           ABENCH4A = 0.0
           ABBM    = 0.0
           ABLITE  = 0.0
           ADFEED  = 0.0

           ASFEED = 0.0
           ASF_SFGC  = 0.0
           ASF_RESP  = 0.0
           ASF_PRED  = 0.0
           ASF_RMORT = 0.0

           ADF_GROW = 0.0
           ADF_RESP = 0.0
           ADF_PRED = 0.0
           ADF_MORT = 0.0
           AJNSF = 0.0
           AJPSF = 0.0
           ASODSF = 0.0
           ASASF = 0.0
           ASUSF = 0.0
           ASFGCIN = 0.0
           ASFCFEC = 0.0
           ASFCPSF = 0.0
           AFLXCSF = 0.0
           AFLXNSF = 0.0
           AFLXPSF = 0.0
           ARPOCSF = 0.0
           ARPONSF = 0.0
           ARPOPSF = 0.0
           ASSISF = 0.0
           ASSISASF = 0.0
           ASSISUSF = 0.0
           ASSIPSF = 0.0

           ACPOC = 0.0
           ACPON = 0.0
           ACPOP = 0.0
         END IF

       END IF

!******* Transport fluxes

       IF (TRANSPORT_FLUXES) THEN
         IF (JDAY >= NXTFL .OR. JDAY >= TFLD(TFLDP+1)) THEN
           IF (JDAY >= TFLD(TFLDP+1)) THEN
             TFLDP = TFLDP+1
             NXTFL = TFLD(TFLDP)
           END IF
           NXTFL = NXTFL+TFLF(TFLDP)
           DO JC=1,13
             DO F=1,NQF
               AFLUX(F,JC) = AFLUX(F,JC)/(ELTMS-ELTMSTFL)
	     END DO
	   END DO  	
           WRITE (TFL) JDAY,AFLUX
           DO JC=1,13
             DO F=1,NQF
               AFLUX(F,JC) = 0.0
	     END DO
	   END DO  	
           ELTMSTFL = ELTMS
         END IF
       END IF

!******* Kinetic fluxes

       IF (KINETIC_FLUXES) THEN                           !MNOEL
         IF (JDAY >= NXKFL .OR. JDAY >= KFLD(KFLDP+1)) THEN
           IF (JDAY >= KFLD(KFLDP+1)) THEN
             KFLDP = KFLDP+1
             NXKFL = KFLD(KFLDP)
           END IF
           NXKFL = NXKFL+KFLF(KFLDP)
           AVGINT = ELTMS-ELTMSKFL

           A_T      = A_T/AVGINT
           AP1      = AP1/AVGINT
           ABM1     = ABM1/AVGINT
           APR1     = APR1/AVGINT
           AP2      = AP2/AVGINT
           ABM2     = ABM2/AVGINT
           APR2     = APR2/AVGINT
           AP3      = AP3/AVGINT
           ABM3     = ABM3/AVGINT
           APR3     = APR3/AVGINT
           AALGDOC  = AALGDOC/AVGINT
           AALGPOC  = AALGPOC/AVGINT
           ADENIT   = ADENIT/AVGINT
           AMNLDOC  = AMNLDOC/AVGINT
           AHDRPOC  = AHDRPOC/AVGINT
           AALGNH4  = AALGNH4/AVGINT
           AALGNO3  = AALGNO3/AVGINT
           AALGDON  = AALGDON/AVGINT
           AALGPON  = AALGPON/AVGINT
           ANT      = ANT/AVGINT
           ANFIX    = ANFIX/AVGINT
           ADENNO3  = ADENNO3/AVGINT
           AMNLDON  = AMNLDON/AVGINT
           AHDRPON  = AHDRPON/AVGINT
           AALGPO4  = AALGPO4/AVGINT
           AALGDOP  = AALGDOP/AVGINT
           AALGPOP  = AALGPOP/AVGINT
           AMNLDOP  = AMNLDOP/AVGINT
           AHDRPOP  = AHDRPOP/AVGINT
           APSD     = APSD/AVGINT
           ASAP     = ASAP/AVGINT
           AALGUP   = AALGUP/AVGINT
           AALGRES  = AALGRES/AVGINT
           ADO      = ADO/AVGINT
           ADORALG  = ADORALG/AVGINT
           ADOPR    = ADOPR/AVGINT
           ADCOD    = ADCOD/AVGINT
           ADDOC    = ADDOC/AVGINT
           ANITRIF  = ANITRIF/AVGINT

           ARSZ     = ARSZ   /AVGINT
           ARLZ     = ARLZ   /AVGINT
           ABMSZ    = ABMSZ  /AVGINT
           ABMLZ    = ABMLZ  /AVGINT
           AMSZ     = AMSZ   /AVGINT
           AMLZ     = AMLZ   /AVGINT
           APRSZLZ  = APRSZLZ/AVGINT
           AGSZ     = AGSZ   /AVGINT
           AGLZ     = AGLZ   /AVGINT
           ADOCSZ   = ADOCSZ /AVGINT
           APOCSZ   = APOCSZ /AVGINT
           ADOCLZ   = ADOCLZ /AVGINT
           APOCLZ   = APOCLZ /AVGINT
           ANH4SZ   = ANH4SZ /AVGINT
           ADONSZ   = ADONSZ /AVGINT
           APONSZ   = APONSZ /AVGINT
           ANH4LZ   = ANH4LZ /AVGINT
           ADONLZ   = ADONLZ /AVGINT
           APONLZ   = APONLZ /AVGINT
           APO4SZ   = APO4SZ /AVGINT
           ADOPSZ   = ADOPSZ /AVGINT
           APOPSZ   = APOPSZ /AVGINT
           APO4LZ   = APO4LZ /AVGINT
           ADOPLZ   = ADOPLZ /AVGINT
           APOPLZ   = APOPLZ /AVGINT
           APRSZ    = APRSZ  /AVGINT
           APRLZ    = APRLZ  /AVGINT
	  
           AB1SZ    = AB1SZ/AVGINT
           AB2SZ    = AB2SZ/AVGINT
           AB3SZ    = AB3SZ/AVGINT
           AB1LZ    = AB1LZ/AVGINT
           AB2LZ    = AB2LZ/AVGINT
           AB3LZ    = AB3LZ/AVGINT
           ADOSZ    = ADOSZ/AVGINT
           ADOLZ    = ADOLZ/AVGINT
           ASASZ    = ASASZ/AVGINT
           ASUSZ    = ASUSZ/AVGINT
           ASALZ    = ASALZ/AVGINT
           ASULZ    = ASULZ/AVGINT


           IF (SAV_CALC) THEN

           END IF
                              
           IF (BALGAE_CALC) THEN
             ABMB       = ABMB/AVGINT
             APB        = APB/AVGINT
             APRB       = APRB/AVGINT
             ABADOC     = ABADOC/AVGINT
             ABAPOC     = ABAPOC/AVGINT
             ABANH4     = ABANH4/AVGINT
             ABANO3     = ABANO3/AVGINT
             ABAPON     = ABAPON/AVGINT
             ABAPO4     = ABAPO4/AVGINT
             ABAPOP     = ABAPOP/AVGINT
             ABADO      = ABADO/AVGINT
           END IF                      

           WRITE(KFL) JDAY 
           WRITE(KFL) A_T,      AP1,      ABM1,     APR1,            &
                      AP2,      ABM2,     APR2,     AP3,             &
                      ABM3,     APR3
           WRITE(KFL) AALGDOC,  AALGPOC,  ADENIT,   AMNLDOC,         &
                      AHDRPOC
           WRITE(KFL) AALGNH4,  AALGNO3,  AALGDON,  AALGPON,         &
                      ANT,      ADENNO3,  AMNLDON,  AHDRPON,         &
                      ANFIX
           WRITE(KFL) AALGPO4,  AALGDOP,  AALGPOP,  AMNLDOP,         &
                      AHDRPOP
           WRITE(KFL) APSD,     ASAP,     AALGUP,   AALGRES
           WRITE(KFL) ADO,      ADORALG,  ADOPR,    ADCOD,           &
                      ADDOC,    ANITRIF
           WRITE(KFL) ARSZ,   ARLZ,   ABMSZ,   ABMLZ,                 &
                      AMSZ,   AMLZ,   APRSZLZ, AGSZ,  AGLZ,   ADOCSZ, &
                      APOCSZ, ADOCLZ, APOCLZ,                         &
                      ANH4SZ, ADONSZ, APONSZ,  ANH4LZ,                &
                      ADONLZ, APONLZ, APO4SZ,  ADOPSZ, APOPSZ,        &
                      APO4LZ, ADOPLZ, APOPLZ,                         &
                      APRSZ,  APRLZ
           WRITE(KFL) AB1SZ,  AB2SZ,  AB3SZ,   AB1LZ, AB2LZ,  AB3LZ,  &
                      ADOSZ,  ADOLZ,  ASASZ,   ASUSZ, ASALZ,  ASULZ

           IF (SAV_CALC) THEN

           END IF
           IF (BALGAE_CALC) WRITE(KFL)                                &
                        ABMB,     APB,      APRB,     ABADOC,         &
                        ABAPOC,   ABANH4,   ABANO3,   ABAPON,         &
                        ABAPO4,   ABAPOP,   ABADO

           A_T      = 0.0
           AP1      = 0.0
           ABM1     = 0.0
           APR1     = 0.0
           AP2      = 0.0
           ABM2     = 0.0
           APR2     = 0.0
           AP3      = 0.0
           ABM3     = 0.0
           APR3     = 0.0
           AALGDOC  = 0.0
           AALGPOC  = 0.0
           ADENIT   = 0.0
           AMNLDOC  = 0.0
           AHDRPOC  = 0.0
           AALGNH4  = 0.0
           AALGNO3  = 0.0
           AALGDON  = 0.0
           AALGPON  = 0.0
           ANT      = 0.0
           ANFIX    = 0.0
           ADENNO3  = 0.0
           AMNLDON  = 0.0
           AHDRPON  = 0.0
           AALGPO4  = 0.0
           AALGDOP  = 0.0
           AALGPOP  = 0.0
           AMNLDOP  = 0.0
           AHDRPOP  = 0.0
           APSD     = 0.0
           ASAP     = 0.0
           AALGUP   = 0.0
           AALGRES  = 0.0
           ADO      = 0.0
           ADORALG  = 0.0
           ADOPR    = 0.0
           ADCOD    = 0.0
           ADDOC    = 0.0
           ANITRIF  = 0.0

           ARSZ     = 0.0
           ARLZ     = 0.0
           ABMSZ    = 0.0
           ABMLZ    = 0.0
           AMSZ     = 0.0
           AMLZ     = 0.0
           APRSZLZ  = 0.0
           AGSZ     = 0.0
           AGLZ     = 0.0
           ADOCSZ   = 0.0
           APOCSZ   = 0.0
           ADOCLZ   = 0.0
           APOCLZ   = 0.0
           ANH4SZ   = 0.0
           ADONSZ   = 0.0
           APONSZ   = 0.0
           ANH4LZ   = 0.0
           ADONLZ   = 0.0
           APONLZ   = 0.0
           APO4SZ   = 0.0
           ADOPSZ   = 0.0
           APOPSZ   = 0.0
           APO4LZ   = 0.0
           ADOPLZ   = 0.0
           APOPLZ   = 0.0
           APRSZ    = 0.0
           APRLZ    = 0.0
            
           AB1SZ    = 0.0
           AB2SZ    = 0.0
           AB3SZ    = 0.0
           AB1LZ    = 0.0
           AB2LZ    = 0.0
           AB3LZ    = 0.0
           ADOSZ    = 0.0
           ADOLZ    = 0.0
           ASASZ    = 0.0
           ASUSZ    = 0.0
           ASALZ    = 0.0
           ASULZ    = 0.0

            
           ABMB       = 0.0
           APB        = 0.0
           APRB       = 0.0
           ABADOC     = 0.0
           ABAPOC     = 0.0
           ABANH4     = 0.0
           ABANO3     = 0.0
           ABAPON     = 0.0
           ABAPO4     = 0.0
           ABAPOP     = 0.0
           ABADO      = 0.0
                                 
           ELTMSKFL = ELTMS
         END IF
       END IF

!******* Timestep, volume balance, and mass balance diagnostics

       IF (DIAGNOSTICS) THEN
         IF (JDAY >= NXDIA .OR. JDAY >= DIAD(DIADP+1)) THEN
           IF (JDAY >= DIAD(DIADP+1)) THEN
             DIADP = DIADP+1
             NXDIA = DIAD(DIADP)
           END IF
           NXDIA = NXDIA+DIAF(DIADP)
           WRITE(DIA,4000) INT(JDAY),(JDAY-INT(JDAY))*24.,NIT
           IF (AUTO_STEPPING) THEN
             WRITE(DIA,4010) INT(DLT),INT(DLTAV),COURMX,COURFS,COURBS
             WRITE(DIA,4012) INT(DLT),INT(DLTAV),DIFFMX,DIFFFS,DIFFBS
           END IF
           IF (VOLUME_BALANCE .AND. NEW_VOLUMES) THEN

           END IF
           IF (MASS_BALANCE) THEN
             WRITE (DIA,4050) 'Constituent total mass',(CNAME(AC(JC)),   &
                                CMASS(AC(JC)),JC=1,NAC)
             WRITE (DIA,4060) ERRWCN,ERRWCP,ERRWCC,ERRSEDN,ERRSEDP,      &
                              ERRSEDC
           END IF
         END IF
       END IF

!******* Restarts

       IF (RESTART_OUT) THEN
         IF (JDAY >= RSOD(RSODP) .OR. END_RUN) THEN
           RSODP  = RSODP+1
           LENGTH = INDEX(RSOFN,'.')
           IJDAY  = AINT(JDAY)
           IF (IJDAY < 10) THEN
             WRITE (EXT1,'(I1)') IJDAY
             FILENAME = RSOFN(1:LENGTH)//EXT1
           ELSE IF (IJDAY < 100) THEN
             WRITE (EXT2,'(I2)') IJDAY
             FILENAME = RSOFN(1:LENGTH)//EXT2
           ELSE IF (IJDAY < 1000) THEN
             WRITE (EXT3,'(I3)') IJDAY
             FILENAME = RSOFN(1:LENGTH)//EXT3
           ELSE
             WRITE (EXT4,'(I4)') IJDAY
             FILENAME = RSOFN(1:LENGTH)//EXT4
           END IF
           OPEN  (RSO,FILE=FILENAME,FORM='UNFORMATTED')
            
           WRITE (RSO) TITLE
           WRITE (RSO) (((C1(I,K,J),I=0,MLOC),K=1,KBM1),J=1,NCP)
        
           IF (SEDIMENT_CALC) WRITE (RSO) (CTEMP(B),B=1,MLOC),          &
             ((CPOP(B,J),B=1,MLOC),J=1,3),                              &
             ((CPON(B,J),B=1,MLOC),J=1,3),                              &
             ((CPOC(B,J),B=1,MLOC),J=1,3),                              &
             (CPOS(B),B=1,MLOC),                                        &
             (PO4T2TM1S(B),B=1,MLOC), (NH4T2TM1S(B),B=1,MLOC),           &
             (NO3T2TM1S(B),B=1,MLOC), (HST2TM1S(B),B=1,MLOC),            &
             (CH4T2TM1S(B),B=1,MLOC), (CH41TM1S(B),B=1,MLOC),            &
             (SO4T2TM1S(B),B=1,MLOC), (SIT2TM1S(B),B=1,MLOC),            &
             (BENSTR1S(B),B=1,MLOC),  (BBM(B),B=1,MLOC)

           IF (DEPFEED) WRITE (RSO) (DFEEDM1S(B),B=1,MLOC)
           IF (SFEEDER) WRITE (RSO) ((SFEED(B,K),B=1,MLOC),K=1,NSPECIES)


           CLOSE (RSO)
         END IF
       END IF

      ! </ezpp-user name = "output_results">

      ! </ezpp-user name = "timestep">

     END IF
   END DO   !END DO WHILE(.NOT.END_RUN)

!************************************************************************
!**                          End Simulation                            **
!************************************************************************

!***** Write results in binary form for use as initial conditions

   IF (ICOND_OUT) THEN

!***** Integrate sediments to steady state                               !MNOEL 2-5-93

     IF (STEADY_STATE_SED) CALL SED_INT
        
     OPEN (ICO,FILE=ICOFN,FORM='UNFORMATTED') 
     WRITE (ICO) TITLE
     WRITE (ICO) (((C1(I,K,J),I=0,MLOC),K=1,KBM1),J=1,NCP)
        
     IF (SEDIMENT_CALC) WRITE (ICO) (CTEMP(B),B=1,MLOC),             &
       ((CPOP(B,J),B=1,MLOC),J=1,3),                                 &
       ((CPON(B,J),B=1,MLOC),J=1,3),                                 &
       ((CPOC(B,J),B=1,MLOC),J=1,3),                                 &
       (CPOS(B),B=1,MLOC),                                           &
       (PO4T2TM1S(B),B=1,MLOC), (NH4T2TM1S(B),B=1,MLOC),              &
       (NO3T2TM1S(B),B=1,MLOC), (HST2TM1S(B),B=1,MLOC),               &
       (CH4T2TM1S(B),B=1,MLOC), (CH41TM1S(B),B=1,MLOC),               &
       (SO4T2TM1S(B),B=1,MLOC), (SIT2TM1S(B),B=1,MLOC),               &
       (BENSTR1S(B),B=1,MLOC),  (BBM(B),B=1,MLOC)

     IF (DEPFEED) WRITE (ICO) (DFEEDM1S(B),B=1,MLOC)
     IF (SFEEDER) WRITE (ICO) ((SFEED(B,K),B=1,MLOC),K=1,NSPECIES)


   CLOSE (ICO)
   END IF

!***** Snapshot FORMAT statements

 3000 FORMAT('1')
 3010 FORMAT(1X,A72)
 3020 FORMAT(//                                                        &
             1X,'Time related variables'/'+',22('_')//                 &
             3X,'Julian day',T25,'=',F8.2/                             &
             3X,'Elapsed time',T25,'=',I8,' days ',F6.2,' hours'/      &
             3X,'Number of iterations',T25,'=',I8/                     &
             3X,'Timestep',T25,'=',F8.0,' sec'/                        &
             3X,'Average time step',T25,'=',F8.0)
 3025 FORMAT(//                                                          &
             1X,'After steady-state sediment computation'/'+',22('_')//  &
             3X,'Julian day',T25,'=',F8.2/                               &
             3X,'Elapsed time',T25,'=',I8,' days ',F6.2,' hours'/        &
             3X,'Number of iterations',T25,'=',I8/                       &
             3X,'Timestep',T25,'=',F8.0,' sec'/                          &
             3X,'Average time step',T25,'=',F8.0)  
 3030 FORMAT(/1X,A24/10(/10F8.4))
 3032 FORMAT(/1X,A20/10(/10F8.3))
 3034 FORMAT(/1X,A20/10(/10(1PE8.2E1)))
 3035 FORMAT(/1X,'G',I1,1X,A20/10(/10(1PE8.2E1)))
 3036 FORMAT(/1X,'N',I1,1X,A20/10(/10(1PE8.2E1)))

!***** Diagnostic FORMAT statements

 4000 FORMAT(/                                                        &
             1X,79('*')//                                             &
             1X,'Date         =',I8,'  days ',F5.2,' hours'/          &
             1X,'# iterations =',I8)
 4010 FORMAT(1X,'Timestep',T15,'=',I8,2X,'Average timestep',T43,      &
               '=',I8/                                                &
             1X,'Courant #',T15,'=',F8.3/                             &
             1X,'at face',T15,'=',I8,2X,'and box',T43,'=',I8/)
 4012 FORMAT(1X,'Timestep',T15,'=',I8,2X,'Average timestep',T43,      &
               '=',I8/                                                &
             1X,'Diffusion #',T15,'=',F8.3/                           &
             1X,'at face',T15,'=',I8,2X,'and box',T43,'=',I8/)
 4020 FORMAT(/                                                        &
             1X,A14,' performed at ',I6,' days and ',F5.2,' hours')
 4030 FORMAT(5X,'V2(',I4,') =',1PE15.6E1,' m*3',3X,'HMV(',I4,') =',   &
               1PE15.6E1,' m*3',3X,'Difference =',1PE15.6E1,' m*3')
 4040 FORMAT(/                                                          &
             3X,'Water quality model volume      =',1PE20.12E2,' m*3'/  &
             3X,'Difference in total grid volume =',1PE20.3E2,' %'/     &
             3X,'Total difference in volume      =',1PE20.3E2,' m*3')
 4050 FORMAT(/                                                          &
             1X,A22//                                                   &
             (:3X,A24,T31,'=',1PE16.8E2,' kg'))
 4060 FORMAT(/                                                          &
             1X,'Water column mass balance error'//                     &
             3X,'Nitrogen   = ',1PE10.3E2,' %'/                         &
             3X,'Phosphorus = ',1PE10.3E2,' %'/                         &
             3X,'Carbon     = ',1PE10.3E2,' %'//                        &
             1X,'Sediment mass balance error'//                         &
             3X,'Nitrogen   = ',1PE10.3E2,' %'/                         &
             3X,'Phosphorus = ',1PE10.3E2,' %'/                         &
             3X,'Carbon     = ',1PE10.3E2,' %'/)
 4070 FORMAT(9F8.2)
 4080 FORMAT(8X,A24)

!***** Mass balance FORMAT's

 5000 FORMAT(10(1X,1PE10.2E2))

   STOP
   END PROGRAM PARWQM

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

   SUBROUTINE EXCHANGE_WQM
!==============================================================================!
!     PERFORM DATA EXCHANGE FOR WATER QUALITY VARIABLES                        |
!==============================================================================!
#  if defined (MULTIPROCESSOR)
   USE MOD_PAR
#  endif      
   USE WQM
   USE LIMS
   USE CONTROL
   IMPLICIT NONE
   INTEGER :: NJ
                                                                                                                         
#  if defined (MULTIPROCESSOR)
   IF(PAR)THEN
     DO NJ = 1,NCP
       CALL EXCHANGE(NC,MTLOC,KB,MYID,NPROCS,C2F(:,:,NJ))
     END DO
   END IF
#  endif
   RETURN
   END SUBROUTINE EXCHANGE_WQM
!==============================================================================!

