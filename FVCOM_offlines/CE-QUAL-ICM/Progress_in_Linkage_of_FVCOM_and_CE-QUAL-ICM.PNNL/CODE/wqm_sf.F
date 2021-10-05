!************************************************************************
!**               S U B R O U T I N E   S U S P F E E D                **
!************************************************************************
!**                                                                    **
!**         Chesapeake Bay Suspension Feeder Model                     **
!**                                                                    **
!**      -- HydroQual, Inc.                                            **
!**         Latest Version: 18 Apr 97                                  **
!**                                                                    **
!**         New hypoxic mortality - SAL 4.16.97                        **
!**            (note: outputs for this not included!)                  **
!**                                                                    **
!************************************************************************

   SUBROUTINE SUSPFEED(N, DTB1, DTB2, DTB3, DTPIB1, DTPIB2, DTPIB3,  &
        DTLPOC,DTLPOP,DTLPON,DTRPOC,DTRPOP,DTRPON,DTSSI,DTSU,DTSA,DTPO4)
   USE WQM
   USE ALL_VARS

   IMPLICIT NONE
! THESE ARE LOCAL WORK ARRAYS (end in X)
   REAL,DIMENSION(MLOC) :: SF,JNH4SFX,JPO4SFX,SODSFX,JSASFX,JSUSFX,SEDTYPEX,   &
                        SFLUXCX,SFLUXNX,SFLUXPX,SF_RPOCX,SF_RPONX,SF_RPOPX, & 
                        SF_SUX,SF_SASX,SF_PIPX,SF_SSIX
   REAL :: MAXINGX
   REAL :: LOGICT,CLOGI,LPOCASF,LPONASF,LPOPASF
   REAL :: NFECES, NFILT, NPSFEC
   INTEGER :: I,N
   REAL :: FILTX, SFA1X, SFA2X, SFA3X, SFA4X, SFA5X, SFCNX, SFCPX, SFRESPX, &
           SFPREDX, SFTMNX, THTAFILTX, THTARESPX, THTAPREDX, XKPO2X, TDX,   &
	   RESPFACTX, FILTFACTX, DOhx, DOqx, ATURB, BTURB, RD, RMORT,       &
	   B1ASF, B2ASF, B3ASF, RPOCASF, RPONASF,  RPOPASF
   REAL :: SSIASF, SUASF, FILTCT, RESPCT, PREDCT,    &
           RATEI, GCMAX, SFGC, GMIN,CFECES, RCFECES, RNFECES, PFECES,       &
	   RPFECES, CFILT, PFILT, CPSFEC, RCPSFEC, RNPSFEC, PPSFEC, RPPSFEC, &
	   DOX, SFD, TSOLID, TURBRED, RELING, GN, GP, SFAC1, SFAC2, SFAC3,  &
	   SFAC4, SFAC5, SFAN1, SFAN2, SFAN3, SFAN4, SFAN5, SFAP1, SFAP2,   &
	   SFAP3, SFAP4, SFAP5, PF, DTDAY 	    

   REAL,DIMENSION(MTLOC,KBM1) :: DTB1, DTB2, DTB3, DTPIB1, DTPIB2, DTPIB3,  &
        DTLPOC,DTLPOP,DTLPON,DTRPOC,DTRPOP,DTRPON,DTSSI,DTSU,DTSA,DTPO4

! Keep track of delta_time, in days
   DLTS=DLT/86400.

! firstly set up calculation for that species type

   DO I=1,MLOC
     SF(I)=SFEED(I,N)
     SEDTYPEX(I)=SEDTYPE(I,N)

!MBM 961226 diagnostic arrays reset everywhere, every time step
     SFFILTCT(I,N) = 0.
     SFRESPCT(I,N) = 0.
     SFPREDCT(I,N) = 0.
     SFRATEI(I,N)  = 0.
     SFGCMAX(I,N)  = 0.
     SFGMIN(I,N)   = 0.
     SFCFECES(I,N) = 0.
     SFNFECES(I,N) = 0.
     SFPFECES(I,N) = 0.
     SFCPSFEC(I,N) = 0.
     SFNPSFEC(I,N) = 0.
     SFPPSFEC(I,N) = 0.
     SF_SFGC(I,N)  = 0.
     SF_RESP(I,N)  = 0.
     SF_PRED(I,N)  = 0.
     SF_RMORT(I,N) = 0.

   END DO

   FILTX=FILT(N)
   SFA1X=SFA1(N)
   SFA2X=SFA2(N)
   SFA3X=SFA3(N)
   SFA4X=SFA4(N)
   SFA5X=SFA5(N)
   MAXINGX=MAXING(N)
   SFCNX=SFCN(N)
   SFCPX=SFCP(N)
   SFRESPX=SFRESP(N)
   SFPREDX=SFPRED(N)
   SFTMNX=SFTMN(N)
   THTAFILTX=THTAFILT(N)
   THTARESPX=THTARESP(N)
   THTAPREDX=THTAPRED(N)
   XKPO2X=XKPO2(N)
   TDX=SFTD(N)
   RESPFACTX=RESPFACT(N)
   FILTFACTX=FILTFACT(N)
   DOhx=SFDOh(N)
   DOqx=SFDOq(N)
   ATURB=SFATURB(N)
   BTURB=SFBTURB(N)

! do some initial calcs for low o2 respiration stress

   RD=4.605/TDX          ! ln(1/100)=4.6=99% mortality

! RMORT is the mortality rate (1/day) resulting from hypoxia
! initialize here; computed from RD and a DO-depdt function below.
! MBM 970928 T-dependence on Rd added below!
 
   RMORT=0.0

! reset local flux contribution to zero at start of each timestep as 
! each species contribution is added to total at bottom

   DO I=1,MLOC  
     JNH4SFX(I)=0.0
     JPO4SFX(I)=0.0
     SODSFX(I)=0.0
     JSASFX(I)=0.0
     JSUSFX(I)=0.0
     SFLUXCX(I)=0.0
     SFLUXNX(I)=0.0
     SFLUXPX(I)=0.0
     SF_RPOCX(I)=0.0
     SF_RPONX(I)=0.0
     SF_RPOPX(I)=0.0
     SF_SSIX(I)=0.0
     SF_SUX(I)=0.0
     SF_SASX(I)=0.0
     SF_PIPX(I)=0.0
   END DO

! Re-init global particulate fluxes 
!  labile (SFLUX{C,N,P}), refract, & unavail Si
! Only re-initialize prior to call for first species (N=1)...
   IF(N == 1)THEN
     DO I=1,MLOC
       JNH4SF(I)=0.0
       JPO4SF(I)=0.0
       SODSF(I)=0.0
       JSASF(I)=0.0
       JSUSF(I)=0.0
       SFLUXC(I)=0.0
       SFLUXN(I)=0.0
       SFLUXP(I)=0.0
       SF_RPOC(I)=0.0
       SF_RPON(I)=0.0
       SF_RPOP(I)=0.0
       SF_SSI(I)=0.0
       SF_SU(I)=0.0
       SF_SA(I)=0.0
       SF_PIP(I)=0.0
     END DO
   END IF

! main loop

   DO I=1,MLOC
!     IWC=BBN(I)

! COMPUTE FOOD,PARTICULATES AVAILABLE TO SUSPENSION FEEDERS

     B1ASF = MAX(1.E-6,B1(I,KBM1))          
     B2ASF = MAX(1.E-6,B2(I,KBM1))          
     B3ASF = MAX(1.E-6,B3(I,KBM1))
     LPOCASF = MAX(1.E-6,LPOC(I,KBM1))          
     RPOCASF = MAX(1.E-6,RPOC(I,KBM1))
     LPONASF = MAX(1.E-6,LPON(I,KBM1))          
     RPONASF = MAX(1.E-6,RPON(I,KBM1))
     LPOPASF = MAX(1.E-6,LPOP(I,KBM1))          
     RPOPASF = MAX(1.E-6,RPOP(I,KBM1))
     SSIASF = MAX(1.E-6,SSI(I,KBM1))          
     SUASF = MAX(1.E-6,SU(I,KBM1))

     FILTCT=0.0
     RESPCT=0.0
     PREDCT=0.0
     RMORT=0.0                    !MBM 972830
     RATEI=0.0
     GCMAX=0.0
     SFGC=0.0
     GMIN=0.0
     CFECES=0.0
     RCFECES=0.0
     NFECES=0.0
     RNFECES=0.0
     PFECES=0.0
     RPFECES=0.0
     CFILT=0.0
     NFILT=0.0
     PFILT=0.0
     CPSFEC=0.0
     RCPSFEC=0.0
     NPSFEC=0.0
     RNPSFEC=0.0
     PPSFEC=0.0
     RPPSFEC=0.0

     IF(CTEMP(I) < SFTMNX) GOTO 255

     IF(DO(I,KBM1) < 0.0)THEN
       DOX=0.0
     ELSE
       DOX=DO(I,KBM1)
     END IF

! take into account habitat type

     IF(SEDTYPEX(I) == 0)THEN
       SF(I)=0.1                 !  10 mgC/m2 "refuge" population
       GOTO 255
     END IF

! calculate biomass dependent respiration and filtration

     SFD=SF(I)/1000.    ! convert to g C /m^2 for this calc.
!     IF(SFD.LE.0.0)THEN   ! MBM 970107 Present scheme maintains SF > 0.
!       RESPCT=0.0
!       FILTCT=0.0
!     ELSE
     RESPCT=SFRESPX/SFD**RESPFACTX
     FILTCT=FILTX/SFD**FILTFACTX
!     ENDIF

! Evaluate temperature dependent terms
! Bivalve responses to temperature ranging to 30C are
!    Arrhenius-type responses: rate(T) = rate(Tref) * theta^(T-Tref)
!    The reference temperature is 20.0C.

     FILTCT=FILTCT*THTAFILTX**(CTEMP(I)-20.0)
     RESPCT=RESPCT*THTARESPX**(CTEMP(I)-20.0)
     PREDCT=SFPREDX*THTAPREDX**(CTEMP(I)-20.0)

! MBM 970928 Temperature dependence on Time-to-Death, DO-mortality rate
!     RD = RD*(1.08**(CTEMP(I)-20.0))

! O2 limitations on filtration, predation and respiration

! MBM 961224 added logistical control on filtr, hypoxic resp fcts
!   logict ranges 0.0-1.0 with DOX; use 1-logict for increased mort/resp.
!   MAX() prevents underflows in the event of superhigh btm DO.

!  One HARDWIRED constant!
     Clogi=1.1

!MBM 970109  hypoxfx switch added
     if ( HYPOXFX ) then

       LOGICT=1.0/( 1.0 + EXP(MAX(Clogi*(DOhx-DOX)/(DOhx-DOqx),-25.)) )

! MBM 961224 added logistical control on filtr, hypoxic resp fcts
       FILTCT=FILTCT*LOGICT
! MBM 970612 added logistical control on resp to simulate
! MBM        greatly reduced (anaerobic) respiration rates as FILT -> 0
! MBM        Relieves SOD from SODSF component when SF are not using oxygen.
       RESPCT=RESPCT*LOGICT
       PREDCT=PREDCT*DOX/(DOX+XKPO2X)

! MBM 961224 added logistical control on filtr, hypoxic resp fcts
! SAL 970416 now use RMORT instead of respiration
! MBM 970928 Re-establishing logistic function for hypoxic mortality
       RMORT=RD*(1.0-LOGICT)
! MBM 970429 New mortality formula from analysis of benthic anoxia LD50 data
!     RMORT = 0.04773*exp( 0.05319*CTEMP(B) - 0.69538*DOX )

     END IF

! MBM 971021
! Total solids (mg DW/L)

     TSOLID = 2.5*(B1ASF+B2ASF+B3ASF+LPOCASF+RPOCASF) + SSIASF

! MBM 971021
! Reduction in filtration from total particulate load from
!   PHK oyster model; solids as ** gm/L **; red as % reduction of FILT
!   Same function, values as in PHK, but has been algebraically modified
!   to be more comprehensible (originally = [log(turb) + 3.38] / 0.0418).

!     TURBRED = 81. + 24. * LOG10( 0.001*TSOLID )
     TURBRED = MAX(ATURB + BTURB * LOG10( 0.001*TSOLID ),0.0)
     FILTCT = MAX(FILTCT * (1.0 - 0.01*TURBRED),0.0)


! CARBON INGESTION RATE
!   FILTCT - REALIZED FILTRATION RATE LITERS/D/MG C BIOMASS

     RATEI = FILTCT * ( B1ASF + B2ASF + B3ASF + LPOCASF + RPOCASF )

!     COMPUTE SUSPENSION FEEDER GROWTH RATE, COMPARING TO MAX
!     INGESTION RATE - MAX AMOUNT OF FOOD GUT CAN TAKE IN
!        if ratei < rateimax  then use normal growth rate
!        if ratei > rateimax  then limit to max rate

     RELING = MIN(1.0,MAXINGX/(RATEI+1.0E-10))

! Total Filtered from Organic Matter Pools    (MBM 971010 moved up from below)
     CFILT = SF(I)*RATEI

     NFILT = SF(I)*FILTCT*(B1ASF*ANC1+B2ASF*ANC2+B3ASF*ANC3+LPONASF+RPONASF)
     PFILT = SF(I)*FILTCT*(B1ASF*Q1(I,KBM1)+B2ASF*Q2(I,KBM1)+B3ASF*Q3(I,KBM1)         &
             +LPOPASF+RPOPASF)

! Maximum potential ASSIMILATED carbon
!    sfa1 - assimilation rate cyanobacteria
!    sfa2 - assimilation rate diatoms
!    sfa3 - assimilation rate greens
!    sfa4 - assimilation rate lpoc
!    sfa5 - assimilation rate rpoc

     GCMAX = SF(I)*FILTCT * ( SFA1X*B1ASF + SFA2X*B2ASF                     &
             + SFA3X*B3ASF + SFA4X*LPOCASF + SFA5X*RPOCASF) 

! Limit carbon growth to a max. theoret. value (mg C/m2.d)
     SFGC = RELING*GCMAX

!     NOW CHECK TO SEE IF THERE IS A NUTRIENT LIMITATION ON GROWTH

!        COMPUTE GROWTH RATE FOR NITROGEN

     GN = SF(I)*FILTCT*(SFA1X*B1ASF*ANC1+SFA2X*B2ASF*ANC2+SFA3X*B3ASF*ANC3  &
          +SFA4X*LPONASF+SFA5X*RPONASF)

!        COMPUTE GROWTH RATE FOR PHOSPHORUS

     GP = SF(I)*FILTCT*(SFA1X*B1ASF*Q1(I,KBM1)+SFA2X*B2ASF*Q2(I,KBM1)             &
          +SFA3X*B3ASF*Q3(I,KBM1)+SFA4X*LPOPASF+SFA5X*RPOPASF) 

!        REQUIREMENTS FOR C, N AND P ARE DIFFERENT, SEE WHICH ONE IS 
!        LIMITING BY USING ANIMAL STOICHIOMETRY
!          SFCN - C/N RATIO   SFCP - C/P RATIO
!        CONVERT TO EQUIVALENT CARBON UNITS TO PERMIT DIRECT COMPARISON

     GN=GN*SFCNX    
     GP=GP*SFCPX
     GMIN=MIN(GN,GP)

!        COMPARE TO CARBON GROWTH RATE - IF NUTRIENT LIMITED
!        THEN RECOMPUTE CARBON ASSIMILATION EFFICIENCIES USING
!        LIMITATION.  THIS HAS THE EFFECT OF LIMITING THE UPTAKE
!        OF CARBON TO MATCH ANIMAL STOICHIOMETRY

     IF(GMIN < SFGC)THEN
       SFAC1=SFA1X*GMIN/SFGC
       SFAC2=SFA2X*GMIN/SFGC
       SFAC3=SFA3X*GMIN/SFGC
       SFAC4=SFA4X*GMIN/SFGC
       SFAC5=SFA5X*GMIN/SFGC
       SFGC=GMIN
     ELSE
       SFAC1=SFA1X
       SFAC2=SFA2X
       SFAC3=SFA3X
       SFAC4=SFA4X
       SFAC5=SFA5X
     ENDIF

!        KNOWING HOW MUCH CARBON WAS TAKEN UP, COMPUTE N AND P UPTAKE
!        KNOWING HOW MUCH N AND P WAS FILTERED, COMPUTE HOW MUCH GOES
!        TO FECES AND PSEUDOFECES

!         NITROGEN ASSIMILATION RATES
     IF(GN > SFGC)THEN
       SFAN1=SFA1X*SFGC/GN        
       SFAN2=SFA2X*SFGC/GN
       SFAN3=SFA3X*SFGC/GN
       SFAN4=SFA4X*SFGC/GN
       SFAN5=SFA5X*SFGC/GN
     ELSE
       SFAN1=SFA1X      
       SFAN2=SFA2X
       SFAN3=SFA3X
       SFAN4=SFA4X
       SFAN5=SFA5X
     ENDIF

!        PHOSPHORUS ASSIMILATION RATES
     IF(GP > SFGC)THEN
       SFAP1=SFA1X*SFGC/GP        
       SFAP2=SFA2X*SFGC/GP
       SFAP3=SFA3X*SFGC/GP
       SFAP4=SFA4X*SFGC/GP
       SFAP5=SFA5X*SFGC/GP
     ELSE
       SFAP1=SFA1X      
       SFAP2=SFA2X
       SFAP3=SFA3X
       SFAP4=SFA4X
       SFAP5=SFA5X
     ENDIF

!        C FECES - C NOT ASSILMIATED     mg / m2 /d
! MBM 970930 Refract feces added
     CFECES = SF(I)*FILTCT*RELING*((1.-SFAC1)*B1ASF+(1.-SFAC2)*B2ASF       &
              +(1.-SFAC3)*B3ASF+(1.-SFAC4)*LPOCASF )
!MBM .       +  (1.-SFAC5)*RPOCASF ) 

     RCFECES = SF(I)*FILTCT*RELING*(1.-SFAC5)*RPOCASF

!        N FECES - N NOT ASSILMIATED
     NFECES = SF(I)*FILTCT*RELING*((1.-SFAN1)*B1ASF*ANC1                   &
              +(1.-SFAN2)*B2ASF*ANC2+(1.-SFAN3)*B3ASF*ANC3                 &
              +(1.-SFAN4)*LPONASF)
!MBM .       + (1.-SFAN5)*RPONASF ) 

     RNFECES = SF(I)*FILTCT*RELING * (1.-SFAN5)*RPONASF

!        P FECES - P NOT ASSILMIATED
     PFECES = SF(I)*FILTCT*RELING*((1.-SFAP1)*B1ASF*Q1(I,KBM1)             &
              +(1.-SFAP2)*B2ASF*Q2(I,KBM1)+(1.-SFAP3)*B3ASF*Q3(I,KBM1)     &
              +(1.-SFAP4)*LPOPASF)
!MBM .       + (1.-SFAP5)*RPOPASF ) 

     RPFECES = SF(I)*FILTCT*RELING * (1.-SFAP5)*RPOPASF

!        KNOWING THE TOTAL AMOUNT OF C,N,P FILTERED AND KNOWING HOW
!        MUCH WENT TO GROWTH AND FECES, THEN CALCULATE HOW MUCH ENDS
!        UP AS PSEUDOFECES
!        PSEUDOFECES = TOTAL FILTERED - GROWTH - FECES

!        PSEUDOFECES
! MBM 970930 Bookkeeping for Refract pseudofeces added

     CPSFEC=MAX(0.0,CFILT-SFGC-CFECES-RCFECES)
     RCPSFEC=CPSFEC * RPOCASF/(B1ASF+B2ASF+B3ASF+LPOCASF+RPOCASF)
     CPSFEC=CPSFEC-RCPSFEC

     NPSFEC=MAX(0.0,NFILT-SFGC/SFCNX-NFECES-RNFECES)
     RNPSFEC=NPSFEC*RPONASF/(ANC1*B1ASF+ANC2*B2ASF+ANC3*B3ASF+LPONASF+RPONASF)
     NPSFEC=NPSFEC-RNPSFEC

     PPSFEC=MAX(0.0,PFILT-SFGC/SFCPX-PFECES-RPFECES)
     RPPSFEC=PPSFEC*RPOPASF                                               &
             /(Q1(I,KBM1)*B1ASF+Q2(I,KBM1)*B2ASF+Q3(I,KBM1)*B3ASF+LPOPASF+RPOPASF)
     PPSFEC=PPSFEC-RPPSFEC

!        APPORTION FECES TO DEPOSITIONAL FLUXES.  FECES ARE PRODUCED
!        FROM FOOD TAKEN IN BUT NOT ASSIMILATED.  (NOTE: HERE THE
!        NUTRIENT LIMITED ASSIMILATION RATES (IF APPROPRIATE) ARE USED 
!        ALSO ASSUME THAT PREDATION LOSSES END UP IN LABILE POC,PON,POP POOLS

! Susp fdr particulate fluxes (local) MG/M2/DAY

! MBM 970930 --> Error in RPO{C,N,P} logic below since RPOM is CONSUMED with 
!            Assim Effy SFA5. Flux ALREADY accounted for in Feces, 
!            Pseudofeces. New logic for RFeces, RPSFec added.
!            Prev. error resulted in 1) double-acctg for RPOM (SF) flux,
!            2) Converting Jpom_ref --> labile splits (90%) in sed module
!        NOW: Labile and refractory conponents of feces are separated.

! LPOC
     SFLUXCX(I) =  CFECES + CPSFEC + (PREDCT*SF(I)+RMORT)*SF(I)

! LPON
     SFLUXNX(I) =  NFECES + NPSFEC + (PREDCT*SF(I)+RMORT)*SF(I)/SFCNX

! LPOP
     SFLUXPX(I) =  PFECES + PPSFEC + (PREDCT*SF(I)+RMORT)*SF(I)/SFCPX

! RPOC
     SF_RPOCX(I) = RCFECES + RCPSFEC
! RPON
     SF_RPONX(I) = RNFECES + RNPSFEC
! RPOP
     SF_RPOPX(I) = RPFECES + RPPSFEC

! Particulate, unavailable Si 
     SF_SUX(I) =   FILTCT*SF(I)*SUASF
! Inorganic solids
     SF_SSIX(I) =  FILTCT*SF(I)*SSIASF*WSSNET(I)/WSS(I,1)
! P sorbed onto SSI (PIP)
     PF = KADPO4*SSI(I,KBM1)/(1.+KADPO4*SSI(I,KBM1))
     SF_PIPX(I) =  FILTCT*SF(I)*WSSNET(I)/WSS(I,1)*PF*MAX(0.,PO4(I,KBM1))
! Si sorbed onto SSI
     PF =          KADPO4*SSI(I,KBM1)/(1.+KADPO4*SSI(I,KBM1))
     SF_SASX(I) =  FILTCT*SF(I)*PF*SA(I,KBM1)*WSSNET(I)/WSS(I,1)

!        CORRECT DERIVATIVES FOR LOSSES
!        NOTE: TOTAL LOSS IS TOTAL FILTERING RATE
! note: water quality derivative units are G/M3/S
!       therefore divide by 1000. to get g from mg
!       and divide by 86400. to get from days to seconds
!       BL(IWC,3) is the deltaZ of the bottom layer wc box in meters

     DTB1(I,KBM1) = DTB1(I,KBM1) - ( FILTCT*SF(I)*B1ASF )               &
                 /(D(I)*DZ(KBM1)) /1000. /86400.           ! units: g/m3/s

     DTB2(I,KBM1) = DTB2(I,KBM1) - ( FILTCT*SF(I)*B2ASF )                     &
                 /(D(I)*DZ(KBM1)) /1000. /86400.

     DTB3(I,KBM1) = DTB3(I,KBM1) - ( FILTCT*SF(I)*B3ASF )                     &
                 /(D(I)*DZ(KBM1)) /1000. /86400.

     DTPIB1(I,KBM1) = DTPIB1(I,KBM1) - Q1(I,KBM1)*( FILTCT*SF(I)*B1ASF )           &
                   /(D(I)*DZ(KBM1)) /1000. /86400.           ! units: g/m3/s

     DTPIB2(I,KBM1) = DTPIB2(I,KBM1) - Q2(I,KBM1)*( FILTCT*SF(I)*B2ASF )           &
                   /(D(I)*DZ(KBM1)) /1000. /86400.

     DTPIB3(I,KBM1) = DTPIB3(I,KBM1) - Q3(I,KBM1)*( FILTCT*SF(I)*B3ASF )           &  
                   /(D(I)*DZ(KBM1)) /1000. /86400.

     DTLPOC(I,KBM1) = DTLPOC(I,KBM1) - ( FILTCT*SF(I)*LPOCASF )               &
                   /(D(I)*DZ(KBM1)) /1000. /86400.

     DTLPOP(I,KBM1) = DTLPOP(I,KBM1) - ( FILTCT*SF(I)*LPOPASF )               &
                   /(D(I)*DZ(KBM1)) /1000. /86400.

     DTLPON(I,KBM1) = DTLPON(I,KBM1) - ( FILTCT*SF(I)*LPONASF )               &
                   /(D(I)*DZ(KBM1)) /1000. /86400.

     DTRPOC(I,KBM1) = DTRPOC(I,KBM1) - ( FILTCT*SF(I)*RPOCASF )               &
                   /(D(I)*DZ(KBM1)) /1000. /86400.

     DTRPOP(I,KBM1) = DTRPOP(I,KBM1) - ( FILTCT*SF(I)*RPOPASF )               &
                   /(D(I)*DZ(KBM1)) /1000. /86400.

     DTRPON(I,KBM1) = DTRPON(I,KBM1) - ( FILTCT*SF(I)*RPONASF )               &
                   /(D(I)*DZ(KBM1)) /1000. /86400.

     DTSSI(I,KBM1) = DTSSI(I,KBM1) - SF_SSIX(I)                               &
                  /(D(I)*DZ(KBM1)) /1000. /86400.

     DTSU(I,KBM1) = DTSU(I,KBM1) - SF_SUX(I)                                  &
                  /(D(I)*DZ(KBM1)) /1000. /86400.

     DTSA(I,KBM1) = DTSA(I,KBM1) - SF_SASX(I)                                 &
                 /(D(I)*DZ(KBM1)) /1000. /86400.

     DTPO4(I,KBM1) = DTPO4(I,KBM1)                                            &
                  + SF_PIPX(I) /(D(I)*DZ(KBM1)) /1000. /86400.

! calculate respiration fluxes

     JNH4SFX(I)=RESPCT*SF(I)/SFCNX               ! units: mg/m2/d
     JPO4SFX(I)=RESPCT*SF(I)/SFCPX 
     SODSFX(I)=RESPCT*SF(I)*2.67/1000.

! Si recycled from diatom consumption             [mg Si / m2 / day]
     JSASFX(I) = FILTCT*SF(I)*(B1ASF*ASC1+B2ASF*ASC2+B3ASF*ASC3)*FRSASF    !  CFC
     JSUSFX(I) = FILTCT*SF(I)*(B1ASF*ASC1+B2ASF*ASC2+B3ASF*ASC3)*(1.-FRSASF)  !  CFC

!        TAKE INTEGRATION STEP FOR SUSPENSION FEEDERS

     DTDAY = DLT/86400.

     SF(I) = SF(I) +  DTDAY * ( SFGC                            &    ! units: mgC/m2
             - RESPCT*SF(I) - RMORT*SF(I) - PREDCT*SF(I)*SF(I))

! DONT LET GO NEGATIVE

     SF(I) = MAX(SF(I),0.1)

!MBM 961226 diagnostic arrays
     SFFILTCT(I,N) = FILTCT
     SFRESPCT(I,N) = RESPCT
     SFPREDCT(I,N) = PREDCT
     SFRATEI(I,N)  = RATEI
     SFGCMAX(I,N)  = GCMAX
     SFGMIN(I,N)   = GMIN
     SFCFECES(I,N) = CFECES
     SFNFECES(I,N) = NFECES
     SFPFECES(I,N) = PFECES
     SFCPSFEC(I,N) = CPSFEC
     SFNPSFEC(I,N) = NPSFEC
     SFPPSFEC(I,N) = PPSFEC
     SF_SFGC(I,N)  = SFGC
     SF_RESP(I,N)  = RESPCT*SF(I)
     SF_PRED(I,N)  = PREDCT*SF(I)*SF(I)
     SF_RMORT(I,N) = RMORT*SF(I)
       
   END DO
255 CONTINUE

! CONVERT BACK INTO SAVED ARRAYS

   DO I=1,MLOC
     SFEED(I,N)=SF(I)
   END DO

! ADD UP EACH SPECIES FLUX CONTRIBUTION
! Partic. fluxes (labile, refrac. C,N,P,Inorg solids,unavail Si,sorbed P,Si) 

   DO I=1,MLOC
     JNH4SF(I)=JNH4SF(I)+JNH4SFX(I)
     JPO4SF(I)=JPO4SF(I)+JPO4SFX(I)
     SODSF(I)=SODSF(I)+SODSFX(I)
     JSASF(I)=JSASF(I)+JSASFX(I)
     JSUSF(I)=JSUSF(I)+JSUSFX(I)
     SFLUXC(I)=SFLUXC(I)+SFLUXCX(I)
     SFLUXN(I)=SFLUXN(I)+SFLUXNX(I)
     SFLUXP(I)=SFLUXP(I)+SFLUXPX(I)
     SF_RPOC(I)=SF_RPOC(I)+SF_RPOCX(I)
     SF_RPON(I)=SF_RPON(I)+SF_RPONX(I)
     SF_RPOP(I)=SF_RPOP(I)+SF_RPOPX(I)
     SF_SSI(I)=SF_SSI(I)+SF_SSIX(I)
     SF_SU(I)=SF_SU(I)+SF_SUX(I)
     SF_SA(I)=SF_SA(I)+SF_SASX(I)
     SF_PIP(I)=SF_PIP(I)+SF_PIPX(I)
   END DO

   RETURN
   END SUBROUTINE SUSPFEED  

