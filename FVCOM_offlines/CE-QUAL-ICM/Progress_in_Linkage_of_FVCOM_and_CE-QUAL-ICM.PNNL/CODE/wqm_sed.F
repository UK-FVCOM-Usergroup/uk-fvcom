!************************************************************************
!**                                                                    **
!**                  Chesapeake Bay Sediment Model                     **
!**                                                                    **
!** Third version (speeded up) received from Fitzpatrick May 8, 1990   **
!**              First modified by C. Cerco May 9, 1990                **
!**           Final modifications D.M. Di Toro, Jan 27, 1992           **
!**                                                                    **
!** Modified (susp feeder inputs; deposit feeder dynamics)             **
!**      -- HydroQual, Inc. (S. Lowe, M. Meyers) Latest: 18 Apr 97     **
!**                                                                    **
!** Modified (updated methanogenesis-sulfate reduction algorithms)     **
!**      -- HydroQual, Inc. (N. Churi, D. Di Toro) Latest: 18 Apr 97   **
!**                                                                    **
!** org flux to sediment ==> diagenesis ==> inorg flux to water column **
!**                                                                    **
!**     3-G model - G1=labile, G2=refractory, G3=slow refractory       **
!**                                                                    **
!** Problems with CH4SAT, DDSO4 fixed by CFC.  DDSO4 11/3/06           **
!**                                                                    **
!************************************************************************
!**                                                                    **
!** Inputs                                                             **
!**                                                                    **
!**            Required inputs for sediment sub-model                  **
!**                                                                    **
!** A.  Passed to sediment subroutine from water quality subroutine    **
!**                                                                    **
!**   1.  Overlying water column segment volume (V1)                   **
!**   2.  Overlying water column segment depth (BL(ISEG,3))            **
!**   3.  Overlying water column depth                                 **
!**   4.  Overlying water column segment temperature and salinity      **
!**   5.  Overlying water column ammonia, nitrate, phosphate, silica   **
!**       and dissolved oxygen concentrations                          **
!**                                                                    **
!** B. Inputs supplied via direct input to the sediment subroutine     **
!**                                                                    **
!**  Variable names        Description                         Units   **
!**                                                                    **
!**                                                                    **
!**     HSEDALL      Depth of sediment layer  (h2)               cm    **
!**      DIFFT       Water column-sediment layer diffusion     cm2/sec **
!**                  coefficient for temperature                       **
!**     SALTSW       Salinity concentration for determining      ppt   **
!**                  whether methane or sulfide SOD formulation        **
!**                  is to be used.  Also determines PO4 sorption.     **
!**     SALTND       Determines whether fresh or saltwater             **
!**                  nitrification/denitrification rates are used      **
!**                                                                    **
!**   Diagenesis stoichiometry                                         **
!**                                                                    **
!**                 Fractions of G1, G2, and G3 contained in ...       **
!**                                                                    **
!**   FRPPH1(3)     Algal group no.1 phosphorus                        **
!**   FRPPH2(3)     Algal group no.2 phosphorus                        **
!**   FRPPH3(3)     Algal group no.3 phosphorus                        **
!**   FRPOP(NSED,3) Non-algal particulate organic phosphorus           **
!**   FRNPH1(3)     Algal group no.1 nitrogen                          **
!**   FRNPH2(3)     Algal group no.2 nitrogen                          **
!**   FRNPH3(3)     Algal group no.3 nitrogen                          **
!**   FRPON(NSED,3) Non-algal particulate organic nitrogen             **
!**   FRCPH1(3)     Algal group no.1 carbon                            **
!**   FRCPH2(3)     Algal group no.2 carbon                            **
!**   FRCPH3(3)     Algal group no.3 carbon                            **
!**   FRPOC(NSED,3) Non-algal particulate organic carbon               **
!**                                                                    **
!**   Diagenesis kinetics                                              **
!**                                                                    **
!**   KPDIAG(3)     Reaction rates for POP G1, G2, and G3        /day  **
!**   DPTHTA(3)     Temperature thetas for POP G1, G2, and G3          **
!**   KNDIAG(3)     Reaction rates for PON G1, G2, and G3        /day  **
!**   DNTHTA(3)     Temperature thetas for PON G1, G2, and G3          **
!**   KCDIAG(3)     Reaction rates for POC G1, G2, and G3        /day  **
!**   DCTHTA(3)     Temperature thetas for POC G1, G2, and G3          **
!**   KSI           Reaction rate for Part. Biogenic Si (PSi)    /day  **
!**   THTASI        Temperature theta for PSi                          **
!**                                                                    **
!**   Solids and transport                                             **
!**                                                                    **
!**   VPMIX(NSED)   Particulate diffusion coefficient (Dp)   m**2/day  **
!**   THTADP        Temperature theta for Dp                           **
!**   VDMIX(NSED)   Porewater diffusion coefficient (Dd)     m**2/day  **
!**   THTADD        Temperature theta for Dd                           **
!**      M1         Concentration of solids in layer 1       kg/l      **
!**      M2         Concentration of solids in layer 2       kg/l      **
!**                                                                    **
!**   Reaction kinetics                                                **
!**                                                                    **
!**    KAPPNH4F     Nitrification reaction velocity                    **
!**                 for freshwater in layer 1                m/day     **
!**    KAPPNH4S     Nitrification reaction velocity                    **
!**                 for saltwater  in layer 1                m/day     **
!**    PIENH4       Ammonia partition coefficient            L/kg      **
!**    THTANH4      Theta for nitrification reaction velicities        **
!**    KMNH4        Nitrification half saturation constant             **
!**                 for ammonia                              mg N/m3   **
!**    KMNH4O2      Nitrification half saturation constant             **
!**                 for oxygen                               mg O2/L   **
!**    KAPPNO3F     Denitrification reaction velocity                  **
!**                 for freshwater in layer 1                m/day     **
!**    KAPPNO3S     Denitrification reaction velocity                  **
!**                 for saltwater  in layer 1                m/day     **
!**    K2NO3        Denitrification reaction velocity                  **
!**                 in layer 2                               m/day     **
!**    THTANO3      Theta for denitrification                          **
!**    KAPPD1       Reaction velocity for dissolved sulfide            **
!**                 oxidation in layer 1                     m/day     **
!**    KAPPP1       Reaction velocity for particulate sulfide          **
!**                 oxidation in layer 1                     m/day     **
!**    PIE1S        Partition coefficient for sulfide                  **
!**                 in layer 1                               L/kg      **
!**    PIE2S        Partition coefficient for sulfide                  **
!**                 in layer 2                               L/kg      **
!**    THTAPD1      Theta for both dissolved and particulate           **
!**                 sulfide oxidation                                  **
!**    KMHSO2       Sulfide oxidation normalization constant           **
!**                 for oxygen                               mg O2/L   **
!**    CSISAT       Saturation concentration for pore water            **
!**                 silica                                   mg Si/m3  **
!**    DPIE1SI      Incremental partition coefficient for              **
!**                 silica in layer 1                        L/kg      **
!**    PIE2SI       Partition coefficient for silica in                **
!**                 layer 2                                  L/kg      **
!**    KMPSI        Particulate biogenic silica half saturation        **
!**                 constant for dissolution                 mg Si/m3  **
!**    O2CRITSI     Critical dissolved oxygen concentration            **
!**                 for layer 1 incremental silica sorption  mg O2/L   **
!**    JSIDETR      Detrital biogenic silica source to                 **
!**                 sediment                                 mg Si/m2-d**
!**    DPIE1PO4F    Incremental partition coefficient for              **
!**                 phosphate in layer 1 (freshwater)        L/kg      **
!**    DPIE1PO4S    Incremental partition coefficient for              **
!**                 phosphate in layer 1 (saltwater)         L/kg      **
!**    PIE2PO4      Partition coefficient for phosphate                **
!**                 in layer 2                               L/kg      **
!**    O2CRIT       Critical dissolved oxygen concentration for        **
!**                 layer 1 incremental phosphate sorption   mg O2/L   **
!**    KMO2DP       Particle mixing half saturation constant           **
!**                 for oxygen                               mg O2/L   **
!**    TEMPBEN      Temperature at which benthic stress                **
!**                 accumulation is reset to zero            deg C     **
!**    KBENSTR      Decay constant for benthic stress        /day      **
!**    KLBNTH       Ratio of bio-irrigation to bioturbation            **
!**    DPMIN        Minimum particle diffusion coefficient   m2/day    **
!**    KAPPCH4      methane oxidation reaction velocity      m/day     **
!**    THTACH4      theta for methane oxidation                        **
!**                                                                    **
!** Output                                                             **
!**                                                                    **
!**    The subroutine returns fluxes for                               **
!**                                                                    **
!**     JSOD, JAQSOD, JCH4AQ and JCH4G  [gm o2*/m2-day]                **
!**     JNH4, JPO4, JNO3 and JSI  [mg/m2-day]                          **
!**                                                                    **
!**    via array BFLUX in COMMON /BENTHC/                              **
!**                                                                    **
!************************************************************************

   MODULE SED
   USE WQM
   USE FILE_INFO
   USE ALL_VARS

   IMPLICIT NONE
   SAVE
   INTEGER  :: INTSEDC, IWC, IERR, ITEMP
   REAL     :: HSEDALL, DIFFT, XKBO2, DFDOH, DFDOQ, ALPHB, CCHLB
   REAL     :: FLX1WC, FLX2WC, FLX3WC, FLX4WC
   REAL     :: FLX5WC, FLX6WC, FLX7WC
   REAL     :: STP20, DF, PO4AVL, BFOR, W12MIN
   REAL     :: DOLOW, LOGICT, IK
   REAL     :: XKPOP1, XKPOP2, XKPOP3, XKPON1, XKPON2, XKPON3
   REAL     :: XKPOC1, XKPOC2, XKPOC3
   REAL     :: RDD, RMORT, XPOC1LIM, TEMP, XPOC2LIM, DOH2
   REAL     :: FRPON1, FRPOP1, FRPOC1, DFSOD
   REAL     :: ERROR, A1, A2
   REAL     :: XJCNO31, FP1SO4, FP2SO4, HS2AV, XJ2, XJ2CH4, X1J2
   REAL     :: PF, PIOP, FTB, PRNB, FRDOB
   REAL     :: FLUXHS, FLUXHSCH4, VJCH4G

   CONTAINS

!********************************************************************************
!**                    S U B R O U T I N E   S E D _ R E A D                   **
!********************************************************************************
   SUBROUTINE SED_READ
#  if defined (MULTIPROCESSOR)
   USE MOD_PAR
#  endif
!   USE WQM
!   USE FILE_INFO
   IMPLICIT NONE
! added next line 10/17/05
   SAVE    

!***** Variable declarations

   CHARACTER(LEN=24) :: FRNAME(14)
   CHARACTER(LEN=3)  :: BALC
   CHARACTER(LEN=8)  :: SPVARS, SPVARLR, SPVARB, PRINTS, PRINTLR, PRINTB
   INTEGER :: I, JG, JT
   REAL, ALLOCATABLE :: RTMP11(:),RTMP12(:),RTMP13(:),RTMP14(:),     &
                        RTMP15(:),RTMP16(:),RTMP17(:)
!***** Data declarations

   DATA FRNAME                                                              &
        /'Group 1 algal phosphorus', 'Group 2 algal phosphorus',            &
         'Group 3 algal phosphorus', 'Detrital org phosphorus ',            &
         'Group 1 algal nitrogen  ', 'Group 2 algal nitrogen  ',            &
         'Group 3 algal nitrogen  ', 'Detrital org nitrogen   ',            &
         'Group 1 algal carbon    ', 'Group 2 algal carbon    ',            &
         'Group 3 algal carbon    ', 'Benthic algal carbon    ',            &
         'Benthic algal nitrogen  ', 'Benthic algal phosphorus'/

!********************************************************************************
!**                                  Inputs                                    **
!********************************************************************************

   READ(BFI,1000,ERR=10100)  HSEDALL, INTSEDC
   READ(BFI,1010,ERR=10100)  DIFFT
   READ(BFI,1010,ERR=10100)  SALTSW, SALTND
   READ(BFI,1010,ERR=10100)  FRPPH1
   READ(BFI,1010,ERR=10100)  FRPPH2
   READ(BFI,1010,ERR=10100)  FRPPH3
   READ(BFI,1010,ERR=10100)  FRPPHB    
   READ(BFI,1010,ERR=10100)  FRNPH1
   READ(BFI,1010,ERR=10100)  FRNPH2
   READ(BFI,1010,ERR=10100)  FRNPH3
   READ(BFI,1010,ERR=10100)  FRNPHB
   READ(BFI,1010,ERR=10100)  FRCPH1
   READ(BFI,1010,ERR=10100)  FRCPH2
   READ(BFI,1010,ERR=10100)  FRCPH3
   READ(BFI,1010,ERR=10100)  FRCPHB
   READ(BFI,1010,ERR=10100) (KPDIAG(JG),DPTHTA(JG),JG=1,3)
   READ(BFI,1010,ERR=10100) (KNDIAG(JG),DNTHTA(JG),JG=1,3)
   READ(BFI,1010,ERR=10100) (KCDIAG(JG),DCTHTA(JG),JG=1,3)
   READ(BFI,1010,ERR=10100)  KSI,THTASI
   READ(BFI,1015,ERR=10100)  M1,M2,THTADP,THTADD
   READ(BFI,1015,ERR=10100)  KAPPNH4F,KAPPNH4S,PIENH4,THTANH4,KMNH4,KMNH4O2
   READ(BFI,1015,ERR=10100)  KAPPNO3F,KAPPNO3S,K2NO3,THTANO3
   READ(BFI,1015,ERR=10100)  KAPPD1,KAPPP1,PIE1S,PIE2S,THTAPD1,KMHSO2
   READ(BFI,1015,ERR=10100)  CSISAT,DPIE1SI,PIE2SI,KMPSI
   READ(BFI,1015,ERR=10100)  O2CRITSI,JSIDETR
   READ(BFI,1015,ERR=10100)  DPIE1PO4F,DPIE1PO4S,PIE2PO4,O2CRIT,KMO2DP
   READ(BFI,1015,ERR=10100)  TEMPBEN,KBENSTR,KLBNTH,DPMIN
   READ(BFI,1015,ERR=10100)  KAPPCH4,THTACH4,KMCH4O2,KMSO4

! deposit feeders
       
   READ(BFI,1015,ERR=10100) XKMI0,ING0,THTAI0,R,THTAR,BETA,THBETA
   READ(BFI,1015,ERR=10100) AMCN,AMCP,AA1,AA2,XKMG1,XKMG2
   READ(BFI,1015,ERR=10100) XKBO2,TDD,DOLOW,DFDOH,DFDOQ

!     write(6,*)'dfeedm1s(5)',dfeedm1s(5)

! option to turn off deposit feeders

   IF(.NOT. DEPFEED)THEN 
     ING0=0.0
     R=0.0
     BETA=0.0
   END IF

! benthic algae 

   READ(BFI,1012,ERR=10100)  BALC
   READ(BFI,1015,ERR=10100)  PMB, ANCB, APCB, KTGB1, KTGB2, TMB
   READ(BFI,1015,ERR=10100)  ALPHB, CCHLB, KESED, KEBALG, KHNB, KHPB, KHRB
   READ(BFI,1015,ERR=10100)  BMRB, BPRB, KTBB, TRB, BALGMIN
   READ(BFI,1015,ERR=10100)  FNIB, FPIB

! net settling rates

   READ(BFI,1060,ERR=10100)  SPVARS,PRINTS
   IF (SPVARS == 'CONSTANT') THEN
     READ(BFI,1020,ERR=10100) WSSBNET(1),WSLBNET(1),WSRBNET(1),          &
                              WS1BNET(1),WS2BNET(1),WS3BNET(1),          &
                              WSUBNET(1)
     DO I=2,MTLOC
       WSSBNET(I)=WSSBNET(1)
       WSLBNET(I)=WSLBNET(1)
       WSRBNET(I)=WSRBNET(1)
       WS1BNET(I)=WS1BNET(1)
       WS2BNET(I)=WS2BNET(1)
       WS3BNET(I)=WS3BNET(1)
       WSUBNET(I)=WSUBNET(1)
     END DO
   ELSE
     ALLOCATE(RTMP11(MGL));     RTMP11 = 0.0
     ALLOCATE(RTMP12(MGL));     RTMP12 = 0.0
     ALLOCATE(RTMP13(MGL));     RTMP13 = 0.0
     ALLOCATE(RTMP14(MGL));     RTMP14 = 0.0
     ALLOCATE(RTMP15(MGL));     RTMP15 = 0.0
     ALLOCATE(RTMP16(MGL));     RTMP16 = 0.0
     ALLOCATE(RTMP17(MGL));     RTMP17 = 0.0
     DO I=1,MGL
       READ(BFI,1020,ERR=10100) WSSBNET(I),WSLBNET(I),WSRBNET(I),     &
                                WS1BNET(I),WS2BNET(I),WS3BNET(I),     &
                                WSUBNET(I)
     END DO
     IF(SERIAL)THEN
       WSSBNET = RTMP11
       WSLBNET = RTMP12
       WSRBNET = RTMP13
       WS1BNET = RTMP14
       WS2BNET = RTMP15
       WS3BNET = RTMP16
       WSUBNET = RTMP17
     END IF
#    if defined (MULTIPROCESSOR)
     IF(PAR)THEN
       DO I=1,MLOC
         WSSBNET(I) = RTMP11(NGID(I))
         WSLBNET(I) = RTMP12(NGID(I))
         WSRBNET(I) = RTMP13(NGID(I))
         WS1BNET(I) = RTMP14(NGID(I))
         WS2BNET(I) = RTMP15(NGID(I))
         WS3BNET(I) = RTMP16(NGID(I))
         WSUBNET(I) = RTMP17(NGID(I))
       END DO
       DO I=1,NHN
         WSSBNET(I+MLOC) = RTMP11(HN_LST(I))
         WSLBNET(I+MLOC) = RTMP12(HN_LST(I))
         WSRBNET(I+MLOC) = RTMP13(HN_LST(I))
         WS1BNET(I+MLOC) = RTMP14(HN_LST(I))
         WS2BNET(I+MLOC) = RTMP15(HN_LST(I))
         WS3BNET(I+MLOC) = RTMP16(HN_LST(I))
         WSUBNET(I+MLOC) = RTMP17(HN_LST(I))
       END DO
     END IF
#    endif
     DEALLOCATE(RTMP11,RTMP12,RTMP13,RTMP14,RTMP15,RTMP16,RTMP17)
   END IF

! burial and mixing rates

   READ(BFI,1060,ERR=10100)  SPVARB,PRINTB
   IF (SPVARB == 'CONSTANT') THEN
     READ(BFI,1022,ERR=10100) VSED(1),VPMIX(1),VDMIX(1)
     DO I=2,MTLOC
       VSED(I)=VSED(1)
       VPMIX(I)=VPMIX(1)
       VDMIX(I)=VDMIX(1)
     END DO
   ELSE
     ALLOCATE(RTMP11(MGL));     RTMP11 = 0.0
     ALLOCATE(RTMP12(MGL));     RTMP12 = 0.0
     ALLOCATE(RTMP13(MGL));     RTMP13 = 0.0
     DO I=1,MGL
       READ(BFI,1022,ERR=10100) RTMP11(I),RTMP12(I),RTMP13(I)
     END DO  
     IF(SERIAL)THEN
       VSED  = RTMP11
       VPMIX = RTMP12
       VDMIX = RTMP13
     END IF
#    if defined (MULTIPROCESSOR)
     IF(PAR)THEN
       DO I=1,MLOC
         VSED(I)  = RTMP11(NGID(I))
         VPMIX(I) = RTMP12(NGID(I))
         VDMIX(I) = RTMP13(NGID(I))
       END DO
       DO I=1,NHN
         VSED(I+MLOC)  = RTMP11(HN_LST(I))
         VPMIX(I+MLOC) = RTMP12(HN_LST(I))
         VDMIX(I+MLOC) = RTMP13(HN_LST(I))
       END DO
     END IF
#    endif
     DEALLOCATE(RTMP11,RTMP12,RTMP13)
   END IF

! splits of refractory water column into G2, G3 sediments 

   READ(BFI,1060,ERR=10100)  SPVARLR,PRINTLR
   IF (SPVARLR == 'CONSTANT') THEN
     READ(BFI,1040,ERR=10100) FRPOP(1,2),FRPOP(1,3),FRPON(1,2),        &
                              FRPON(1,3),FRPOC(1,2),FRPOC(1,3)
     DO I=2,MTLOC
       FRPOP(I,2)=FRPOP(1,2)
       FRPOP(I,3)=FRPOP(1,3)
       FRPON(I,2)=FRPON(1,2)
       FRPON(I,3)=FRPON(1,3)
       FRPOC(I,2)=FRPOC(1,2)
       FRPOC(I,3)=FRPOC(1,3)
     END DO
   ELSE
     ALLOCATE(RTMP11(MGL));     RTMP11 = 0.0
     ALLOCATE(RTMP12(MGL));     RTMP12 = 0.0
     ALLOCATE(RTMP13(MGL));     RTMP13 = 0.0
     ALLOCATE(RTMP14(MGL));     RTMP14 = 0.0
     ALLOCATE(RTMP15(MGL));     RTMP15 = 0.0
     ALLOCATE(RTMP16(MGL));     RTMP16 = 0.0
     DO I=1,MGL
       READ(BFI,1040,ERR=10100) FRPOP(I,2),FRPOP(I,3),FRPON(I,2),    &
                                FRPON(I,3),FRPOC(I,2),FRPOC(I,3)
     END DO
     IF(SERIAL)THEN
       FRPOP(:,2) = RTMP11
       FRPOP(:,3) = RTMP12
       FRPON(:,2) = RTMP13
       FRPON(:,3) = RTMP14
       FRPOC(:,2) = RTMP15
       FRPOC(:,3) = RTMP16
     END IF
#    if defined (MULTIPROCESSOR)
     IF(PAR)THEN
       DO I=1,MLOC
         FRPOP(I,2) = RTMP11(NGID(I))
         FRPOP(I,3) = RTMP12(NGID(I))
         FRPON(I,2) = RTMP13(NGID(I))
         FRPON(I,3) = RTMP14(NGID(I))
         FRPOC(I,2) = RTMP15(NGID(I))
         FRPOC(I,3) = RTMP16(NGID(I))
       END DO
       DO I=1,NHN
         FRPOP(I+MLOC,2) = RTMP11(HN_LST(I))
         FRPOP(I+MLOC,3) = RTMP12(HN_LST(I))
         FRPON(I+MLOC,2) = RTMP13(HN_LST(I))
         FRPON(I+MLOC,3) = RTMP14(HN_LST(I))
         FRPOC(I+MLOC,2) = RTMP15(HN_LST(I))
         FRPOC(I+MLOC,3) = RTMP16(HN_LST(I))
       END DO
     END IF
#    endif
     DEALLOCATE(RTMP11,RTMP12,RTMP13,RTMP14,RTMP15,RTMP16)
   END IF
 
!***** Define logical variables

   STEADY_STATE_SED = INTSEDC == 1
   BALGAE_CALC = BALC == ' ON'

!********************************************************************************
!**                                 Outputs                                    **
!********************************************************************************

   IF (BENTHIC_OUTPUT) THEN
     IF(MSR)THEN
     OPEN (BFO,FILE=BFOFN)
     WRITE(BFO,2000)
     WRITE(BFO,2020) HSEDALL
     IF (STEADY_STATE_SED) THEN
       WRITE(BFO,2022)
     ELSE
       WRITE(BFO,2025)
     END IF
     WRITE(BFO,2030)
     WRITE(BFO,2040)  SSNAME(1)
     WRITE(BFO,2050) (CTEMP(I),I=1,MLOC)
     WRITE(BFO,2060)  SSNAME(2)
     WRITE(BFO,2070) ((CPOP(I,JG),JG=1,3),I=1,MLOC)
     WRITE(BFO,2060)  SSNAME(3)
     WRITE(BFO,2070) ((CPON(I,JG),JG=1,3),I=1,MLOC)
     WRITE(BFO,2060)  SSNAME(4)
     WRITE(BFO,2070) ((CPOC(I,JG),JG=1,3),I=1,MLOC)
     WRITE(BFO,2060)  SSNAME(5)
     WRITE(BFO,2070) (CPOS(I),I=1,MLOC)
     WRITE(BFO,2040)  SSNAME(6)
     WRITE(BFO,2050) (PO4T2TM1S(I),I=1,MLOC)
     WRITE(BFO,2040)  SSNAME(7)
     WRITE(BFO,2050) (NH4T2TM1S(I),I=1,MLOC)
     WRITE(BFO,2040)  SSNAME(8)
     WRITE(BFO,2050) (NO3T2TM1S(I),I=1,MLOC)
     WRITE(BFO,2040)  SSNAME(9)
     WRITE(BFO,2050) (HST2TM1S(I),I=1,MLOC)
     WRITE(BFO,2040)  SSNAME(10)
     WRITE(BFO,2050) (SIT2TM1S(I),I=1,MLOC)
     WRITE(BFO,2040)  SSNAME(11)
     WRITE(BFO,2050) (BENSTR1S(I),I=1,MLOC)
     WRITE(BFO,2040)  SSNAME(13)
     WRITE(BFO,2050) (CH4T2TM1S(I),I=1,MLOC)
     WRITE(BFO,2040)  SSNAME(14)
     WRITE(BFO,2050) (SO4T2TM1S(I),I=1,MLOC)
     WRITE(BFO,2080)  0.0001*DIFFT
     WRITE(BFO,2090)  SALTSW,SALTND
     WRITE(BFO,2100)
     WRITE(BFO,2110)  FRNAME(1),FRPPH1
     WRITE(BFO,2110)  FRNAME(2),FRPPH2
     WRITE(BFO,2110)  FRNAME(3),FRPPH3
     WRITE(BFO,2110)  FRNAME(14),FRPPHB
     WRITE(BFO,2110)  FRNAME(5),FRNPH1
     WRITE(BFO,2110)  FRNAME(6),FRNPH2
     WRITE(BFO,2110)  FRNAME(7),FRNPH3
     WRITE(BFO,2110)  FRNAME(13),FRNPHB
     WRITE(BFO,2110)  FRNAME(9),FRCPH1
     WRITE(BFO,2110)  FRNAME(10),FRCPH2
     WRITE(BFO,2110)  FRNAME(11),FRCPH3
     WRITE(BFO,2110)  FRNAME(12),FRCPHB
     WRITE(BFO,2250) (I,FRPOP(I,2),FRPOP(I,3),FRPON(I,2),        &
                      FRPON(I,3),FRPOC(I,2),FRPOC(I,3),I=1,MLOC)
     WRITE(BFO,2120) (KPDIAG(JG),DPTHTA(JG),JG=1,3),(KNDIAG(JG),          &
                      DNTHTA(JG),JG=1,3),(KCDIAG(JG),DCTHTA(JG),          &
                      JG=1,3),KSI,THTASI
     WRITE(BFO,2252)  BALC
     WRITE(BFO,2242)  PMB, ANCB, APCB, KTGB1, KTGB2, TMB
     WRITE(BFO,2244)  ALPHB, CCHLB, KESED, KEBALG, KHNB, KHPB, KHRB
     WRITE(BFO,2246)  BMRB, BPRB, KTBB, TRB, BALGMIN
     WRITE(BFO,2248)  FNIB, FPIB
     WRITE(BFO,2170)  M1,M2,THTADP,THTADD
     WRITE(BFO,2180)  KAPPNH4F,KAPPNH4S,PIENH4,THTANH4,KMNH4,KMNH4O2
     WRITE(BFO,2190)  KAPPNO3F,KAPPNO3S,K2NO3,THTANO3
     WRITE(BFO,2200)  KAPPD1,KAPPP1,PIE1S,PIE2S,THTAPD1,KMHSO2
     WRITE(BFO,2210)  CSISAT,DPIE1SI,PIE2SI,KMPSI,O2CRITSI,JSIDETR
     WRITE(BFO,2220)  DPIE1PO4F,DPIE1PO4S,PIE2PO4,O2CRIT,KMO2DP
     WRITE(BFO,2230)  TEMPBEN,KBENSTR,KLBNTH,DPMIN
     WRITE(BFO,2240)  KAPPCH4,THTACH4
     WRITE(BFO,2130) (I,WSSBNET(I),WSLBNET(I),WSRBNET(I),             &
                      WS1BNET(I),WS2BNET(I),WS3BNET(I),               &
                      WSUBNET(I),I=1,MLOC)
     WRITE(BFO,2140) (I,VSED(I),I=1,MLOC)
     WRITE(BFO,2150) (I,VPMIX(I),I=1,MLOC)
     WRITE(BFO,2160) (I,VDMIX(I),I=1,MLOC)
     CLOSE(BFO)
     END IF
   END IF

!********************************************************************************
!**                             Initializations                                **
!********************************************************************************

!***** Convert cell heights and burial velocities to sediment units

   DIFFT = 0.0001*DIFFT
   DO I=1,MLOC
     HSED(I) = HSEDALL*0.01
     VSED(I) = VSED(I)*2.73791E-5
   END DO

!***** Set sediment concentrations to initial concentrations

   DO I=1,MLOC
     POP1TM1S(I) = CPOP(I,1)
     POP2TM1S(I) = CPOP(I,2)
     POP3TM1S(I) = CPOP(I,3)
     PON1TM1S(I) = CPON(I,1)
     PON2TM1S(I) = CPON(I,2)
     PON3TM1S(I) = CPON(I,3)
     POC1TM1S(I) = CPOC(I,1)
     POC2TM1S(I) = CPOC(I,2)
     POC3TM1S(I) = CPOC(I,3)
     PSITM1S(I)  = CPOS(I)
   END DO

!***** Initialize mass balance variables

   ISEDMN = 0.
   ISEDMP = 0.
   ISEDMC = 0.
   DO I=1,MLOC
     CPIP(I) = PO4T2TM1S(I)
     CNO3(I) = NO3T2TM1S(I)
     CNH4(I) = NH4T2TM1S(I)
     ISEDMN   = ISEDMN+(CPON(I,1)+CPON(I,2)+CPON(I,3)+CNH4(I)      &
                +CNO3(I))*SFA(I)*HSED(I)/1.E6
     ISEDMP   = ISEDMP+(CPOP(I,1)+CPOP(I,2)+CPOP(I,3)+CPIP(I))     &
                *SFA(I)*HSED(I)/1.E6
     ISEDMC   = ISEDMC+(CPOC(I,1)+CPOC(I,2)+CPOC(I,3))              &
                *SFA(I)*HSED(I)/1.E6
   END DO

!***** Set up reaction rates in table look-up form

   DO JT=1,350
     TEMP         = REAL(JT-1)/10.+0.05
     TEMP20       = TEMP-20.
     TEMP202      = TEMP20/2.
     ZHTANH4F(JT) = KAPPNH4F*THTANH4**TEMP202
     ZHTANH4S(JT) = KAPPNH4S*THTANH4**TEMP202
     ZHTAD1(JT)   = KAPPD1*THTAPD1**TEMP202
     ZHTAP1(JT)   = KAPPP1*THTAPD1**TEMP202
     ZHTANO3F(JT) = KAPPNO3F*THTANO3**TEMP202
     ZHTANO3S(JT) = KAPPNO3S*THTANO3**TEMP202
     ZHTA2NO3(JT) = K2NO3*THTANO3**TEMP20
     ZL12NOM(JT)  = THTADD**TEMP20
     ZW12NOM(JT)  = THTADP**TEMP20
     ZHTAPON1(JT) = KPON1*THTAPON1**TEMP20
     ZHTAPON2(JT) = KPON2*THTAPON2**TEMP20
     ZHTAPON3(JT) = KPON3*THTAPON3**TEMP20
     ZHTAPOC1(JT) = KPOC1*THTAPOC1**TEMP20
     ZHTAPOC2(JT) = KPOC2*THTAPOC2**TEMP20
     ZHTAPOC3(JT) = KPOC3*THTAPOC3**TEMP20
     ZHTAPOP1(JT) = KPOP1*THTAPOP1**TEMP20
     ZHTAPOP2(JT) = KPOP2*THTAPOP2**TEMP20
     ZHTAPOP3(JT) = KPOP3*THTAPOP3**TEMP20
     ZHTASI(JT)   = KSI*THTASI**TEMP20
     ZHTACH4(JT)  = KAPPCH4*THTACH4**TEMP202
     ZHTAI0(JT)   = ING0*THTAI0**TEMP20           ! deposit feeders
     ZHTAR(JT)    = R*THTAR**TEMP20               ! deposit feeders
     ZHTABETA(JT) = BETA*THBETA**TEMP20           ! deposit feeders
   END DO

!***** Turn off settling

   IF (.NOT.SETTLING) THEN
     DO I=1,MLOC
       WSSBNET(I) = 0.
       WSLBNET(I) = 0.
       WSRBNET(I) = 0.
       WS1BNET(I) = 0.
       WS2BNET(I) = 0.
       WS3BNET(I) = 0.
       WSUBNET(I) = 0.
     END DO
   END IF

!***** Initialize accumulators for steady-state computations

   IF (STEADY_STATE_SED) THEN
     TINTIM = 0.
     DO I=1,MLOC
       AG3CFL(I) = 0.
       AG3NFL(I) = 0.
       AG3PFL(I) = 0.
       ASDTMP(I) = 0.
     END DO
   END IF

!***** Input FORMAT'S

1000 FORMAT(:////8X,F8.0,I8)
1002 format(/7f10.4)
1010 FORMAT(8F10.0)
1012 FORMAT(//13X,A3)
1015 FORMAT(//8X,8F8.1)
1020 FORMAT(:///(8X,7F8.1))
1022 FORMAT(:///(8X,3F8.1))
1030 FORMAT(I10)
1040 FORMAT(:///(8X,6F8.1))
1060 FORMAT(//8X,2A8,F8.0)
 
!***** Output FORMAT'S

2000 FORMAT(///34X,'Sediment-water column linkages and sediment ',      &
            'depths and volumes'/)
2020 FORMAT(/' ACTIVE LAYER DEPTH ',F8.3,' CM')
2022 FORMAT(/' STEADY-STATE VALUES OF G3 COMPUTED'/)
2025 FORMAT(/' NO STEADY-STATE VALUES OF G3 COMPUTED'/)
2030 FORMAT(////33X,'S E D I M E N T   I N I T I A L   C O N D I T ',   &
            'I O N S'/)
2040 FORMAT(//25X,'Sediment initial conditions for ',A20/)
2050 FORMAT(13X,3(7X,1PE11.4))
2060 FORMAT(//25X,'Sediment initial conditions for ',A20/               &
            37X,'G1',22X,'G2',22X,'G3'/)
2070 FORMAT(18X,3(2X,1PE11.4))
2080 FORMAT(//30X,'Temperature diffusion coefficient ',E10.3,           &
            ' cm**2/sec')
2090 FORMAT(//31X,'If salinity < ',F10.3,' ppt, methane formed',/       &
            30X,'If salinity < ',F10.3,' ppt, high nit/denit used')
2100 FORMAT(//30X,'Particulate organic matter G-model splits'/          &
            10X,'fraction of....',5X,'recycled to',5X,'G1',5X,'G2',     &
            5X,'G3')
2110 FORMAT(6X,A24,11X,3F7.2)
2120 FORMAT(//30X,'Diagenesis rates (/day)  Temp corr factor'/          &
            30X,'Phosphorus'/                                           &
	    39X,'G1',E11.3,5X,F7.3/                                     &
            39X,'G2',E11.3,5X,F7.3/                                     &
            39X,'G3',E11.3,5X,F7.3/                                     &
            30X,'Nitrogen'/                                             &
            39X,'G1',E11.3,5X,F7.3/                                     &
            39X,'G2',E11.3,5X,F7.3/                                     &
            39X,'G3',E11.3,5X,F7.3/                                     &
            30X,'Carbon'/                                               &
            39X,'G1',E11.3,5X,F7.3/                                     &
            39X,'G2',E11.3,5X,F7.3/                                     &
            39X,'G3',E11.3,5X,F7.3/                                     &
            30X,'Silica'/                                               &
            41X,E11.3,5X,F7.3)
2130 FORMAT(//3X,'BBN',6X,'WSSNET',4X,'WSLNET',4X,'WSRNET',4X,          &
            'WS1NET',4X,'WS2NET',4X,'WS3NET',4X,'WSUNET'/               &
            (I7,7F10.3))
2140 FORMAT(//31X,'Sedimentation rates (cm/yr)'/                        &
            10X,8(I5,F6.2))
2150 FORMAT(//30X,'Sediment solid-phase mixing rates (m**2/day)'/       &
            10X,8(I5,F6.2))
2160 FORMAT(//30X,'Sediment dissolved-phase mixing rates (m**2/day)'/   &
            10X,8(I5,F6.2))
2170 FORMAT(//35X,'Additional constants'/                               &
            30X,'M1........',F8.2,' kg/l'/                              &
            30X,'M2........',F8.2,' kg/l'/                              &
            30X,'THTADP....',F8.3,/                                     &
            30X,'THTADD....',F8.3)
2180 FORMAT(30X,'KAPPNH4F..',F8.3,' m/day'/                             &
            30X,'KAPPNH4S..',F8.3,' m/day'/                             &
            30X,'PIENH4....',F8.3,' l/kg'/                              &
            30X,'THTANH4...',F8.3,/                                     &
            30X,'KMNH4.....',F8.3,' mg n/m**3'/                         &
            30X,'KMNH4O2...',F8.3,' mg o2/l')
2190 FORMAT(30X,'KAPPNO3F..',F8.3,' m/day'/                             &
            30X,'KAPPNO3S..',F8.3,' m/day'/                             &
            30X,'K2NO3.....',F8.3,' /day'/                              &
            30X,'THTANO3...',F8.3)
2200 FORMAT(30X,'KAPPD1....',F8.3,' m/day'/                             &
            30X,'KAPPP1....',F8.3,' m/day'/                             &
            30X,'PIE1S.....',F8.3,' l/kg'/                              &
            30X,'PIE2S.....',F8.3,' l/kg'/                              &
            30X,'THTAPD1...',F8.3,/                                     &
            30X,'KMHSO2....',F8.3,' mg o2/l')
2210 FORMAT(30X,'CSISAT....',F8.1,' mg si/m**3'/                        &
            30X,'DPIE1SI....',F8.3,' l/kg'/                             &
            30X,'PIE2SI....',F8.3,' l/kg'/                              &
            30X,'KMPSI.....',E8.2,' mg si/m**3'/                        &
            30X,'O2CRITSI..',E8.2,' mg O2/L'/                           &
            30X,'JSIDETR...',E8.2,' mg Si/m2-d')
2220 FORMAT(30X,'DPIE1PO4F..',F8.3,' l/kg'/                             &
            30X,'DPIE1PO4S..',F8.3,' l/kg'/                             &
            30X,'PIE2PO4..',F8.3,' l/kg'/                               &
            30X,'O2CRIT....',F8.3,' mg o2/l'/                           &
            30X,'KMO2DP....',F8.3,' mg o2/l')
2230 FORMAT(30X,'TEMPBEN...',F8.3,' deg c'/                             &
            30X,'KBENSTR...',F8.3,' /day'/                              &
            30X,'KLBNTH....',F8.3,'---'/                                &
            30X,'DPMIN.....',F8.3,' m2/d')           
2240 FORMAT(30X,'KAPPCH4...',F8.3,' m/day'/                             &
            30X,'THTACH4...',F8.3)
2242 FORMAT(' PMB = ',F10.3/' ANCB = ',F10.3/' APCB = ',F10.3/          &
            ' KTGB1 = ',F10.3/' KTGB2 = ',F10.3/' TMB = ',F10.3)
2244 FORMAT(' ALPHB = ',F10.3/' CCHLB = ',F10.3/                        &
            ' KESED = ',F10.3/' KEBALG = ',F10.3,                       &
            ' KHNB = ',F10.3/' KHPB = ',F10.3/' KHRB = ',F10.3)
2246 FORMAT(' BMRB = ',F10.3/' BPRB = ',F10.3/' KTBB = ', F10.3/        &
            ' TRB = ',F10.3/ ' BALGMIN = ',F10.3)
2248 FORMAT(' FNIB = ',F10.3/' FPIB = ',F10.3)
2250 FORMAT(//31X,'G2 - G3 splits for Refractory Particulates'/         &
            '   NBB   FRG2P   FRG3P   FRG2N   FRG3N   FRG2C   FRG3C'/   &
            (I5,6F8.3))
2252 FORMAT(/' BENTHIC ALGAE ARE ',A3)

!***** Error output FORMAT's

3000 FORMAT(///5X,'Zbrent failure, ierr=',I2,' in segment ',I5/         &
            5X,'Program termination follows diagnostic dumps')
3010 FORMAT(/' Read error in sediment input deck')

   RETURN

!***** Error traps

10100 CONTINUE

   IF (BENTHIC_OUTPUT) WRITE(BFO,3010)

   STOP 'SED_READ'
    
   RETURN
   END SUBROUTINE SED_READ



!********************************************************************************
!**                    S U B R O U T I N E   S E D _ C A L C                   **
!**                           Sediment Calculations                            **
!********************************************************************************

   SUBROUTINE SED_CALC
   IMPLICIT NONE
! added next line 10/17/05
   SAVE    
   INTEGER :: I, J, JSF, N, IERR
   REAL :: SODMIN, SODMAX, SAVEFCT
   REAL :: SOD,ZBRENT,ZBOUT

!***** Pass WQM time-step (in days) to sediment subr

   DLTS = DLT/86400.
   IF (STEADY_STATE_SED) TINTIM = TINTIM+DLTS

!***** Initialize sediment nutrient masses

   SEDMN = 0.
   SEDMP = 0.
   SEDMC = 0.

!******* Assign base net settling rates

   DO I=1,MLOC
     WSSNET(I) = WSSBNET(I)
     WSLNET(I) = WSLBNET(I)
     WSRNET(I) = WSRBNET(I)
     WS1NET(I) = WS1BNET(I)
     WS2NET(I) = WS2BNET(I)
     WS3NET(I) = WS3BNET(I)
     WSUNET(I) = WSUBNET(I)
   END DO

!******* Adjust net settling for SAV effect

!vjp modified 11/3/2005
!JQI   IF (SAV_CALC) THEN
!JQI     DO I=1,NSAVCELL
!JQI       B=SAVCELL(I)
!JQI       DO N=1,NSAVDPH(B)
!JQI         SAVEFCT = (LEAF(B,N)+STEM(B,N)) * SAVFRAC(B,N)
!JQI         WSSNET(B) = WSSNET(B) + WSSSAV * SAVEFCT
!JQI         WSLNET(B) = WSLNET(B) + WSLSAV * SAVEFCT
!JQI         WSRNET(B) = WSRNET(B) + WSRSAV * SAVEFCT
!JQI         WS1NET(B) = WS1NET(B) + WS1SAV * SAVEFCT
!JQI         WS2NET(B) = WS2NET(B) + WS2SAV * SAVEFCT
!JQI         WS3NET(B) = WS3NET(B) + WS3SAV * SAVEFCT
!JQI         WSUNET(B) = WSUNET(B) + WSUSAV * SAVEFCT
!JQI       END DO
!JQI     END DO
!JQI   END IF

!***** Calculate fluxes

   DO I=1,MLOC
     IWC = KBM1

!******* Flux rate

     FLX1WC    = 1000.*WSSNET(I)
     FLX2WC    = 1000.*WSLNET(I)
     FLX3WC    = 1000.*WSRNET(I)
     FLX4WC    = 1000.*WS1NET(I)
     FLX5WC    = 1000.*WS2NET(I)
     FLX6WC    = 1000.*WS3NET(I)
     FLX7WC    = 1000.*WSUNET(I)

!      *** Fluxes

     FLXPOP(I,1) = FLX4WC*Q1(I,IWC)*FRPPH1(1)*B1(I,IWC)                  &
                   +FLX5WC*Q2(I,IWC)*FRPPH2(1)*B2(I,IWC)                 &
                   +FLX6WC*Q3(I,IWC)*FRPPH3(1)*B3(I,IWC)                 &
                   +FLX2WC*LPOP(I,IWC)                                   &
                   +SFLUXP(I)*FRPPH3(1)       ! suspension feeders
     FLXPOP(I,2) = FLX4WC*Q1(I,IWC)*FRPPH1(2)*B1(I,IWC)                  &
                   +FLX5WC*Q2(I,IWC)*FRPPH2(2)*B2(I,IWC)                 &
                   +FLX6WC*Q3(I,IWC)*FRPPH3(2)*B3(I,IWC)                 &
                   +FLX3WC*RPOP(I,IWC)*FRPOP(I,2)/                       &
                    (FRPOP(I,2)+FRPOP(I,3))                              &
                   +SFLUXP(I)*FRPPH3(2)       ! suspension feeders
     FLXPOP(I,3) = FLX4WC*Q1(I,IWC)*FRPPH1(3)*B1(I,IWC)                  &
                   +FLX5WC*Q2(I,IWC)*FRPPH2(3)*B2(I,IWC)                 &
                   +FLX6WC*Q3(I,IWC)*FRPPH3(3)*B3(I,IWC)                 &
                   +FLX3WC*RPOP(I,IWC)*FRPOP(I,3)/                       &
                    (FRPOP(I,2)+FRPOP(I,3))                              &
                   +SF_RPOP(I)                                           & ! suspension feeders
                   +SFLUXP(I)*FRPPH3(3)       ! suspension feeders
     FLXPON(I,1) = FLX4WC*ANC1*FRNPH1(1)*B1(I,IWC)                       &
                   +FLX5WC*ANC2*FRNPH2(1)*B2(I,IWC)                      &
                   +FLX6WC*ANC3*FRNPH3(1)*B3(I,IWC)                      &
                   +FLX2WC*LPON(I,IWC)                                   &
                   +SFLUXN(I)*FRNPH3(1)       ! suspension feeders
     FLXPON(I,2) = FLX4WC*ANC1*FRNPH1(2)*B1(I,IWC)                       &
                   +FLX5WC*ANC2*FRNPH2(2)*B2(I,IWC)                      &
                   +FLX6WC*ANC3*FRNPH3(2)*B3(I,IWC)                      &
                   +FLX3WC*RPON(I,IWC)*FRPON(I,2)/                       &
                    (FRPON(I,2)+FRPON(I,3))                              &
                   +SFLUXN(I)*FRNPH3(2)       ! suspension feeders
     FLXPON(I,3) = FLX4WC*ANC1*FRNPH1(3)*B1(I,IWC)                       &
                   +FLX5WC*ANC2*FRNPH2(3)*B2(I,IWC)                      &
                   +FLX6WC*ANC3*FRNPH3(3)*B3(I,IWC)                      &
                   +FLX3WC*RPON(I,IWC)*FRPON(I,3)/                       &
                    (FRPON(I,2)+FRPON(I,3))                              &
                   +SF_RPON(I)                                           &  ! suspension feeders
                   +SFLUXN(I)*FRNPH3(3)       ! suspension feeders
     FLXPOC(I,1) = FLX4WC*FRCPH1(1)*B1(I,IWC)                            &
                   +FLX5WC*FRCPH2(1)*B2(I,IWC)                           &
                   +FLX6WC*FRCPH3(1)*B3(I,IWC)                           &
                   +FLX2WC*LPOC(I,IWC)                                   &
                   +SFLUXC(I)*FRCPH3(1)       ! suspension feeders
     FLXPOC(I,2) = FLX4WC*FRCPH1(2)*B1(I,IWC)                            &
                   +FLX5WC*FRCPH2(2)*B2(I,IWC)                           &
                   +FLX6WC*FRCPH3(2)*B3(I,IWC)                           &
                   +FLX3WC*RPOC(I,IWC)*FRPOC(I,2)/                       &
                    (FRPOC(I,2)+FRPOC(I,3))                              &
                   +SFLUXC(I)*FRCPH3(2)       ! suspension feeders
     FLXPOC(I,3) = FLX4WC*FRCPH1(3)*B1(I,IWC)+                           &
                    FLX5WC*FRCPH2(3)*B2(I,IWC)                           &
                   +FLX6WC*FRCPH3(3)*B3(I,IWC)                           &
                   +FLX3WC*RPOC(I,IWC)*FRPOC(I,3)/                       &
                    (FRPOC(I,2)+FRPOC(I,3))                              &
                   +SF_RPOC(I)                                           & ! suspension feeders
                   +SFLUXC(I)*FRCPH3(3)       ! suspension feeders

!       *** Sum particulate fluxes to sediments, negative into sediments

     PPFWS(I)=-0.001*(FLXPOP(I,1)+FLXPOP(I,2)+FLXPOP(I,3))
     PNFWS(I)=-0.001*(FLXPON(I,1)+FLXPON(I,2)+FLXPON(I,3))
     PCFWS(I)=-0.001*(FLXPOC(I,1)+FLXPOC(I,2)+FLXPOC(I,3))
     PSFWS(I)=-0.001*FLXPOS(I)
     SSFWS(I)=-WSSNET(I)*SSI(I,IWC)-0.001*SF_SSI(I)
     FLXPOS(I) = FLX4WC*ASC1*B1(I,IWC)+FLX5WC*ASC2*B2(I,IWC)               &
                  +FLX6WC*ASC3*B3(I,IWC)+FLX7WC*SU(I,IWC)                  &
                  +SF_SU(I) + JSUSF(I)

   END DO

! ADD IN THE FLUX FROM ROOT MORTALITY

!vjp modified 11/3/2005
!JQI   IF (SAV_CALC) THEN
!JQI     DO I=1,NSAVCELL
!JQI       B=SAVCELL(I)
!JQI       DO J=1,3
!JQI         FLXPOC(B,J) = FLXPOC(B,J)+1000.*SEDCSAV(B)*FRCSAV(J)
!JQI         FLXPON(B,J) = FLXPON(B,J)+1000.*SEDNSAV(B)*FRNSAV(J)
!JQI         FLXPOP(B,J) = FLXPOP(B,J)+1000.*SEDPSAV(B)*FRPSAV(J)
!JQI       END DO
!JQI     END DO
!JQI   END IF

!    *** Accumulate fluxes for steady-state computation

   IF (STEADY_STATE_SED) THEN
     DO I=1,MLOC
       AG3CFL(I) = AG3CFL(I)+FLXPOC(I,3)*DLTS
       AG3NFL(I) = AG3NFL(I)+FLXPON(I,3)*DLTS
       AG3PFL(I) = AG3PFL(I)+FLXPOP(I,3)*DLTS
     END DO
   END IF

!***** Assign previous timestep concentrations to particulate organics

   DO I=1,MLOC
     CPOP(I,1) = POP1TM1S(I)
     CPOP(I,2) = POP2TM1S(I)
     CPOP(I,3) = POP3TM1S(I)
     CPON(I,1) = PON1TM1S(I)
     CPON(I,2) = PON2TM1S(I)
     CPON(I,3) = PON3TM1S(I)
     CPOC(I,1) = POC1TM1S(I)
     CPOC(I,2) = POC2TM1S(I)
     CPOC(I,3) = POC3TM1S(I)
     CPOS(I)   = PSITM1S(I)
   END DO

!***** Update sediment concentrations

   DO I=1,MLOC

!******* Assign previous timestep concentrations

     NH41TM1  = NH41TM1S(I)
     NO31TM1  = NO31TM1S(I)
     HS1TM1   = HS1TM1S(I)
     SI1TM1   = SI1TM1S(I)
     PO41TM1  = PO41TM1S(I)
     BENSTR1  = BENSTR1S(I)
     NH4T2TM1 = NH4T2TM1S(I)
     NO3T2TM1 = NO3T2TM1S(I)
     HST2TM1  = HST2TM1S(I)
     SIT2TM1  = SIT2TM1S(I)
     PO4T2TM1 = PO4T2TM1S(I)
     PON1TM1  = PON1TM1S(I)
     PON2TM1  = PON2TM1S(I)
     PON3TM1  = PON3TM1S(I)
     POC1TM1  = POC1TM1S(I)
     POC1     = POC1TM1
     POC2TM1  = POC2TM1S(I)
     POC3TM1  = POC3TM1S(I)
     POP1TM1  = POP1TM1S(I)
     POP2TM1  = POP2TM1S(I)
     POP3TM1  = POP3TM1S(I)
     PSITM1   = PSITM1S(I)
     dfeedm1  = dfeedm1s(I)
     CH4T2TM1 = CH4T2TM1S(I)           ! CH4
     CH41TM1  = CH41TM1S(I)            ! CH4
     SO4T2TM1 = SO4T2TM1S(I)           ! CH4


! ACCOUNT FOR SAV NUTRIENT UPTAKE
! NOTE THIS IS OVER ALL CELLS, NOT JUST SAV CELLS SO SEDNH4SAV
! MUST BE ZEROED OUT FOR ALL CELLS

!JQI     IF (SAV_CALC) THEN
!JQI       NH4T2TM1 = NH4T2TM1 - 1000.*SEDNH4SAV(I)*DLTS/HSED(I)
!JQI       PO4T2TM1 = PO4T2TM1 - 1000.*SEDPO4SAV(I)*DLTS/HSED(I)
!JQI     END IF
        
     BFORMAX  = BFORMAXS(I)
     ISWBEN   = ISWBENS(I)
     H2       = HSED(I)

!******* Sedimentation, mixing rates, and sediment temperature

     W2    = VSED(I)
     DPP    = VPMIX(I)
     DD    = VDMIX(I)
     TEMPD = CTEMP(I)
     STP20 = TEMPD-20.

!******* Convert overlying water column concentrations into mg/m**3

     IWC  = KBM1
     DF   = 1./(1.+KADPO4*SSI(I,IWC))
     PO4AVL = DF*PO4(I,IWC)
     PO40 = PO4AVL*1000.
     NH40 = NH4(I,IWC)*1000.
     NO30 = NO3(I,IWC)*1000.
     DF   = 1./(1.+KADSA*SSI(I,IWC))
     SI0  = DF*SA(I,IWC)*1000.
     O20  = AMAX1(DO(I,IWC),0.010)
     HS0  = COD(I,IWC)
     SAL  = SALT(I,IWC)

! lets flag DO if it starts near zero as it may cause root finder problems
! with suspension and deposit feeders on.

!vjp    if(do(iwc).lt.0.1.and.jday.lt.0.01)
!vjp .  write(6,*)'initial DO box',bb,' is ',do(iwc)

!       open(8,file='o20.initial')
!       write(8,*)bb,do(iwc)
!       if(bb.eq.729)stop

!       Regression function to get SO4 concentration from SAL
!       [SO4] = 20 mg/L          for        [Cl] < 6 mg/L
!             = (10/3)[Cl]       for        [Cl] > 6 mg/L
!       1 ppt = 607.445 mg/L Cl

     IF (SAL > 0.0099) THEN
       SO40MG = 20.0 + (27./190.)*607.445*SAL
     ELSE
       SO40MG = 20.0
     ENDIF

!******* Methane saturation

     CH4SAT = 99.*(1.+(ZD(I,IWC)+D(I)*DZ(IWC)+HSED(I))/10.)*0.9759**STP20

!******* Evaluate the temperature dependent coefficients

     ITEMP    = 10.*TEMPD+1

!******* Salinity dependence of nitrification and denitrification

     IF (SAL <= SALTND) THEN
       XAPPNH4  = ZHTANH4F(ITEMP)
       XAPP1NO3 = ZHTANO3F(ITEMP)
     ELSE
       XAPPNH4  = ZHTANH4S(ITEMP)
       XAPP1NO3 = ZHTANO3S(ITEMP)
     END IF
     XAPPD1   = ZHTAD1(ITEMP)
     XAPPP1   = ZHTAP1(ITEMP)
     XK2NO3   = ZHTA2NO3(ITEMP)*H2
     XKSI     = ZHTASI(ITEMP)*H2
     XAPPCH4  = ZHTACH4(ITEMP)
     KL12NOM  = DD/H2*ZL12NOM(ITEMP)
!???DDP=DPP??     W12NOM   = DDP/H2*ZW12NOM(ITEMP)*POC1/1.0E5
     W12NOM   = DPP/H2*ZW12NOM(ITEMP)*POC1/1.0E5
     IF (ISWBEN == 0) THEN
       IF (TEMPD >= TEMPBEN) THEN
         ISWBEN  = 1
         BFORMAX = 0.
       END IF
       BFOR = KMO2DP/(KMO2DP+O20)
     ELSE
       IF (TEMPD < TEMPBEN) THEN
         ISWBEN = 0
       END IF
       BFORMAX = AMAX1(KMO2DP/(KMO2DP+O20),BFORMAX)
       BFOR    = BFORMAX
     END IF
     BENSTR = (BENSTR1+DLTS*BFOR)/(1.+KBENSTR*DLTS)
!## -- add minimum mixing term and bio-irrigation formulation
!##
!##     W12    = W12NOM*(1.-KBENSTR*BENSTR)
!##     KL12   = KL12NOM
!## -- w12min= Dpmin/h2 is minimum particle mixing
     W12MIN = DPMIN/H2
     W12    = W12NOM*(1.-KBENSTR*BENSTR)+W12MIN
!## -- klbnth is ratio of bio-irrigation to bio-particle mixing
     KL12   = KL12NOM + KLBNTH*W12

!******* Lookup reaction rates

     ITEMP  = 10.*TEMPD+1
     XKPOP1 = ZHTAPOP1(ITEMP)*H2
     XKPOP2 = ZHTAPOP2(ITEMP)*H2
     XKPOP3 = ZHTAPOP3(ITEMP)*H2
     XKPON1 = ZHTAPON1(ITEMP)*H2
     XKPON2 = ZHTAPON2(ITEMP)*H2
     XKPON3 = ZHTAPON3(ITEMP)*H2
     XKPOC1 = ZHTAPOC1(ITEMP)*H2
     XKPOC2 = ZHTAPOC2(ITEMP)*H2
     XKPOC3 = ZHTAPOC3(ITEMP)*H2
!
! DEPOSIT FEEDER CALCULATION
!
!
! Deposit feeding ingestion rate 
!
     xki0=zhtai0(itemp)
!
! respiration rate
!
     xkr=zhtar(itemp)                           ! deposit feeders
!
! quadratic predation
!
     xkbeta=zhtabeta(itemp)                     ! deposit feeders


!MBM 970109 control switch for hypoxic effects
     if ( HYPOXFX ) then

       LOGICT=1.0/(1.0 + EXP(MAX(1.1*(DFDOh-o20)/(DFDOh-DFDOq),-25.)))

!  Reduce ingestion when o2 is low
!       xki0=xki0*(o20/(o20+xkmi0))
       xki0=xki0*LOGICT

! Mortality due to hypoxia (adds to sediment POM pools)
       rdd=4.6/tdd                   ! ln(1/100) for 99% kill in time tdd
       rmort = rdd*(1.0-logict)

! Reduce predation when o2 low
       xkbeta=xkbeta*o20/(o20+xkbo2)
     endif

!
! Growth rate limitation
!
     xpoc1lim=xkmg1/(poc1tm1+xkmg1)             ! deposit feeders
     xpoc2lim=xkmg2/(poc2tm1+xkmg2)             ! deposit feeders
!
! calculate deposit feeders biomass
!
     dfeed = dfeedm1 + dlts*                                &
           ( aa1*xki0/(m2*1e+09)*poc1tm1*xpoc1lim*dfeedm1   &  ! deposit feeders
           + aa2*xki0/(m2*1e+09)*poc2tm1*xpoc2lim*dfeedm1   &
           - xkr*dfeedm1                                    &
           - xkbeta*dfeedm1*dfeedm1                         &
           - rmort*dfeedm1)

     DF_GROW(I) = aa1*xki0/(m2*1e+09)*poc1tm1*xpoc1lim*dfeedm1    &
                 + aa2*xki0/(m2*1e+09)*poc2tm1*xpoc2lim*dfeedm1
     DF_RESP(I) = xkr*dfeedm1 
     DF_PRED(I) = xkbeta*dfeedm1*dfeedm1
     DF_MORT(I) = rmort*dfeedm1

! dont let go negative
     dfeed=max(dfeed,0.1)

!******* Calculate sediment concentrations

     DOH2=DLTS/H2

     frpon1=1.-(frpon(I,2)+frpon(I,3))
     frpop1=1.-(frpop(I,2)+frpop(I,3))
     frpoc1=1.-(frpoc(I,2)+frpoc(I,3))
     FD2  = 1./(1.+M2*PIE2SI)
     K3   = XKSI*(CSISAT-FD2*SIT2TM1)/(PSITM1+KMPSI)

     PON1 = (FLXPON(I,1)*DOH2+PON1TM1                      &
           -aa1*xki0/(m2*1e+09)*poc1tm1*xpoc1lim*dfeedm1    &    ! deposit feeders
                                       *dlts/h2/amcn        &
           +frpon1*(rmort*dfeedm1 + xkbeta*dfeedm1*dfeedm1) &
                                       *dlts/h2/amcn        &
           )/(1.+(XKPON1+W2)*DOH2)
     IF(PON1 < 0.0)PON1=0.0

     PON2 = (FLXPON(I,2)*DOH2+PON2TM1                       &
           -aa2*xki0/(m2*1e+09)*poc2tm1*xpoc2lim*dfeedm1    &    ! deposit feeders
                                       *dlts/h2/amcn        &
         +frpon(I,2)*(rmort*dfeedm1 + xkbeta*dfeedm1*dfeedm1)  &
                                       *dlts/h2/amcn              &
           )/(1.+(XKPON2+W2)*DOH2)
!JQIcheckPON=PON2???????     IF(PON2 < 0.0)PON=0.0
     IF(PON2 < 0.0)PON2=0.0

     PON3 = (FLXPON(I,3)*DOH2+PON3TM1)/(1.+(XKPON3+W2)*DOH2)

     POC1 = (FLXPOC(I,1)*DOH2+POC1TM1                       & 
           -aa1*xki0/(m2*1e+09)*poc1tm1*xpoc1lim*dfeedm1    &    ! deposit feeders
                                       *dlts/h2             &
           +frpoc1*(rmort*dfeedm1 + xkbeta*dfeedm1*dfeedm1) &
                                       *dlts/h2             & 
           )/(1.+(XKPOC1+W2)*DOH2)
     IF(POC1 < 0.0)POC1=0.0

     POC2 = (FLXPOC(I,2)*DOH2+POC2TM1                       &
           -aa2*xki0/(m2*1e+09)*poc2tm1*xpoc2lim*dfeedm1    &    ! deposit feeders
                                       *dlts/h2             &
           +frpoc(I,2)*(rmort*dfeedm1 + xkbeta*dfeedm1*dfeedm1)  &
                                       *dlts/h2             &
           )/(1.+(XKPOC2+W2)*DOH2)
     IF(POC2 < 0.0)POC2=0.0

     POC3 = (FLXPOC(I,3)*DOH2+POC3TM1)/(1.+(XKPOC3+W2)*DOH2)

     POP1 = (FLXPOP(I,1)*DOH2+POP1TM1                       &
           -aa1*xki0/(m2*1e+09)*poc1tm1*xpoc1lim*dfeedm1    &    ! deposit feeders
                                       *dlts/h2/amcp        &
           +frpop1*(rmort*dfeedm1 + xkbeta*dfeedm1*dfeedm1) &
                                       *dlts/h2/amcp        &
           )/(1.+(XKPOP1+W2)*DOH2)
     IF(POP1 < 0.0)POP1=0.0

     POP2 = (FLXPOP(I,2)*DOH2+POP2TM1                       &
           -aa2*xki0/(m2*1e+09)*poc2tm1*xpoc2lim*dfeedm1    &    ! deposit feeders
                                       *dlts/h2/amcp        &
           +frpop(I,2)*(rmort*dfeedm1 + xkbeta*dfeedm1*dfeedm1)  &
                                       *dlts/h2/amcp        &
           )/(1.+(XKPOP2+W2)*DOH2)
     IF(POP2 < 0.0)POP2=0.0

     POP3 = (FLXPOP(I,3)*DOH2+POP3TM1)/(1.+(XKPOP3+W2)*DOH2)

!## -- modification for detrital Si input to sediment
!##     PSI  = (FLXPOS(BB)*DLTS/H2+PSITM1)/(1.+(K3+W2)*DLTS/H2)
     PSI  = ((FLXPOS(I)+JSIDETR)*DOH2+PSITM1)/(1.+(K3+W2)*DOH2)

!******* Assign diagenesis values for sediment model

     XJN = XKPON1*PON1+XKPON2*PON2+XKPON3*PON3             &
          +XKR*DFEEDM1*(1./AMCN)                         !deposit feeders
     XJC = XKPOC1*POC1+XKPOC2*POC2+XKPOC3*POC3
     XJP = XKPOP1*POP1+XKPOP2*POP2+XKPOP3*POP3             &
          +XKR*DFEEDM1*(1./AMCP)                         !deposit feeders

! TEMPORARY BYPASS OF FLUX ALGORITHMS
!        GO TO 66666
        
!****** Evaluate the NH4, NO3, and SOD equations

     SOD = ZBRENT(IERR)

!       IF (IERR.NE.0.AND.BENTHIC_OUTPUT) WRITE(BFO,3000) IERR,BB

     IF(IERR /= 0) THEN
       dfsod = xkr*dfeedm1*2.667E-3       ! deposit feeders resp.
       sodmin =   0.0001
       sodmax = 100.
       IF(MSR)write(6,9000)   jday,ierr,bb,sal,so40mg,dfeed                     &
                     ,(sfeed(bb,jsf),jsf=1,3),sodmin,sodmax
       IF(MSR)write(6,9911) csodhs, csodch4, csod
9911    format(/1x,' csodhs, csodch4, csod'/3e10.3)
       IF(MSR)write(6,9910)  ch41,ch42,hst1,hs1,hs2
9910    format(/1x,' ch41   ch42   hst1   hs1   hs2'/5e10.3)
       if(ierr == 2)  then
         IF(MSR)write(6,9900)  jday,ctemp(i),pop1,pop2,pop3
         IF(MSR)write(6,9901)  pon1,pon2,pon3,poc1,poc2,poc3
         IF(MSR)write(6,9902)  po4t2,hst2,sit2,psi
         IF(MSR)write(6,9903)                                                    &
              (flxpop(i,1)+flxpop(i,2)+flxpop(i,3))                    &
             ,(flxpon(i,1)+flxpon(i,2)+flxpon(i,3))                    &
             ,(flxpoc(i,1)+flxpoc(i,2)+flxpoc(i,3))
         IF(MSR)write(6,9904)  o20,csod,dfsod,sod,s                              &
                       ,h2,HSED(i),VSED(i)
         IF(MSR)write(6,9905)  xjp,xjn,xjc,jo2nh4,xjc1
         IF(MSR)write(6,9906)  jpo4,jnh4,jno3,jhs,jsi,jch4aq,jch4g,benstr
         IF(MSR)write(6,9907)  po40,po41,po42,po4t2,nh40,nh41,nh42,nh4t2
         IF(MSR)write(6,9908)  no30,no31,no32,no3t2,hs1,hs2,hst2
         IF(MSR)write(6,9909)  si0,si1,si2,sit2
         STOP
       ELSE
         error=sedf(sodmin)
         IF(MSR)write(6,9889)  jday,sodmin,error
         IF(MSR)write(6,9900)  jday,ctemp(i),pop1,pop2,pop3
         IF(MSR)write(6,9901)  pon1,pon2,pon3,poc1,poc2,poc3
         IF(MSR)write(6,9902)  po4t2,hst2,sit2,psi
         IF(MSR)write(6,9903)                                                   &
              (flxpop(i,1)+flxpop(i,2)+flxpop(i,3))                   &
             ,(flxpon(i,1)+flxpon(i,2)+flxpon(i,3))                   &
             ,(flxpoc(i,1)+flxpoc(i,2)+flxpoc(i,3))
         IF(MSR)write(6,9904)  o20,csod,dfsod,sod,s                             &
                       ,h2,HSED(i),VSED(i)
         IF(MSR)write(6,9911) csodhs, csodch4, csod
         IF(MSR)write(6,9905)  xjp,xjn,xjc,jo2nh4,xjc1
         IF(MSR)write(6,9906)  jpo4,jnh4,jno3,jhs,jsi,jch4aq,jch4g,benstr
         IF(MSR)write(6,9907)  po40,po41,po42,po4t2,nh40,nh41,nh42,nh4t2
         IF(MSR)write(6,9908)  no30,no31,no32,no3t2,hs1,hs2,hst2
         IF(MSR)write(6,9909)  si0,si1,si2,sit2
         error=sedf(sodmax)
         IF(MSR)write(6,9889)  jday,sodmax,error
         IF(MSR)write(6,9900)  jday,ctemp(i),pop1,pop2,pop3
         IF(MSR)write(6,9901)  pon1,pon2,pon3,poc1,poc2,poc3
         IF(MSR)write(6,9902)  po4t2,hst2,sit2,psi
         IF(MSR)write(6,9903)                                                 &
              (flxpop(i,1)+flxpop(i,2)+flxpop(i,3))                 &
             ,(flxpon(i,1)+flxpon(i,2)+flxpon(i,3))                 & 
             ,(flxpoc(i,1)+flxpoc(i,2)+flxpoc(i,3))
         IF(MSR)write(6,9904)  o20,csod,dfsod,sod,s                           &
                       ,h2,HSED(i),VSED(i)
         IF(MSR)write(6,9911) csodhs, csodch4, csod
         IF(MSR)write(6,9905)  xjp,xjn,xjc,jo2nh4,xjc1
         IF(MSR)write(6,9906)  jpo4,jnh4,jno3,jhs,jsi,jch4aq,jch4g,benstr
         IF(MSR)write(6,9907)  po40,po41,po42,po4t2,nh40,nh41,nh42,nh4t2
         IF(MSR)write(6,9908)  no30,no31,no32,no3t2,hs1,hs2,hst2
         IF(MSR)write(6,9909)  si0,si1,si2,sit2
         STOP
       END IF
     END IF
9889 format(/5x,'Zbrent diagnostics at time =',f8.3,               &
            ' for sod =',f8.4,' error =',e12.3/)
9000 format(/                                                      &
            5x,'Zbrent failure at time =',f8.3,' with ierr=',i2/   &
            5x,'in sediment segment ir=',i5/                       &
            5x,'with Salt, SO40mg     =',2e10.3/                   &
            5x,'Dfeed=',f10.3,' Sfeed=',3f11.3/                    &
            5x,'(sodmin,sodmax=',f6.4,f6.1,')'/                    &
            5x,'Program termination follows diagnostic dumps')
9900 format(/1x,' time,ctemp,pop1,pop2,pop3'/8e10.3)
9901 format(/1x,' pon1,pon2,pon3,poc1,poc2,poc3'/ 8e10.3)
9902 format(/1x,' po4t2,hst2,sit2,psi'/8e10.3)
9903 format(/1x,' flxpop,flxpon,flxpoc'/8e10.3)
9904 format(/1x,' o20,csod,dfsod,sod,s,h2',',hsed,vsed'/10e10.3)
9905 format(/1x,' jp,jn,jc,jo2nh4,xjc1'/8e10.3)
9906 format(/1x,' jpo4,jnh4,jno3,jhs,jsi,jch4aq,jch4g,benstr'/ 8e10.3)
9907 format(/1x,' po40,po41,po42,po4t2,nh40,nh41,nh42,nh4t2'/8e10.3)
9908 format(/1x,' no30,no31,no32,no3t2,hs1,hs2,hst2'/8e10.3)
9909 format(/1x,' si0,si1,si2,sit2'/8e10.3)

!******* Accumulate remaining sums for steady-state computation

     IF (STEADY_STATE_SED) THEN
       ASDTMP(I) = ASDTMP(I)+TEMPD*DLTS
     END IF

!******* Evaluate the PO4 and Si equations

     K0H1D = 0.
     K0H1P = 0.
     KMC1  = 0.
     K1H1D = S
     K1H1P = 0.
     K2H2D = 0.
     K2H2P = 0.
     J1    = S*SI0

!******* Oxygen dependency of pie1

     IF (O20 < O2CRITSI) THEN
       PIE1 = PIE2SI*DPIE1SI**(O20/O2CRITSI)
     ELSE
       PIE1 = PIE2SI*DPIE1SI
     END IF
     PIE2 = PIE2SI

!******* Silica dissolution kinetics

     FD2 = 1./(1.+M2*PIE2)
     K3  = XKSI*PSI/(PSI+KMPSI)*FD2
     PF  = KADSA*SSI(I,IWC)/(1.+KADSA*SSI(I,IWC))
     J2  = XKSI*PSI/(PSI+KMPSI)*CSISAT                   &
           + FLX1WC*PF*SA(I,IWC)                         &
           + SF_SA(I)                                 ! Suspension Feeders
     CALL SEDTSFNL (SI1,SI2,SIT1,SIT2,SI1TM1,SIT2TM1)
     JSI = S*(SI1-SI0)

!******* Phosphate

     K0H1D = 0.
     K0H1P = 0.
     KMC1  = 0.
     K1H1D = S
     K1H1P = 0.
     K2H2D = 0.
     K2H2P = 0.
     J1    = S*PO40
     K3    = 0.
     PF    = KADPO4*SSI(I,IWC)/(1.+KADPO4*SSI(I,IWC))
     PIOP  = PF*PO4(I,IWC)
     J2    = XJP + FLX1WC*PIOP + FLX1WC*PIP(I,IWC) + SF_PIP(I)                              ! Suspension Feeders

!******* Salinity dependence of PIE1

     IF (SAL <= SALTSW) THEN
       DPIE1PO4=DPIE1PO4F
     ELSE
       DPIE1PO4=DPIE1PO4S
     END IF

!******* Oxygen dependency of pie1

     IF (O20 < O2CRIT) THEN
       PIE1 = PIE2PO4*DPIE1PO4**(O20/O2CRIT)
     ELSE
       PIE1 = PIE2PO4*DPIE1PO4
     ENDIF
     PIE2 = PIE2PO4
     CALL SEDTSFNL (PO41,PO42,PO4T1,PO4T2,PO41TM1,PO4T2TM1)
     JPO4 = S*(PO41-PO40)

!******* Assign flux-flux results to wqm arrays

     ITEMP      = 10*TEMPD+1
     IF (SAL <= SALTND) THEN
       XAPP1NO3 = ZHTANO3F(ITEMP)
     ELSE
       XAPP1NO3 = ZHTANO3S(ITEMP)
     END IF
     XK2NO3     = ZHTA2NO3(ITEMP)*H2
     BENDO(I)   = -SOD                      &
                  - SODSF(I)          ! suspension feeders
     MTVEL(I)   = SOD/O20
     BENNH4(I)  = JNH4/1000.                &
                  + JNH4SF(I)/1000.     ! suspension feeders
     BENNO3(I)  = JNO3/1000.
     BENPO4(I)  = JPO4/1000.                &
                  + JPO4SF(I)/1000.     ! suspension feeders
     BENDOC(I)  = 0.
     BENCOD(I)  = JHS+JCH4AQ
     BENCH4G(I) = JCH4G 
     BENCH4A(I) = JCH4AQ
     BENSA(I)   = JSI/1000.                 &
                  + JSASF(I)/1000.      ! suspension feeders
     BENDEN(I)  = (XAPP1NO3*XAPP1NO3*NO31/S+XK2NO3*NO32)/1000.

!******* Fluxes due to burial of particulates

     BURIALN(I) = (PON1+PON2+PON3+NO3T2+NH4T2)*W2
     BURIALP(I) = (POP1+POP2+POP3+PO4T2)*W2
     BURIALC(I) = (POC1+POC2+POC3)*W2

!******* Diagenesis of carbon forms

     DIAGENC(I) = XJC/1000.

     IF (BALGAE_CALC) THEN

!******* Benthic algae algorithms start here        

!******* Calculate mean light in algal mat

       BLITE(I) = IATBOT(I,IWC)*EXP(-KESED)/(KEBALG+1.0E-8)/BBM(I)   &
                   *(1. - EXP(-(KEBALG+1.0E-8)*BBM(I)))
        
!******* Temperature effects

       IF (T(I,IWC) < TMB) THEN
         FTB = EXP(-KTGB1*(T(I,IWC)-TMB)**2)
       ELSE
         FTB = EXP(-KTGB2*(TMB-T(I,IWC))**2)
       END IF
        
!******* Light effects

       IK = PMB*FTB/ALPHB
       FIB(I) = BLITE(I)/SQRT(IK*IK+BLITE(I)*BLITE(I)+1.0E-8)
        
!******* Nutrient limitations

! COMPUTE AVAILABLE AMMONIUM AND NITRATE

       NH4AVL = BENNH4(I)*DLTS + NH4(I,IWC)*D(I)*DZ(IWC)
       NH4AVL = MAX(0.,NH4AVL)
       NO3AVL = BENNO3(I)*DLTS + NO3(I,IWC)*D(I)*DZ(IWC)
       NO3AVL = MAX(0.,NO3AVL)

! COMPUTE NITROGEN LIMITATION 

       NLB(I) = (NH4AVL+NO3AVL)/(KHNB+NH4AVL+NO3AVL)

! COMPUTE NITROGEN PREFERENCE

       PRNB = NH4AVL*NO3AVL/((KHNB+NH4AVL)*(KHNB+NO3AVL))         &
             +NH4AVL*KHNB/((1.E-30+NH4AVL+NO3AVL)*(KHNB+NO3AVL))

!******* Phosphorus available for algal growth

       DF     = 1./(1.+KADPO4*SSI(I,IWC))
       PO4AVL = DF*PO4(I,IWC)*D(I)*DZ(IWC)
       PO4AVL = PO4AVL + BENPO4(I)*DLTS
       PO4AVL = MAX(0.,PO4AVL)
       PLB(I) = PO4AVL/(KHPB+PO4AVL)

!******* Base metabolism

! IF BIOMASS IS LESS THAN ALLOWED MINIMUM, SET METABOLISM TO ZERO

       IF (BBM(I) > BALGMIN) THEN
         BMB(I) = BMRB*EXP(KTBB*(T(I,IWC)-TRB))
       ELSE
         BMB(BB) = 0.
       END IF

!******* Production

       PB(I) = PMB*FTB*AMIN1(FIB(I),NLB(I),PLB(I))/CCHLB

!******* Net primary production
      
       NPPB(I) = (PB(I)-BMB(I))*BBM(I)                     
 
!******* Predation

! IF BIOMASS IS LESS THAN ALLOWED MINIMUM, SET PREDATION TO ZERO

       IF (BBM(I) > BALGMIN) THEN
         PRB(I) = BBM(I)*BPRB*EXP(KTBB*(T(I,IWC)-TRB))
       ELSE
         PRB(I) = 0.
       END IF

! ADJUST PREDATION SO BIOMASS DOESN'T GO NEGATIVE

       PRB(I) = MIN(PRB(I),PB(I)-BMB(I)+0.99/DLTS)              

!******* Compute effects of algal activity on benthic flux

       BANH4(I) = ANCB *(BMB(I)*FNIB - PRNB*PB(I)+ PRB(I)*FNIB) * BBM(I) 
       BANO3(I) = -(1. - PRNB) * PB(I) * ANCB * BBM(I)
       BAPO4(I) = APCB *(BMB(I)*FPIB - PB(I)+ PRB(I)*FPIB) * BBM(I) 
       FRDOB     = 1.-KHRB/(DO(I,IWC)+KHRB)
       BADO(I)  = ((1.3-0.3*PRNB)*PB(I)-FRDOB*BMB(I)) * AOCR*BBM(I)
       BADOC(I) = (1.-FRDOB)*BMB(I)*BBM(I)

! TEMPORARY FIX UP WHEN BENTHIC ALGAE ARE RUN WITHOUT DIAGENESIS     
!          BENNH4(BB) =  BANH4(BB)
!          BENNO3(BB) =  BANO3(BB) 
!          BENPO4(BB) =  BAPO4(BB)
!          BENDOC(BB) =  BADOC(BB)
!          BENDO(BB)  =  BADO(BB)  

       BENNH4(I) = BENNH4(I) + BANH4(I)
       BENNO3(I) = BENNO3(I) + BANO3(I) 
       BENPO4(I) = BENPO4(I) + BAPO4(I)
       BENDOC(I) = BENDOC(I) + BADOC(I)
       BENDO(I)  = BENDO(I)  + BADO(I)  

! COMPUTE EFFECTS OF ALGAL ACTIVITY ON ORGANIC PARTICULATES (MG/M**3)

       BAPOC(I) = PRB(I)*BBM(I)
       BAPON(I) = ANCB*(1.-FNIB)*(BMB(I)+PRB(I))*BBM(I)
       BAPOP(I) = APCB*(1.-FPIB)*(BMB(I)+PRB(I))*BBM(I)
          
       POC1 = POC1 + 1000. * BAPOC(I)*FRCPHB(1)*DLTS/H2
       POC2 = POC2 + 1000. * BAPOC(I)*FRCPHB(2)*DLTS/H2
       POC3 = POC3 + 1000. * BAPOC(I)*FRCPHB(3)*DLTS/H2
       PON1 = PON1 + 1000. * BAPON(I)*FRNPHB(1)*DLTS/H2
       PON2 = PON2 + 1000. * BAPON(I)*FRNPHB(2)*DLTS/H2
       PON3 = PON3 + 1000. * BAPON(I)*FRNPHB(3)*DLTS/H2
       POP1 = POP1 + 1000. * BAPOP(I)*FRPPHB(1)*DLTS/H2
       POP2 = POP2 + 1000. * BAPOP(I)*FRPPHB(2)*DLTS/H2
       POP3 = POP3 + 1000. * BAPOP(I)*FRPPHB(3)*DLTS/H2

!******* Accumulate fluxes for steady-state computation

       IF (STEADY_STATE_SED) THEN
        
         AG3CFL(I) = AG3CFL(I)+1000.*PRB(I)*FRCPHB(3)*BBM(I)*DLTS
         AG3NFL(I) = AG3NFL(I)+                                     &
                      1000. * PRB(I)*FRNPHB(3)*ANCB*BBM(I)*DLTS
         AG3PFL(I) = AG3PFL(I)+                                     &
                      1000. * PRB(I)*FRPPHB(3)*APCB*BBM(I)*DLTS
     
       END IF
        
!******* Change in benthic algal biomass

       BBM(I) = BBM(I) * (1. + DLTS*(PB(I)-BMB(I)-PRB(I)))

     END IF
! TEMPORARY FIX UP TO EXAMINE EFFECT OF SAV ON SEDIMENTS
66666   CONTINUE
!        NH4T2 = NH4T2TM1
!        PO4T2 = PO4T2TM1
!        NO3T2 = NO3T2TM1
!        HST2  = HST2TM1
! END TEMPORARY FIX UP

!******* Total sediment nutrient mass

     SEDMN = SEDMN+(PON1+PON2+PON3+NH4T2+NO3T2)*SFA(I)*H2/1.E6
     SEDMP = SEDMP+(POP1+POP2+POP3+PO4T2)*SFA(I)*H2/1.E6
     SEDMC = SEDMC+(POC1+POC2+POC3)*SFA(I)*H2/1.E6

!******* Replace the t minus 1 concentrations

     NH41TM1S(I)  = NH41
     NO31TM1S(I)  = NO31
     HS1TM1S(I)   = HS1
     SI1TM1S(I)   = SI1
     PO41TM1S(I)  = PO41
     BENSTR1S(I)  = BENSTR
     NH4T2TM1S(I) = NH4T2
     NO3T2TM1S(I) = NO3T2
     HST2TM1S(I)  = HST2
     SIT2TM1S(I)  = SIT2
     PO4T2TM1S(I) = PO4T2
     PON1TM1S(I)  = PON1
     PON2TM1S(I)  = PON2
     PON3TM1S(I)  = PON3
     POC1TM1S(I)  = POC1
     POC2TM1S(I)  = POC2
     POC3TM1S(I)  = POC3
     POP1TM1S(I)  = POP1
     POP2TM1S(I)  = POP2
     POP3TM1S(I)  = POP3
     PSITM1S(I)   = PSI
     BFORMAXS(I)  = BFORMAX
     ISWBENS(I)   = ISWBEN
     DFEEDM1S(I)  = DFEED
     CH4T2TM1S(I) = CH4T2               ! CH4
     CH41TM1S(I)  = CH41                ! CH4
     SO4T2TM1S(I) = SO4T2               ! CH4
     DIAGN(I)     = XJN                 ! CFC

   END DO

!***** Assign concentrations to plot variables

   DO I=1,MLOC
     CPON(I,1) = PON1TM1S(I)
     CPON(I,2) = PON2TM1S(I)
     CPON(I,3) = PON3TM1S(I)
     CNH4(I)   = NH4T2TM1S(I)
     CNO3(I)   = NO3T2TM1S(I)
     CPOP(I,1) = POP1TM1S(I)
     CPOP(I,2) = POP2TM1S(I)
     CPOP(I,3) = POP3TM1S(I)
     CPIP(I)   = PO4T2TM1S(I)
     CPOC(I,1) = POC1TM1S(I)
     CPOC(I,2) = POC2TM1S(I)
     CPOC(I,3) = POC3TM1S(I)
     CPOS(I)   = PSITM1S(I)
     CCH4(I)   = CH4T2TM1S(I)
     CSO4(I)   = SO4T2TM1S(I)
     CHS(I)    = HST2TM1S(I)                             !CFC
     CSI(I)    = SIT2TM1S(I)                             !CFC
   END DO

!***** Take temperature integration step

   DO I=1,MLOC
     CTEMP(I) = CTEMP(I)+DLT*DIFFT/HSED(I)/HSED(I)          &
                 *(T(I,KBM1)-CTEMP(I))
   END DO

   RETURN

!***** Compute and print out steady-state sediment concentrations

   ENTRY SED_INT

!***** Compute time-average values

   DO I=1,MLOC
     AG3CFL(I) = AG3CFL(I)/TINTIM
     AG3NFL(I) = AG3NFL(I)/TINTIM
     AG3PFL(I) = AG3PFL(I)/TINTIM
     ASDTMP(I) = ASDTMP(I)/TINTIM
   END DO

!***** Compute G3 organic concentrations

   DO I=1,MLOC
     CPOC(I,3) = AG3CFL(I)/(KCDIAG(3)*DCTHTA(3)**(ASDTMP(I)-20.)   &
                  *HSED(I)+VSED(I))
     CPON(I,3) = AG3NFL(I)/(KNDIAG(3)*DNTHTA(3)**(ASDTMP(I)-20.)   &
                  *HSED(I)+VSED(I))
     CPOP(I,3) = AG3PFL(I)/(KPDIAG(3)*DPTHTA(3)**(ASDTMP(I)-20.)   &
                  *HSED(I)+VSED(I))
   END DO

   RETURN
   END SUBROUTINE SED_CALC


!********************************************************************************
!**                          F U N C T I O N   S E D F                         **
!********************************************************************************

   FUNCTION SEDF(SOD1) RESULT(SEDFOUT)
   ! <ezpp-noinst>
   USE WQM
   IMPLICIT NONE
! added next line 10/17/05
   SAVE    
   REAL :: SOD1, SEDFOUT
   REAL(8) AD(4,4), BX(4), G(2), H(2,2)
   REAL(8) DBLSO41, DBLSO42, RA0, RA1, RA2, R1, R2, DISC, SN1

!***** Compute the NH4, NO3, and SOD fluxes

   S = SOD1/O20

!***** Ammonia flux

   K0H1P = 0.
   K1H1P = 0.
   K2H2D = 0.
   K2H2P = 0.
   IF (KMNH4 /= 0.) THEN
     K0H1D = XAPPNH4**2/S*KMNH4*(O20/(KMNH4O2+O20))
     K1H1D = S
   ELSE
     K1H1D = XAPPNH4**2/S*(O20/(KMNH4O2+O20))+S
     K0H1D = 0.
   ENDIF
   J1   = S*NH40
   K3   = 0.
   J2   = XJN
   PIE1 = PIENH4
   PIE2 = PIENH4
   KMC1 = KMNH4
   CALL SEDTSFNL (NH41,NH42,NH4T1,NH4T2,NH41TM1,NH4T2TM1)
   JNH4 = S*(NH41-NH40)

!***** Oxygen consumed by nitrification

   A1 = 0.0045714
   IF (KMNH4 /= 0.) THEN
     JO2NH4 = A1*K0H1D*NH41/(KMNH4+NH41TM1)
   ELSE
     JO2NH4 = A1*(K1H1D-S)*NH41
   ENDIF

!***** Denitrification

   K0H1D = 0.
   K0H1P = 0.
   KMC1  = 0.
   K1H1D = XAPP1NO3**2/S+S
   K1H1P = 0.
   K2H2D = XK2NO3
   K2H2P = 0.
   IF (KMNH4 /= 0.) THEN
     J1 = S*NO30+XAPPNH4**2/S*KMNH4*(O20/(KMNH4O2+O20))*NH41        &
          /(KMNH4+NH41TM1)
   ELSE
     J1 = S*NO30+XAPPNH4**2/S*(O20/(KMNH4O2+O20))*NH41
   END IF
   K3   = 0.
   J2   = 0.
   PIE1 = 0.
   PIE2 = 0.
   CALL SEDTSFNL(NO31,NO32,NO3T1,NO3T2,NO31TM1,NO3T2TM1)
   JNO3 = S*(NO31-NO30)

!***** Sulfide/methane oxidation

   A2      = 0.00285714
   XJCNO31 = A2*XAPP1NO3**2/S*NO31
   XJCNO3  = A2*XK2NO3*NO32

!***** Add the aerobic and first anaerobic layer to keep mass balance

   XJCNO3 = XJCNO31+XJCNO3

!***** Convert carbon diagenesis flux to O2 units

   XJC1 = AMAX1(2.667E-3*XJC-XJCNO3,1.0e-10)
         
!**** **********************************************************
!**** New code for methane formation.  CH4 starts forming
!**** once all sulfate is used up.
!**** **********************************************************

!**** Sulfide and sulfate in O2 equivalents
!**** units: so4 in o2 equivalents
!     SO4 (mg so4/L)* 1 mmol SO4 /98 mg SO4 * 2 mmol O2/ 1 mmol SO4
!     * 32 mg O2 / mmol O2= 0.65306122

   SO40=SO40MG*0.65306122
   K0H1D=0.
   K0H1P=0.
   KMC1=0.0
   K1H1D=XAPPD1**2/S*(O20/KMHSO2) + S
   K1H1P=XAPPP1**2/S*(O20/KMHSO2)
   K2H2D=0.
   K2H2P=0.
   J1=0.
   K3=0.0
   J2=XJC1
   PIE1=PIE1S
   PIE2=PIE2S

!**** Set KL12 using H for SO4
   ITEMP = 10.*TEMPD+1
   DDSO4 = ZL12NOM(ITEMP)*DD             ! Fixed by CFC as per JF 11/11/06
   HSO4  =SQRT(2.*DDSO4*SO40*H2/XJC1)

!**** No deeper than H2
   IF(HSO4 > H2) HSO4=H2
   KL12SO4=KL12*H2/HSO4

!**** Fractions and overall decay reaction velocity
   FD1=1./(1.+M1*PIE1)
   FP1=M1*PIE1/(1.+M1*PIE1)
   FD2=1./(1.+M2*PIE2)
   FP2=M2*PIE2/(1.+M2*PIE2)
   FP1SO4=FP1
   FP2SO4=FP2
   KHS_1=FP1*XAPPP1**2/S*(O20/KMHSO2)+FD1*XAPPD1**2/S*(O20/KMHSO2)

   BX(1) = DBLE(S)*DBLE(SO40)
   BX(2) = DBLE(H2)*DBLE(SO4T2TM1)/DBLE(DLTS)
   BX(3) = DBLE(HS0)*DBLE(S)
   BX(4) = DBLE(H2)*DBLE(HST2TM1)/DBLE(DLTS)

   AD(1,1) = -DBLE(S)-DBLE(KL12SO4)
   AD(1,2) = DBLE(KL12SO4)
   AD(1,3) = DBLE(KHS_1)
   AD(2,1) = DBLE(KL12SO4)
   AD(2,2) = -(DBLE(DLTS)*DBLE(KL12SO4)+DBLE(H2))/DBLE(DLTS)
   AD(3,3) = -DBLE(W2)-DBLE(FP1)*DBLE(W12)-DBLE(FD1)*DBLE(S)         &
             -DBLE(FD1)*DBLE(KL12SO4)-DBLE(KHS_1)
   AD(3,4) = DBLE(FP2)*DBLE(W12)+DBLE(FD2)*DBLE(KL12SO4)
   AD(4,3) = DBLE(W2)+DBLE(FP1)*DBLE(W12)+DBLE(FD1)*DBLE(KL12SO4)
   AD(4,4) = -(DBLE(DLTS)*DBLE(FP2)*DBLE(W12)                        &
             +DBLE(DLTS)*DBLE(FD2)*DBLE(KL12SO4)+DBLE(DLTS)*DBLE(W2) &
             +DBLE(H2)) /DBLE(DLTS)

   G(1) = ((BX(1)*AD(3,3) - AD(1,3)*BX(3))*AD(4,4) -                 &
          BX(1)*AD(3,4)*AD(4,3) + AD(1,3)*AD(3,4)*BX(4) +            &
          AD(1,3)*BX(2)*AD(3,4))/(AD(1,3)*AD(3,4))

   G(2) = ((BX(1)*AD(3,3) - AD(1,3)*BX(3))*AD(4,4) -                 &
          BX(1)*AD(3,4)*AD(4,3) + AD(1,3)*AD(3,4)*BX(4))             &
          /(AD(1,3)*AD(3,4))

   H(1,1) = (AD(1,1)*AD(3,3)*AD(4,4)-AD(1,1)*AD(3,4)*AD(4,3)+AD(1,3) &
            *AD(2,1)*AD(3,4))/(AD(1,3)*AD(3,4))
   H(1,2) = (AD(1,2)*AD(3,3)*AD(4,4)-AD(1,2)*AD(3,4)*AD(4,3)+AD(1,3) &
            *AD(2,2)*AD(3,4))/(AD(1,3)*AD(3,4))
   H(2,1) = (AD(1,1)*AD(3,3)*AD(4,4)-AD(1,1)*AD(3,4)*                &
             AD(4,3))/(AD(1,3)*AD(3,4))
   H(2,2) = (AD(1,2)*AD(3,3)*AD(4,4)-AD(1,2)*AD(3,4)*                &
             AD(4,3))/(AD(1,3)*AD(3,4))

   RA0 = (H(1,1)*G(2)-G(1)*H(2,1))*DBLE(KMSO4)
   RA1 = - G(1)*H(2,1) + H(1,1)*G(2) +                               &
         (H(1,1)*H(2,2)-H(1,2)*H(2,1))*DBLE(KMSO4) + H(1,1)*J2
   RA2 = H(1,1)*H(2,2)-H(1,2)*H(2,1)

   SN1 = 1.                          !solution of a2*x^2+a1*x+a0
   IF (RA1 <= 0.0) SN1 = -1.        !see Num Rec p178
   DISC = -(RA1+SN1*DSQRT(RA1**2-4.0D0*RA2*RA0))/2.0D0
   IF (DABS(DISC) /= 0.0D0) THEN
     R1 = DISC / RA2
     R2 = RA0 / DISC
   ELSE   ! vjp 11/17/2005 added logic for case (a2*a0) = 0
     IF (DABS(RA2) == 0.0D0) THEN   !  a2 = 0
       R1 = -RA0/RA1
       R2 = R1
     ELSE                           !  a0 = 0
       R1 = -RA1/RA2
       R2 = 0.0D0
     END IF
   END IF

   DBLSO42 = R1
   IF (DBLSO42 < 0.) DBLSO42 = R2

   DBLSO41 = -(H(1,2)*DBLSO42+G(1))/H(1,1)
   HST1 = -(AD(1,2)*DBLSO42+AD(1,1)*DBLSO41+BX(1))/AD(1,3)
   HST2 = (AD(1,2)*AD(3,3)*DBLSO42+AD(1,1)*AD(3,3)*DBLSO41+BX(1)    &
          *AD(3,3)-AD(1,3)*BX(3))/(AD(1,3)*AD(3,4))
   HS1=FD1*HST1
   HS2=FD2*HST2
   HS2AV=FD2*HST2
   SO42=DBLSO42
   SO42AV=SO42
   SO4T2 = SO42
   SO41=DBLSO41
   XJ2=J2*KMSO4/(SO42+KMSO4)
   XJ2CH4=XJ2
   X1J2=J2*DBLSO42/(SO42+KMSO4)
   JHS=S*(HS1-HS0)
   CSODHS=(XAPPD1**2/S*FD1 + XAPPP1**2/S*FP1)*(O20/KMHSO2)*HST1

!**** Methane
   CH40 = 0.
   K0H1P=0.
   K1H1P=0.
   K2H2D=0.
   K2H2P=0.
   K1H1D=XAPPCH4**2/S*(O20/(KMCH4O2+O20))+S
   K0H1D=0.
   J1=S*CH40
   K3=0.0
   J2=XJ2
   PIE1=0.0
   PIE2=0.0
   KMC1=0.0

   CALL SEDSSFNL(CH41,CH42,CH42AV,CH4T1,CH4T2,CH4T2AV,CH41TM1,CH4T2TM1,1)

   IF(CH42 > CH4SAT) THEN
     CH42=CH4SAT
     CH41 = (CH40*S**2+CH42*KL12*S)/                                  &
            (S**2+KL12*S+XAPPCH4**2*(O20/(KMCH4O2+O20)))
   ENDIF

!**** Calculate changes in CH4 and HS stored in the sediment
   DCH4T2 = (CH4T2 - CH4T2TM1)*H2/DLTS
   DHST2  = (HST2 - HST2TM1)*H2/DLTS

!**** Calculate CSOD
   CSODCH4 = XAPPCH4**2/S*(O20/(KMCH4O2+O20))*CH41
   CSOD    = CSODCH4+CSODHS

!**** Calculate Fluxes
   JCH4      = S*(CH41-CH40)
   JCH4AQ    = S*CH41
   FLUXHS    = S*FD1*HS1
   FLUXHSCH4 = JCH4AQ + FLUXHS

!**** If not flux or SOD or stored then it must escape as gas flux
   JCH4G = 0.
   IF (CH42 == CH4SAT) THEN
     JCH4G = XJ2CH4 - DCH4T2 - CSODCH4 - JCH4AQ
   END IF

!**** Volumetric methane and total gas flux (L/m2-d)
   VJCH4G=22.4/64.*JCH4G
!   JGAS=JN2GAS+VJCH4G                   ! jn2gas not computed

!***** SOD function

   DFSOD = XKR*DFEEDM1*2.667E-3       ! deposit feeders

   SOD  = CSOD+JO2NH4                                    &
          + DFSOD                         ! deposit feeders
   SEDFOUT = SOD-SOD1

   RETURN
   END FUNCTION SEDF


!********************************************************************************
!**                        F U N C T I O N   Z B R E N T                       **
!********************************************************************************

   FUNCTION ZBRENT(IERR) RESULT(ZBOUT)
   ! <ezpp-noinst>
   IMPLICIT NONE
   INTEGER,PARAMETER  :: IMAX=100
   REAL,PARAMETER     :: EPS=3.E-8, TOL=1.E-5
   INTEGER            :: IERR, I
   REAL               :: ZBOUT, SODMIN, SODMAX, TOL1
   REAL               :: A, B, C, D, E, P, Q, R, XM
   REAL               :: FA, FB, FC
!
   SODMIN = 1.E-4
   SODMAX = 100.
   ZBOUT  = 0.0
!***** Initialize upper and lower limits for solution

   IERR = 0
   A    = SODMIN
   B    = SODMAX
   FA   = SEDF(A)
   FB   = SEDF(B)

!***** Root must bracket ZBRENT

   IF (FB*FA > 0.) THEN
     IERR = 1
     RETURN
   ENDIF
   FC = FB
   DO I=1,IMAX
     IF (FB*FC > 0.) THEN
       C  = A
       FC = FA
       D  = B-A
       E  = D
     END IF
     IF (ABS(FC) < ABS(FB)) THEN
       A  = B
       B  = C
       C  = A
       FA = FB
       FB = FC
       FC = FA
     END IF
     TOL1 = 2.*EPS*ABS(B)+0.5*TOL
     XM   = 0.5*(C-B)
     IF (ABS(XM) <= TOL1 .OR. FB == 0.) THEN
       ZBOUT  = B
       RETURN
     END IF
     IF (ABS(E) >= TOL1 .AND. ABS(FA) > ABS(FB)) THEN
       S = FB/FA
       IF (A == C) THEN
         P = 2.*XM*S
         Q = 1.-S
       ELSE
         Q = FA/FC
         R = FB/FC
         P = S*(2.*XM*Q*(Q-R)-(B-A)*(R-1.))
         Q = (Q-1.)*(R-1.)*(S-1.)
       END IF
       IF (P > 0.) Q = -Q
       P = ABS(P)
       IF (2.*P < MIN(3.*XM*Q-ABS(TOL1*Q),ABS(E*Q))) THEN
         E = D
         D = P/Q
       ELSE
         D = XM
         E = D
       END IF
     ELSE
       D = XM
       E = D
     END IF
     A  = B
     FA = FB
     IF (ABS(D) > TOL1) THEN
       B = B+D
     ELSE
       B = B+SIGN(TOL1,XM)
     ENDIF
     FB = SEDF(B)
   END DO
   IERR   = 2
   ZBOUT  = B

   RETURN
   END FUNCTION ZBRENT



!********************************************************************************
!**                    S U B R O U T I N E   S E D T S F N L                   **
!********************************************************************************

   SUBROUTINE SEDTSFNL(C1S,C2S,CT1S,CT2S,C1TM1S,CT2TM1S)
   ! <ezpp-noinst>
   USE WQM
   USE FILE_INFO
   IMPLICIT NONE
   REAL :: C1S,C2S,CT1S,CT2S,C1TM1S,CT2TM1S
   REAL :: A11, A12, A21, A22, B_1, B_2
   REAL :: F12, F21, XK0, XK1, XK2, DELTA

!***** Initialize constants
     
   FD1 = 1./(1.+M1*PIE1)
   FP1 = M1*PIE1/(1.+M1*PIE1)
   FD2 = 1./(1.+M2*PIE2)
   FP2 = M2*PIE2/(1.+M2*PIE2)
   F12 = W12*FP1+KL12*FD1
   F21 = W12*FP2+KL12*FD2

!***** Evaluate the MM term at time level t-1

   IF (KMC1 /= 0.) THEN
     XK0 = (K0H1D*FD1+K0H1P*FP1)/(KMC1+C1TM1S)
   ELSE
     XK0 = 0.
   END IF
   XK1 = XK0+K1H1D*FD1+K1H1P*FP1
   XK2 = K2H2D*FD2+K2H2P*FP2
   A11 = -F12-XK1-W2
   A21 = F12+W2
   A12 = F21
   B_1 = -J1
   A22 = -F21-XK2-W2-K3-H2/DLTS
   B_2 = -J2-H2/DLTS*CT2TM1S

!***** Solve the 2x2 set of linear equations

   DELTA = A11*A22-A12*A21
   IF (DELTA == 0.) THEN
     IF(MSR)PRINT *,'Twod is singular: A11,A12,A21,A22'
     IF(MSR)write(*,*) f12, xk1, w2
     IF(MSR)write(*,*) w12, fp1, kl12, fd1
     IF(MSR)write(*,*) xk0, k1h1d, fd1, k1h1p, fp1
        
     IF(MSR)PRINT *,A11,A12,A21,A22
     STOP
   END IF

!***** Assign results

   CT1S = (B_1*A22-B_2*A12)/DELTA
   CT2S = (B_2*A11-B_1*A21)/DELTA
   C1S  = FD1*CT1S
   C2S  = FD2*CT2S
 
   RETURN
   END SUBROUTINE SEDTSFNL


!********************************************************************************
!**                    S U B R O U T I N E   S E D S S F N L                   **
!********************************************************************************

   SUBROUTINE SEDSSFNL(C1,C2,C2AV,CT1,CT2,CT2AV,C1TM1,CT2TM1,ITYPE)
   ! <ezpp-noinst>
   IMPLICIT NONE
   INTEGER ITYPE
   REAL :: C1,C2,C2AV,CT1,CT2,CT2AV,C1TM1,CT2TM1

!    This subroutine translates between SEDTSFNL and SEDF
!    This is called by some sections of the new code

   CALL SEDTSFNL (C1,C2,CT1,CT2,C1TM1,CT2TM1)

   C2AV  = C2
   CT2AV = CT2

   RETURN
   END SUBROUTINE SEDSSFNL

   END MODULE SED
