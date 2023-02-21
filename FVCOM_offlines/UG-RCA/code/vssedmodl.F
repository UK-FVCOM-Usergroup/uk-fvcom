 MODULE FLUXES
 IMPLICIT NONE
 SAVE
 
 !REAL, ALLOCATABLE :: DEPFLUX(:,:),CFLUXS(:,:) !Tian
 
 END MODULE FLUXES

 

!OPTION CROSSREF ON
!OPTION SYMTABLE ON
!PAGEWIDTH 132
      SUBROUTINE SED_READ

      USE ALL_VARS,ONLY : CARAY,CDARAY,M,MT,SERIAL,PAR,MSR,MYID,NPROCS,VX,VY
      USE MOD_RCA
      USE MOD_SED
      USE FLUXES
      USE ALLOCEUTRO,ONLY: PCRB1,PCRB2,PCRB3,NCRB1,NCRB2,NCRB3,SCRB1,SCRB2,SCRB3, &
                           DEPFLUX,CFLUXS !Tian     
# if defined (MULTIPROCESSOR)
!      USE MOD_PAR ,ONLY : NMAP,PWRITE,NHN,NGID,HN_LST,GATHER,MSR,SEDWRITE
      USE MOD_PAR,ONLY : NMAP,PWRITE,NHN,NGID,HN_LST,GATHER,MSR,MPI_COMM_WORLD,SEDWRITE      
# endif
      IMPLICIT NONE
      SAVE
!
!*******************************************************************************
!
!             VARIABLE STOICHIOMETRY SEDIMENT SUBMODEL - VSSEDMODL
!             ----------------------------------------------------
!
!                      ORIGINALLY DEVELOPED FOR THE CHESAPEAKE BAY
!
!   ORGANIC FLUX TO SEDIMENT ==> DIAGENESIS ==> INORGANIC FLUX TO WATER COLUMN
!             3-G MODEL - G1=LABILE, G2=REFRACTORY, G3=CONSERVATIVE
!
!*******************************************************************************
!
!  INPUTS
!  ------
!             REQUIRED INPUTS FOR SEDIMENT SUB-MODEL
!
!  A.  PASSED TO SEDIMENT SUBROUTINE FROM WATER QUALITY SUBROUTINE
!
!    1.  OVERLYING WATER COLUMN CONCENTRATIONS OF AMMONIA, NITRATE,
!        SALINITY, DISSOLVED OXYGEN, PHOSPHORUS AND SILICA
!    2.  OVERLYING WATER COLUMN SEGMENT VOLUME (V1)
!    3.  OVERLYING WATER COLUMN SEGMENT DEPTH (BL(ISEG,3))
!    4.  OVERLYING WATER COLUMN SEGMENT TEMPERATURE
!    8.  ALGAL PHOSPHORUS TO CARBON, NITROGEN TO CARBON, AND SILICA
!        TO CARBON RATIOS
!    9.  DEPTH OF OVERLYING WATER COLUMN
!
!  B.  INPUTS SUPPLIED VIA DIRECT INPUT TO THE SEDIMENT SUBROUTINE
!
!     VARIABLE NAMES        DESCRIPTION                            UNITS
!     --------------   ----------------------------------------    -----
!           HSED       DEPTH OF SEDIMENT LAYER                        CM
!          DIFFT       WATER COLUMN-SEDIMENT LAYER DIFFUSION     CM2/SEC
!                      COEFFICIENT FOR TEMPERATURE
!         SALTSW       SALINITY SWITCH: (ARCHAIC: FORMERLY USED TO   PPT
!                      DETERMINE WHETHER METHANE OR SULFIDE SOD
!                      FORMULATION IS TO BE USED); NOW AFFECTS
!                      BENTHIC NITRIF AND DE-NITRIF RATES
!         XXSNET       NET SETTLING VELOCITY FROM WATER COLUMN
!
!  POM FLUX RELATED
!                      FRACTIONS OF G1, G2, AND G3 CONTAINED IN ...
!        FRPPH1(3)     ALGAL GROUP NO.1 PHOSPHORUS
!        FRPPH2(3)     ALGAL GROUP NO.2 PHOSPHORUS
!        FRPPH3(3)     ALGAL GROUP NO.3 PHOSPHORUS
!        FRPOP(3)      NON-ALGAL PARTICULATE ORGANIC PHOSPHORUS
!        FRNPH1(3)     ALGAL GROUP NO.1 NITROGEN
!        FRNPH2(3)     ALGAL GROUP NO.2 NITROGEN
!        FRNPH3(3)     ALGAL GROUP NO.3 NITROGEN
!        FRPON(3)      NON-ALGAL PARTICULATE ORGANIC NITROGEN
!        FRCPH1(3)     ALGAL GROUP NO.1 CARBON
!        FRCPH2(3)     ALGAL GROUP NO.2 CARBON
!        FRCPH3(3)     ALGAL GROUP NO.3 CARBON
!        FRPOC(3)      NON-ALGAL PARTICULATE ORGANIC CARBON
!
!  DIAGENESIS RELATED
!        KPDIAG(3)     REACTION RATES FOR POP G1, G2, AND G3        /DAY
!        DPTHTA(3)     TEMPERATURE THETAS FOR POP G1, G2, AND G3
!        KNDIAG(3)     REACTION RATES FOR PON G1, G2, AND G3        /DAY
!        DNTHTA(3)     TEMPERATURE THETAS FOR PON G1, G2, AND G3
!        KCDIAG(3)     REACTION RATES FOR POC G1, G2, AND G3        /DAY
!        DCTHTA(3)     TEMPERATURE THETAS FOR POC G1, G2, AND G3
!                      
!
!  FLUX-FLUX RELATED
!        VPMIX(NSED)   SOLID-PHASE MIXING RATE                  M**2/DAY
!        VDMIX(NSED)   DISSOLVED-PHASE MIXING RATE              M**2/DAY
!           M1         CONCENTRATION OF SS IN LAYER 1            KG SS/L
!           M2         CONCENTRATION OF SS IN LAYER 2            KG SS/L
!          PIE1S       PARTITION COEFFICIENT ASSOCIATED             L/KG
!                      SULFIDE SORPTION IN LAYER 1
!          PIE2S       PARTITION COEFFICIENT ASSOCIATED             L/KG
!                      SULFIDE SORPTION IN LAYER 2
!          KLN2        NITRATE TRANSFER COEFFICIENT BETWEEN        M/DAY
!                        SEDIMENT LAYERS 1 AND 2
!        REACTION VELOCITIES FOR...
!          KAPPD1      DISSOLVED SULFIDE IN LAYER 1                M/DAY
!          KAPPP1      PARTICULATE SULFIDE IN LAYER 1              M/DAY
!          KAPPNH4     AMMONIA NITROGEN                            M/DAY
!          KP1NO3      AEROBIC DENITRIFICATION                     M/DAY
!          KP2NO3      ANAEROBIC DENITRIFICATION                   M/DAY
!          KAPPC       METHANE                                     M/DAY
!          KAPPD       METHANE DIFFUSION MASS TRANSFER             M/DAY
!
!
!  OUTPUT
!  ------
!     THE SUBROUTINE RETURNS FLUXES FOR....
!      JSOD, JAQSOD, JCH4AQ AND JCH4G [GM O2*/M2-DAY]
!      JNH4, JPO4, JNO3 AND JSI [MG/M2-DAY]
!
!***********************************************************************
!
!      INCLUDE  'RCACM'
!      INCLUDE  'SEDCM'

      CHARACTER(LEN=25) :: SSNAME(25)
      CHARACTER(LEN=24) :: FRNAME(9)
      CHARACTER(LEN=40) :: SEDFILNA
      CHARACTER(LEN=80) :: COMMENT
      CHARACTER(LEN=4 ) :: TWARPSED
      CHARACTER(LEN=8 ) :: DUMPNAMES(76)
      INTEGER  ISEDPRNT,PIESIOPT,PIEPO4OPT
      REAL  PGFRAC(3),NGFRAC(3),CGFRAC(3),H1S(MT)

      EXTERNAL  SEDF

!        WATER COLUMN STATE-VARIABLES
      REAL, ALLOCATABLE ::                                &
            SAL(:,:)  ,NO23(:,:),DO(:,:)
!JQI     .      SAL(:,:)  ,PHYT1(:,:) ,PHYT2(:,:) ,PHYT3(:,:)
!JQI     .    , RPOP(:,:) ,LPOP(:,:)  ,RDOP(:,:)  ,LDOP(:,:)
!JQI     .    , PO4T(:,:) ,RPON(:,:)  ,LPON(:,:)  ,RDON(:,:)
!JQI     .    , LDON(:,:) ,NH4T(:,:)  ,NO23(:,:)  ,BSI(:,:)
!JQI     .    , SIT(:,:)  ,RPOC(:,:)  ,LPOC(:,:)  ,RDOC(:,:)
!JQI     .    , LDOC(:,:) ,EXDOC(:,:) ,REPOC(:,:) ,REDOC(:,:)
!JQI     .    , O2EQ(:,:) ,DO(:,:)
 
!        GLOBAL DUMP AVERAGE VARIABLES
      REAL,DIMENSION(MT) ::                                    &
           CTEMP_GDA,POP1R_GDA,POP2R_GDA,POP3R_GDA,POPR_GDA    &
          ,PON1R_GDA,PON2R_GDA,PON3R_GDA,PONR_GDA,POC1R_GDA    &
	  ,POC2R_GDA,POC3R_GDA,POCR_GDA,PO4T2R_GDA,HST2R_GDA   &
          ,SIT2R_GDA,PSIAVR_GDA,FLXPOP_GDA,FLXPON_GDA,FLXPOC_GDA   &
	  ,FLXPOS_GDA,FLXPO4T2S_GDA,FLXSIT2S_GDA
      REAL,DIMENSION(MT) ::                                    &
           FLXPOP1_GDA,FLXPOP2_GDA,FLXPOP3_GDA,FLXPON1_GDA     &
	  ,FLXPON2_GDA,FLXPON3_GDA,FLXPOC1_GDA,FLXPOC2_GDA     &
	  ,FLXPOC3_GDA
      REAL,DIMENSION(MT) ::                                    &
           O20_GDA,CSOD_GDA,SOD_GDA,S_GDA,JP_GDA,JN_GDA,JC_GDA   &
	  ,JO2NH4_GDA,XJCO2AV_GDA,XJC1AV_GDA
      REAL,DIMENSION(MT) ::                                    &
           JPO4_GDA,JNH4_GDA,JNO3_GDA,JH2S_GDA,JSI_GDA,JCH4AQ_GDA  &
	  ,JCH4G_GDA
     ! REAL  JN2_GDA(NXY),H1_GDA(NXY)
      REAL  JN2_GDA(MT),H1_GDA(MT)      
      REAL,DIMENSION(MT) ::                                    &
           PO40_GDA,PO41_GDA,PO42_GDA,PO4T2_GDA,NH40_GDA       &
	  ,NH41_GDA,NH42_GDA,NH4T2_GDA,NO30_GDA,NO31_GDA       &
          ,NO32_GDA,NO3T2_GDA,HS1_GDA,HS2_GDA,HST2_GDA         &
          ,SI0_GDA,SI1_GDA,SI2_GDA,SIT2_GDA
      REAL,DIMENSION(MT) ::                                    &
           JN2,CH41_GDA,CH42_GDA,CH4T2_GDA,SO41_GDA,SO42_GDA,SO4T2_GDA	

      REAL,DIMENSION(NXY,1) :: TEMP2   
      REAL,DIMENSION(MT,1) :: TEMP1  
!        REAL AND LABELED COMMON FOR LINKING WATER COLUMN AND SEDIMENT SUBMODELS
!        LABELED COMMON FOR EUTROPHICATION ALGAL GROWTH ROUTINES
      REAL,DIMENSION(MT,NZ) :: PO4,NH4,SI
!JQI      EQUIVALENCE
!JQI     .    (CKINARRAY(1,1,1),PO4(1,1))
!JQI     .   ,(CKINARRAY(1,1,2),NH4(1,1))
!JQI     .   ,(CKINARRAY(1,1,3),SI(1,1))
    !  REAL,DIMENSION(MT,NZ) ::                                 &   !Temporal
    !     PCRB1,PCRB2,PCRB3,NCRB1,NCRB2,NCRB3,SCRB1,SCRB2,SCRB3
!JQI      COMMON /EUTRO/                                           &
!JQI         PCRB1,PCRB2,PCRB3,NCRB1,NCRB2,NCRB3,SCRB1,SCRB2,SCRB3
!JQI      REAL,DIMENSION(MT) ::                                    &
!JQI         SODS,FLXHS,FLXNH4,FLXPO4,FLXNO3,FLXSI,FLXCH4AQ,FLXCH4G
!JQI      EQUIVALENCE                                              &
!JQI         (CFLUXS(1,1),SODS(1))     , (CFLUXS(1,2),FLXHS(1))    &
!JQI        ,(CFLUXS(1,3),FLXCH4AQ(1)) , (CFLUXS(1,4),FLXCH4G(1))  &
!JQI        ,(CFLUXS(1,5),FLXNH4(1))   , (CFLUXS(1,6),FLXNO3(1))   &
!JQI        ,(CFLUXS(1,7),FLXPO4(1))   , (CFLUXS(1,8),FLXSI(1))

!JQI      EQUIVALENCE
!JQI     .   (CARAY(1,1,1),SAL(1,1))    , (CARAY(1,1,2),PHYT1(1,1))
!JQI     . , (CARAY(1,1,3),PHYT2(1,1))  , (CARAY(1,1,4),PHYT3(1,1)) 
!JQI     . , (CARAY(1,1,5),RPOP(1,1))   , (CARAY(1,1,6),LPOP(1,1)) 
!JQI     . , (CARAY(1,1,7),RDOP(1,1))   , (CARAY(1,1,8),LDOP(1,1))
!JQI     . , (CARAY(1,1,9),PO4T(1,1))
!JQI     . , (CARAY(1,1,10),RPON(1,1))  , (CARAY(1,1,11),LPON(1,1))
!JQI     . , (CARAY(1,1,12),RDON(1,1))  , (CARAY(1,1,13),LDON(1,1))
!JQI     . , (CARAY(1,1,14),NH4T(1,1))  , (CARAY(1,1,15),NO23(1,1))
!JQI     . , (CARAY(1,1,16),BSI(1,1))   , (CARAY(1,1,17),SIT(1,1))
!JQI     . , (CARAY(1,1,18),RPOC(1,1))  , (CARAY(1,1,19),LPOC(1,1))
!JQI     . , (CARAY(1,1,20),RDOC(1,1))  , (CARAY(1,1,21),LDOC(1,1))
!JQI     . , (CARAY(1,1,22),EXDOC(1,1)) , (CARAY(1,1,23),REPOC(1,1))
!JQI     . , (CARAY(1,1,24),REDOC(1,1)) , (CARAY(1,1,25),O2EQ(1,1))
!JQI     . , (CARAY(1,1,26),DO(1,1))

      DATA  SSNAME/                                                     &
        '    Temperature [Deg C]    ' , '     G1 POP [mg P/m**3]    ' , &
        '     G1 PON [mg N/m**3]    ' , '     G1 POC [mg C/m**3]    ' , &
        '     G2 POP [mg P/m**3]    ' , '     G2 PON [mg N/m**3]    ' , &
        '     G2 POC [mg C/m**3]    ' , '     G3 POP [mg P/m**3]    ' , &
        '     G3 PON [mg N/m**3]    ' , '     G3 POC [mg C/m**3]    ' , &
        '  Biogenic Si [mg Si/m**3] ' , ' Dissolved PO4 [mg P/m**3] ' , &
        ' Dissolved NH4 [mg N/m**3] ' , ' Dissolved NO3 [mg N/m**3] ' , &
        'Dissolved H2S [mg O2*/m**3]' , ' Dissolved Si [mg Si/m**3] ' , &
        '      Benthic Stress       ' , ' Dissolved CH4 [mg O2/m**3]' , &
        '    Sulfate [mg O2/m**3]   ' , '                           ' , &
        '                           ' , '                           ' , &
        '                           ' , '                           ' , &
        '                           '/

      DATA   FRNAME/                                                    &
          'WINTER PHYTO PHOSPHORUS ','SUMMER PHYTO PHOSPHORUS '         &
         ,'DETRITAL ORG PHOSPHORUS '                                    &
         ,'WINTER PHYTO NITROGEN   ','SUMMER PHYTO NITROGEN   '         &
         ,'DETRITAL ORG NITROGEN   '                                    &
         ,'WINTER PHYTO CARBON     ','SUMMER PHYTO CARBON     '         &
         ,'DETRITAL ORG CARBON     '/
      DATA  DUMPNAMES  /                                                &
         ' sedtemp' , '  G1 POP' , '  G2 POP' , '  G3 POP' , 'Totl POP' &
       , '  G1 PON' , '  G2 PON' , '  G3 PON' , 'Totl PON' , '  G1 POC' &
       , '  G2 POC' , '  G3 POC' , 'Totl POC' , 'Ly2 TPO4' , 'Ly2 TH2S' &
       , 'Lyr2 TSi' , 'BiogenSi' , '    JPOP' , '    JPON' , '    JPOC' &
       , '     O20' , '    CSOD' , '     SOD' , 's-MasTrf' , 'JPdiagen' &
       , 'JNdiagen' , 'JCdiagen' , '  JO2NH4' , ' XJCO2AV' , '  XJC1AV' &
       , '    JPO4' , '    JNH4' , '    JNO3' , '     JHS' , '     JSI' &
       , '  JCH4AQ' , '   JCH4G' , '      H1' , '    PO40' , '    PO41' &
       , '    PO42' , '   PO4T2' , '    NH40' , '    NH41' , '    NH42' &
       , '   NH4T2' , '    NO30' , '    NO31' , '    NO32' , '   NO3T2' &
       , '     HS1' , '     HS2' , '    HST2' , '     SI0' , '     SI1' &
       , '     SI2' , '    SIT2' , '     JN2' , '    CH41' , '    CH42' &
       , '   CH4T2' , '    SO41' , '    SO42' , '   SO4T2' , '  JPOPG1' &
       , '  JPOPG2' , '  JPOPG3' , '  JPONG1' , '  JPONG2' , '  JPONG3' &
       , '  JPOCG1' , '  JPOCG2' , '  JPOCG3' , '   JPBSi' , ' JdepPO4' &
       , '  JdepSi' /

      INTEGER IPRNTSED,IGDSEDOPT,ISCALPSED,IPRNTSEDSECS
      INTEGER IGDCNT,NXPRTSED,M1M2OPT,IGDCT
      REAL PRNTSED,DIFFT,SODMIN,SODMAX,EPS
      REAL TERM,PPFLX,PNFLX,PCFLX,SAREA,STP20
      REAL OWCDPH,error,sedf,zbrent
      REAL pop1r,pop2r,pop3r,popr,po4t2r,hst2r,sit2r,psiavr
      REAL pon1r,pon2r,pon3r,ponr,poc1r,poc2r,poc3r,pocr,bnthden
      REAL si0z,si1z,si2z,sit2z,po40z,po41z,po42z,po4t2z
      REAL hs1z,hs2z,hst2z,TOTJSOD
      INTEGER I,IG,ITER,ierr,itemp,iseddisk,nh40z,nh41z,nh42z,nh4t2z
      INTEGER no30z,no31z,no32z,no3t2z
!             INITIALIZATION

      REAL,ALLOCATABLE :: HSED_TMP(:)
# if defined (MULTIPROCESSOR)
!      REAL,ALLOCATABLE,DIMENSION(:) :: CTEMP_TMP    ,CPOS_TMP     ,PO4T2TM1S_TMP     &
!          ,NH4T2TM1S_TMP,NO3T2TM1S_TMP,HST2TM1S_TMP ,SIT2TM1S_TMP ,BNTHSTR1S_TMP     &
!	  ,PO4T1TM1S_TMP,NH4T1TM1S_TMP,NO3T1TM1S_TMP,SIT1TM1S_TMP ,HST1TM1S_TMP      &
!          ,CH4T1TM1S_TMP,CH4T2TM1S_TMP,CH41TM1S_TMP ,SO4T2TM1S_TMP,PO41TM1S_TMP      &
!	  ,NH41TM1S_TMP ,NO31TM1S_TMP ,HS1TM1S_TMP  ,SI1TM1S_TMP  ,O20TM1S_TMP       &
!	  ,SODTM1S_TMP  ,DD0TM1S_TMP  ,BFORMAXS_TMP ,ISWBNTHS_TMP
      REAL,DIMENSION(NXY) :: CTEMP_TMP    ,CPOS_TMP     ,PO4T2TM1S_TMP     &
          ,NH4T2TM1S_TMP,NO3T2TM1S_TMP,HST2TM1S_TMP ,SIT2TM1S_TMP ,BNTHSTR1S_TMP     &
	  ,PO4T1TM1S_TMP,NH4T1TM1S_TMP,NO3T1TM1S_TMP,SIT1TM1S_TMP ,HST1TM1S_TMP      &
          ,CH4T1TM1S_TMP,CH4T2TM1S_TMP,CH41TM1S_TMP ,SO4T2TM1S_TMP,PO41TM1S_TMP      &
	  ,NH41TM1S_TMP ,NO31TM1S_TMP ,HS1TM1S_TMP  ,SI1TM1S_TMP  ,O20TM1S_TMP       &
	  ,SODTM1S_TMP  ,DD0TM1S_TMP  ,BFORMAXS_TMP ,ISWBNTHS_TMP	  
	  
!      REAL,ALLOCATABLE,DIMENSION(:,:) :: CPOP_TMP,CPON_TMP,CPOC_TMP
      REAL,DIMENSION(NXY,3) :: CPOP_TMP,CPON_TMP,CPOC_TMP

!      REAL, DIMENSION(NXY) :: &
!            POP1R_TMP,POP2R_TMP,POP3R_TMP,POPR_TMP,PON1R_TMP,PON2R_TMP,PON3R_TMP  &
!	   ,PONR_TMP,POC1R_TMP,POC2R_TMP,POC3R_TMP,POCR_TMP,PO4T2R_TMP,HST2R_TMP,SIT2R_TMP &
!	   ,PSIAVR_TMP,FLXPOP_TMP,FLXPON_TMP,FLXPOC_TMP,O20_TMP,CSOD_TMP,SOD_TMP,S_TMP  &
!	   ,JP_TMP,JN_TMP,JC_TMP,JO2NH4_TMP,XJCO2AV_TMP,XJC1AV_TMP,JPO4_TMP,JNH4_TMP,JNO3_TMP  &
!	   ,JH2S_TMP,JSI_TMP,JCH4AQ_TMP,JCH4G_TMP,H1_TMP,PO40_TMP,PO41_TMP,PO42_TMP,PO4T2_TMP  &
!	   ,NH40_TMP,NH41_TMP,NH42_TMP,NH4T2_TMP,NO30_TMP,NO31_TMP,NO32_TMP,NO3T2_TMP,HS1_TMP  &
!	   ,HS2_TMP,HST2_TMP,SI0_TMP,SI1_TMP,SI2_TMP,SIT2_TMP,JN2_TMP,CH41_TMP,CH42_TMP,CH4T2_TMP  &
!	   ,SO41_TMP,SO42_TMP,SO4T2_TMP,FLXPOP1_TMP,FLXPOP2_TMP,FLXPOP3_TMP,FLXPON1_TMP,FLXPON2_TMP  &
!	   ,FLXPON3_TMP,FLXPOC1_TMP,FLXPOC2_TMP,FLXPOC3_TMP,FLXPOS_TMP,FLXPO4T2S_TMP,FLXSIT2S_TMP

# endif
      
      ALLOCATE(SAL(0:MT,NZ))   !Tian
!JQI      ALLOCATE(PHYT1(MT,NZ));PHYT1
!JQI      ALLOCATE(PHYT2(MT,NZ));PHYT2
!JQI      ALLOCATE(PHYT3(MT,NZ));PHYT3
!JQI      ALLOCATE(RPOP(MT,NZ));RPOP
!JQI      ALLOCATE(LPOP(MT,NZ));LPOP
!JQI      ALLOCATE(RDOP(MT,NZ));
!JQI      ALLOCATE(LDOP(MT,NZ));
!JQI      ALLOCATE(PO4T(MT,NZ));
!JQI      ALLOCATE(RPON(MT,NZ));
!JQI      ALLOCATE(LPON(MT,NZ));
!JQI      ALLOCATE(RDON(MT,NZ));
!JQI!JQI      ALLOCATE(LDON(MT,NZ));
!JQI      ALLOCATE(NH4T(MT,NZ));
      ALLOCATE(NO23(0:MT,NZ))   !Tian
!JQI      ALLOCATE(BSI(MT,NZ));
!JQI      ALLOCATE(SIT(MT,NZ));
!JQI      ALLOCATE(RPOC(MT,NZ));
!JQI      ALLOCATE(LPOC(MT,NZ));
!JQI      ALLOCATE(RDOC(MT,NZ));
!JQI      ALLOCATE(LDOC(MT,NZ));
!JQI      ALLOCATE(EXDOC(MT,NZ));
!JQI      ALLOCATE(REPOC(MT,NZ));
!JQI      ALLOCATE(REDOC(MT,NZ));
!JQI      ALLOCATE(O2EQ(MT,NZ));
      ALLOCATE(DO(0:MT,NZ))   !Tian

      SAL(:,:)   = CARAY(:,:,1) 
!JQI      PHYT1(:,:) = CARAY(:,:,2)
!JQI      PHYT2(:,:) = CARAY(:,:,3)
!JQI      PHYT3(:,:) = CARAY(:,:,4)
!JQI      RPOP(:,:)  = CARAY(:,:,5) 
!JQI      LPOP(:,:)  = CARAY(:,:,6)
!JQI      RDOP(:,:)  = CARAY(:,:,7)
!JQI      LDOP(:,:)  = CARAY(:,:,8)
!JQI      PO4T(:,:)  = CARAY(:,:,9)
!JQI      RPON(:,:)  = CARAY(:,:,10)
!JQI      LPON(:,:)  = CARAY(:,:,11)
!JQI      RDON(:,:)  = CARAY(:,:,12) 
!JQI      LDON(:,:)  = CARAY(:,:,13)
!JQI      NH4T(:,:)  = CARAY(:,:,14)
      NO23(:,:)  = CARAY(:,:,15)
!JQI      BSI(:,:)   = CARAY(:,:,16)  
!JQI      SIT(:,:)   = CARAY(:,:,17)
!JQI      RPOC(:,:)  = CARAY(:,:,18) 
!JQI      LPOC(:,:)  = CARAY(:,:,19)
!JQI      RDOC(:,:)  = CARAY(:,:,20)
!JQI      LDOC(:,:)  = CARAY(:,:,21)
!JQI      EXDOC(:,:) = CARAY(:,:,22)
!JQI      REPOC(:,:) = CARAY(:,:,23)
!JQI      REDOC(:,:) = CARAY(:,:,24)
!JQI      O2EQ(:,:)  = CARAY(:,:,25)
      DO(:,:)    = CARAY(:,:,26)  

      
!        OPEN FILES FOR SEDIMENT DUMPS

      IF(MSR)THEN
       OPEN(14,FILE='RCAF14',FORM='UNFORMATTED')
       OPEN(16,FILE='RCAFICSED',FORM='UNFORMATTED')
       IF(MASSBAL == 1) OPEN(18,FILE='RCAFSEDBAL',FORM='UNFORMATTED')

!      WRITE -FSM- (LAND-MASK) TO RCAF14
      ! IF(MSR) WRITE(14)  FSM,76
      END IF 
!     do 1069 iy = 1,NY
!     WRITE(14,1068)  (FSM(ix,iy),ix=1,NX)
!1068 format(53f4.0)
!1069 continue

!        SET NUMBER OF LAYERS FOR SEDIMENT MODEL => 2
!        (AEROBIC + 1 ANAEROBIC)
      
      nsedlyr = 2
      xnsedlyr = nsedlyr

      OPEN(40,FILE=KINFILNA(1),FORM='FORMATTED')
      print*,'FILE= ',KINFILNA(1)

!        PRINT CONTROLS
      READ(40,'(A80)')   COMMENT
      READ(40,'(2I10,6X,A4,I10)') ISEDPRNT,IPRNTSED,TWARPSED,IGDSEDOPT
      IF(TWARPSED /= 'SECS' .AND. TWARPSED /= 'secs' .AND.             &
         TWARPSED /= 'MINS' .AND. TWARPSED /= 'mins' .AND.             &
         TWARPSED /= 'HRS ' .AND. TWARPSED /= 'hrs ' .AND.             &
         TWARPSED /= ' HRS' .AND. TWARPSED /= ' hrs' .AND.             &
         TWARPSED /= 'DAYS' .AND. TWARPSED /= 'days')  THEN
       IF(MSR) WRITE(OUT,9001)  TWARPSED
 9001  FORMAT(/5X,'THE UNITS CHOSEN FOR TWARPSED ',A4,' ARE NOT VALID'/ &
         5X,'RCA TERMINATED')
!       CALL EXIT
       CALL PSTOP('vss362    ')
      ENDIF
      IF(TWARPSED == 'SECS'.OR.TWARPSED == 'secs')  ISCALPSED=1
      IF(TWARPSED == 'MINS'.OR.TWARPSED == 'mins')  ISCALPSED=60
      IF(TWARPSED == 'HRS '.OR.TWARPSED == 'hrs ')  ISCALPSED=3600
      IF(TWARPSED == ' HRS'.OR.TWARPSED == ' hrs')  ISCALPSED=3600
      IF(TWARPSED == 'DAYS'.OR.TWARPSED == 'days')  ISCALPSED=86400
      IPRNTSEDSECS=ISCALPSED*IPRNTSED
      PRNTSED=IPRNTSEDSECS/86400.
      IF(MSR)THEN
       WRITE(OUT,1110)   IPRNTSEDSECS,PRNTSED
       IF(IGDSEDOPT == 0)  WRITE(OUT,'(5X,"USER SELECTED NO GLOBAL DUMP AVERAGING OPTION")')
       IF(IGDSEDOPT == 1)  WRITE(OUT,'(5X,"USER SELECTED GLOBAL DUMP AVERAGING OPTION")')
      END IF 
 1110 FORMAT(//5X,'SEDIMENT DUMP PRINT INTERVAL',I7,' SECONDS (',E12.3,' DAYS)')
      IGDCNT=0
      NXPRTSED = IPRNTSEDSECS
      IF(TZERO /= 0.)  NXPRTSED = ITIMESECS+IPRNTSEDSECS
!        Initialize variables for Global Dump Averaging, if required
      IF(IGDSEDOPT == 1) THEN
       CTEMP_GDA = 0. ;POP1R_GDA  = 0. ;POP2R_GDA = 0. 
       POP3R_GDA = 0. ;POPR_GDA   = 0. ;PON1R_GDA = 0. 
       PON2R_GDA = 0. ;PON3R_GDA  = 0. ;PONR_GDA  = 0. 
       POC1R_GDA = 0. ;POC2R_GDA  = 0. ;POC3R_GDA = 0.
       POCR_GDA  = 0. ;PO4T2R_GDA = 0. ;HST2R_GDA = 0. 
       SIT2R_GDA = 0. ;PSIAVR_GDA = 0.

       FLXPOP_GDA  = 0. ;FLXPOP1_GDA = 0. ;FLXPOP2_GDA = 0.
       FLXPOP3_GDA = 0. ;FLXPON_GDA  = 0. ;FLXPON1_GDA = 0.
       FLXPON2_GDA = 0. ;FLXPON3_GDA = 0. ;FLXPOC_GDA  = 0.
       FLXPOC1_GDA = 0. ;FLXPOC2_GDA = 0. ;FLXPOC3_GDA = 0.
       FLXPOS_GDA  = 0. ;FLXPO4T2S   = 0. ;FLXSIT2S    = 0.
       O20_GDA     = 0. ;CSOD_GDA    = 0. ;SOD_GDA     = 0.
       S_GDA       = 0.

       JP_GDA     = 0. ;JN_GDA      = 0. ;JC_GDA     = 0. 
       JO2NH4_GDA = 0. ;XJCO2AV_GDA = 0. ;XJC1AV_GDA = 0.
       JPO4_GDA   = 0. ;JNH4_GDA    = 0. ;JNO3_GDA   = 0.
       JH2S_GDA   = 0. ;JSI_GDA     = 0. ;JCH4AQ_GDA = 0.
       JCH4G_GDA  = 0.

       H1_GDA    = 0. ;PO40_GDA  = 0. ;PO41_GDA  = 0.
       PO42_GDA  = 0. ;PO4T2_GDA = 0. ;NH40_GDA  = 0.
       NH41_GDA  = 0. ;NH42_GDA  = 0. ;NH4T2_GDA = 0.
       NO30_GDA  = 0. ;NO31_GDA  = 0. ;NO32_GDA  = 0.
       NO3T2_GDA = 0. ;HS1_GDA   = 0. ;HS2_GDA   = 0.
       HST2_GDA  = 0. ;SI0_GDA   = 0. ;SI1_GDA   = 0.
       SI2_GDA   = 0. ;SIT2_GDA  = 0.

       JN2_GDA   = 0. ;CH41_GDA = 0. ;CH42_GDA = 0.
       CH4T2_GDA = 0. ;SO41_GDA = 0. ;SO42_GDA = 0.
       SO4T2_GDA = 0.
      ENDIF

!       GET MODEL COEFFICIENTS REQUIRED FOR SEDIMENT MODEL

!       SEDIMENT DEPTH
      ALLOCATE(HSED_TMP(NXY))
      
      READ(40,'(A80)')   COMMENT
      READ(40,'(8F10.0)',ERR=900) (HSED_TMP(IXY),IXY=1,NXY)

!       COMPUTE SEDIMENT VOLUMES ==> M**3
      IF(SERIAL)THEN
       DO IXY=1,NXY
        HSED(IXY) = HSED_TMP(IXY)
!        SEDIMENT VOLUME (M**3)
        BSVOL(IXY) = 0.01*HSED(IXY)*XAZ(IXY)
!        SET BFORMAX
        BFORMAXS(IXY) = 0.0
        ISWBNTHS(IXY) = 0
       END DO
      END IF 

# if defined (MULTIPROCESSOR)
      IF(PAR)THEN
       DO IXY=1,M
        HSED(IXY) = HSED_TMP(NGID(IXY))
!        SEDIMENT VOLUME (M**3)
        BSVOL(IXY) = 0.01*HSED(IXY)*XAZ(IXY)
!        SET BFORMAX
        BFORMAXS(IXY) = 0.0
        ISWBNTHS(IXY) = 0
       END DO
       DO IXY=1,NHN
!JQI2017
!        HSED(IXY) = HSED_TMP(HN_LST(IXY))
!!        SEDIMENT VOLUME (M**3)
!        BSVOL(IXY+M) = 0.01*HSED(IXY)*XAZ(IXY+M)
        HSED(IXY+M) = HSED_TMP(HN_LST(IXY))
!        SEDIMENT VOLUME (M**3)
        BSVOL(IXY+M) = 0.01*HSED(IXY+M)*XAZ(IXY+M)
!JQI2017
!        SET BFORMAX
        BFORMAXS(IXY+M) = 0.0
        ISWBNTHS(IXY+M) = 0
       END DO
      END IF	
# endif

      DEALLOCATE(HSED_TMP)
      
      IF(MSR)THEN
       WRITE(OUT,'(///34X,"SEDIMENT-WATER COLUMN LINKAGES AND SEDIMENT DEPTHS AND VOLUMES"/)')
       WRITE(OUT,'(10X,"SEDIMENT SEGMENT DEPTHS (CM)")')
      END IF 
!      IF(ISEDPRNT == 1) CALL RCAPRNT(HSED,1,1,MT)             !Tian
!      IF(MSR) WRITE(OUT,'(10X,"SEDIMENT VOLUMES (M**3)")')      !Tian
!      IF(ISEDPRNT == 1) CALL RCAPRNT(BSVOL,1,1,MT)           !Tian

!      CONVERT DEPTHS TO METERS

      HSED = 0.01*HSED
 
!        READ SEDIMENT INITIAL CONDITIONS
      IF(MSR) WRITE(OUT,'(////38X,"S E D I M E N T   I N I T I A L   C O N D I T I O N S"/)')

!       INITIAL CONDITIONS

      CALL ICREAD(NXY,OUT,ISEDPRNT,CTEMP,SSNAME(1))
      CALL ICREAD(NXY,OUT,ISEDPRNT,CPOP(1,1),SSNAME(2))
      CALL ICREAD(NXY,OUT,ISEDPRNT,CPON(1,1),SSNAME(3))
      CALL ICREAD(NXY,OUT,ISEDPRNT,CPOC(1,1),SSNAME(4))
      CALL ICREAD(NXY,OUT,ISEDPRNT,CPOP(1,2),SSNAME(5))
      CALL ICREAD(NXY,OUT,ISEDPRNT,CPON(1,2),SSNAME(6))
      CALL ICREAD(NXY,OUT,ISEDPRNT,CPOC(1,2),SSNAME(7))
      CALL ICREAD(NXY,OUT,ISEDPRNT,CPOP(1,3),SSNAME(8))
      CALL ICREAD(NXY,OUT,ISEDPRNT,CPON(1,3),SSNAME(9))
      CALL ICREAD(NXY,OUT,ISEDPRNT,CPOC(1,3),SSNAME(10))
      CALL ICREAD(NXY,OUT,ISEDPRNT,CPOS,SSNAME(11))
      CALL ICREAD(NXY,OUT,ISEDPRNT,PO4T2TM1S,SSNAME(12))
      CALL ICREAD(NXY,OUT,ISEDPRNT,NH4T2TM1S,SSNAME(13))
      CALL ICREAD(NXY,OUT,ISEDPRNT,NO3T2TM1S,SSNAME(14))
      CALL ICREAD(NXY,OUT,ISEDPRNT,HST2TM1S,SSNAME(15))
      CALL ICREAD(NXY,OUT,ISEDPRNT,SIT2TM1S,SSNAME(16))
      CALL ICREAD(NXY,OUT,ISEDPRNT,BNTHSTR1S,SSNAME(17))

      CALL ICREAD(NXY,OUT,ISEDPRNT,PO4T1TM1S,SSNAME(12))
      CALL ICREAD(NXY,OUT,ISEDPRNT,NH4T1TM1S,SSNAME(13))
      CALL ICREAD(NXY,OUT,ISEDPRNT,NO3T1TM1S,SSNAME(14))
      CALL ICREAD(NXY,OUT,ISEDPRNT,SIT1TM1S,SSNAME(16))
      CALL ICREAD(NXY,OUT,ISEDPRNT,HST1TM1S,SSNAME(15))
      CALL ICREAD(NXY,OUT,ISEDPRNT,CH4T1TM1S,SSNAME(18))
      CALL ICREAD(NXY,OUT,ISEDPRNT,CH4T2TM1S,SSNAME(18))
      CALL ICREAD(NXY,OUT,ISEDPRNT,SO4T2TM1S,SSNAME(19))

      CH41TM1S = CH4T1TM1S

      IF(CYCLE == 1) THEN
       REWIND(16)
       IF(SERIAL)THEN
        READ(16)   CTEMP,CPOP,CPON,CPOC,CPOS                      &
                  ,PO4T2TM1S,NH4T2TM1S,NO3T2TM1S                  &
                  ,HST2TM1S,SIT2TM1S,BNTHSTR1S                    &
                  ,PO4T1TM1S,NH4T1TM1S,NO3T1TM1S                  &
                  ,SIT1TM1S,HST1TM1S                              &
                  ,CH4T1TM1S,CH4T2TM1S,CH41TM1S,SO4T2TM1S         &
                  ,PO41TM1S,NH41TM1S,NO31TM1S,HS1TM1S,SI1TM1S     &
                  ,O20TM1S,SODTM1S,DD0TM1S                        &
                  ,BFORMAXS,ISWBNTHS
       END IF		  
# if defined (MULTIPROCESSOR)
       IF(PAR)THEN
!        ALLOCATE(CTEMP_TMP(NXY),CPOS_TMP(NXY),PO4T2TM1S_TMP(NXY))
!        ALLOCATE(NH4T2TM1S_TMP(NXY),NO3T2TM1S_TMP(NXY),HST2TM1S_TMP(NXY))
!	ALLOCATE(SIT2TM1S_TMP(NXY),BNTHSTR1S_TMP(NXY),PO4T1TM1S_TMP(NXY))
!	ALLOCATE(NH4T1TM1S_TMP(NXY),NO3T1TM1S_TMP(NXY),SIT1TM1S_TMP(NXY))
!	ALLOCATE(HST1TM1S_TMP(NXY) ,CH4T1TM1S_TMP(NXY),CH4T2TM1S_TMP(NXY))
!	ALLOCATE(CH41TM1S_TMP(NXY) ,SO4T2TM1S_TMP(NXY),PO41TM1S_TMP(NXY))
!	ALLOCATE(NH41TM1S_TMP(NXY) ,NO31TM1S_TMP(NXY) ,HS1TM1S_TMP(NXY))
!	ALLOCATE(SI1TM1S_TMP(NXY)  ,O20TM1S_TMP(NXY)  ,SODTM1S_TMP(NXY))
!	ALLOCATE(DD0TM1S_TMP(NXY)  ,BFORMAXS_TMP(NXY) ,ISWBNTHS_TMP(NXY))
!        ALLOCATE(CPOP_TMP(NXY,3),CPON_TMP(NXY,3),CPOC_TMP(NXY,3))
        READ(16)   CTEMP_TMP,CPOP_TMP,CPON_TMP,CPOC_TMP,CPOS_TMP                  &
                  ,PO4T2TM1S_TMP,NH4T2TM1S_TMP,NO3T2TM1S_TMP                      &
                  ,HST2TM1S_TMP,SIT2TM1S_TMP,BNTHSTR1S_TMP                        &
                  ,PO4T1TM1S_TMP,NH4T1TM1S_TMP,NO3T1TM1S_TMP                      &
                  ,SIT1TM1S_TMP,HST1TM1S_TMP                                      &
                  ,CH4T1TM1S_TMP,CH4T2TM1S_TMP,CH41TM1S_TMP,SO4T2TM1S_TMP         &
                  ,PO41TM1S_TMP,NH41TM1S_TMP,NO31TM1S_TMP,HS1TM1S_TMP,SI1TM1S_TMP &
                  ,O20TM1S_TMP,SODTM1S_TMP,DD0TM1S_TMP                            &
                  ,BFORMAXS_TMP,ISWBNTHS_TMP
        DO IXY=1,M
         CTEMP(IXY)     = CTEMP_TMP(NGID(IXY))     ;CPOS_TMP(IXY)  = CPOS_TMP(NGID(IXY))
	 PO4T2TM1S(IXY) = PO4T2TM1S_TMP(NGID(IXY)) ;NH4T2TM1S(IXY) = NH4T2TM1S_TMP(NGID(IXY))
	 NO3T2TM1S(IXY) = NO3T2TM1S_TMP(NGID(IXY)) ;HST2TM1S(IXY)  = HST2TM1S_TMP(NGID(IXY))
	 SIT2TM1S(IXY)  = SIT2TM1S_TMP(NGID(IXY))  ;BNTHSTR1S(IXY) = BNTHSTR1S_TMP(NGID(IXY))
	 PO4T1TM1S(IXY) = PO4T1TM1S_TMP(NGID(IXY)) ;NH4T1TM1S(IXY) = NH4T1TM1S_TMP(NGID(IXY))
	 NO3T1TM1S(IXY) = NO3T1TM1S_TMP(NGID(IXY)) ;SIT1TM1S(IXY)  = SIT1TM1S_TMP(NGID(IXY))
	 HST1TM1S(IXY)  = HST1TM1S_TMP(NGID(IXY))  ;CH4T1TM1S(IXY) = CH4T1TM1S_TMP(NGID(IXY))
	 CH4T2TM1S(IXY) = CH4T2TM1S_TMP(NGID(IXY)) ;CH41TM1S(IXY)  = CH41TM1S_TMP(NGID(IXY))
	 SO4T2TM1S(IXY) = SO4T2TM1S_TMP(NGID(IXY)) ;PO41TM1S(IXY)  = PO41TM1S_TMP(NGID(IXY))
	 NH41TM1S(IXY)  = NH41TM1S_TMP(NGID(IXY))  ;NO31TM1S(IXY)  = NO31TM1S_TMP(NGID(IXY))
	 HS1TM1S(IXY)   = HS1TM1S_TMP(NGID(IXY))   ;SI1TM1S(IXY)   = SI1TM1S_TMP(NGID(IXY))  
	 O20TM1S(IXY)   = O20TM1S_TMP(NGID(IXY))   ;SODTM1S(IXY)   = SODTM1S_TMP(NGID(IXY))
	 DD0TM1S(IXY)   = DD0TM1S_TMP(NGID(IXY))   ;BFORMAXS(IXY)  = BFORMAXS_TMP(NGID(IXY)) 
	 ISWBNTHS(IXY)  = ISWBNTHS_TMP(NGID(IXY))  ;CPOP(IXY,3)    = CPOP_TMP(NGID(IXY),3)
	 CPON(IXY,3)    = CPON_TMP(NGID(IXY),3)    ;CPOC(IXY,3)    = CPOC_TMP(NGID(IXY),3)
	END DO
	DO IXY=1,NHN
         CTEMP(IXY+M)     = CTEMP_TMP(HN_LST(IXY))     ;CPOS_TMP(IXY+M)  = CPOS_TMP(HN_LST(IXY))
	 PO4T2TM1S(IXY+M) = PO4T2TM1S_TMP(HN_LST(IXY)) ;NH4T2TM1S(IXY+M) = NH4T2TM1S_TMP(HN_LST(IXY))
	 NO3T2TM1S(IXY+M) = NO3T2TM1S_TMP(HN_LST(IXY)) ;HST2TM1S(IXY+M)  = HST2TM1S_TMP(HN_LST(IXY))
	 SIT2TM1S(IXY+M)  = SIT2TM1S_TMP(HN_LST(IXY))  ;BNTHSTR1S(IXY+M) = BNTHSTR1S_TMP(HN_LST(IXY))
	 PO4T1TM1S(IXY+M) = PO4T1TM1S_TMP(HN_LST(IXY)) ;NH4T1TM1S(IXY+M) = NH4T1TM1S_TMP(HN_LST(IXY))
	 NO3T1TM1S(IXY+M) = NO3T1TM1S_TMP(HN_LST(IXY)) ;SIT1TM1S(IXY+M)  = SIT1TM1S_TMP(HN_LST(IXY))
	 HST1TM1S(IXY+M)  = HST1TM1S_TMP(HN_LST(IXY))  ;CH4T1TM1S(IXY+M) = CH4T1TM1S_TMP(HN_LST(IXY))
	 CH4T2TM1S(IXY+M) = CH4T2TM1S_TMP(HN_LST(IXY)) ;CH41TM1S(IXY+M)  = CH41TM1S_TMP(HN_LST(IXY))
	 SO4T2TM1S(IXY+M) = SO4T2TM1S_TMP(HN_LST(IXY)) ;PO41TM1S(IXY+M)  = PO41TM1S_TMP(HN_LST(IXY))
	 NH41TM1S(IXY+M)  = NH41TM1S_TMP(HN_LST(IXY))  ;NO31TM1S(IXY+M)  = NO31TM1S_TMP(HN_LST(IXY))
	 HS1TM1S(IXY+M)   = HS1TM1S_TMP(HN_LST(IXY))   ;SI1TM1S(IXY+M)   = SI1TM1S_TMP(HN_LST(IXY))  
	 O20TM1S(IXY+M)   = O20TM1S_TMP(HN_LST(IXY))   ;SODTM1S(IXY+M)   = SODTM1S_TMP(HN_LST(IXY))
	 DD0TM1S(IXY+M)   = DD0TM1S_TMP(HN_LST(IXY))   ;BFORMAXS(IXY+M)  = BFORMAXS_TMP(HN_LST(IXY)) 
	 ISWBNTHS(IXY+M)  = ISWBNTHS_TMP(HN_LST(IXY))  ;CPOP(IXY+M,3)    = CPOP_TMP(HN_LST(IXY),3)
	 CPON(IXY+M,3)    = CPON_TMP(HN_LST(IXY),3)      ;CPOC(IXY+M,3)    = CPOC_TMP(HN_LST(IXY),3)
        END DO
!        DEALLOCATE(CTEMP_TMP,CPOS_TMP,PO4T2TM1S_TMP,NH4T2TM1S_TMP)
!	DEALLOCATE(NO3T2TM1S_TMP,HST2TM1S_TMP,SIT2TM1S_TMP,BNTHSTR1S_TMP,PO4T1TM1S_TMP)
!	DEALLOCATE(NH4T1TM1S_TMP,NO3T1TM1S_TMP,SIT1TM1S_TMP)
!	DEALLOCATE(HST1TM1S_TMP ,CH4T1TM1S_TMP,CH4T2TM1S_TMP)
!	DEALLOCATE(CH41TM1S_TMP ,SO4T2TM1S_TMP,PO41TM1S_TMP)
!	DEALLOCATE(NH41TM1S_TMP ,NO31TM1S_TMP ,HS1TM1S_TMP)
!	DEALLOCATE(SI1TM1S_TMP  ,O20TM1S_TMP  ,SODTM1S_TMP)
!	DEALLOCATE(DD0TM1S_TMP  ,BFORMAXS_TMP ,ISWBNTHS_TMP)
!        DEALLOCATE(CPOP_TMP,CPON_TMP,CPOC_TMP)
       END IF	
# endif
      ENDIF

!        TRANSFER I.C. TO APPROPRIATE VECTORS
      CALL LOADSED

!        TEMPERATURE DIFFUSION COEFFICIENT

      READ(40,'(A80)')   COMMENT
      READ(40,'(8F10.0)',ERR=900)  DIFFT
      DIFFT = 0.0001*86400.*DIFFT
      READ(40,'(A80)')   COMMENT
      READ(40,'(8F10.0)',ERR=900)  SALTSW
      READ(40,'(A80)')   COMMENT
      READ(40,'(8F10.0)',ERR=900)  FRPPH1
      IF(CONST(2) >= 2) READ(40,'(A80)')   COMMENT
      IF(CONST(2) >= 2) READ(40,'(8F10.0)',ERR=900)  FRPPH2
      IF(CONST(2) >= 3) READ(40,'(A80)')   COMMENT
      IF(CONST(2) >= 3) READ(40,'(8F10.0)',ERR=900)  FRPPH3
      READ(40,'(A80)')   COMMENT
      READ(40,'(8F10.0)',ERR=900)  FRPOP
      READ(40,'(A80)')   COMMENT
      READ(40,'(8F10.0)',ERR=900)  FRNPH1
      IF(CONST(2) >= 2) READ(40,'(A80)')   COMMENT
      IF(CONST(2) >= 2) READ(40,'(8F10.0)',ERR=900)  FRNPH2
      IF(CONST(2) >= 3) READ(40,'(A80)')   COMMENT
      IF(CONST(2) >= 3) READ(40,'(8F10.0)',ERR=900)  FRNPH3
      READ(40,'(A80)')   COMMENT
      READ(40,'(8F10.0)',ERR=900)  FRPON
      READ(40,'(A80)')   COMMENT
      READ(40,'(8F10.0)',ERR=900)  FRCPH1
      IF(CONST(2) >= 2) READ(40,'(A80)')   COMMENT
      IF(CONST(2) >= 2) READ(40,'(8F10.0)',ERR=900)  FRCPH2
      IF(CONST(2) >= 3) READ(40,'(A80)')   COMMENT
      IF(CONST(2) >= 3) READ(40,'(8F10.0)',ERR=900)  FRCPH3
      READ(40,'(A80)')   COMMENT
      READ(40,'(8F10.0)',ERR=900)  FRPOC
      READ(40,'(A80)')   COMMENT
      READ(40,'(8F10.0)',ERR=900)  (KPDIAG(I),DPTHTA(I),I=1,3)
      READ(40,'(A80)')   COMMENT
      READ(40,'(8F10.0)',ERR=900)  (KNDIAG(I),DNTHTA(I),I=1,3)
      READ(40,'(A80)')   COMMENT
      READ(40,'(8F10.0)',ERR=900)  (KCDIAG(I),DCTHTA(I),I=1,3)
      READ(40,'(A80)')   COMMENT
      READ(40,'(8F10.0)',ERR=900)  KSI,THTASI

      IF(MSR)THEN
       WRITE(OUT,1210)  DIFFT
       WRITE(OUT,1215)  SALTSW            ! New CH4 formation mechanism
       WRITE(OUT,1220)
       WRITE(OUT,1230)  FRNAME(1),FRPPH1
       WRITE(OUT,1230)  FRNAME(2),FRPPH2
       WRITE(OUT,1230)  FRNAME(3),FRPOP
       WRITE(OUT,1230)  FRNAME(4),FRNPH1
       WRITE(OUT,1230)  FRNAME(5),FRNPH2
       WRITE(OUT,1230)  FRNAME(6),FRPON
       WRITE(OUT,1230)  FRNAME(7),FRCPH1
       WRITE(OUT,1230)  FRNAME(8),FRCPH2
       WRITE(OUT,1230)  FRNAME(9),FRPOC
       WRITE(OUT,1240)  (KPDIAG(I),DPTHTA(I),I=1,3),(KNDIAG(I),       &
         DNTHTA(I),I=1,3),(KCDIAG(I),DCTHTA(I),I=1,3),KSI,THTASI
      END IF
 1210 FORMAT(//30X,'TEMPERATURE DIFFUSION COEFFICIENT ',E10.3,' CM**2/SEC')
 1215 FORMAT(//30X,'SALINITY SWITCH:      ',F10.3,' PPT')
 1220 FORMAT(//30X,'PARTICULATE ORGANIC MATTER G-MODEL SPLITS'/ 10X,       &
            'FRACTION OF....',5X,'RECYCLED TO',5X,'G1',5X,'G2',5X,'G3')
 1230 FORMAT(6X,A24,11X,3F7.2)
 1240 FORMAT(//30X,'DIAGENESIS RATES (/DAY)  TEMP CORR FACTOR'/            &
         30X,'PHOSPHORUS'/                                                 &
         39X,'G1',E11.3,5X,F7.3/39X,'G2',E11.3,5X,F7.3/                    &
         39X,'G3',E11.3,5X,F7.3/                                           &
         30X,'NITROGEN'/                                                   &
         39X,'G1',E11.3,5X,F7.3/39X,'G2',E11.3,5X,F7.3/                    &
         39X,'G3',E11.3,5X,F7.3/                                           &
         30X,'CARBON'/                                                     &
         39X,'G1',E11.3,5X,F7.3/39X,'G2',E11.3,5X,F7.3/                    &
         39X,'G3',E11.3,5X,F7.3/                                           &
         30X,'SILICA'/                                                     &
         41X,E11.3,5X,F7.3)

!        READ SEDIMENTATION AND PARTICLE MIXING RATES
      IF(MSR) WRITE(OUT,1050)
 1050 FORMAT(////38X,                                                      &
        'S E D I M E N T A T I O N   A N D   P A R T I C L E   M I X I N G ', &
        '  R A T E S'//)

      CALL VREAD(NXY,OUT,ISEDPRNT,VSED,1)
      CALL VREAD(NXY,OUT,ISEDPRNT,VPMIX,2)
      CALL VREAD(NXY,OUT,ISEDPRNT,VDMIX,3)

!        COMPUTE G FRACTION SPLITS FOR REFRACTORY POM
      DO I=2,3
       PGFRAC(I) = FRPOP(I)/(FRPOP(2)+FRPOP(3))
       NGFRAC(I) = FRPON(I)/(FRPON(2)+FRPON(3))
       CGFRAC(I) = FRPOC(I)/(FRPOC(2)+FRPOC(3))
      END DO

!        CONVERT CM/YEAR TO M/DAY

      VSED = 2.73791E-5*VSED

      READ(40,'(A80)')   COMMENT
      READ(40,'(3I10)',ERR=900)  m1m2opt,piesiopt,piepo4opt
      IF(M1M2OPT == 0) THEN
        READ(40,'(A80)')   COMMENT
        READ(40,'(8F10.0)',ERR=900)  m1g,m2g,thtaDp,thtaDd
        IF (MSR) WRITE(OUT,1269)   M1g,M2g,thtaDp,thtaDd
 1269   FORMAT(//35X,'ADDITIONAL CONSTANTS'/                &
          /30X,'m1........',F8.2,' KG/L'                    &
          /30X,'m2........',F8.2,' KG/L'                    &
          /30X,'thtaDp....',F8.3,                           &
          /30X,'thtaDd....',F8.3)
        DO IXY=1,M
         IF(FSM(IXY) == 1) THEN
          M1(IXY) = M1G
          M2(IXY) = M2G
         ENDIF
        END DO
      ELSE
        CALL VREAD(NXY,OUT,ISEDPRNT,M1,4)
        CALL VREAD(NXY,OUT,ISEDPRNT,M2,5)
        READ(40,'(A80)')   COMMENT
        READ(40,'(8F10.0)',ERR=900)  thtaDp,thtaDd
        IF(MSR) WRITE(OUT,1270)   thtaDp,thtaDd
 1270   FORMAT(/30X,'thtaDp....',F8.3,/30X,'thtaDd....',F8.3)
      ENDIF
      READ(40,'(A80)')   COMMENT
      READ(40,'(8F10.0)',ERR=900)  Dd0,thtaDd0
      IF(MSR) WRITE(OUT,1271)   Dd0,thtaDd0
 1271 FORMAT(/30X,'Dd0.......',F8.3,' M/DAY'/30X,'thtaDd0...',F8.3)
      READ(40,'(A80)')   COMMENT
      READ(40,'(8F10.0)',ERR=900) kappnh4s,pienh4,thtanh4s,kmnh4,thtakmnh4,kmnh4o2
      IF(MSR) WRITE(OUT,1272) kappnh4s,pienh4,thtanh4s,kmnh4,thtakmnh4,kmnh4o2
 1272 FORMAT(                                          &
          /30X,'Saltwater values'                      &
          /30X,'kappnh4s..',F8.3,' M/DAY'              &
          /30X,'pienh4....',F8.3,' L/KG'               &
          /30X,'thtanh4s..',F8.3,                      &
          /30X,'kmnh4.....',F8.3,' MG N/M**3'          &
          /30X,'thtakmnh4.',F8.3,                      &
          /30X,'kmnh4o2...',F8.3,' MG O2/L')         
      READ(40,'(A80)')   COMMENT
      READ(40,'(8F10.0)',ERR=900)  kappnh4f,thtanh4f
      IF(MSR) WRITE(OUT,1273)  kappnh4f,thtanh4f
 1273 FORMAT(                                          &
          /30X,'Freshwater values'                     &
          /30X,'kappnh4f..',F8.3,' M/DAY'              &
          /30X,'thtanh4f..',F8.3)
      READ(40,'(A80)')   COMMENT
      READ(40,'(8F10.0)',ERR=900)  kapp1no3s,k2no3s,thtano3s
      IF(MSR) WRITE(OUT,1274)  kapp1no3s,k2no3s,thtano3s
 1274 FORMAT(                                          &
          /30X,'Saltwater values'                      &
          /30X,'kapp1no3s.',F8.3,' M/DAY'              &
          /30X,'k2no3s....',F8.3,' /DAY'               &
          /30X,'thtano3s..',F8.3)
      READ(40,'(A80)')   COMMENT
      READ(40,'(8F10.0)',ERR=900)  kapp1no3f,k2no3f,thtano3f
      IF(MSR) WRITE(OUT,1275)  kapp1no3f,k2no3f,thtano3f
 1275 FORMAT(                                          &
          /30X,'Freshwater values'                     &
          /30X,'kapp1no3f.',F8.3,' M/DAY'              &
          /30X,'k2no3f....',F8.3,' /DAY'               &
          /30X,'thtano3f..',F8.3)
      READ(40,'(A80)')   COMMENT
      READ(40,'(8F10.0)',ERR=900) kappd1,kappp1,pie1s,pie2s,thtapd1,kmhso2
      IF(MSR) WRITE(OUT,1276)  kappd1,kappp1,pie1s,pie2s,thtapd1,kmhso2
 1276 FORMAT(                                          &
          /30X,'kappd1....',F8.3,' M/DAY'              &
          /30X,'kappp1....',F8.3,' M/DAY'              &
          /30X,'pie1s.....',F8.3,' L/KG'               &
          /30X,'pie2s.....',F8.3,' L/KG'               &
          /30X,'thtapd1...',F8.3,                      &
          /30X,'kmhso2....',F8.3,' MG O2/L')
      IF(PIESIOPT == 0) THEN
        READ(40,'(A80)')   COMMENT
        READ(40,'(8F10.0)',ERR=900)                    &
             csisat,pie1sig,pie2sig,ksi,thtasi,kmpsi,o2critsi,jsidetr
        IF(MSR) WRITE(OUT,1278)  csisat,pie1sig,pie2sig,ksi,thtasi,kmpsi,o2critsi,jsidetr
 1278 FORMAT(                                          &      !Tian
          /30X,'csisat....',F12.3,' MG SI/M**3'         &
          /30X,'pie1si....',F12.3,' L/KG'               &
          /30X,'pie2si....',F12.3,' L/KG'               &
          /30X,'ksi.......',F12.3,' /DAY'               &
          /30X,'thtasi....',F12.3,                      &
          /30X,'kmpsi.....',F12.3,' MG SI/M**3'         &
          /30X,'o2critsi..',F12.3,' MG O2/L'            &
          /30X,'jsidetr...',F12.1,' MG SI/M**2')
        DO IXY=1,M
         IF(FSM(IXY) == 1) THEN
          pie1si(IXY) = pie1sig
          pie2si(IXY) = pie2sig
         ENDIF
        END DO
      ELSE
        CALL VREAD(NXY,OUT,ISEDPRNT,pie1si,6)
        CALL VREAD(NXY,OUT,ISEDPRNT,pie2si,7)
        READ(40,'(A80)')   COMMENT
        READ(40,'(8F10.0)',ERR=900)  csisat,ksi,thtasi,kmpsi,o2critsi,jsidetr
        IF(MSR) WRITE(OUT,1378)  csisat,ksi,thtasi,kmpsi,o2critsi,jsidetr
 1378 FORMAT(                                          &
          /30X,'csisat....',F8.3,' MG SI/M**3'         &
          /30X,'ksi.......',F8.3,' /DAY'               &
          /30X,'thtasi....',F8.3,                      &
          /30X,'kmpsi.....',F8.3,' MG SI/M**3'         &
          /30X,'o2critsi..',F8.3,' MG O2/L'            &
          /30X,'jsidetr...',F8.1,' MG SI/M**2') 
      ENDIF
      IF(PIEPO4OPT == 0) THEN
        READ(40,'(A80)')   COMMENT
        READ(40,'(8F10.0)',ERR=900)  pie1po4mg,pie1po4ng,o2crit,kmo2Dp
        IF(MSR) WRITE(OUT,1280)  pie1po4mg,pie1po4ng,o2crit,kmo2Dp
 1280   FORMAT(                                        &
          /30X,'pie1po4m..',F8.3,' L/KG'               &
          /30X,'pie1po4n..',F8.3,' L/KG'               &
          /30X,'o2crit....',F8.3,' MG O2/L'            &
          /30X,'kmo2Dp....',F8.3,' MG O2/L')        
        DO IXY=1,M
          IF(FSM(IXY) == 1) THEN
           pie1po4m(IXY) = pie1po4mg
           pie1po4n(IXY) = pie1po4ng
          ENDIF
        END DO
      ELSE
        CALL VREAD(NXY,OUT,ISEDPRNT,pie1po4m,8)
        CALL VREAD(NXY,OUT,ISEDPRNT,pie1po4n,9)
        READ(40,'(A80)')   COMMENT
        READ(40,'(8F10.0)',ERR=900)  o2crit,kmo2Dp
        IF(MSR) WRITE(OUT,1380)  o2crit,kmo2Dp
 1380   FORMAT(/30X,'o2crit....',F8.3,' MG O2/L'/30X,'kmo2Dp....',F8.3,' MG O2/L')
      ENDIF
      READ(40,'(A80)')   COMMENT
      READ(40,'(8F10.0)',ERR=900)  tempbnth,kbnthstr,klbnth,Dpmin
      IF(MSR) WRITE(OUT,1282)  tempbnth,kbnthstr,klbnth,Dpmin
 1282 FORMAT(                                         &
        /30X,'tempbnth..',F8.3,' DEG C'               &
        /30X,'kbnthstr..',F8.3,' /DAY'                &
        /30X,'klbnth....',F8.3,                       &
        /30X,'Dpmin.....',F8.3,' M**2/DAY')
      READ(40,'(A80)')   COMMENT
      READ(40,'(8F10.0)',ERR=900)  kappch4,thtach4,KMCH4O2,KMSO4   !CH4-SO4 Code
      IF(MSR) WRITE(OUT,1284)  kappch4,thtach4,KMCH4O2,KMSO4         !CH4-SO4 Code
 1284 FORMAT(                                         &
        /30X,'kappch4...',F8.3,' M/DAY'               &
        /30X,'thtach4...',F8.3,                       &
        /30X,'KmCH4O2...',F8.3,                       &
        /30X,'KmSO4  ...',F8.3) 

!        SET MIN AND MAX RANGES FOR ROOT FINDER

      SODMIN = 0.0001   
      SODMAX = 100.
      ITER = 50
      EPS = 0.00005

      DO IXY=1,M
       IF(FSM(IXY) == 1.) THEN
!        SET UP REACTION RATES IN TABLE LOOK-UP FORM
!        (ASSUMES ALL SEDIMENT DEPTHS AND MIXING RATES ARE THE SAME)
         H20 = HSED(IXY)
         DELTAZ = HSED(IXY)
         w2 = VSED(IXY)
         Dp = VPMIX(IXY)
         Dd = VDMIX(IXY)
         CALL SEDTS2TH
!        RETURN TO MAIN PROGRAM AFTER INITIALIZATION
         RETURN
        ENDIF
      END DO

   47 RETURN


      ENTRY  SED_FLUX


!     Increment counter
      IGDCT = IGDCT + 1


!        PASS WQ TIME-STEP (IN DAYS) TO SEDIMENT SUBR
      IF(DTWQ /= 0.0) THEN
        DELTAT = DTWQ
      ELSE
        DELTAT = DT
      ENDIF

!      COMPUTE TEMPERATURE AND BENTHIC-STRESS DEPENDENT DEPOSITION RATES
!     DO 55 IY=1,NY
!     DO 55 IX=1,NX
!     IF(FSM(IX,IY).NE.1.) GO TO 55
!     TEMPD = CTEMP(IX,IY)
!     TEMPD = MIN(CTEMP(IX,IY),25.)
!     ITEMP = 10.*TEMPD+1
!     TERM = (1.-KBNTHSTR*BNTHSTR1S(IX,IY))*ZW12NOM(ITEMP)
!  55 CONTINUE

!             PARTICULATE ORGANIC FLUX TO SEDIMENT
!  c$doacross local(iy,ix,term,ppflx,pnflx,pcflx,sarea)

      DO IXY=1,M
       IF(FSM(IXY) == 1.) THEN

!        FLUX RATE FROM WATER COLUMN = DEPOSITION RATE * CONC
!                    M/DAY * MG/L * 1000 L/M**3 ==> MG/M2-DAY 

        TERM = 1000./XAZ(IXY)
        SSFWS(IXY) = 0.
        PCFWS(IXY) = 0.
        PNFWS(IXY) = 0.
        PPFWS(IXY) = 0.
        PSFWS(IXY) = 0.

        DO I=1,3

! USE LABILE PARTICULATES AS G1, SPLIT REFRACTORY BETWEEN G2, G3

         IF (I == 1) THEN
          PPFLX = TERM*DEPFLUX(IXY,5)
          PNFLX = TERM*DEPFLUX(IXY,7)
          PCFLX = TERM*(DEPFLUX(IXY,10)+DEPFLUX(IXY,11))
         ELSE
          PPFLX = TERM*DEPFLUX(IXY,4)*PGFRAC(I)
          PNFLX = TERM*DEPFLUX(IXY,6)*NGFRAC(I)
          PCFLX = TERM*DEPFLUX(IXY,9)*CGFRAC(I)
         END IF

!          PHOSPHORUS

         FLXPOP(IXY,I)                                       &
              = TERM*PCRB1(IXY,NZ)*FRPPH1(I)*DEPFLUX(IXY,1)  &
              + TERM*PCRB2(IXY,NZ)*FRPPH2(I)*DEPFLUX(IXY,2)  &
              + TERM*PCRB3(IXY,NZ)*FRPPH3(I)*DEPFLUX(IXY,3)  &
              + PPFLX

!        NITROGEN

         FLXPON(IXY,I)                                       &
              = TERM*NCRB1(IXY,NZ)*FRNPH1(I)*DEPFLUX(IXY,1)  &
              + TERM*NCRB2(IXY,NZ)*FRNPH2(I)*DEPFLUX(IXY,2)  &
              + TERM*NCRB3(IXY,NZ)*FRNPH3(I)*DEPFLUX(IXY,3)  &
              + PNFLX

!        CARBON

         FLXPOC(IXY,I) = TERM*FRCPH1(I)*DEPFLUX(IXY,1)       &
                      + TERM*FRCPH2(I)*DEPFLUX(IXY,2)        &
                      + TERM*FRCPH3(I)*DEPFLUX(IXY,3)        &
                      + PCFLX

        END DO

!        SILICA
 
        FLXPOS(IXY) = TERM*SCRB1(IXY,NZ)*DEPFLUX(IXY,1)      &
                    + TERM*SCRB2(IXY,NZ)*DEPFLUX(IXY,2)      &
                    + TERM*SCRB3(IXY,NZ)*DEPFLUX(IXY,3)      &
                    + TERM*DEPFLUX(IXY,8)

! WILL ADD ADSORBED PHOSPHATE AND SILICA TO LAYER 2 INORGANIC POOL

        FLXPO4T2S(IXY) = TERM*DEPFLUX(IXY,12)
        FLXSIT2S(IXY) = TERM*DEPFLUX(IXY,13)

! SUM PARTICULATE FLUXES TO SEDIMENTS, NEGATIVE INTO SEDIMENTS

        SAREA=1./XAZ(IXY)
!       SAREA=XAZ(IX,IY)/1000.
        DO I = 1,3
         PPFWS(IXY) = PPFWS(IXY) - FLXPOP(IXY,I)*SAREA
         PNFWS(IXY) = PNFWS(IXY) - FLXPON(IXY,I)*SAREA
         PCFWS(IXY) = PCFWS(IXY) - FLXPOC(IXY,I)*SAREA
        END DO
        PSFWS(IXY) = PSFWS(IXY) - FLXPOS(IXY)*SAREA
       END IF
      END DO

      DO 200 IXY=1,M
       IF(FSM(IXY) /= 1.) GO TO 200

!        LOAD CONCS AT TIME LEVEL   T - DELTAT

        CALL LOADTM1

!       -DIAGX- UNITS ARE [GM/M**3-DAY]
!       -JDIAGX- UNITS ARE [MG/M2/DAY]

!        SEDIMENT DEPTH
        H2 = HSED(IXY)
        DELTAZ = H2

!        SEDIMENTATION AND MIXING RATES
        w2 = VSED(IXY)
        Dp = VPMIX(IXY)
        Dd = VDMIX(IXY)

!        SEDIMENT TEMPERATURE
        TEMPD = MAX(CTEMP(IXY),0.0)
        TEMPD = MIN(TEMPD,34.9)
        STP20 = TEMPD - 20.

!        CONVERT OVERLYING WATER COLUMN CONCENTRATIONS INTO MG/M**3
!JQI        PO40 = PO4(IXY,NZ)*1000.
        PO40 = CKINARRAY(IXY,NZ,1)*1000.
!JQI        NH40 = NH4(IXY,NZ)*1000.
        NH40 = CKINARRAY(IXY,NZ,2)*1000.
        !NO30 = NO23(IXY,NZ)*1000.
	
        NO30 = CARAY(IXY,NZ,15)*1000.
		
!JQI        SI0  = SI(IXY,NZ)*1000.
        SI0  = CKINARRAY(IXY,NZ,3)*1000.

!        OVERLYING WATER COLUMN OXYGEN (WITH 0.001 MG/L FLOOR)
!        O20 = AMAX1(DO(IXY,NZ),0.001)
        O20 = AMAX1(CARAY(IXY,NZ,26),0.001)	
        HS0 = 0.

!        OVERLYING WATER COLUMN SALINITY
        SAL0 = SAL(IXY,NZ)

!       Regression function to get SO4 concentration from SAL0
!       [SO4] = 20 mg/L            for        [Cl] < 6 mg/L
!             = 20 + (27/190)[Cl]  for        [Cl] > 6 mg/L
!       1 ppt = 607.445 mg/L Cl
        IF(SAL0 > 0.0099) THEN
         SO40MG = 20.0 + (27./190.)*607.445*SAL0
        ELSE
         SO40MG = 20.0
        ENDIF

!        DEPTH OF OVERLYING WATER COLUMN
        OWCDPH = HBAR(IXY)
!       OWCDPH = ZD(IX,IY,NZ)+HBAR(IX,IY)

!        METHANE SATURATION
        CH4SAT = 99.0*(1.+(OWCDPH+HSED(IXY))/10.)*0.9759**STP20

!        SORBED PO4 AND SI FLUXES
        FLXPO4T2 = FLXPO4T2S(IXY)
        FLXSIT2 = FLXSIT2S(IXY)

!-- evaluate the temperature dependent coefficients
        error=sedf(sodmin,0)
!-- diagenesis computation
        call diagen

!d        write(out,9911)  time,ctemp(ix,iy),pop1r,pop2r,pop3r,popr
!d9911    format(/1x,'entry Zbrent time,ctemp,pop1r,pop2r,pop3r,popr'/
!d    .          8e10.3)
!d        write(out,9901)  pon1r,pon2r,pon3r,ponr,poc1r,poc2r,poc3r,pocr
!d        write(out,9902)  po4t2r,hst2r,sit2r,psiavr
!d        write(out,9903)
!d    .        (flxpop(ix,iy,1)+flxpop(ix,iy,2)+flxpop(ix,iy,3))
!d    .       ,(flxpon(ix,iy,1)+flxpon(ix,iy,2)+flxpon(ix,iy,3))
!d    .       ,(flxpoc(ix,iy,1)+flxpoc(ix,iy,2)+flxpoc(ix,iy,3))
!d        write(out,9904)  o20,csod,sod,s
!d        write(out,9905)  jp,jn,jc,jo2nh4,xjco2av,xjc1av
!d        write(out,9906)  jpo4,jnh4,jno3,jhs,jsi,jch4aq,jch4g,bnthstr
!d        write(out,9907)  po40,po41,po42,po4t2,nh40,nh41,nh42,nh4t2
!d        write(out,9908)  no30,no31,no32,no3t2,hs1,hs2,hst2
!d        write(out,9909)  si0,si1,si2,sit2
!-- solve the nh4,no3,sod equations

        sod=zbrent(sedf,sodmin,sodmax,eps,ierr)
	
!print*,'vss 1048 ',sod,sodmin,sodmax,ierr,ixy,ngid(ixy),myid
        if(ierr /= 0) then
         ! IF(MSR) write(out,9000)   time,ierr,ixy,sodmin,sodmax
         write(out,9000)   time,ierr,ixy,sodmin,sodmax	  
 9000     format(/                                                   &
            5x,'Zbrent failure at time =',f8.3,' with ierr=',i2/    &
            5x,'in sediment segment ixy=',i5, /                     &
            5x,'(sodmin,sodmax=',f6.4,f6.1,')'/                     &
            5x,'Program termination follows diagnostic dumps')
         if(ierr == 2) then
	   !IF(MSR)THEN
            write(out,9900)  time,ctemp(ixy),pop1r,pop2r,pop3r,popr
 9900       format(/1x,' time,ctemp(ix,iy),pop1r,pop2r,pop3r,popr'/8e10.3)
            write(out,9901)  pon1r,pon2r,pon3r,ponr,poc1r,poc2r,poc3r,pocr
 9901       format(/1x,' pon1r,pon2r,pon3r,ponr,poc1r,poc2r,poc3r,pocr'/8e10.3)
            write(out,9902)  po4t2r,hst2r,sit2r,psiavr
 9902       format(/1x,' po4t2r,hst2r,sit2r,psiavr'/8e10.3)
            write(out,9903)                                          &
              (flxpop(ixy,1)+flxpop(ixy,2)+flxpop(ixy,3))          &
             ,(flxpon(ixy,1)+flxpon(ixy,2)+flxpon(ixy,3))          &
             ,(flxpoc(ixy,1)+flxpoc(ixy,2)+flxpoc(ixy,3))
 9903       format(/1x,' flxpop,flxpon,flxpoc'/8e10.3)
            write(out,9904)  o20,csod,sod,s,h1,h2,h1dot,h2dot
 9904       format(/1x,' o20,csod,sod,s,h1,h2,h1dot,h2dot'/8e10.3)
            write(out,9905)  jp,jn,jc,jo2nh4,xjco2av,xjc1av
 9905       format(/1x,' jp,jn,jc,jo2nh4,xjco2av,xjc1av'/8e10.3)
            write(out,9906)  jpo4,jnh4,jno3,jhs,jsi,jch4aq,jch4g,bnthstr
 9906       format(/1x,' jpo4,jnh4,jno3,jhs,jsi,jch4aq,jch4g,bnthstr'/8e10.3)
            write(out,9907)  po40,po41,po42,po4t2,nh40,nh41,nh42,nh4t2
 9907       format(/1x,' po40,po41,po42,po4t2,nh40,nh41,nh42,nh4t2'/8e10.3)
            write(out,9908)  no30,no31,no32,no3t2,hs1,hs2,hst2
 9908       format(/1x,' no30,no31,no32,no3t2,hs1,hs2,hst2'/8e10.3)
            write(out,9909)  si0,si1,si2,sit2
 9909       format(/1x,' si0,si1,si2,sit2'/8e10.3)
           !END IF 
!          call exit
          CALL PSTOP('vss1082   ')
         else
          error=sedf(sodmin,1)
          !IF(MSR)THEN
	   write(out,9889)  time,sodmin,error
 9889      format(/5x,'Zbrent diagnostics at time =',f8.3,' for sod =',f8.4,' error =',e12.3/)
           write(out,9900)  time,ctemp(ixy),pop1r,pop2r,pop3r,popr
           write(out,9901)  pon1r,pon2r,pon3r,ponr,poc1r,poc2r,poc3r,pocr
           write(out,9902)  po4t2r,hst2r,sit2r,psiavr
           write(out,9903)                                       &
              (flxpop(ixy,1)+flxpop(ixy,2)+flxpop(ixy,3))       &
             ,(flxpon(ixy,1)+flxpon(ixy,2)+flxpon(ixy,3))       &
             ,(flxpoc(ixy,1)+flxpoc(ixy,2)+flxpoc(ixy,3))
           write(out,9904)  o20,csod,sod,s,h1,h2,h1dot,h2dot
           write(out,9905)  jp,jn,jc,jo2nh4,xjco2av,xjc1av
           write(out,9906)  jpo4,jnh4,jno3,jhs,jsi,jch4aq,jch4g,bnthstr
           write(out,9907)  po40,po41,po42,po4t2,nh40,nh41,nh42,nh4t2
           write(out,9908)  no30,no31,no32,no3t2,hs1,hs2,hst2
           write(out,9909)  si0,si1,si2,sit2
	  !END IF 
          error=sedf(sodmax,1)
          !IF(MSR)THEN!
	   write(out,9889)  time,sodmax,error
           write(out,9900)  time,ctemp(ixy),pop1r,pop2r,pop3r,popr
           write(out,9901)  pon1r,pon2r,pon3r,ponr,poc1r,poc2r,poc3r,pocr
           write(out,9902)  po4t2r,hst2r,sit2r,psiavr
           write(out,9903)                                      &
              (flxpop(ixy,1)+flxpop(ixy,2)+flxpop(ixy,3))      &
             ,(flxpon(ixy,1)+flxpon(ixy,2)+flxpon(ixy,3))      &
             ,(flxpoc(ixy,1)+flxpoc(ixy,2)+flxpoc(ixy,3))
           write(out,9904)  o20,csod,sod,s,h1,h2,h1dot,h2dot
           write(out,9905)  jp,jn,jc,jo2nh4,xjco2av,xjc1av
           write(out,9906)  jpo4,jnh4,jno3,jhs,jsi,jch4aq,jch4g,bnthstr
           write(out,9907)  po40,po41,po42,po4t2,nh40,nh41,nh42,nh4t2
           write(out,9908)  no30,no31,no32,no3t2,hs1,hs2,hst2
           write(out,9909)  si0,si1,si2,sit2
	  !END IF 
!          call exit
          CALL PSTOP('vss1120   ')
         endif
        endif
        h1s(ixy)=h1
!-- and evaluate the po4,si equations
        error=sedf(sod,2)

!-- replace the t minus 1 concentrations
        call storetm1

! ASSIGN FLUX-FLUX RESULTS TO WQM ARRAYS

        CFLUXS(IXY,1) = SOD
        CFLUXS(IXY,2) = JHS
        CFLUXS(IXY,3) = JCH4AQ
        CFLUXS(IXY,4) = JCH4G
        CFLUXS(IXY,5) = JNH4/1000.
        CFLUXS(IXY,6) = JNO3/1000.
        CFLUXS(IXY,7) = JPO4/1000.
        CFLUXS(IXY,8) = JSI/1000.

!- denitrification flux
        itemp = 10*tempd+1
        if(sal0 > saltsw) then
         xapp1no3=zhtano3s(itemp)
         xk2no3=zhta2no3s(itemp)*h2
        else
         xapp1no3=zhtano3f(itemp)
         xk2no3=zhta2no3f(itemp)*h2
        endif
        bnthden = (xapp1no3*xapp1no3/s*no31 + xk2no3*no32)

!        Perform Global Dump Sums, if required
        IF(IGDSEDOPT == 1) THEN
         M2G=M2(IXY)
         CTEMP_GDA(IXY)  = CTEMP_GDA(IXY) + CTEMP(IXY)
         POP1R_GDA(IXY)  = POP1R_GDA(IXY) + POP1AV/1.0E6/M2G
         POP2R_GDA(IXY)  = POP2R_GDA(IXY) + POP2AV/1.0E6/M2G
         POP3R_GDA(IXY)  = POP3R_GDA(IXY) + POP3AV/1.0E6/M2G
         POPR_GDA(IXY)   = POPR_GDA(IXY) + (POP1AV+POP2AV+POP3AV)/1.0E6/M2G
         PON1R_GDA(IXY)  = PON1R_GDA(IXY) + PON1AV/1.0E6/M2G
         PON2R_GDA(IXY)  = PON2R_GDA(IXY) + PON2AV/1.0E6/M2G
         PON3R_GDA(IXY)  = PON3R_GDA(IXY) + PON3AV/1.0E6/M2G
         PONR_GDA(IXY)   = PONR_GDA(IXY) + (PON1AV+PON2AV+PON3AV)/1.0E6/M2G
         POC1R_GDA(IXY)  = POC1R_GDA(IXY) + POC1AV/1.0E6/M2G
         POC2R_GDA(IXY)  = POC2R_GDA(IXY) + POC2AV/1.0E6/M2G
         POC3R_GDA(IXY)  = POC3R_GDA(IXY) + POC3AV/1.0E6/M2G
         POCR_GDA(IXY)   = POCR_GDA(IXY) + (POC1AV+POC2AV+POC3AV)/1.0E6/M2G
         PO4T2R_GDA(IXY) = PO4T2R_GDA(IXY) + PO4T2AV/1000./M2G
         HST2R_GDA(IXY)  = HST2R_GDA(IXY) + HST2AV/1000./M2G
         SIT2R_GDA(IXY)  = SIT2R_GDA(IXY) + (PSIAV+SIT2AV)/1.0E6/M2G
         PSIAVR_GDA(IXY) = PSIAVR_GDA(IXY) + PSIAV/1.0E6/M2G
         FLXPOP_GDA(IXY) = FLXPOP_GDA(IXY) + FLXPOP(IXY,1)        &
                         + FLXPOP(IXY,2) + FLXPOP(IXY,3)
         FLXPOP1_GDA(IXY) = FLXPOP1_GDA(IXY) + FLXPOP(IXY,1)
         FLXPOP2_GDA(IXY) = FLXPOP2_GDA(IXY) + FLXPOP(IXY,2)
         FLXPOP3_GDA(IXY) = FLXPOP3_GDA(IXY) + FLXPOP(IXY,3)
         FLXPON_GDA(IXY) = FLXPON_GDA(IXY) + FLXPON(IXY,1)        &
                         + FLXPON(IXY,2) + FLXPON(IXY,3)
         FLXPON1_GDA(IXY) = FLXPON1_GDA(IXY) + FLXPON(IXY,1)
         FLXPON2_GDA(IXY) = FLXPON2_GDA(IXY) + FLXPON(IXY,2)
         FLXPON3_GDA(IXY) = FLXPON3_GDA(IXY) + FLXPON(IXY,3)
         FLXPOC_GDA(IXY) = FLXPOC_GDA(IXY) + FLXPOC(IXY,1)        &
                         + FLXPOC(IXY,2) + FLXPOC(IXY,3)
         FLXPOC1_GDA(IXY) = FLXPOC1_GDA(IXY) + FLXPOC(IXY,1)
         FLXPOC2_GDA(IXY) = FLXPOC2_GDA(IXY) + FLXPOC(IXY,2)
         FLXPOC3_GDA(IXY) = FLXPOC3_GDA(IXY) + FLXPOC(IXY,3)
         FLXPOS_GDA(IXY) = FLXPOS_GDA(IXY) + FLXPOS(IXY)
         FLXPO4T2S_GDA(IXY) = FLXPO4T2S_GDA(IXY)+FLXPO4T2S(IXY)
         FLXSIT2S_GDA(IXY) = FLXSIT2S_GDA(IXY) + FLXSIT2S(IXY)
         O20_GDA(IXY)    = O20_GDA(IXY) + O20
         CSOD_GDA(IXY)   = CSOD_GDA(IXY) + CSOD
         SOD_GDA(IXY)    = SOD_GDA(IXY) + SOD
         S_GDA(IXY)      = S_GDA(IXY) + S
         JP_GDA(IXY)     = JP_GDA(IXY) + JP
         JN_GDA(IXY)     = JN_GDA(IXY) + JN
         JC_GDA(IXY)     = JC_GDA(IXY) + JC
         JO2NH4_GDA(IXY) = JO2NH4_GDA(IXY) + JO2NH4
         XJCO2AV_GDA(IXY) = XJCO2AV_GDA(IXY) + XJCO2AV
         XJC1AV_GDA(IXY) = XJC1AV_GDA(IXY) + XJC1AV
         JPO4_GDA(IXY)   = JPO4_GDA(IXY) + JPO4
         JNH4_GDA(IXY)   = JNH4_GDA(IXY) + JNH4
         JNO3_GDA(IXY)   = JNO3_GDA(IXY) + JNO3
         JH2S_GDA(IXY)    = JH2S_GDA(IXY) + JHS
         JSI_GDA(IXY)    = JSI_GDA(IXY) + JSI
         JCH4AQ_GDA(IXY) = JCH4AQ_GDA(IXY) + JCH4AQ
         JCH4G_GDA(IXY)  = JCH4G_GDA(IXY) + JCH4G
         H1_GDA(IXY)     = H1_GDA(IXY) + H1
         PO40_GDA(IXY)   = PO40_GDA(IXY) + PO40
         PO41_GDA(IXY)   = PO41_GDA(IXY) + PO41
         PO42_GDA(IXY)   = PO42_GDA(IXY) + PO42
         PO4T2_GDA(IXY)  = PO4T2_GDA(IXY) + PO4T2
         NH40_GDA(IXY)   = NH40_GDA(IXY) + NH40
         NH41_GDA(IXY)   = NH41_GDA(IXY) +NH41
         NH42_GDA(IXY)   = NH42_GDA(IXY) + NH42
         NH4T2_GDA(IXY)  = NH4T2_GDA(IXY) + NH4T2
         NO30_GDA(IXY)   = NO30_GDA(IXY) + NO30
         NO31_GDA(IXY)   = NO31_GDA(IXY) + NO31
         NO32_GDA(IXY)   = NO32_GDA(IXY) + NO32
         NO3T2_GDA(IXY)  = NO3T2_GDA(IXY) + NO3T2
         HS1_GDA(IXY)    = HS1_GDA(IXY) + HS1
         HS2_GDA(IXY)    = HS2_GDA(IXY) + HS2
         HST2_GDA(IXY)   = HST2_GDA(IXY) + HST2
         SI0_GDA(IXY)    = SI0_GDA(IXY) + SI0
         SI1_GDA(IXY)    = SI1_GDA(IXY) + SI1
         SI2_GDA(IXY)    = SI2_GDA(IXY) + SI2
         SIT2_GDA(IXY)   = SIT2_GDA(IXY) + SIT2
         JN2_GDA(IXY) = JN2_GDA(IXY) + BNTHDEN
         CH41_GDA(IXY) = CH41_GDA(IXY) + CH41
         CH42_GDA(IXY) = CH42_GDA(IXY) + CH42
         CH4T2_GDA(IXY) = CH4T2_GDA(IXY) + CH4T2
         SO41_GDA(IXY) = SO41_GDA(IXY) + SO41
         SO42_GDA(IXY) = SO42_GDA(IXY) + SO42
         SO4T2_GDA(IXY) = SO4T2_GDA(IXY) + SO4T2

        ENDIF

        if(itimesecs >= nxprtsed) then
         iseddisk=1

!- output solid phase concentrations in mg/g (O2) mg/kg (po4) mg/g (Si)
         m2g=m2(ixy)
         hst2r=hst2av/1000./m2g
         po4t2r=po4t2av/1000./m2g
         sit2r=(psiav+sit2av)/1.0e6/m2g
         psiavr=(psiav)/1.0e6/m2g
!- output POC,PON,etc in mg/g
         pon1r=pon1av/1.0e6/m2g
         pon2r=pon2av/1.0e6/m2g
         pon3r=pon3av/1.0e6/m2g
         ponr=pon1r+pon2r+pon3r
         poc1r=poc1av/1.0e6/m2g
         poc2r=poc2av/1.0e6/m2g
         poc3r=poc3av/1.0e6/m2g
         pocr=poc1r+poc2r+poc3r
         pop1r=pop1av/1.0e6/m2g
         pop2r=pop2av/1.0e6/m2g
         pop3r=pop3av/1.0e6/m2g
         popr=pop1r+pop2r+pop3r
!- output dissolved concentrations in mg/L
         nh40z=nh40/1000.
         nh41z=nh41/1000.
         nh42z=nh42av/1000.
         nh4t2z=nh4t2av/1000.
         no30z=no30/1000.
         no31z=no31/1000.
         no32z=no32av/1000.
         no3t2z=no3t2av/1000.
         si0z=si0/1000.
         si1z=si1/1000.
         si2z=si2av/1000.
         sit2z=sit2av/1000.
         po40z=po40/1000.
         po41z=po41/1000.
         po42z=po42av/1000.
         po4t2z=po4t2av/1000.
         hs1z=hs1
         hs2z=hs2av
         hst2z=hst2av

!         IF(PAR) CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
         IF(IGDSEDOPT == 0) THEN
	 
!              IF(SERIAL) THEN	 	 
!        Instantaneous Dumps 
	 
!        IF(NGID(IXY).EQ.3920) write(3920,'(I6,F12.3,I8,15F12.3)')MYID,TIME, &
!	  NGID(IXY),vx(ixy),vy(ixy),jnh4,jno3,jsi,jpo4,sod,bnthden,jhs &
!	  ,(flxpon(ixy,1)+flxpon(ixy,2)+flxpon(ixy,3)) &
!	   ,(flxpoc(ixy,1)+flxpoc(ixy,2)+flxpoc(ixy,3))            &
!	   ,jp,jn,jc,ctemp(ixy) 
!        IF(NGID(IXY).EQ.3546) write(3546,'(I6,F12.3,I8,15F12.3)')MYID,TIME, &
!	  NGID(IXY),vx(ixy),vy(ixy),jnh4,jno3,jsi,jpo4,sod,bnthden,jhs &
!	  ,(flxpon(ixy,1)+flxpon(ixy,2)+flxpon(ixy,3)) &
!	   ,(flxpoc(ixy,1)+flxpoc(ixy,2)+flxpoc(ixy,3))            &
!	   ,jp,jn,jc,ctemp(ixy) 	   
!        IF(NGID(IXY).EQ.3626) write(3626,'(I6,F12.3,I8,15F12.3)')MYID,TIME, &
!	  NGID(IXY),vx(ixy),vy(ixy),jnh4,jno3,jsi,jpo4,sod,bnthden,jhs &
!	  ,(flxpon(ixy,1)+flxpon(ixy,2)+flxpon(ixy,3)) &
!	   ,(flxpoc(ixy,1)+flxpoc(ixy,2)+flxpoc(ixy,3))            &
!	   ,jp,jn,jc,ctemp(ixy) 	   
!	   
!        IF(NGID(IXY).EQ.1779) write(1779,'(I6,F12.3,I8,15F12.3)')MYID,TIME, &
!	  NGID(IXY),vx(ixy),vy(ixy),jnh4,jno3,jsi,jpo4,sod,bnthden,jhs &
!	  ,(flxpon(ixy,1)+flxpon(ixy,2)+flxpon(ixy,3)) &
!	   ,(flxpoc(ixy,1)+flxpoc(ixy,2)+flxpoc(ixy,3))            &
!	   ,jp,jn,jc,ctemp(ixy) 	   
!	   
!        IF(NGID(IXY).EQ.1780) write(1780,'(I6,F12.3,I8,15F12.3)')MYID,TIME, &
!	  NGID(IXY),vx(ixy),vy(ixy),jnh4,jno3,jsi,jpo4,sod,bnthden,jhs &
!	  ,(flxpon(ixy,1)+flxpon(ixy,2)+flxpon(ixy,3)) &
!	   ,(flxpoc(ixy,1)+flxpoc(ixy,2)+flxpoc(ixy,3))            &
!	   ,jp,jn,jc,ctemp(ixy) 	   
!	   
!        IF(NGID(IXY).EQ.2038) write(2038,'(I6,F12.3,I8,15F12.3)')MYID,TIME, &
!	  NGID(IXY),vx(ixy),vy(ixy),jnh4,jno3,jsi,jpo4,sod,bnthden,jhs &
!	  ,(flxpon(ixy,1)+flxpon(ixy,2)+flxpon(ixy,3)) &
!	   ,(flxpoc(ixy,1)+flxpoc(ixy,2)+flxpoc(ixy,3))            &
!	   ,jp,jn,jc,ctemp(ixy) 	   
!	   
!        IF(NGID(IXY).EQ.1119) write(1119,'(I6,F12.3,I8,15F12.3)')MYID,TIME, &
!	  NGID(IXY),vx(ixy),vy(ixy),jnh4,jno3,jsi,jpo4,sod,bnthden,jhs &
!	  ,(flxpon(ixy,1)+flxpon(ixy,2)+flxpon(ixy,3)) &
!	   ,(flxpoc(ixy,1)+flxpoc(ixy,2)+flxpoc(ixy,3))            &
!	   ,jp,jn,jc,ctemp(ixy) 	   
!	   
!        IF(NGID(IXY).EQ.3805) write(3805,'(I6,F12.3,I8,15F12.3)')MYID,TIME, &
!	  NGID(IXY),vx(ixy),vy(ixy),jnh4,jno3,jsi,jpo4,sod,bnthden,jhs &
!	  ,(flxpon(ixy,1)+flxpon(ixy,2)+flxpon(ixy,3)) &
!	   ,(flxpoc(ixy,1)+flxpoc(ixy,2)+flxpoc(ixy,3))            &
!	   ,jp,jn,jc,ctemp(ixy) 	   
!	   
!        IF(NGID(IXY).EQ.2107) write(2107,'(I6,F12.3,I8,15F12.3)')MYID,TIME, &
!	  NGID(IXY),vx(ixy),vy(ixy),jnh4,jno3,jsi,jpo4,sod,bnthden,jhs &
!	  ,(flxpon(ixy,1)+flxpon(ixy,2)+flxpon(ixy,3)) &
!	   ,(flxpoc(ixy,1)+flxpoc(ixy,2)+flxpoc(ixy,3))            &
!	   ,jp,jn,jc,ctemp(ixy) 
!	   
!        IF(NGID(IXY).EQ.2693) write(2693,'(I6,F12.3,I8,15F12.3)')MYID,TIME, &
!	  NGID(IXY),vx(ixy),vy(ixy),jnh4,jno3,jsi,jpo4,sod,bnthden,jhs &
!	  ,(flxpon(ixy,1)+flxpon(ixy,2)+flxpon(ixy,3)) &
!	   ,(flxpoc(ixy,1)+flxpoc(ixy,2)+flxpoc(ixy,3))            &
!	   ,jp,jn,jc,ctemp(ixy)	   	   
!
!          write(14)MYID,NGID(IXY),vx(ixy),vy(ixy),ctemp(ixy),pop1r,pop2r,pop3r,popr       &
!          ,pon1r,pon2r,pon3r,ponr,poc1r,poc2r,poc3r,pocr          &
!          ,po4t2r,hst2r,sit2r,psiavr                              &
!          ,(flxpop(ixy,1)+flxpop(ixy,2)+flxpop(ixy,3))            &
!          ,(flxpon(ixy,1)+flxpon(ixy,2)+flxpon(ixy,3))            &
!          ,(flxpoc(ixy,1)+flxpoc(ixy,2)+flxpoc(ixy,3))            &
!          ,o20,csod,sod,s,jp,jn,jc,jo2nh4,xjco2av,xjc1av          &
!          ,jpo4,jnh4,jno3,jhs,jsi,jch4aq,jch4g,h1                 &
!          ,po40,po41,po42,po4t2,nh40,nh41,nh42,nh4t2              &
!          ,no30,no31,no32,no3t2,hs1,hs2,hst2,si0,si1,si2,sit2     &
!          ,bnthden,ch41,ch42,ch4t2,so41,so42,so4t2                &
!          ,flxpop(ixy,1),flxpop(ixy,2),flxpop(ixy,3)              &
!          ,flxpon(ixy,1),flxpon(ixy,2),flxpon(ixy,3)              &
!          ,flxpoc(ixy,1),flxpoc(ixy,2),flxpoc(ixy,3)              &
!          ,flxpos(ixy),flxpo4t2s(ixy),flxsit2s(ixy)
!	      ELSE
             IF(PAR) THEN          
           CTEMP_GDA(IXY)=ctemp(ixy);POP1R_GDA(IXY)=pop1r;POP2R_GDA(IXY)=pop2r
	   POP3R_GDA(IXY) =pop3r;POPR_GDA(IXY) =popr;PON1R_GDA(IXY)=pon1r
	   PON2R_GDA(IXY)=pon2r;PON3R_GDA(IXY) =pon3r
	   PONR_GDA(IXY)  =ponr;POC1R_GDA(IXY) =poc1r;POC2R_GDA(IXY) =poc2r
	   POC3R_GDA(IXY) =poc3r;POCR_GDA(IXY)  =pocr;PO4T2R_GDA(IXY) =po4t2r
	   HST2R_GDA(IXY) =hst2r;SIT2R_GDA(IXY) =sit2r;PSIAVR_GDA(IXY) =psiavr
           FLXPOP_GDA(IXY) =(flxpop(ixy,1)+flxpop(ixy,2)+flxpop(ixy,3))
           FLXPON_GDA(IXY) =(flxpon(ixy,1)+flxpon(ixy,2)+flxpon(ixy,3))
           FLXPOC_GDA(IXY) = (flxpoc(ixy,1)+flxpoc(ixy,2)+flxpoc(ixy,3))
           O20_GDA(IXY)=o20;CSOD_GDA(IXY)=csod;SOD_GDA(IXY)=sod;S_GDA(IXY)=s
	   JP_GDA(IXY)=jp;JN_GDA(IXY)=jn;JC_GDA(IXY)=jc;JO2NH4_GDA(IXY)=jo2nh4
	   XJCO2AV_GDA(IXY)=xjco2av;XJC1AV_GDA(IXY)=xjc1av;JPO4_GDA(IXY)=jpo4
	   JNH4_GDA(IXY)=jnh4;JNO3_GDA(IXY)=jno3
	   JH2S_GDA(IXY)=jhs;JSI_GDA(IXY)=jsi;JCH4AQ_GDA(IXY)=jch4aq;JCH4G_GDA(IXY)=jch4g
	   H1_GDA(IXY)=h1;PO40_GDA(IXY)=po40;PO41_GDA(IXY)=po41;PO42_GDA(IXY)=po42
	   PO4T2_GDA(IXY)=po4t2;NH40_GDA(IXY)=nh40;NH41_GDA(IXY)=nh41;NH42_GDA(IXY)=nh42
	   NH4T2_GDA(IXY)=nh4t2;NO30_GDA(IXY)=no30;NO31_GDA(IXY)=no31;NO32_GDA(IXY)=no32
	   NO3T2_GDA(IXY)=no3t2;HS1_GDA(IXY)=hs1;HS2_GDA(IXY)=hs2;HST2_GDA(IXY)=hst2
	   SI0_GDA(IXY)=si0;SI1_GDA(IXY)=si1;SI2_GDA(IXY)=si2;SIT2_GDA(IXY)=sit2
	   JN2_GDA(IXY)=bnthden;CH41_GDA(IXY)=ch41;CH42_GDA(IXY)=ch42;CH4T2_GDA(IXY)=ch4t2
	   SO41_GDA(IXY)=so41;SO42_GDA(IXY)=so42;SO4T2_GDA(IXY)=so4t2;FLXPOP1_GDA(IXY)=flxpop(ixy,1)
	   FLXPOP2_GDA(IXY)=flxpop(ixy,2);FLXPOP3_GDA(IXY)=flxpop(ixy,3);FLXPON1_GDA(IXY)=flxpon(ixy,1)
	   FLXPON2_GDA(IXY)=flxpon(ixy,2);FLXPON3_GDA(IXY)=flxpon(ixy,3);FLXPOC1_GDA(IXY)=flxpoc(ixy,1)
	   FLXPOC2_GDA(IXY) =flxpoc(ixy,2);FLXPOC3_GDA(IXY)=flxpoc(ixy,3);
           FLXPOS_GDA(IXY)=flxpos(ixy);FLXPO4T2S_GDA(IXY)=flxpo4t2s(ixy);FLXSIT2S_GDA(IXY)=flxsit2s(ixy)	      
             END IF !(PAR)
	     ! END IF  !(SERIAL)	  
	  
          if(iseddisk.eq.1) then
           jpo4_gda(ixy)=jpo4
           jnh4_gda(ixy)=jnh4
           jno3_gda(ixy)=jno3
           jn2_gda(ixy)=bnthden
           jsi_gda(ixy)=jsi
           jh2s_gda(ixy)=jhs
           jch4aq_gda(ixy)=jch4aq
           jch4g_gda(ixy)=jch4g
           sod_gda(ixy)=sod	   
          endif	  
         ELSE
!        Global Dumps
          CTEMP_GDA(IXY) = CTEMP_GDA(IXY)/FLOAT(IGDCNT)
          POP1R_GDA(IXY) = POP1R_GDA(IXY)/FLOAT(IGDCNT)
          POP2R_GDA(IXY) = POP2R_GDA(IXY)/FLOAT(IGDCNT)
          POP3R_GDA(IXY) = POP3R_GDA(IXY)/FLOAT(IGDCNT)
          POPR_GDA(IXY)  = POPR_GDA(IXY)/FLOAT(IGDCNT)
          PON1R_GDA(IXY) = PON1R_GDA(IXY)/FLOAT(IGDCNT)
          PON2R_GDA(IXY) = PON2R_GDA(IXY)/FLOAT(IGDCNT)
          PON3R_GDA(IXY) = PON3R_GDA(IXY)/FLOAT(IGDCNT)
          PONR_GDA(IXY)  = PONR_GDA(IXY)/FLOAT(IGDCNT)
          POC1R_GDA(IXY) = POC1R_GDA(IXY)/FLOAT(IGDCNT)
          POC2R_GDA(IXY) = POC2R_GDA(IXY)/FLOAT(IGDCNT)
          POC3R_GDA(IXY) = POC3R_GDA(IXY)/FLOAT(IGDCNT)
          POCR_GDA(IXY)  = POCR_GDA(IXY)/FLOAT(IGDCNT)
          PO4T2R_GDA(IXY) = PO4T2R_GDA(IXY)/FLOAT(IGDCNT)
          HST2R_GDA(IXY) = HST2R_GDA(IXY)/FLOAT(IGDCNT)
          SIT2R_GDA(IXY) = SIT2R_GDA(IXY)/FLOAT(IGDCNT)
          PSIAVR_GDA(IXY) = PSIAVR_GDA(IXY)/FLOAT(IGDCNT)
          FLXPOP_GDA(IXY) = FLXPOP_GDA(IXY)/FLOAT(IGDCNT)
          FLXPOP1_GDA(IXY) = FLXPOP1_GDA(IXY)/FLOAT(IGDCNT)
          FLXPOP2_GDA(IXY) = FLXPOP2_GDA(IXY)/FLOAT(IGDCNT)
          FLXPOP3_GDA(IXY) = FLXPOP3_GDA(IXY)/FLOAT(IGDCNT)
          FLXPON_GDA(IXY) = FLXPON_GDA(IXY)/FLOAT(IGDCNT)
          FLXPON1_GDA(IXY) = FLXPON1_GDA(IXY)/FLOAT(IGDCNT)
          FLXPON2_GDA(IXY) = FLXPON2_GDA(IXY)/FLOAT(IGDCNT)
          FLXPON3_GDA(IXY) = FLXPON3_GDA(IXY)/FLOAT(IGDCNT)
          FLXPOC_GDA(IXY) = FLXPOC_GDA(IXY)/FLOAT(IGDCNT)
          FLXPOC1_GDA(IXY) = FLXPOC1_GDA(IXY)/FLOAT(IGDCNT)
          FLXPOC2_GDA(IXY) = FLXPOC2_GDA(IXY)/FLOAT(IGDCNT)
          FLXPOC3_GDA(IXY) = FLXPOC3_GDA(IXY)/FLOAT(IGDCNT)
          FLXPOS_GDA(IXY) = FLXPOS_GDA(IXY)/FLOAT(IGDCNT)
          FLXPO4T2S_GDA(IXY) = FLXPO4T2S_GDA(IXY)/FLOAT(IGDCNT)
          FLXSIT2S_GDA(IXY) = FLXSIT2S_GDA(IXY)/FLOAT(IGDCNT)
          O20_GDA(IXY)   = O20_GDA(IXY)/FLOAT(IGDCNT)
          CSOD_GDA(IXY)  = CSOD_GDA(IXY)/FLOAT(IGDCNT)
          SOD_GDA(IXY)   = SOD_GDA(IXY)/FLOAT(IGDCNT)
          S_GDA(IXY)     = S_GDA(IXY)/FLOAT(IGDCNT)
          JP_GDA(IXY)    = JP_GDA(IXY)/FLOAT(IGDCNT)
          JN_GDA(IXY)    = JN_GDA(IXY)/FLOAT(IGDCNT)
          JC_GDA(IXY)    = JC_GDA(IXY)/FLOAT(IGDCNT)
          JO2NH4_GDA(IXY) = JO2NH4_GDA(IXY)/FLOAT(IGDCNT)
          XJCO2AV_GDA(IXY) = XJCO2AV_GDA(IXY)/FLOAT(IGDCNT)
          XJC1AV_GDA(IXY) = XJC1AV_GDA(IXY)/FLOAT(IGDCNT)
          JPO4_GDA(IXY)  = JPO4_GDA(IXY)/FLOAT(IGDCNT)
          JNH4_GDA(IXY)  = JNH4_GDA(IXY)/FLOAT(IGDCNT)
          JNO3_GDA(IXY)  = JNO3_GDA(IXY)/FLOAT(IGDCNT)
          JH2S_GDA(IXY)   = JH2S_GDA(IXY)/FLOAT(IGDCNT)
          JSI_GDA(IXY)   = JSI_GDA(IXY)/FLOAT(IGDCNT)
          JCH4AQ_GDA(IXY) = JCH4AQ_GDA(IXY)/FLOAT(IGDCNT)
          JCH4G_GDA(IXY) = JCH4G_GDA(IXY)/FLOAT(IGDCNT)
          H1_GDA(IXY)    = H1_GDA(IXY) /FLOAT(IGDCNT)
          PO40_GDA(IXY)  = PO40_GDA(IXY)/FLOAT(IGDCNT)
          PO41_GDA(IXY)  = PO41_GDA(IXY)/FLOAT(IGDCNT)
          PO42_GDA(IXY)  = PO42_GDA(IXY)/FLOAT(IGDCNT)
          PO4T2_GDA(IXY) = PO4T2_GDA(IXY)/FLOAT(IGDCNT)
          NH40_GDA(IXY)  = NH40_GDA(IXY)/FLOAT(IGDCNT)
          NH41_GDA(IXY)  = NH41_GDA(IXY)/FLOAT(IGDCNT)
          NH42_GDA(IXY)  = NH42_GDA(IXY)/FLOAT(IGDCNT)
          NH4T2_GDA(IXY) = NH4T2_GDA(IXY)/FLOAT(IGDCNT)
          NO30_GDA(IXY)  = NO30_GDA(IXY)/FLOAT(IGDCNT)
          NO31_GDA(IXY)  = NO31_GDA(IXY)/FLOAT(IGDCNT)
          NO32_GDA(IXY)  = NO32_GDA(IXY)/FLOAT(IGDCNT)
          NO3T2_GDA(IXY) = NO3T2_GDA(IXY)/FLOAT(IGDCNT)
          HS1_GDA(IXY)   = HS1_GDA(IXY)/FLOAT(IGDCNT)
          HS2_GDA(IXY)   = HS2_GDA(IXY)/FLOAT(IGDCNT)
          HST2_GDA(IXY)  = HST2_GDA(IXY)/FLOAT(IGDCNT)
          SI0_GDA(IXY)   = SI0_GDA(IXY)/FLOAT(IGDCNT)
          SI1_GDA(IXY)   = SI1_GDA(IXY)/FLOAT(IGDCNT)
          SI2_GDA(IXY)   = SI2_GDA(IXY)/FLOAT(IGDCNT)
          SIT2_GDA(IXY)  = SIT2_GDA(IXY)/FLOAT(IGDCNT)
          JN2_GDA(IXY) = JN2_GDA(IXY)/FLOAT(IGDCNT)
          CH41_GDA(IXY) = CH41_GDA(IXY)/FLOAT(IGDCNT)
          CH42_GDA(IXY) = CH42_GDA(IXY)/FLOAT(IGDCNT)
          CH4T2_GDA(IXY) = CH4T2_GDA(IXY)/FLOAT(IGDCNT)
          SO41_GDA(IXY) = SO41_GDA(IXY)/FLOAT(IGDCNT)
          SO42_GDA(IXY) = SO42_GDA(IXY)/FLOAT(IGDCNT)
          SO4T2_GDA(IXY) = SO4T2_GDA(IXY)/FLOAT(IGDCNT)
	  
          IF(SERIAL) THEN 
	   WRITE(14)  CTEMP_GDA(IXY),POP1R_GDA(IXY),          &
           POP2R_GDA(IXY),POP3R_GDA(IXY),POPR_GDA(IXY),           &
           PON1R_GDA(IXY),PON2R_GDA(IXY),PON3R_GDA(IXY),          &
           PONR_GDA(IXY),POC1R_GDA(IXY),POC2R_GDA(IXY),           &
           POC3R_GDA(IXY),POCR_GDA(IXY),PO4T2R_GDA(IXY),          &
           HST2R_GDA(IXY),SIT2R_GDA(IXY),PSIAVR_GDA(IXY),         &
           FLXPOP_GDA(IXY),FLXPON_GDA(IXY),FLXPOC_GDA(IXY),       &
           O20_GDA(IXY),CSOD_GDA(IXY),SOD_GDA(IXY),S_GDA(IXY),    &
           JP_GDA(IXY),JN_GDA(IXY),JC_GDA(IXY),JO2NH4_GDA(IXY),   &
           XJCO2AV_GDA(IXY),XJC1AV_GDA(IXY),JPO4_GDA(IXY),        &
           JNH4_GDA(IXY),JNO3_GDA(IXY),JH2S_GDA(IXY),             &
           JSI_GDA(IXY),JCH4AQ_GDA(IXY),JCH4G_GDA(IXY),           &
           H1_GDA(IXY),PO40_GDA(IXY),PO41_GDA(IXY),               &
           PO42_GDA(IXY),PO4T2_GDA(IXY),NH40_GDA(IXY),            &
           NH41_GDA(IXY),NH42_GDA(IXY),NH4T2_GDA(IXY),            &
           NO30_GDA(IXY),NO31_GDA(IXY),NO32_GDA(IXY),             &
           NO3T2_GDA(IXY),HS1_GDA(IXY),HS2_GDA(IXY),              &
           HST2_GDA(IXY),SI0_GDA(IXY),SI1_GDA(IXY),               &
           SI2_GDA(IXY),SIT2_GDA(IXY),JN2_GDA(IXY),               &
           CH41_GDA(IXY),CH42_GDA(IXY),CH4T2_GDA(IXY),            &
           SO41_GDA(IXY),SO42_GDA(IXY),SO4T2_GDA(IXY),            &
           FLXPOP1_GDA(IXY),FLXPOP2_GDA(IXY),FLXPOP3_GDA(IXY),    &
           FLXPON1_GDA(IXY),FLXPON2_GDA(IXY),FLXPON3_GDA(IXY),    &
           FLXPOC1_GDA(IXY),FLXPOC2_GDA(IXY),FLXPOC3_GDA(IXY),    &
           FLXPOS_GDA(IXY),FLXPO4T2S_GDA(IXY),FLXSIT2S_GDA(IXY)
!	  ELSE
!           CTEMP_TMP(NGID(IXY))=CTEMP_GDA(ixy);POP1R_TMP(NGID(IXY))=POP1R_GDA(IXY);POP2R_TMP(NGID(IXY))=POP2R_GDA(IXY)
!	   POP3R_TMP(NGID(IXY))=POP3R_GDA(IXY);POPR_TMP(NGID(IXY))=POPR_GDA(IXY);PON1R_TMP(NGID(IXY))=PON1R_GDA(IXY)
!	   PON2R_TMP(NGID(IXY))=PON2R_GDA(IXY);PON3R_TMP(NGID(IXY))=PON3R_GDA(IXY)
!	   PONR_TMP(NGID(IXY))=PONR_GDA(IXY);POC1R_TMP(NGID(IXY))=POC1R_GDA(IXY);POC2R_TMP(NGID(IXY))=POC2R_GDA(IXY)
!	   POC3R_TMP(NGID(IXY))=POC3R_GDA(IXY);POCR_TMP(NGID(IXY))=POCR_GDA(IXY);PO4T2R_TMP(NGID(IXY))=PO4T2R_GDA(IXY)
!	   HST2R_TMP(NGID(IXY))=HST2R_GDA(IXY);SIT2R_TMP(NGID(IXY))=SIT2R_GDA(IXY);PSIAVR_TMP(NGID(IXY))=PSIAVR_GDA(IXY)
!           FLXPON_TMP(NGID(IXY))=FLXPON_GDA(IXY);FLXPOC_TMP(NGID(IXY))=FLXPOC_GDA(IXY)
!           O20_TMP(NGID(IXY))=O20_GDA(IXY);CSOD_TMP(NGID(IXY))=CSOD_GDA(IXY);SOD_TMP(NGID(IXY))=SOD_GDA(IXY);
!	   S_TMP(NGID(IXY))=S_GDA(IXY);JP_TMP(NGID(IXY))=JP_GDA(IXY);JN_TMP(NGID(IXY))=JN_GDA(IXY)
!	   JC_TMP(NGID(IXY))=JC_GDA(IXY);JO2NH4_TMP(NGID(IXY))=JO2NH4_GDA(IXY);XJCO2AV_TMP(NGID(IXY))=XJCO2AV_GDA(IXY)
!	   XJC1AV_TMP(NGID(IXY))=XJC1AV_GDA(IXY);JPO4_TMP(NGID(IXY))=JPO4_GDA(IXY);JNH4_TMP(NGID(IXY))=JNH4_GDA(IXY)
!	   JNO3_TMP(NGID(IXY))=JNO3_GDA(IXY);JH2S_TMP(NGID(IXY))=JH2S_GDA(IXY);JSI_TMP(NGID(IXY))=JSI_GDA(IXY)
!	   JCH4AQ_TMP(NGID(IXY))=JCH4AQ_GDA(IXY);JCH4G_TMP(NGID(IXY))=JCH4G_GDA(IXY)
!	   H1_TMP(NGID(IXY))=H1_GDA(IXY);PO40_TMP(NGID(IXY))=PO40_GDA(IXY);PO41_TMP(NGID(IXY))=PO41_GDA(IXY)
!	   PO42_TMP(NGID(IXY))= PO42_GDA(IXY);PO4T2_TMP(NGID(IXY))=PO4T2_GDA(IXY);NH40_TMP(NGID(IXY))=NH40_GDA(IXY)
!	   NH41_TMP(NGID(IXY))=NH41_GDA(IXY);NH42_TMP(NGID(IXY))=NH42_GDA(IXY)
!	   NH4T2_TMP(NGID(IXY))=NH4T2_GDA(IXY);NO30_TMP(NGID(IXY))=NO30_GDA(IXY);NO31_TMP(NGID(IXY))=NO31_TMP(IXY)
!	   NO32_TMP(NGID(IXY))=NO32_GDA(IXY);NO3T2_TMP(NGID(IXY))=NO3T2_GDA(IXY);HS1_TMP(NGID(IXY))=HS1_GDA(IXY)
!	   HS2_TMP(NGID(IXY))=HS2_GDA(IXY);HST2_TMP(NGID(IXY))=HST2_GDA(IXY);SI0_TMP(NGID(IXY))=SI0_GDA(IXY)
!	   SI1_TMP(NGID(IXY))=SI1_GDA(IXY);SI2_TMP(NGID(IXY))=SI2_GDA(IXY);SIT2_TMP(NGID(IXY))=SIT2_GDA(IXY)
!	   JN2_TMP(NGID(IXY))=JN2_GDA(IXY);CH41_TMP(NGID(IXY))=CH41_GDA(IXY);CH42_TMP(NGID(IXY))=CH42_GDA(IXY)
!	   CH4T2_TMP(NGID(IXY))=CH4T2_GDA(IXY);SO41_TMP(NGID(IXY))=SO41_GDA(IXY);SO42_TMP(NGID(IXY))=SO42_GDA(IXY)
!	   SO4T2_TMP(NGID(IXY))=SO4T2_GDA(IXY);FLXPOP1_TMP(NGID(IXY))=FLXPOP1_GDA(IXY)
!	   FLXPOP2_TMP(NGID(IXY))=FLXPOP2_GDA(IXY);FLXPOP3_TMP(NGID(IXY))=FLXPOP3_GDA(IXY)
!	   FLXPON1_TMP(NGID(IXY))=FLXPON1_GDA(IXY);FLXPON2_TMP(NGID(IXY))=FLXPON2_GDA(IXY)
!	   FLXPON3_TMP(NGID(IXY))=FLXPON3_GDA(IXY);FLXPOC1_TMP(NGID(IXY))=FLXPOC1_GDA(IXY)
!	   FLXPOC2_TMP(NGID(IXY)) =FLXPOC2_GDA(IXY);FLXPOC3_TMP(NGID(IXY))=FLXPOC3_GDA(IXY)
!           FLXPOS_TMP(NGID(IXY))=FLXPOS_GDA(IXY);FLXPO4T2S_TMP(NGID(IXY))=FLXPO4T2S_GDA(IXY)
!	   FLXSIT2S_TMP(NGID(IXY))=FLXSIT2S_GDA(IXY)	  
	  
	  END IF !(SERIAL) 	   	   
	   
         END IF !IGPOT
        END IF     ! endif for itimesecs .ge. nxprtsed

        CALL STORESED

  200 CONTINUE
!################   OUTPUT   ###########
# if defined (MULTIPROCESSOR)
!CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
        if(itimesecs >= nxprtsed) then
	   IF (PAR) THEN 	   	
!          IF (MSR) WRITE(14) TIME,CTEMP_TMP,POP1R_TMP,          &
!           POP2R_TMP,POP3R_TMP,POPR_TMP,           &
!           PON1R_TMP,PON2R_TMP,PON3R_TMP,          &
!           PONR_TMP,POC1R_TMP,POC2R_TMP,           &
!           POC3R_TMP,POCR_TMP,PO4T2R_TMP,          &
!           HST2R_TMP,SIT2R_TMP,PSIAVR_TMP,         &
!           FLXPOP_TMP,FLXPON_TMP,FLXPOC_TMP,       &
!           O20_TMP,CSOD_TMP,SOD_TMP,S_TMP,    &
!           JP_TMP,JN_TMP,JC_TMP,JO2NH4_TMP,   &
!           XJCO2AV_TMP,XJC1AV_TMP,JPO4_TMP,        &
!           JNH4_TMP,JNO3_TMP,JH2S_TMP,             &
!           JSI_TMP,JCH4AQ_TMP,JCH4G_TMP,           &
!           H1_TMP,PO40_TMP,PO41_TMP,               &
!           PO42_TMP,PO4T2_TMP,NH40_TMP,            &
!           NH41_TMP,NH42_TMP,NH4T2_TMP,            &
!           NO30_TMP,NO31_TMP,NO32_TMP,             &
!           NO3T2_TMP,HS1_TMP,HS2_TMP,              &
!           HST2_TMP,SI0_TMP,SI1_TMP,               &
!           SI2_TMP,SIT2_TMP,JN2_TMP,               &
!           CH41_TMP,CH42_TMP,CH4T2_TMP,            &
!           SO41_TMP,SO42_TMP,SO4T2_TMP,            &
!           FLXPOP1_TMP,FLXPOP2_TMP,FLXPOP3_TMP,    &
!           FLXPON1_TMP,FLXPON2_TMP,FLXPON3_TMP,    &
!           FLXPOC1_TMP,FLXPOC2_TMP,FLXPOC3_TMP,    &
!           FLXPOS_TMP,FLXPO4T2S_TMP,FLXSIT2S_TMP        
	  

        CALL PWRITE(14,MYID,NPROCS,CTEMP_GDA,1,MT,M,NXY,1,NMAP,1,"CTEMP_GDA")
        CALL PWRITE(14,MYID,NPROCS,POP1R_GDA,1,MT,M,NXY,1,NMAP,1,"POP1R_GDA")
        CALL PWRITE(14,MYID,NPROCS,POP2R_GDA,1,MT,M,NXY,1,NMAP,1,"POP2R_GDA")
        CALL PWRITE(14,MYID,NPROCS,POP3R_GDA,1,MT,M,NXY,1,NMAP,1,"POP3R_GDA")	
        CALL PWRITE(14,MYID,NPROCS,POPR_GDA,1,MT,M,NXY,1,NMAP,1,"POPR_GDA")					
        CALL PWRITE(14,MYID,NPROCS,PON1R_GDA,1,MT,M,NXY,1,NMAP,1,"PON1R_GDA")
        CALL PWRITE(14,MYID,NPROCS,PON2R_GDA,1,MT,M,NXY,1,NMAP,1,"PON2R")	     				
        CALL PWRITE(14,MYID,NPROCS,PON3R_GDA,1,MT,M,NXY,1,NMAP,1,"PON3R_GDA")
        CALL PWRITE(14,MYID,NPROCS,PONR_GDA,1,MT,M,NXY,1,NMAP,1,"PONR_GDA")			
        CALL PWRITE(14,MYID,NPROCS,POC1R_GDA,1,MT,M,NXY,1,NMAP,1,"POC1R_GDA")
        CALL PWRITE(14,MYID,NPROCS,POC2R_GDA,1,MT,M,NXY,1,NMAP,1,"POC2R_GDA")	     				
        CALL PWRITE(14,MYID,NPROCS,POC3R_GDA,1,MT,M,NXY,1,NMAP,1,"POC3R_GDA")
        CALL PWRITE(14,MYID,NPROCS,POCR_GDA,1,MT,M,NXY,1,NMAP,1,"POCR_GDA")			
        CALL PWRITE(14,MYID,NPROCS,PO4T2R_GDA,1,MT,M,NXY,1,NMAP,1,"PO4T2R_GDA")
        CALL PWRITE(14,MYID,NPROCS,HST2R_GDA,1,MT,M,NXY,1,NMAP,1,"HST2R_GDA")	     				
        CALL PWRITE(14,MYID,NPROCS,SIT2R_GDA,1,MT,M,NXY,1,NMAP,1,"SIT2R_GDA")
        CALL PWRITE(14,MYID,NPROCS,PSIAVR_GDA,1,MT,M,NXY,1,NMAP,1,"PSIAVR_GDA")			
        CALL PWRITE(14,MYID,NPROCS,FLXPOP_GDA,1,MT,M,NXY,1,NMAP,1,"FLXPOP_GDA")
        CALL PWRITE(14,MYID,NPROCS,FLXPON_GDA,1,MT,M,NXY,1,NMAP,1,"FLXPON_GDA")	     				
        CALL PWRITE(14,MYID,NPROCS,FLXPOC_GDA,1,MT,M,NXY,1,NMAP,1,"FLXPOC_GDA")
        CALL PWRITE(14,MYID,NPROCS,O20_GDA,1,MT,M,NXY,1,NMAP,1,"O20_GDA")			
        CALL PWRITE(14,MYID,NPROCS,CSOD_GDA,1,MT,M,NXY,1,NMAP,1,"CSOD_GDA")
        CALL PWRITE(14,MYID,NPROCS,SOD_GDA,1,MT,M,NXY,1,NMAP,1,"SOD_GDA")	     				
        CALL PWRITE(14,MYID,NPROCS,S_GDA,1,MT,M,NXY,1,NMAP,1,"S_GDA")
        CALL PWRITE(14,MYID,NPROCS,JP_GDA,1,MT,M,NXY,1,NMAP,1,"JP_GDA")			
        CALL PWRITE(14,MYID,NPROCS,JN_GDA,1,MT,M,NXY,1,NMAP,1,"JN_GDA")
        CALL PWRITE(14,MYID,NPROCS,JC_GDA,1,MT,M,NXY,1,NMAP,1,"JC_GDA")	     				
        CALL PWRITE(14,MYID,NPROCS,JO2NH4_GDA,1,MT,M,NXY,1,NMAP,1,"JO2NH4_GDA")
        CALL PWRITE(14,MYID,NPROCS,XJCO2AV_GDA,1,MT,M,NXY,1,NMAP,1,"XJCO2AV_GDA")			
        CALL PWRITE(14,MYID,NPROCS,XJC1AV_GDA,1,MT,M,NXY,1,NMAP,1,"XJC1AV_GDA")
        CALL PWRITE(14,MYID,NPROCS,JPO4_GDA,1,MT,M,NXY,1,NMAP,1,"JPO4_GDA")	     				
        CALL PWRITE(14,MYID,NPROCS,JNH4_GDA,1,MT,M,NXY,1,NMAP,1,"JNH4_GDA")
        CALL PWRITE(14,MYID,NPROCS,JNO3_GDA,1,MT,M,NXY,1,NMAP,1,"JNO3_GDA")			
        CALL PWRITE(14,MYID,NPROCS,JH2S_GDA,1,MT,M,NXY,1,NMAP,1,"JH2S_GDA")
        CALL PWRITE(14,MYID,NPROCS,JSI_GDA,1,MT,M,NXY,1,NMAP,1,"JSI_GDA")	     				
        CALL PWRITE(14,MYID,NPROCS,JCH4AQ_GDA,1,MT,M,NXY,1,NMAP,1,"JCH4AQ_GDA")
        CALL PWRITE(14,MYID,NPROCS,JCH4G_GDA,1,MT,M,NXY,1,NMAP,1,"JCH4G_GDA")			
        CALL PWRITE(14,MYID,NPROCS,H1_GDA,1,MT,M,NXY,1,NMAP,1,"H1_GDA")
        CALL PWRITE(14,MYID,NPROCS,PO40_GDA,1,MT,M,NXY,1,NMAP,1,"PO40_GDA")	     				
        CALL PWRITE(14,MYID,NPROCS,PO41_GDA,1,MT,M,NXY,1,NMAP,1,"PO41_GDA")
        CALL PWRITE(14,MYID,NPROCS,PO42_GDA,1,MT,M,NXY,1,NMAP,1,"PO42_GDA")
        CALL PWRITE(14,MYID,NPROCS,PO4T2_GDA,1,MT,M,NXY,1,NMAP,1,"PO4T2_GDA")
        CALL PWRITE(14,MYID,NPROCS,NH40_GDA,1,MT,M,NXY,1,NMAP,1,"NH40_GDA")	     				
        CALL PWRITE(14,MYID,NPROCS,NH41_GDA,1,MT,M,NXY,1,NMAP,1,"NH41_GDA")
        CALL PWRITE(14,MYID,NPROCS,NH42_GDA,1,MT,M,NXY,1,NMAP,1,"NH42_GDA")
        CALL PWRITE(14,MYID,NPROCS,NH4T2_GDA,1,MT,M,NXY,1,NMAP,1,"NH4T2_GDA")
        CALL PWRITE(14,MYID,NPROCS,NO30_GDA,1,MT,M,NXY,1,NMAP,1,"NO30_GDA")	     				
        CALL PWRITE(14,MYID,NPROCS,NO31_GDA,1,MT,M,NXY,1,NMAP,1,"NO31_GDA")
        CALL PWRITE(14,MYID,NPROCS,NO32_GDA,1,MT,M,NXY,1,NMAP,1,"NO32_GDA")			
        CALL PWRITE(14,MYID,NPROCS,NO3T2_GDA,1,MT,M,NXY,1,NMAP,1,"NO3T2_GDA")
        CALL PWRITE(14,MYID,NPROCS,HS1_GDA,1,MT,M,NXY,1,NMAP,1,"HS1_GDA")	     				
        CALL PWRITE(14,MYID,NPROCS,HS2_GDA,1,MT,M,NXY,1,NMAP,1,"HS2_GDA")
        CALL PWRITE(14,MYID,NPROCS,HST2_GDA,1,MT,M,NXY,1,NMAP,1,"HST2_GDA")
        CALL PWRITE(14,MYID,NPROCS,SI0_GDA,1,MT,M,NXY,1,NMAP,1,"SI0_GDA")	     				
        CALL PWRITE(14,MYID,NPROCS,SI1_GDA,1,MT,M,NXY,1,NMAP,1,"SI1_GDA")
        CALL PWRITE(14,MYID,NPROCS,SI2_GDA,1,MT,M,NXY,1,NMAP,1,"SI2_GDA")			
        CALL PWRITE(14,MYID,NPROCS,SIT2_GDA,1,MT,M,NXY,1,NMAP,1,"SIT2_GDA")
        CALL PWRITE(14,MYID,NPROCS,JN2_GDA,1,MT,M,NXY,1,NMAP,1,"JN2_GDA")	     				
        CALL PWRITE(14,MYID,NPROCS,CH41_GDA,1,MT,M,NXY,1,NMAP,1,"CH41_GDA")
        CALL PWRITE(14,MYID,NPROCS,CH42_GDA,1,MT,M,NXY,1,NMAP,1,"CH42_GDA")			
        CALL PWRITE(14,MYID,NPROCS,CH4T2_GDA,1,MT,M,NXY,1,NMAP,1,"CH4T2_GDA")
        CALL PWRITE(14,MYID,NPROCS,SO41_GDA,1,MT,M,NXY,1,NMAP,1,"SO41_GDA")	     				
        CALL PWRITE(14,MYID,NPROCS,SO42_GDA,1,MT,M,NXY,1,NMAP,1,"SO42_GDA")
        CALL PWRITE(14,MYID,NPROCS,SO4T2_GDA,1,MT,M,NXY,1,NMAP,1,"SO4T2_GDA")			
        CALL PWRITE(14,MYID,NPROCS,FLXPOP1_GDA,1,MT,M,NXY,1,NMAP,1,"FLXPOP1_GDA")
        CALL PWRITE(14,MYID,NPROCS,FLXPOP2_GDA,1,MT,M,NXY,1,NMAP,1,"FLXPOP2_GDA")	     				
        CALL PWRITE(14,MYID,NPROCS,FLXPOP3_GDA,1,MT,M,NXY,1,NMAP,1,"FLXPOP3_GDA")	     	
        CALL PWRITE(14,MYID,NPROCS,FLXPON1_GDA,1,MT,M,NXY,1,NMAP,1,"FLXPON1_GDA")
        CALL PWRITE(14,MYID,NPROCS,FLXPON2_GDA,1,MT,M,NXY,1,NMAP,1,"FLXPON2_GDA")	     				
        CALL PWRITE(14,MYID,NPROCS,FLXPON3_GDA,1,MT,M,NXY,1,NMAP,1,"FLXPON3_GDA")
        CALL PWRITE(14,MYID,NPROCS,FLXPOC1_GDA,1,MT,M,NXY,1,NMAP,1,"FLXPOC1_GDA")			
        CALL PWRITE(14,MYID,NPROCS,FLXPOC2_GDA,1,MT,M,NXY,1,NMAP,1,"FLXPOC2_GDA")
        CALL PWRITE(14,MYID,NPROCS,FLXPOC3_GDA,1,MT,M,NXY,1,NMAP,1,"FLXPOC3_GDA")	     				
        CALL PWRITE(14,MYID,NPROCS,FLXPOS_GDA,1,MT,M,NXY,1,NMAP,1,"FLXPOS_GDA")
        CALL PWRITE(14,MYID,NPROCS,FLXPO4T2S_GDA,1,MT,M,NXY,1,NMAP,1,"FLXPO4T2S_GDA")			
        CALL PWRITE(14,MYID,NPROCS,FLXSIT2S_GDA,1,MT,M,NXY,1,NMAP,1,"FLXSIT2S_GDA")	   
	   END IF !(PAR)  	
	END IF  !itinestep
# endif  
  
  
      IF(IGDSEDOPT == 1) IGDCNT=IGDCNT+1

!     TAKE TEMPERATURE INTEGRATION STEP
      DO IXY=1,M
       IF(FSM(IXY) == 1.) THEN
        CTEMP(IXY) = CTEMP(IXY)+DELTAT*DIFFT/HSED(IXY)/          &
                     HSED(IXY)*(HYDTEMP(IXY,NZ)-CTEMP(IXY))
       CTEMP(IXY)=MIN(CTEMP(IXY),25.)
       END IF
      END DO

      IF(MASSBAL == 1) THEN
       TOTORGP=0.
       TOTORGN=0.
       TOTORGC=0.
       TOTBSI=0.
       TOTPO4=0.
       TOTNH4=0.
       TOTNO3=0.
       TOTSI=0.
       TOTH2S=0.
       TOTCH4=0.
       TOTSO4=0.
       DO IG=1,3
        DO IXY=1,M
         IF(FSM(IXY) == 1.) THEN
          TOTORGP=TOTORGP+CPOP(IXY,IG)*BSVOL(IXY)
          TOTORGN=TOTORGN+CPON(IXY,IG)*BSVOL(IXY)
          TOTORGC=TOTORGC+CPOC(IXY,IG)*BSVOL(IXY)
         ENDIF
        END DO
       END DO
       DO IXY=1,M
        IF(FSM(IXY) == 1.) THEN
         H1=H1S(IXY)
         TOTBSI=TOTBSI+CPOS(IXY)*BSVOL(IXY)
         TOTPO4=TOTPO4+XAZ(IXY)*(H1*PO4T1TM1S(IXY)+(H2-H1)*PO4T2TM1S(IXY))
         TOTNH4=TOTNH4+XAZ(IXY)*(H1*NH4T1TM1S(IXY)+(H2-H1)*NH4T2TM1S(IXY))
         TOTNO3=TOTNO3+XAZ(IXY)*(H1*NO3T1TM1S(IXY)+(H2-H1)*NO3T2TM1S(IXY))
         TOTSI=TOTSI+XAZ(IXY)*(H1*SIT1TM1S(IXY)+(H2-H1)*SIT2TM1S(IXY))
         TOTH2S=TOTH2S+XAZ(IXY)*(H1*HST1TM1S(IXY)+(H2-H1)*HST2TM1S(IXY))
         TOTCH4=TOTCH4+XAZ(IXY)*(H1*PO41TM1S(IXY)+(H2-H1)*PO4T2TM1S(IXY))
         TOTSO4=TOTSO4+XAZ(IXY)*(H1*SO41TM1S(IXY)+(H2-H1)*SO4T2TM1S(IXY))
        ENDIF
       END DO
       JPOP=0.
       JPON=0.
       JPOC=0.
       JBSI=0.
       JPO4SS=0.
       JSISS=0.
       BURIALORGP=0.
       BURIALORGN=0.
       BURIALORGC=0.
       BURIALBSI=0.
       BURIALPO4=0.
       BURIALNH4=0.
       BURIALNO3=0.
       BURIALSI=0.
       BURIALH2S=0.
       BURIALCH4=0.
       BURIALSO4=0.
       DO IG=1,3
        DO IXY=1,M
         IF(FSM(IXY) == 1.) THEN
          BURIALORGP=BURIALORGP+VSED(IXY)*CPOP(IXY,IG)
          BURIALORGN=BURIALORGN+VSED(IXY)*CPON(IXY,IG)
          BURIALORGC=BURIALORGC+VSED(IXY)*CPOC(IXY,IG)
         ENDIF
	END DO 
       END DO
       BURIALBSI=BURIALBSI+VSED(IXY)*CPOS(IXY)
       BURIALPO4=BURIALPO4+VSED(IXY)*PO4T2TM1S(IXY)
       BURIALNH4=BURIALNH4+VSED(IXY)*NH4T2TM1S(IXY)
       BURIALNO3=BURIALNO3+VSED(IXY)*NO3T2TM1S(IXY)
       BURIALSI=BURIALSI+VSED(IXY)*SIT2TM1S(IXY)
       BURIALH2S=BURIALH2S+VSED(IXY)*HST2TM1S(IXY)
       BURIALCH4=BURIALCH4+VSED(IXY)*CH4T2TM1S(IXY)
       BURIALSO4=BURIALSO4+VSED(IXY)*SO4T2TM1S(IXY)
       DO IXY=1,M
        IF(FSM(IXY) == 1.) THEN
         JPOP=JPOP+FLXPOP(IXY,1)+FLXPOP(IXY,2)+FLXPOP(IXY,3)
         JPON=JPON+FLXPON(IXY,1)+FLXPON(IXY,2)+FLXPON(IXY,3)
         JPOC=JPOC+FLXPOC(IXY,1)+FLXPOC(IXY,2)+FLXPOC(IXY,3)
         JBSI=JBSI+FLXPOS(IXY)
         JPO4SS=JPO4SS+FLXPO4T2S(IXY)
         JSISS=JSISS+FLXSIT2S(IXY)
         IF(IGDSEDOPT == 0) THEN
          TOTJPO4=TOTJPO4+JPO4_GDA(IXY)
          TOTJNH4=TOTJNH4+JNH4_GDA(IXY)
          TOTJNO3=TOTJNO3+JNO3_GDA(IXY)
          TOTJN2=TOTJN2+JN2_GDA(IXY)
          TOTJSI=TOTJSI+JSI_GDA(IXY)
          TOTJH2S=TOTJH2S+JH2S_GDA(IXY)
          TOTJCH4AQ=TOTJCH4AQ+JCH4AQ_GDA(IXY)
          TOTJCH4G=TOTJCH4G+JCH4G_GDA(IXY)
          TOTJSOD=TOTJSOD+SOD_GDA(IXY)
         ELSE
          TOTJPO4=TOTJPO4+JPO4_GDA(IXY)/FLOAT(IGDCNT)
          TOTJNH4=TOTJNH4+JNH4_GDA(IXY)/FLOAT(IGDCNT)
          TOTJNO3=TOTJNO3+JNO3_GDA(IXY)/FLOAT(IGDCNT)
          TOTJN2=TOTJN2+JN2_GDA(IXY)/FLOAT(IGDCNT)
          TOTJSI=TOTJSI+JSI_GDA(IXY)/FLOAT(IGDCNT)
          TOTJH2S=TOTJH2S+JH2S_GDA(IXY)/FLOAT(IGDCNT)
          TOTJCH4AQ=TOTJCH4AQ+JCH4AQ_GDA(IXY)/FLOAT(IGDCNT)
          TOTJCH4G=TOTJCH4G+JCH4G_GDA(IXY)/FLOAT(IGDCNT)
          TOTJSOD=TOTJSOD+SOD_GDA(IXY)/FLOAT(IGDCNT)
         ENDIF
        ENDIF
       END DO
       IF(MSR) WRITE(18)  TIME,TOTORGP,TOTORGN,TOTORGC,TOTBSI,TOTPO4,TOTNH4   &
                  ,TOTNO3,TOTSI,TOTH2S,TOTCH4,TOTSO4                  &
                  ,JPOP,JPON,JPOC,JBSI,JPO4SS,JSISS                   &
                  ,BURIALORGP,BURIALORGN,BURIALORGC                   &
                  ,BURIALBSI,BURIALPO4,BURIALNH4,BURIALNO3,BURIALSI   &
                  ,BURIALH2S,BURIALCH4,BURIALSO4,TOTJPO4,TOTJNH4      &
                  ,TOTJNO3,TOTJN2,TOTJSI,TOTJH2S,TOTJCH4AQ,TOTJCH4G   &
                  ,TOTSOD
      ENDIF

      IF(ISEDDISK == 1) THEN
       IF(IGDSEDOPT == 1) THEN
        IGDCNT=0

        CTEMP_GDA = 0. ;POP1R_GDA  = 0. ;POP2R_GDA = 0.
        POP3R_GDA = 0. ;POPR_GDA   = 0. ;PON1R_GDA = 0.
        PON2R_GDA = 0. ;PON3R_GDA  = 0. ;PONR_GDA  = 0.
        POC1R_GDA = 0. ;POC2R_GDA  = 0. ;POC3R_GDA = 0.
        POCR_GDA  = 0. ;PO4T2R_GDA = 0. ;HST2R_GDA = 0.
        SIT2R_GDA = 0. ;PSIAVR_GDA = 0.

        FLXPOP_GDA  = 0. ;FLXPOP1_GDA   = 0. ;FLXPOP2_GDA  = 0.
        FLXPOP3_GDA = 0. ;FLXPON_GDA    = 0. ;FLXPON1_GDA  = 0.
        FLXPON2_GDA = 0. ;FLXPON3_GDA   = 0. ;FLXPOC_GDA   = 0.
        FLXPOC1_GDA = 0. ;FLXPOC2_GDA   = 0. ;FLXPOC3_GDA  = 0.
        FLXPOS_GDA  = 0. ;FLXPO4T2S_GDA = 0. ;FLXSIT2S_GDA = 0.
        O20_GDA     = 0. ;CSOD_GDA      = 0. ;SOD_GDA      = 0.
        S_GDA       = 0. ;JP_GDA        = 0. ;JN_GDA       = 0.
        JC_GDA      = 0. ;JO2NH4_GDA    = 0. ;XJCO2AV_GDA  = 0.
        XJC1AV_GDA  = 0. ;JPO4_GDA      = 0. ;JNH4_GDA     = 0.
        JNO3_GDA    = 0. ;JH2S_GDA      = 0. ;JSI_GDA      = 0.
        JCH4AQ_GDA  = 0. ;JCH4G_GDA     = 0.

        H1_GDA    = 0. ;PO40_GDA  = 0. ;PO41_GDA  = 0.
        PO42_GDA  = 0. ;PO4T2_GDA = 0. ;NH40_GDA  = 0.
        NH41_GDA  = 0. ;NH42_GDA  = 0. ;NH4T2_GDA = 0.
        NO30_GDA  = 0. ;NO31_GDA  = 0. ;NO32_GDA  = 0.
        NO3T2_GDA = 0. ;HS1_GDA   = 0. ;HS2_GDA   = 0.
        HST2_GDA  = 0. ;SI0_GDA   = 0. ;SI1_GDA   = 0.
        SI2_GDA   = 0. ;SIT2_GDA  = 0.

        JN2_GDA   = 0. ;CH41_GDA = 0. ;CH42_GDA = 0.
        CH4T2_GDA = 0. ;SO41_GDA = 0. ;SO42_GDA = 0.
        SO4T2_GDA = 0.

       ENDIF
       ISEDDISK = 0
      ENDIF

      IF(IDISK >= 1) THEN
        REWIND(16)
       IF(SERIAL)WRITE(16)CTEMP,CPOP,CPON,CPOC,CPOS                  &
              ,PO4T2TM1S,NH4T2TM1S,NO3T2TM1S                  &
              ,HST2TM1S,SIT2TM1S,BNTHSTR1S                    &
              ,PO4T1TM1S,NH4T1TM1S,NO3T1TM1S                  &
              ,SIT1TM1S,HST1TM1S                              &
              ,CH4T1TM1S,CH4T2TM1S,CH41TM1S,SO4T2TM1S         &
              ,PO41TM1S,NH41TM1S,NO31TM1S,HS1TM1S,SI1TM1S     &
              ,O20TM1S,SODTM1S,DD0TM1S                        &
              ,BFORMAXS,ISWBNTHS
	      
# if defined (MULTIPROCESSOR)
       IF (PAR) THEN

        CALL PWRITE(16,MYID,NPROCS,CTEMP,1,MT,M,NXY,1,NMAP,1,"CTEMP")
        CALL PWRITE(16,MYID,NPROCS,CPOP,1,MT,M,NXY,1,NMAP,1,"CPOP")	     				
        CALL PWRITE(16,MYID,NPROCS,CPON,1,MT,M,NXY,1,NMAP,1,"CPON")
        CALL PWRITE(16,MYID,NPROCS,CPOC,1,MT,M,NXY,1,NMAP,1,"CPOC")			
        CALL PWRITE(16,MYID,NPROCS,CPOS,1,MT,M,NXY,1,NMAP,1,"CPOS")
        CALL PWRITE(16,MYID,NPROCS,PO4T2TM1S,1,MT,M,NXY,1,NMAP,1,"PO4T2TM1S")	     				
        CALL PWRITE(16,MYID,NPROCS,NH4T2TM1S,1,MT,M,NXY,1,NMAP,1,"NH4T2TM1S")
        CALL PWRITE(16,MYID,NPROCS,NO3T2TM1S,1,MT,M,NXY,1,NMAP,1,"NO3T2TM1S")			
        CALL PWRITE(16,MYID,NPROCS,HST2TM1S,1,MT,M,NXY,1,NMAP,1,"HST2TM1S")
        CALL PWRITE(16,MYID,NPROCS,SIT2TM1S,1,MT,M,NXY,1,NMAP,1,"SIT2TM1S")	     				
        CALL PWRITE(16,MYID,NPROCS,BNTHSTR1S,1,MT,M,NXY,1,NMAP,1,"BNTHSTR1S")
        CALL PWRITE(16,MYID,NPROCS,PO4T1TM1S,1,MT,M,NXY,1,NMAP,1,"PO4T1TM1S")			
        CALL PWRITE(16,MYID,NPROCS,NH4T1TM1S,1,MT,M,NXY,1,NMAP,1,"NH4T1TM1S")
        CALL PWRITE(16,MYID,NPROCS,NO3T1TM1S,1,MT,M,NXY,1,NMAP,1,"NO3T1TM1S")	     				
        CALL PWRITE(16,MYID,NPROCS,SIT1TM1S,1,MT,M,NXY,1,NMAP,1,"SIT1TM1S")
        CALL PWRITE(16,MYID,NPROCS,HST1TM1S,1,MT,M,NXY,1,NMAP,1,"HST1TM1S")			
        CALL PWRITE(16,MYID,NPROCS,CH4T1TM1S,1,MT,M,NXY,1,NMAP,1,"CH4T1TM1S")
        CALL PWRITE(16,MYID,NPROCS,CH4T2TM1S,1,MT,M,NXY,1,NMAP,1,"CH4T2TM1S")	     				
        CALL PWRITE(16,MYID,NPROCS,CH41TM1S,1,MT,M,NXY,1,NMAP,1,"CH41TM1S")
        CALL PWRITE(16,MYID,NPROCS,SO4T2TM1S,1,MT,M,NXY,1,NMAP,1,"SO4T2TM1S")			
        CALL PWRITE(16,MYID,NPROCS,PO41TM1S,1,MT,M,NXY,1,NMAP,1,"PO41TM1S")
        CALL PWRITE(16,MYID,NPROCS,NH41TM1S,1,MT,M,NXY,1,NMAP,1,"NH41TM1S")	     				
        CALL PWRITE(16,MYID,NPROCS,NO31TM1S,1,MT,M,NXY,1,NMAP,1,"NO31TM1S")
        CALL PWRITE(16,MYID,NPROCS,HS1TM1S,1,MT,M,NXY,1,NMAP,1,"HS1TM1S")			
        CALL PWRITE(16,MYID,NPROCS,SI1TM1S,1,MT,M,NXY,1,NMAP,1,"SI1TM1S")
        CALL PWRITE(16,MYID,NPROCS,O20TM1S,1,MT,M,NXY,1,NMAP,1,"O20TM1S")	     				
        CALL PWRITE(16,MYID,NPROCS,SODTM1S,1,MT,M,NXY,1,NMAP,1,"SODTM1S")
        CALL PWRITE(16,MYID,NPROCS,DD0TM1S,1,MT,M,NXY,1,NMAP,1,"DD0TM1S")			
        CALL PWRITE(16,MYID,NPROCS,BFORMAXS,1,MT,M,NXY,1,NMAP,1,"BFORMAXS")
        CALL PWRITE(16,MYID,NPROCS,ISWBNTHS,1,MT,M,NXY,1,NMAP,1,"ISWBNTHS")
       END IF !(PAR)
# endif

!       IF(MSR)THEN
!        REWIND(16)
!        WRITE(16)   CTEMP,CPOP,CPON,CPOC,CPOS                  &
!              ,PO4T2TM1S,NH4T2TM1S,NO3T2TM1S                  &
!              ,HST2TM1S,SIT2TM1S,BNTHSTR1S                    &
!              ,PO4T1TM1S,NH4T1TM1S,NO3T1TM1S                  &
!              ,SIT1TM1S,HST1TM1S                              &
!              ,CH4T1TM1S,CH4T2TM1S,CH41TM1S,SO4T2TM1S         &
!              ,PO41TM1S,NH41TM1S,NO31TM1S,HS1TM1S,SI1TM1S     &
!              ,O20TM1S,SODTM1S,DD0TM1S                        &
!              ,BFORMAXS,ISWBNTHS
!       END IF !(MSR)	
      
      ENDIF 

!      IF(MSR.and.ITIMESECS >= NXPRTSED) NXPRTSED = NXPRTSED + IPRNTSEDSECS
      IF(ITIMESECS >= NXPRTSED) NXPRTSED = NXPRTSED + IPRNTSEDSECS
    
      RETURN

  900 IF(MSR) WRITE(OUT,9005)
 9005 FORMAT(/' READ ERROR IN SEDIMENT INPUT DECK')
      CALL PSTOP('vss1596   ')
      
      END SUBROUTINE SED_READ
