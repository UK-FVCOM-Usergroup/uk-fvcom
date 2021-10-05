
   MODULE SIZES
!***** Parameter declarations
!***** ST. JOHNS RIVER COARSE GRID:6 LAYER MODEL
   INTEGER,PARAMETER :: NCP=32,                               &
!                        NBP=12720,                            &
                        NQFP=32686,                           &
                        NHQP=22086,                           &
!                        NSBP=2120,                            &
!                        NLP=6,                                &
!                        NSQFP=3681,                           &
                        NS1P=378,                             &
                        NS2P=378,                             &
                        NS3P=378,                             &
                        NBCP=492,                             &
!                        NMP=1,                                &
                        NDP=500,                              &
                        NSINC=10,                             &
                        NFLP=20,                              &
                        NOIP=10,                              &
                        NSSFP=10 
   INTEGER :: MGL,NGL,OBCGL,NOBTY			  
   END MODULE SIZES


   MODULE FILE_INFO
   USE SIZES
   IMPLICIT NONE
   SAVE
     
   INTEGER :: HYD, DIA, CBC, S1,  S2,  S3,                      &
              BFI, MET, BFO, KEI,                               &
              ATM, STL, AGR, SVI, SVO, KFL, ZOO, ZFO, ALO,      &
              CON, RSO, SNP, GEO, PLT, APL, TFL, OPL, SUS, SUD, &
              MAP, ICI, ICO, MRL, MBL

   CHARACTER(LEN=20) :: SSNAME(17)
   CHARACTER(LEN=24) :: CNAME(NCP)
   CHARACTER(LEN=18) :: CONFN ='wqm_con.npt'

!--------------------end of data declarations---------------------------------c

   CONTAINS

   SUBROUTINE INIT_FILE_INFO()
!
   CON = 10 
   HYD = 20
   BFI = 21
    S1 = 23
    S2 = 24 
    S3 = 34
   ATM = 25 
   STL = 26 
   AGR = 27
   SVI = 28 
   MET = 29 
   KEI = 30 
   SVO = 31
   ZOO = 38
   ZFO = 39
   ALO = 50
   DIA = 40 
   BFO = 41 
   KFL = 47
   MAP = 32 
   GEO = 33  
   ICI = 35 
   ICO = 36
   MRL = 37 
   SUS = 58
   SUD = 59
   RSO = 42 
   SNP = 43 
   PLT = 44 
   APL = 45 
   TFL = 46 
   OPL = 48
   MBL = 49 
   CBC = 60 

   SSNAME(1)  = 'Sediment Temperature'
   SSNAME(2)  = 'Sediment POP        '
   SSNAME(3)  = 'Sediment PON        '  
   SSNAME(4)  = 'Sediment POC        ' 
   SSNAME(5)  = 'Sediment PBS        ' 
   SSNAME(6)  = 'Sediment PO4        '
   SSNAME(7)  = 'Sediment NH4        '
   SSNAME(8)  = 'Sediment NO3        '
   SSNAME(9)  = 'Sediment HS         '
   SSNAME(10) = 'Sediment CH4        '
   SSNAME(11) = 'Sediment CH4        '
   SSNAME(12) = 'Sediment SO4        '
   SSNAME(13) = 'Sediment DSIL       '
   SSNAME(14) = 'Benthic Stress      '
   SSNAME(15) = 'Benthic Algae       '
   SSNAME(16) = 'Deposit Feeders     '
   SSNAME(17) = 'Suspension Feeders  '

   CNAME(1)  =  'Temperature'
   CNAME(2)  =  'Salinity'
   CNAME(3)  =  'Inorganic Solids'
   CNAME(4)  =  'Algal Group 1'
   CNAME(5)  =  'Algal Group 2' 
   CNAME(6)  =  'Algal Group 3'
   CNAME(7)  =  'Zooplankton Group 1'
   CNAME(8)  =  'Zooplankton Group 2'
   CNAME(9)  =  'Labile DOC'           
   CNAME(10) =  'Refractory DOC'      
   CNAME(11) =  'Labile POC'                 
   CNAME(12) =  'Refractory POC'      
   CNAME(13) =  'Ammonium'
   CNAME(14) =  'Nitrate-nitrite' 
   CNAME(15) =  'Urea'             
   CNAME(16) =  'Labile DON'                 
   CNAME(17) =  'Refractory DON'      
   CNAME(18) =  'Labile PON'                 
   CNAME(19) =  'Refractory PON'      
   CNAME(20) =  'Total phosphate'
   CNAME(21) =  'Labile DOP'                 
   CNAME(22) =  'Refractory DOP'      
   CNAME(23) =  'Labile POP'                 
   CNAME(24) =  'Refractory POP'      
   CNAME(25) =  'Particulate Inorganic P'
   CNAME(26) =  'COD'
   CNAME(27) =  'Dissolved oxygen'
   CNAME(28) =  'Particulate silica  '
   CNAME(29) =  'Dissolved silica'
   CNAME(30) =  'Internal P Group 1'
   CNAME(31) =  'Internal P Group 2'
   CNAME(32) =  'Internal P Group 3'

   RETURN
   END SUBROUTINE INIT_FILE_INFO

   END MODULE FILE_INFO

   MODULE WQM    
   USE SIZES
   USE MOD_PREC
   USE ALL_VARS
   IMPLICIT NONE
   SAVE
!********************************************************************************
!**                                                                            **
!**                        COMMON file for CE-QUAL-IC                          **
!**                                                                            **
!**                                Version 1.1                                 **
!**                               July 28, 1992                                **
!**                                                                            **
!**                        Water Quality Modeling Group                        **
!**                        U.S. Army Corps of Engineers                        **
!**                        Waterways Experiment Station                        **
!**                        Vicksburg, Mississippi 39180                        **
!**                                                                            **
!********************************************************************************

!********************************************************************************
!**                                                                            **
!**                           PARAMETER definitions                            **
!**                                                                            **
!**    NBP   - Number of boxes                                                 **
!**    NQFP  - Number of horizontal and vertical flow faces                    **
!**    NHQP  - Number of horizontal flow faces                                 **
!**    NSBP  - Number of boxes in the surface layer                            **
!**    NLP   - Maximum number of layers                                        **
!**    NS1P  - Number of source one inputs                                     **
!**    NS2P  - Number of source two inputs                                     **
!**    NS3P  - Number of source three inputs                                   **
!**    NBCP  - Number of boundary concentration inputs                         **
!**    NMP   - Number of modifications to initial box concentrations           **
!**    NDP   - Maximum number of days for any output                           **
!**    NFLP  - Number of files for each type of time-varying data              **
!**    NCP   - Number of constituent state variables                           **
!**    NSINC - Number of SAV depth increments                                  **
!**    NSSFP - Number of suspension-feeding species                            **
!**                                                                            **
!********************************************************************************


!********************************************************************************
!**                        Water Quality Model Setup                           **
!********************************************************************************


!***** Data type declarations

     INTEGER :: B, BB, SAVB
     INTEGER :: HYDPTR, METPTR, CBCPTR, S1PTR,  S2PTR,  S3PTR,  BFIPTR,     &
                ATMPTR, SAVPTR
     REAL :: KHR1,   KHR2,   KHR3,   KHN1,   KHN2,   KHN3,   KHP1,       &
             KHP2,   KHP3,   KHONT,  KHOCOD, KHNNT,  KHODOC,             &
             KHTIS,  KHNDN,  KHS1,   KHS2,   KHS3,   KHST1,      &
             KHST2,  KHST3,  KHNFIX
      
     REAL :: KHNAVG, KHPAVG
     REAL :: KSO,    KSNH4,  KSNO3,  KSPO4,  KSSA,   KSDOC,  KHSO,       &
                 MTCNO3
     REAL :: KTNT1,  KTNT2,  KTCOD,  KTMNL,  KTHDR,  KTSUA,  KHCOAG

     REAL :: NXCBC,  JDAY,   JDAYMBL

     REAL :: KT

     REAL :: NXMET,  NXS1,   NXS2,   NXS3,   NXBFI,  NXATM,  NXSAV
     REAL :: I0,     IT,     ITNX,   ISMIN

     REAL :: KADPO4, KADSA  
     
     REAL :: KHCSZ,  KHCLZ,  MZEROSZ,MZEROLZ

!  variables needed by autostep and main   VJP 10/11/04
     INTEGER,SAVE :: NXHYD = 0
     REAL(4),SAVE :: COURMX, COURQS, COURVS
     REAL(4),SAVE :: DIFFMX, DIFFAS, DIFFDS, FNDLT

     REAL(4),ALLOCATABLE :: V1SINGLE(:,:)

     REAL(8),SAVE :: DLT8, MAXDLTDP, MXDLTDP, NXHYDDP, ELTMS1

     REAL(8) :: ELTMS

     LOGICAL :: BOUNDARY_CONC,  SOURCE_ONE,       SOURCE_TWO,               &
                SOURCE_THR,     SOLIDS_CALC,      PIP_CALC,                 &
                BENTHIC_FLUXES, SEDIMENT_CALC,    LIGHT_EXTINCTION,         &
                ATMOS_LOADS,    TRANSPORT_FLUXES,                           &
                AVERAGE_PLOTS,  QUALITY_DIAG,     KINETIC_FLUXES,           &
                SEDIMENT_DIAG,  DIAGNOSTICS,      CONSERVE_MASS,            &
                SETTLING,       STEP_BOUNDARY,    MASS_BALANCE      
     LOGICAL :: SAV_LOADS,      SAV_CALC,         SAV_PLOTS
     LOGICAL :: FLOW, XY_DIFFUSION, Z_DIFFUSION, BINARY_HYDRO, ASCII_HYDRO,  &
                DEPTH_AVG_HYDRO, SIGMA_HYDRO                     !JLM ST JOHNS
     CHARACTER(LEN=8) :: SPVARKE, PRINTKE
     CHARACTER(LEN=72) :: KEIFN, SVOFN, BFOFN, ZOOFN, ZFOFN, ALOFN
     CHARACTER(LEN=8) :: MINPROD

!***** Dimension declarations

! vjp 10/04/04 moved declaration from module WQM_INIT
     REAL,ALLOCATABLE :: DTM(:,:,:)

     REAL,ALLOCATABLE,DIMENSION(:,:,:) :: C1, C2, C2F, CSTAR, DTC, AC1
     
     REAL,DIMENSION(NCP) :: CMASS

     REAL :: S1L(NS1P,NCP),    S2L(NS2P,NCP),    S3L(NS3P,NCP)
     INTEGER :: S1LB(NS1P,NCP),   S2LB(NS2P,NCP),   S3LB(NS3P,NCP)

     REAL :: CB(NBCP,NCP),CBNX(NBCP,NCP), CBOLD(NBCP,NCP)

     REAL,ALLOCATABLE,DIMENSION(:,:) :: CPOP, FLXPOP, CPON, FLXPON,    &
                                        CPOC, FLXPOC
     REAL,ALLOCATABLE,DIMENSION(:) :: CPOS,       CPIP,       CNO3,    &
               CNH4,       FLXPOS,     CTEMP,          &
               CDTEMP,     BSVOL,      HSED,           &
               CCH4,       CSO4,       CHS,            & !CFC
               CSI                                               !CFC

     REAL,ALLOCATABLE,DIMENSION(:) :: ATMFLXNB,   ATMFLXPB,   ATMFLXCB,       &
              BENFLXPNB,  BENFLXDNB,  BENFLXPPB,      &
               BENFLXDPB,  BENFLXPCB,  DLSEDKNB,       &
               DLSEDKCB,                                           &
               BURIALFLXNB,BURIALFLXPB,BURIALFLXCB

     REAL :: FLUXT(0:NQFP,NCP), AFLUX(NQFP,13)
     REAL,ALLOCATABLE :: FLUXS(:,:,:)

     REAL,DIMENSION(NQFP) :: FLXTTEM,    FLXTSAL,    FLXTSSI,        &
               FLXT1,      FLXT2,      FLXT3,          &
               FLXTLDOC,   FLXTLPOC,   FLXTRPOC,       &
               FLXTNH4,    FLXTNO3,    FLXTLDON,       &
               FLXTLPON,   FLXTRPON,   FLXTPO4,        &
               FLXTLDOP,   FLXTLPOP,   FLXTRPOP,       &
               FLXTCOD,    FLXTDO,     FLXTSU,         &
               FLXTSA,     FLXTSZ,     FLXTLZ,         &
               FLXTRDOC,   FLXTRDON,   FLXTRDOP,       &
               FLXTUREA,   FLXTPIP,    FLXTPIB1,       &
               FLXTPIB2,   FLXTPIB3

     REAL,ALLOCATABLE,DIMENSION(:,:) ::            &
               FLXSPIB1,    FLXSPIB2,        &
               FLXSPIB3

     REAL,ALLOCATABLE,DIMENSION(:) :: ABENDOC,    ABENNH4,    ABENNO3,        &
               ABENPO4,    ABENCOD,    ABENDO,         &
               ABENSA,     ASSFWS,     APCFWS,         &
               APNFWS,     APPFWS,     APSFWS,         &
               ACPIP,      ACPOS,                            &
               ADFEED,                          &
               ABENCH4G,   ABENCH4A

     REAL,ALLOCATABLE,DIMENSION(:,:) :: ACPOC, ACPON, ACPOP, ASFEED
     
     REAL,ALLOCATABLE,DIMENSION(:) :: AJNSF,      AJPSF,      ASODSF          &
              ,ASASF,      ASUSF,      ASFGCIN         &
              ,ASFCFEC,    ASFCPSF,    AFLXCSF         &
              ,AFLXNSF,    AFLXPSF,    ARPOCSF         &
              ,ARPONSF,    ARPOPSF,    ASSISF          &
              ,ASSISASF,   ASSISUSF,   ASSIPSF         

     REAL,ALLOCATABLE,DIMENSION(:,:) :: ACCHL1,  ACCHL2,  ACCHL3

     INTEGER :: S1LN(NCP),S2LN(NCP),S3LN(NCP),AC(NCP),    &
                NCB(NCP)        !J    ,IBC(NCP,NBCP)
     
     REAL,DIMENSION(0:NQFP) :: Q, A, DIFF

     REAL,ALLOCATABLE :: BL(:,:,:)
     REAL,ALLOCATABLE,DIMENSION(:,:) :: V1S, HMV, ZD
     REAL,ALLOCATABLE,DIMENSION(:) :: HMBV

     REAL(8),ALLOCATABLE,DIMENSION(:,:) :: V1,    V2

     REAL,ALLOCATABLE,DIMENSION(:,:) :: PN1,     PN2,     PN3,     BM1,  &
               BM2,     BM3,     MNLLDOC, FTCOD,     &
               NPP,     P1,      P2,      P3,        &
               NT,      PR1,     PR2,     PR3,       &
               DENIT,   RATOX,   GPP,     MNLRDOC, P1NNF
     REAL,ALLOCATABLE,DIMENSION(:) :: ASRAT

     REAL,ALLOCATABLE,DIMENSION(:) :: CFIX, SNFIX
     
     REAL,ALLOCATABLE,DIMENSION(:,:) :: AFI1,    ANL1,    APL1,    AFI2,      &
               ANL2,    APL2,    ASL2,    AFI3,      &
               ANL3,    APL3,    ANPP,    ARESP,     &
               AKE,     ASL1,    ASL3,    AGPP

     REAL,ALLOCATABLE,DIMENSION(:) :: AASRAT, ACFIX

     REAL,ALLOCATABLE,DIMENSION(:) :: AFIB,   ANLB,   APLB,   ANPPB,    &
               ABBM,   ABLITE, ABMB,   APB,      &
               APRB,   ABADOC, ABAPOC, ABANH4,   &
               ABANO3, ABAPON, ABAPO4, ABAPOP,   &
               ABADO

     REAL,ALLOCATABLE,DIMENSION(:,:) :: ALEAF,      AEP,                  &
               AROOT,      AFIEP,                &
               ANPPSAV,    ANPPEP,               &
               AWATATN,              &
               AEPATN,     ATUBER,               &
               ASTEM,      AFISH,                &
               APLEAF
     REAL,ALLOCATABLE,DIMENSION(:) :: ANLSAV,           APLSAV,       &
               AFHS,             AFNSED,                     &
               AFPSED,           ANLEPI,                     &
               APLEPI

     REAL,ALLOCATABLE,DIMENSION(:,:) :: APEP, ABMEP, APREP

     REAL,ALLOCATABLE,DIMENSION(:) :: APOCSAV,   ADOCEPI,     &
               APOCEPI,   ASEDCSAV,                          &
               ANH4SAVW,  ANO3SAVW,  ADONSAVW,         &
               APONSAVW,  ANH4EPI,                           &
               ANO3EPI,   ADONEPI,   APONEPI,          &
               ASEDNSAV,  ASEDNH4SAV,                        &
               APO4SAVW,  ADOPSAVW,  APOPSAVW,         &
               APO4EPI,   ADOPEPI,                           &
               APOPEPI,   ASEDPSAV,                          &
               ASEDPO4SAV,ADOSAV,    ADOEPI,           &
               ASEDDOSAV,                              &
               ABMLEAF, ASLSH, ABMTUBER, ADOCSAV

     REAL,ALLOCATABLE,DIMENSION(:,:) :: T, SSI, B1, B2, B3, LDOC, LPOC,    &
               RPOC, NH4, NO3, LDON, LPON, RPON, PO4, LDOP, LPOP, RPOP,    &
	       COD, DO, SU, SA, SALT, SZ, LZ, RDOC, RDON, RDOP, UREA,      &
               PIP, PIB1, PIB2, PIB3

     REAL,ALLOCATABLE,DIMENSION(:,:) :: WSS,   WSL,   WSR,   WS1,     &
               WS2,   WS3,   WSU,   WSSHI

     REAL,ALLOCATABLE,DIMENSION(:,:) :: PM1,     PM2,     PM3,     BMR1,   &
               BMR2,    BMR3,    BPR1,    BPR2,      &
               BPR3,    CCHL1,   CCHL2,   CCHL3,     &
               Q1,      Q2,      Q3

!     REAL,ALLOCATABLE,DIMENSION(:,:) :: DTT, DTSSI, DTB1, DTB2, DTB3,      &
     REAL,ALLOCATABLE,DIMENSION(:,:) ::   &
!               DTLDOC, DTLPOC, DTRPOC, DTNH4, DTNO3, DTLDON, DTLPON,       &
!               DTRPON, DTPO4, DTLDOP, DTLPOP, DTRPOP, DTCOD, DTDO, DTSU,   &
!               DTSA, DTSZ, DTLZ, DTRDOC, DTRDON, DTRDOP, DTUREA, DTPIP,    &
                DTUREA
!               DTPIB1, DTPIB2, DTPIB3

     REAL,ALLOCATABLE,DIMENSION(:,:) :: KE, FI1, FI2, FI3, NL1, NL2, NL3,  &
               PL1, PL2, PL3, SL2, RESP, KESS, SL1, SL3, IAVG, IATBOT,     &
	       KEISS, KEVSS, KEDOC

     REAL,ALLOCATABLE,DIMENSION(:,:) :: FTMNL,   FTHDR
     REAL,ALLOCATABLE,DIMENSION(:,:) :: RESPC,   DLALGC

     REAL,ALLOCATABLE,DIMENSION(:,:) :: IWCMNB,  IWCMPB,  IWCMCB,  IWCMSB,    &
               WCMNB,   WCMPB,   WCMCB,   WCMSB,          &
               DLWCMNB, DLWCMPB, DLWCKMNB,  &
               DLWCKMCB,S1FLXNB, S1FLXPB, S1FLXCB,        &
               S2FLXNB, S2FLXPB, S2FLXCB,                 &
               S3FLXNB, S3FLXPB, S3FLXCB

     REAL,DIMENSION(NCP) :: DLWCMCB

     REAL,ALLOCATABLE,DIMENSION(:) :: BENDOC, BENNH4, BENNO3, BENDON,   &
               BENDOP, BENPO4, BENCOD, BENDO,    &
               BENSA,  BENDEN, BENCH4G,BENCH4A

     REAL,ALLOCATABLE,DIMENSION(:) :: BENDOCB,BENNH4B,BENNO3B,BENPO4B,  &
               BENCODB,BENDOB, BENSAB

     INTEGER,ALLOCATABLE,DIMENSION(:) :: SBN,    BBN,    HMSBV,  SFA

     REAL,ALLOCATABLE,DIMENSION(:) :: WSSNET, WSLNET, WSRNET, WS1NET,   &
               WS2NET, WS3NET, VSED,   VPMIX,    &
               VDMIX,  DIAGP,  DIAGN,  DIAGC,    &  !CFC
               DIAGS,  MTVEL,  WSUNET

     REAL,ALLOCATABLE,DIMENSION(:) :: WSSBNET, WSLBNET, WSRBNET,       & 
               WS1BNET, WS2BNET, WS3BNET, WSUBNET 

     REAL,ALLOCATABLE,DIMENSION(:) :: PPFWS,  PNFWS,  PCFWS,  PSFWS,  SSFWS

     CHARACTER(LEN=72),DIMENSION(NFLP) :: METFN, S1FN, S2FN, S3FN, HYDFN,    &
               CBCFN, ATMFN, SVIFN, BFIFN

     REAL,DIMENSION(3) :: FRPPH1,    FRPPH2,    FRPPH3,      &
               FRNPH1,    FRNPH2,    FRNPH3,     &
               FRCPH1,    FRCPH2,    FRCPH3,     &
               FRCPHB,    FRNPHB,    FRPPHB 
     REAL,ALLOCATABLE,DIMENSION(:,:) :: FRPOP,   FRPON,   FRPOC

     REAL,ALLOCATABLE,DIMENSION(:,:) :: KLDC,    KRDC,     KDCALG, KLPC,      &
               KLCALG,  KRPC,     KSUA,   KCOD,      &
               KLDN,    KRDN,     KDNALG, KLDP,      &
               KRDP,    KDPALG,   KLPN,   KLNALG,    &
               KLPP,    KLPALG,   KRPN,    KRPP,     &
               NTM,     KRCOAG

     REAL,ALLOCATABLE,DIMENSION(:,:) :: ALGDOC,  ALGPOC,   HDRLPOC, HDRRPOC 

     REAL,ALLOCATABLE,DIMENSION(:,:) :: ALGNH4,  ALGNO3,   ALGDON,  ALGPON,   &
               DENNO3,  MNLLDON,  HDRLPON, HDRRPON,  &
               MNLRDON, NFIX

     REAL,ALLOCATABLE,DIMENSION(:,:) :: ALGPO4,  ALGDOP,   ALGPOP,  MNLLDOP,  &
               HDRLPOP, HDRRPOP,  MNLRDOP

     REAL,ALLOCATABLE,DIMENSION(:,:) :: PSD,     SAP,      ALGUP,   ALGRES

     REAL,ALLOCATABLE,DIMENSION(:,:) :: DORALG, DOPR, DCOD, DDOC, NITRIF

     REAL,ALLOCATABLE,DIMENSION(:,:) :: A_T, AP1, ABM1, APR1, AP2, ABM2, APR2, &
          AP3, ABM3, APR3,                                                     &
          AALGDOC, AALGPOC, ADENIT, AMNLDOC, AHDRPOC,                          &
          AALGNH4, AALGNO3, AALGDON, AALGPON,                                  &
          ANT, ADENNO3, AMNLDON, AHDRPON, ANFIX,                               &
          AALGPO4, AALGDOP, AALGPOP, AMNLDOP, AHDRPOP,                         &
          APSD, ASAP, AALGUP, AALGRES,                                         &
          ADO, ADORALG, ADOPR, ADCOD, ADDOC, ANITRIF

     REAL,ALLOCATABLE,DIMENSION(:,:) :: B1ASZ,   B2ASZ,    B3ASZ,   LPOCASZ,  &
               RPOCASZ, PRASZ,    B1ALZ,   B2ALZ,    &
               B3ALZ,   SZALZ,    LPOCALZ, RPOCALZ,  &
               PRALZ,   CLSZ,     CLLZ,    RSZ,      &
               RLZ,     RMAXSZ,   RMAXLZ,  BMSZ,     &
               BMLZ,    BMRSZ,    BMRLZ,   MSZ,      &
               MLZ,     PRSZLZ,   GSZ,     GLZ,      &
               ESZ,     ELZ,      RFSZ,    RFLZ,     &
               PRSZ,    PRLZ,     LDOCASZ, BPRSZ,    &
               BPRLZ,   RDOCASZ

     REAL,ALLOCATABLE,DIMENSION(:,:) :: LDOCSZ,  LPOCSZ,   RPOCSZ,  LDOCLZ,   &
               LPOCLZ,  RPOCLZ,   NH4SZ,   LDONSZ,   &
               LPONSZ,  RPONSZ,   NH4LZ,   LDONLZ,   &
               LPONLZ,  RPONLZ,   PO4SZ,   LDOPSZ,   &
               LPOPSZ,  RPOPSZ,   PO4LZ,   LDOPLZ,   &
               LPOPLZ,  RPOPLZ,   RDOCSZ,  RDONSZ,   &
               RDOPSZ,  RDOCLZ,   RDONLZ,  RDOPLZ,   &
               PIB1SZ,  PIB2SZ,   PIB3SZ,  PIB1LZ,   &
               PIB2LZ,  PIB3LZ


     REAL,ALLOCATABLE,DIMENSION(:,:) :: B1SZ,    B2SZ,     B3SZ,    B1LZ,     &
               B2LZ,    B3LZ,     DOSZ,    DOLZ,     &
               SASZ,    SUSZ,     SALZ,    SULZ 

     REAL,ALLOCATABLE,DIMENSION(:,:) :: ACLSZ,   ACLLZ,    ARSZ,    ARLZ,     &
               ABMSZ,   ABMLZ,    AMSZ,    AMLZ,     &
               APRSZLZ, AGSZ,     AGLZ,    ADOCSZ,   &
               APOCSZ,  ADOCLZ,   APOCLZ,                 &
               ANH4SZ,  ADONSZ,   APONSZ,                 &
               ANH4LZ,  ADONLZ,   APONLZ,                 &
               APO4SZ,  ADOPSZ,   APOPSZ,                 &
               APO4LZ,  ADOPLZ,   APOPLZ,                 &
               APRSZ,   APRLZ,    APISZ,  APILZ

     REAL,ALLOCATABLE,DIMENSION(:,:) :: AB1SZ,   AB2SZ,    AB3SZ,  AB1LZ,     &
               AB2LZ,   AB3LZ,    ADOSZ,  ADOLZ,     &
               ASASZ,   ASUSZ,    ASALZ,  ASULZ

     REAL,DIMENSION(-50:400) :: FTLZ,  FTSZ,  FTBMSZ,          &
               FTBMLZ,FTPRSZ,FTPRLZ,          &
               FT1,   FT2,   FT3,             &
               FTBM1, FTBM2, FTBM3,           &
               FTPR
     REAL,DIMENSION(366) :: TVPRSZ,    TVPRLZ, TVPR,   B2GR

     REAL,ALLOCATABLE :: QLIT(:,:),    CONLIT(:,:,:) 
! sigma grid and evaporation
!J    REAL :: HMCV(NSBP),   DLZF(NLP)
  
!J    REAL :: RNMEVP(0:NSBP)                                            ! JLM FOR LOWER ST. JOHNS
     
!vic 
!vjp  added these declarations to split main into init and timestep loop
!vic 

     integer,save ::  length, ijday 
     integer,save ::  napl, ndlt, nsnp, nplt, ntfl
     integer,save ::  nkfl, nopl, noint, nmbl
     integer,save ::  kfldp, mbldp, inflow
     integer,save ::  ndia, nrso, nhydf, ntvdf, keiptr, numbox
     integer,save ::  jcb, it1, it2, it3, jt1, ivtmp, jvtmp
     integer,save ::  jcs1max, jcs2max, jcs3max, jdayvb

     real,save ::  th, tmstrt, xydf, ctempi
     real,save ::  bensti, bbmi, cposi, ch4t2i, ch41ti, dfeedi, dummy
     real,save ::  eltmsplt, eltmstfl, eltmskfl, epi, hst2i, po4t2i
     real,save ::  rti, sfatot, shi, sit2i, so4t2i, sum 
     real,save ::  ratio, reltms, tods, dssr, savent, sedent, sfent
     real,save ::  sfatmp, alfas, conc1, conc2, conc3, cadv, omth 
     real,save ::  bndn, bndp, bndc 
     real,save ::  dos, dltav, eltmjd, hourvb, hmtv2
     real,save ::  wcmn, wcmp, wcmc, wcms, wqmdlv, wqmtv2
     real,save ::  dlwcmn, dlwcmp, dlwcmc, dtmp1
     real,save ::  dlwcssmn, dlwcssmp
     real,save ::  avgint, dlwcssmc, dtmp2, dftmp  
     real,save ::  dlsedmn, dlsedmp, dlsedmc, dlsedssmn  
     real,save ::  dlsedssmp, dlsedssmc, errwcn, errwcp
     real,save ::  errwcc, errsedn, errsedp, errsedc, eltmmbl

!***** Equivalence declarations

!     EQUIVALENCE (C1(0,1),    CSTAR(0,1))
!     EQUIVALENCE (DTT(1),     DTC(1,1)),                                &
!                 (DTSSI(1),   DTC(1,3)),    (DTB1(1),     DTC(1,4)),    &
!                 (DTB2(1),    DTC(1,5)),    (DTB3(1),     DTC(1,6)),    &
!                 (DTSZ(1),    DTC(1,7)),    (DTLZ(1),     DTC(1,8)),    &
!                 (DTLDOC(1),  DTC(1,9)),    (DTRDOC(1),   DTC(1,10)),   &
!                 (DTLPOC(1),  DTC(1,11)),   (DTRPOC(1),   DTC(1,12)),   &
!                 (DTNH4(1),   DTC(1,13)),   (DTNO3(1),    DTC(1,14)),   &
!                 (DTUREA(1),  DTC(1,15)),   (DTLDON(1),   DTC(1,16)),   &
!                 (DTRDON(1),  DTC(1,17)),   (DTLPON(1),   DTC(1,18)),   &
!                 (DTRPON(1),  DTC(1,19)),   (DTPO4(1),    DTC(1,20)),   &
!                 (DTLDOP(1),  DTC(1,21)),   (DTRDOP(1),   DTC(1,22)),   &
!                 (DTLPOP(1),  DTC(1,23)),   (DTRPOP(1),   DTC(1,24)),   &
!                 (DTPIP(1),   DTC(1,25)),   (DTCOD(1),    DTC(1,26)),   &
!                 (DTDO(1),    DTC(1,27)),   (DTSU(1),     DTC(1,28)),   &
!                 (DTSA(1),    DTC(1,29)),   (DTPIB1(1),   DTC(1,30)),   &
!                 (DTPIB2(1),  DTC(1,31)),   (DTPIB3(1),   DTC(1,32))

! vjp 10/04/2004 moved this line from module WQM_INIT
!     EQUIVALENCE (DTC(0,1), DTM(0,1))

!     EQUIVALENCE (FLXTTEM(1), FLUXT(1,1)),  (FLXTSAL(1),  FLUXT(1,2)),  &
!                 (FLXTSSI(1), FLUXT(1,3)),  (FLXT1(1),    FLUXT(1,4)),  &
!                 (FLXT2(1),   FLUXT(1,5)),  (FLXT3(1),    FLUXT(1,6)),  &
!                 (FLXTSZ(1),  FLUXT(1,7)),  (FLXTLZ(1),   FLUXT(1,8)),  &
!                 (FLXTLDOC(1),FLUXT(1,9)),  (FLXTRDOC(1), FLUXT(1,10)), &
!                 (FLXTLPOC(1),FLUXT(1,11)), (FLXTRPOC(1), FLUXT(1,12)), &
!                 (FLXTNH4(1), FLUXT(1,13)), (FLXTNO3(1),  FLUXT(1,14)), &
!                 (FLXTUREA(1),FLUXT(1,15)), (FLXTLDON(1), FLUXT(1,16)), &
!                 (FLXTRDON(1),FLUXT(1,17)), (FLXTLPON(1), FLUXT(1,18)), &
!                 (FLXTRPON(1),FLUXT(1,19)), (FLXTPO4(1),  FLUXT(1,20)), &
!                 (FLXTLDOP(1),FLUXT(1,21)), (FLXTRDOP(1), FLUXT(1,22)), &
!                 (FLXTLPOP(1),FLUXT(1,23)), (FLXTRPOP(1), FLUXT(1,24)), &
!                 (FLXTPIP(1), FLUXT(1,25)), (FLXTCOD(1),  FLUXT(1,26)), &
!                 (FLXTDO(1),  FLUXT(1,27)), (FLXTSU(1),   FLUXT(1,28)), &
!                 (FLXTSA(1),  FLUXT(1,29)), (FLXTPIB1(1), FLUXT(1,30)), &
!                 (FLXTPIB2(1),FLUXT(1,31)), (FLXTPIB3(1), FLUXT(1,32))
!     EQUIVALENCE (FLXSSSI(1), FLUXS(1,1)),  (FLXS1(1),    FLUXS(1,2)),  &
!                 (FLXS2(1),   FLUXS(1,3)),  (FLXS3(1),    FLUXS(1,4)),  &
!                 (FLXSPOC(1), FLUXS(1,5)),  (FLXSPON(1),  FLUXS(1,6)),  &
!                 (FLXSPO4(1), FLUXS(1,7)),  (FLXSPOP(1),  FLUXS(1,8)),  &
!                 (FLXSPIP(1), FLUXS(1,9)),  (FLXSSI(1),   FLUXS(1,10)), &
!                 (FLXSPIB1(1),FLUXS(1,11)),                             &
!                 (FLXSPIB2(1),FLUXS(1,12)), (FLXSPIB3(1), FLUXS(1,13))
     
!     EQUIVALENCE (AFLXPOC(1), AFLUX(1,1)),  (AFLXDOC(1),  AFLUX(1,2)),  &
!                 (AFLXTC(1),  AFLUX(1,3)),  (AFLXDON(1),  AFLUX(1,4)),  &
!                 (AFLXDIN(1), AFLUX(1,5)),  (AFLXPON(1),  AFLUX(1,6)),  &
!                 (AFLXTN(1),  AFLUX(1,7)),  (AFLXDOP(1),  AFLUX(1,8)),  &
!                 (AFLXDIP(1), AFLUX(1,9)),  (AFLXPOP(1),  AFLUX(1,10)), &
!                 (AFLXPIP(1), AFLUX(1,11)), (AFLXTP(1),   AFLUX(1,12)), &
!                 (AFLXSSI(1), AFLUX(1,13))
!     EQUIVALENCE (BENDOC(1),  BFLUX(1,1)),  (BENNH4(1),   BFLUX(1,2)),  &
!                 (BENNO3(1),  BFLUX(1,3)),  (BENDON(1),   BFLUX(1,4)),  &
!                 (BENPO4(1),  BFLUX(1,5)),  (BENDOP(1),   BFLUX(1,6)),  &
!                 (BENCOD(1),  BFLUX(1,7)),  (BENDO(1),    BFLUX(1,8)),  &
!                 (BENSA(1),   BFLUX(1,9)),  (BENCH4A(1),  BFLUX(1,10)), &
!                 (BENCH4G(1), BFLUX(1,11))
!     EQUIVALENCE (BENDOCB(1), BFLUXB(1,1)), (BENNH4B(1),  BFLUXB(1,2)), &
!                 (BENNO3B(1), BFLUXB(1,3)), (BENPO4B(1),  BFLUXB(1,4)), &
!                 (BENCODB(1), BFLUXB(1,5)), (BENDOB(1),   BFLUXB(1,6)), &
!                 (BENSAB(1),  BFLUXB(1,7))

!***** Common declarations

     REAL :: TMEND,OLDNXCBC,TE,WMS
     REAL :: AOCR,AONT,FDOP,AREAR,BREAR,CREAR
     INTEGER  :: NAC,NB,NQF,NL,NSB,NBB,NHQF,NSQF
     REAL :: TRCOD
     REAL :: ASC1,ASC2,ASC3,FSAP,TRSUA
     REAL :: FR,SCTOX
     REAL :: PRSP1,PRSP2,PRSP3,STF1,STF2,STF3,VMAX1,VMAX2,VMAX3
     REAL :: TMNT,ANC1,ANC2,ANC3,FNI1,FNI2,FNI3,FNLD1,FNLD2,FNLD3,   &
                 FNRD1,FNRD2,FNRD3,FNLP1,FNLP2,FNLP3,FNRP1,FNRP2,FNRP3,  &
		 FNIP,FNLDP,FNRDP,FNLPP,FNRPP,FNUP
     REAL :: ALPHMIN1,ALPHMIN2,ALPHMIN3,ALPHRAT1,ALPHRAT2,ALPHRAT3,  &
                 CHLCMN1,CHLCMN2,CHLCMN3,ACHLC1,ACHLC2,ACHLC3,BCHLC1,    &
		 BCHLC2,BCHLC3,CCHLC1,CCHLC2,CCHLC3
     REAL :: ANDC
     REAL :: Q01,Q02,Q03,FPI1,FPI2,FPI3,FPLD1,FPLD2,FPLD3,FPRD1,     &
                 FPRD2,FPRD3,FPLP1,FPLP2,FPLP3,FPRP1,FPRP2,FPRP3,FPIP,   &
		 FPLDP,FPRDP,FPLPP,FPRPP	
     REAL :: FCLD1,FCLD2,FCLD3,FCRD1,FCRD2,FCRD3,FCLP1,FCLP2,FCLP3,  &
                 FCRP1,FCRP2,FCRP3,FCLDP,FCRDP,FCLPP,FCRPP,TRMNL,TRHDR,  &
		 AANOX	
     REAL :: FNFIX
     REAL :: FD,TTSS,SREFLECT,PRECIP,ATMNH4,ATMNO3,ATMLDON,ATMRDON,  &
                 ATMPO4,ATMLDOP,ATMRDOP 
     REAL :: TRSO,TRSNH4,TRSNO3,TRSPO4,TRSSA,TRSDOC,SEDNO3	
     REAL :: REDS1C,REDS1N,REDS1P,REDS2C,REDS2N,REDS2P,REDCBC,       &
                 REDCBN,REDCBP,REDS3C,REDS3N,REDS3P	 	 	 
     REAL :: PS1,PS2,PS3
     REAL :: CTSZ,UB1SZ,UB2SZ,UB3SZ,ULDSZ,URDSZ,ULPSZ,URPSZ,         &
                 CTLZ,UB1LZ,UB2LZ,UB3LZ,ULLZ,URLZ,USZLZ,TRSZ,TRLZ,       &
		 DOCRITSZ,DOCRITLZ,ANCSZ,ANCLZ,APCSZ,APCLZ,AOCRSZ,       &
		 AOCRLZ,FRSASZ,FRSALZ,UDSZ,Z1TP,Z2TP
     REAL :: FLDOCSZ,FRDOCSZ,FLPOCSZ,FRPOCSZ,FNH4SZ,FUREASZ,FLDONSZ, &
                 FRDONSZ,FLPONSZ,FRPONSZ,FPO4SZ,FLDOPSZ,FRDOPSZ,FLPOPSZ, &
		 FRPOPSZ,FLDOCLZ,FRDOCLZ,FLPOCLZ,FRPOCLZ,FNH4LZ,FUREALZ, &
		 FLDONLZ,FRDONLZ,FLPONLZ,FRPONLZ,FPO4LZ,FLDOPLZ,FRDOPLZ, &
		 FLPOPLZ,FRPOPLZ
     REAL :: RESTART_IN
     REAL :: DCH4T2,DHST2
     REAL :: DLT,AHMDLT,FILGTH,ZDFMUL,ZDFBCK
     INTEGER  :: KTBSZ,KTBLZ,NHMDLT,NWQMR,NHMR,NIT

!********************************************************************************
!**                           Sediment Model Setup                             **
!********************************************************************************



!***** Variable declarations

     REAL :: M1,       M2,        KAPPNH4F, KMNH4,    KMNH4O2,          &
             KAPPNO3F, K2NO3,     KAPPD1,   KAPPP1,   KMHSO2,           &
             KSI,      KMPSI,     KMO2DP,   KBENSTR,                    &
             KAPPCH4,  KAPPNH4S,         &
             KAPPNO3S, O2CRITSI,  JSIDETR,  KLBNTH,   DPMIN
     REAL :: K0H1D,    K0H1P,     K1H1D,    K1H1P,    K2H2D,            &
             K2H2P,    K3,        J1,       J2,       KMC1,             &
             KL12,     KL12NOM,   KHS_1,    KL12SO4
     REAL :: NH40,     NH41,      NH42,     NH4T1,     NH4T2,           &
             NH41TM1,  NH4T2TM1,  JNH4,     NO30,      NO31,            &
             NO32,     NO3T1,     NO3T2,    NO31TM1,   NO3T2TM1,        &
             JNO3,     JHS,       JSI,      JPO4,      JCH4AQ,          &
             JO2NH4,    KMCH4O2,  KMSO4,                                &
             JCH4G,     CH40,     CH41,     CH42,                       &
             CH42AV,   CH4T1,     CH4T2,    CH4T2AV,  CH41TM1,          &
             CH4T2TM1, JCH4,      CH4T2SAV, JCH4GASO2,                  &
             JGAS,     SO40,      SO41,     SO42,     SO42AV,           &
             SO4T1,    SO4T2,     SO4T2AV,  SO40MG,   SO41TM1,          &
             SO4T2TM1, JSO4,      SO4T2SAV, HS0 
     REAL :: KPOP1,    KPOP2,     KPOP3,    KPON1,     KPON2,           &
             KPON3,    KPOC1,     KPOC2,    KPOC3 
     REAL :: ISEDMN,   ISEDMP,    ISEDMC,   S2FLXN,    S2FLXP,          &
             S2FLXC,   IWCMN,           &
             IWCMP,    IWCMC
     REAL :: KTGB1,    KTGB2,     KHNB,     KHPB,                       &  !CFC NOV24
             KTBB,     KHRB,      NH4AVL,          &
             NO3AVL,   KESED,     KEBALG,   KETOTL
     LOGICAL :: BENTHIC_OUTPUT, STEADY_STATE_SED, BALGAE_CALC

!***** Dimension declarations

     REAL,DIMENSION(3) :: KPDIAG, DPTHTA, KNDIAG, DNTHTA, KCDIAG, DCTHTA

     REAL,ALLOCATABLE,DIMENSION(:) :: PON1TM1S,  PON2TM1S,  PON3TM1S,        &
               POC1TM1S,  POC2TM1S,  POC3TM1S,        &
               POP1TM1S,  POP2TM1S,  POP3TM1S,        &
               PSITM1S,   BENSTR1S,  BFORMAXS,        &
               ISWBENS

     REAL,ALLOCATABLE,DIMENSION(:) :: NH41TM1S,  NH4T2TM1S, NO31TM1S,     &
               NO3T2TM1S, HS1TM1S,   HST2TM1S,        &
               SI1TM1S,   SIT2TM1S,  PO41TM1S,        &
               PO4T2TM1S

     REAL,ALLOCATABLE,DIMENSION(:) :: CH4T2TM1S, SO4T2TM1S, CH41TM1S       !CH4

     REAL,ALLOCATABLE,DIMENSION(:) :: BURIALN,   BURIALP,   BURIALC,         &
               DIAGENC
     
     REAL,DIMENSION(350) :: ZHTADP,     ZHTADD,     ZHTANH4F,         &
               ZHTANO3F,   ZHTA2NO3,   ZHTAD1,           &
               ZHTAP1,     ZHTASI,     ZL12NOM,          &
               ZW12NOM,    ZHTAPON1,   ZHTAPON2,         &
               ZHTAPON3,   ZHTAPOC1,   ZHTAPOC2,         &
               ZHTAPOC3,   ZHTAPOP1,   ZHTAPOP2,         &
               ZHTAPOP3,   ZHTACH4,    ZHTANH4S,         &
               ZHTANO3S

     REAL,ALLOCATABLE,DIMENSION(:) :: AG3CFL, AG3NFL, AG3PFL, ASDTMP

     REAL,ALLOCATABLE,DIMENSION(:) :: FIB,       NLB,       PLB,      &
               NPPB,      BBM,       BLITE,           &
               BMB,       PB,        PRB,             &
               BANH4,     BANO3,     BAPO4,           &
               BADOC,     BADO,      BAPOC,           &
               BAPON,     BAPOP
     
     REAL :: THTAPOP1,THTAPOP2,THTAPOP3,THTAPON1,THTAPON2,         &
             THTAPON3,THTAPOC1,THTAPOC2,THTAPOC3

!***** Equivalence declarations

     EQUIVALENCE (KPDIAG(1),KPOP1),    (KPDIAG(2),KPOP2),               &
                 (KPDIAG(3),KPOP3),    (KNDIAG(1),KPON1),               &
                 (KNDIAG(2),KPON2),    (KNDIAG(3),KPON3),               &
                 (KCDIAG(1),KPOC1),    (KCDIAG(2),KPOC2),               &
                 (KCDIAG(3),KPOC3),    (DPTHTA(1),THTAPOP1),            &
                 (DPTHTA(2),THTAPOP2), (DPTHTA(3),THTAPOP3),            &
                 (DNTHTA(1),THTAPON1), (DNTHTA(2),THTAPON2),            &
                 (DNTHTA(3),THTAPON3), (DCTHTA(1),THTAPOC1),            &
                 (DCTHTA(2),THTAPOC2), (DCTHTA(3),THTAPOC3)


!***** Common declarations
!     REAL :: DP,W2,DD,THTADP,THTADD,PIENH4,THTANH4,THTANO3,         &
     REAL :: DPP,W2,DD,THTADP,THTADD,PIENH4,THTANH4,THTANO3,         &
                 PIE1S,PIE2S,THTAPD1,CSISAT,DPIE1SI,PIE2SI,H2,THTASI,   &
		 DPIE1PO4,PIE2PO4,O2CRIT,TEMPBEN,THTACH4,DPIE1PO4F,     &
		 DPIE1PO4S
     INTEGER  :: KAPPNH4,KAPP1NO3,KAPCH4
     REAL :: HS1,HS2,HST1,HST2,HS1TM1,HST2TM1,SI0,SI1,SI2,SIT1,SIT2, &
                 SI1TM1,SIT2TM1,PO40,PO41,PO42,PO4T1,PO4T2,PO41TM1,      &
		 PO4T2TM1
     REAL :: PON1,PON1TM1,PON2,PON2TM1,PON3,PON3TM1,POC1,POC1TM1,    &
                 POC2,POC2TM1,POC3,POC3TM1,POP1,POP1TM1,POP2,POP2TM1,    &
		 POP3,POP3TM1,XJN,XJC,XJP,PSI,PSITM1,XJCNO3,XJCO2,XJC1
     REAL :: PIE1,PIE2,W12,TEMPD,O20,CH4SAT,SAL
     REAL :: SALTSW,DLTS,SALTND
     REAL :: XAPPNH4,XAPPD1,XAPPP1,XAPP1NO3,XK2NO3,XKSI,XAPPCH4,     &
                 TEMP20,TEMP202,FD1,FP1,FD2,FP2,SOD,CSOD,S,W12NOM,       &
		 BENSTR,BENSTRS,BENSTR1,ISWBEN,BFORMAX,HSO4,DDSO4,       &
		 CSODHS,CSODCH4
     REAL :: ZHTANH4,ZHTANO3
     REAL :: SEDMN,SEDMP,SEDMC,S1FLXN,S1FLXP,S1FLXC,S3FLXN,S3FLXP,   &
             S3FLXC,ATMFLXN,ATMFLXP,BENFLXPN,BENFLXPP,BENFLXPC,      &
	     BENFLXDN,BENFLXDP,DLWCKMN,DLWCKMC,BNDFLXN,BNDFLXP,      &
	     BNDFLXC,DLSEDKN,DLSEDKC,BURIALFLXN,BURIALFLXP,BURIALFLXC
     INTEGER  :: IWCMS		 
     REAL :: TMB,BMRB,TRB,PMB,BPRB,FNIB,ANCB,FPIB,APCB,FCDB,BALGMIN

     REAL :: TINTIM

!********************************************************************************
!**                             SAV Model Setup                                **
!********************************************************************************


!***** Variable declarations

     CHARACTER(LEN=8) SPNAM,    EPINAM
     REAL :: KHSTOX,   KESAV,    KEEPI,                       &
             KHEP,      NLIMEPI
     REAL :: KHNLEAF,  KHNROOT,  KHPLEAF,   KHPROOT,  KHNEPI,          &
             KHPEPI,   KHNPSAV,   KHNPEPI
     
     REAL :: PMSAV,BMSAV,BMTBRREF,FPSR,ALPHSAV,FDOSR,ALAC,SLSAV,     &
             SALMAX,PRSPSAVB,ACDWSAV,ANCSAV,ANDWSAV,APDWSAV,HCAN,    &
	     APCSAV,ACAN,BCAN
     REAL :: FNISAV,FNLDSAV,FNRDSAV,FNLPSAV,FNRPSAV,FPISAV,FPLDSAV,  &
             FPRDSAV,FPLPSAV,FPRPSAV,FCLDSAV,FCRDSAV,FCLPSAV,        &
	     FCRPSAV,FDOSAV
     REAL :: WSSSAV,WSLSAV,WSRSAV,WS1SAV,WS2SAV,WS3SAV,WSUSAV
     INTEGER  :: NDDI,NSAVCELL,NSAVDPHM   
     REAL :: PMEPI,BMEPI,PREPI,ALPHEPI,FCLDPEP,FCRDPEP,FCLPPEP,      &
             FCRPPEP,PRSPEPI,CCHLEPI,ANCEPI,APCEPI,ADWCEPI
     REAL :: FNIEPI,FNLDEPI,FNRDEPI,FNLPEPI,FNRPEPI
!***** Dimension declarations

     REAL,DIMENSION(0:1000) :: FPLEAF,FPROOT,FPSTEM,FPTUBER,TRTBRLF
!     REAL :: SLSH(NSBP)
     REAL,ALLOCATABLE :: NSAVDPH(:)
     REAL,ALLOCATABLE,DIMENSION(:,:) :: SAVWDH, SAVDPH, SAVFRAC
     INTEGER,ALLOCATABLE,DIMENSION(:) :: SAVCELL

     REAL,ALLOCATABLE,DIMENSION(:,:) :: EPATN, FISH, PLEAF, SAVATN,   &
                 NPPSAV, NPPEPI, LEAF, ROOT, EP, FIEP, WATATN, STEM, TUBER
     REAL,ALLOCATABLE,DIMENSION(:) :: DOSAV, LDOCSAV, LPOCSAV, RPOCSAV,   &
                 DOEPI, LDOCEPI, LPOCEPI, RPOCEPI, FHS, RDOCSAV, RDOCEPI, &
                 DGRDAYS

     REAL,ALLOCATABLE,DIMENSION(:) :: BMLEAF, BMSTEM, BMROOT, BMTUBER,      &
                 NLSAV,    FNSEDSAV, PLSAV,    FPSEDSAV, NH4SAVW,  NO3SAVW, &
                 LDONSAVW, LPONSAVW, RPONSAVW, PO4SAVW,  LDOPSAVW, LPOPSAVW,&
                 RPOPSAVW, RDONSAVW, RDOPSAVW, NLEPI,    PLEPI,    NH4EPI,  &
                 NO3EPI,   LDONEPI,  LPONEPI,  RPONEPI,  PO4EPI,   LDOPEPI, &
		 LPOPEPI,  RPOPEPI,  RDONEPI,  RDOPEPI,  SAVAREA,  SEDCSAV, &
		 SEDDOSAV, SEDNH4SAV,SEDPO4SAV,SEDNSAV,SEDPSAV
     REAL,ALLOCATABLE,DIMENSION(:,:) :: PEP,    BMEP,    PREP
     REAL,DIMENSION(3) :: FRCSAV,      FRNSAV,      FRPSAV
     REAL,DIMENSION(-50:400) ::  FTPSAV,FTRSAV,FTPEP, FTREP, FTPREP
     REAL :: FNIPEP,FNLDPEP,FNRDPEP,FNLPPEP,FNRPPEP,FPIEPI,FPLDEPI,    &
             FPRDEPI,FPLPEPI,FPRPEPI,FPIPEP,FPLDPEP,FPRDPEP,FPLPPEP,   &
	     FPRPPEP,FCLDEPI,FCRDEPI

!********************************************************************************
!**                             OWQ Model Setup                                **
!*******************************************************************************


!***** Variable declarations

     REAL :: LAT, LONG
     
     REAL :: GSTAR440,PHISTAR676,BNSTARNTU,BNSTARCHL,        &
             PSTARINT,PSTAR440,PSTARCOLOR,SCDOM,EZEROINT,    &
	     TCHL2CHL,DOFFSET,DEPTHKE

     INTEGER  :: NWAVEL
!***** Dimension declarations

     REAL,ALLOCATABLE,DIMENSION(:,:) :: COLOR, TURB
     REAL,SAVE :: OPZ(9), G1(9), G2(9)
     REAL,DIMENSION(100) :: ALAMB, GLAMB, PLAMB, PHILAMB,         &
                            BLAMB, WAVEL, EZERO
     REAL,DIMENSION(10) :: GMUZERO

!vjp 10/11/04 removed OPZ from common block OWQ2 
!    and moved init of OPZ, G1, G2 to wqm_owq.F

! *** deposit feeders ***

     LOGICAL :: DEPFEED
     
     REAL :: XKR,XKI0,XKBETA

     REAL,ALLOCATABLE :: DFEEDM1S(:)
     REAL :: DFEED,DFEEDM1,                                               &
           XKMI0,ING0,THTAI0,R,THTAR,BETA,THBETA,                         &
           AMCN,AMCP,AA1,AA2,XKMG1,XKMG2,TDD,                             &
           ZHTAI0(350),ZHTAR(350),ZHTABETA(350)

     REAL,ALLOCATABLE,DIMENSION(:) :: DF_GROW, DF_RESP, DF_PRED, DF_MORT     &
                           ,ADF_GROW,ADF_RESP,ADF_PRED,ADF_MORT

! **** Suspension Feeders ****
!MBM 971021 added turbidity sensitivity parameters
!  MBM 970109 added logical for hypoxic fx on benthos

     LOGICAL :: SFEEDER, HYPOXFX
     REAL :: FRSASF
     INTEGER  :: NSPECIES

     INTEGER,ALLOCATABLE :: SEDTYPE(:,:)

     REAL,DIMENSION(NSSFP) :: FILT,  SFA1,  SFA2,  SFA3,          &
          SFA4,  SFA5,                                      &
          MAXING,SFCN,  SFCP,                        &
          SFRESP,SFPRED,SFTMN, THTAFILT,      &
          THTARESP, THTAPRED, XKPO2,                 &
          SFTD,    FILTFACT, RESPFACT,               &
          SFDOH, SFDOQ, SFATURB, SFBTURB
!MBM 971021 added turbidity sensitivity parameters
     REAL,ALLOCATABLE,DIMENSION(:) :: SFLUXC,  SFLUXN,  SFLUXP, SF_RPOC  &
                            ,SF_RPON, SF_RPOP, JNH4SF, JPO4SF        &
                            ,SODSF,   JSASF,   JSUSF                 &
                            ,SF_SSI,  SF_SU,  SF_SA,   SF_PIP
     REAL,ALLOCATABLE :: SFEED(:,:) 
 
!MBM 961226 sfeeder diagnostic arrays for output from main, inc in SUSFDERS cmn
     REAL,ALLOCATABLE,DIMENSION(:,:) :: SFFILTCT,SFRESPCT,                 &
                                   SFPREDCT,SFRATEI,                       &
                                   SFGCMAX, SFGMIN,                        &
                                   SFGCING, SFCFECES,                      &
                                   SFNFECES,SFPFECES,                      &
                                   SFCPSFEC,SFNPSFEC,                      &
                                   SFPPSFEC

     REAL,ALLOCATABLE,DIMENSION(:,:) :: SF_SFGC,  SF_RESP,  SF_PRED,  SF_RMORT,&
                                   ASF_SFGC, ASF_RESP, ASF_PRED, ASF_RMORT
				   
   CONTAINS
   
   SUBROUTINE ALLOC_WQM
   USE ALL_VARS
   IMPLICIT NONE
   
   ALLOCATE(T(0:MTLOC,KBM1));     T = 0.0
   ALLOCATE(SSI(0:MTLOC,KBM1));   SSI = 0.0
   ALLOCATE(B1(0:MTLOC,KBM1));    B1  = 0.0
   ALLOCATE(B2(0:MTLOC,KBM1));    B2  = 0.0
   ALLOCATE(B3(0:MTLOC,KBM1));    B3  = 0.0
   ALLOCATE(LDOC(0:MTLOC,KBM1));  LDOC = 0.0
   ALLOCATE(LPOC(0:MTLOC,KBM1));  LPOC = 0.0
   ALLOCATE(RPOC(0:MTLOC,KBM1));  RPOC = 0.0
   ALLOCATE(NH4(0:MTLOC,KBM1));   NH4  = 0.0
   ALLOCATE(NO3(0:MTLOC,KBM1));   NO3  = 0.0
   ALLOCATE(LDON(0:MTLOC,KBM1));  LDON = 0.0
   ALLOCATE(LPON(0:MTLOC,KBM1));  LPON = 0.0
   ALLOCATE(RPON(0:MTLOC,KBM1));  RPON = 0.0
   ALLOCATE(PO4(0:MTLOC,KBM1));   PO4  = 0.0
   ALLOCATE(LDOP(0:MTLOC,KBM1));  LDOP = 0.0
   ALLOCATE(LPOP(0:MTLOC,KBM1));  LPOP = 0.0
   ALLOCATE(RPOP(0:MTLOC,KBM1));  RPOP = 0.0
   ALLOCATE(COD(0:MTLOC,KBM1));   COD  = 0.0
   ALLOCATE(DO(0:MTLOC,KBM1));    DO   = 0.0
   ALLOCATE(SU(0:MTLOC,KBM1));    SU   = 0.0
   ALLOCATE(SA(0:MTLOC,KBM1));    SA   = 0.0
   ALLOCATE(SALT(0:MTLOC,KBM1));  SALT = 0.0
   ALLOCATE(SZ(0:MTLOC,KBM1));    SZ   = 0.0
   ALLOCATE(LZ(0:MTLOC,KBM1));    LZ   = 0.0
   ALLOCATE(RDOC(0:MTLOC,KBM1));  RDOC = 0.0
   ALLOCATE(RDON(0:MTLOC,KBM1));  RDON = 0.0
   ALLOCATE(RDOP(0:MTLOC,KBM1));  RDOP = 0.0
   ALLOCATE(UREA(0:MTLOC,KBM1));  UREA = 0.0
   ALLOCATE(PIP(0:MTLOC,KBM1));   PIP  = 0.0
   ALLOCATE(PIB1(0:MTLOC,KBM1));  PIB1 = 0.0
   ALLOCATE(PIB2(0:MTLOC,KBM1));  PIB2 = 0.0 
   ALLOCATE(PIB3(0:MTLOC,KBM1));  PIB3 = 0.0

   ALLOCATE(PN1(MTLOC,KBM1));            PN1     = 0.0
   ALLOCATE(PN2(MTLOC,KBM1));            PN2     = 0.0
   ALLOCATE(PN3(MTLOC,KBM1));            PN3     = 0.0
   ALLOCATE(BM1(MTLOC,KBM1));            BM1     = 0.0
   ALLOCATE(BM2(MTLOC,KBM1));            BM2     = 0.0
   ALLOCATE(BM3(MTLOC,KBM1));            BM3     = 0.0
   ALLOCATE(MNLLDOC(MTLOC,KBM1));        MNLLDOC = 0.0
   ALLOCATE(FTCOD(MTLOC,KBM1));          FTCOD   = 0.0     
   ALLOCATE(NPP(MTLOC,KBM1));            NPP     = 0.0
   ALLOCATE(P1(MTLOC,KBM1));             P1      = 0.0
   ALLOCATE(P2(MTLOC,KBM1));             P2      = 0.0
   ALLOCATE(P3(MTLOC,KBM1));             P3      = 0.0
   ALLOCATE(NT(MTLOC,KBM1));             NT      = 0.0
   ALLOCATE(PR1(MTLOC,KBM1));            PR1     = 0.0
   ALLOCATE(PR2(MTLOC,KBM1));            PR2     = 0.0
   ALLOCATE(PR3(MTLOC,KBM1));            PR3     = 0.0
   ALLOCATE(DENIT(MTLOC,KBM1));          DENIT   = 0.0 
   ALLOCATE(RATOX(MTLOC,KBM1));          RATOX   = 0.0
   ALLOCATE(GPP(MTLOC,KBM1));            GPP     = 0.0
   ALLOCATE(MNLRDOC(MTLOC,KBM1));        MNLRDOC = 0.0
   ALLOCATE(P1NNF(MTLOC,KBM1));          P1NNF   = 0.0
   
   ALLOCATE(ASRAT(MTLOC));             ASRAT   = 0.0

   ALLOCATE(KE(MTLOC,KBM1));           KE     = 0.0  
   ALLOCATE(FI1(MTLOC,KBM1));          FI1    = 0.0
   ALLOCATE(FI2(MTLOC,KBM1));          FI2    = 0.0
   ALLOCATE(FI3(MTLOC,KBM1));          FI3    = 0.0
   ALLOCATE(NL1(MTLOC,KBM1));          NL1    = 0.0
   ALLOCATE(NL2(MTLOC,KBM1));          NL2    = 0.0
   ALLOCATE(NL3(MTLOC,KBM1));          NL3    = 0.0
   ALLOCATE(PL1(MTLOC,KBM1));          PL1    = 0.0
   ALLOCATE(PL2(MTLOC,KBM1));          PL2    = 0.0 
   ALLOCATE(PL3(MTLOC,KBM1));          PL3    = 0.0
   ALLOCATE(SL2(MTLOC,KBM1));          SL2    = 0.0
   ALLOCATE(RESP(MTLOC,KBM1));         RESP   = 0.0
   ALLOCATE(KESS(MTLOC,KBM1));         KESS   = 0.0
   ALLOCATE(SL1(MTLOC,KBM1));          SL1    = 0.0
   ALLOCATE(SL3(MTLOC,KBM1));          SL3    = 0.0
   ALLOCATE(IAVG(MTLOC,KBM1));         IAVG   = 0.0
   ALLOCATE(IATBOT(MTLOC,KBM1));       IATBOT = 0.0
   ALLOCATE(KEISS(MTLOC,KBM1));        KEISS  = 0.0
   ALLOCATE(KEVSS(MTLOC,KBM1));        KEVSS  = 0.0
   ALLOCATE(KEDOC(MTLOC,KBM1));        KEDOC  = 0.0

   ALLOCATE(PM1(MTLOC,KBM1));          PM1   = 0.0   
   ALLOCATE(PM2(MTLOC,KBM1));          PM2   = 0.0
   ALLOCATE(PM3(MTLOC,KBM1));          PM3   = 0.0
   ALLOCATE(BMR1(MTLOC,KBM1));         BMR1  = 0.0
   ALLOCATE(BMR2(MTLOC,KBM1));         BMR2  = 0.0
   ALLOCATE(BMR3(MTLOC,KBM1));         BMR3  = 0.0
   ALLOCATE(BPR1(MTLOC,KBM1));         BPR1  = 0.0
   ALLOCATE(BPR2(MTLOC,KBM1));         BPR2  = 0.0
   ALLOCATE(BPR3(MTLOC,KBM1));         BPR3  = 0.0
   ALLOCATE(CCHL1(MTLOC,KBM1));        CCHL1 = 0.0
   ALLOCATE(CCHL2(MTLOC,KBM1));        CCHL2 = 0.0
   ALLOCATE(CCHL3(MTLOC,KBM1));        CCHL3 = 0.0
   ALLOCATE(Q1(MTLOC,KBM1));           Q1    = 0.0
   ALLOCATE(Q2(MTLOC,KBM1));           Q2    = 0.0
   ALLOCATE(Q3(MTLOC,KBM1));           Q3    = 0.0

   ALLOCATE(DTUREA(MTLOC,KBM1));       DTUREA = 0.0
   
   ALLOCATE(B1SZ(MTLOC,KBM1));         B1SZ = 0.0    
   ALLOCATE(B2SZ(MTLOC,KBM1));         B2SZ = 0.0
   ALLOCATE(B3SZ(MTLOC,KBM1));         B3SZ = 0.0
   ALLOCATE(B1LZ(MTLOC,KBM1));         B1LZ = 0.0
   ALLOCATE(B2LZ(MTLOC,KBM1));         B2LZ = 0.0
   ALLOCATE(B3LZ(MTLOC,KBM1));         B3LZ = 0.0
   ALLOCATE(DOSZ(MTLOC,KBM1));         DOSZ = 0.0
   ALLOCATE(DOLZ(MTLOC,KBM1));         DOLZ = 0.0
   ALLOCATE(SASZ(MTLOC,KBM1));         SASZ = 0.0
   ALLOCATE(SUSZ(MTLOC,KBM1));         SUSZ = 0.0
   ALLOCATE(SALZ(MTLOC,KBM1));         SALZ = 0.0
   ALLOCATE(SULZ(MTLOC,KBM1));         SULZ = 0.0

   ALLOCATE(CFIX(MTLOC));              CFIX  = 0.0
   ALLOCATE(SNFIX(MTLOC));             SNFIX = 0.0

   ALLOCATE(WSS(0:MTLOC,KBM1));        WSS   = 0.0
   ALLOCATE(WSL(0:MTLOC,KBM1));        WSL   = 0.0
   ALLOCATE(WSR(0:MTLOC,KBM1));        WSR   = 0.0
   ALLOCATE(WS1(0:MTLOC,KBM1));        WS1   = 0.0
   ALLOCATE(WS2(0:MTLOC,KBM1));        WS2   = 0.0
   ALLOCATE(WS3(0:MTLOC,KBM1));        WS3   = 0.0
   ALLOCATE(WSU(0:MTLOC,KBM1));        WSU   = 0.0
   ALLOCATE(WSSHI(0:MTLOC,KBM1));      WSSHI = 0.0

   ALLOCATE(FLXSPIB1(MTLOC,KBM1));     FLXSPIB1 = 0.0
   ALLOCATE(FLXSPIB2(MTLOC,KBM1));     FLXSPIB2 = 0.0
   ALLOCATE(FLXSPIB3(MTLOC,KBM1));     FLXSPIB3 = 0.0

   ALLOCATE(V1(0:MTLOC,KBM1));         V1 = 0.0    
   ALLOCATE(V2(0:MTLOC,KBM1));         V2 = 0.0
   
   ALLOCATE(WSSNET(MTLOC));            WSSNET = 0.0
   ALLOCATE(WSLNET(MTLOC));            WSLNET = 0.0
   ALLOCATE(WSRNET(MTLOC));            WSRNET = 0.0
   ALLOCATE(WS1NET(MTLOC));            WS1NET = 0.0
   ALLOCATE(WS2NET(MTLOC));            WS2NET = 0.0
   ALLOCATE(WS3NET(MTLOC));            WS3NET = 0.0
   ALLOCATE(VSED(MTLOC));              VSED   = 0.0
   ALLOCATE(VPMIX(MTLOC));             VPMIX  = 0.0
   ALLOCATE(VDMIX(MTLOC));             VDMIX  = 0.0
   ALLOCATE(DIAGP(MTLOC));             DIAGP  = 0.0
   ALLOCATE(DIAGN(MTLOC));             DIAGN  = 0.0
   ALLOCATE(DIAGC(MTLOC));             DIAGC  = 0.0
   ALLOCATE(DIAGS(MTLOC));             DIAGS  = 0.0
   ALLOCATE(MTVEL(MTLOC));             MTVEL  = 0.0
   ALLOCATE(WSUNET(MTLOC));            WSUNET = 0.0
   
   ALLOCATE(BL(0:MTLOC,KBM1,3));       BL   = 0.0
   ALLOCATE(V1S(0:MTLOC,KBM1));        V1S  = 0.0
   ALLOCATE(HMV(0:MTLOC,KBM1));        HMV  = 0.0
   ALLOCATE(HMBV(MTLOC));              HMBV = 0.0
   ALLOCATE(ZD(0:MTLOC,KBM1));         ZD   = 0.0

   ALLOCATE(DTM(0:MTLOC,KBM1,NCP));    DTM = 0.0

   ALLOCATE(B1ASZ(MTLOC,KBM1));        B1ASZ = 0.0 
   ALLOCATE(B2ASZ(MTLOC,KBM1));        B2ASZ = 0.0
   ALLOCATE(B3ASZ(MTLOC,KBM1));        B3ASZ = 0.0
   ALLOCATE(LPOCASZ(MTLOC,KBM1));      LPOCASZ = 0.0
   ALLOCATE(RPOCASZ(MTLOC,KBM1));      RPOCASZ = 0.0
   ALLOCATE(PRASZ(MTLOC,KBM1));        PRASZ = 0.0
   ALLOCATE(B1ALZ(MTLOC,KBM1));        B1ALZ = 0.0
   ALLOCATE(B2ALZ(MTLOC,KBM1));        B2ALZ = 0.0
   ALLOCATE(B3ALZ(MTLOC,KBM1));        B3ALZ = 0.0
   ALLOCATE(SZALZ(MTLOC,KBM1));        SZALZ = 0.0
   ALLOCATE(LPOCALZ(MTLOC,KBM1));      LPOCALZ = 0.0
   ALLOCATE(RPOCALZ(MTLOC,KBM1));      RPOCALZ = 0.0
   ALLOCATE(PRALZ(MTLOC,KBM1));        PRALZ = 0.0
   ALLOCATE(CLSZ(MTLOC,KBM1));         CLSZ = 0.0
   ALLOCATE(CLLZ(MTLOC,KBM1));         CLLZ = 0.0
   ALLOCATE(RSZ(MTLOC,KBM1));          RSZ = 0.0
   ALLOCATE(RLZ(MTLOC,KBM1));          RLZ = 0.0
   ALLOCATE(RMAXSZ(MTLOC,KBM1));       RMAXSZ = 0.0
   ALLOCATE(RMAXLZ(MTLOC,KBM1));       RMAXLZ = 0.0
   ALLOCATE(BMSZ(MTLOC,KBM1));         BMSZ = 0.0
   ALLOCATE(BMLZ(MTLOC,KBM1));         BMLZ = 0.0
   ALLOCATE(BMRSZ(MTLOC,KBM1));        BMRSZ = 0.0
   ALLOCATE(BMRLZ(MTLOC,KBM1));        BMRLZ = 0.0
   ALLOCATE(MSZ(MTLOC,KBM1));          MSZ = 0.0
   ALLOCATE(MLZ(MTLOC,KBM1));          MLZ = 0.0
   ALLOCATE(PRSZLZ(MTLOC,KBM1));       PRSZLZ = 0.0
   ALLOCATE(GSZ(MTLOC,KBM1));          GSZ = 0.0
   ALLOCATE(GLZ(MTLOC,KBM1));          GLZ = 0.0
   ALLOCATE(ESZ(MTLOC,KBM1));          ESZ = 0.0
   ALLOCATE(ELZ(MTLOC,KBM1));          ELZ = 0.0
   ALLOCATE(RFSZ(MTLOC,KBM1));         RFSZ = 0.0
   ALLOCATE(RFLZ(MTLOC,KBM1));         RFLZ = 0.0
   ALLOCATE(PRSZ(MTLOC,KBM1));         PRSZ = 0.0
   ALLOCATE(PRLZ(MTLOC,KBM1));         PRLZ = 0.0
   ALLOCATE(LDOCASZ(MTLOC,KBM1));      LDOCASZ = 0.0
   ALLOCATE(BPRSZ(MTLOC,KBM1));        BPRSZ = 0.0
   ALLOCATE(BPRLZ(MTLOC,KBM1));        BPRLZ = 0.0
   ALLOCATE(RDOCASZ(MTLOC,KBM1));      RDOCASZ = 0.0

   ALLOCATE(LDOCSZ(MTLOC,KBM1));       LDOCSZ = 0.0
   ALLOCATE(LPOCSZ(MTLOC,KBM1));       LPOCSZ = 0.0
   ALLOCATE(RPOCSZ(MTLOC,KBM1));       RPOCSZ = 0.0
   ALLOCATE(LDOCLZ(MTLOC,KBM1));       LDOCLZ = 0.0
   ALLOCATE(LPOCLZ(MTLOC,KBM1));       LPOCLZ = 0.0
   ALLOCATE(RPOCLZ(MTLOC,KBM1));       RPOCLZ = 0.0
   ALLOCATE(NH4SZ(MTLOC,KBM1));        NH4SZ = 0.0
   ALLOCATE(LDONSZ(MTLOC,KBM1));       LDONSZ = 0.0
   ALLOCATE(LPONSZ(MTLOC,KBM1));       LPONSZ = 0.0
   ALLOCATE(RPONSZ(MTLOC,KBM1));       RPONSZ = 0.0
   ALLOCATE(NH4LZ(MTLOC,KBM1));        NH4LZ = 0.0
   ALLOCATE(LDONLZ(MTLOC,KBM1));       LDONLZ = 0.0
   ALLOCATE(LPONLZ(MTLOC,KBM1));       LPONLZ = 0.0
   ALLOCATE(RPONLZ(MTLOC,KBM1));       RPONLZ = 0.0
   ALLOCATE(PO4SZ(MTLOC,KBM1));        PO4SZ = 0.0
   ALLOCATE(LDOPSZ(MTLOC,KBM1));       LDOPSZ = 0.0
   ALLOCATE(LPOPSZ(MTLOC,KBM1));       LPOPSZ = 0.0
   ALLOCATE(RPOPSZ(MTLOC,KBM1));       RPOPSZ = 0.0
   ALLOCATE(PO4LZ(MTLOC,KBM1));        PO4LZ = 0.0
   ALLOCATE(LDOPLZ(MTLOC,KBM1));       LDOPLZ = 0.0
   ALLOCATE(LPOPLZ(MTLOC,KBM1));       LPOPLZ = 0.0
   ALLOCATE(RPOPLZ(MTLOC,KBM1));       RPOPLZ = 0.0
   ALLOCATE(RDOCSZ(MTLOC,KBM1));       RDOCSZ = 0.0
   ALLOCATE(RDONSZ(MTLOC,KBM1));       RDONSZ = 0.0
   ALLOCATE(RDOPSZ(MTLOC,KBM1));       RDOPSZ = 0.0
   ALLOCATE(RDOCLZ(MTLOC,KBM1));       RDOCLZ = 0.0
   ALLOCATE(RDONLZ(MTLOC,KBM1));       RDONLZ = 0.0
   ALLOCATE(RDOPLZ(MTLOC,KBM1));       RDOPLZ = 0.0
   ALLOCATE(PIB1SZ(MTLOC,KBM1));       PIB1SZ = 0.0
   ALLOCATE(PIB2SZ(MTLOC,KBM1));       PIB2SZ = 0.0
   ALLOCATE(PIB3SZ(MTLOC,KBM1));       PIB3SZ = 0.0
   ALLOCATE(PIB1LZ(MTLOC,KBM1));       PIB1LZ = 0.0
   ALLOCATE(PIB2LZ(MTLOC,KBM1));       PIB2LZ = 0.0
   ALLOCATE(PIB3LZ(MTLOC,KBM1));       PIB3LZ = 0.0

   ALLOCATE(ALGDOC(MTLOC,KBM1));       ALGDOC  = 0.0 
   ALLOCATE(ALGPOC(MTLOC,KBM1));       ALGPOC  = 0.0
   ALLOCATE(HDRLPOC(MTLOC,KBM1));      HDRLPOC = 0.0
   ALLOCATE(HDRRPOC(MTLOC,KBM1));      HDRRPOC = 0.0

   ALLOCATE(KLDC(MTLOC,KBM1));         KLDC = 0.0   
   ALLOCATE(KRDC(MTLOC,KBM1));         KRDC = 0.0
   ALLOCATE(KDCALG(MTLOC,KBM1));       KDCALG = 0.0
   ALLOCATE(KLPC(MTLOC,KBM1));         KLPC = 0.0
   ALLOCATE(KLCALG(MTLOC,KBM1));       KLCALG = 0.0
   ALLOCATE(KRPC(MTLOC,KBM1));         KRPC = 0.0
   ALLOCATE(KSUA(MTLOC,KBM1));         KSUA = 0.0
   ALLOCATE(KCOD(MTLOC,KBM1));         KCOD = 0.0
   ALLOCATE(KLDN(MTLOC,KBM1));         KLDN = 0.0
   ALLOCATE(KRDN(MTLOC,KBM1));         KRDN = 0.0
   ALLOCATE(KDNALG(MTLOC,KBM1));       KDNALG = 0.0
   ALLOCATE(KLDP(MTLOC,KBM1));         KLDP = 0.0
   ALLOCATE(KRDP(MTLOC,KBM1));         KRDP = 0.0
   ALLOCATE(KDPALG(MTLOC,KBM1));       KDPALG = 0.0
   ALLOCATE(KLPN(MTLOC,KBM1));         KLPN = 0.0
   ALLOCATE(KLNALG(MTLOC,KBM1));       KLNALG = 0.0
   ALLOCATE(KLPP(MTLOC,KBM1));         KLPP = 0.0
   ALLOCATE(KLPALG(MTLOC,KBM1));       KLPALG = 0.0
   ALLOCATE(KRPN(MTLOC,KBM1));         KRPN = 0.0
   ALLOCATE(KRPP(MTLOC,KBM1));         KRPP = 0.0
   ALLOCATE(NTM(MTLOC,KBM1));          NTM = 0.0
   ALLOCATE(KRCOAG(MTLOC,KBM1));       KRCOAG = 0.0

   ALLOCATE(FTMNL(MTLOC,KBM1));        FTMNL = 0.0  
   ALLOCATE(FTHDR(MTLOC,KBM1));        FTHDR = 0.0
   ALLOCATE(RESPC(MTLOC,KBM1));        RESPC = 0.0   
   ALLOCATE(DLALGC(MTLOC,KBM1));       DLALGC = 0.0
   
   ALLOCATE(ALGNH4(MTLOC,KBM1));       ALGNH4 = 0.0
   ALLOCATE(ALGNO3(MTLOC,KBM1));       ALGNO3 = 0.0
   ALLOCATE(ALGDON(MTLOC,KBM1));       ALGDON = 0.0
   ALLOCATE(ALGPON(MTLOC,KBM1));       ALGPON = 0.0
   ALLOCATE(DENNO3(MTLOC,KBM1));       DENNO3 = 0.0
   ALLOCATE(MNLLDON(MTLOC,KBM1));      MNLLDON = 0.0
   ALLOCATE(HDRLPON(MTLOC,KBM1));      HDRLPON = 0.0
   ALLOCATE(HDRRPON(MTLOC,KBM1));      HDRRPON = 0.0
   ALLOCATE(MNLRDON(MTLOC,KBM1));      MNLRDON = 0.0
   ALLOCATE(NFIX(MTLOC,KBM1));         NFIX = 0.0

   ALLOCATE(ALGPO4(MTLOC,KBM1));       ALGPO4 = 0.0
   ALLOCATE(ALGDOP(MTLOC,KBM1));       ALGDOP = 0.0
   ALLOCATE(ALGPOP(MTLOC,KBM1));       ALGPOP = 0.0
   ALLOCATE(MNLLDOP(MTLOC,KBM1));      MNLLDOP = 0.0
   ALLOCATE(HDRLPOP(MTLOC,KBM1));      HDRLPOP = 0.0
   ALLOCATE(HDRRPOP(MTLOC,KBM1));      HDRRPOP = 0.0
   ALLOCATE(MNLRDOP(MTLOC,KBM1));      MNLRDOP = 0.0
   
   ALLOCATE(DORALG(MTLOC,KBM1));       DORALG = 0.0
   ALLOCATE(DOPR(MTLOC,KBM1));         DOPR = 0.0
   ALLOCATE(DCOD(MTLOC,KBM1));         DCOD = 0.0
   ALLOCATE(DDOC(MTLOC,KBM1));         DDOC = 0.0
   ALLOCATE(NITRIF(MTLOC,KBM1));       NITRIF = 0.0
   
   ALLOCATE(PSD(MTLOC,KBM1));          PSD = 0.0
   ALLOCATE(SAP(MTLOC,KBM1));          SAP = 0.0
   ALLOCATE(ALGUP(MTLOC,KBM1));        ALGUP = 0.0
   ALLOCATE(ALGRES(MTLOC,KBM1));       ALGRES = 0.0

   ALLOCATE(BENDOC(MTLOC));            BENDOC = 0.0
   ALLOCATE(BENNH4(MTLOC));            BENNH4 = 0.0
   ALLOCATE(BENNO3(MTLOC));            BENNO3 = 0.0
   ALLOCATE(BENDON(MTLOC));            BENDON = 0.0
   ALLOCATE(BENDOP(MTLOC));            BENDOP = 0.0
   ALLOCATE(BENPO4(MTLOC));            BENPO4 = 0.0
   ALLOCATE(BENCOD(MTLOC));            BENCOD = 0.0
   ALLOCATE(BENDO(MTLOC));             BENDO = 0.0
   ALLOCATE(BENSA(MTLOC));             BENSA = 0.0
   ALLOCATE(BENDEN(MTLOC));            BENDEN = 0.0
   ALLOCATE(BENCH4G(MTLOC));           BENCH4G = 0.0
   ALLOCATE(BENCH4A(MTLOC));           BENCH4A = 0.0

   ALLOCATE(COLOR(MTLOC,KBM1));        COLOR = 0.0 
   ALLOCATE(TURB(MTLOC,KBM1));         TURB  = 0.0
   
   ALLOCATE(PON1TM1S(MTLOC));          PON1TM1S = 0.0
   ALLOCATE(PON2TM1S(MTLOC));          PON2TM1S = 0.0
   ALLOCATE(PON3TM1S(MTLOC));          PON3TM1S = 0.0
   ALLOCATE(POC1TM1S(MTLOC));          POC1TM1S = 0.0
   ALLOCATE(POC2TM1S(MTLOC));          POC2TM1S = 0.0
   ALLOCATE(POC3TM1S(MTLOC));          POC3TM1S = 0.0
   ALLOCATE(POP1TM1S(MTLOC));          POP1TM1S = 0.0
   ALLOCATE(POP2TM1S(MTLOC));          POP2TM1S = 0.0
   ALLOCATE(POP3TM1S(MTLOC));          POP3TM1S = 0.0
   ALLOCATE(PSITM1S(MTLOC));           PSITM1S  = 0.0
   ALLOCATE(BENSTR1S(MTLOC));          BENSTR1S = 0.0
   ALLOCATE(BFORMAXS(MTLOC));          BFORMAXS = 0.0
   ALLOCATE(ISWBENS(MTLOC));           ISWBENS  = 0.0

   ALLOCATE(C1(0:MTLOC,KBM1,NCP));         C1    = 0.0
   ALLOCATE(C2(0:MTLOC,KBM1,NCP));         C2    = 0.0
   ALLOCATE(C2F(0:MTLOC,KBM1,NCP));        C2F    = 0.0
   ALLOCATE(CSTAR(0:MTLOC,KBM1,NCP));      CSTAR = 0.0
   ALLOCATE(DTC(0:MTLOC,KBM1,NCP));        DTC   = 0.0 
   ALLOCATE(AC1(0:MTLOC,KBM1,NCP));        AC1   = 0.0
   
   ALLOCATE(QLIT(MTLOC,KBM1));             QLIT = 0.0
   ALLOCATE(CONLIT(MTLOC,KBM1,NCP));       CONLIT = 0.0

   ALLOCATE(AFI1(MTLOC,KBM1));       AFI1 = 0.0 
   ALLOCATE(ANL1(MTLOC,KBM1));       ANL1 = 0.0
   ALLOCATE(APL1(MTLOC,KBM1));       APL1 = 0.0
   ALLOCATE(AFI2(MTLOC,KBM1));       AFI2 = 0.0
   ALLOCATE(ANL2(MTLOC,KBM1));       ANL2 = 0.0
   ALLOCATE(APL2(MTLOC,KBM1));       APL2 = 0.0
   ALLOCATE(ASL2(MTLOC,KBM1));       ASL2 = 0.0
   ALLOCATE(AFI3(MTLOC,KBM1));       AFI3 = 0.0
   ALLOCATE(ANL3(MTLOC,KBM1));       ANL3 = 0.0
   ALLOCATE(APL3(MTLOC,KBM1));       APL3 = 0.0
   ALLOCATE(ANPP(MTLOC,KBM1));       ANPP = 0.0
   ALLOCATE(ARESP(MTLOC,KBM1));      ARESP = 0.0
   ALLOCATE(AKE(MTLOC,KBM1));        AKE = 0.0
   ALLOCATE(ASL1(MTLOC,KBM1));       ASL1 = 0.0
   ALLOCATE(ASL3(MTLOC,KBM1));       ASL3 = 0.0
   ALLOCATE(AGPP(MTLOC,KBM1));       AGPP = 0.0
   
   ALLOCATE(ACCHL1(MTLOC,KBM1));     ACCHL1 = 0.0  
   ALLOCATE(ACCHL2(MTLOC,KBM1));     ACCHL2 = 0.0
   ALLOCATE(ACCHL3(MTLOC,KBM1));     ACCHL3 = 0.0

   ALLOCATE(ACLSZ(MTLOC,KBM1));        ACLSZ = 0.0
   ALLOCATE(ACLLZ(MTLOC,KBM1));        ACLLZ = 0.0
   ALLOCATE(ARSZ(MTLOC,KBM1));         ARSZ = 0.0
   ALLOCATE(ARLZ(MTLOC,KBM1));         ARLZ = 0.0
   ALLOCATE(ABMSZ(MTLOC,KBM1));        ABMSZ = 0.0
   ALLOCATE(ABMLZ(MTLOC,KBM1));        ABMLZ = 0.0
   ALLOCATE(AMSZ(MTLOC,KBM1));         AMSZ = 0.0
   ALLOCATE(AMLZ(MTLOC,KBM1));         AMLZ = 0.0
   ALLOCATE(APRSZLZ(MTLOC,KBM1));      APRSZLZ = 0.0
   ALLOCATE(AGSZ(MTLOC,KBM1));         AGSZ = 0.0
   ALLOCATE(AGLZ(MTLOC,KBM1));         AGLZ = 0.0
   ALLOCATE(ADOCSZ(MTLOC,KBM1));       ADOCSZ = 0.0
   ALLOCATE(APOCSZ(MTLOC,KBM1));       APOCSZ = 0.0
   ALLOCATE(ADOCLZ(MTLOC,KBM1));       ADOCLZ = 0.0
   ALLOCATE(APOCLZ(MTLOC,KBM1));       APOCLZ = 0.0
   ALLOCATE(ANH4SZ(MTLOC,KBM1));       ANH4SZ = 0.0
   ALLOCATE(ADONSZ(MTLOC,KBM1));       ADONSZ = 0.0
   ALLOCATE(APONSZ(MTLOC,KBM1));       APONSZ = 0.0
   ALLOCATE(ANH4LZ(MTLOC,KBM1));       ANH4LZ = 0.0
   ALLOCATE(ADONLZ(MTLOC,KBM1));       ADONLZ = 0.0
   ALLOCATE(APONLZ(MTLOC,KBM1));       APONLZ = 0.0
   ALLOCATE(APO4SZ(MTLOC,KBM1));       APO4SZ = 0.0
   ALLOCATE(ADOPSZ(MTLOC,KBM1));       ADOPSZ = 0.0
   ALLOCATE(APOPSZ(MTLOC,KBM1));       APOPSZ = 0.0
   ALLOCATE(APO4LZ(MTLOC,KBM1));       APO4LZ = 0.0
   ALLOCATE(ADOPLZ(MTLOC,KBM1));       ADOPLZ = 0.0
   ALLOCATE(APOPLZ(MTLOC,KBM1));       APOPLZ = 0.0
   ALLOCATE(APRSZ(MTLOC,KBM1));        APRSZ = 0.0
   ALLOCATE(APRLZ(MTLOC,KBM1));        APRLZ = 0.0
   ALLOCATE(APISZ(MTLOC,KBM1));        APISZ = 0.0
   ALLOCATE(APILZ(MTLOC,KBM1));        APILZ = 0.0
   
   ALLOCATE(AB1SZ(MTLOC,KBM1));        AB1SZ = 0.0  
   ALLOCATE(AB2SZ(MTLOC,KBM1));        AB2SZ = 0.0
   ALLOCATE(AB3SZ(MTLOC,KBM1));        AB3SZ = 0.0
   ALLOCATE(AB1LZ(MTLOC,KBM1));        AB1LZ = 0.0
   ALLOCATE(AB2LZ(MTLOC,KBM1));        AB2LZ = 0.0
   ALLOCATE(AB3LZ(MTLOC,KBM1));        AB3LZ = 0.0
   ALLOCATE(ADOSZ(MTLOC,KBM1));        ADOSZ = 0.0
   ALLOCATE(ADOLZ(MTLOC,KBM1));        ADOLZ = 0.0
   ALLOCATE(ASASZ(MTLOC,KBM1));        ASASZ = 0.0
   ALLOCATE(ASUSZ(MTLOC,KBM1));        ASUSZ = 0.0
   ALLOCATE(ASALZ(MTLOC,KBM1));        ASALZ = 0.0
   ALLOCATE(ASULZ(MTLOC,KBM1));        ASULZ = 0.0
   
   ALLOCATE(ABENDOC(MTLOC));          ABENDOC = 0.0  
   ALLOCATE(ABENNH4(MTLOC));          ABENNH4 = 0.0
   ALLOCATE(ABENNO3(MTLOC));          ABENNO3 = 0.0
   ALLOCATE(ABENPO4(MTLOC));          ABENPO4 = 0.0
   ALLOCATE(ABENCOD(MTLOC));          ABENCOD = 0.0
   ALLOCATE(ABENDO(MTLOC));           ABENDO = 0.0
   ALLOCATE(ABENSA(MTLOC));           ABENSA = 0.0
   ALLOCATE(ASSFWS(MTLOC));           ASSFWS = 0.0
   ALLOCATE(APCFWS(MTLOC));           APCFWS = 0.0
   ALLOCATE(APNFWS(MTLOC));           APNFWS = 0.0
   ALLOCATE(APPFWS(MTLOC));           APPFWS = 0.0
   ALLOCATE(APSFWS(MTLOC));           APSFWS = 0.0
   ALLOCATE(ACPIP(MTLOC));            ACPIP = 0.0
   ALLOCATE(ACPOS(MTLOC));            ACPOS = 0.0
   ALLOCATE(ADFEED(MTLOC));           ADFEED = 0.0
   ALLOCATE(ABENCH4G(MTLOC));         ABENCH4G = 0.0
   ALLOCATE(ABENCH4A(MTLOC));         ABENCH4A = 0.0

   ALLOCATE(AFIB(MTLOC));             AFIB = 0.0
   ALLOCATE(ANLB(MTLOC));             ANLB = 0.0
   ALLOCATE(APLB(MTLOC));             APLB = 0.0
   ALLOCATE(ANPPB(MTLOC));            ANPPB = 0.0
   ALLOCATE(ABBM(MTLOC));             ABBM = 0.0
   ALLOCATE(ABLITE(MTLOC));           ABLITE = 0.0
   ALLOCATE(ABMB(MTLOC));             ABMB = 0.0
   ALLOCATE(APB(MTLOC));              APB = 0.0
   ALLOCATE(APRB(MTLOC));             APRB = 0.0
   ALLOCATE(ABADOC(MTLOC));           ABADOC = 0.0
   ALLOCATE(ABAPOC(MTLOC));           ABAPOC = 0.0
   ALLOCATE(ABANH4(MTLOC));           ABANH4 = 0.0
   ALLOCATE(ABANO3(MTLOC));           ABANO3 = 0.0
   ALLOCATE(ABAPON(MTLOC));           ABAPON = 0.0
   ALLOCATE(ABAPO4(MTLOC));           ABAPO4 = 0.0
   ALLOCATE(ABAPOP(MTLOC));           ABAPOP = 0.0
   ALLOCATE(ABADO(MTLOC));            ABADO = 0.0
   
   ALLOCATE(ACPOC(MTLOC,3));          ACPOC = 0.0
   ALLOCATE(ACPON(MTLOC,3));          ACPON = 0.0
   ALLOCATE(ACPOP(MTLOC,3));          ACPOP = 0.0
   ALLOCATE(ASFEED(MTLOC,NSSFP));     ASFEED = 0.0
      
   ALLOCATE(AJNSF(MTLOC));            AJNSF = 0.0    
   ALLOCATE(AJPSF(MTLOC));            AJPSF = 0.0
   ALLOCATE(ASODSF(MTLOC));           ASODSF = 0.0
   ALLOCATE(ASASF(MTLOC));            ASASF = 0.0
   ALLOCATE(ASUSF(MTLOC));            ASUSF = 0.0
   ALLOCATE(ASFGCIN(MTLOC));          ASFGCIN = 0.0
   ALLOCATE(ASFCFEC(MTLOC));          ASFCFEC = 0.0
   ALLOCATE(ASFCPSF(MTLOC));          ASFCPSF = 0.0
   ALLOCATE(AFLXCSF(MTLOC));          AFLXCSF = 0.0
   ALLOCATE(AFLXNSF(MTLOC));          AFLXNSF = 0.0
   ALLOCATE(AFLXPSF(MTLOC));          AFLXPSF = 0.0
   ALLOCATE(ARPOCSF(MTLOC));          ARPOCSF = 0.0
   ALLOCATE(ARPONSF(MTLOC));          ARPONSF = 0.0
   ALLOCATE(ARPOPSF(MTLOC));          ARPOPSF = 0.0
   ALLOCATE(ASSISF(MTLOC));           ASSISF = 0.0
   ALLOCATE(ASSISASF(MTLOC));         ASSISASF = 0.0
   ALLOCATE(ASSISUSF(MTLOC));         ASSISUSF = 0.0
   ALLOCATE(ASSIPSF(MTLOC));          ASSIPSF = 0.0
   
   ALLOCATE(A_T(MTLOC,KBM1));              A_T = 0.0
   ALLOCATE(AP1(MTLOC,KBM1));              AP1 = 0.0
   ALLOCATE(ABM1(MTLOC,KBM1));             ABM1 = 0.0
   ALLOCATE(APR1(MTLOC,KBM1));             APR1 = 0.0
   ALLOCATE(AP2(MTLOC,KBM1));              AP2 = 0.0
   ALLOCATE(ABM2(MTLOC,KBM1));             ABM2 = 0.0
   ALLOCATE(APR2(MTLOC,KBM1));             APR2 = 0.0
   ALLOCATE(AP3(MTLOC,KBM1));              AP3 = 0.0
   ALLOCATE(ABM3(MTLOC,KBM1));             ABM3  = 0.0
   ALLOCATE(APR3(MTLOC,KBM1));             APR3 = 0.0
   ALLOCATE(AALGDOC(MTLOC,KBM1));          AALGDOC = 0.0
   ALLOCATE(AALGPOC(MTLOC,KBM1));          AALGPOC = 0.0
   ALLOCATE(ADENIT(MTLOC,KBM1));           ADENIT = 0.0
   ALLOCATE(AMNLDOC(MTLOC,KBM1));          AMNLDOC = 0.0
   ALLOCATE(AHDRPOC(MTLOC,KBM1));          AHDRPOC = 0.0
   ALLOCATE(AALGNH4(MTLOC,KBM1));          AALGNH4 = 0.0
   ALLOCATE(AALGNO3(MTLOC,KBM1));          AALGNO3 = 0.0
   ALLOCATE(AALGDON(MTLOC,KBM1));          AALGDON = 0.0
   ALLOCATE(AALGPON(MTLOC,KBM1));          AALGPON = 0.0
   ALLOCATE(ANT(MTLOC,KBM1));              ANT = 0.0
   ALLOCATE(ADENNO3(MTLOC,KBM1));          ADENNO3  = 0.0
   ALLOCATE(AMNLDON(MTLOC,KBM1));          AMNLDON = 0.0
   ALLOCATE(AHDRPON(MTLOC,KBM1));          AHDRPON = 0.0
   ALLOCATE(ANFIX(MTLOC,KBM1));            ANFIX = 0.0
   ALLOCATE(AALGPO4(MTLOC,KBM1));          AALGPO4 = 0.0
   ALLOCATE(AALGDOP(MTLOC,KBM1));          AALGDOP = 0.0
   ALLOCATE(AALGPOP(MTLOC,KBM1));          AALGPOP = 0.0
   ALLOCATE(AMNLDOP(MTLOC,KBM1));          AMNLDOP = 0.0
   ALLOCATE(AHDRPOP(MTLOC,KBM1));          AHDRPOP = 0.0
   ALLOCATE(APSD(MTLOC,KBM1));             APSD = 0.0
   ALLOCATE(ASAP(MTLOC,KBM1));             ASAP = 0.0
   ALLOCATE(AALGUP(MTLOC,KBM1));           AALGUP = 0.0
   ALLOCATE(AALGRES(MTLOC,KBM1));          AALGRES = 0.0
   ALLOCATE(ADO(MTLOC,KBM1));              ADO = 0.0
   ALLOCATE(ADORALG(MTLOC,KBM1));          ADORALG = 0.0
   ALLOCATE(ADOPR(MTLOC,KBM1));            ADOPR = 0.0
   ALLOCATE(ADCOD(MTLOC,KBM1));            ADCOD = 0.0
   ALLOCATE(ADDOC(MTLOC,KBM1));            ADDOC = 0.0
   ALLOCATE(ANITRIF(MTLOC,KBM1));          ANITRIF = 0.0

   ALLOCATE(APOCSAV(MTLOC));      APOCSAV = 0.0
   ALLOCATE(ADOCEPI(MTLOC));      ADOCEPI = 0.0
   ALLOCATE(APOCEPI(MTLOC));      APOCEPI = 0.0
   ALLOCATE(ASEDCSAV(MTLOC));     ASEDCSAV = 0.0
   ALLOCATE(ANH4SAVW(MTLOC));     ANH4SAVW = 0.0
   ALLOCATE(ANO3SAVW(MTLOC));     ANO3SAVW = 0.0
   ALLOCATE(ADONSAVW(MTLOC));     ADONSAVW = 0.0
   ALLOCATE(APONSAVW(MTLOC));     APONSAVW = 0.0
   ALLOCATE(ANH4EPI(MTLOC));      ANH4EPI = 0.0
   ALLOCATE(ANO3EPI(MTLOC));      ANO3EPI   = 0.0
   ALLOCATE(ADONEPI(MTLOC));      ADONEPI  = 0.0
   ALLOCATE(APONEPI(MTLOC));      APONEPI = 0.0
   ALLOCATE(ASEDNSAV(MTLOC));     ASEDNSAV = 0.0
   ALLOCATE(ASEDNH4SAV(MTLOC));   ASEDNH4SAV = 0.0
   ALLOCATE(APO4SAVW(MTLOC));     APO4SAVW = 0.0
   ALLOCATE(ADOPSAVW(MTLOC));     ADOPSAVW  = 0.0
   ALLOCATE(APOPSAVW(MTLOC));     APOPSAVW = 0.0
   ALLOCATE(APO4EPI(MTLOC));      APO4EPI   = 0.0
   ALLOCATE(ADOPEPI(MTLOC));      ADOPEPI = 0.0
   ALLOCATE(APOPEPI(MTLOC));      APOPEPI  = 0.0
   ALLOCATE(ASEDPSAV(MTLOC));     ASEDPSAV = 0.0
   ALLOCATE(ASEDPO4SAV(MTLOC));   ASEDPO4SAV = 0.0
   ALLOCATE(ADOSAV(MTLOC));       ADOSAV    = 0.0
   ALLOCATE(ADOEPI(MTLOC));       ADOEPI = 0.0
   ALLOCATE(ASEDDOSAV(MTLOC));    ASEDDOSAV = 0.0
   ALLOCATE(ABMLEAF(MTLOC));      ABMLEAF = 0.0
   ALLOCATE(ASLSH(MTLOC));        ASLSH = 0.0
   ALLOCATE(ABMTUBER(MTLOC));     ABMTUBER = 0.0
   ALLOCATE(ADOCSAV(MTLOC));      ADOCSAV = 0.0

   ALLOCATE(V1SINGLE(0:MTLOC,KBM1)); V1SINGLE = 0.0

   ALLOCATE(SBN(MTLOC));          SBN = 0  
   ALLOCATE(BBN(MTLOC));          BBN = 0
   ALLOCATE(HMSBV(MTLOC));        HMSBV = 0 
   ALLOCATE(SFA(MTLOC));          SFA = 0

   ALLOCATE(FLUXS(0:MTLOC,KBM1,13));         FLUXS = 0.0

   ALLOCATE(IWCMNB(MTLOC,KBM1));        IWCMNB = 0.0
   ALLOCATE(IWCMPB(MTLOC,KBM1));        IWCMPB = 0.0
   ALLOCATE(IWCMCB(MTLOC,KBM1));        IWCMCB = 0.0
   ALLOCATE(IWCMSB(MTLOC,KBM1));        IWCMSB = 0.0
   ALLOCATE(WCMNB(MTLOC,KBM1));         WCMNB = 0.0
   ALLOCATE(WCMPB(MTLOC,KBM1));         WCMPB = 0.0
   ALLOCATE(WCMCB(MTLOC,KBM1));         WCMCB = 0.0
   ALLOCATE(WCMSB(MTLOC,KBM1));         WCMSB = 0.0
   ALLOCATE(DLWCMNB(MTLOC,KBM1));       DLWCMNB = 0.0
   ALLOCATE(DLWCMPB(MTLOC,KBM1));       DLWCMPB = 0.0
   ALLOCATE(DLWCKMNB(MTLOC,KBM1));      DLWCKMNB = 0.0
   ALLOCATE(DLWCKMCB(MTLOC,KBM1));      DLWCKMCB = 0.0
   ALLOCATE(S1FLXNB(MTLOC,KBM1));       S1FLXNB = 0.0
   ALLOCATE(S1FLXPB(MTLOC,KBM1));       S1FLXPB = 0.0
   ALLOCATE(S1FLXCB(MTLOC,KBM1));       S1FLXCB = 0.0
   ALLOCATE(S2FLXNB(MTLOC,KBM1));       S2FLXNB = 0.0
   ALLOCATE(S2FLXPB(MTLOC,KBM1));       S2FLXPB = 0.0
   ALLOCATE(S2FLXCB(MTLOC,KBM1));       S2FLXCB = 0.0
   ALLOCATE(S3FLXNB(MTLOC,KBM1));       S3FLXNB = 0.0
   ALLOCATE(S3FLXPB(MTLOC,KBM1));       S3FLXPB = 0.0
   ALLOCATE(S3FLXCB(MTLOC,KBM1));       S3FLXCB = 0.0
   
   ALLOCATE(WSSBNET(MTLOC));            WSSBNET = 0.0
   ALLOCATE(WSLBNET(MTLOC));            WSLBNET = 0.0
   ALLOCATE(WSRBNET(MTLOC));            WSRBNET = 0.0
   ALLOCATE(WS1BNET(MTLOC));            WS1BNET = 0.0
   ALLOCATE(WS2BNET(MTLOC));            WS2BNET = 0.0
   ALLOCATE(WS3BNET(MTLOC));            WS3BNET = 0.0
   ALLOCATE(WSUBNET(MTLOC));            WSUBNET = 0.0
   
   ALLOCATE(FRPOP(MTLOC,3));            FRPOP = 0.0  
   ALLOCATE(FRPON(MTLOC,3));            FRPON = 0.0  
   ALLOCATE(FRPOC(MTLOC,3));            FRPOC = 0.0

   ALLOCATE(CPOP(MTLOC,3));             CPOP   = 0.0
   ALLOCATE(FLXPOP(MTLOC,3));           FLXPOP = 0.0 
   ALLOCATE(CPON(MTLOC,3));             CPON   = 0.0
   ALLOCATE(FLXPON(MTLOC,3));           FLXPON = 0.0
   ALLOCATE(CPOC(MTLOC,3));             CPOC   = 0.0
   ALLOCATE(FLXPOC(MTLOC,3));           FLXPOC = 0.0
   
   ALLOCATE(CPOS(MTLOC));               CPOS   = 0.0      
   ALLOCATE(CPIP(MTLOC));               CPIP   = 0.0      
   ALLOCATE(CNO3(MTLOC));               CNO3   = 0.0
   ALLOCATE(CNH4(MTLOC));               CNH4   = 0.0     
   ALLOCATE(FLXPOS(MTLOC));             FLXPOS = 0.0     
   ALLOCATE(CTEMP(MTLOC));              CTEMP  = 0.0
   ALLOCATE(CDTEMP(MTLOC));             CDTEMP = 0.0   
   ALLOCATE(BSVOL(MTLOC));              BSVOL  = 0.0     
   ALLOCATE(HSED(MTLOC));               HSED   = 0.0
   ALLOCATE(CCH4(MTLOC));               CCH4   = 0.0      
   ALLOCATE(CSO4(MTLOC));               CSO4   = 0.0     
   ALLOCATE(CHS(MTLOC));                CHS    = 0.0
   ALLOCATE(CSI(MTLOC));                CSI    = 0.0

   ALLOCATE(NH41TM1S(MTLOC));           NH41TM1S  = 0.0
   ALLOCATE(NH4T2TM1S(MTLOC));          NH4T2TM1S = 0.0
   ALLOCATE(NO31TM1S(MTLOC));           NO31TM1S  = 0.0
   ALLOCATE(NO3T2TM1S(MTLOC));          NO3T2TM1S = 0.0
   ALLOCATE(HS1TM1S(MTLOC));            HS1TM1S   = 0.0 
   ALLOCATE(HST2TM1S(MTLOC));           HST2TM1S  = 0.0
   ALLOCATE(SI1TM1S(MTLOC));            SI1TM1S   = 0.0 
   ALLOCATE(SIT2TM1S(MTLOC));           SIT2TM1S  = 0.0
   ALLOCATE(PO41TM1S(MTLOC));           PO41TM1S  = 0.0
   ALLOCATE(PO4T2TM1S(MTLOC));          PO4T2TM1S = 0.0

   ALLOCATE(AG3CFL(MTLOC));             AG3CFL = 0.0
   ALLOCATE(AG3NFL(MTLOC));             AG3NFL = 0.0
   ALLOCATE(AG3PFL(MTLOC));             AG3PFL = 0.0
   ALLOCATE(ASDTMP(MTLOC));             ASDTMP = 0.0

   ALLOCATE(SFLUXC(MTLOC));             SFLUXC  = 0.0
   ALLOCATE(SFLUXN(MTLOC));             SFLUXN  = 0.0
   ALLOCATE(SFLUXP(MTLOC));             SFLUXP  = 0.0
   ALLOCATE(SF_RPOC(MTLOC));            SF_RPOC = 0.0
   ALLOCATE(SF_RPON(MTLOC));            SF_RPON = 0.0
   ALLOCATE(SF_RPOP(MTLOC));            SF_RPOP = 0.0
   ALLOCATE(JNH4SF(MTLOC));             JNH4SF  = 0.0
   ALLOCATE(JPO4SF(MTLOC));             JPO4SF  = 0.0
   ALLOCATE(SODSF(MTLOC));              SODSF   = 0.0 
   ALLOCATE(JSASF(MTLOC));              JSASF   = 0.0  
   ALLOCATE(JSUSF(MTLOC));              JSUSF   = 0.0
   ALLOCATE(SF_SSI(MTLOC));             SF_SSI  = 0.0
   ALLOCATE(SF_SU(MTLOC));              SF_SU   = 0.0
   ALLOCATE(SF_SA(MTLOC));              SF_SA   = 0.0
   ALLOCATE(SF_PIP(MTLOC));             SF_PIP  = 0.0
   
   ALLOCATE(PPFWS(MTLOC));              PPFWS = 0.0
   ALLOCATE(PNFWS(MTLOC));              PNFWS = 0.0 
   ALLOCATE(PCFWS(MTLOC));              PCFWS = 0.0 
   ALLOCATE(PSFWS(MTLOC));              PSFWS = 0.0 
   ALLOCATE(SSFWS(MTLOC));              SSFWS = 0.0

   ALLOCATE(CH4T2TM1S(MTLOC));          CH4T2TM1S = 0.0
   ALLOCATE(SO4T2TM1S(MTLOC));          SO4T2TM1S = 0.0
   ALLOCATE(CH41TM1S(MTLOC));           CH41TM1S = 0.0      

   ALLOCATE(BURIALN(MTLOC));            BURIALN = 0.0  
   ALLOCATE(BURIALP(MTLOC));            BURIALP = 0.0  
   ALLOCATE(BURIALC(MTLOC));            BURIALC = 0.0
   ALLOCATE(DIAGENC(MTLOC));            DIAGENC = 0.0
     
   ALLOCATE(FIB(MTLOC));                FIB   = 0.0     
   ALLOCATE(NLB(MTLOC));                NLB   = 0.0     
   ALLOCATE(PLB(MTLOC));                PLB   = 0.0
   ALLOCATE(NPPB(MTLOC));               NPPB  = 0.0     
   ALLOCATE(BBM(MTLOC));                BBM   = 0.0   
   ALLOCATE(BLITE(MTLOC));              BLITE = 0.0
   ALLOCATE(BMB(MTLOC));                BMB   = 0.0    
   ALLOCATE(PB(MTLOC));                 PB    = 0.0      
   ALLOCATE(PRB(MTLOC));                PRB   = 0.0
   ALLOCATE(BANH4(MTLOC));              BANH4 = 0.0   
   ALLOCATE(BANO3(MTLOC));              BANO3 = 0.0   
   ALLOCATE(BAPO4(MTLOC));              BAPO4 = 0.0
   ALLOCATE(BADOC(MTLOC));              BADOC = 0.0    
   ALLOCATE(BADO(MTLOC));               BADO  = 0.0    
   ALLOCATE(BAPOC(MTLOC));              BAPOC = 0.0
   ALLOCATE(BAPON(MTLOC));              BAPON = 0.0    
   ALLOCATE(BAPOP(MTLOC));              BAPOP = 0.0
     
   ALLOCATE(DFEEDM1S(MTLOC));           DFEEDM1S = 0.0
   
   ALLOCATE(DF_GROW(MTLOC));            DF_GROW = 0.0
   ALLOCATE(DF_RESP(MTLOC));            DF_RESP = 0.0
   ALLOCATE(DF_PRED(MTLOC));            DF_PRED = 0.0
   ALLOCATE(DF_MORT(MTLOC));            DF_MORT = 0.0
   ALLOCATE(ADF_GROW(MTLOC));           ADF_GROW = 0.0
   ALLOCATE(ADF_RESP(MTLOC));           ADF_RESP = 0.0
   ALLOCATE(ADF_PRED(MTLOC));           ADF_PRED = 0.0
   ALLOCATE(ADF_MORT(MTLOC));           ADF_MORT = 0.0

   ALLOCATE(SFEED(MTLOC,NSSFP));        SFEED = 0.0 
 
   ALLOCATE(SEDTYPE(MTLOC,NSSFP));      SEDTYPE = 0

   ALLOCATE(AASRAT(MTLOC));             AASRAT = 0.0
   ALLOCATE(ACFIX(MTLOC));              ACFIX  = 0.0

   ALLOCATE(ALEAF(MTLOC,NSINC));        ALEAF   = 0.0   
   ALLOCATE(AEP(MTLOC,NSINC));          AEP     = 0.0
   ALLOCATE(AROOT(MTLOC,NSINC));        AROOT   = 0.0     
   ALLOCATE(AFIEP(MTLOC,NSINC));        AFIEP   = 0.0
   ALLOCATE(ANPPSAV(MTLOC,NSINC));      ANPPSAV = 0.0  
   ALLOCATE(ANPPEP(MTLOC,NSINC));       ANPPEP  = 0.0
   ALLOCATE(AWATATN(MTLOC,NSINC));      AWATATN = 0.0
   ALLOCATE(AEPATN(MTLOC,NSINC));       AEPATN  = 0.0  
   ALLOCATE(ATUBER(MTLOC,NSINC));       ATUBER  = 0.0
   ALLOCATE(ASTEM(MTLOC,NSINC));        ASTEM   = 0.0   
   ALLOCATE(AFISH(MTLOC,NSINC));        AFISH   = 0.0
   ALLOCATE(APLEAF(MTLOC,NSINC));       APLEAF  = 0.0
   
   ALLOCATE(ANLSAV(MTLOC));             ANLSAV = 0.0       
   ALLOCATE(APLSAV(MTLOC));             APLSAV = 0.0
   ALLOCATE(AFHS(MTLOC));               AFHS   = 0.0         
   ALLOCATE(AFNSED(MTLOC));             AFNSED = 0.0
   ALLOCATE(AFPSED(MTLOC));             AFPSED = 0.0          
   ALLOCATE(ANLEPI(MTLOC));             ANLEPI = 0.0
   ALLOCATE(APLEPI(MTLOC));             APLEPI = 0.0

   ALLOCATE(BENDOCB(MTLOC));            BENDOCB = 0.0
   ALLOCATE(BENNH4B(MTLOC));            BENNH4B = 0.0
   ALLOCATE(BENNO3B(MTLOC));            BENNO3B = 0.0
   ALLOCATE(BENPO4B(MTLOC));            BENPO4B = 0.0
   ALLOCATE(BENCODB(MTLOC));            BENCODB = 0.0
   ALLOCATE(BENDOB(MTLOC));             BENDOB  = 0.0
   ALLOCATE(BENSAB(MTLOC));             BENSAB  = 0.0

   ALLOCATE(NSAVDPH(MTLOC));            NSAVDPH = 0.0
   ALLOCATE(SAVWDH(MTLOC,NSINC));       SAVWDH  = 0.0
   ALLOCATE(SAVDPH(MTLOC,NSINC));       SAVDPH  = 0.0
   ALLOCATE(SAVFRAC(MTLOC,NSINC));      SAVFRAC = 0.0
   ALLOCATE(SAVCELL(MTLOC));            SAVCELL = 0

   ALLOCATE(EPATN(MTLOC,NSINC));        EPATN  = 0.0
   ALLOCATE(FISH(MTLOC,NSINC));         FISH   = 0.0
   ALLOCATE(PLEAF(MTLOC,NSINC));        PLEAF  = 0.0
   ALLOCATE(SAVATN(MTLOC,NSINC));       SAVATN = 0.0
   ALLOCATE(NPPSAV(MTLOC,NSINC));       NPPSAV = 0.0
   ALLOCATE(NPPEPI(MTLOC,NSINC));       NPPEPI = 0.0
   ALLOCATE(LEAF(MTLOC,NSINC));         LEAF   = 0.0
   ALLOCATE(ROOT(MTLOC,NSINC));         ROOT   = 0.0
   ALLOCATE(EP(MTLOC,NSINC));           EP     = 0.0
   ALLOCATE(FIEP(MTLOC,NSINC));         FIEP   = 0.0
   ALLOCATE(WATATN(MTLOC,NSINC));       WATATN = 0.0
   ALLOCATE(STEM(MTLOC,NSINC));         STEM   = 0.0
   ALLOCATE(TUBER(MTLOC,NSINC));        TUBER  = 0.0
   
   ALLOCATE(DOSAV(MTLOC));              DOSAV   = 0.0
   ALLOCATE(LDOCSAV(MTLOC));            LDOCSAV = 0.0
   ALLOCATE(LPOCSAV(MTLOC));            LPOCSAV = 0.0
   ALLOCATE(RPOCSAV(MTLOC));            RPOCSAV = 0.0
   ALLOCATE(DOEPI(MTLOC));              DOEPI   = 0.0
   ALLOCATE(LDOCEPI(MTLOC));            LDOCEPI = 0.0
   ALLOCATE(LPOCEPI(MTLOC));            LPOCEPI = 0.0
   ALLOCATE(RPOCEPI(MTLOC));            RPOCEPI = 0.0
   ALLOCATE(FHS(MTLOC));                FHS     = 0.0
   ALLOCATE(RDOCSAV(MTLOC));            RDOCSAV = 0.0
   ALLOCATE(RDOCEPI(MTLOC));            RDOCEPI = 0.0
   ALLOCATE(DGRDAYS(MTLOC));            DGRDAYS = 0.0

   ALLOCATE(BMLEAF(MTLOC));             BMLEAF    = 0.0
   ALLOCATE(BMSTEM(MTLOC));             BMSTEM    = 0.0
   ALLOCATE(BMROOT(MTLOC));             BMROOT    = 0.0
   ALLOCATE(BMTUBER(MTLOC));            BMTUBER   = 0.0
   ALLOCATE(NLSAV(MTLOC));              NLSAV     = 0.0
   ALLOCATE(FNSEDSAV(MTLOC));           FNSEDSAV  = 0.0
   ALLOCATE(PLSAV(MTLOC));              PLSAV     = 0.0
   ALLOCATE(FPSEDSAV(MTLOC));           FPSEDSAV  = 0.0
   ALLOCATE(NH4SAVW(MTLOC));            NH4SAVW   = 0.0
   ALLOCATE(NO3SAVW(MTLOC));            NO3SAVW   = 0.0
   ALLOCATE(LDONSAVW(MTLOC));           LDONSAVW  = 0.0 
   ALLOCATE(LPONSAVW(MTLOC));           LPONSAVW  = 0.0
   ALLOCATE(RPONSAVW(MTLOC));           RPONSAVW  = 0.0
   ALLOCATE(PO4SAVW(MTLOC));            PO4SAVW   = 0.0
   ALLOCATE(LDOPSAVW(MTLOC));           LDOPSAVW  = 0.0
   ALLOCATE(LPOPSAVW(MTLOC));           LPOPSAVW  = 0.0
   ALLOCATE(RPOPSAVW(MTLOC));           RPOPSAVW  = 0.0
   ALLOCATE(RDONSAVW(MTLOC));           RDONSAVW  = 0.0
   ALLOCATE(RDOPSAVW(MTLOC));           RDOPSAVW  = 0.0
   ALLOCATE(NLEPI(MTLOC));              NLEPI     = 0.0
   ALLOCATE(PLEPI(MTLOC));              PLEPI     = 0.0
   ALLOCATE(NH4EPI(MTLOC));             NH4EPI    = 0.0
   ALLOCATE(NO3EPI(MTLOC));             NO3EPI    = 0.0
   ALLOCATE(LDONEPI(MTLOC));            LDONEPI   = 0.0
   ALLOCATE(LPONEPI(MTLOC));            LPONEPI   = 0.0
   ALLOCATE(RPONEPI(MTLOC));            RPONEPI   = 0.0
   ALLOCATE(PO4EPI(MTLOC));             PO4EPI    = 0.0
   ALLOCATE(LDOPEPI(MTLOC));            LDOPEPI   = 0.0
   ALLOCATE(LPOPEPI(MTLOC));            LPOPEPI   = 0.0
   ALLOCATE(RPOPEPI(MTLOC));            RPOPEPI   = 0.0
   ALLOCATE(RDONEPI(MTLOC));            RDONEPI   = 0.0
   ALLOCATE(RDOPEPI(MTLOC));            RDOPEPI   = 0.0
   ALLOCATE(SAVAREA(MTLOC));            SAVAREA   = 0.0
   ALLOCATE(SEDCSAV(MTLOC));            SEDCSAV   = 0.0
   ALLOCATE(SEDDOSAV(MTLOC));           SEDDOSAV  = 0.0
   ALLOCATE(SEDNH4SAV(MTLOC));          SEDNH4SAV = 0.0
   ALLOCATE(SEDPO4SAV(MTLOC));          SEDPO4SAV = 0.0
   ALLOCATE(SEDNSAV(MTLOC));            SEDNSAV   = 0.0
   ALLOCATE(SEDPSAV(MTLOC));            SEDPSAV   = 0.0
   
   ALLOCATE(PEP(MTLOC,NSINC));          PEP  = 0.0 
   ALLOCATE(BMEP(MTLOC,NSINC));         BMEP = 0.0
   ALLOCATE(PREP(MTLOC,NSINC));         PREP = 0.0

   ALLOCATE(SFFILTCT(MTLOC,NSSFP));     SFFILTCT = 0.0
   ALLOCATE(SFRESPCT(MTLOC,NSSFP));     SFRESPCT = 0.0
   ALLOCATE(SFPREDCT(MTLOC,NSSFP));     SFPREDCT = 0.0
   ALLOCATE(SFRATEI(MTLOC,NSSFP));      SFRATEI  = 0.0
   ALLOCATE(SFGCMAX(MTLOC,NSSFP));      SFGCMAX  = 0.0
   ALLOCATE(SFGMIN(MTLOC,NSSFP));       SFGMIN   = 0.0
   ALLOCATE(SFGCING(MTLOC,NSSFP));      SFGCING  = 0.0
   ALLOCATE(SFCFECES(MTLOC,NSSFP));     SFCFECES = 0.0
   ALLOCATE(SFNFECES(MTLOC,NSSFP));     SFNFECES = 0.0
   ALLOCATE(SFPFECES(MTLOC,NSSFP));     SFPFECES = 0.0
   ALLOCATE(SFCPSFEC(MTLOC,NSSFP));     SFCPSFEC = 0.0
   ALLOCATE(SFNPSFEC(MTLOC,NSSFP));     SFNPSFEC = 0.0
   ALLOCATE(SFPPSFEC(MTLOC,NSSFP));     SFPPSFEC = 0.0

   ALLOCATE(SF_SFGC(MTLOC,NSSFP));      SF_SFGC   = 0.0
   ALLOCATE(SF_RESP(MTLOC,NSSFP));      SF_RESP   = 0.0
   ALLOCATE(SF_PRED(MTLOC,NSSFP));      SF_PRED   = 0.0
   ALLOCATE(SF_RMORT(MTLOC,NSSFP));     SF_RMORT  = 0.0
   ALLOCATE(ASF_SFGC(MTLOC,NSSFP));     ASF_SFGC  = 0.0
   ALLOCATE(ASF_RESP(MTLOC,NSSFP));     ASF_RESP  = 0.0
   ALLOCATE(ASF_PRED(MTLOC,NSSFP));     ASF_PRED  = 0.0
   ALLOCATE(ASF_RMORT(MTLOC,NSSFP));    ASF_RMORT = 0.0

   ALLOCATE(ATMFLXNB(MTLOC));           ATMFLXNB    = 0.0  
   ALLOCATE(ATMFLXPB(MTLOC));           ATMFLXPB    = 0.0 
   ALLOCATE(ATMFLXCB(MTLOC));           ATMFLXCB    = 0.0
   ALLOCATE(BENFLXPNB(MTLOC));          BENFLXPNB   = 0.0  
   ALLOCATE(BENFLXDNB(MTLOC));          BENFLXDNB   = 0.0
   ALLOCATE(BENFLXPPB(MTLOC));          BENFLXPPB   = 0.0
   ALLOCATE(BENFLXDPB(MTLOC));          BENFLXDPB   = 0.0
   ALLOCATE(BENFLXPCB(MTLOC));          BENFLXPCB   = 0.0
   ALLOCATE(DLSEDKNB(MTLOC));           DLSEDKNB    = 0.0
   ALLOCATE(DLSEDKCB(MTLOC));           DLSEDKCB    = 0.0 
   ALLOCATE(BURIALFLXNB(MTLOC));        BURIALFLXNB = 0.0
   ALLOCATE(BURIALFLXPB(MTLOC));        BURIALFLXPB = 0.0
   ALLOCATE(BURIALFLXCB(MTLOC));        BURIALFLXCB = 0.0

   ALLOCATE(APEP(MTLOC,NSINC));         APEP  = 0.0
   ALLOCATE(ABMEP(MTLOC,NSINC));        ABMEP = 0.0
   ALLOCATE(APREP(MTLOC,NSINC));        APREP = 0.0

   RETURN
   END SUBROUTINE ALLOC_WQM
				   
   END MODULE WQM

     MODULE WQM_INIT
     USE SIZES
     USE WQM
     IMPLICIT NONE
     SAVE
!***** Variable declarations

     INTEGER :: RSODP, DLTDP, SNPDP, TFLDP, PLTDP, APLDP,   &
                OPLDP,    DIADP
     INTEGER :: COURB,    COURFS,   COURBS,   F,            &
                SB,       S1LNMAX,  S2LNMAX,  S3LNMAX,  DIFFFS, DIFFBS  
     REAL    :: NXSNP,    NXPLT,    NXAPL,    NXTFL,    NXKFL,  NXTVD,   &
                NXOPL,    NXMBL,    NXDIA,    NO3T2I,   NH4T2I,          &
                LEAFI,    STEMI,    ROOTI,    TUBERI

     REAL    :: MAXDLT,   MXDLT,    INTKE,    INITKE

     REAL(8) :: TM1,      TM2,      TM3,   MINSTEP,  NXKEI
     CHARACTER(LEN=3) :: SNPC, RSIC, RSOC, BCC, S1C,           &
               S2C, MDC*3, PLTC, FLC,           &
               MBLC,   BFC,    VBC,    QPLTC,  XYDFC,  XYDFU,  &
               ZDFC,   ICOC,   ATMC,   SAVLC,  SEDC,          &
               AUTOC,  SPLTC,  TFLC,   DIAC,   STLC,          &
               APLTC,  KFLC,   OPLC,   BFOC,   S3C,           &
               SAVMC,  SAVPLTC,DFLC,   SFLC,   LOXC
     CHARACTER EXT1*1,   EXT2*2,   EXT3*3,   EXT4*4
     CHARACTER(LEN=8) :: SLC,    HYDC,   BNDTC,  CONSC,  ICIC   
     CHARACTER(LEN=8) :: TIMEA,  TIMEB,  SPVARM, PRINTM
     CHARACTER(LEN=72) :: TITLE(6), OLDTITLE(6), FILENAME
     CHARACTER(LEN=72) :: MAPFN, GEOFN, ICIFN, RSIFN,                  &
               AGRFN, STLFN, MRLFN, KFLFN, ICOFN
     CHARACTER(LEN=72) :: SNPFN, PLTFN, APLFN, DIAFN, TFLFN,        &
               RSOFN, OPLFN, MBLFN, SUSFN, SUDFN
     LOGICAL   RESTART_OUT,      SNAPSHOTS,  END_RUN,                   &
               MODIFY_ICONC,     VOLUME_BALANCE,                        &
               QUICKEST,         UPWIND,           ICOND_OUT,           &
               UNI_ICON_IN,      BIN_ICON_IN,                           &
               AUTO_STEPPING,    STOP_RUN,                              &
               PLOTS,            OXYGEN_PLOTS,     ZOO_CALC,            &
               NEW_VOLUMES
     LOGICAL   TEMPERATURE_CALC, ALGAE_CALC,                            &
               CARBON_CALC,      NITROGEN_CALC,    PHOSPHORUS_CALC,     &
               COD_CALC,         OXYGEN_CALC,      SILICA_CALC
     LOGICAL   LEFT_FLOWB(NHQP), RIGHT_FLOWB(NHQP),              &
               LEFTM1_BOUNDARY(NHQP), RIGHTP1_BOUNDARY(NHQP)      

!***** Dimension declarations

     REAL,DIMENSION(NHQP,2) :: DEN1, DEN2, DEN3,         &
                               TP1, TP2, TP3,          &
                               T2,      SF2
     REAL,DIMENSION(NHQP) :: T1, T3, SF1
     REAL,DIMENSION(NHQP) :: TERM1, TERM2, TERM3,          &
                             GRAD1, GRAD2, GRAD3,          &
                             COUR
    
     REAL :: GRAD(NHQP,3),TERM(NHQP,3),IFLOWP(NBCP),IBT(NHQP),  &
               COURBSV(NHQP),COURVSV(NHQP)
     EQUIVALENCE (GRAD1,GRAD(1,1)),(GRAD2,GRAD(1,2)),(GRAD3,GRAD(1,3))
     EQUIVALENCE (TERM1,TERM(1,1)),(TERM2,TERM(1,2)),(TERM3,TERM(1,3))

!     REAL(8) :: CR1(0:NLP,NSBP), CR2(0:NLP,NSBP),  GAMFM1(NLP,NSBP),     &
!               GAMF(NLP,NSBP),  WDILB(NLP,NSBP),  WDIB(NLP,NSBP),       &
!               WAILB(NLP,NSBP), WAIB(NLP,NSBP)
!     REAL :: DIFFS(NQFP)
     
!     REAL(8),DIMENSION(NLP,NSBP) :: AT,    BT,     CT
!     REAL(8),DIMENSION(NLP,NCP) :: DT,     GAMMA,   BTA
!     REAL(8) :: VT(NBP,NCP)

!     REAL,DIMENSION(NBP,NCP) :: C1MIN,   C1MAX
     REAL,ALLOCATABLE,DIMENSION(:,:,:) :: C1MIN,   C1MAX

!     REAL,DIMENSION(NBP) :: V2VB, DLBV
     REAL,ALLOCATABLE :: DOVDAYS(:,:,:)
     REAL :: OINT(NOIP)
     CHARACTER(LEN=3) :: ACC(NCP)
     REAL :: CIC(NCP),                              &
             COUT(NCP),       CPOPI(3),                              &
             CPONI(3),        CPOCI(3),         SFEEDI(10)
     REAL :: DLTD(NDP),       SNPD(NDP),        RSOD(NDP),           &
             SNPF(NDP),       DLTVAL(NDP),      DLTMAX(NDP),         &
             DLTFTN(NDP),     PLTD(NDP),        PLTF(NDP),           &
             APLTD(NDP),      APLF(NDP),        TFLD(NDP),           &
             TFLF(NDP),       KFLD(NDP),        KFLF(NDP),           &
             OPLD(NDP),       OPLF(NDP),        DIAD(NDP),           &
             DIAF(NDP),       MBLD(NDP),        MBLF(NDP)
     
!***** Data declarations
     REAL,PARAMETER :: DOCLIT  = 0.0, LPOCLIT = 0.0,          &
                           RPOCLIT = 0.0, PBSLIT  = 0.0,          &
                           DSILLIT = 0.0

   CONTAINS
   
   SUBROUTINE ALLOC_WQM_INIT
   USE ALL_VARS
   IMPLICIT NONE
   
   ALLOCATE(C1MIN(MTLOC,KBM1,NCP));     C1MIN = 0.0   
   ALLOCATE(C1MAX(MTLOC,KBM1,NCP));     C1MAX = 0.0

   ALLOCATE(DOVDAYS(MTLOC,KBM1,NOIP));  DOVDAYS = 0.0

   PHOSPHORUS_CALC = .FALSE.
   
   RETURN
   END SUBROUTINE ALLOC_WQM_INIT

     END MODULE WQM_INIT
