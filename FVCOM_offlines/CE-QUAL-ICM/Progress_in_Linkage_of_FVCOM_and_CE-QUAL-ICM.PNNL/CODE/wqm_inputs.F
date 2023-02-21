   SUBROUTINE INPUTS()
   USE FILE_INFO
   USE ALL_VARS
   USE WQM
   USE WQM_INIT
   USE ALGAL
   USE SED
#  if defined (MULTIPROCESSOR)
   USE MOD_PAR
#  endif

   IMPLICIT NONE
   INTEGER :: J, JF, JG, JC, K, L, N
   INTEGER :: I, II, ISF, JP, JNP
   INTEGER :: JS1, JS2, JS3
   INTEGER :: YEARSTRT
   CHARACTER(LEN=100) :: NCFILE
   CHARACTER(LEN=4) :: YEAR
   REAL(SP), ALLOCATABLE :: FTEMP(:)      !,FTEMP2(:)
   REAL(SP), ALLOCATABLE :: RTMP(:,:,:)
   REAL(SP), ALLOCATABLE :: RTMP2(:,:),RTMP21(:,:),RTMP22(:,:),     &
                        RTMP23(:,:),RTMP24(:,:),RTMP25(:,:),RTMP26(:,:)
   REAL(SP), ALLOCATABLE :: CTEMP_GL(:),CPOP_GL(:,:),CPON_GL(:,:),         &
                        CPOC_GL(:,:),CPOS_GL(:),PO4T2TM1S_GL(:),       &
                        NH4T2TM1S_GL(:),NO3T2TM1S_GL(:),HST2TM1S_GL(:), &
                        CH4T2TM1S_GL(:),CH41TM1S_GL(:),SO4T2TM1S_GL(:), &
                        SIT2TM1S_GL(:),BENSTR1S_GL(:),BBM_GL(:)
   REAL(SP), ALLOCATABLE :: SEDTYPE_GL(:,:),SFEED_GL(:,:),            &
                        DFEEDM1S_GL(:)


!************************************************************************
!**                              Inputs                                **
!************************************************************************

!***** Open control file

   OPEN (CON,FILE=CONFN,STATUS='OLD')

!***** Title cards

   READ(CON,1000)
   READ(CON,1010) (TITLE(J),J=1,6)

!***** Grid setup -- TYKIM -- NOBTY (open boundary numbers)

   READ (CON,1020)  MGL,NGL,KB,NOBTY

!***** Time control -- TYKIM -- DTI (FVCOM internal time steps)

   READ (CON,1031)  TMSTRT,TMEND,YEARSTRT,DTI
   READ (CON,1040)  NDLT
   READ (CON,1030) (DLTD(J),J=1,NDLT)
   READ (CON,1030) (DLTVAL(J),J=1,NDLT)
   READ (CON,1030) (DLTMAX(J),J=1,NDLT)
   READ (CON,1030) (DLTFTN(J),J=1,NDLT)

!TYKIM
! FILGTH IS LENGTH (IN DAYS) OF PERIOD SPANNED BY MET AND OTHER INPUT
! AHMDLT IS FVCOM output interval
   READ (CON,1030)  AHMDLT, FILGTH

!--SELECT NETCDF FILE TO READ FOR GRID CONSTRUCTION
   IF(MSR)WRITE(YEAR,'(I4.4)') YEARSTRT
   NCFILE ="../input_netcdf/"//TRIM(CASENAME)//"_0001.nc"
  
!--DETERMINE NUMBER OF ELEMENTS AND NODES IN THE MODEL
   CALL NCD_READ_GRID(NCFILE)
   IF(MSR)WRITE(*,*)  '!  # OF NODES            :',MGL
   IF(MSR)WRITE(*,*)  '!  # OF ELEMENTS         :',NGL
   IF(MSR)WRITE(*,*)  '!  # OF SIGMA LEVELS     :',KB     !KBM1
   IF(MSR)WRITE(*,*)
   IF(MSR)WRITE(*,*)  '!      MESH READING      :    FINISHED'

   CALL NCD_READ_SHAPE(NCFILE)

!
!--DECOMPOSE DOMAIN BY ELEMENTS USING METIS
!
#  if defined (MULTIPROCESSOR) 
   ALLOCATE(EL_PID(NGL))  ;  EL_PID = 1
   IF(PAR)CALL DOMDEC(NGL,NPROCS,EL_PID,MSR)
#  endif 

!
!--GENERATE GLOBAL<==>LOCAL ELEMENT/NODE MAPPING
!
#  if defined (MULTIPROCESSOR)
   IF(PAR)CALL GENMAP
#  endif

!
!--MAP OPEN BOUNDARY CONDITION NODES TO LOCAL DOMAIN
!
   CALL BCMAP

!
!--INPUT AND SETUP BOUNDARY FORCING (HEAT/RIVERS/WIND/etc)
!
   CALL BCS_FORCE

    CALL ALLOC_WQM
    CALL ALLOC_WQM_INIT
!
!--SHIFT GRID/CORIOLIS/BATHYMETRY TO LOCAL DOMAIN
!
   CALL PDOMDEC
!
!--SET UP GRID METRICS (FLUX EDGES/CONTROL VOLUMES/ETC)
!
   CALL TRIANGLE_GRID_EDGE      !Set up fluxes and control Volumes
   CALL CELL_AREA               !Calculate Element and Control Volume Areas

!--SET ISBCE AND ISONB CORRECTLY IN HALO CELLS/NODES
#  if defined (MULTIPROCESSOR)
   ALLOCATE(FTEMP(0:NTLOC)) ; FTEMP = ISBCE
   IF(PAR)CALL EXCHANGE(EC,NTLOC,1,MYID,NPROCS,FTEMP)
   ISBCE = FTEMP
   DEALLOCATE(FTEMP)
   ALLOCATE(FTEMP(0:MTLOC)) ; FTEMP = ISONB
   IF(PAR)CALL EXCHANGE(NC,MTLOC,1,MYID,NPROCS,FTEMP)
   ISONB = FTEMP
   DEALLOCATE(FTEMP)
#  endif

!
!--EXCHANGE SHAPE FACTOR INFORMATION
!
#  if defined (MULTIPROCESSOR)
!   IF(PAR)CALL EXCHANGE(EC,NTLOC,4,MYID,NPROCS,A1U,A2U)
!   IF(PAR)CALL EXCHANGE(EC,NTLOC,3,MYID,NPROCS,AWX,AWY,AW0)
!  IF(PAR)CALL EXCHANGE(EC,NTLOC,1,MYID,NPROCS,ALPHA) 
   IF(PAR)CALL EXCHANGE(EC,NTLOC,1,MYID,NPROCS,ART)
#  endif

!***** Output control

   READ (CON,1050)  SNPC,NSNP
   READ (CON,1030) (SNPD(J),J=1,NSNP)
   READ (CON,1030) (SNPF(J),J=1,NSNP)
   READ (CON,1055)  PLTC,QPLTC,SPLTC,SAVPLTC,NPLT
   READ (CON,1030) (PLTD(J),J=1,NPLT)
   READ (CON,1030) (PLTF(J),J=1,NPLT)
   READ (CON,1050)  APLTC,NAPL
   READ (CON,1030) (APLTD(J),J=1,NAPL)
   READ (CON,1030) (APLF(J),J=1,NAPL)
   READ (CON,1050)  TFLC,NTFL
   READ (CON,1030) (TFLD(J),J=1,NTFL)
   READ (CON,1030) (TFLF(J),J=1,NTFL)
   READ (CON,1052)  KFLC,NKFL
   READ (CON,1030) (KFLD(J),J=1,NKFL)
   READ (CON,1030) (KFLF(J),J=1,NKFL)
   READ (CON,1052)  OPLC,NOPL,NOINT
   READ (CON,1030) (OINT(J),J=1,NOINT)
   READ (CON,1030) (OPLD(J),J=1,NOPL)
   READ (CON,1030) (OPLF(J),J=1,NOPL)
   READ (CON,1050)  MBLC,NMBL
   READ (CON,1030) (MBLD(J),J=1,NMBL)
   READ (CON,1030) (MBLF(J),J=1,NMBL)
   READ (CON,1050)  DIAC,NDIA
   READ (CON,1030) (DIAD(J),J=1,NDIA)
   READ (CON,1030) (DIAF(J),J=1,NDIA)
   READ (CON,1050)  RSOC,NRSO,RSIC
   READ (CON,1030) (RSOD(J),J=1,NRSO)

!***** Hydrodynamic solution scheme

   READ (CON,1060)  SLC, CONSC, TH, MINSTEP

!***** Controls

   READ (CON,1071)  SEDC, AUTOC, VBC, BFOC, STLC, ICIC, ICOC, SAVMC

! suspension and deposit feeders

   READ (CON,1071)  SFLC, DFLC, LOXC

!***** Dead sea case

   READ (CON,1070)  FLC,XYDFC,ZDFC

!***** Dispersion

   READ (CON,1030)  XYDF,ZDFMUL,ZDFBCK

!***** Dispersion type (closure (ON) or constant (OFF))

   READ (CON,1070)  XYDFU

!***** Constituent control cards

   READ (CON,1070)  BCC,    S1C,    S2C,    S3C,  MDC,    BFC,  ATMC,   SAVLC
   READ (CON,1080)  REDS1C, REDS1N, REDS1P, REDS2C, REDS2N, REDS2P,     &
                    REDS3C, REDS3N, REDS3P
   READ (CON,1080)  REDCBC, REDCBN, REDCBP
   READ (CON,1065)  BNDTC
   READ (CON,1070) (ACC(JC),JC=1,NCP)

!***** Input filenames

   READ (CON,1020)  NHYDF,NTVDF
   READ (CON,1090)  MAPFN
   READ (CON,1090)  GEOFN
   READ (CON,1090)  ICIFN
   READ (CON,1090)  AGRFN
   READ (CON,1090)  ZOOFN
   READ (CON,1090)  SUSFN
   READ (CON,1090)  STLFN
   READ (CON,1090)  MRLFN                            !MNOEL 2/20/93
   READ (CON,1090)  KEIFN
   READ (CON,1090) (METFN(J),J=1,NTVDF)
   READ (CON,1090) (S1FN(J),J=1,NTVDF)
   READ (CON,1090) (S2FN(J),J=1,NTVDF)
   READ (CON,1090) (S3FN(J),J=1,NTVDF)
   READ (CON,1090) (ATMFN(J),J=1,NTVDF)
   READ (CON,1090) (SVIFN(J),J=1,NTVDF)
   READ (CON,1090) (BFIFN(J),J=1,NTVDF)
   READ (CON,1090) (CBCFN(J),J=1,NTVDF)            !JLM 2/23/01

!***** Output filenames

   READ (CON,1090)  ICOFN                   !MNOEL  2-5-93
   READ (CON,1090)  SNPFN
   READ (CON,1090)  RSOFN
   READ (CON,1090)  PLTFN
   READ (CON,1090)  APLFN
   READ (CON,1090)  DIAFN
   READ (CON,1090)  TFLFN
   READ (CON,1090)  KFLFN
   READ (CON,1090)  OPLFN
   READ (CON,1090)  MBLFN
   READ (CON,1090)  ALOFN
   READ (CON,1090)  ZFOFN
   READ (CON,1090)  BFOFN
   READ (CON,1090)  SVOFN
   READ (CON,1090)  SUDFN
   CLOSE (CON)

!***** Initialize I/O logical control variables

   BOUNDARY_CONC    = BCC == ' ON'
   MODIFY_ICONC     = MDC == ' ON'
   SOURCE_ONE       = S1C == ' ON'           !MNOEL   1-25-93
   BENTHIC_FLUXES   = BFC == ' ON'
   SOURCE_TWO       = S2C == ' ON'           !MNOEL   1-25-93
   SOURCE_THR       = S3C == ' ON'           !MNOEL   1-25-93
   ATMOS_LOADS      = ATMC == ' ON'
   SAV_LOADS        = SAVLC == ' ON'
   SAV_CALC         = SAVMC == ' ON'
   SETTLING         = STLC == ' ON'
   ICOND_OUT        = ICOC == ' ON'              !MNOEL 2-5-93
   SEDIMENT_CALC    = SEDC == ' ON'
   KINETIC_FLUXES   = KFLC == ' ON'
   LIGHT_EXTINCTION = ((ACC(4) == ' ON').OR.(ACC(5) == ' ON')         &
                      .OR.(ACC(6) == ' ON'))
   ZOO_CALC         = ((ACC(7) == ' ON').OR.(ACC(8) == ' ON'))
   UNI_ICON_IN      = ICIC == ' UNIFORM'              !MNOEL 2-5-93
   BIN_ICON_IN      = ICIC == '  BINARY'              !MNOEL 2-5-93
! deposit and suspension feeder control

   DEPFEED          = DFLC == ' ON'
   SFEEDER          = SFLC == ' ON'
   HYPOXFX          = LOXC == ' ON'

!***** Active Constituents

   NAC = 0
   DO JC=1,NCP
     IF (ACC(JC) == ' ON') THEN
       NAC     = NAC+1
       AC(NAC) = JC
     END IF
   END DO

!***** Open remaining input files

   HYDPTR = 1
   METPTR = 1
   CBCPTR = 1
   S1PTR  = 1
   S2PTR  = 1
   S3PTR  = 1
   BFIPTR = 1
   KEIPTR = 1
   ATMPTR = 1
   SAVPTR = 1
        
   OPEN (MET,FILE=METFN(METPTR),STATUS='OLD')
   IF (SETTLING)         OPEN (STL,FILE=STLFN,STATUS='OLD')
   IF (LIGHT_EXTINCTION) OPEN (KEI,FILE=KEIFN,STATUS='OLD')
   IF (LIGHT_EXTINCTION) OPEN (AGR,FILE=AGRFN,STATUS='OLD')
   IF (SEDIMENT_CALC.OR.BENTHIC_FLUXES)                      &
                         OPEN (BFI,FILE=BFIFN(BFIPTR),STATUS='OLD')
   IF (BOUNDARY_CONC)    OPEN (CBC,FILE=CBCFN(CBCPTR),STATUS='OLD') 
   IF (SOURCE_ONE)       OPEN (S1, FILE=S1FN(S1PTR),  STATUS='OLD')
   IF (SOURCE_TWO)       OPEN (S2, FILE=S2FN(S2PTR),  STATUS='OLD')
   IF (SOURCE_THR)       OPEN (S3, FILE=S3FN(S3PTR),  STATUS='OLD')
   IF (SAV_LOADS.OR.SAV_CALC)                                &
                         OPEN (SVI,FILE=SVIFN(SAVPTR),STATUS='OLD')
   IF (ATMOS_LOADS)      OPEN (ATM,FILE=ATMFN(ATMPTR),STATUS='OLD')

! Tykim - Array for FVCOM grid and results

  ALLOCATE(UNC1(0:NTLOC,KB));  UNC1  = 0.0
  ALLOCATE(VNC1(0:NTLOC,KB));  VNC1  = 0.0
  ALLOCATE(WNC1(0:MTLOC,KB));  WNC1  = 0.0
  ALLOCATE(WTSNC1(0:MTLOC,KB));  WTSNC1  = 0.0
  ALLOCATE(UARD_OBCNNC1(0:NOBTY+1))   ;  UARD_OBCNNC1  = 0.0
  ALLOCATE(XFLUX_OBCNC1(0:NOBTY,KBM1));  XFLUX_OBCNC1  = 0.0
  ALLOCATE(DTFANC1(0:MTLOC))   ;  DTFANC1  = 0.0
  ALLOCATE(KHNC1(0:MTLOC,KB)); KHNC1 = 0.0
  ALLOCATE(TNC1(0:MTLOC,KB));  TNC1 = 0.0
  ALLOCATE(SNC1(0:MTLOC,KB));  SNC1 = 0.0
  ALLOCATE(ELNC1(0:MTLOC)); ELNC1 = 0.0

  ALLOCATE(UNC2(0:NTLOC,KB));  UNC2 = 0.0
  ALLOCATE(VNC2(0:NTLOC,KB));  VNC2 = 0.0
  ALLOCATE(WNC2(0:MTLOC,KB));  WNC2  = 0.0
  ALLOCATE(WTSNC2(0:MTLOC,KB));  WTSNC2  = 0.0
  ALLOCATE(UARD_OBCNNC2(0:NOBTY+1))   ;  UARD_OBCNNC2  = 0.0
  ALLOCATE(XFLUX_OBCNC2(0:NOBTY,KBM1));  XFLUX_OBCNC2  = 0.0
  ALLOCATE(DTFANC2(0:MTLOC))   ;  DTFANC2  = 0.0
  ALLOCATE(KHNC2(0:MTLOC,KB)); KHNC2 = 0.0
  ALLOCATE(TNC2(0:MTLOC,KB));  TNC2 = 0.0
  ALLOCATE(SNC2(0:MTLOC,KB));  SNC2 = 0.0
  ALLOCATE(ELNC2(0:MTLOC)); ELNC2 = 0.0

  ALLOCATE(UU(0:NTLOC,KB));  UU  = 0.0
  ALLOCATE(VV(0:NTLOC,KB));  VV  = 0.0
  ALLOCATE(WTS(0:MTLOC,KB));  WTS  = 0.0
  ALLOCATE(UARD_OBCN(0:NOBTY+1))   ;  UARD_OBCN  = 0.0
  ALLOCATE(XFLUX_OBC(0:NOBTY,KBM1));  XFLUX_OBC  = 0.0
  ALLOCATE(KH(0:MTLOC,KB)); KH = 0.0
  ALLOCATE(EL(0:MTLOC)); EL = 0.0
  ALLOCATE(ET(0:MTLOC)); ET = 0.0
  ALLOCATE(D(0:MTLOC)); D = 0.0
  ALLOCATE(DT(0:MTLOC)); DT = 0.0
  ALLOCATE(DTFA(0:MTLOC)); DTFA = 0.0
  ALLOCATE(DTFAT(0:MTLOC)); DTFAT = 0.0
  ALLOCATE(VISCOFH(0:NTLOC,KB));  VISCOFH  = 0.0
  ALLOCATE(UUT(0:NTLOC,KB));  UUT  = 0.0
  ALLOCATE(VVT(0:NTLOC,KB));  VVT  = 0.0
  ALLOCATE(WTST(0:MTLOC,KB));  WTST  = 0.0
  ALLOCATE(UARD_OBCNT(0:NOBTY+1))   ;  UARD_OBCNT  = 0.0
  ALLOCATE(XFLUX_OBCT(0:NOBTY,KBM1));  XFLUX_OBCT  = 0.0
  ALLOCATE(DT1(0:NTLOC)); DT1 = 0.0
  

! Tykim FVCOM netcdf outfile is daily base  
  DAY = DAY + 1
  WRITE(FILENUMBER,'(I4.4)') DAY

  PREFF ="../input_netcdf/"//TRIM(CASENAME)//"_"
  NCFILE = TRIM(PREFF)//TRIM(FILENUMBER)//'.nc'

  CALL NCD_READ(NCFILE,UNC1,VNC1,WTSNC1,UARD_OBCNNC1,XFLUX_OBCNC1,DTFANC1,KHNC1,ELNC1,TNC1,SNC1,TYHOUR1)

  TYHOUR1=TYHOUR1+1

  UUT=UNC1
  VVT=VNC1
  WTST=WTSNC1
  UARD_OBCNT=UARD_OBCNNC1
  XFLUX_OBCT=XFLUX_OBCNC1
  DTFAT=DTFANC1
  ET=ELNC1
  DT=H+ET

  DO I=1,NGL
    DT1(I)=DT(NV(I,1))+DT(NV(I,2))+DT(NV(I,3))
    DT1(I)=DT1(I)/3.0
  END DO
    
!--HOURLY READING OF VELOCITY FIELDS IN NETCDF FILE
  WRITE(FILENUMBER,'(I4.4)') DAY
  NCFILE = TRIM(PREFF)//TRIM(FILENUMBER)//'.nc'
  CALL NCD_READ(NCFILE,UNC2,VNC2,WTSNC2,UARD_OBCNNC2,XFLUX_OBCNC2,DTFANC2,KHNC2,ELNC2,TNC2,SNC2,TYHOUR1)

  TYHOUR1=TYHOUR1+1     
   IF( MOD(TYHOUR1-1,(86400/INT(AHMDLT))) == 0)then
    TYHOUR1=1
    DAY=DAY+1
  ENDIF

   NXHYD = (DAY-1)*86400+TYHOUR1*INT(AHMDLT)-INT(AHMDLT)
!***** Suspension feeders

     IF (SFEEDER) THEN
       OPEN (SUS,FILE=SUSFN,STATUS='OLD')
       READ(SUS,1032)
       READ(SUS,1170) NSPECIES    ! # of susp. feeders species to be modeled
       READ(SUS,1005) FRSASF      ! Recycle fraction of grazed diatom Si
       DO N=1,NSPECIES
         READ (SUS,1032)
         READ (SUS,1003) FILT(N),SFA1(N),SFA2(N),SFA3(N),                &
               SFA4(N),SFA5(N),MAXING(N),SFCN(N) 
         READ (SUS,1002) SFCP(N),SFRESP(N),SFPRED(N),SFTMN(N),           &
               THTAFILT(N),THTARESP(N),THTAPRED(N)
         READ (SUS,1003) XKPO2(N),SFTD(N),FILTFACT(N),RESPFACT(N),       &
               SFDOh(N),SFDOq(N),SFATURB(N),SFBTURB(N)
!MBM 971021 added turbidity sensitivity parameters ^^^^

       END DO

       ALLOCATE(SEDTYPE_GL(MGL,NCP));       SEDTYPE_GL = 0.0
       ALLOCATE(SFEED_GL(MGL,NCP));         SFEED_GL   = 0.0
       READ(SUS,1032)
       DO B=1,MGL
         READ (SUS,1001) NUMBOX, (SEDTYPE_GL(B,N),SFEED_GL(B,N), N=1,NSPECIES)
       END DO
       
       IF(SERIAL)THEN
         SEDTYPE = SEDTYPE_GL
         SFEED   = SFEED_GL
       END IF
       
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO N = 1,NSPECIES
           DO B = 1, MLOC
             SEDTYPE(B,N) = SEDTYPE_GL(NGID(B),N)
             SFEED(B,N)   = SFEED_GL(NGID(B),N)
           END DO
           DO B=1,NHN
             SEDTYPE(B+MLOC,N) = SEDTYPE_GL(HN_LST(B),N)
             SFEED(B+MLOC,N)   = SFEED_GL(HN_LST(B),N)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(SEDTYPE_GL,SFEED_GL)
       
       CLOSE (SUS)
     END IF

! initialize arrays that are passed in case susp. feeder model not used

     SFLUXC  = 0.0
     SF_RPOC = 0.0
     SFLUXN  = 0.0
     SF_RPON = 0.0
     SFLUXP  = 0.0
     SF_RPOP = 0.0
     JNH4SF  = 0.0
     JPO4SF  = 0.0
     SODSF   = 0.0
     JSUSF   = 0.0
     JSASF   = 0.0
     SF_SSI  = 0.0
     SF_SU   = 0.0
     SF_SA   = 0.0
     SF_PIP  = 0.0

!************************************************************************
!**                   Parameters for SAV Submodel                      **
!************************************************************************

!JQI     IF (SAV_CALC)      CALL SAV_READ


!******* Initial conditions 

!******* Uniform constituent initial concentrations

     IF (UNI_ICON_IN) THEN                              !MNOEL 2-5-93 
       OPEN (ICI,FILE=ICIFN,STATUS='OLD')
       READ(ICI,*)
       READ(ICI,*) (CIC(JC),JC=1,NCP)
       write(*,*)'initial',(CIC(JC),JC=1,NCP)
       IF (SEDIMENT_CALC) THEN
         READ (ICI,1030) CTEMPI
         READ (ICI,1030) (CPOPI(JG),JG=1,3)
         READ (ICI,1030) (CPONI(JG),JG=1,3)
         READ (ICI,1030) (CPOCI(JG),JG=1,3)
         READ (ICI,1030) CPOSI, PO4T2I, NH4T2I, NO3T2I  
         READ (ICI,1030) HST2I, CH4T2I, CH41TI, SO4T2I, SIT2I, BENSTI
         READ (ICI,1030) BBMI 
       END IF

       IF (DEPFEED) READ (ICI,1030) DFEEDI
       IF (SFEEDER) READ (ICI,1030) (SFEEDI(N), N=1,NSPECIES) 
       IF (SAV_CALC) READ (ICI,1030) LEAFI, STEMI, ROOTI, TUBERI, EPI
 
!***** Constituent concentrations 

       DO JC=1,NAC
         DO K=1,KBM1
           DO I=1,MLOC
             C1(I,K,AC(JC))    = CIC(AC(JC))
             C2(I,K,AC(JC))    = CIC(AC(JC))
             C1MIN(I,K,AC(JC)) = 1.E10
             C1MAX(I,K,AC(JC)) = 0.
	   END DO  
         END DO
       END DO	 

       IF (SEDIMENT_CALC) THEN
         DO I=1,MLOC
           CTEMP(I)      = CTEMPI
           DO JG=1,3
             CPOP(I,JG)  = CPOPI(JG)
             CPON(I,JG)  = CPONI(JG)
             CPOC(I,JG)  = CPOCI(JG)
           END DO
           BBM(I)        = BBMI
           DO N=1,NSPECIES
             SFEED(I,N)      = SFEEDI(N)
           END DO
           DFEEDM1S(I)   = DFEEDI
           CPOS(I)       = CPOSI
           PO4T2TM1S(I)  = PO4T2I
           NH4T2TM1S(I)  = NH4T2I
           NO3T2TM1S(I)  = NO3T2I
           HST2TM1S(I)   = HST2I
           CH4T2TM1S(I)  = CH4T2I
           CH41TM1S(I)   = CH41TI
           SO4T2TM1S(I)  = SO4T2I
           SIT2TM1S(I)   = SIT2I
           BENSTR1S(I)   = BENSTI
         END DO
       END IF

!******* Binary constituent initial concentrations

     ELSE IF (BIN_ICON_IN) THEN                              !MNOEL 2-5-93
       OPEN (ICI,FILE=ICIFN,STATUS='OLD',FORM='UNFORMATTED') 
       READ (ICI) OLDTITLE

       ALLOCATE(RTMP(0:MGL,KBM1,NCP));   RTMP = 0.0
       READ (ICI) (((RTMP(I,K,JC),I=0,MGL),K=1,KBM1),JC=1,NCP)

       IF(SERIAL) C1 = RTMP
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO JC = 1,NCP
	   DO K=1,KBM1
	     DO I=1,MLOC
	       C1(I,K,JC) = RTMP(NGID(I),K,JC)
	     END DO
	     DO I=1,NHN
	       C1(I+MLOC,K,JC) = RTMP(HN_LST(I),K,JC)
	     END DO
	   END DO
	 END DO
       END IF
#      endif       	     
       DEALLOCATE(RTMP)
    
       DO JC=1,NAC
         DO K=1,KBM1
           DO I=1,MLOC
             C1(I,K,AC(JC)) = MAX(C1(I,K,AC(JC)),0.0)     
             C2(I,K,AC(JC)) = C1(I,K,AC(JC))
	   END DO  
         END DO
       END DO	 

       IF (SEDIMENT_CALC) THEN
         ALLOCATE(CTEMP_GL(MGL));     CTEMP_GL     = 0.0
	 ALLOCATE(CPOP_GL(MGL,3));    CPOP_GL      = 0.0
	 ALLOCATE(CPON_GL(MGL,3));    CPON_GL      = 0.0
	 ALLOCATE(CPOC_GL(MGL,3));    CPOC_GL      = 0.0
	 ALLOCATE(CPOS_GL(MGL));      CPOS_GL      = 0.0
         ALLOCATE(PO4T2TM1S_GL(MGL)); PO4T2TM1S_GL = 0.0
	 ALLOCATE(NH4T2TM1S_GL(MGL)); NH4T2TM1S_GL = 0.0
         ALLOCATE(NO3T2TM1S_GL(MGL)); NO3T2TM1S_GL = 0.0
	 ALLOCATE(HST2TM1S_GL(MGL));  HST2TM1S_GL  = 0.0
         ALLOCATE(CH4T2TM1S_GL(MGL)); CH4T2TM1S_GL = 0.0
	 ALLOCATE(CH41TM1S_GL(MGL));  CH41TM1S_GL  = 0.0
         ALLOCATE(SO4T2TM1S_GL(MGL)); SO4T2TM1S_GL = 0.0
	 ALLOCATE(SIT2TM1S_GL(MGL));  SIT2TM1S_GL  = 0.0
         ALLOCATE(BENSTR1S_GL(MGL));  BENSTR1S_GL  = 0.0
	 ALLOCATE(BBM_GL(MGL));       BBM_GL       = 0.0
	 
         READ (ICI) (CTEMP_GL(I),I=1,MGL),                         &
              ((CPOP_GL(I,J),I=1,MGL),J=1,3),                      &
              ((CPON_GL(I,J),I=1,MGL),J=1,3),                      &
              ((CPOC_GL(I,J),I=1,MGL),J=1,3),                      &
              (CPOS_GL(I),I=1,MGL),                                &
              (PO4T2TM1S_GL(I),I=1,MGL), (NH4T2TM1S_GL(I),I=1,MGL),   &
              (NO3T2TM1S_GL(I),I=1,MGL), (HST2TM1S_GL(I),I=1,MGL),    &
              (CH4T2TM1S_GL(I),I=1,MGL), (CH41TM1S_GL(I),I=1,MGL),    &
              (SO4T2TM1S_GL(I),I=1,MGL), (SIT2TM1S_GL(I),I=1,MGL),    &
              (BENSTR1S_GL(I),I=1,MGL),  (BBM_GL(I),I=1,MGL)
	      

         IF(SERIAL)THEN 
           CTEMP     = CTEMP_GL    
	   CPOP      = CPOP_GL     
	   CPON      = CPON_GL     
	   CPOC      = CPOC_GL     
	   CPOS      = CPOS_GL     
           PO4T2TM1S = PO4T2TM1S_GL
	   NH4T2TM1S = NH4T2TM1S_GL
           NO3T2TM1S = NO3T2TM1S_GL
	   HST2TM1S  = HST2TM1S_GL 
           CH4T2TM1S = CH4T2TM1S_GL
	   CH41TM1S  = CH41TM1S_GL 
           SO4T2TM1S = SO4T2TM1S_GL
	   SIT2TM1S  = SIT2TM1S_GL 
           BENSTR1S  = BENSTR1S_GL 
	   BBM       = BBM_GL      
	 END IF  

#        if defined (MULTIPROCESSOR)
         IF(PAR)THEN
           DO I=1,MLOC
             CTEMP(I)     = CTEMP_GL(NGID(I))    
	     CPOP(I,:)    = CPOP_GL(NGID(I),:)     
	     CPON(I,:)    = CPON_GL(NGID(I),:)     
	     CPOC(I,:)    = CPOC_GL(NGID(I),:)     
	     CPOS(I)      = CPOS_GL(NGID(I))     
             PO4T2TM1S(I) = PO4T2TM1S_GL(NGID(I))
	     NH4T2TM1S(I) = NH4T2TM1S_GL(NGID(I))
             NO3T2TM1S(I) = NO3T2TM1S_GL(NGID(I))
	     HST2TM1S(I)  = HST2TM1S_GL(NGID(I))
             CH4T2TM1S(I) = CH4T2TM1S_GL(NGID(I))
	     CH41TM1S(I)  = CH41TM1S_GL(NGID(I)) 
             SO4T2TM1S(I) = SO4T2TM1S_GL(NGID(I))
	     SIT2TM1S(I)  = SIT2TM1S_GL(NGID(I)) 
             BENSTR1S(I)  = BENSTR1S_GL(NGID(I)) 
	     BBM(I)       = BBM_GL(NGID(I))      
           END DO
           DO I=1,NHN
             CTEMP(I+MLOC)     = CTEMP_GL(HN_LST(I))    
	     CPOP(I+MLOC,:)    = CPOP_GL(HN_LST(I),:)     
	     CPON(I+MLOC,:)    = CPON_GL(HN_LST(I),:)     
	     CPOC(I+MLOC,:)    = CPOC_GL(HN_LST(I),:)     
	     CPOS(I+MLOC)      = CPOS_GL(HN_LST(I))     
             PO4T2TM1S(I+MLOC) = PO4T2TM1S_GL(HN_LST(I))
	     NH4T2TM1S(I+MLOC) = NH4T2TM1S_GL(HN_LST(I))
             NO3T2TM1S(I+MLOC) = NO3T2TM1S_GL(HN_LST(I))
	     HST2TM1S(I+MLOC)  = HST2TM1S_GL(HN_LST(I)) 
             CH4T2TM1S(I+MLOC) = CH4T2TM1S_GL(HN_LST(I))
	     CH41TM1S(I+MLOC)  = CH41TM1S_GL(HN_LST(I)) 
             SO4T2TM1S(I+MLOC) = SO4T2TM1S_GL(HN_LST(I))
	     SIT2TM1S(I+MLOC)  = SIT2TM1S_GL(HN_LST(I)) 
             BENSTR1S(I+MLOC)  = BENSTR1S_GL(HN_LST(I)) 
	     BBM(I+MLOC)       = BBM_GL(HN_LST(I))      
           END DO
         END IF
#        endif

         DEALLOCATE(CTEMP_GL,CPOP_GL,CPON_GL,CPOC_GL,CPOS_GL,PO4T2TM1S_GL)
	 DEALLOCATE(NH4T2TM1S_GL,NO3T2TM1S_GL,HST2TM1S_GL,CH4T2TM1S_GL)
	 DEALLOCATE(CH41TM1S_GL,SO4T2TM1S_GL,SIT2TM1S_GL,BENSTR1S_GL,BBM_GL)
	      
       END IF

       IF (DEPFEED)THEN
         ALLOCATE(DFEEDM1S_GL(MGL));      DFEEDM1S_GL = 0.0
         READ (ICI) (DFEEDM1S_GL(I),I=1,MGL)
         IF(SERIAL) DFEEDM1S = DFEEDM1S_GL
#        if defined (MULTIPROCESSOR)
         IF(PAR)THEN
           DO I=1,MLOC
             DFEEDM1S(I) = DFEEDM1S_GL(NGID(I))
           END DO
           DO I=1,NHN
             DFEEDM1S(I+MLOC) = DFEEDM1S_GL(HN_LST(I))
           END DO
         END IF
#        endif
         DEALLOCATE(DFEEDM1S_GL)
       END IF
       IF (SFEEDER)THEN
         ALLOCATE(SFEED_GL(MGL,NCP));    SFEED_GL = 0.0
         READ (ICI) ((SFEED_GL(I,K),I=1,MGL),K=1,NSPECIES)
         IF(SERIAL) SFEED = SFEED_GL
#        if defined (MULTIPROCESSOR)
         IF(PAR)THEN
           DO K = 1,NSPECIES
             DO I = 1,MLOC
               SFEED(I,K) = SFEED_GL(NGID(I),K)
             END DO
             DO I=1,NHN
               SFEED(I+MLOC,K) = SFEED_GL(HN_LST(I),K)
             END DO
           END DO
         END IF
#        endif
         DEALLOCATE(SFEED_GL)
       END IF

       CLOSE (ICI)

     ELSE
       WRITE(*,*) 'initial conditions file specified incorrectly'
       STOP
     END IF

!***** Mineralization rates

     OPEN (MRL,FILE=MRLFN,STATUS='OLD')                        !MNOEL 2-20-93
     READ (MRL,1032)
      
!***** Spatially-invariant kinetics parameters

     READ (MRL,1080)  KHONT,  KHNNT,  KHOCOD, KHODOC, KHNDN
     READ (MRL,1080)  AOCR,   AONT,   KHCOAG
     READ (MRL,1080)  TRCOD,  TRMNL,  TRHDR,  TRSUA
     READ (MRL,1080)  KTCOD,  KTMNL,  KTHDR,  KTSUA
     READ (MRL,1080)  KTNT1,  KTNT2,  TMNT
     READ (MRL,1080)  KADPO4, KADSA
     READ (MRL,1080)  AANOX,  ANDC                             !MNOEL  2-20-93
     READ (MRL,1080)  AREAR,  BREAR,  CREAR

!***** Spatially-varying kinetics parameters

     READ (MRL,1060) SPVARM, PRINTM
     IF (SPVARM == 'CONSTANT') THEN
       READ(MRL,1033) KLDC(1,1)
       DO K=1,KBM1
         DO I=1,MLOC
           KLDC(I,K)=KLDC(1,1)
	 END DO  
       END DO
     ELSE
       ALLOCATE(RTMP2(MGL,KBM1));   RTMP2 = 0.0
       DO K=1,KBM1
         READ (MRL,1033) (RTMP2(I,K),I=1,MGL)
       END DO
       IF(SERIAL)KLDC = RTMP2
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO K=1,KBM1
           DO I=1,MLOC
             KLDC(I,K) = RTMP2(NGID(I),K)
           END DO
           DO I=1,NHN
             KLDC(I+MLOC,K) = RTMP2(HN_LST(I),K)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(RTMP2)
     END IF
     READ (MRL,1060) SPVARM, PRINTM
     IF (SPVARM == 'CONSTANT') THEN
       READ(MRL,1033) KRDC(1,1)
       DO K=1,KBM1
         DO I=1,MLOC
           KRDC(I,K)=KRDC(1,1)
	 END DO  
       END DO
     ELSE
       ALLOCATE(RTMP2(MGL,KBM1));    RTMP2 = 0.0
       DO K=1,KBM1
         READ (MRL,1033) (RTMP2(I,K),I=1,MGL)
       END DO	 
       IF(SERIAL) KRDC = RTMP2
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO K=1,KBM1
           DO I=1,MLOC
             KRDC(I,K) = RTMP2(NGID(I),K)
           END DO
           DO I=1,NHN
             KRDC(I+MLOC,K) = RTMP2(HN_LST(I),K)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(RTMP2)
     END IF
     READ (MRL,1060) SPVARM, PRINTM
     IF (SPVARM == 'CONSTANT') THEN
       READ(MRL,1033) KLPC(1,1)
       DO K=1,KBM1
         DO I=1,MTLOC
           KLPC(I,K)=KLPC(1,1)
	 END DO
       END DO
     ELSE
       ALLOCATE(RTMP2(MGL,KBM1));    RTMP2 = 0.0
       DO K=1,KBM1
         READ (MRL,1033) (RTMP2(I,K),I=1,MGL)
       END DO
       IF(SERIAL) KLPC = RTMP2
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO K=1,KBM1
           DO I=1,MLOC
             KLPC(I,K) = RTMP2(NGID(I),K)
           END DO
           DO I=1,NHN
             KLPC(I+MLOC,K) = RTMP2(HN_LST(I),K)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(RTMP2)
     END IF
     READ (MRL,1060) SPVARM, PRINTM
     IF (SPVARM == 'CONSTANT') THEN
       READ(MRL,1033) KRPC(1,1)
       DO K=1,KBM1
         DO I=1,MTLOC
           KRPC(I,K)=KRPC(1,1)
	 END DO
       END DO
     ELSE
       ALLOCATE(RTMP2(MGL,KBM1));    RTMP2 = 0.0
       DO K=1,KBM1
         READ (MRL,1033) (RTMP2(I,K),I=1,MGL)
       END DO
       IF(SERIAL) KRPC = RTMP2
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO K=1,KBM1
           DO I=1,MLOC
             KRPC(I,K) = RTMP2(NGID(I),K)
           END DO
           DO I=1,NHN
             KRPC(I+MLOC,K) = RTMP2(HN_LST(I),K)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(RTMP2)
     END IF
     READ (MRL,1060) SPVARM, PRINTM
     IF (SPVARM == 'CONSTANT') THEN
       READ(MRL,1033) KLDN(1,1)
       DO K=1,KBM1
         DO I=1,MTLOC
           KLDN(I,K)=KLDN(1,1)
	 END DO  
       END DO
     ELSE
       ALLOCATE(RTMP2(MGL,KBM1));      RTMP2 = 0.0
       DO K=1,KBM1
         READ (MRL,1033) (RTMP2(I,K),I=1,MGL)
       END DO
       IF(SERIAL) KLDN = RTMP2
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO K=1,KBM1
           DO I=1,MLOC
             KLDN(I,K) = RTMP2(NGID(I),K)
           END DO
           DO I=1,NHN
             KLDN(I+MLOC,K) = RTMP2(HN_LST(I),K)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(RTMP2)
     END IF
     READ (MRL,1060) SPVARM, PRINTM
     IF (SPVARM == 'CONSTANT') THEN
       READ(MRL,1033) KRDN(1,1)
       DO K=1,KBM1
         DO I=1,MTLOC
           KRDN(I,K)=KRDN(1,1)
	 END DO  
       END DO
     ELSE
       ALLOCATE(RTMP2(MGL,KBM1));     RTMP2 = 0.0
       DO K=1,KBM1
         READ (MRL,1033) (RTMP2(I,K),I=1,MGL)
       END DO	 
       IF(SERIAL) KRDN = RTMP2
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO K=1,KBM1
           DO I=1,MLOC
             KRDN(I,K) = RTMP2(NGID(I),K)
           END DO
           DO I=1,NHN
             KRDN(I+MLOC,K) = RTMP2(HN_LST(I),K)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(RTMP2)
     END IF
     READ (MRL,1060) SPVARM, PRINTM
     IF (SPVARM == 'CONSTANT') THEN
       READ(MRL,1033) KLPN(1,1)
       DO K=1,KBM1
         DO I=1,MTLOC
           KLPN(I,K)=KLPN(1,1)
	 END DO  
       END DO
     ELSE
       ALLOCATE(RTMP2(MGL,KBM1));     RTMP2 = 0.0
       DO K=1,KBM1
         READ (MRL,1033) (RTMP2(I,K),I=1,MGL)
       END DO	 
       IF(SERIAL) KLPN = RTMP2
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO K=1,KBM1
           DO I=1,MLOC
             KLPN(I,K) = RTMP2(NGID(I),K)
           END DO
           DO I=1,NHN
             KLPN(I+MLOC,K) = RTMP2(HN_LST(I),K)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(RTMP2)
     END IF
     READ (MRL,1060) SPVARM, PRINTM
     IF (SPVARM == 'CONSTANT') THEN
       READ(MRL,1033) KRPN(1,1)
       DO K=1,KBM1
         DO I=1,MTLOC
           KRPN(I,K)=KRPN(1,1)
	 END DO  
       END DO
     ELSE
       ALLOCATE(RTMP2(MGL,KBM1));     RTMP2 = 0.0
       DO K=1,KBM1
         READ (MRL,1033) (RTMP2(I,K),I=1,MGL)
       END DO
       IF(SERIAL) KRPN = RTMP2
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO K=1,KBM1
           DO I=1,MLOC
             KRPN(I,K) = RTMP2(NGID(I),K)
           END DO
           DO I=1,NHN
             KRPN(I+MLOC,K) = RTMP2(HN_LST(I),K)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(RTMP2)
     END IF
     READ (MRL,1060) SPVARM, PRINTM
     IF (SPVARM == 'CONSTANT') THEN
       READ(MRL,1033) KLDP(1,1)
       DO K=1,KBM1
         DO I=1,MTLOC
	   KLDP(I,K)=KLDP(1,1)
	 END DO  
       END DO
     ELSE
       ALLOCATE(RTMP2(MGL,KBM1));     RTMP2 = 0.0
       DO K=1,KBM1
         READ (MRL,1033) (RTMP2(I,K),I=1,MGL)
       END DO
       IF(SERIAL) KLDP = RTMP2
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO K=1,KBM1
           DO I=1,MLOC
             KLDP(I,K) = RTMP2(NGID(I),K)
           END DO
           DO I=1,NHN
             KLDP(I+MLOC,K) = RTMP2(HN_LST(I),K)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(RTMP2)
     END IF
     READ (MRL,1060) SPVARM, PRINTM
     IF (SPVARM == 'CONSTANT') THEN
       READ(MRL,1033) KRDP(1,1)
       DO K=1,KBM1
         DO I=1,MTLOC
           KRDP(I,K)=KRDP(1,1)
	 END DO  
       END DO
     ELSE
       ALLOCATE(RTMP2(MGL,KBM1));    RTMP2 = 0.0
       DO K=1,KBM1
         READ (MRL,1033) (RTMP2(I,K),I=1,MGL)
       END DO	 
       IF(SERIAL) KRDP = RTMP2
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO K=1,KBM1
           DO I=1,MLOC
             KRDP(I,K) = RTMP2(NGID(I),K)
           END DO
           DO I=1,NHN
             KRDP(I+MLOC,K) = RTMP2(HN_LST(I),K)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(RTMP2)
     END IF
     READ (MRL,1060) SPVARM, PRINTM
     IF (SPVARM == 'CONSTANT') THEN
       READ(MRL,1033) KLPP(1,1)
       DO K=1,KBM1
         DO I=1,MTLOC
           KLPP(I,K)=KLPP(1,1)
	 END DO  
       END DO
     ELSE
       ALLOCATE(RTMP2(MGL,KBM1)); RTMP2 = 0.0
       DO K=1,KBM1
         READ (MRL,1033) (RTMP2(I,K),I=1,MGL)
       END DO	 
       IF(SERIAL) KLPP = RTMP2
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO K=1,KBM1
           DO I=1,MLOC
             KLPP(I,K) = RTMP2(NGID(I),K)
           END DO
           DO I=1,NHN
             KLPP(I+MLOC,K) = RTMP2(HN_LST(I),K)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(RTMP2)
     END IF
     READ (MRL,1060) SPVARM, PRINTM
     IF (SPVARM == 'CONSTANT') THEN
       READ(MRL,1033) KRPP(1,1)
       DO K=1,KBM1
         DO I=1,MTLOC
           KRPP(I,K)=KRPP(1,1)
	 END DO  
       END DO
     ELSE
       ALLOCATE(RTMP2(MGL,KBM1));    RTMP2 = 0.0
       DO K=1,KBM1
         READ (MRL,1033) (RTMP2(I,K),I=1,MGL)
       END DO	 
       IF(SERIAL) KRPP = RTMP2
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO K=1,KBM1
           DO I=1,MLOC
             KRPP(I,K) = RTMP2(NGID(I),K)
           END DO
           DO I=1,NHN
             KRPP(I+MLOC,K) = RTMP2(HN_LST(I),K)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(RTMP2)
     END IF
     READ (MRL,1060) SPVARM, PRINTM
     IF (SPVARM == 'CONSTANT') THEN
       READ(MRL,1033) KSUA(1,1)
       DO K=1,KBM1
         DO I=1,MTLOC
           KSUA(I,K)=KSUA(1,1)
	 END DO  
       END DO
     ELSE
       ALLOCATE(RTMP2(MGL,KBM1));    RTMP2 = 0.0
       DO K=1,KBM1
         READ (MRL,1033) (RTMP2(I,K),I=1,MGL)
       END DO	 
       IF(SERIAL) KSUA = RTMP2
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO K=1,KBM1
           DO I=1,MLOC
             KSUA(I,K) = RTMP2(NGID(I),K)
           END DO
           DO I=1,NHN
             KSUA(I+MLOC,K) = RTMP2(HN_LST(I),K)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(RTMP2)
     END IF
     READ (MRL,1060) SPVARM, PRINTM
     IF (SPVARM == 'CONSTANT') THEN
       READ(MRL,1033) KCOD(1,1)
       DO K=1,KBM1
         DO I=1,MTLOC
           KCOD(I,K)=KCOD(1,1)
	 END DO  
       END DO
     ELSE
       ALLOCATE(RTMP2(MGL,KBM1));     RTMP2 = 0.0
       DO K=1,KBM1
         READ (MRL,1033) (RTMP2(I,K),I=1,MGL)
       END DO	 
       IF(SERIAL) KCOD = RTMP2
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO K=1,KBM1
           DO I=1,MLOC
             KCOD(I,K) = RTMP2(NGID(I),K)
           END DO
           DO I=1,NHN
             KCOD(I+MLOC,K) = RTMP2(HN_LST(I),K)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(RTMP2)
     END IF
     READ (MRL,1060) SPVARM, PRINTM
     IF (SPVARM == 'CONSTANT') THEN
       READ(MRL,1033) KDCALG(1,1)
       DO K=1,KBM1
         DO I=1,MTLOC
           KDCALG(I,K)=KDCALG(1,1)
	 END DO  
       END DO
     ELSE
       ALLOCATE(RTMP2(MGL,KBM1));     RTMP2 = 0.0
       DO K=1,KBM1
         READ (MRL,1033) (RTMP2(I,K),I=1,MGL)
       END DO	 
       IF(SERIAL) KDCALG = RTMP2
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO K=1,KBM1
           DO I=1,MLOC
             KDCALG(I,K) = RTMP2(NGID(I),K)
           END DO
           DO I=1,NHN
             KDCALG(I+MLOC,K) = RTMP2(HN_LST(I),K)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(RTMP2)
     END IF
     READ (MRL,1060) SPVARM, PRINTM
     IF (SPVARM == 'CONSTANT') THEN
       READ(MRL,1033) KLCALG(1,1)
       DO K=1,KBM1
         DO I=1,MTLOC
           KLCALG(I,K)=KLCALG(1,1)
	 END DO  
       END DO
     ELSE
       ALLOCATE(RTMP2(MGL,KBM1));     RTMP2 = 0.0
       DO K=1,KBM1
         READ (MRL,1033) (RTMP2(I,K),I=1,MGL)
       END DO	 
       IF(SERIAL) KLCALG = RTMP2
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO K=1,KBM1
           DO I=1,MLOC
             KLCALG(I,K) = RTMP2(NGID(I),K)
           END DO
           DO I=1,NHN
             KLCALG(I+MLOC,K) = RTMP2(HN_LST(I),K)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(RTMP2)
     END IF
     READ (MRL,1060) SPVARM, PRINTM
     IF (SPVARM .EQ. 'CONSTANT') THEN
       READ(MRL,1033) KRCOAG(1,1)
       DO K=1,KBM1
         DO I=1,MTLOC
           KRCOAG(I,K)=KRCOAG(1,1)
	 END DO  
       END DO
     ELSE
       ALLOCATE(RTMP2(MGL,KBM1));     RTMP2 = 0.0
       DO K=1,KBM1
         READ (MRL,1033) (RTMP2(I,K),I=1,MGL)
       END DO	 
       IF(SERIAL) KRCOAG = RTMP2
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO K=1,KBM1
           DO I=1,MLOC
             KRCOAG(I,K) = RTMP2(NGID(I),K)
           END DO
           DO I=1,NHN
             KRCOAG(I+MLOC,K) = RTMP2(HN_LST(I),K)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(RTMP2)
     END IF
     READ (MRL,1060) SPVARM, PRINTM
     IF (SPVARM == 'CONSTANT') THEN
       READ(MRL,1033) KDNALG(1,1)
       DO K=1,KBM1
         DO I=1,MTLOC
           KDNALG(I,K)=KDNALG(1,1)
	 END DO  
       END DO
     ELSE
       ALLOCATE(RTMP2(MGL,KBM1));     RTMP2 = 0.0
       DO K=1,KBM1
         READ (MRL,1033) (RTMP2(I,K),I=1,MGL)
       END DO	 
       IF(SERIAL) KDNALG = RTMP2
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO K=1,KBM1
           DO I=1,MLOC
             KDNALG(I,K) = RTMP2(NGID(I),K)
           END DO
           DO I=1,NHN
             KDNALG(I+MLOC,K) = RTMP2(HN_LST(I),K)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(RTMP2)
     END IF
     READ (MRL,1060) SPVARM, PRINTM
     IF (SPVARM == 'CONSTANT') THEN
       READ(MRL,1033) KLNALG(1,1)
       DO K=1,KBM1
         DO I=1,MTLOC
           KLNALG(I,K)=KLNALG(1,1)
	 END DO  
       END DO
     ELSE
       ALLOCATE(RTMP2(MGL,KBM1));     RTMP2 = 0.0
       DO K=1,KBM1
         READ (MRL,1033) (RTMP2(I,K),I=1,MGL)
       END DO	 
       IF(SERIAL) KLNALG = RTMP2
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO K=1,KBM1
           DO I=1,MLOC
             KLNALG(I,K) = RTMP2(NGID(I),K)
           END DO
           DO I=1,NHN
             KLNALG(I+MLOC,K) = RTMP2(HN_LST(I),K)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(RTMP2)
     END IF
     READ (MRL,1060) SPVARM, PRINTM
     IF (SPVARM == 'CONSTANT') THEN
       READ(MRL,1033) KDPALG(1,1)
       DO K=1,KBM1
         DO I=1,MTLOC
           KDPALG(I,K)=KDPALG(1,1)
	 END DO  
       END DO
     ELSE
       ALLOCATE(RTMP2(MGL,KBM1));     RTMP2 = 0.0
       DO K=1,KBM1
         READ (MRL,1033) (RTMP2(I,K),I=1,MGL)
       END DO	 
       IF(SERIAL) KDPALG = RTMP2
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO K=1,KBM1
           DO I=1,MLOC
             KDPALG(I,K) = RTMP2(NGID(I),K)
           END DO
           DO I=1,NHN
             KDPALG(I+MLOC,K) = RTMP2(HN_LST(I),K)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(RTMP2)
     END IF
     READ (MRL,1060) SPVARM, PRINTM
     IF (SPVARM == 'CONSTANT') THEN
       READ(MRL,1033) KLPALG(1,1)
       DO K=1,KBM1
         DO I=1,MTLOC
           KLPALG(I,K)=KLPALG(1,1)
	 END DO  
       END DO
     ELSE
       ALLOCATE(RTMP2(MGL,KBM1));     RTMP2 = 0.0
       DO K=1,KBM1
         READ (MRL,1033) (RTMP2(I,K),I=1,MGL)
       END DO	 
       IF(SERIAL) KLPALG = RTMP2
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO K=1,KBM1
           DO I=1,MLOC
             KLPALG(I,K) = RTMP2(NGID(I),K)
           END DO
           DO I=1,NHN
             KLPALG(I+MLOC,K) = RTMP2(HN_LST(I),K)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(RTMP2)
     END IF
     READ (MRL,1060) SPVARM, PRINTM
     IF (SPVARM == 'CONSTANT') THEN
       READ(MRL,1033) NTM(1,1)
       DO K=1,KBM1
         DO I=1,MTLOC
           NTM(I,K)=NTM(1,1)
	 END DO  
       END DO
     ELSE
       ALLOCATE(RTMP2(MGL,KBM1));     RTMP2 = 0.0
       DO K=1,KBM1
         READ (MRL,1033) (RTMP2(I,K),I=1,MGL)
       END DO 	 
       IF(SERIAL) NTM = RTMP2
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO K=1,KBM1
           DO I=1,MLOC
             NTM(I,K) = RTMP2(NGID(I),K)
           END DO
           DO I=1,NHN
             NTM(I+MLOC,K) = RTMP2(HN_LST(I),K)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(RTMP2)
     END IF
     CLOSE (MRL)

!***** Boundary concentrations

     IF (BOUNDARY_CONC) THEN
       READ (CBC,1100)
       READ (CBC,1020) (NCB(JC),JC=1,NCP)
       READ (CBC,1100)
     END IF

!***** Sources One
!vjp  OPEN(UNIT=130,file='NPS96.OUT',status='unknown') 
     IF (SOURCE_ONE) THEN                         !MNOEL   1-25-93
       READ (S1,1100)
       READ (S1,1020) (S1LN(JC),JC=1,NCP)
       DO JC=1,NCP
         READ (S1,1020) (S1LB(JP,JC),JP=1,S1LN(JC))
       END DO
       READ (S1,1100)
     END IF

!***** Sources Two

     IF (SOURCE_TWO) THEN                         !MNOEL   1-25-93
       READ (S2,1100)
       READ (S2,1020) (S2LN(JC),JC=1,NCP)
       DO JC=1,NCP
         READ (S2,1020) (S2LB(JNP,JC),JNP=1,S2LN(JC))
       END DO
       READ (S2,1100)
     END IF

!***** Sources Three

     IF (SOURCE_THR) THEN                         
       READ (S3,1100)
       READ (S3,1020) (S3LN(JC),JC=1,NCP)
       DO JC=1,NCP
         READ (S3,1020) (S3LB(JNP,JC),JNP=1,S3LN(JC))
       END DO
       READ (S3,1100)
     END IF

!***** Light extinction and algal growth

     IF (LIGHT_EXTINCTION) THEN

       READ(KEI,1032)
       READ(KEI,1038) INTKE, INITKE
       READ(KEI,1037) GSTAR440,PHISTAR676,BNSTARNTU,BNSTARCHL,PSTARINT,   &
                      PSTAR440,PSTARCOLOR,SCDOM
       READ(KEI,1036) LAT, LONG, NWAVEL, TCHL2CHL, DOFFSET, DEPTHKE

! INITIALIZE KEI INTERVAL, KESS

       NXKEI = INTKE/24.
       DO K=1,KBM1
         DO I=1,MLOC
           KESS(I,K) = INITKE
	 END DO  
       END DO

! CONVERT FROM DECIMAL DEGREES TO RADIANS
       LAT = 6.2832*LAT/360.
       LONG = 6.2832*LONG/360.

       READ(KEI,1032)
       READ(KEI,*) (WAVEL(I),ALAMB(I),GLAMB(I),PHILAMB(I),PLAMB(I),      &
                    BLAMB(I),EZERO(I),I=1,NWAVEL)

       READ (KEI,1082) SPVARKE, PRINTKE
       IF (SPVARKE == 'CONSTANT') THEN
         READ (KEI,1087) COLOR(1,1),TURB(1,1)
	 DO K=1,KBM1
           DO I=1,MTLOC
             COLOR(I,K)   = COLOR(1,1)
             TURB(I,K)    = TURB(1,1)
	   END DO  
         END DO
       ELSE
         ALLOCATE(RTMP2(MGL,KBM1));    RTMP2 = 0.0
         ALLOCATE(RTMP21(MGL,KBM1));   RTMP21 = 0.0
         DO K=1,KBM1
           READ (KEI,1087) (COLOR(I,K),TURB(I,K),I=1,MGL)
	 END DO  
	 IF(SERIAL)THEN
           COLOR = RTMP2
           TURB  = RTMP21
         END IF
#        if defined (MULTIPROCESSOR)
         IF(PAR)THEN
           DO K=1,KBM1
             DO I=1,MLOC
               COLOR(I,K) = RTMP2(NGID(I),K)
               TURB(I,K)  = RTMP21(NGID(I),K)
             END DO
             DO I=1,NHN
               COLOR(I+MLOC,K) = RTMP2(HN_LST(I),K)
               TURB(I+MLOC,K)  = RTMP21(HN_LST(I),K)
             END DO
           END DO
         END IF
#        endif
         DEALLOCATE(RTMP2,RTMP21)
       END IF

       CLOSE(KEI)

! INTEGRATE AVERAGE QUANTUM SPECTRUM OVER VISIBLE WAVELENGTHS

       SUM=0
       DO I=2,NWAVEL-1
         SUM=SUM+EZERO(I)
       END DO
       EZEROINT = 2.5*(EZERO(1)+EZERO(NWAVEL))+5.*SUM

     END IF

!***** Algae

     IF (LIGHT_EXTINCTION) THEN
       CALL ALG_READ
     ENDIF

!***** Zooplankton 

     IF (ZOO_CALC) CALL ZOO_READ

!***** Settling rates

     IF (SETTLING) THEN
       READ (STL,1032)
       READ (STL,1060) SPVARM, PRINTM
       IF (SPVARM == 'CONSTANT') THEN
         READ(STL,1033) WSS(1,1),WSL(1,1),WSR(1,1),WS1(1,1),WS2(1,1),    &
                        WS3(1,1),WSU(1,1)
         DO K=1,KBM1
	   DO I=1,MLOC
             WSS(I,K)=WSS(1,1)
             WSL(I,K)=WSL(1,1)
             WSR(I,K)=WSR(1,1)
             WS1(I,K)=WS1(1,1)
             WS2(I,K)=WS2(1,1)
             WS3(I,K)=WS3(1,1)
             WSU(I,K)=WSU(1,1)
	   END DO  
         END DO
       ELSE
         ALLOCATE(RTMP2(MGL,KBM1));     RTMP2  = 0.0
         ALLOCATE(RTMP21(MGL,KBM1));    RTMP21 = 0.0
         ALLOCATE(RTMP22(MGL,KBM1));    RTMP22 = 0.0
         ALLOCATE(RTMP23(MGL,KBM1));    RTMP23 = 0.0
         ALLOCATE(RTMP24(MGL,KBM1));    RTMP24 = 0.0
         ALLOCATE(RTMP25(MGL,KBM1));    RTMP25 = 0.0
         ALLOCATE(RTMP26(MGL,KBM1));    RTMP26 = 0.0
         DO K=1,KBM1
           DO I=1,MGL
             READ(STL,1085) WSS(I,K),WSL(I,K),WSR(I,K),WS1(I,K),WS2(I,K),  &
	                    WS3(I,K),WSU(I,K)
	   END DO
         END DO
	 IF(SERIAL)THEN
           WSS = RTMP2
           WSL = RTMP21
           WSR = RTMP22
           WS1 = RTMP23
           WS2 = RTMP24
           WS3 = RTMP25
           WSU = RTMP26
         END IF
#        if defined (MULTIPROCESSOR)
         IF(PAR)THEN
           DO K=1,KBM1
             DO I=1,MLOC
               WSS(I,K) = RTMP2(NGID(I),K)
               WSL(I,K) = RTMP21(NGID(I),K)
               WSR(I,K) = RTMP22(NGID(I),K)
               WS1(I,K) = RTMP23(NGID(I),K)
               WS2(I,K) = RTMP24(NGID(I),K)
               WS3(I,K) = RTMP25(NGID(I),K)
               WSU(I,K) = RTMP26(NGID(I),K)
             END DO
             DO I=1,NHN
               WSS(I+MLOC,K) = RTMP2(HN_LST(I),K)
               WSL(I+MLOC,K) = RTMP21(HN_LST(I),K)
               WSR(I+MLOC,K) = RTMP22(HN_LST(I),K)
               WS1(I+MLOC,K) = RTMP23(HN_LST(I),K)
               WS2(I+MLOC,K) = RTMP24(HN_LST(I),K)
               WS3(I+MLOC,K) = RTMP25(HN_LST(I),K)
               WSU(I+MLOC,K) = RTMP26(HN_LST(I),K)
             END DO
           END DO
         END IF
#        endif
         DEALLOCATE(RTMP2,RTMP21,RTMP22,RTMP23,RTMP24,RTMP25,RTMP26)
       END IF
       CLOSE (STL)
     END IF

!***** Atmospheric loads

     IF (ATMOS_LOADS) READ (ATM,1000)

!***** Submerged aquatic vegetation

!JQI     IF (SAV_LOADS) THEN
!JQI       READ (SVI,1100)
!JQI       READ (SVI,1030) (SAVAREA(B),B=1,NSAVP)
!JQI       READ (SVI,1100)
!JQI     END IF

!***** Benthic fluxes

     IF (BENTHIC_FLUXES) THEN
       READ (BFI,1000)
       READ (BFI,1085)  KSDOC,KSNH4,KSNO3,KSPO4,KSO,KSSA
       READ (BFI,1080)  TRSDOC,TRSNH4,TRSNO3,TRSPO4,TRSO,TRSSA
       READ (BFI,1080)  MTCNO3, SEDNO3, KHSO  
       READ (BFI,1100)
     END IF

!***** Meteorologic data

     READ (MET,1000)

!***** Input FORMAT statements

1000 FORMAT(///)
1001 FORMAT(I6,10(I4,F10.1))
1002 FORMAT(7F10.3)
1003 FORMAT(8F10.3)
1005 FORMAT(//F10.1)
1010 FORMAT(A72)
1020 FORMAT(//(8X,9I8))
1025 FORMAT(8X,9I8)
1027 FORMAT(//(11X,8I8))
1030 FORMAT(//(8X,9F8.0))
1031 FORMAT(//(8X,2F8.0,I8,F8.2))
1032 FORMAT(/)
1033 FORMAT(//:(8X,9F8.0))
1035 FORMAT(:///10(10F8.0:/))
1036 FORMAT(//8X,2F8.1,I8,3F8.1)
1037 FORMAT(//8X,8F8.1)
1038 FORMAT(//8X,8F8.1)
1040 FORMAT(//8X,I8,8F8.0)
1050 FORMAT(//13X,A3,I8,5X,A3)
1052 FORMAT(//13X,A3,9I8)
1055 FORMAT(//8X,4(5X,A3),I8)
1060 FORMAT(//8X,2A8,2F8.0)
1065 FORMAT(://(8X,9A8))
1070 FORMAT(//(8X,9(5X,A3)))
1071 FORMAT(//(8X,5(5X,A3),A8,5X,A3,5X,A3))
1080 FORMAT(://8X,9F8.0)
1082 FORMAT(//8X,2A8//)
1085 FORMAT(8X,9F8.0)
1087 FORMAT(8X,4F8.0)
1090 FORMAT(//(8X,A72))
1100 FORMAT(/)
1110 FORMAT(:////////(8X,5I8))
1120 FORMAT(////(8X,I8))
1130 FORMAT(8X,9I8)
1140 FORMAT(://(2I8))
1150 FORMAT(5X,3F15.0,F18.0,F12.0,I10)
1160 format(://(13X,F13.0))
1170 FORMAT(://(2I10))
1190 FORMAT(I8,9F8.2)
1200 FORMAT(/(10F12.0))
1220 FORMAT(//(:8X,6F8.0))

!************************************************************************
!**                 Initialize Computational Variables                 **
!************************************************************************

!***** Logical control variables

     VOLUME_BALANCE   = VBC == ' ON'
     FLOW             = FLC == ' ON'
     PLOTS            = PLTC == ' ON'
     MASS_BALANCE     = MBLC == ' ON'
     OXYGEN_PLOTS     = OPLC == ' ON'
     SNAPSHOTS        = SNPC == ' ON'
     BENTHIC_OUTPUT   = BFOC == ' ON'
     TRANSPORT_FLUXES = TFLC == ' ON'.OR.MBLC == ' ON'
     RESTART_OUT      = RSOC == ' ON'
     DIAGNOSTICS      = DIAC == ' ON'
     Z_DIFFUSION      = ZDFC == ' ON'
     AVERAGE_PLOTS    = APLTC == ' ON'
     QUALITY_DIAG     = QPLTC == ' ON'
     AUTO_STEPPING    = AUTOC == ' ON'
     XY_DIFFUSION     = XYDFC == ' ON'
     UPWIND           = SLC == '  UPWIND'
     QUICKEST         = SLC == 'QUICKEST'
     STEP_BOUNDARY    = BNDTC == '    STEP'
     CONSERVE_MASS    = CONSC == '    MASS'
     SEDIMENT_DIAG    = SPLTC == ' ON'.AND.(SEDIMENT_CALC .OR. BENTHIC_FLUXES)
     SAV_PLOTS        = SAVPLTC == ' ON'.AND.(SAV_LOADS.OR.SAV_CALC)
     END_RUN          = .FALSE.

     IF (ACC(1) == ' ON')  TEMPERATURE_CALC = .TRUE.
     IF (ACC(3) == ' ON')  SOLIDS_CALC      = .TRUE.
     IF ((ACC(4) == ' ON').OR.(ACC(5) == ' ON').OR.(ACC(6) == ' ON'))  &
                           ALGAE_CALC   = .TRUE.
     IF (ACC(9) == ' ON')  CARBON_CALC      = .TRUE.
     IF (ACC(13) == ' ON') NITROGEN_CALC    = .TRUE.
     IF (ACC(20) == ' ON') PHOSPHORUS_CALC  = .TRUE.
     IF (ACC(26) == ' ON') COD_CALC         = .TRUE.
     IF (ACC(27) == ' ON') OXYGEN_CALC      = .TRUE.
     IF (ACC(29) == ' ON') SILICA_CALC      = .TRUE.
     IF (ACC(25) == ' ON') PIP_CALC         = .TRUE.

!***** Time variables

     NIT      = 0
     JDAY     = TMSTRT
     Print*, 'JDAY', JDAY
     JDAYMBL  = TMSTRT
     DLTDP    = 1
     TFLDP    = 1
     KFLDP    = 1
     RSODP    = 1
     SNPDP    = 1
     PLTDP    = 1
     APLDP    = 1
     OPLDP    = 1
     MBLDP    = 1
     DIADP    = 1
     NWQMR    = 0
     DLT      = DLTVAL(DLTDP)
     ELTMSPLT = JDAY*86400.
     ELTMSTFL = JDAY*86400.
     ELTMSKFL = JDAY*86400.
      
     NHMR  = 0
     NXTVD = JDAY
     ELTMS = JDAY*86400.
     MXDLT = DLTMAX(DLTDP)
     FNDLT = DLTFTN(DLTDP)
     NXSNP = SNPD(SNPDP)
     NXPLT = PLTD(PLTDP)
     NXTFL = TFLD(TFLDP)
     NXKFL = KFLD(KFLDP)
     NXOPL = OPLD(OPLDP)
     NXMBL = MBLD(MBLDP)
     NXDIA = DIAD(DIADP)
     NXAPL = APLTD(APLDP)

!***** Output control variables

     SNPD(NSNP+1)  = TMEND+1.
     PLTD(NPLT+1)  = TMEND+1.
     OPLD(NOPL+1)  = TMEND+1.
     MBLD(NMBL+1)  = TMEND+1.
     TFLD(NTFL+1)  = TMEND+1.
     KFLD(NKFL+1)  = TMEND+1.
     RSOD(NRSO+1)  = TMEND+1.
     DLTD(NDLT+1)  = TMEND+1.
     DIAD(NDIA+1)  = TMEND+1.
     APLTD(NAPL+1) = TMEND+1.

     NXCBC = 0.
     AC1   = 0.0

     AKE   = 0.0
     ACCHL1= 0.0
     ACCHL2= 0.0
     ACCHL3= 0.0
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

     AASRAT= 0.0
     ACFIX = 0.0

     ACLSZ    = 0.0
     ACLLZ    = 0.0
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
     AFIB    = 0.0
     ANLB    = 0.0
     APLB    = 0.0
     ANPPB   = 0.0
     ABBM    = 0.0
     ABLITE  = 0.0
     ADFEED  = 0.0

     ASFEED  = 0.0

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

     ALEAF  = 0.0
     ASTEM  = 0.0
     ATUBER = 0.0
     AEP    = 0.0
     AROOT  = 0.0
     AFISH  = 0.0
     AFIEP  = 0.0
     ANPPSAV= 0.0
     ANPPEP = 0.0
     AEPATN = 0.0
     AWATATN= 0.0

     ANLSAV = 0.0
     APLSAV = 0.0
     AFNSED = 0.0
     AFPSED = 0.0
     ANLEPI = 0.0
     APLEPI = 0.0
     AFHS   = 0.0

     A_T      = 0.0
     AP1     = 0.0
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

     APLEAF     = 0.0
     APEP       = 0.0
     ABMEP      = 0.0
     APREP      = 0.0

     ABMLEAF    = 0.0
     ABMTUBER   = 0.0
     ASLSH      = 0.0
     ADOCSAV    = 0.0
     APOCSAV    = 0.0
     ADOCEPI    = 0.0
     APOCEPI    = 0.0
     ASEDCSAV   = 0.0
     ANH4SAVW   = 0.0
     ANO3SAVW   = 0.0
     ADONSAVW   = 0.0
     APONSAVW   = 0.0
     ANH4EPI    = 0.0
     ANO3EPI    = 0.0
     ADONEPI    = 0.0
     APONEPI    = 0.0
     ASEDNSAV   = 0.0
     ASEDNH4SAV = 0.0          
     APO4SAVW   = 0.0
     ADOPSAVW   = 0.0
     APOPSAVW   = 0.0
     APO4EPI    = 0.0
     ADOPEPI    = 0.0
     APOPEPI    = 0.0
     ASEDPSAV   = 0.0
     ASEDPO4SAV = 0.0
     ADOSAV     = 0.0
     ADOEPI     = 0.0
     ASEDDOSAV  = 0.0

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

!***** Initial water column mass

     IF (MASS_BALANCE) THEN
       DO JC=1,NAC
         CMASS(AC(JC)) = 0.0
         DO K=1,KBM1
	   DO I=1,MTLOC
             CMASS(AC(JC)) = CMASS(AC(JC))+                       &
	           C1(I,K,AC(JC))*ART1(I)*D(I)*DZ(K)/1000.0
           END DO
	 END DO
       END DO
       IWCMN = ANC1*CMASS(4)+ANC2*CMASS(5)+ANC3*CMASS(6)+CMASS(12)    &
               +ANCSZ*CMASS(7)+ANCLZ*CMASS(8)                         &
               +CMASS(13)+CMASS(14)+CMASS(15)+CMASS(16)               &
               +CMASS(17)+CMASS(18)+CMASS(19)
       IWCMP = CMASS(20)+CMASS(21)+CMASS(22)+CMASS(23)+CMASS(24)      &
               +CMASS(25)+APCSZ*CMASS(7)+APCLZ*CMASS(8)               &
               +CMASS(30)+CMASS(31)+CMASS(32)
       IWCMC = CMASS(4)+CMASS(5)+CMASS(6)+CMASS(7)+CMASS(8)           &
               +CMASS(9)+CMASS(10)+CMASS(11)+CMASS(12)
       IWCMS = ASC1*CMASS(4)*ASC2*CMASS(5)+ASC3*CMASS(6)+CMASS(28)    &
               +CMASS(29)
     END IF

!***** Change in concentrations

     DTC = 0.0
    
!
!    *** DO days
!
     DOVDAYS = 0.0

!***** Maximum number of Sources One through Three

     DO JC=1,NCP
       IF (S1LN(JC) > S1LNMAX) THEN
         S1LNMAX = S1LN(JC)
         JCS1MAX = JC
       END IF
       IF (S2LN(JC) > S2LNMAX) THEN
         S2LNMAX = S2LN(JC)
         JCS2MAX = JC
       END IF
       IF (S3LN(JC) > S3LNMAX) THEN
         S3LNMAX = S3LN(JC)
         JCS3MAX = JC
       END IF
     END DO

!***** Horizontal diffusion

     VISCOFH = 0.0
     IF (XY_DIFFUSION) THEN
       DO K=1,KBM1
         DO I=1,MLOC
           VISCOFH(I,K) = XYDF
	 END DO  
       END DO
     END IF


!************************************************************************
!**                   Parameters for Sediment Submodels                **
!************************************************************************

     IF (SEDIMENT_CALC) CALL SED_READ

!************************************************************************
!**                              Outputs                               **
!************************************************************************

!***** Open output files

     IF (SNAPSHOTS)        OPEN (SNP,FILE=SNPFN)
     IF (DIAGNOSTICS)      OPEN (DIA,FILE=DIAFN)
     IF (KINETIC_FLUXES)   OPEN (KFL,FILE=KFLFN,FORM='UNFORMATTED')  !MNOEL
     IF (MASS_BALANCE)     OPEN (MBL,FILE=MBLFN,FORM='UNFORMATTED')
     IF (PLOTS)            OPEN (PLT,FILE=PLTFN)
     IF (AVERAGE_PLOTS)    OPEN (APL,FILE=APLFN,FORM='UNFORMATTED')
     IF (TRANSPORT_FLUXES) OPEN (TFL,FILE=TFLFN,FORM='UNFORMATTED')
     IF (OXYGEN_PLOTS)     OPEN (OPL,FILE=OPLFN,FORM='UNFORMATTED')
     IF (BENTHIC_FLUXES.AND.BENTHIC_OUTPUT) OPEN (BFO,FILE=BFOFN)
     IF (SFEEDER)          OPEN (SUD,FILE=SUDFN,FORM='UNFORMATTED')

!***** Snapshots

     IF (SNAPSHOTS) THEN
       IF(MSR)WRITE (SNP,2000) 'CE-QUAL-IC - Version 5.0 w LRDOM, Internal P'
       IF(MSR)WRITE (SNP,2010)  TITLE
       IF(MSR)WRITE (SNP,2020)  MAPFN,  GEOFN,  ICIFN,  AGRFN, ZOOFN, SUSFN
       IF(MSR)WRITE (SNP,2027)  STLFN, MRLFN, KEIFN
       IF(MSR)WRITE (SNP,2021) (HYDFN(JF),JF=1,NHYDF)
       IF(MSR)WRITE (SNP,2022) (METFN(JF),JF=1,NTVDF)
       IF(MSR)WRITE (SNP,2023) (S1FN(JF),JF=1,NTVDF)
       IF(MSR)WRITE (SNP,2024) (S2FN(JF),JF=1,NTVDF)
       IF(MSR)WRITE (SNP,2029) (S3FN(JF),JF=1,NTVDF)
       IF(MSR)WRITE (SNP,2031) (ATMFN(JF),JF=1,NTVDF)
       IF(MSR)WRITE (SNP,2028) (SVIFN(JF),JF=1,NTVDF)
       IF(MSR)WRITE (SNP,2025) (CBCFN(JF),JF=1,NTVDF)         ! JLM 2/23/01
       IF(MSR)WRITE (SNP,2026) (BFIFN(JF),JF=1,NTVDF)
       IF(MSR)WRITE (SNP,2030)  ICOFN,  SNPFN,  RSOFN,  PLTFN,  APLFN,        &
                         DIAFN,  TFLFN,  KFLFN,  OPLFN,  MBLFN,        &
                         ALOFN,  ZFOFN,  BFOFN,  SVOFN,  SUDFN
       IF(MSR)WRITE (SNP,2040)  NB,     NSB,    NQF,    NHQF,   NL
       IF(MSR)WRITE (SNP,2050)  TMSTRT, TMEND
       IF(MSR)WRITE (SNP,2060) (DLTD(J),J=1,NDLT)
       IF(MSR)WRITE (SNP,2070) (INT(DLTVAL(J)),J=1,NDLT)
       IF(MSR)WRITE (SNP,2071)  AUTOC
       IF(MSR)WRITE (SNP,2072) (INT(DLTMAX(J)),J=1,NDLT)
       IF(MSR)WRITE (SNP,2073) (DLTFTN(J),J=1,NDLT)
       IF(MSR)WRITE (SNP,2080)  RSIC,   BCC,    S1C,    S2C,    S3C,  BFC,    &
                         MDC,    ICIC,   ATMC,   SAVLC,  STLC
       IF(MSR)WRITE (SNP,2090)  ICOC,   SNPC,   PLTC,   QPLTC,  SPLTC,  APLTC,  &
                         DIAC,   TFLC,   KFLC,   OPLC,   RSOC
       IF(MSR)WRITE (SNP,2100) (SNPD(K),K=1,NSNP)
       IF(MSR)WRITE (SNP,2110) (PLTD(J),J=1,NPLT)
       IF(MSR)WRITE (SNP,2115) (APLTD(J),J=1,NAPL)
       IF(MSR)WRITE (SNP,2116) (TFLD(J),J=1,NTFL)
       IF(MSR)WRITE (SNP,2118) (OPLD(J),J=1,NOPL)
       IF(MSR)WRITE (SNP,2120) (RSOD(J),J=1,NRSO)
       IF(MSR)WRITE (SNP,2130) (SNPF(K),K=1,NSNP)
       IF(MSR)WRITE (SNP,2135) (PLTF(J),J=1,NPLT)
       IF(MSR)WRITE (SNP,2136) (APLF(J),J=1,NAPL)
       IF(MSR)WRITE (SNP,2137) (TFLF(J),J=1,NTFL)
       IF(MSR)WRITE (SNP,2138) (OPLF(J),J=1,NOPL)
       IF(MSR)WRITE (SNP,2141)  MBLC, VBC
       IF(MSR)WRITE (SNP,2150)  HYDC, INT(AHMDLT), SLC, CONSC, TH, MINSTEP, FILGTH
       IF(MSR)WRITE (SNP,2152)  SEDC
       IF(MSR)WRITE (SNP,2154)  SAVMC
       IF(MSR)WRITE (SNP,2160)  BNDTC
       IF(MSR)WRITE (SNP,2165)  XYDF,   ZDFMUL
       IF(MSR)WRITE (SNP,2170)  FLC,    XYDFC,  ZDFC
       IF(MSR)WRITE (SNP,2180) (CNAME(JC),ACC(JC),CIC(JC),JC=1,NCP)
       IF(MSR)WRITE (SNP,2190)  REDS1N, REDS2N, REDS3N, REDCBN,                 &
                         REDS1P, REDS2P, REDS3P, REDCBP,                 &
                         REDS1C, REDS2C, REDS3C, REDCBC
       IF(MSR)WRITE (SNP,2200)  KHONT,  KHNNT,  KHODOC, KHOCOD, KHNDN
       IF(MSR)WRITE (SNP,2210)  AOCR,   AONT
       IF(MSR)WRITE (SNP,2260)  KTNT1,  KTNT2,  TMNT
       IF(MSR)WRITE (SNP,2280)  TRCOD,  KTCOD,  TRMNL,                          &
                         KTMNL,  TRHDR,  KTHDR,                          &
                         TRHDR,  KTHDR,                                  &
                         TRMNL,  KTMNL,                                  &
                         TRHDR,  KTHDR,  TRHDR,                          &
                         KTHDR,  TRMNL,  KTMNL,                          &
                         TRHDR,  KTHDR,                                  &
                         TRHDR,  KTHDR,                                  &
                         TRSUA,  KTSUA
       IF(MSR)WRITE (SNP,2317)  KADPO4, KADSA
       IF(MSR)WRITE (SNP,2330)  AANOX,  ANDC,  KHCOAG
       IF(MSR)WRITE (SNP,2332)  AREAR,  BREAR, CREAR
       IF(MSR)WRITE (SNP,2310)  KE(1,1), KEISS(1,1), KEVSS(1,1), KEDOC(1,1)
     END IF

!***** Benthic flux parameters

     IF (BENTHIC_FLUXES.AND.BENTHIC_OUTPUT) THEN
       IF(MSR)WRITE(BFO,2501) KSDOC, KSNH4, KSNO3, KSPO4, KSO, KSSA
       IF(MSR)WRITE(BFO,2503) TRSDOC, TRSNH4, TRSNO3, TRSPO4, TRSO, TRSSA
       IF(MSR)WRITE(BFO,2502) MTCNO3, SEDNO3, KHSO
     END IF

!***** Diagnostics

     IF (DIAGNOSTICS) THEN                                !MNOEL 2-5-93
       IF(MSR)WRITE (DIA,2010) TITLE
       IF (UNI_ICON_IN) THEN
         IF(MSR)WRITE (DIA,2012)
         IF(MSR)WRITE (DIA,2011) ICIC
       END IF
       IF (BIN_ICON_IN) THEN
         IF(MSR)WRITE (DIA,2012)
         IF(MSR)WRITE (DIA,2011) ICIC
         IF(MSR)WRITE (DIA,2013)
         IF(MSR)WRITE (DIA,2010) OLDTITLE
       END IF
     END IF

!*****  Change double precision volumes to single so they can be written out
     DO K=1,KBM1
       DO I=0,MTLOC
         V1SINGLE(I,K) = V1(I,K)
       END DO	
     END DO

!***** Plots

     IF (MSR .AND. PLOTS) WRITE (PLT,*) TITLE,NAC,AC,NB,NSPECIES,                  &
       QUALITY_DIAG,SEDIMENT_DIAG,SAV_PLOTS,                           &
       ANC1,ANC2,ANC3,ASC1,ASC2,ASC3,ANCSZ,ANCLZ,                      &
         APCSZ,APCLZ,KADPO4,KADSA,M2,PIE2S,PIE2PO4,                    &  !CFC
         PIE2SI,PIENH4,ADWCEPI                                           !CFC
       IF(MSR)WRITE (PLT,*) NSB,((V1SINGLE(I,K),I=0,MTLOC),K=1,KBM1),           &
                  (SFA(I),I=1,MTLOC)

!***** Average plots

     IF (AVERAGE_PLOTS) THEN
       IF(MSR)WRITE (APL) TITLE,NAC,AC,NB,NSPECIES,QUALITY_DIAG,              &
        SEDIMENT_DIAG,SAV_PLOTS,                                       &
         ANC1,ANC2,ANC3,ASC1,ASC2,ASC3,ANCSZ,ANCLZ,                    &
         APCSZ,APCLZ,KADPO4,KADSA,ADWCEPI
       IF(MSR)WRITE (APL) NSB,((V1SINGLE(I,K),I=0,MTLOC),K=1,KBM1),           &
                  (SFA(I),I=1,MTLOC)
     END IF

!***** Constituent fluxes

!JQI     IF (TRANSPORT_FLUXES) WRITE (TFL) TITLE,NQF

!***** Kinetics fluxes

     IF (MSR .AND. KINETIC_FLUXES) WRITE (KFL) TITLE, MTLOC, KBM1,              &
       (SBN(I),I=1,MTLOC), (BBN(I),I=1,MTLOC),                        &
                         ((V1SINGLE(I,K),I=0,MTLOC),K=1,KBM1),        &
       (SFA(I),I=1,MTLOC), SAV_CALC, BALGAE_CALC

!***** Oxygen plots

     IF (OXYGEN_PLOTS) THEN
       IF(MSR)WRITE (OPL) TITLE,MTLOC,KBM1,NOINT,(OINT(I),I=1,NOINT),                &
                  ((V1SINGLE(I,K),I=0,MTLOC),K=1,KBM1)
     END IF

!***** Mass balances

     IF (MASS_BALANCE) THEN
       IF(MSR)WRITE (MBL) NSB,NB,NHQF,(SBN(B),B=1,MTLOC), (BBN(B),B=1,MTLOC)
     END IF

!***** Output FORMAT statements

2000 FORMAT(1X,A24/'+',10('_')//)
2010 FORMAT(1X,A72)
2012 FORMAT(/1X,79('*')/)
2011 FORMAT(3X,'Type of initial conditions file ',T35,' is ',A8)
2013 FORMAT(//' Title lines read from binary initial conditions file'/)
2020 FORMAT(//                                                    &
            1X,'Input filenames'/                                 &
            '+',15('_')//                                         &
            3X,'Map',T28,'= ',A72/                                &
            3X,'Geometry',T28,'= ',A72/                           &
            3X,'Initial conditions in',T28,'= ',A72/              &
            3X,'Algal parameters',T28,'= ',A72/                   &
            3X,'Zooplankton',T28,'= ',A72/                        &
            3X,'Suspension Feeders',T28,'= ',A72)
2021 FORMAT(3X,'Hydrodynamic',T28,'= ',A72:/(T30,A72))
2022 FORMAT(3X,'Meteorologic',T28,'= ',A72:/(T30,A72))
2023 FORMAT(3X,'Source One',T28,'= ',A72:/(T30,A72))
2024 FORMAT(3X,'Source Two',T28,'= ',A72:/(T30,A72))
2025 FORMAT(3X,'Boundary concentrations',T28,'= ',A72:/(T30,A72))
2026 FORMAT(3X,'Benthic fluxes',T28,'= ',A72:/(T30,A72))
2027 FORMAT(3X,'Settling',T28,'= ',A72/                           &
            3X,'Mineralization',T28,'= ',A72/                     &
            3X,'Light Extinction',T28,'= ',A72)
2028 FORMAT(3X,'Aquatic vegetation',T28,'= ',A72:/(T30,A72))
2029 FORMAT(3X,'Source Three',T28,'= ',A72:/(T30,A72))
2031 FORMAT(3X,'Atmospheric Loads',T28,'= ',A72:/(T30,A72))
2030 FORMAT(//                                                        &
            '1','Output filenames'/                                   &
            '+',16('_')//                                             &
            3X,'Initial conditions out',T28,'= ',A72/                 &
            3X,'Snapshot',T28,'= ',A72/                               &
            3X,'Restart output',T28,'= ',A72/                         &
            3X,'Plot',T28,'= ',A72/                                   &
            3X,'Average plot',T28,'= ',A72/                           &
            3X,'Diagnostics',T28,'= ',A72/                            &
            3X,'Transport fluxes',T28,'= ',A72/                       &
            3X,'Kinetic fluxes',T28,'= ',A72/                         &
            3X,'Oxygen plot',T28,'= ',A72/                            &
            3X,'Mass balance',T28,'= ',A72/                           &
            3X,'Algal parameters',T28,'= ',A72/                       &
            3X,'Zooplankton',T28,'= ',A72/                            &
            3X,'Sediment model',T28,'= ',A72/                         &
            3X,'Aquatic vegetation',T28,'= ',A72/                     &
            3X,'Suspension feeders',T28,'= ',A72)                 
2040 FORMAT(//                                                        &
            1X,'Geometry'/                                            &
            '+',8('_')//                                              & 
            3X,'Total boxes',T21,'= ',I5/                             &
            3X,'Surface boxes',T21,'= ',I5/                           &
            3X,'Total faces',T21,'= ',I5/                             &
            3X,'Horizontal faces',T21,'= ',I5/                        &
            3X,'Layers',T21,'= ',I5)
2050 FORMAT(//                                                        &
            1X,'Time control'/                                        &
            '+',12('_')//                                             &
            3X,'Starting time = ',F8.2,' Julian day'/                 &
            3X,'Ending time   = ',F8.2,' Julian day')           
2060 FORMAT(//                                                        &
            1X,'Timestep control'/                                    &
            '+',16('_')//                                             &
            3X,'Fixed'/                                               &
            (5X,'Julian Day ',T39,'= ',8F8.2):/)
2070 FORMAT(5X,'Timestep (sec)',T39,'= ',8I8/ (T49,8I8))
2071 FORMAT(3X,'Variable'/ 5X,'Autostepping',T39,'= ',5X,A3)
2072 FORMAT(5X,'Maximum allowable timestep (sec)',T39,'= ',8I8/ (T41,8I8))
2073 FORMAT(5X,'Fraction of calculated timestep',T39,'= ',8F8.2/ (T41,8F8.2))
2080 FORMAT(//                                                        &
            1X,'Input controls'/                                      &
            '+',14('_')//                                             &
            3X,'Restart ',T33,'= ',A8/                                &
            3X,'Boundary constituents',T33,'= ',A8/                   &
            3X,'Source One loadings',T33,'= ',A8/                     &
            3X,'Source Two loadings',T33,'= ',A8/                     &
            3X,'Source Three loadings',T33,'= ',A8/                   &
            3X,'Benthic fluxes',T33,'= ',A8/                          &
            3X,'Constituent modifications',T33,'= ',A8/               &
            3X,'Initial conditions in',T33,'= ',A8/                   &
            3X,'Atmospheric loadings',T33,'= ',A8/                    &
            3X,'SAV loads',T33,'= ',A8/                               &
            3X,'Settling',T33,'= ',A8)
2090 FORMAT(//                                                        &
            '1','Output controls'/                                    &
            '+',15('_')//                                             &
            3X,'Initial conditions out',T32,'= ',A3/                  &
            3X,'Snapshot',T32,'= ',A3/                                &
            3X,'Plot',T32,'= ',A3/                                    &
            5X,'Water quality diagnostics',T32,'= ',A3/               &
            5X,'Sediment diagnostics',T32,'= ',A3/                    &
            3X,'Average plot',T32,'= ',A3/                            &
            3X,'Diagnostics',T32,'= ',A3/                             &
            3X,'Transport fluxes',T32,'= ',A3/                        &
            3X,'Kinetic fluxes',T32,'= ',A3/                          &
            3X,'Oxygen plots',T32,'= ',A3/                            &
            3X,'Restart',T32,'= ',A3)
2100 FORMAT(//                                                        &
            3X,'Output dates (Julian day)'/                           &
            '+',2X,12('_')//                                          &
            5X,'Snapshots',T24,'= ',9F8.2,(:/T26,9F8.2))
2110 FORMAT(5X,'Plots',T24,'= ',9F8.2,(:/T26,9F8.2))
2115 FORMAT(5X,'Average plots',T24,'= ',9F8.2,(:/T26,9F8.2))
2116 FORMAT(5X,'Transport fluxes',T24,'= ',9F8.2,(:/T26,9F8.2))
2118 FORMAT(5X,'Oxygen plots',T24,'= ',9F8.2,(:/T26,9F8.2))
2120 FORMAT(5X,'Restarts',T24,'= ',9F8.2,(:/T26,9F8.2))
2130 FORMAT(/                                                         &
            3X,'Output frequencies (days)'/                           &
            '+',2X,18('_')//                                          &
            5X,'Snapshots',T24,'= ',9F8.2,(:/T26,9F8.2))
2135 FORMAT(5X,'Plots',T24,'= ',9F8.2,(:/T26,9F8.2))
2136 FORMAT(5X,'Average plots',T24,'= ',9F8.2,(:/T26,9F8.2))
2137 FORMAT(5X,'Transport fluxes',T24,'= ',9F8.2,(:/T26,9F8.2))
2138 FORMAT(5X,'Oxygen plots',T24,'= ',9F8.2,(:/T26,9F8.2))
2141 FORMAT(//                                                        &
            1X,'Balance calculations'/                                &
            '+',20('_')//                                             &
            3X,'Mass balance',T21,'= ',A3/                            &
            3X,'Volume balance',T21,'= ',A3)
2150 FORMAT(//                                                        &
            1X,'Hydrodynamics'/                                       &
            '+',13('_')//                                             &
            3X,'Origin',T40,'= ',A8/                                  &
            3X,'Interval between updates',T40,'= ',I8,' sec'/         &
            3X,'Horizontal transport',T40,'= ',A8/                    &
            3X,'Conservation type',T40,'= ',A8/                       &
            3X,'Theta for vertical solution',T40,'= ',F8.2/           &
            3X,'Minimum time step',T40,'= ',F8.2,' sec'/              &
            3X,'Days in Time-variable input files',T40,'= ',F8.2//)
2152 FORMAT('1','Sediment calculations'/ '+',21('_')// 3X,'Model = ',A3)
2154 FORMAT('1','SAV calculations'/ '+',21('_')// 3X,'Model = ',A3)
2160 FORMAT(//                                                        &
            1X,'Boundary interpolation'/                              &
            '+',22('_')//                                             &
            3X,'Temporal',T13,'=',A8)
2165 FORMAT(//                                                        &
            1X,'Diffusion'/                                           &
            '+',9('_')//                                              &
            3X,'Horizontal',T24,'=',F6.1/                             &
            3X,'Vertical multiplier',T24,'=',F6.1)
2170 FORMAT(//                                                        &
            1X,'Dead sea case'/                                       &
            '+',13('_')//                                             &
            3X,'Flow',T25,'= ',A3/                                    &
            3X,'Horizontal diffusion',T25,'= ',A3/                    &
            3X,'Vertical diffusion',T25,'= ',A3)
2180 FORMAT(//                                                        &
            1X,'Constituent controls'/                                &
            '+',20('_')//                                             &
            5X,'Constituent',T28,'Computation',T43,'Initial concen',  &
              'tration'/                                              &
            '+',4X,11('_'),T28,11('_'),T43,7('_'),1X,13('_')/         &
            T49,'(g/m**3)'//                                          &
            (3X,A24,T32,A3,T47,F8.3))       
!           (3X,A24,T32,A3,T47,F8.3)//)
2190 FORMAT(//                                                        &
            '1',2X,'Nutrient reductions'/                             &
            '+',2X,19('_')//                                          &
            5X,'Nitrogen'/                                            &
            7X,'Fall line',T24,'=',F5.2/                              &
            7X,'Below fall line',T24,'=',F5.2/                        &
            7X,'Atmospheric',T24,'=',F5.2/                            &
            7X,'Boundaries',T24,'=',F5.2/                             &
            5X,'Phosphorus'/                                          &
            7X,'Fall line',T24,'=',F5.2/                              &
            7X,'Below fall line',T24,'=',F5.2/                        &
            7X,'Atmospheric',T24,'=',F5.2/                            &
            7X,'Boundaries',T24,'=',F5.2/                             &
            5X,'Carbon'/                                              &
            7X,'Fall line',T24,'=',F5.2/                              &
            7X,'Below fall line',T24,'=',F5.2/                        &
            7X,'Atmospheric',T24,'=',F5.2/                            &
            7X,'Boundaries',T24,'=',F5.2)   
2200 FORMAT(//                                                        & 
            3X,'Half-saturation coefficients'/                        &
            '+',2X,28('_')//                                          &
            T25,'Oxygen',T38,'Nitrogen',T51,'Phosphorus',T67,         &
              'Carbon',T80,'Silica',T93,'Respiration'/                &
            '+',T25,6('_'),T38,8('_'),T51,10('_'),T67,6('_'),T80,     &
              6('_'),T93,11('_')/                                     &
            T23,'(g DO/m**3)',T37,'(g N/m**3)',T51,'(g P/m**3)',T65,  &
              '(g C/m**3)',T78,'(g Si/m**3)',T93,'(g DO/m**3)'//      &
            5X,'Nitrification',T24,F6.3,T38,F6.3/                     &
            5X,'COD oxidation',T24,F6.3/                              &
            5X,'DOC oxidation',T24,F6.3/                              &
            5X,'Denitrification',T24,F6.3//)
2210 FORMAT(3X,'Nutrient to carbon ratios'/                            &
            '+',2X,25('_')//T26,'Oxygen',T37,'Nitrogen',T50,           &
            'Phosphorus',T66,'Silica'/                                 &
            '+',T26,6('_'),T37,8('_'),T50,10('_'),T66,6('_')//         &
            5X,'Respiration',T25,F6.3/                                 &
            5X,'Nitrification',T25,F6.3//)
2260 FORMAT(3X,'Temperature/algal effects'/                            &
            '+',2X,25('_')//                                           &
            T28,'Suboptimal',T50,'Superoptimal',T77,'Maximum'/         &
            '+',T28,10('_'),T50,12('_'),T77,7('_')/                    &
            T27,'(/degrees C)',T50,'(/degrees C)',T75,'(degrees C)'//  &
            5X,'Nitrification',T29,F6.3,T52,F6.3,T77,F6.1/)
2270 FORMAT(T26,'Reference rate',T46,'Reference temperature',T72,      &
              'Temperature effect',T98,'Algal effect'/                 &
            '+',T26,14('_'),T46,21('_'),T72,18('_'),T98,12('_')/       &
            T30,'(/day)',T51,'(degrees C)',T75,'(/degrees C)',T97,     &
              '(m**3/day/g C)'//                                       &
            5X,'Base metabolism'/                                      &
            7X,'Cyanobacteria',T52,F6.1,T77,F6.3/                      &
            7X,'Diatoms',T52,F6.1,T77,F6.3/                            &
            7X,'Greens',T52,F6.1,T77,F6.3)
2280 FORMAT(5X,'COD oxidation',T52,F6.1,T77,F6.3/                      &
            5X,'DOC oxidation',T52,F6.1,T77,F6.3/                      &
            5X,'LPOC hydrolysis',T52,F6.1,T77,F6.3/                    &
            5X,'RPOC hydrolysis',T52,F6.1,T77,F6.3/                    &
            5X,'DON oxidation',T52,F6.1,T77,F6.3/                      &
            5X,'LPON hydrolysis',T52,F6.1,T77,F6.3/                    &
            5X,'RPON hydrolysis',T52,F6.1,T77,F6.3/                    &
            5X,'DOP oxidation',T52,F6.1,T77,F6.3/                      &
            5X,'LPOP hydrolysis',T52,F6.1,T77,F6.3/                    &
            5X,'RPOP hydrolysis',T52,F6.1,T77,F6.3/                    &
            5X,'Silica dissolution',T52,F6.1,T77,F6.3//)
2317 FORMAT(3X,'Sorption coefficients'/                                &
            '+',2X,21('_')//                                           &
            5X,'Phosphorus',T17,'=',F6.2,' m**3/gm'/                   &
            5X,'Silica',T17,'=',F6.2,' m**3/gm'//)
2330 FORMAT(3X,'Miscellaneous coefficients'/                           &
            '+',2X,26('_')//                                           &
            5X,'Ratio of anoxic to oxic metabolism',T41,'=',F6.2/      &
            5X,'N/C ratio for anoxic metabolism',T41,'=',F6.3//,       &
            5X,'Half-saturation salt for coagulation',T41,'=',F6.3)
2332 FORMAT(3X,'Reaeration (m/d) = ',F8.3,                             &
            ' * Rnu * (',F6.3,' * WMS) **',F6.3/)     
2310 FORMAT(3X,'Extinction coefficients'/                              &
            '+',2X,26('_')//                                           &
            5X,'Background',T41,'=',F6.2,' 1/m'/                       &
            5X,'Inorganic Solids',T41,'=',F6.2,' m**2/gm'/             &
            5X,'Volatile Solids',T41,'=',F6.2,' m**2/gm'/              &
            5X,'Dissolved Organic C',T41,'=',F6.2,' m**2/gm'//)
2501 FORMAT(' Temperature effects'//' DOC = ',F8.4,' per Degree C'/    &
            ' NH4 = ',F8.4,' per Degree C'/                            &
            ' NO3 = ',F8.4,' per Degree C'/                            &
            ' PO4 = ',F8.4,' per Degree C'/                            &
            ' SOD = ',F8.4,' per Degree C'/                            &
            ' SA  = ',F8.4,' per Degree C')
2502 FORMAT(//' Other parameters'//                                    &
            ' MTCNO3   = ',F8.4,' m/day'/                              &
            ' SEDNO3   = ',F8.4,' gm/m3'/                              &
            ' KHSO     = ',F8.4,' gm/m3')
2503 FORMAT (//' Reference Temperatures'//' DOC = ',F8.4,' Degrees C'/ &
             ' NH4 = ',F8.4,' Degrees C'/                              &
             ' NO3 = ',F8.4,' Degrees C'/                              &
             ' PO4 = ',F8.4,' Degrees C'/                              &
             ' SOD = ',F8.4,' Degrees C'/                              &
             ' SA  = ',F8.4,' Degrees C')


     NXAPL = APLTD(APLDP)
    
     INFLOW=0
     DO F=1,NHQF
       IF(RIGHT_FLOWB(F) .OR. LEFT_FLOWB(F)) THEN
         INFLOW=INFLOW+1
         IFLOWP(INFLOW)=F
       END IF
     END DO

     RETURN
     END SUBROUTINE INPUTS

 
