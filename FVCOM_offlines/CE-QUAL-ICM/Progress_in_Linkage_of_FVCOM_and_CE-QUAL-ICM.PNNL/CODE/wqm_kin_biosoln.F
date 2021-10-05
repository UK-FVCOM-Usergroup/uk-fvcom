!************************************************************************
!** additional paramters were made spatially variable these include:   **
!**     KDC,    KLC,    KRC,    KDN,    KLN,    KRN,    KDP,           **
!**     KLP,    KRP,   KSUA,   KCOD, KDCALG, KLCALG, KRCALG,           **
!**  KDNALG, KLNALG, KRNALG, KDPALG, KLPALG, KRPALG,    NTM,           **
!**      DL,      R        ***** MNOEL 2-20-93 *****                   **
!**  **** took out kfl_%%% subroutines that performed writing          **
!**       operations and included them in main program so they could   **
!**       be averaged over time periods instead of instantaneous       **
!**       output.  This process included vectorizing some variables.   **
!**                        ***** MNOEL 7-28-93 *****                   **
!**                                                                    **
!**                Kinetics Subroutines for CE-QUAL-IC                 **
!**                                                                    **
!**                            Version 1.0                             **
!**                         February 24, 1989                          **
!**                                                                    **
!**                    Water Quality Modeling Group                    **
!**                    U.S. Army Corps of Engineers                    **
!**                    Waterways Experiment Station                    **
!**                    Vicksburg, Mississippi 39180                    **
!**                                                                    **
!************************************************************************


!************************************************************************
!**                 S U B R O U T I N E   T E M P E R                  **
!************************************************************************

   SUBROUTINE TEMPER(DTT) 
   USE WQM
   USE ALL_VARS

   IMPLICIT NONE
   REAL :: RHO,CP
   REAL :: DTT(0:MTLOC,KBM1)    
   INTEGER :: I
   DATA     RHO /1.0E6/, CP /4.1796/

   DTT = 0.0

   DO I=1,MLOC
     DTT(I,1) = KT/(RHO*CP*D(I)*DZ(1))*(TE-T(I,1))
   END DO  

   RETURN
   END SUBROUTINE TEMPER

!************************************************************************
!**                 S U B R O U T I N E   S O L I D S                  **
!************************************************************************

   SUBROUTINE SOLIDS(DTSSI,FLXSSSI)
   USE WQM
   USE ALL_VARS

   IMPLICIT NONE
   INTEGER :: I,K
   REAL,DIMENSION(MTLOC,KBM1) :: DTSSI, FLXSSSI    
!******* Set special settling rate for high solids concs
   DTSSI = 0.0
   FLXSSSI = 0.0

   WSSHI = 0.0
   DO K=1,KBM1
     DO I=1,MLOC
       IF (SSI(I,K) < 100.) THEN
         WSSHI(I,K) = WSS(I,K)
       ELSE
         WSSHI(I,K) = 5.0
       END IF
     END DO  
   END DO

!******* Settle particulate fraction

   DO I=1,MLOC
     FLXSSSI(I,1) = WSSHI(I,1)*SSI(I,1)*V2(I,1)/(D(I)*DZ(1)*86400.)
     DTSSI(I,1)   = -WSSHI(I,1)*SSI(I,1)/(D(I)*DZ(1)*86400.)
   END DO  

   DO K=2,KBM1
     DO I=1,MLOC
       FLXSSSI(I,K) = WSSHI(I,K)*SSI(I,K)*V2(I,K)/(D(I)*DZ(K)*86400.)
       DTSSI(I,K)   = (WSSHI(I,K-1)*SSI(I,K-1)-WSSHI(I,K)*SSI(I,K))    &
                     /(D(I)*DZ(K)*86400.)
     END DO  
   END DO

!******  Resuspension

   IF (SEDIMENT_CALC) THEN

     DO I=1,MLOC
       DTSSI(I,KBM1) = DTSSI(I,KBM1)+(WSSHI(I,KBM1)-WSSNET(I))        &
                       *SSI(I,KBM1)/(D(I)*DZ(KBM1))/86400.
     END DO			 
   END IF

   RETURN
   END SUBROUTINE SOLIDS

!************************************************************************
!**             S U B R O U T I N E   Z O O _ R E A D                  **
!************************************************************************

   SUBROUTINE ZOO_READ
   USE WQM
   USE FILE_INFO
   USE ALL_VARS
#  if defined (MULTIPROCESSOR)
   USE MOD_PAR
#  endif     
   IMPLICIT NONE
   CHARACTER(LEN=72) :: TITLE(6)
   CHARACTER(LEN=8) :: SPVARSZ, SPVARLZ, PRINTSZ, PRINTLZ, TVARSZ,      &
                       TPRINTSZ, TVARLZ, TPRINTLZ

! vjp 10/12/04  These variables already declared in module WQM
!     REAL         KTGSZ1, KTGSZ2, KTBSZ, KTGLZ1, KTGLZ2, KTBLZ, 
!    $             KTPRSZ, KTPRLZ

   REAL :: FTB(0:35)
   INTEGER :: I,J,K
   REAL :: TMSZ, KTGSZ1, KTGSZ2,TRPRSZ,KTPRSZ
   REAL :: TMLZ, KTGLZ1, KTGLZ2, TRPRLZ,KTPRLZ
   REAL :: TLOOK
   REAL,ALLOCATABLE :: RTMP21(:,:),RTMP22(:,:),RTMP23(:,:),      &
                       RTMP24(:,:),RTMP25(:,:)

! TITLE CARDS

   OPEN(ZOO,FILE=ZOOFN,STATUS='OLD')
   READ(ZOO,1010) (TITLE(J),J=1,6)

! READ SPATIALLY-INVARIANT PARAMETERS FOR GROUP 1

   READ(ZOO,1030) CTSZ, KHCSZ, DOCRITSZ, MZEROSZ
   READ(ZOO,1030) ANCSZ, APCSZ, AOCRSZ
   READ(ZOO,1030) UB1SZ, UB2SZ, UB3SZ, ULDSZ, URDSZ, ULPSZ, URPSZ
   READ(ZOO,1030) TMSZ, KTGSZ1, KTGSZ2, TRSZ, KTBSZ, TRPRSZ,KTPRSZ
   READ(ZOO,1030) FLDOCSZ, FRDOCSZ, FLPOCSZ, FRPOCSZ
   READ(ZOO,1030) FNH4SZ, FUREASZ, FLDONSZ, FRDONSZ, FLPONSZ,FRPONSZ
   READ(ZOO,1030) FPO4SZ, FLDOPSZ, FRDOPSZ, FLPOPSZ, FRPOPSZ
   READ(ZOO,1030) FRSASZ

! READ SPATIALLY-INVARIANT PARAMETERS FOR GROUP 2

   READ(ZOO,1030) CTLZ, KHCLZ, DOCRITLZ, MZEROLZ
   READ(ZOO,1030) ANCLZ, APCLZ, AOCRLZ
   READ(ZOO,1030) UB1LZ, UB2LZ, UB3LZ, USZLZ, ULLZ, URLZ
   READ(ZOO,1030) TMLZ, KTGLZ1, KTGLZ2, TRLZ, KTBLZ, TRPRLZ,KTPRLZ
   READ(ZOO,1030) FLDOCLZ, FRDOCLZ, FLPOCLZ, FRPOCLZ
   READ(ZOO,1030) FNH4LZ, FUREALZ, FLDONLZ, FRDONLZ, FLPONLZ,FRPONLZ
   READ(ZOO,1030) FPO4LZ, FLDOPLZ, FRDOPLZ, FLPOPLZ, FRPOPLZ
   READ(ZOO,1030) FRSALZ

! ARE REMAINING GROUP 1 PARAMETERS SPATIALLY VARYING?

   READ(ZOO,1040) SPVARSZ, PRINTSZ
   IF (SPVARSZ == 'CONSTANT') THEN
     READ(ZOO,1030) RMAXSZ(1,1), ESZ(1,1), RFSZ(1,1), BMRSZ(1,1), BPRSZ(1,1)
     DO K=1,KBM1
       DO I=1,MTLOC
         RMAXSZ(I,K) = RMAXSZ(1,1)
         ESZ(I,K) = ESZ(1,1)
         RFSZ(I,K) = RFSZ(1,1)
         BMRSZ(I,K) = BMRSZ(1,1)
         BPRSZ(I,K) = BPRSZ(1,1)
       END DO
     END DO  
   ELSE
    ALLOCATE(RTMP21(MGL,KBM1));     RTMP21 = 0.0
    ALLOCATE(RTMP22(MGL,KBM1));     RTMP22 = 0.0
    ALLOCATE(RTMP23(MGL,KBM1));     RTMP23 = 0.0
    ALLOCATE(RTMP24(MGL,KBM1));     RTMP24 = 0.0
    ALLOCATE(RTMP25(MGL,KBM1));     RTMP25 = 0.0
     DO K=1,KBM1
       DO I=1,MGL
         READ(ZOO,1030) RTMP21(I,K), RTMP22(I,K), RTMP23(I,K),    &
                        RTMP24(I,K), RTMP25(I,K)
       END DO
     END DO  	 
     IF(SERIAL)THEN
       RMAXSZ = RTMP21
       ESZ    = RTMP22
       RFSZ   = RTMP23
       BMRSZ  = RTMP24
       BPRSZ  = RTMP25
     END IF
#    if defined (MULTIPROCESSOR)
     IF(PAR)THEN
       DO K=1,KBM1
         DO I=1,MLOC
           RMAXSZ(I,K) = RTMP21(NGID(I),K)
           ESZ(I,K)    = RTMP22(NGID(I),K)
           RFSZ(I,K)   = RTMP23(NGID(I),K)
           BMRSZ(I,K)  = RTMP24(NGID(I),K)
           BPRSZ(I,K)  = RTMP25(NGID(I),K)
         END DO
         DO I=1,NHN
           RMAXSZ(I+MLOC,K) = RTMP21(HN_LST(I),K)
           ESZ(I+MLOC,K)    = RTMP22(HN_LST(I),K)
           RFSZ(I+MLOC,K)   = RTMP23(HN_LST(I),K)
           BMRSZ(I+MLOC,K)  = RTMP24(HN_LST(I),K)
           BPRSZ(I+MLOC,K)  = RTMP25(HN_LST(I),K)
         END DO
       END DO
     END IF
#    endif
     DEALLOCATE(RTMP21,RTMP22,RTMP23,RTMP24,RTMP25)
   END IF      
      
! ARE REMAINING GROUP 2 PARAMETERS SPATIALLY VARYING?

   READ(ZOO,1040) SPVARLZ, PRINTLZ
   IF (SPVARLZ == 'CONSTANT') THEN
     READ(ZOO,1030) RMAXLZ(1,1), ELZ(1,1), RFLZ(1,1), BMRLZ(1,1), BPRLZ(1,1)
     DO K=1,KBM1
       DO I=1,MLOC
         RMAXLZ(I,K) = RMAXLZ(1,1)
         ELZ(I,K) = ELZ(1,1)
         RFLZ(I,K) = RFLZ(1,1)
         BMRLZ(I,K) = BMRLZ(1,1)
         BPRLZ(I,K) = BPRLZ(1,1)
       END DO
     END DO  
   ELSE
    ALLOCATE(RTMP21(MGL,KBM1));     RTMP21 = 0.0
    ALLOCATE(RTMP22(MGL,KBM1));     RTMP22 = 0.0
    ALLOCATE(RTMP23(MGL,KBM1));     RTMP23 = 0.0
    ALLOCATE(RTMP24(MGL,KBM1));     RTMP24 = 0.0
    ALLOCATE(RTMP25(MGL,KBM1));     RTMP25 = 0.0
     DO K=1,KBM1
       DO I=1,MGL
         READ(ZOO,1030) RTMP21(I,K), RTMP22(I,K), RTMP23(I,K),       &
                        RTMP24(I,K), RTMP25(I,K)
       END DO
     END DO   
     IF(SERIAL)THEN
       RMAXLZ = RTMP21
       ELZ    = RTMP22
       RFLZ   = RTMP23
       BMRLZ  = RTMP24
       BPRLZ  = RTMP25
     END IF
#    if defined (MULTIPROCESSOR)
     IF(PAR)THEN
       DO K=1,KBM1
         DO I=1,MLOC
           RMAXLZ(I,K) = RTMP21(NGID(I),K)
           ELZ(I,K)    = RTMP22(NGID(I),K)
           RFLZ(I,K)   = RTMP23(NGID(I),K)
           BMRLZ(I,K)  = RTMP24(NGID(I),K)
           BPRLZ(I,K)  = RTMP25(NGID(I),K)
         END DO
         DO I=1,NHN
           RMAXLZ(I+MLOC,K) = RTMP21(HN_LST(I),K)
           ELZ(I+MLOC,K)    = RTMP22(HN_LST(I),K)
           RFLZ(I+MLOC,K)   = RTMP23(HN_LST(I),K)
           BMRLZ(I+MLOC,K)  = RTMP24(HN_LST(I),K)
           BPRLZ(I+MLOC,K)  = RTMP25(HN_LST(I),K)
         END DO
       END DO
     END IF
#    endif
     DEALLOCATE(RTMP21,RTMP22,RTMP23,RTMP24,RTMP25)
   END IF

! TIME DEPENDENCE OF PREDATION ON GROUP 1

   READ(ZOO,1040) TVARSZ, TPRINTSZ
   IF (TVARSZ == 'CONSTANT') THEN
     READ(ZOO,1060) TVPRSZ(1)
     DO J=2,366
       TVPRSZ(J) = TVPRSZ(1)
     END DO
   ELSE
     READ(ZOO,1060) (TVPRSZ(J), J=1,366)
   END IF 

! TIME DEPENDENCE OF PREDATION ON GROUP 2

   READ(ZOO,1040) TVARLZ, TPRINTLZ
   IF (TVARLZ == 'CONSTANT') THEN
     READ(ZOO,1060) TVPRLZ(1)
     DO J=2,366
       TVPRLZ(J) = TVPRLZ(1)
     END DO
   ELSE
     READ(ZOO,1060) (TVPRLZ(J), J=1,366)
   END IF 

! CREATE LOOK-UP TABLE FOR TEMPERATURE EFFECTS         

   DO I=-50,350
     TLOOK = FLOAT(I)/10.
     IF (TLOOK < TMSZ) THEN
       FTSZ(I) = EXP(-KTGSZ1*(TLOOK-TMSZ)**2)
     ELSE
       FTSZ(I) = EXP(-KTGSZ2*(TMSZ-TLOOK)**2)
     END IF
!       IF (T(B) < TMLZ) THEN
!         FTLZ = EXP(-KTGLZ1*(T(B)-TMLZ)**2)
!       ELSE
!         FTLZ = EXP(-KTGLZ2*(TMLZ-T(B))**2)
!       END IF
     FTBMSZ(I) = EXP(KTBSZ*(TLOOK-TRSZ))
     FTBMLZ(I) = EXP(KTBLZ*(TLOOK-TRLZ))
     FTPRSZ(I) = EXP(KTPRSZ*(TLOOK-TRPRSZ))
     FTPRLZ(I) = EXP(KTPRLZ*(TLOOK-TRPRLZ))
   END DO

! READ BI-MODAL TEMPERATURE FUNCTION FOR GROUP TWO

   READ(ZOO,1050) (FTB(I), I=0,35)
   DO I=-50,0
     FTLZ(I)=0.
   END DO
   DO I=0,34
     DO J=0,10
       K = 10*I+J
       FTLZ(K) = FTB(I)+FLOAT(J)*(FTB(I+1)-FTB(I))/10.
     END DO
   END DO

   CLOSE(38)

     
!***** Input FORMAT statements

1010 FORMAT(A72)
1030 FORMAT(//(8X,9F8.0))
1040 FORMAT(//8X,8A8)
1050 FORMAT(//(8X,F8.0))
1060 FORMAT(//(16X,F8.0))


! OUTPUT WHAT WAS INPUT

   OPEN(39,FILE=ZFOFN,STATUS='UNKNOWN')
   IF(MSR)WRITE(39,2002) (TITLE(J),J=1,6)

! WRITE SPATIALLY-INVARIANT PARAMETERS FOR GROUP 1

   IF(MSR)WRITE(39,2000)
   IF(MSR)WRITE(39,2010) CTSZ, KHCSZ, DOCRITSZ, MZEROSZ
   IF(MSR)WRITE(39,2020) ANCSZ, APCSZ, AOCRSZ
   IF(MSR)WRITE(39,2030) UB1SZ, UB2SZ, UB3SZ, ULDSZ, URDSZ, ULPSZ, URPSZ
   IF(MSR)WRITE(39,2040) TMSZ, KTGSZ1, KTGSZ2, TRSZ, KTBSZ, TRPRSZ, KTPRSZ
   IF(MSR)WRITE(39,2045)
   IF(MSR)WRITE(39,2050) FLDOCSZ, FRDOCSZ, FLPOCSZ, FRPOCSZ
   IF(MSR)WRITE(39,2060) FNH4SZ, FUREASZ, FLDONSZ, FRDONSZ, FLPONSZ,FRPONSZ
   IF(MSR)WRITE(39,2070) FPO4SZ, FLDOPSZ, FRDOPSZ, FLPOPSZ, FRPOPSZ
   IF(MSR)WRITE(39,2080) FRSASZ

! WRITE SPATIALLY-VARYING PARAMETERS FOR GROUP 1

   IF (SPVARSZ == 'CONSTANT') THEN
     IF(MSR)WRITE(39,2085)
   ELSE
     IF(MSR)WRITE(39,2086)
   END IF 
   IF(MSR)WRITE(39,2090)
   IF (PRINTSZ /= '     ALL') THEN
     IF(MSR)WRITE(39,3000) RMAXSZ(1,1), ESZ(1,1), RFSZ(1,1), BMRSZ(1,1), BPRSZ(1,1)
   ELSE
     DO K=1,KBM1
       DO I=1,MLOC
         IF(MSR)WRITE(39,3010) I, K, RMAXSZ(I,K), ESZ(I,K), RFSZ(I,K), BMRSZ(I,K), &
	                BPRSZ(I,K)
       END DO
     END DO  			
   END IF
      
! WRITE TEMPORALLY-VARYING PARAMETERS FOR GROUP 1

   IF (TVARSZ == 'CONSTANT') THEN
     IF(MSR)WRITE(39,2087)
   ELSE
     IF(MSR)WRITE(39,2088)
   END IF 
   IF(MSR)WRITE(39,2092)
   IF (TPRINTSZ /= '     ALL') THEN
     IF(MSR)WRITE(39,3040) (J, TVPRSZ(J), J=1,1)
   ELSE
     IF(MSR)WRITE(39,3040) (J, TVPRSZ(J), J=1,366)
   END IF
      
! WRITE SPATIALLY-INVARIANT PARAMETERS FOR GROUP 2

   IF(MSR)WRITE(39,2005)
   IF(MSR)WRITE(39,2010) CTLZ, KHCLZ, DOCRITLZ, MZEROLZ
   IF(MSR)WRITE(39,2020) ANCLZ, APCLZ, AOCRLZ
   IF(MSR)WRITE(39,2035) UB1LZ, UB2LZ, UB3LZ, USZLZ, ULLZ, URLZ
   IF(MSR)WRITE(39,2040) TMLZ, KTGLZ1, KTGLZ2, TRLZ, KTBLZ, TRPRLZ, KTPRLZ
   IF(MSR)WRITE(39,2045)
   IF(MSR)WRITE(39,2050) FLDOCLZ, FRDOCLZ, FLPOCLZ, FRPOCLZ
   IF(MSR)WRITE(39,2060) FNH4LZ, FUREALZ, FLDONLZ, FRDONLZ, FLPONLZ, FRPONLZ
   IF(MSR)WRITE(39,2070) FPO4LZ, FLDOPLZ, FRDOPLZ, FLPOPLZ, FRPOPLZ
   IF(MSR)WRITE(39,2080) FRSALZ

! WRITE SPATIALLY-VARYING PARAMETERS FOR GROUP 2

   IF (SPVARLZ == 'CONSTANT') THEN
     IF(MSR)WRITE(39,2085)
   ELSE
     IF(MSR)WRITE(39,2086)
   END IF 
   IF(MSR)WRITE(39,2090)
   IF (PRINTLZ /= '     ALL') THEN
     IF(MSR)WRITE(39,3000) RMAXLZ(1,1), ELZ(1,1), RFLZ(1,1), BMRLZ(1,1), BPRLZ(1,1)
   ELSE
     DO K=1,KBM1
       DO I=1,MLOC
         IF(MSR)WRITE(39,3010) I, K, RMAXLZ(I,K), ELZ(I,K), RFLZ(I,K), BMRLZ(I,K),  &
                        BPRLZ(I,K)
       END DO
     END DO
   END IF

! WRITE TEMPORALLY-VARYING PARAMETERS FOR GROUP 2

   IF (TVARLZ == 'CONSTANT') THEN
     IF(MSR)WRITE(39,2087)
   ELSE
     IF(MSR)WRITE(39,2088)
   END IF 
   IF(MSR)WRITE(39,2092)
   IF (TPRINTLZ /= '     ALL') THEN
     IF(MSR)WRITE(39,3040) (J, TVPRLZ(J), J=1,1)
   ELSE
     IF(MSR)WRITE(39,3040) (J, TVPRLZ(J), J=1,366)
   END IF
      
! WRITE GROUP 2 TEMPERATURE FUNCTION

   IF(MSR)WRITE(39,3020)
   DO I=0,350,10
     IF(MSR)WRITE(39,3030) I/10, FTLZ(I)
   END DO

   CLOSE(39)
        

!***** Output FORMAT statements

2000 FORMAT(/' ZOOPLANKTON GROUP 1')
2002 FORMAT(1X,A72)
2005 FORMAT(/' ZOOPLANKTON GROUP 2')
2010 FORMAT(/' CARBON THRESHOLD = ',F8.2,' GM C/M**3'/                  &
       ' HALF-SATURATION CONCENTRATION FOR CARBON UPTAKE = ',F8.2,      &
       ' GM C/M**3'/                                                    &
       ' DO AT WHICH MORTALITY COMMENCES = ',F8.2,' GM DO/M**3'/        &
       ' MORTALITY UNDER ANOXIC CONDITIONS = ',F8.2,' /DAY')
2020 FORMAT(' NITROGEN TO CARBON RATIO = ',F8.3,' GM N/GM C'/           &
       ' PHOSPHORUS TO CARBON RATIO = ',F8.3, ' GM P/GM C'/             &
       ' RESPIRATION RATIO = ',F8.3,' GM DO/GM C')
2030 FORMAT(' UTILIZATION OF ALGAL GROUP 1   ',F8.2/                    &
            '                ALGAL GROUP 2   ',F8.2/                    &
            '                ALGAL GROUP 3   ',F8.2/                    &
            '                LABILE DOC      ',F8.2/                    &
            '                REFRACTORY DOC  ',F8.2/                    &
            '                LABILE PARTICLES',F8.2/                    &
            '                REFRACTORY PTCLS',F8.2)
2035 FORMAT(' UTILIZATION OF ALGAL GROUP 1   ',F8.2/                    &
            '                ALGAL GROUP 2   ',F8.2/                    &
            '                ALGAL GROUP 3   ',F8.2/                    &
            '                ZOOPL GROUP 1   ',F8.2/                    &
            '                LABILE PARTICLES',F8.2/                    &
            '                REFRACTORY PTCLS',F8.2)
2040 FORMAT(' OPTIMUM PRODUCTION SPECIFIED AT ',F8.2,' C.'/             & 
            ' KT1 = ',F8.3,' KT2 = ',F8.3,' PER DEGREE**2'/             &
            ' METABOLISM SPECIFIED AT ',F8.2,' C.'/                     &
            ' TEMPERATURE EFFECT = ',F8.3,' PER DEGREE'/                &
            ' PREDATION SPECIFIED AT ',F8.2,' C.'/                      &
            ' TEMPERATURE EFFECT = ',F8.3,' PER DEGREE')
2045 FORMAT(/' RECYCLING BY ZOOPLANKTON'/                               &
       '            DIS INORG  LAB DISS  REF DISS  LAB PART  REF PART')
2050 FORMAT(' CARBON    ',10X,4F10.3)
2060 FORMAT(' NITROGEN  ',6F10.3)
2070 FORMAT(' PHOSPHORUS',5F10.3)
2080 FORMAT(' SILICA    ',F10.3)
2085 FORMAT(/' REMAINING PARAMETERS ARE SPATIALLY-INVARIANT')
2086 FORMAT(/' REMAINING PARAMETERS VARY SPATIALLY')
2087 FORMAT(/' HIGHER-LEVEL PREDATION IS TEMPORALLY-INVARIANT')
2088 FORMAT(/' HIGHER-LEVEL PREDATION VARIES TEMPORALLY')
2090 FORMAT(/'    BOX   RATION   EFNCY  RESPFR   METAB  PRLOSS'/        &
             '           1/DAY                   1/DAY M**3/GM C/'/     &
             '                                            DAY')
2092 FORMAT(/'    DAY     TVPR')
3000 FORMAT(8X,5F8.3)
3010 FORMAT(2I8,5F8.3)
3020 FORMAT(/' TEMPERATURE FUNCTION FOR GROUP 2'/                       &
            '    TEMP      f(T)'/)
3030 FORMAT(I8,F10.4)
3040 FORMAT(I8,F8.3)

   RETURN
   END SUBROUTINE ZOO_READ

!************************************************************************
!**                  S U B R O U T I N E   Z O O P L                   **
!************************************************************************

   SUBROUTINE ZOOPL(DTSZ,DTLZ)
   USE WQM
   USE ALL_VARS
     
   IMPLICIT NONE
   REAL :: NRRATE,DOREF,CRRATE,FRLD,FRRD,FRLP,FRRP,ANCPSZ,FRL,FRR,   &
           ANCPLZ,APCPSZ,PRRATE,APCPLZ,FRB1SZ,FRB2SZ,FRB3SZ,FRB1LZ,  &
	   FRB2LZ,FRB3LZ
   REAL,DIMENSION(MTLOC,KBM1) :: DTSZ, DTLZ
   INTEGER :: I,J,K,ITEMP

! DETERMINE JULIAN DAY
   DTSZ = 0.0
   DTLZ = 0.0
   
   J = 1.0 + AMOD(JDAY,365.25)

   DO K=1,KBM1
     DO I=1,MLOC

! COMPUTE PREY AVAILABLE TO ZOOPLANKTON

       B1ASZ(I,K) = MAX(0.,B1(I,K)-CTSZ)          
       B2ASZ(I,K) = MAX(0.,B2(I,K)-CTSZ)          
       B3ASZ(I,K) = MAX(0.,B3(I,K)-CTSZ)
       LDOCASZ(I,K) = MAX(0.,LDOC(I,K)-CTSZ)          
       RDOCASZ(I,K) = MAX(0.,RDOC(I,K)-CTSZ)          
       LPOCASZ(I,K) = MAX(0.,LPOC(I,K)-CTSZ)          
       RPOCASZ(I,K) = MAX(0.,RPOC(I,K)-CTSZ)
       PRASZ(I,K) = UB1SZ*B1ASZ(I,K)+UB2SZ*B2ASZ(I,K)+UB3SZ*B3ASZ(I,K)      &
                + ULDSZ*LDOCASZ(I,K)+URDSZ*RDOCASZ(I,K)+ULPSZ*LPOCASZ(I,K)  &
                + URPSZ*RPOCASZ(I,K)        

       B1ALZ(I,K) = MAX(0.,B1(I,K)-CTLZ)          
       B2ALZ(I,K) = MAX(0.,B2(I,K)-CTLZ)          
       B3ALZ(I,K) = MAX(0.,B3(I,K)-CTLZ)
       SZALZ(I,K) = MAX(0.,SZ(I,K)-CTLZ)
       LPOCALZ(I,K) = MAX(0.,LPOC(I,K)-CTLZ)          
       RPOCALZ(I,K) = MAX(0.,RPOC(I,K)-CTLZ)
       PRALZ(I,K) = UB1LZ*B1ALZ(I,K)+UB2LZ*B2ALZ(I,K)+UB3LZ*B3ALZ(I,K)      &
                + USZLZ*SZALZ(I,K)+ULLZ*LPOCALZ(I,K)+URLZ*RPOCALZ(I,K)

     END DO
   END DO  

! COMPUTE ZOOPLANKTON RATION AND METABOLISM

   DO K=1,KBM1
     DO I = 1,MLOC

! NUTRIENT LIMITATION

       CLSZ(I,K) = PRASZ(I,K)/(KHCSZ+PRASZ(I,K)+1.0E-30)
       CLLZ(I,K) = PRALZ(I,K)/(KHCLZ+PRALZ(I,K)+1.0E-30)

! RATION (GM PREY CARBON/GM ZOOPL CARBON/DAY)

       ITEMP = 10.*(T(I,K)+0.05)
       RSZ(I,K) = FTSZ(ITEMP)*CLSZ(I,K)*RMAXSZ(I,K)
       RLZ(I,K) = FTLZ(ITEMP)*CLLZ(I,K)*RMAXLZ(I,K)

! BASAL METABOLISM (1/DAY)

       BMSZ(I,K) = BMRSZ(I,K)*FTBMSZ(ITEMP)
       BMLZ(I,K) = BMRLZ(I,K)*FTBMLZ(ITEMP)
!       IF(I.eq.25) THEN
!        Print*, 'basal metab at node 25-BMSZ,BMLZ', BMSZ(I,K), BMLZ(I,K)
!       ENDIF
! LOSS TO PREDATION (GM C/M**3/DAY)

       PRSZLZ(I,K) = USZLZ*SZALZ(I,K)/(PRALZ(I,K)+1.0E-30)*RLZ(I,K)*LZ(I,K)
       PRSZ(I,K) = BPRSZ(I,K)*SZ(I,K)*SZ(I,K)*FTPRSZ(ITEMP)*TVPRSZ(J)
       PRLZ(I,K) = BPRLZ(I,K)*LZ(I,K)*LZ(I,K)*FTPRLZ(ITEMP)*TVPRLZ(J)
! RGL added prints here
!       IF(I.eq.25) THEN
!         Print*, 'loss to pred, PRSZ,PRLZ', PRSZ(I,K), PRLZ(I,K)
!       ENDIF
     END DO
   END DO  

! COMPUTE MORTALITY FACTORS

   DO K=1,KBM1
     DO I=1,MLOC

! DISSOLVED OXYGEN EFFECTS (1/DAY)

       DOREF = MIN(DOCRITSZ,DO(I,K))
       MSZ(I,K) = MZEROSZ*(1.-DOREF/DOCRITSZ)
       DOREF = MIN(DOCRITLZ,DO(I,K))
       MLZ(I,K) = MZEROLZ*(1.-DOREF/DOCRITLZ)

     END DO
   END DO  

! RATE OF CHANGE OF ZOOPLANKTON BIOMASS (GM C/M**3/DAY)

   DO K=1,KBM1
     DO I=1,MLOC

       GSZ(I,K) = ESZ(I,K)*(1-RFSZ(I,K))*RSZ(I,K)
       GLZ(I,K) = ELZ(I,K)*(1-RFLZ(I,K))*RLZ(I,K)
       DTSZ(I,K) = ((GSZ(I,K)-BMSZ(I,K)-MSZ(I,K))*SZ(I,K)-PRSZLZ(I,K)    &
                   -PRSZ(I,K))/86400.
       DTLZ(I,K) = ((GLZ(I,K)-BMLZ(I,K)-MLZ(I,K))*LZ(I,K)-PRLZ(I,K))/86400.

     END DO
   END DO  

! EFFECT OF ZOOPLANKTON ON ALGAE (GM C/M**3/DAY)

   DO K=1,KBM1
     DO I=1,MLOC

       B1SZ(I,K) = UB1SZ*B1ASZ(I,K)/(PRASZ(I,K)+1.0E-30)*RSZ(I,K)*SZ(I,K)
       B2SZ(I,K) = UB2SZ*B2ASZ(I,K)/(PRASZ(I,K)+1.0E-30)*RSZ(I,K)*SZ(I,K)
       B3SZ(I,K) = UB3SZ*B3ASZ(I,K)/(PRASZ(I,K)+1.0E-30)*RSZ(I,K)*SZ(I,K)

       B1LZ(I,K) = UB1LZ*B1ALZ(I,K)/(PRALZ(I,K)+1.0E-30)*RLZ(I,K)*LZ(I,K)
       B2LZ(I,K) = UB2LZ*B2ALZ(I,K)/(PRALZ(I,K)+1.0E-30)*RLZ(I,K)*LZ(I,K)
       B3LZ(I,K) = UB3LZ*B3ALZ(I,K)/(PRALZ(I,K)+1.0E-30)*RLZ(I,K)*LZ(I,K)

     END DO
   END DO  

! EFECT OF ZOOPLANKTON ON CARBON (GM C/M**3/DAY)

   DO K=1,KBM1
     DO I=1,MLOC

       CRRATE = ((1.-ESZ(I,K))*RSZ(I,K)+MSZ(I,K))*SZ(I,K)+PRSZ(I,K)
       FRLD = ULDSZ*LDOCASZ(I,K)/(PRASZ(I,K)+1.0E-30)
       FRRD = URDSZ*RDOCASZ(I,K)/(PRASZ(I,K)+1.0E-30)
       FRLP = ULPSZ*LPOCASZ(I,K)/(PRASZ(I,K)+1.0E-30)
       FRRP = URPSZ*RPOCASZ(I,K)/(PRASZ(I,K)+1.0E-30)
       LDOCSZ(I,K)  = -FRLD*RSZ(I,K)*SZ(I,K)+CRRATE*FLDOCSZ
       RDOCSZ(I,K)  = -FRRD*RSZ(I,K)*SZ(I,K)+CRRATE*FRDOCSZ
       LPOCSZ(I,K) = -FRLP*RSZ(I,K)*SZ(I,K)+CRRATE*FLPOCSZ
       RPOCSZ(I,K) = -FRRP*RSZ(I,K)*SZ(I,K)+CRRATE*FRPOCSZ

       CRRATE = ((1.-ELZ(I,K))*RLZ(I,K)+MLZ(I,K))*LZ(I,K)+PRLZ(I,K)
       FRLP = ULLZ*LPOCALZ(I,K)/(PRALZ(I,K)+1.0E-30)
       FRRP = URLZ*RPOCALZ(I,K)/(PRALZ(I,K)+1.0E-30)
       LDOCLZ(I,K)  = CRRATE*FLDOCLZ
       RDOCLZ(I,K)  = CRRATE*FRDOCLZ
       LPOCLZ(I,K) = -FRLP*RLZ(I,K)*LZ(I,K)+CRRATE*FLPOCLZ
       RPOCLZ(I,K) = -FRRP*RLZ(I,K)*LZ(I,K)+CRRATE*FRPOCLZ

     END DO
   END DO  

! EFFECT OF ZOOPLANKTON ON NITROGEN (GM N/M**3/DAY)

   DO K=1,KBM1
     DO I=1,MLOC

       ANCPSZ = (ANC1*UB1SZ*B1ASZ(I,K)+ANC2*UB2SZ*B2ASZ(I,K)              &
                +ANC3*UB3SZ*B3ASZ(I,K)                                    &
                +ULPSZ*LPON(I,K)*LPOCASZ(I,K)/(LPOC(I,K)+1.0E-30)         & 
                +URPSZ*RPON(I,K)*RPOCASZ(I,K)/(RPOC(I,K)+1.0E-30))        &
                /(PRASZ(I,K)+1.0E-30)
       NRRATE = ((ANCPSZ-ANCSZ*ESZ(I,K)*(1.-RFSZ(I,K)))*RSZ(I,K)          &
                +(BMSZ(I,K)+MSZ(I,K))*ANCSZ)*SZ(I,K)                      &
                +PRSZ(I,K)*ANCSZ
       FRL = ULPSZ*LPOCASZ(I,K)/(PRASZ(I,K)+1.0E-30)
       FRR = URPSZ*RPOCASZ(I,K)/(PRASZ(I,K)+1.0E-30)
       NH4SZ(I,K) =  NRRATE*FNH4SZ
       LDONSZ(I,K) = NRRATE*FLDONSZ
       RDONSZ(I,K) = NRRATE*FRDONSZ
       LPONSZ(I,K) = -FRL*LPON(I,K)/(LPOC(I,K)+1.0E-30)                   &
                   *RSZ(I,K)*SZ(I,K)+NRRATE*FLPONSZ         
       RPONSZ(I,K) = -FRR*RPON(I,K)/(RPOC(I,K)+1.0E-30)                   &
                   *RSZ(I,K)*SZ(I,K)+NRRATE*FRPONSZ         

       ANCPLZ = (ANC1*UB1LZ*B1ALZ(I,K)+ANC2*UB2LZ*B2ALZ(I,K)              &
                +ANC3*UB3LZ*B3ALZ(I,K)+ANCSZ*USZLZ*SZALZ(I,K)             &
                +ULLZ*LPON(I,K)*LPOCALZ(I,K)/(LPOC(I,K)+1.0E-30)          &
                +URLZ*RPON(I,K)*RPOCALZ(I,K)/(RPOC(I,K)+1.0E-30))         &
                /(PRALZ(I,K)+1.0E-30)
       NRRATE = ((ANCPLZ-ANCLZ*ELZ(I,K)*(1.-RFLZ(I,K)))*RLZ(I,K)          &
                +(BMLZ(I,K)+MLZ(I,K))*ANCLZ)*LZ(I,K)                      &
                +PRLZ(I,K)*ANCLZ
       FRL = ULLZ*LPOCALZ(I,K)/(PRALZ(I,K)+1.0E-30)
       FRR = URLZ*RPOCALZ(I,K)/(PRALZ(I,K)+1.0E-30)
       NH4LZ(I,K) = NRRATE*FNH4LZ
       LDONLZ(I,K) = NRRATE*FLDONLZ
       RDONLZ(I,K) = NRRATE*FRDONLZ
       LPONLZ(I,K) = -FRL*LPON(I,K)/(LPOC(I,K)+1.0E-30)*RLZ(I,K)*LZ(I,K)  &
                   +NRRATE*FLPONLZ         
       RPONLZ(I,K) = -FRR*RPON(I,K)/(RPOC(I,K)+1.0E-30)*RLZ(I,K)*LZ(I,K)  &
                   +NRRATE*FRPONLZ

     END DO
   END DO  

! EFFECT OF ZOOPLANKTON ON PHOSPHORUS (GM P/M**3/DAY)

   DO K=1,KBM1
     DO I=1,MLOC

       APCPSZ = (Q1(I,K)*UB1SZ*B1ASZ(I,K)+Q2(I,K)*UB2SZ*B2ASZ(I,K)        &
                +Q3(I,K)*UB3SZ*B3ASZ(I,K)                                 &
                +ULPSZ*LPOP(I,K)*LPOCASZ(I,K)/(LPOC(I,K)+1.0E-30)         &
                +URPSZ*RPOP(I,K)*RPOCASZ(I,K)/(RPOC(I,K)+1.0E-30))        &
                /(PRASZ(I,K)+1.0E-30)
       PRRATE = ((APCPSZ-APCSZ*ESZ(I,K)*(1.-RFSZ(I,K)))*RSZ(I,K)          &
                +(BMSZ(I,K)+MSZ(I,K))*APCSZ)*SZ(I,K)                      &
                +PRSZ(I,K)*APCSZ
       FRL = ULPSZ*LPOCASZ(I,K)/(PRASZ(I,K)+1.0E-30)
       FRR = URPSZ*RPOCASZ(I,K)/(PRASZ(I,K)+1.0E-30)
       PO4SZ(I,K)  = PRRATE*FPO4SZ
       LDOPSZ(I,K) = PRRATE*FLDOPSZ
       RDOPSZ(I,K) = PRRATE*FRDOPSZ
       LPOPSZ(I,K) = -FRL*LPOP(I,K)/(LPOC(I,K)+1.0E-30)                   &
                   *RSZ(I,K)*SZ(I,K)+PRRATE*FLPOPSZ         
       RPOPSZ(I,K) = -FRR*RPOP(I,K)/(RPOC(I,K)+1.0E-30)                   &
                   *RSZ(I,K)*SZ(I,K)+PRRATE*FRPOPSZ 
       PIB1SZ(I,K) = Q1(I,K)*B1SZ(I,K)        
       PIB2SZ(I,K) = Q2(I,K)*B2SZ(I,K)        
       PIB3SZ(I,K) = Q3(I,K)*B3SZ(I,K)        

       APCPLZ = (Q1(I,K)*UB1LZ*B1ALZ(I,K)+Q2(I,K)*UB2LZ*B2ALZ(I,K)        &
                +Q3(I,K)*UB3LZ*B3ALZ(I,K)+USZLZ*APCSZ*SZALZ(I,K)          &
                +ULLZ*LPOP(I,K)*LPOCALZ(I,K)/(LPOC(I,K)+1.0E-30)          &
                +URLZ*RPOP(I,K)*RPOCALZ(I,K)/(RPOC(I,K)+1.0E-30))         &
                /(PRALZ(I,K)+1.0E-30)
       PRRATE = ((APCPLZ-APCLZ*ELZ(I,K)*(1.-RFLZ(I,K)))*RLZ(I,K)          &
                +(BMLZ(I,K)+MLZ(I,K))*APCLZ)*LZ(I,K)                      &
                +PRLZ(I,K)*APCLZ
       FRL = ULLZ*LPOCALZ(I,K)/(PRALZ(I,K)+1.0E-30)
       FRR = URLZ*RPOCALZ(I,K)/(PRALZ(I,K)+1.0E-30)
       PO4LZ(I,K)  = PRRATE*FPO4LZ
       LDOPLZ(I,K) = PRRATE*FLDOPLZ
       RDOPLZ(I,K) = PRRATE*FRDOPLZ
       LPOPLZ(I,K) = -FRL*LPOP(I,K)/(LPOC(I,K)+1.0E-30)                   &
                   *RLZ(I,K)*LZ(I,K)+PRRATE*FLPOPLZ         
       RPOPLZ(I,K) = -FRR*RPOP(I,K)/(RPOC(I,K)+1.0E-30)                   &
                   *RLZ(I,K)*LZ(I,K)+PRRATE*FRPOPLZ
       PIB1LZ(I,K) = Q1(I,K)*B1LZ(I,K)        
       PIB2LZ(I,K) = Q2(I,K)*B2LZ(I,K)        
       PIB3LZ(I,K) = Q3(I,K)*B3LZ(I,K)        

     END DO
   END DO  

! EFFECT OF ZOOPLANKTON ON DISSOLVED OXYGEN (GM DO/M**3/DAY)

   DO K=1,KBM1
     DO I=1,MLOC

       DOSZ(I,K) = (ESZ(I,K)*RFSZ(I,K)*RSZ(I,K)+BMSZ(I,K))*AOCRSZ*SZ(I,K)
       DOLZ(I,K) = (ELZ(I,K)*RFLZ(I,K)*RLZ(I,K)+BMLZ(I,K))*AOCRLZ*LZ(I,K)

     END DO
   END DO  

! EFFECT OF ZOOPLANKTON ON SILICA (GM SI/M**3/DAY)

   DO K=1,KBM1
     DO I=1,MLOC

       FRB1SZ = UB1SZ*B1ASZ(I,K)/(PRASZ(I,K)+1.0E-30)
       FRB2SZ = UB2SZ*B2ASZ(I,K)/(PRASZ(I,K)+1.0E-30)
       FRB3SZ = UB3SZ*B3ASZ(I,K)/(PRASZ(I,K)+1.0E-30)
       SASZ(I,K) = (FRB1SZ*ASC1+FRB2SZ*ASC2+FRB3SZ*ASC3)*RSZ(I,K)*SZ(I,K)*FRSASZ
       SUSZ(I,K) = (FRB1SZ*ASC1+FRB2SZ*ASC2+FRB3SZ*ASC3)*RSZ(I,K)*SZ(I,K)*(1.-FRSASZ)

       FRB1LZ = UB1LZ*B1ALZ(I,K)/(PRALZ(I,K)+1.0E-30)
       FRB2LZ = UB2LZ*B2ALZ(I,K)/(PRALZ(I,K)+1.0E-30)
       FRB3LZ = UB3LZ*B3ALZ(I,K)/(PRALZ(I,K)+1.0E-30)
       SALZ(I,K) = (FRB1LZ*ASC1+FRB2LZ*ASC2+FRB3LZ*ASC3)*RLZ(I,K)*LZ(I,K)*FRSALZ
       SULZ(I,K) = (FRB1LZ*ASC1+FRB2LZ*ASC2+FRB3LZ*ASC3)*RLZ(I,K)*LZ(I,K)*(1.-FRSALZ)
         
     END DO
   END DO  

   RETURN
   END SUBROUTINE ZOOPL
          
        
!************************************************************************
!**                 S U B R O U T I N E   C A R B O N                  **
!************************************************************************

   SUBROUTINE CARBON(DTLDOC,DTRDOC,DTLPOC,DTRPOC,FLXSPOC)
   USE WQM
   USE ALL_VARS
     
   IMPLICIT NONE
   REAL :: LPOC1, LPOC2, LPOC3, LPSETL, KLDOC, KLPOC, LDOC1, LDOC2, LDOC3
   INTEGER :: I,K
   REAL :: DOCBM1, DOCBM2, DOCBM3, CP1, CP2, CP3, RDOC1, RDOC2, RDOC3,   &
           RPOC1, RPOC2, RPOC3, ALGCAR, SALTC, COAG, FDO1, FDO2, FDO3
   REAL :: DICBM,DICPR, RPSETL,FTN
   REAL,DIMENSION(MTLOC,KBM1) :: DTLDOC, DTRDOC, DTLPOC, DTRPOC, FLXSPOC

   DTLDOC = 0.0
   DTRDOC = 0.0
   DTLPOC = 0.0
   DTRPOC = 0.0
   FLXSPOC = 0.0
!RGL added below to propagate doc correctly??
!   Print*, 'LDOC before RGL change', LDOC(25,1)
!   LDOC(:,:)=C1(:,:,9)
!   RDOC(:,:)=C1(:,:,10)
!   Print*, 'LDOC,RDOC in wqm_kin', LDOC(25,1), RDOC(25,1)
!   Print*, 'DO in wqm_kin', DO(25,1), C1(25,1,27)
!   Print*, 'phyto biomass', B1(25,1), C1(25,1,4) 
  DO K=1,KBM1
     DO I=1,MLOC

!********* Algal contribution

       DOCBM1 = KHR1/(KHR1+DO(I,K))
       DOCBM2 = KHR2/(KHR2+DO(I,K))
       DOCBM3 = KHR3/(KHR3+DO(I,K))
       CP1 = (P1(I,K)*PRSP1+BM1(I,K))*B1(I,K)
       CP2 = (P2(I,K)*PRSP2+BM2(I,K))*B2(I,K)
       CP3 = (P3(I,K)*PRSP3+BM3(I,K))*B3(I,K)
       LDOC1 = FCLD1*CP1+(1.-FCLD1-FCRD1-FCLP1-FCRP1)*DOCBM1*CP1        &
            + (FCLDP+FDOP*DOCBM1)*PR1(I,K)
       LDOC2 = FCLD2*CP2+(1.-FCLD2-FCRD2-FCLP2-FCRP2)*DOCBM2*CP2        &
            + (FCLDP+FDOP*DOCBM2)*PR2(I,K)
       LDOC3 = FCLD3*CP3+(1.-FCLD3-FCRD3-FCLP3-FCRP3)*DOCBM3*CP3        &
            + (FCLDP+FDOP*DOCBM3)*PR3(I,K)
       RDOC1 = FCRD1*CP1+FCRDP*PR1(I,K)
!       IF(I.eq.11126) THEN
!          Print*, 'LDOC1 middle', LDOC1
!          Print*, 'RDOC1 middle', RDOC1
!       ENDIF
       RDOC2 = FCRD2*CP2+FCRDP*PR2(I,K)
       RDOC3 = FCRD3*CP3+FCRDP*PR3(I,K)
       LPOC1      = FCLP1*CP1+FCLPP*PR1(I,K)
       LPOC2      = FCLP2*CP2+FCLPP*PR2(I,K)
       LPOC3      = FCLP3*CP3+FCLPP*PR3(I,K)
       RPOC1      = FCRP1*CP1+FCRPP*PR1(I,K)
       RPOC2      = FCRP2*CP2+FCRPP*PR2(I,K)
       RPOC3      = FCRP3*CP3+FCRPP*PR3(I,K)
       ALGDOC(I,K)  = LDOC1+LDOC2+LDOC3+RDOC1+RDOC2+RDOC3
       ALGPOC(I,K)  = LPOC1+LPOC2+LPOC3+RPOC1+RPOC2+RPOC3
!********* Mineralization and hydrolysis

       ALGCAR      = B1(I,K)+B2(I,K)+B3(I,K)
       KLDOC       = KLDC(I,K)+KDCALG(I,K)*ALGCAR
       KLPOC       = KLPC(I,K)+KLCALG(I,K)*ALGCAR
       SALTC       = MAX(0., SALT(I,K))          
       COAG        = 0.5*(1.+TANH(SALTC-KHCOAG))*KRCOAG(I,K)*RDOC(I,K)
       FTMNL(I,K)  = EXP(KTMNL*(T(I,K)-TRMNL))
       FTHDR(I,K)  = EXP(KTHDR*(T(I,K)-TRHDR))
       RATOX(I,K)  = (DO(I,K)+AANOX*KHODOC)/(KHODOC+DO(I,K))

       DENIT(I,K)  = KLDOC*FTMNL(I,K)*AANOX*KHODOC/(KHODOC+DO(I,K))        &
                    *NO3(I,K)/(KHNDN+NO3(I,K))*LDOC(I,K)
!        IF(I.eq.11126) Print*, 'DENIT', DENIT(I,K)
       MNLLDOC(I,K)= KLDOC*FTMNL(I,K)*DO(I,K)/(KHODOC+DO(I,K))*LDOC(I,K)
       MNLRDOC(I,K)= KRDC(I,K)*FTMNL(I,K)*DO(I,K)/(KHODOC+DO(I,K))*RDOC(I,K)
!       IF(I.eq.25) THEN
!          Print*,'KLDOC,FTMNL,DO,LDOC', KLDOC,FTMNL(I,K),DO(I,K),LDOC(I,K)
!          Print*, 'KRDC,RDOC', KRDC(I,K),RDOC(I,K)
!       ENDIF
       HDRLPOC(I,K)= RATOX(I,K)*KLPOC*FTHDR(I,K)*LPOC(I,K)
       HDRRPOC(I,K)= RATOX(I,K)*KRPC(I,K)*FTHDR(I,K)*RPOC(I,K)

!********* Change in carbon species

       DTLDOC(I,K) = (LDOC1+LDOC2+LDOC3-MNLLDOC(I,K)-DENIT(I,K)            &
                    + HDRLPOC(I,K)+HDRRPOC(I,K)+LDOCSZ(I,K)+LDOCLZ(I,K))/86400.
       DTRDOC(I,K) = (RDOC1+RDOC2+RDOC3-MNLRDOC(I,K)+RDOCSZ(I,K)+RDOCLZ(I,K) &
                    - COAG)/86400.
!       IF(I.eq.11126) THEN
!          Print*, 'DTLDOC in wqm_kin at river, middle', DTLDOC(I,K)
!          Print*, 'DTRDOC in wqm_kin at river, middle', DTRDOC(I,K)
!       ENDIF
       DTLPOC(I,K) = (LPOC1+LPOC2+LPOC3-HDRLPOC(I,K)+LPOCSZ(I,K)+LPOCLZ(I,K))/86400.
       DTRPOC(I,K) = (RPOC1+RPOC2+RPOC3-HDRRPOC(I,K)+RPOCSZ(I,K)+RPOCLZ(I,K)+COAG)/86400.

     END DO
   END DO  

! NET EFFECT OF ALGAE ON CARBON FOR MASS BALANCE

   DO K=1,KBM1
     DO I=1,MLOC

       FDO1 = DO(I,K)/(KHR1+DO(I,K))
       FDO2 = DO(I,K)/(KHR2+DO(I,K))
       FDO3 = DO(I,K)/(KHR3+DO(I,K))
       DICBM =                                                               &
          (P1(I,K)*PRSP1+BM1(I,K))*(1.-FCLD1-FCRD1-FCLP1-FCRP1)*FDO1*B1(I,K) &
         +(P2(I,K)*PRSP2+BM2(I,K))*(1.-FCLD2-FCRD2-FCLP2-FCRP2)*FDO2*B2(I,K) &
         +(P3(I,K)*PRSP3+BM3(I,K))*(1.-FCLD3-FCRD3-FCLP3-FCRP3)*FDO3*B3(I,K)
       DICPR = (PR1(I,K)+PR2(I,K)+PR3(I,K))*FDOP
       DLALGC(I,K) = P1(I,K)*B1(I,K)+P2(I,K)*B2(I,K)+P3(I,K)*B3(I,K)-DICBM-DICPR

     END DO
   END DO  

   DO I=1,MLOC

!********* Settling

     LPSETL = -WSL(I,1)*LPOC(I,1)/(D(I)*DZ(1))
     RPSETL = -WSR(I,1)*RPOC(I,1)/(D(I)*DZ(1))
     DTLPOC(I,1) = DTLPOC(I,1)+LPSETL/86400.
     DTRPOC(I,1) = DTRPOC(I,1)+RPSETL/86400.

!********* Settling flux

     FLXSPOC(I,1) = (WSL(I,1)*LPOC(I,1)+WSR(I,1)*RPOC(I,1))*V2(I,1)     &
                   /(D(I)*DZ(1)*86400.)

   END DO  

   DO K=2,KBM1
     DO I=1,MLOC

!********* Settling

       LPSETL = (WSL(I,K-1)*LPOC(I,K-1)-WSL(I,K)*LPOC(I,K))/(D(I)*DZ(K))
       RPSETL = (WSR(I,K-1)*RPOC(I,K-1)-WSR(I,K)*RPOC(I,K))/(D(I)*DZ(K))
       DTLPOC(I,K) = DTLPOC(I,K)+LPSETL/86400.
       DTRPOC(I,K) = DTRPOC(I,K)+RPSETL/86400.

!********* Settling flux

       FLXSPOC(I,K) = (WSL(I,K)*LPOC(I,K)+WSR(I,K)*RPOC(I,K))*V2(I,K)     &
                     /(D(I)*DZ(K)*86400.)

     END DO  
   END DO  

!******* Benthic fluxes

   DO I=1,MLOC
     DTLDOC(I,KBM1)  = DTLDOC(I,KBM1)+BENDOC(I)/(D(I)*DZ(KBM1))/86400.
   END DO  

!******* Resuspension

   IF (SEDIMENT_CALC) THEN

     DO I=1,MLOC
       DTLPOC(I,KBM1) = DTLPOC(I,KBM1)+(WSL(I,KBM1)-WSLNET(I))      &
                        *LPOC(I,KBM1)/(D(I)*DZ(KBM1))/86400.
       DTRPOC(I,KBM1) = DTRPOC(I,KBM1)+(WSR(I,KBM1)-WSRNET(I))      &
                        *RPOC(I,KBM1)/(D(I)*DZ(KBM1))/86400.
     END DO			  
   END IF

!******* SAV and epiphytes

!vjp modified 11/3/2005
!JQI   IF (SAV_CALC) THEN
!JQI     DO I=1,NSAVCELL
!JQI       B=SAVCELL(I)
!JQI       DTLDOC(B)  = DTLDOC(B)+(LDOCSAV(B)+LDOCEPI(B))/BL(B,3)/86400.
!JQI       DTRDOC(B)  = DTRDOC(B)+(RDOCSAV(B)+RDOCEPI(B))/BL(B,3)/86400.
!JQI       DTLPOC(B)  = DTLPOC(B)+(LPOCSAV(B)+LPOCEPI(B))/BL(B,3)/86400.
!JQI       DTRPOC(B)  = DTRPOC(B)+(RPOCSAV(B)+RPOCEPI(B))/BL(B,3)/86400.
!JQI     END DO
!JQI   END IF

   RETURN
   END SUBROUTINE CARBON

!************************************************************************
!**                 S U B R O U T I N E   N I T R O G                  **
!************************************************************************

   SUBROUTINE NITROG(DTNH4,DTNO3,DTLDON,DTRDON,DTLPON,DTRPON,FLXSPON)
   USE WQM
   USE ALL_VARS
 
   IMPLICIT NONE
   REAL ::    KLDON, KLPON, LPON1, LPON2, LPON3,                        &
              NH4A1, NH4A2,  NH4A3,  NO3A1,  NO3A2,  NO3A3,  LPSETL,    &
              NP1,  NP2,   NP3,   LDON1, LDON2, LDON3, NL1MOD
   REAL :: FTN, RDON1, RDON2, RDON3, RPON1, RPON2, RPON3
   REAL :: ALGCAR, SALTC, COAG, RPSETL
   INTEGER :: I,K
   REAL,DIMENSION(MTLOC,KBM1) :: DTNH4, DTNO3, DTLDON, DTRDON, DTLPON,  &
                                 DTRPON, FLXSPON

   DTLDON = 0.0
   DTRDON = 0.0
   DTLPON = 0.0
   DTRPON = 0.0
   FLXSPON = 0.0
   
! NITRIFICATION

   DO K=1,KBM1
     DO I=1,MLOC
        
!********* Temperature effect

       IF (T(I,K) < TMNT) THEN
         FTN = EXP(-KTNT1*(T(I,K)-TMNT)**2)
       ELSE
         FTN = EXP(-KTNT2*(TMNT-T(I,K))**2)
       END IF
       NT(I,K) = DO(I,K)/(KHONT+DO(I,K))*NH4(I,K)/(KHNNT+NH4(I,K))*FTN*NTM(I,K)
!       IF(I.eq.11126) Print*, 'NT', NT(I,K)
     END DO
   END DO  
    
   DO K=1,KBM1
     DO I=1,MLOC
!     IF(I.eq.11126) Print*, 'starting algal nitrogen preference'
!********* Algal nitrogen preference
       PN1(I,K) = NH4(I,K)*(NO3(I,K)/((KHN1+NH4(I,K))*(KHN1+NO3(I,K)))+KHN1  &
                /((1.E-30+NH4(I,K)+NO3(I,K))*(KHN1+NO3(I,K))))
       PN2(I,K) = NH4(I,K)*(NO3(I,K)/((KHN2+NH4(I,K))*(KHN2+NO3(I,K)))+KHN2  &
                /((1.E-30+NH4(I,K)+NO3(I,K))*(KHN2+NO3(I,K))))
       PN3(I,K) = NH4(I,K)*(NO3(I,K)/((KHN3+NH4(I,K))*(KHN3+NO3(I,K)))+KHN3  &
                /((1.E-30+NH4(I,K)+NO3(I,K))*(KHN3+NO3(I,K))))

!********* Nitrogen Fixation
! RGL commented out nfix for analytical test
       NFIX(I,K) = (P1(I,K)-P1NNF(I,K))*ANC1*B1(I,K)

!********* Algal sources/sinks

       NP1 = ANC1*(P1(I,K)*PRSP1+BM1(I,K))*B1(I,K)
       NP2 = ANC2*(P2(I,K)*PRSP2+BM2(I,K))*B2(I,K)          
       NP3 = ANC3*(P3(I,K)*PRSP3+BM3(I,K))*B3(I,K)          
       NH4A1 = FNI1*NP1-PN1(I,K)*P1NNF(I,K)*ANC1*B1(I,K)+FNIP*PR1(I,K)*ANC1
       NH4A2 = FNI2*NP2-PN2(I,K)*P2(I,K)*ANC2*B2(I,K)+FNIP*PR2(I,K)*ANC2
       NH4A3 = FNI3*NP3-PN3(I,K)*P3(I,K)*ANC3*B3(I,K)+FNIP*PR3(I,K)*ANC3
! RGL added below for analytical solution- set preference to zero
       PN1(I,K) = 0.0
       NO3A1      = (PN1(I,K)-1.)*P1NNF(I,K)*ANC1*B1(I,K)
!       IF(I.eq.11126) THEN
!          Print*, 'NO3A1 from algae1', NO3A1
!       ENDIF
       NO3A2      = (PN2(I,K)-1.)*P2(I,K)*ANC2*B2(I,K)
       NO3A3      = (PN3(I,K)-1.)*P3(I,K)*ANC3*B3(I,K)
       LDON1      = FNLD1*NP1+FNLDP*PR1(I,K)*ANC1
       LDON2      = FNLD2*NP2+FNLDP*PR2(I,K)*ANC2
       LDON3      = FNLD3*NP3+FNLDP*PR3(I,K)*ANC3
       RDON1      = FNRD1*NP1+FNRDP*PR1(I,K)*ANC1
       RDON2      = FNRD2*NP2+FNRDP*PR2(I,K)*ANC2
       RDON3      = FNRD3*NP3+FNRDP*PR3(I,K)*ANC3
       LPON1     = FNLP1*NP1+FNLPP*PR1(I,K)*ANC1
       LPON2     = FNLP2*NP2+FNLPP*PR2(I,K)*ANC2
       LPON3     = FNLP3*NP3+FNLPP*PR3(I,K)*ANC3
       RPON1     = FNRP1*NP1+FNRPP*PR1(I,K)*ANC1
       RPON2     = FNRP2*NP2+FNRPP*PR2(I,K)*ANC2
       RPON3     = FNRP3*NP3+FNRPP*PR3(I,K)*ANC3
       ALGNH4(I,K) = NH4A1+NH4A2+NH4A3
       ALGNO3(I,K) = NO3A1+NO3A2+NO3A3
       ALGDON(I,K) = LDON1+LDON2+LDON3+RDON1+RDON2+RDON3
       ALGPON(I,K) = LPON1+LPON2+LPON3+RPON1+RPON2+RPON3

!********* Mineralization and hydrolysis

       ALGCAR     = B1(I,K)+B2(I,K)+B3(I,K)
       KLDON      = KLDN(I,K)+KDNALG(I,K)*ALGCAR*KHNAVG/(KHNAVG+NH4(I,K)+NO3(I,K))
       KLPON      = KLPN(I,K)+KLNALG(I,K)*ALGCAR*KHNAVG/(KHNAVG+NH4(I,K)+NO3(I,K))    
       SALTC       = MAX(0., SALT(I,K))          
       COAG        = 0.5*(1.+TANH(SALTC-KHCOAG))*KRCOAG(I,K)*RDON(I,K)
       MNLLDON(I,K) = RATOX(I,K)*KLDON*FTMNL(I,K)*LDON(I,K)
       MNLRDON(I,K) = RATOX(I,K)*KRDN(I,K)*FTMNL(I,K)*RDON(I,K)
       HDRLPON(I,K) = RATOX(I,K)*KLPON*FTHDR(I,K)*LPON(I,K)
       HDRRPON(I,K) = RATOX(I,K)*KRPN(I,K)*FTHDR(I,K)*RPON(I,K)
       DENNO3(I,K)  = -ANDC*DENIT(I,K)

!********* Change in nitrogen species

       DTNH4(I,K)  = (NH4A1+NH4A2+NH4A3+MNLLDON(I,K)+MNLRDON(I,K)-NT(I,K)       &
                     +NH4SZ(I,K)+NH4LZ(I,K))/86400.
! RGL changed below to reflect only algae for analytical solution
!       DTNO3(I,K)  = (NT(I,K)-ANDC*DENIT(I,K)+NO3A1+NO3A2+NO3A3)/86400.
        DTNO3(I,K)  = (NO3A1)/86400.                     
!RGL added print
!       IF(I.eq.11126) Then
!          Print*, 'DTNO3 from bio', DTNO3(I,K)
!       ENDIF
       DTLDON(I,K) = (LDON1+LDON2+LDON3-MNLLDON(I,K)+HDRLPON(I,K)               &
                     +HDRRPON(I,K)+LDONSZ(I,K)+LDONLZ(I,K))/86400.
       DTRDON(I,K) = (RDON1+RDON2+RDON3-MNLRDON(I,K)+RDONSZ(I,K)+RDONLZ(I,K)    &
                     -COAG)/86400.
       DTLPON(I,K) = (LPON1+LPON2+LPON3-HDRLPON(I,K)                            &
                     +LPONSZ(I,K)+LPONLZ(I,K))/86400.
       DTRPON(I,K) = (RPON1+RPON2+RPON3-HDRRPON(I,K)                            &
                     +RPONSZ(I,K)+RPONLZ(I,K)+COAG)/86400.
     END DO
   END DO  

   DO I=1,MLOC

!********* Settling flux

     FLXSPON(I,1) = (WSL(I,1)*LPON(I,1)+WSR(I,1)*RPON(I,1))*V2(I,1)/(D(I)*DZ(1)*86400.)

!********* Settling

     LPSETL = -WSL(I,1)*LPON(I,1)/(D(I)*DZ(1))
     RPSETL = -WSR(I,1)*RPON(I,1)/(D(I)*DZ(1))
     DTLPON(I,1) = DTLPON(I,1)+LPSETL/86400.
     DTRPON(I,1) = DTRPON(I,1)+RPSETL/86400.

   END DO
   
   DO K=2,KBM1
     DO I=1,MLOC

!********* Settling flux

       FLXSPON(I,K) = (WSL(I,K)*LPON(I,K)+WSR(I,K)*RPON(I,K))*V2(I,K)/(D(I)*DZ(K)*86400.)

!********* Settling

       LPSETL = (WSL(I,K-1)*LPON(I,K-1)-WSL(I,K)*LPON(I,K))/(D(I)*DZ(K))
       RPSETL = (WSR(I,K-1)*RPON(I,K-1)-WSR(I,K)*RPON(I,K))/(D(I)*DZ(K))
       DTLPON(I,K) = DTLPON(I,K)+LPSETL/86400.
       DTRPON(I,K) = DTRPON(I,K)+RPSETL/86400.

     END DO
   END DO  

!******* Benthic fluxes

   DO I=1,MLOC
     DTNH4(I,KBM1)  = DTNH4(I,KBM1)+BENNH4(I)/(D(I)*DZ(KBM1))/86400.
     DTNO3(I,KBM1)  = DTNO3(I,KBM1)+BENNO3(I)/(D(I)*DZ(KBM1))/86400.
     DTLDON(I,KBM1) = DTLDON(I,KBM1)+BENDON(I)/(D(I)*DZ(KBM1))/86400.
   END DO  

!******* Resuspension

   IF (SEDIMENT_CALC) THEN

     DO I=1,MLOC
       DTLPON(I,KBM1) = DTLPON(I,KBM1)+(WSL(I,KBM1)-WSLNET(I))       &
                        *LPON(I,KBM1)/(D(I)*DZ(KBM1))/86400.
       DTRPON(I,KBM1) = DTRPON(I,KBM1)+(WSR(I,KBM1)-WSRNET(I))       &
                        *RPON(I,KBM1)/(D(I)*DZ(KBM1))/86400.
     END DO
   END IF

!******* SAV and epiphytes

!vjp modified 11/3/2005
!JQI   IF (SAV_CALC) THEN
!JQI     DO I=1,NSAVCELL
!JQI       B=SAVCELL(I)
!JQI       DTNH4(B)   = DTNH4(B)+(NH4SAVW(B)+NH4EPI(B))/BL(B,3)/86400.
!JQI       DTNO3(B)   = DTNO3(B)+(NO3SAVW(B)+NO3EPI(B))/BL(B,3)/86400.
!JQI       DTLDON(B)  = DTLDON(B)+(LDONSAVW(B)+LDONEPI(B))/BL(B,3)/86400.
!JQI       DTRDON(B)  = DTRDON(B)+(RDONSAVW(B)+RDONEPI(B))/BL(B,3)/86400.
!JQI       DTLPON(B)  = DTLPON(B)+(LPONSAVW(B)+LPONEPI(B))/BL(B,3)/86400.
!JQI       DTRPON(B)  = DTRPON(B)+(RPONSAVW(B)+RPONEPI(B))/BL(B,3)/86400.
!JQI     END DO
!JQI   END IF

!******* Atmospheric Loads

   DO I=1,MLOC
!     IF(I.eq.11126) Print*, 'start of atmospheric loading'
     DTNH4(I,1) = DTNH4(I,1)+PRECIP*ATMNH4/(D(I)*DZ(1))
     DTNO3(I,1) = DTNO3(I,1)+PRECIP*ATMNO3/(D(I)*DZ(1))
!RGl added print
!      IF(I.eq.11126) Print*, 'Precip N, atmosN', PRECIP, ATMNO3
!      IF(I.eq.11126) Print*, 'DTNO3 after atmos', DTNO3(I,1)
     DTLDON(I,1)= DTLDON(I,1)+PRECIP*ATMLDON/(D(I)*DZ(1))
     DTRDON(I,1)= DTRDON(I,1)+PRECIP*ATMRDON/(D(I)*DZ(1))
   END DO

   RETURN
   END SUBROUTINE NITROG

!************************************************************************
!**                 S U B R O U T I N E   P H O S P H                  **
!************************************************************************

   SUBROUTINE PHOSPH(DTPO4,DTLDOP,DTRDOP,DTLPOP,DTRPOP,DTPIP,           &
                     DTPIB1,DTPIB2,DTPIB3,FLXSPO4,FLXSPOP,FLXSPIP)
   USE WQM
   USE ALL_VARS
     
   IMPLICIT NONE
   REAL :: KLDOP, KLPOP, LPOP1, LPOP2, LPOP3, LPSETL, LDOP1, LDOP2, LDOP3
   REAL :: DF, PO4AVL, PUP1, PUP2, PUP3, PP1, PP2, PP3, PO43,     &
           RDOP1, RDOP2, RDOP3, RPOP1, RPOP2, RPOP3
   REAL :: ALGCAR, PFU, PPU, PFD, PPD, PO4SET, RPSETL, PIB1SETL,    &
           PIB2SETL, PIB3SETL, PF, PP 
   INTEGER :: I, J, K
   REAL,DIMENSION(MTLOC,KBM1) :: DTPO4, DTLDOP, DTRDOP, DTLPOP, DTRPOP,  &
        DTPIP, DTPIB1, DTPIB2, DTPIB3, FLXSPO4, FLXSPOP, FLXSPIP

   DTPO4 = 0.0
   DTLDOP = 0.0
   DTRDOP = 0.0
   DTLPOP = 0.0
   DTRPOP = 0.0
   DTPIP = 0.0
   DTPIB1 = 0.0
   DTPIB2 = 0.0
   DTPIB3 = 0.0
   FLXSPO4 = 0.0
   FLXSPOP = 0.0
   FLXSPIP = 0.0
   
   DO K=1,KBM1
     DO I=1,MLOC

!********* Algae sources/sinks

!********* Uptake via Monod formulation

       DF     = 1./(1.+KADPO4*SSI(I,K))
       PO4AVL = MAX(0.,DF*PO4(I,K))
       PUP1   = VMAX1*B1(I,K)*PO4AVL/(KHP1+PO4AVL+1.0E-6)
       PUP2   = VMAX2*B2(I,K)*PO4AVL/(KHP2+PO4AVL+1.0E-6)
       PUP3   = VMAX3*B3(I,K)*PO4AVL/(KHP3+PO4AVL+1.0E-6)
          
       PP1  = Q01*(P1(I,K)*PRSP1+BM1(I,K))*B1(I,K)
       PP2  = Q02*(P2(I,K)*PRSP2+BM2(I,K))*B2(I,K)
       PP3  = Q03*(P3(I,K)*PRSP3+BM3(I,K))*B3(I,K)
       PO41 = FPI1*PP1-PUP1+FPIP*PR1(I,K)*Q1(I,K)
!       IF(I.eq.25) THEN
!        Print*, 'PP1', PP1
!        Print*, 'PO41', PO41
!        Print*, 'PRSP1', PRSP1
!        Print*, 'PO4AVL', PO4AVL
!       ENDIF
       PO42 = FPI2*PP2-PUP2+FPIP*PR2(I,K)*Q2(I,K)
       PO43 = FPI3*PP3-PUP3+FPIP*PR3(I,K)*Q3(I,K)
       LDOP1     = FPLD1*PP1+FPLDP*PR1(I,K)*Q1(I,K) 
       LDOP2     = FPLD2*PP2+FPLDP*PR2(I,K)*Q2(I,K)
       LDOP3     = FPLD3*PP3+FPLDP*PR3(I,K)*Q3(I,K)
       RDOP1     = FPRD1*PP1+FPRDP*PR1(I,K)*Q1(I,K) 
       RDOP2     = FPRD2*PP2+FPRDP*PR2(I,K)*Q2(I,K)
       RDOP3     = FPRD3*PP3+FPRDP*PR3(I,K)*Q3(I,K)
       LPOP1     = FPLP1*PP1+FPLPP*PR1(I,K)*Q1(I,K)
       LPOP2     = FPLP2*PP2+FPLPP*PR2(I,K)*Q2(I,K)
       LPOP3     = FPLP3*PP3+FPLPP*PR3(I,K)*Q3(I,K)
       RPOP1     = FPRP1*PP1+FPRPP*PR1(I,K)*Q1(I,K)
       RPOP2     = FPRP2*PP2+FPRPP*PR2(I,K)*Q2(I,K)
       RPOP3     = FPRP3*PP3+FPRPP*PR3(I,K)*Q3(I,K)
       ALGPO4(I,K) = PO41+PO42+PO43
       ALGDOP(I,K) = LDOP1+LDOP2+LDOP3+RDOP1+RDOP2+RDOP3
       ALGPOP(I,K) = LPOP1+LPOP2+LPOP3+RPOP1+RPOP2+RPOP3


!********* Mineralization and hydrolysis

       ALGCAR  = B1(I,K)+B2(I,K)+B3(I,K)
       DF      = 1./(1.+KADPO4*SSI(I,K))
       PO4AVL  = MAX(1.E-6,DF*PO4(I,K))
       KLDOP   = KLDP(I,K)+KDPALG(I,K)*ALGCAR*KHPAVG/(KHPAVG+PO4AVL)
       KLPOP   = KLPP(I,K)+KLPALG(I,K)*ALGCAR*KHPAVG/(KHPAVG+PO4AVL)           
       MNLLDOP(I,K) = RATOX(I,K)*KLDOP*FTMNL(I,K)*LDOP(I,K)
       MNLRDOP(I,K) = RATOX(I,K)*KRDP(I,K)*FTMNL(I,K)*RDOP(I,K)
       HDRLPOP(I,K) = RATOX(I,K)*KLPOP*FTHDR(I,K)*LPOP(I,K)
       HDRRPOP(I,K) = RATOX(I,K)*KRPP(I,K)*FTHDR(I,K)*RPOP(I,K)

!********* Change in phosphorus species

       DTPO4(I,K)  = (PO41+PO42+PO43+MNLLDOP(I,K)+MNLRDOP(I,K)                &
                   +PO4SZ(I,K)+PO4LZ(I,K))/86400.
       DTLDOP(I,K) = (LDOP1+LDOP2+LDOP3-MNLLDOP(I,K)+HDRLPOP(I,K)             &
                   +HDRRPOP(I,K)+LDOPSZ(I,K)+LDOPLZ(I,K))/86400.
       DTRDOP(I,K) = (RDOP1+RDOP2+RDOP3-MNLRDOP(I,K)+RDOPSZ(I,K)+RDOPLZ(I,K))   &
                   /86400.
       DTLPOP(I,K) = (LPOP1+LPOP2+LPOP3-HDRLPOP(I,K)+LPOPSZ(I,K)+LPOPLZ(I,K))/86400.
       DTRPOP(I,K) = (RPOP1+RPOP2+RPOP3-HDRRPOP(I,K)+RPOPSZ(I,K)+RPOPLZ(I,K))/86400.
       DTPIB1(I,K) = (PUP1-PP1-PR1(I,K)*Q1(I,K)-PIB1SZ(I,K)-PIB1LZ(I,K))/86400.
       DTPIB2(I,K) = (PUP2-PP2-PR2(I,K)*Q2(I,K)-PIB2SZ(I,K)-PIB2LZ(I,K))/86400.
       DTPIB3(I,K) = (PUP3-PP3-PR3(I,K)*Q3(I,K)-PIB3SZ(I,K)-PIB3LZ(I,K))/86400.
         
     END DO
   END DO  

   DO I=1,MLOC

!********* Settling

     PFD    = KADPO4*SSI(I,1)/(1.+KADPO4*SSI(I,1))
     PPD    = PFD*PO4(I,1)
     PO4SET = -WSS(I,1)*PPD/(D(I)*DZ(1))
     LPSETL = -WSL(I,1)*LPOP(I,1)/(D(I)*DZ(1))
     RPSETL = -WSR(I,1)*RPOP(I,1)/(D(I)*DZ(1))
     PIB1SETL = -WS1(I,1)*PIB1(I,1)/(D(I)*DZ(1))
     PIB2SETL = -WS2(I,1)*PIB2(I,1)/(D(I)*DZ(1))
     PIB3SETL = -WS3(I,1)*PIB3(I,1)/(D(I)*DZ(1))
     DTPO4(I,1)  = DTPO4(I,1)+PO4SET/86400.
     DTLPOP(I,1) = DTLPOP(I,1)+LPSETL/86400.
     DTRPOP(I,1) = DTRPOP(I,1)+RPSETL/86400.
     DTPIB1(I,1) = DTPIB1(I,1)+PIB1SETL/86400.
     DTPIB2(I,1) = DTPIB2(I,1)+PIB2SETL/86400.
     DTPIB3(I,1) = DTPIB3(I,1)+PIB3SETL/86400.

!********* Settling flux

     FLXSPO4(I,1) = WSS(I,1)*PPD*V2(I,1)/(D(I)*DZ(1)*86400.)
     FLXSPOP(I,1) = (WSL(I,1)*LPOP(I,1)+WSR(I,1)*RPOP(I,1)                     &
                    +WS1(I,1)*PIB1(I,1)+WS2(I,1)*PIB2(I,1)+WS3(I,1)*PIB3(I,1))   &
                    *V2(I,1)/(D(I)*DZ(1)*86400.)
     
   END DO
   
   DO K=2,KBM1
     DO I=1,MLOC

!********* Settling

       PFU    = KADPO4*SSI(I,K-1)/(1.+KADPO4*SSI(I,K-1))
       PPU    = PFU*PO4(I,K-1)
       PFD    = KADPO4*SSI(I,K)/(1.+KADPO4*SSI(I,K))
       PPD    = PFD*PO4(I,K)
       PO4SET = (WSS(I,K-1)*PPU-WSS(I,K)*PPD)/(D(I)*DZ(K))
       LPSETL = (WSL(I,K-1)*LPOP(I,K-1)-WSL(I,K)*LPOP(I,K))/(D(I)*DZ(K))
       RPSETL = (WSR(I,K-1)*RPOP(I,K-1)-WSR(I,K)*RPOP(I,K))/(D(I)*DZ(K))
       PIB1SETL = (WS1(I,K-1)*PIB1(I,K-1)-WS1(I,K)*PIB1(I,K))/(D(I)*DZ(K))
       PIB2SETL = (WS2(I,K-1)*PIB2(I,K-1)-WS2(I,K)*PIB2(I,K))/(D(I)*DZ(K))
       PIB3SETL = (WS3(I,K-1)*PIB3(I,K-1)-WS3(I,K)*PIB3(I,K))/(D(I)*DZ(K))
       DTPO4(I,K)  = DTPO4(I,K)+PO4SET/86400.
       DTLPOP(I,K) = DTLPOP(I,K)+LPSETL/86400.
       DTRPOP(I,K) = DTRPOP(I,K)+RPSETL/86400.
       DTPIB1(I,K) = DTPIB1(I,K)+PIB1SETL/86400.
       DTPIB2(I,K) = DTPIB2(I,K)+PIB2SETL/86400.
       DTPIB3(I,K) = DTPIB3(I,K)+PIB3SETL/86400.

!********* Settling flux

       FLXSPO4(I,K) = WSS(I,K)*PPD*V2(I,K)/(D(I)*DZ(K)*86400.)
       FLXSPOP(I,K) = (WSL(I,K)*LPOP(I,K)+WSR(I,K)*RPOP(I,K)                     &
                    +WS1(I,K)*PIB1(I,K)+WS2(I,K)*PIB2(I,K)+WS3(I,K)*PIB3(I,K))   &
                    *V2(I,K)/(D(I)*DZ(K)*86400.)
     
     END DO
   END DO  

!******* Benthic fluxes

   DO I=1,MLOC
     DTPO4(I,KBM1)  = DTPO4(I,KBM1)+BENPO4(I)/(D(I)*DZ(KBM1))/86400.
     DTLDOP(I,KBM1) = DTLDOP(I,KBM1)+BENDOP(I)/(D(I)*DZ(KBM1))/86400.
   END DO

!******* Resuspension

   IF (SEDIMENT_CALC) THEN

     DO I=1,MLOC
       PF = KADPO4*SSI(I,KBM1)/(1.+KADPO4*SSI(I,KBM1))
       PP = PF*PO4(I,KBM1)
       DTPO4(I,KBM1)  = DTPO4(I,KBM1)+(WSS(I,KBM1)-WSSNET(I))*PP     &
                        /(D(I)*DZ(KBM1))/86400.
       DTLPOP(I,KBM1) = DTLPOP(I,KBM1)+(WSL(I,KBM1)-WSLNET(I))       &
                        *LPOP(I,KBM1)/(D(I)*DZ(KBM1))/86400.
       DTRPOP(I,KBM1) = DTRPOP(I,KBM1)+(WSR(I,KBM1)-WSRNET(I))       &
                        *RPOP(I,KBM1)/(D(I)*DZ(KBM1))/86400.
       DTPIB1(I,KBM1) = DTPIB1(I,KBM1)+(WS1(I,KBM1)-WS1NET(I))*      &
                        PIB1(I,KBM1)/(D(I)*DZ(KBM1))/86400.
       DTPIB2(I,KBM1) = DTPIB2(I,KBM1)+(WS2(I,KBM1)-WS2NET(I))*      &
                        PIB2(I,KBM1)/(D(I)*DZ(KBM1))/86400.
       DTPIB3(I,KBM1) = DTPIB3(I,KBM1)+(WS3(I,KBM1)-WS3NET(I))*      &
                        PIB3(I,KBM1)/(D(I)*DZ(KBM1))/86400.
     END DO
   END IF

!******* SAV and epiphytes

!vjp modified 11/3/2005
!JQI     IF (SAV_CALC) THEN
!JQI       DO I=1,NSAVCELL
!JQI         B=SAVCELL(I)
!JQI         DTPO4(B)   = DTPO4(B)+(PO4SAVW(B)+PO4EPI(B))                  &
!JQI                      /BL(B,3)/86400.
!JQI         DTLDOP(B)  = DTLDOP(B)+(LDOPSAVW(B)+LDOPEPI(B))               &
!JQI                      /BL(B,3)/86400.
!JQI         DTRDOP(B)  = DTRDOP(B)+(RDOPSAVW(B)+RDOPEPI(B))               &
!JQI                      /BL(B,3)/86400.
!JQI         DTLPOP(B)  = DTLPOP(B)+(LPOPSAVW(B)+LPOPEPI(B))               &
!JQI                      /BL(B,3)/86400.
!JQI         DTRPOP(B)  = DTRPOP(B)+(RPOPSAVW(B)+RPOPEPI(B))               &
!JQI                      /BL(B,3)/86400.
!JQI       END DO
!JQI     END IF

!******* Atmospheric loads

   DO I=1,MLOC
     DTPO4(I,1) = DTPO4(I,1)+PRECIP*ATMPO4/(D(I)*DZ(1))
     DTLDOP(I,1)= DTLDOP(I,1)+PRECIP*ATMLDOP/(D(I)*DZ(1))
     DTRDOP(I,1)= DTRDOP(I,1)+PRECIP*ATMRDOP/(D(I)*DZ(1))
   END DO  

!******* Particulate Inorganic Phosphorus

   IF (SOLIDS_CALC .AND. PIP_CALC) THEN

!******  Settling
     DO I=1,MLOC
       FLXSPIP(I,1) = WSSHI(I,1)*PIP(I,1)*V2(I,1)/(D(I)*DZ(1)*86400.)
       DTPIP(I,1)   = -WSSHI(I,1)*PIP(I,1)/(D(I)*DZ(1)*86400.)
     END DO
 
     DO K=2,KBM1
       DO I=1,MLOC
         FLXSPIP(I,K) = WSSHI(I,K)*PIP(I,K)*V2(I,K)/(D(I)*DZ(K)*86400.)
         DTPIP(I,K)   = (WSSHI(I,K-1)*PIP(I,K-1)-WSSHI(I,K)*PIP(I,K))       &
                      /(D(I)*DZ(K)*86400.)
       END DO
     END DO  

!******  Resuspension

     IF (SEDIMENT_CALC) THEN
       DO I=1,MLOC
         DTPIP(I,KBM1) = DTPIP(I,KBM1)+(WSSHI(I,KBM1)-WSSNET(I))    &
                         *PIP(I,KBM1)/(D(I)*DZ(KBM1))/86400.
       END DO
     END IF
          
   END IF
        
   RETURN
   END SUBROUTINE PHOSPH
      

!************************************************************************
!**                S U B R O U T I N E   C O D M N D                   **
!************************************************************************

   SUBROUTINE CODMND(DTCOD)
   USE WQM
   USE ALL_VARS
     
   IMPLICIT NONE
   INTEGER :: I,K
   REAL :: DTCOD(MTLOC,KBM1)
   
   DTCOD  = 0.0

!******* Change in chemical oxygen demand

   DO K=1,KBM1
     DO I=1,MLOC
! Analytical solution
       FTCOD(I,K) = KCOD(I,K)!*EXP(-KTCOD)
!       FTCOD(I,K) = KCOD(I,K)*EXP(KTCOD*(T(I,K)-TRCOD))
       DTCOD(I,K) = (-DO(I,K)/(KHOCOD+DO(I,K))*FTCOD(I,K)*COD(I,K))/86400.
       DTCOD(I,K) = -FTCOD(I,K)*COD(I,K)/86400.
     END DO
   END DO  

!******* Sediment demand

   DO I=1,MLOC
     DTCOD(I,KBM1) = DTCOD(I,KBM1)!+BENCOD(I)/(D(I)*DZ(KBM1))/86400.
   END DO

   RETURN
   END SUBROUTINE CODMND

!************************************************************************
!**                 S U B R O U T I N E   O X Y G E N                  **
!************************************************************************

   SUBROUTINE OXYGEN(DTDO)
   USE WQM
   USE ALL_VARS
     
   IMPLICIT NONE
   REAL :: KRDO
   INTEGER :: I,K
   REAL :: FRDO1, FRDO2, FRDO3, CP1, CP2, CP3, ALGDO, DOR1, DOR2, DOR3,  &
           DOP1, DOP2, DOP3
   REAL :: FACTOR, CHLORN, TDOS, RNU 
   REAL :: DTDO(MTLOC,KBM1)
   
   DTDO   = 0.0

   DO K=1,KBM1
     DO I=1,MLOC

!********* Nitrification

       NITRIF(I,K) = AONT*NT(I,K)

!********* Uptake/respiration

       FRDO1  = (1.-FCLD1-FCRD1-FCLP1-FCRP1)*DO(I,K)/(KHR1+DO(I,K))
       FRDO2  = (1.-FCLD2-FCRD2-FCLP2-FCRP2)*DO(I,K)/(KHR2+DO(I,K))
       FRDO3  = (1.-FCLD3-FCRD3-FCLP3-FCRP3)*DO(I,K)/(KHR3+DO(I,K))
       CP1    = P1(I,K)*PRSP1+BM1(I,K)
       CP2    = P2(I,K)*PRSP2+BM2(I,K)
       CP3    = P3(I,K)*PRSP3+BM3(I,K)
       ALGDO  = (FRDO1*CP1*B1(I,K)+FRDO2*CP2*B2(I,K)                       &
                +FRDO3*CP3*B3(I,K))*AOCR
!       IF(I.eq.25) THEN
!          Print*, 'PN1,P1', PN1(I,K),P1(I,K)
!          Print*, 'FRDO1,CP1', FRDO1,CP1
!          Print*, 'AOCR,B1', AOCR, B1(I,K)
!       ENDIF
!       DOR1   = ((1.3-0.3*PN1(I,K))*P1(I,K)-FRDO1*CP1)*AOCR*B1(I,K)
! changed below for analytical solution
       DOR1   = (1.3*P1(I,K)-CP1)*AOCR*B1(I,K)
       DOR2   = ((1.3-0.3*PN2(I,K))*P2(I,K)-FRDO2*CP2)*AOCR*B2(I,K)          
       DOR3   = ((1.3-0.3*PN3(I,K))*P3(I,K)-FRDO3*CP3)*AOCR*B3(I,K)
       DOP1   = FDOP*PR1(I,K)*AOCR*DO(I,K)/(KHR1+DO(I,K))
       DOP2   = FDOP*PR2(I,K)*AOCR*DO(I,K)/(KHR2+DO(I,K))
       DOP3   = FDOP*PR3(I,K)*AOCR*DO(I,K)/(KHR3+DO(I,K))
       DOPR(I,K)  = DOP1+DOP2+DOP3
       DCOD(I,K)  = DO(I,K)/(KHOCOD+DO(I,K))*FTCOD(I,K)*COD(I,K)
!       DCOD(I,K)  = FTCOD(I,K)*COD(I,K)
       DDOC(I,K)  = AOCR*(MNLLDOC(I,K)+MNLRDOC(I,K))
!       IF(I.eq.11126) THEN
!          Print*, 'MNLLDOC, MNLRDOC in wqm_kin', MNLLDOC(I,K), MNLRDOC(I,K)
!       ENDIF
       DORALG(I,K)= DOR1+DOR2+DOR3

!********* Change in dissolved oxygen

       DTDO(I,K)  = (DOR1+DOR2+DOR3-DOP1-DOP2-DOP3-DDOC(I,K)-DCOD(I,K)       &
                  -NITRIF(I,K)-DOSZ(I,K)-DOLZ(I,K))/86400.
!RGl added print here
!         IF(I.eq.11126) THEN
!            Print*, 'DOR1,DDOC', DOR1, DDOC(I,K)
!            Print*, 'DOP1,DOP2,DOP3', DOP1,DOP2,DOP3
!            Print*, 'DDOC,DCOD', DDOC(I,K),DCOD(I,K)
!            Print*, 'NITRIF', NITRIF(I,K)
!            Print*, 'DOSZ,DOLZ', DOSZ(I,K), DOLZ(I,K)
!         ENDIF
       RESP(I,K)  = ALGDO + DOPR(I,K)+DDOC(I,K)+DCOD(I,K)+NITRIF(I,K)+DOSZ(I,K)+DOLZ(I,K)
     END DO
   END DO  

!******* Reaeration

   FACTOR = AREAR*(BREAR*WMS)**CREAR
   DO I=1,MLOC
     CHLORN    = SALT(I,1)/1.80655
     TDOS      = T(I,1)
     RNU       = 0.54 + 0.7*TDOS/30 - 0.07*SALT(I,1)/35.
!     KRDO      = 0.157*RNU*(1.5*WMS)**1.5
     KRDO      = FACTOR*RNU
!     KRDO      = 1.46785
     DOS       = 14.5532+TDOS*(0.0054258*TDOS-0.38217)-CHLORN     &
                    *(0.1665+TDOS*(9.796E-5*TDOS-5.866E-3))
     DTDO(I,1) = DTDO(I,1)+KRDO/(D(I)*DZ(1))*(DOS-DO(I,1))/86400.
     DTDO(I,2) = DTDO(I,2)+KRDO/(D(I)*DZ(2))*(DOS-DO(I,2))/86400.
     DTDO(I,3) = DTDO(I,3)+KRDO/(D(I)*DZ(3))*(DOS-DO(I,3))/86400.
!     IF(I.eq.11126) Print*, 'KRDO/DZ', KRDO/(D(I)*DZ(1))
!     if(I.eq.11126)write(2235,*)'middle',KRDO,TDOS,CHLORN,DOS,DO(I,1)
!     if(I.eq.26)write(2235,*)'rea_river',KRDO,TDOS,CHLORN,DOS,DO(I,1)
!     if(I.eq.25526)write(2235,*)'BND',KRDO,TDOS,CHLORN,DOS,DO(I,1)
   END DO

!******* Sediment oxygen demand

   DO I=1,MLOC
     DTDO(I,KBM1) = DTDO(I,KBM1)!+BENDO(I)/(D(I)*DZ(KBM1))/86400.
   END DO  

!******* SAV and epiphytes

!vjp modified 11/3/2005
!JQI   IF (SAV_CALC) THEN
!JQI     DO I=1,NSAVCELL
!JQI       B=SAVCELL(I)
!JQI       DTDO(B) = DTDO(B)+(DOSAV(B)+DOEPI(B)+SEDDOSAV(B))/BL(B,3)/86400.
!JQI     END DO
!JQI   END IF

   RETURN
   END SUBROUTINE OXYGEN

!************************************************************************
!**                  S U B R O U T I N E   S I L I C A                 **
!************************************************************************

   SUBROUTINE SILICA(DTSU,DTSA,FLXSSI)
   USE WQM
   USE ALL_VARS
     
   IMPLICIT NONE
   INTEGER :: I,K
   REAL :: SUP, SP1, SP2, SP3, PFU, PF, SASETL,SUSETL 
   REAL,DIMENSION(MTLOC,KBM1) :: DTSU, DTSA, FLXSSI

   DTSU = 0.0
   DTSA = 0.0
   FLXSSI = 0.0 
   
   DO K=1,KBM1
     DO I=1,MLOC

!********* Particulate silica dissolution

       PSD(I,K) = KSUA(I,K)*SU(I,K)*EXP(KTSUA*(T(I,K)-TRSUA))
 
!********* Predation

       SAP(I,K) = FSAP*(PR1(I,K)*ASC1+PR2(I,K)*ASC2+PR3(I,K)*ASC3)
       SUP = (1.-FSAP)*(PR1(I,K)*ASC1+PR2(I,K)*ASC2+PR3(I,K)*ASC3)

!********* Algal uptake and respiration

       ALGUP(I,K) = P1(I,K)*ASC1*B1(I,K)+P2(I,K)*ASC2*B2(I,K)+P3(I,K)*ASC3*B3(I,K)
       SP1 = ASC1*(P1(I,K)*PRSP1+BM1(I,K))*B1(I,K)
       SP2 = ASC2*(P2(I,K)*PRSP2+BM2(I,K))*B2(I,K)
       SP3 = ASC3*(P3(I,K)*PRSP3+BM3(I,K))*B3(I,K)
       ALGRES(I,K) = SP1+SP2+SP3

!********* Change in silica

       DTSU(I,K) = (SUP-PSD(I,K)+ALGRES(I,K)+SUSZ(I,K)+SULZ(I,K))/86400.
       DTSA(I,K) = (SAP(I,K)+PSD(I,K)-ALGUP(I,K)+SASZ(I,K)+SALZ(I,K))/86400.

     END DO
   END DO  

   DO I=1,MLOC

!********* Settling

     PF     = KADSA*SSI(I,1)/(1.+KADSA*SSI(I,1))
     SASETL = -WSS(I,1)*PF*SA(I,1)/(D(I)*DZ(1))
     SUSETL = -WSU(I,1)*SU(I,1)/(D(I)*DZ(1))

!********* Change in silica

     DTSU(I,1) = DTSU(I,1)+SUSETL/86400.
     DTSA(I,1) = DTSA(I,1)+SASETL/86400.

!********* Settling fluxes

     FLXSSI(I,1) = (WSS(I,1)*PF*SA(I,1)+WSU(I,1)*SU(I,1))*V2(I,1)/(D(I)*DZ(1)*86400.)

   END DO

   DO K=2,KBM1
     DO I=1,MLOC

!********* Settling

       PFU    = KADSA*SSI(I,K-1)/(1.+KADSA*SSI(I,K-1))
       PF     = KADSA*SSI(I,K)/(1.+KADSA*SSI(I,K))
       SASETL = (WSS(I,K-1)*PFU*SA(I,K-1)-WSS(I,K)*PF*SA(I,K))/(D(I)*DZ(K))
       SUSETL = (WSU(I,K-1)*SU(I,K-1)-WSU(I,K)*SU(I,K))/(D(I)*DZ(K))

!********* Change in silica

       DTSU(I,K) = DTSU(I,K)+SUSETL/86400.
       DTSA(I,K) = DTSA(I,K)+SASETL/86400.

!********* Settling fluxes

       FLXSSI(I,K) = (WSS(I,K)*PF*SA(I,K)+WSU(I,K)*SU(I,K))*V2(I,K)/(D(I)*DZ(K)*86400.)

     END DO
   END DO  

!******* Benthic fluxes

   DO I=1,MLOC
     DTSA(I,KBM1) = DTSA(I,KBM1)+BENSA(I)/(D(I)*DZ(KBM1))/86400.
   END DO

!******* Resuspension

   IF (SEDIMENT_CALC) THEN

     DO I=1,MLOC
       PF           = KADSA*SSI(I,KBM1)/(1.+KADSA*SSI(I,KBM1))
       DTSA(I,KBM1) = DTSA(I,KBM1)+(WSS(I,KBM1)-WSSNET(I))*PF        &
                      *SA(I,KBM1)/(D(I)*DZ(KBM1))/86400.
       DTSU(I,KBM1) = DTSU(I,KBM1)+(WSU(I,KBM1)-WSUNET(I))           &
                      *SU(I,KBM1)/(D(I)*DZ(KBM1))/86400.
     END DO			
   END IF

   RETURN
   END SUBROUTINE SILICA

!************************************************************************
!**               S U B R O U T I N E   B E N C O M P                  **
!************************************************************************

   SUBROUTINE BEN_FLUX
   USE WQM
   USE ALL_VARS
     
   IMPLICIT NONE
   INTEGER :: I,K
   REAL :: FTSOD, FTNH4, FTNO3, FTPO4, FTSA, FTDOC 
   REAL :: PF, PIOP

   DO I=1,MLOC

!******* Calculate temperature effects

     FTSOD    = EXP(KSO*(T(I,KBM1)-TRSO))
     FTNH4    = EXP(KSNH4*(T(I,KBM1)-TRSNH4))
     FTNO3    = EXP(KSNO3*(T(I,KBM1)-TRSNO3))
     FTPO4    = EXP(KSPO4*(T(I,KBM1)-TRSPO4))
     FTSA     = EXP(KSSA*(T(I,KBM1)-TRSSA))
     FTDOC    = EXP(KSDOC*(T(I,KBM1)-TRSDOC))

!********* Dissolved oxygen effect on sediment oxygen demand

     BENDO(I) = BENDOB(I)*FTSOD*DO(I,KBM1)/(KHSO+DO(I,KBM1))
     BENCOD(I) = BENCODB(I)-BENDOB(I)*FTSOD*KHSO/(KHSO+DO(I,KBM1))

!********* Balance of nutrients

     BENNH4(I) = BENNH4B(I)*FTNH4
     BENNO3(I) = BENNO3B(I)+FTNO3*MTCNO3*(SEDNO3-NO3(I,KBM1))
     BENPO4(I) = BENPO4B(I)*FTPO4
     BENSA(I)  = BENSAB(I)*FTSA
     BENDOC(I) = BENDOCB(I)*FTDOC

   END DO

!******* Compute particle flux to sediments for mass-balance purposes

   DO I=1,MLOC

!******* First zero flux accumlators

     PCFWS(I) = 0.
     PNFWS(I) = 0.
     PPFWS(I) = 0.
     PSFWS(I) = 0.

!******* Now accumulate fluxes of labile and refractory particles

     PPFWS(I) = PPFWS(I)-WSL(I,KBM1)*LPOP(I,KBM1)-WSR(I,KBM1)*RPOP(I,KBM1)
     PNFWS(I) = PNFWS(I)-WSL(I,KBM1)*LPON(I,KBM1)-WSR(I,KBM1)*RPON(I,KBM1)
     PCFWS(I) = PCFWS(I)-WSL(I,KBM1)*LPOC(I,KBM1)-WSR(I,KBM1)*RPOC(I,KBM1)
     PSFWS(I) = PSFWS(I)-WSU(I,KBM1)*SU(I,KBM1)

!******* Now accumulate fluxes of algal biomass

     PPFWS(I) = PPFWS(I)-Q1(I,KBM1)*WS1(I,KBM1)*B1(I,KBM1)               &
                -Q2(I,KBM1)*WS2(I,KBM1)*B2(I,KBM1)                       &
                -Q3(I,KBM1)*WS3(I,KBM1)*B3(I,KBM1)
     PNFWS(I) = PNFWS(I)-WS1(I,KBM1)*ANC1*B1(I,KBM1)                     &
                -WS3(I,KBM1)*ANC3*B3(I,KBM1)-WS2(I,KBM1)*ANC2*B2(I,KBM1)
     PCFWS(I) = PCFWS(I)-WS1(I,KBM1)*B1(I,KBM1)                          &
                -WS2(I,KBM1)*B3(I,KBM1)-WS2(I,KBM1)*B2(I,KBM1)
     PSFWS(I) = PSFWS(I)-WS1(I,KBM1)*ASC1*B1(I,KBM1)                     &
                -WS2(I,KBM1)*ASC2*B2(I,KBM1)-WS3(I,KBM1)*ASC3*B3(I,KBM1)

!******* Accumulate adsorbed phosphate and silica

     PF           = KADPO4*SSI(I,KBM1)/(1.+KADPO4*SSI(I,KBM1))
     PIOP          = PF*PO4(I,KBM1)
     PPFWS(I)     = PPFWS(I)-WSS(I,KBM1)*PIOP
     PF           = KADSA*SSI(I,KBM1)/(1.+KADSA*SSI(I,KBM1))
     PSFWS(I)     = PSFWS(I)-WSS(I,KBM1)*PF*SA(I,KBM1)

   END DO

   RETURN
   END SUBROUTINE BEN_FLUX


