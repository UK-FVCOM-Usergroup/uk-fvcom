!************************************************************************
!** This is a new version of algal input and kinetics.  Created for    **
!** The Tributary Refinements portion of the Chesapeake Bay study.     **
!**                                                                    **
!** Refinements include creation of an algal input routine, a new      **
!** P vs. I curve, a silica fraction for all algal groups, option to   **
!** spatially vary inputs, lookup table for temperature functions,     **
!** switch to uE from langleys, option to specify minimum or product   **
!** function for growth limitations.                                   **
!**                                                                    **
!**                  Algal  Subroutines for CE-QUAL-ICM                **
!**                                                                    **
!**                  Chesapeake Bay Tributary Refinements              **
!**                         February 7, 1996                           **
!**                                                                    **
!**                    Water Quality Modeling Group                    **
!**                    U.S. Army Corps of Engineers                    **
!**                    Waterways Experiment Station                    **
!**                    Vicksburg, Mississippi 39180                    **
!**                                                                    **
!************************************************************************

   MODULE ALGAL
   USE WQM
   USE FILE_INFO
   USE ALL_VARS
   IMPLICIT NONE
   SAVE
!
   CHARACTER(LEN=72) :: ALG_TITLE(6)
   CHARACTER(LEN=8)  :: SPVAR1, SPVAR2, SPVAR3
   CHARACTER(LEN=8)  :: PRINT1, PRINT2, PRINT3
   CHARACTER(LEN=8)  :: TPVAR,  TPRINT, TB2GR,  PRINTB2
   REAL :: KTG11, KTG12, KTB1, KTG21, KTG22, KTB2, KTG31 
   REAL :: KTG32, KTB3,  KTPR
   REAL :: NETP1, NETP2, NETP3, IK, ITTOP, ITAVG, NUTLIM
   REAL,ALLOCATABLE :: XL(:), STOX1(:,:), STOX2(:,:), STOX3(:,:)
   REAL,ALLOCATABLE :: ITBOT(:,:), NL1NNF(:,:)
   REAL :: TRPR, TMP1, TR1, TMP2, TR2, TMP3, TR3, TLOOK
   REAL :: SALTOX, DIN, DSIL, ALPHA, OPTDEPTH
   REAL :: FI01, FI02, FI03, GPP1, GPP2, GPP3, TREC
!
   CONTAINS
!

!************************************************************************
!**             S U B R O U T I N E   A L G _ R E A D                  **
!************************************************************************

   SUBROUTINE ALG_READ
#  if defined (MULTIPROCESSOR)
   USE MOD_PAR
#  endif
   IMPLICIT NONE
   INTEGER :: I, J, F, K
   REAL, ALLOCATABLE :: RTMP21(:,:),RTMP22(:,:),RTMP23(:,:)

!vjp  OPEN(UNIT=150,FILE='CCHL.OUT',STATUS='UNKNOWN')

! TITLE CARDS

     READ(AGR,1010) (ALG_TITLE(J),J=1,6)

! READ PARAMETERS WHICH ARE IDENTICAL FOR ALL GROUPS

     READ (AGR,1040)  MINPROD
     READ (AGR,1030)  TRPR,   KTPR
     READ (AGR,1030)  FNIP,   FNUP,   FNLDP,  FNRDP,  FNLPP,   FNRPP
     READ (AGR,1030)  FPIP,   FPLDP,  FPRDP,  FPLPP,  FPRPP
     READ (AGR,1030)  FDOP,   FCLDP,  FCRDP,  FCLPP,  FCRPP
     READ (AGR,1030)  FSAP
! READ SPATIALLY-INVARIANT PARAMETERS FOR ALGAL GROUP 1

     READ(AGR,1030) ANC1, Q01, ASC1, STF1
     READ(AGR,1030) CHLCMN1, ACHLC1, BCHLC1, CCHLC1      
     READ(AGR,1030) KHN1,KHP1,KHS1,KHR1,KHST1,KHNFIX,FNFIX
     READ(AGR,1030) ALPHMIN1,ALPHRAT1,PRSP1,VMAX1
     READ(AGR,1030) TMP1,TR1
     READ(AGR,1030) KTG11,KTG12,KTB1
     READ(AGR,1030) FNI1,FNLD1,FNRD1,FNLP1,FNRP1
     READ(AGR,1030) FPI1,FPLD1,FPRD1,FPLP1,FPRP1
     READ(AGR,1030) FCLD1,FCRD1,FCLP1,FCRP1

! READ SPATIALLY-INVARIANT PARAMETERS FOR ALGAL GROUP 2

     READ(AGR,1030) ANC2, Q02, ASC2, STF2
     READ(AGR,1030) CHLCMN2, ACHLC2, BCHLC2, CCHLC2      
     READ(AGR,1030) KHN2,KHP2,KHS2,KHR2,KHST2
     READ(AGR,1030) ALPHMIN2,ALPHRAT2,PRSP2,VMAX2
     READ(AGR,1030) TMP2,TR2
     READ(AGR,1030) KTG21,KTG22,KTB2
     READ(AGR,1030) FNI2,FNLD2,FNRD2,FNLP2,FNRP2
     READ(AGR,1030) FPI2,FPLD2,FPRD2,FPLP2,FPRP2
     READ(AGR,1030) FCLD2,FCRD2,FCLP2,FCRP2

! READ SPATIALLY-INVARIANT PARAMETERS FOR ALGAL GROUP 3

     READ(AGR,1030) ANC3, Q03, ASC3, STF3
     READ(AGR,1030) CHLCMN3, ACHLC3, BCHLC3, CCHLC3      
     READ(AGR,1030) KHN3,KHP3,KHS3,KHR3,KHST3
     READ(AGR,1030) ALPHMIN3,ALPHRAT3,PRSP3,VMAX3
     READ(AGR,1030) TMP3,TR3
     READ(AGR,1030) KTG31,KTG32,KTB3
     READ(AGR,1030) FNI3,FNLD3,FNRD3,FNLP3,FNRP3
     READ(AGR,1030) FPI3,FPLD3,FPRD3,FPLP3,FPRP3
     READ(AGR,1030) FCLD3,FCRD3,FCLP3,FCRP3

! CREATE LOOKUP TABLE FOR TEMPERATURE EFFECTS
     DO I = -50,400
       TLOOK = REAL(I)/10.
       IF (TLOOK < TMP1) THEN
         FT1(I) = EXP(-KTG11*(TLOOK-TMP1)**2)
       ELSE
         FT1(I) = EXP(-KTG12*(TMP1-TLOOK)**2)
       END IF
       FTBM1(I)=EXP(KTB1*(TLOOK-TR1))
       IF (TLOOK < TMP2) THEN
         FT2(I) = EXP(-KTG21*(TLOOK-TMP2)**2)
       ELSE
         FT2(I) = EXP(-KTG22*(TMP2-TLOOK)**2)
       END IF
       FTBM2(I)=EXP(KTB2*(TLOOK-TR2))
       IF (TLOOK < TMP3) THEN
         FT3(I) = EXP(-KTG31*(TLOOK-TMP3)**2)
       ELSE
         FT3(I) = EXP(-KTG32*(TMP3-TLOOK)**2)
       END IF
       FTBM3(I)=EXP(KTB3*(TLOOK-TR3))
       FTPR(I)=EXP(KTPR*(TLOOK-TRPR))
     END DO

     KHNAVG = (KHN1+KHN2+KHN3)/3.
     KHPAVG = (KHP1+KHP2+KHP3)/3.

! ARE REMAINING GROUP 1 PARAMETERS SPATIALLY VARYING?

     READ(AGR,1040) SPVAR1, PRINT1
     IF (SPVAR1 == 'CONSTANT') THEN
       READ(AGR,1030) PM1(1,1), BMR1(1,1), BPR1(1,1)
       DO K=1,KBM1
         DO I=1,MTLOC
           PM1(I,K) = PM1(1,1)
           BMR1(I,K) = BMR1(1,1)
           BPR1(I,K) = BPR1(1,1)
	 END DO  
       END DO
     ELSE
       ALLOCATE(RTMP21(MGL,KBM1));     RTMP21 = 0.0
       ALLOCATE(RTMP22(MGL,KBM1));     RTMP22 = 0.0
       ALLOCATE(RTMP23(MGL,KBM1));     RTMP23 = 0.0
       DO K=1,KBM1
         DO I=1,MGL
           READ(AGR,1032) RTMP21(I,K), RTMP22(I,K), RTMP23(I,K)
	 END DO  
       END DO
       IF(SERIAL)THEN
         PM1  = RTMP21
         BMR1 = RTMP22
         BPR1 = RTMP23
       END IF
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO K=1,KBM1
           DO I=1,MLOC
             PM1(I,K)  = RTMP21(NGID(I),K)
             BMR1(I,K) = RTMP22(NGID(I),K)
             BPR1(I,K) = RTMP23(NGID(I),K)
           END DO
           DO I=1,NHN
             PM1(I+MLOC,K)  = RTMP21(HN_LST(I),K)
             BMR1(I+MLOC,K) = RTMP22(HN_LST(I),K)
             BPR1(I+MLOC,K) = RTMP23(HN_LST(I),K)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(RTMP21,RTMP22,RTMP23)
     END IF      

! ARE REMAINING GROUP 2 PARAMETERS SPATIALLY VARYING?

     READ(AGR,1040) SPVAR2, PRINT2
     IF (SPVAR2 == 'CONSTANT') THEN
       READ(AGR,1030) PM2(1,1), BMR2(1,1), BPR2(1,1)
       DO K=1,KBM1
         DO I=1,MTLOC
           PM2(I,K) = PM2(1,1)
           BMR2(I,K) = BMR2(1,1)
           BPR2(I,K) = BPR2(1,1)
	 END DO  
       END DO
     ELSE
       ALLOCATE(RTMP21(MGL,KBM1));    RTMP21 = 0.0
       ALLOCATE(RTMP22(MGL,KBM1));    RTMP22 = 0.0
       ALLOCATE(RTMP23(MGL,KBM1));    RTMP23 = 0.0
       DO K=1,KBM1
         DO I=1,MGL
           READ(AGR,1032) RTMP21(I,K), RTMP22(I,K), RTMP23(I,K)
	 END DO  
       END DO
       IF(SERIAL)THEN
         PM2  = RTMP21
         BMR2 = RTMP22
         BPR2 = RTMP23
       END IF
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO K=1,KBM1
           DO I=1,MLOC
             PM2(I,K)  = RTMP21(NGID(I),K)
             BMR2(I,K) = RTMP22(NGID(I),K)
             BPR2(I,K) = RTMP23(NGID(I),K)
           END DO
           DO I=1,NHN
             PM2(I+MLOC,K)  = RTMP21(HN_LST(I),K)
             BMR2(I+MLOC,K) = RTMP22(HN_LST(I),K)
             BPR2(I+MLOC,K) = RTMP23(HN_LST(I),K)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(RTMP21,RTMP22,RTMP23)
     END IF      

! ARE REMAINING GROUP 3 PARAMETERS SPATIALLY VARYING?

     READ(AGR,1040) SPVAR3, PRINT3
     IF (SPVAR3 == 'CONSTANT') THEN
       READ(AGR,1030) PM3(1,1), BMR3(1,1), BPR3(1,1)
       DO K=1,KBM1
         DO I=1,MTLOC
           PM3(I,K) = PM3(1,1)
           BMR3(I,K) = BMR3(1,1)
           BPR3(I,K) = BPR3(1,1)
	 END DO  
       END DO
     ELSE
       ALLOCATE(RTMP21(MGL,KBM1));    RTMP21 = 0.0
       ALLOCATE(RTMP22(MGL,KBM1));    RTMP22 = 0.0
       ALLOCATE(RTMP23(MGL,KBM1));    RTMP23 = 0.0
       DO K=1,KBM1
         DO I=1,MGL
           READ(AGR,1032) RTMP21(I,K), RTMP22(I,K), RTMP23(I,K)
	 END DO  
       END DO
       IF(SERIAL)THEN
         PM3  = RTMP21
         BMR3 = RTMP22
         BPR3 = RTMP23
       END IF
#      if defined (MULTIPROCESSOR)
       IF(PAR)THEN
         DO K=1,KBM1
           DO I=1,MLOC
             PM3(I,K)  = RTMP21(NGID(I),K)
             BMR3(I,K) = RTMP22(NGID(I),K)
             BPR3(I,K) = RTMP23(NGID(I),K)
           END DO
           DO I=1,NHN
             PM3(I+MLOC,K)  = RTMP21(HN_LST(I),K)
             BMR3(I+MLOC,K) = RTMP22(HN_LST(I),K)
             BPR3(I+MLOC,K) = RTMP23(HN_LST(I),K)
           END DO
         END DO
       END IF
#      endif
       DEALLOCATE(RTMP21,RTMP22,RTMP23)
     END IF

! TIME DEPENDENCE OF PREDATION BY HIGHER TROPHIC LEVELS

     READ(AGR,1040) TPVAR, TPRINT
     IF (TPVAR == 'CONSTANT') THEN
       READ(AGR,1060) TVPR(1)
       DO J=2,366
         TVPR(J) = TVPR(1)
       END DO
     ELSE
       READ(AGR,1060) (TVPR(J), J=1,366)
     END IF 

! TIME DEPENDENCE OF GROWTH BY SPRING ALGAL GROUP

     READ(AGR,1040) TB2GR, PRINTB2
     IF (TB2GR == 'CONSTANT') THEN
       READ(AGR,1060) B2GR(1)
       DO J=2,366
         B2GR(J) = B2GR(1)
       END DO
     ELSE
       READ(AGR,1060) (B2GR(J), J=1,366)
     END IF 

!***** Input FORMAT statements

1010 FORMAT(A72)
1030 FORMAT(//(8X,9F8.0))
1032 FORMAT(8X,9F8.0)
1040 FORMAT(//8X,8A8)
1060 FORMAT(//(16X,F8.0))

! OUTPUT WHAT WAS INPUT
     OPEN(ALO,FILE=ALOFN)
     IF(MSR)WRITE(ALO,2002) (ALG_TITLE(J),J=1,6)

! WRITE ALGAL PROPERTIES WHICH ARE NOT SPATIALLY VARYING

     IF(MSR)WRITE(ALO,3000)
     IF(MSR)WRITE(ALO,3010) CHLCMN1, CHLCMN2, CHLCMN3
     IF(MSR)WRITE(ALO,3012) ACHLC1, ACHLC2, ACHLC3
     IF(MSR)WRITE(ALO,3014) BCHLC1, BCHLC2, BCHLC3
     IF(MSR)WRITE(ALO,3016) CCHLC1, CCHLC2, CCHLC3
     IF(MSR)WRITE(ALO,3020) ANC1, ANC2, ANC3
     IF(MSR)WRITE(ALO,3022) Q01,  Q02,  Q03
     IF(MSR)WRITE(ALO,3030) ASC1, ASC2, ASC3
     IF(MSR)WRITE(ALO,3072) STF1, STF2, STF3
     IF(MSR)WRITE(ALO,3040) KHN1, KHN2, KHN3
     IF(MSR)WRITE(ALO,3050) KHP1, KHP2, KHP3
     IF(MSR)WRITE(ALO,3042) KHNFIX
     IF(MSR)WRITE(ALO,3043) FNFIX
     IF(MSR)WRITE(ALO,3060) KHS1, KHS2, KHS3
     IF(MSR)WRITE(ALO,3070) KHR1, KHR2, KHR3
     IF(MSR)WRITE(ALO,3074) KHST1,KHST2,KHST3
     IF(MSR)WRITE(ALO,3076) VMAX1, VMAX2, VMAX3
     IF(MSR)WRITE(ALO,3080) ALPHMIN1, ALPHMIN2, ALPHMIN3
     IF(MSR)WRITE(ALO,3090) ALPHRAT1, ALPHRAT2, ALPHRAT3
     IF(MSR)WRITE(ALO,4000) PRSP1, PRSP2, PRSP3
     IF(MSR)WRITE(ALO,4010) TMP1, TMP2, TMP3
     IF(MSR)WRITE(ALO,5020) TR1, TR2, TR3
     IF(MSR)WRITE(ALO,5030) KTG11, KTG21, KTG31
     IF(MSR)WRITE(ALO,5040) KTG12, KTG22, KTG32
     IF(MSR)WRITE(ALO,5050) KTB1, KTB2, KTB3

! WRITE DISTRIBUTION OF BIOMASS UPON MORTALITY

     IF(MSR)WRITE(ALO,4020)
     IF(MSR)WRITE(ALO,4030) FNI1,FNLD1,FNRD1,FNLP1,FNRP1
     IF(MSR)WRITE(ALO,4040) FPI1,FPLD1,FPRD1,FPLP1,FPRP1
     IF(MSR)WRITE(ALO,4050) FCLD1,FCRD1,FCLP1,FCRP1
     IF(MSR)WRITE(ALO,4060)
     IF(MSR)WRITE(ALO,4030) FNI2,FNLD2,FNRD2,FNLP2,FNRP2
     IF(MSR)WRITE(ALO,4040) FPI2,FPLD2,FPRD2,FPLP2,FPRP2
     IF(MSR)WRITE(ALO,4050) FCLD2,FCRD2,FCLP2,FCRP2
     IF(MSR)WRITE(ALO,4070)
     IF(MSR)WRITE(ALO,4030) FNI3,FNLD3,FNRD3,FNLP3,FNRP3
     IF(MSR)WRITE(ALO,4040) FPI3,FPLD3,FPRD3,FPLP3,FPRP3
     IF(MSR)WRITE(ALO,4050) FCLD3,FCRD3,FCLP3,FCRP3
     IF(MSR)WRITE(ALO,4090)
     IF(MSR)WRITE(ALO,4030) FNIP,FNLDP,FNRDP,FNLPP,FNRPP
     IF(MSR)WRITE(ALO,4040) FPIP,FPLDP,FPRDP,FPLPP,FPRPP
     IF(MSR)WRITE(ALO,4080) FDOP,FCLDP,FCRDP,FCLPP,FCRPP
     IF(MSR)WRITE(ALO,4082) FSAP,(1.-FSAP)
 
     IF (MINPROD == ' MINIMUM' .OR. MINPROD == ' PRODUCT') THEN
       IF(MSR)WRITE(ALO,2010) MINPROD
     ELSE
       IF(MSR)WRITE(ALO,2020) MINPROD
       STOP
     END IF

! WRITE SPATIALLY-VARYING PARAMETERS FOR GROUP 1

     IF(MSR)WRITE(ALO,2000)
     IF (SPVAR1 == 'CONSTANT') THEN
       IF(MSR)WRITE(ALO,2085)
     ELSE
       IF(MSR)WRITE(ALO,2086)
     END IF
     IF(MSR)WRITE(ALO,5060)
     IF (PRINT1 /= '     ALL') THEN
       IF(MSR)WRITE(ALO,5070) PM1(1,1), BMR1(1,1), BPR1(1,1)
     ELSE
       DO K=1,KBM1
         DO I=1,MLOC
	   IF(MSR)WRITE(ALO,5080) I, K, PM1(I,K), BMR1(I,K), BPR1(I,K)
	 END DO
       END DO	   
     END IF
      
! WRITE SPATIALLY-VARYING PARAMETERS FOR GROUP 2

     IF(MSR)WRITE(ALO,2005)
     IF (SPVAR2 == 'CONSTANT') THEN
       IF(MSR)WRITE(ALO,2085)
     ELSE
       IF(MSR)WRITE(ALO,2086)
     END IF
     IF(MSR)WRITE(ALO,5060)
     IF (PRINT2 /= '     ALL') THEN
       IF(MSR)WRITE(ALO,5070) PM2(1,1), BMR2(1,1), BPR2(1,1)
     ELSE
       DO K=1,KBM1
         DO I=1,MLOC
           IF(MSR)WRITE(ALO,5080) I, K, PM2(I,K), BMR2(I,K), BPR2(I,K)
	 END DO
       END DO	   
     END IF

! WRITE SPATIALLY-VARYING PARAMETERS FOR GROUP 3

     IF(MSR)WRITE(ALO,2006)
     IF (SPVAR3 == 'CONSTANT') THEN
       IF(MSR)WRITE(ALO,2085)
     ELSE
       IF(MSR)WRITE(ALO,2086)
     END IF
     IF(MSR)WRITE(ALO,5060)
     IF (PRINT3 /= '     ALL') THEN
       IF(MSR)WRITE(ALO,5070) PM3(1,1), BMR3(1,1), BPR3(1,1)
     ELSE
       DO K=1,KBM1
         DO I=1,MLOC
           IF(MSR)WRITE(ALO,5080) I, K, PM3(I,K), BMR3(I,K), BPR3(I,K)
	 END DO
       END DO	   
     END IF

! WRITE TERMS FOR PREDATION BY HIGHER TROPHIC LEVELS

     IF (TPVAR == 'CONSTANT') THEN
       IF(MSR)WRITE(ALO,2087)
     ELSE
       IF(MSR)WRITE(ALO,2088)
     END IF 
     IF(MSR)WRITE(ALO,2040) TRPR, KTPR
     IF(MSR)WRITE(ALO,2092)
     IF (TPRINT /= '     ALL') THEN
       IF(MSR)WRITE(ALO,2090) (J, TVPR(J), J=1,1)
     ELSE
       IF(MSR)WRITE(ALO,2090) (J, TVPR(J), J=1,366)
     END IF
      
! WRITE TIME DEPENDENCE OF GROWTH OF SPRING ALGAL GROUP

     IF (TB2GR == 'CONSTANT') THEN
       IF(MSR)WRITE(ALO,2094)
     ELSE
       IF(MSR)WRITE(ALO,2095)
     END IF 
     IF(MSR)WRITE(ALO,2093)
     IF (PRINTB2 /= '     ALL') THEN
       IF(MSR)WRITE(ALO,2090) (J, B2GR(J), J=1,1)
     ELSE
       IF(MSR)WRITE(ALO,2090) (J, B2GR(J), J=1,366)
     END IF
      
      
!***** Output FORMAT statements

2000 FORMAT(/' ALGAL GROUP 1')
2002 FORMAT(1X,A72)
2005 FORMAT(/' ALGAL GROUP 2')
2006 FORMAT(/' ALGAL GROUP 3')
2010 FORMAT(/A8,' FORMULATION SELECTED FOR LIGHT AND NUTRIENT',' LIMITATIONS')
2020 FORMAT(/' MINPROD INCORRECTLY SPECIFIED AS ',A8)
2040 FORMAT(' PREDATION SPECIFIED AT ',F8.2,' C.'/              &
            ' TEMPERATURE EFFECT = ',F8.3,' PER DEGREE')
2085 FORMAT(/' REMAINING PARAMETERS ARE SPATIALLY-INVARIANT')
2086 FORMAT(/' REMAINING PARAMETERS ARE SPATIALLY-VARYING')
2087 FORMAT(/' HIGHER-LEVEL PREDATION IS TEMPORALLY-INVARIANT')
2088 FORMAT(/' HIGHER-LEVEL PREDATION VARIES TEMPORALLY')
2090 FORMAT(I8,F8.3)
2092 FORMAT(/'    DAY     TVPR')
2093 FORMAT(/'    DAY     B2GR')
2094 FORMAT(/' BASE GROUP 2 GROWTH IS TEMPORALLY-INVARIANT')
2095 FORMAT(/' BASE GROUP 2 VARIES TEMPORALLY')
3000 FORMAT(/' ALGAL PROPERTIES',30X,'GROUP 1   GROUP 2   GROUP 3')
3010 FORMAT(/' MINIMUM GM CHL/GM C    ',20X,3F10.4)
3012 FORMAT(' ACHCL (GM CHL/GM C)    ',20X,3F10.4)
3014 FORMAT(' BCHCL (1/DEGREE C)     ',20X,3F10.4)
3016 FORMAT(' CCHCL (M**2 DAY/E)     ',20X,3F10.4)
3020 FORMAT(' GM N/GM C              ',20X,3F10.4)
3022 FORMAT(' MINIMUM GM P/GM C      ',20X,3F10.4)
3030 FORMAT(' GM SI/GM C             ',20X,3F10.4)
3040 FORMAT(' KHN (GM N/M**3)        ',20X,3F10.4)
3042 FORMAT(' KHNFIX (GM N/M**3)     ',20X,F10.4)
3043 FORMAT(' FRACTION N FIXERS      ',20X,F10.4)
3050 FORMAT(' KHP (GM P/M**3)        ',20X,3F10.4)
3060 FORMAT(' KHS (GM SI/M**3)       ',20X,3F10.4)
3070 FORMAT(' KHR (GM DO/M**3)       ',20X,3F10.4)
3072 FORMAT(' SALT TOXICITY (1/DAY)  ',20X,3F10.4)
3074 FORMAT(' KHST (ppt)             ',20X,3F10.4)
3076 FORMAT(' VMAX (GM P/GM C/DAY)   ',20X,3F10.4)
3080 FORMAT(' ALPHA (GM C/GM CHL/DAY)/(uE/M**2/SEC)      ',3F10.3)
3090 FORMAT(' ALPHA TO P RATIO (1/(uE/M**2/SEC))',9X,3F10.3)
4000 FORMAT(' PHOTORESPIRATION FRACTION     ',13X,3F10.3)
4010 FORMAT(' OPTIMAL TEMPERATURE FOR PRODUCTION (C)',5X,3F10.3) 
4020 FORMAT(/' DISTRIBUTION OF ALGAE UPON MORTALITY'//                       &
     ' GROUP 1 RESPIRATION  DIS INORG  LAB DISS  REF DISS  LAB PART  REF PART')
4030 FORMAT(' NITROGEN            ',5F10.3)
4040 FORMAT(' PHOSPHORUS          ',5F10.3)
4050 FORMAT(' CARBON              ',10X,4F10.3)
4060 FORMAT(/' GROUP 2 RESPIRATION')
4070 FORMAT(/' GROUP 3 RESPIRATION')
4080 FORMAT(' CARBON              ',5F10.3)
4082 FORMAT(' SILICA              ',F10.3,10X,F10.3)
4090 FORMAT(/' PREDATION')
5000 FORMAT(' PHOSPHORUS     ',4F10.3)
5010 FORMAT(' CARBON         ',4F10.3)
5020 FORMAT(' REFERENCE TEMPERATURE FOR RESPIRATION (C)  ',3F10.3)
5030 FORMAT(' EFFECT OF SUBOPTIMAL TEMP ON PRODUCTION    ',3F10.4) 
5040 FORMAT(' EFFECT OF SUPEROPTIMAL TEMP ON PRODUCTION  ',3F10.4) 
5050 FORMAT(' EFFECT OF TEMPERATURE ON RESPIRATION       ',3F10.4)
5060 FORMAT('       CELL     PMAX     METAB     PRDTN',/                   &
            '             C/CHL/DAY   1/DAY  GM/M**3/DAY'/)
5070 FORMAT(10X,3F10.3)
5080 FORMAT(2I10,3F10.3)

     RETURN
     END SUBROUTINE ALG_READ
      


!************************************************************************
!**                  S U B R O U T I N E   A L G A E                   **
!************************************************************************

   SUBROUTINE ALGAE(DTB1,DTB2,DTB3,FLXS1,FLXS2,FLXS3)
   IMPLICIT NONE
   INTEGER  ::  I, J, F, K, ITEMP
   REAL,DIMENSION(0:MTLOC,KBM1) :: DTB1, DTB2,DTB3
   REAL,DIMENSION(0:MTLOC,KBM1) :: FLXS1,FLXS2,FLXS3
   
   ALLOCATE(XL(MLOC));          XL = 0.0
   ALLOCATE(STOX1(MLOC,KBM1));  STOX1 = 0.0
   ALLOCATE(STOX2(MLOC,KBM1));  STOX2 = 0.0
   ALLOCATE(STOX3(MLOC,KBM1));  STOX3 = 0.0
   ALLOCATE(NL1NNF(MLOC,KBM1)); NL1NNF = 0.0
   ALLOCATE(ITBOT(MLOC,KBM1));  ITBOT = 0.0

! DETERMINE JULIAN DAY

   J = 1.0 + AMOD(JDAY,365.25)
!     if (jday .gt. 730.) fnfix = 0.9

! SALINITY TOXICITY

   DO K=1,KBM1
     DO I=1,MLOC
       SALTOX = MAX(0., SALT(I,K))
       STOX1(I,K)=STF1*0.5*(1.+TANH(SALTOX-KHST1))
       STOX2(I,K)=STF2*(1.-0.5*(1.+TANH(SALTOX-KHST2)))
       STOX3(I,K)=STF3*0.5*(1.+TANH(SALTOX-KHST3))
     END DO
   END DO  

! TEMPERATURE EFFECTS ON PRODUCTION AND RESPIRATION

   DO K=1,KBM1
     DO I=1,MLOC
       ITEMP = 10.*T(I,K)+0.05
!       IF(I.eq.25) THEN
!         Print*, 'FT1(ITEMP)', FT1(ITEMP)
!       ENDIF
       P1(I,K) = PM1(I,K)*FT1(ITEMP)
!       IF(I.eq.11126) THEN
!          Print*, 'PM1, max photos rate, temp effect', PM1(I,K), FT1(ITEMP)
!          Print*, 'P1 after temp', P1(I,K)/30.303
!       ENDIF
       P2(I,K) = PM2(I,K)*FT2(ITEMP)*B2GR(J)
       P3(I,K) = PM3(I,K)*FT3(ITEMP)
! RGL changed below to leave out salinity toxicity for now
! will test and put it back in
!       BM1(I,K) = BMR1(I,K)*FTBM1(ITEMP)+STOX1(I,K)
       BM1(I,K) = BMR1(I,K)
!       IF(I.eq.25) THEN
!          Print*, 'BM1', BM1(I,K)
!          Print*, 'BMR1', BMr1(I,K)
!          Print*, 'STOX1', STOX1(I,K)
!          Print*, 'FTBM1', FTBM1(ITEMP)
!      ENDIF
       BM2(I,K) = BMR2(I,K)*FTBM2(ITEMP)+STOX2(I,K)
       BM3(I,K) = BMR3(I,K)*FTBM3(ITEMP)+STOX3(I,K)
       PR1(I,K) = BPR1(I,K)*B1(I,K)*B1(I,K)*FTPR(ITEMP)*TVPR(J)
       PR2(I,K) = BPR2(I,K)*B2(I,K)*B2(I,K)*FTPR(ITEMP)*TVPR(J)
       PR3(I,K) = BPR3(I,K)*B3(I,K)*B3(I,K)*FTPR(ITEMP)*TVPR(J)
     END DO	
   END DO  

! NUTRIENT LIMITATIONS - NITROGEN

   DO K=1,KBM1
     DO I=1,MLOC
!RGL changed for biosoln
!       DIN = NH4(I,K)+NO3(I,K)
       DIN = NO3(I,K)
       NL1(I,K) = (1.-FNFIX)*DIN/(KHN1+DIN) + FNFIX*(DIN+KHNFIX)/(KHN1+DIN+KHNFIX)
       NL1NNF(I,K) = DIN/(KHN1+DIN) 
!       IF(I.eq.11126) Print*, 'NL1,NL1NNF', NL1(I,K),NL1NNF(I,K)
       NL2(I,K) = DIN/(KHN2+DIN)
       NL3(I,K) = DIN/(KHN3+DIN)
     END DO	
   END DO  


! PHOSPHORUS

! DROOP FORMULA FOR P LIMIT

   DO K=1,KBM1
     DO I=1,MLOC
       PL1(I,K) = MAX((Q1(I,K)-Q01)/(Q1(I,K)+1.0E-30),0.0)
!       IF(I.eq.25) THEN 
!        Print*, 'PL1,Q1,Q01', PL1(I,K), Q1(I,K), Q01
!       ENDIF
       PL2(I,K) = MAX((Q2(I,K)-Q02)/(Q2(I,K)+1.0E-30),0.0)
       PL3(I,K) = MAX((Q3(I,K)-Q03)/(Q3(I,K)+1.0E-30),0.0)                  
     END DO	
   END DO  

! SILICA 

   DO K=1,KBM1
     DO I=1,MLOC
       DSIL = SA(I,K)/(1.+KADSA*SSI(I,K))
       SL1(I,K) = (DSIL+1.0E-30)/(KHS1+DSIL+1.0E-30)
       SL2(I,K) = (DSIL+1.0E-30)/(KHS2+DSIL+1.0E-30)
       SL3(I,K) = (DSIL+1.0E-30)/(KHS3+DSIL+1.0E-30)
     END DO
   END DO  

! PHOTOSYNTHESIS VS IRRADIANCE

!vjp 10/07/04  modified sqrt formulae in next loop
   DO K=1,KBM1
     DO I=1,MLOC
       ALPHA = ALPHMIN1+ALPHRAT1*P1(I,K)
!      RGL commented out orig for analytical solution w/out P lim
!       IK = P1(I,K)*AMIN1(NL1(I,K),PL1(I,K),SL1(I,K))/(ALPHA+1.0E-10)
       IK = P1(I,K)*(NL1(I,K))/(ALPHA+1.0E-10)
       FI1(I,K) = IAVG(I,K)/(SQRT(IK*IK+IAVG(I,K)*IAVG(I,K)+1.0E-10))
!      FI1(B) = (IAVG(B)+1.0E-10)/(SQRT(IK*IK+IAVG(B)*IAVG(B)+1.0E-10))
!       IF(I.eq.25) Then
!         Print*, 'ALPHA', ALPHA
!         Print*, 'IAVG', IAVG(I,K)
!         Print*, 'IK, FI1', IK, FI1(I,K)
!       ENDIF
       ALPHA = ALPHMIN2+ALPHRAT2*P2(I,K)
       IK = P2(I,K)*AMIN1(NL2(I,K),PL2(I,K),SL2(I,K))/(ALPHA+1.0E-10)
       FI2(I,K) = IAVG(I,K)/(SQRT(IK*IK+IAVG(I,K)*IAVG(I,K)+1.0E-10))
!      FI2(B) = (IAVG(B)+1.0E-10)/(SQRT(IK*IK+IAVG(B)*IAVG(B)+1.0E-10))

       ALPHA = ALPHMIN3+ALPHRAT3*P3(I,K)
       IK = P3(I,K)*AMIN1(NL3(I,K),PL3(I,K),SL3(I,K))/(ALPHA+1.0E-10)
       FI3(I,K) = IAVG(I,K)/(SQRT(IK*IK+IAVG(I,K)*IAVG(I,K)+1.0E-10))
!      FI3(B) = (IAVG(B)+1.0E-10)/(SQRT(IK*IK+IAVG(B)*IAVG(B)+1.0E-10))
     END DO
   END DO  

!     *** Compute carbon to chlorophyll ratio in each cell 

   DO I=1,MLOC
     ITTOP = IT
     OPTDEPTH = KESS(I,1)*D(I)*DZ(1)
     ITBOT(I,1) = ITTOP*EXP(-OPTDEPTH)
     ITAVG = (ITTOP-ITBOT(I,1))/OPTDEPTH
!RGL changed below to use nitrogen only for bio solution
!     NUTLIM = AMIN1(NL1(I,1),PL1(I,1))
     NUTLIM = NL1(I,1)
     CCHL1(I,1) = 1./(CHLCMN1+ACHLC1*EXP(BCHLC1*T(I,1))*NUTLIM/EXP(CCHLC1*ITAVG))
!     IF(I.eq.25) THEN
!      Print*, 'max growth rate d-1', PM1(I,1)/CCHL1(I,1)
!      Print*, 'NL1, NUTLIM in surface', NL1(K,1), NUTLIM
!     ENDIF
     NUTLIM = AMIN1(NL2(I,1),PL2(I,1))
     CCHL2(I,1) = 1./(CHLCMN2+ACHLC2*EXP(BCHLC2*T(I,1))*NUTLIM/EXP(CCHLC2*ITAVG))
     NUTLIM = AMIN1(NL3(I,1),PL3(I,1))
     CCHL3(I,1) = 1./(CHLCMN3+ACHLC3*EXP(BCHLC3*T(I,1))*NUTLIM/EXP(CCHLC3*ITAVG))
   END DO 

   DO K=2,KBM1
     DO I=1,MLOC
       ITTOP = ITBOT(I,K-1)
       OPTDEPTH = KESS(I,K)*D(I)*DZ(K)
       ITBOT(I,K) = ITTOP*EXP(-OPTDEPTH)
       ITAVG = (ITTOP-ITBOT(I,K))/OPTDEPTH
!RGL commented out below for analytical solution
!       NUTLIM = AMIN1(NL1(I,K),PL1(I,K))
       NUTLIM = NL1(I,K)
       CCHL1(I,K) = 1./(CHLCMN1+ACHLC1*EXP(BCHLC1*T(I,K))*NUTLIM/EXP(CCHLC1*ITAVG))
       NUTLIM = AMIN1(NL2(I,K),PL2(I,K))
       CCHL2(I,K) = 1./(CHLCMN2+ACHLC2*EXP(BCHLC2*T(I,K))*NUTLIM/EXP(CCHLC2*ITAVG))
       NUTLIM = AMIN1(NL3(I,K),PL3(I,K))
       CCHL3(I,K) = 1./(CHLCMN3+ACHLC3*EXP(BCHLC3*T(I,K))*NUTLIM/EXP(CCHLC3*ITAVG))
!RGL added prints here
!       IF(I.eq.25) THEN
!          Print*, 'NUTLIM', NUTLIM
!       ENDIF
     END DO
   END DO 

! COMPUTE ASSIMILATION RATIO AT WATER SURFACE (GM C/GM CHL/DAY), 
! CARBON FIXATION (GM C/M**3/DAY)  COPIED FROM CHES BAY CODE MARCH 14, 2007

   DO I=1,MLOC
!RGL changed below for analytical solution
!     ASRAT(I) = (P1(I,1)*AMIN1(NL1(I,1),PL1(I,1),SL1(I,1))*B1(I,1)                  &
!               + P2(I,1)*AMIN1(NL2(I,1),PL2(I,1),SL2(I,1))*B2(I,1)        &
!               + P3(I,1)*AMIN1(NL3(I,1),PL3(I,1),SL3(I,1))*B3(I,1))       &
!               / (B1(I,1)+B2(I,1)+B3(I,1)+1.0e-6)

!     NETP1 = (P1(I,1)*AMIN1(NL1(I,1),PL1(I,1),SL1(I,1))*(1.-PRSP1)        &
!             /CCHL1(I,1)-BM1(I,1))*B1(I,1)

     ASRAT(I) = (P1(I,1)*(NL1(I,1))*B1(I,1)                  &
               + P2(I,1)*AMIN1(NL2(I,1),SL2(I,1))*B2(I,1)        &
               + P3(I,1)*AMIN1(NL3(I,1),SL3(I,1))*B3(I,1))       &
               / (B1(I,1)+B2(I,1)+B3(I,1)+1.0e-6)
!     IF(I.eq.11126) THEN
!      Print*, 'C:CHL', CCHL1(I,1)
!      Print*, 'instant growth rate', ASRAT(I)/CCHL1(I,1)
!     ENDIF
     NETP1 = (P1(I,1)*(NL1(I,1))*(1.-PRSP1)        &
             /CCHL1(I,1)-BM1(I,1))*B1(I,1)
!     IF(I.eq.11126) THEN
!         Print*, 'NET growth rate after limits applied', NETP1
!         Print*, 'n limitation', NL1(I,1)
!        Print*, 'PRSP1', PRSP1
!        Print*, 'BM1', BM1(I,1)
!        Print*, 'B1', B1(I,1)
!     ENDIF

!end RGL changes above
     NETP2 = (P2(I,1)*AMIN1(NL2(I,1),PL2(I,1),SL2(I,1))*(1.-PRSP2)        &
             /CCHL2(I,1)-BM2(I,1))*B2(I,1)
     NETP3 = (P3(I,1)*AMIN1(NL3(I,1),PL3(I,1),SL3(I,1))*(1.-PRSP3)        &
             /CCHL3(I,1)-BM3(I,1))*B3(I,1)

     CFIX(I) = NETP1+NETP2+NETP3
   END DO

! EFFECTS OF LIGHT AND NUTRIENTS ON PRODUCTION
! CONVERT FROM GM C/GM CHL/DAY TO SPECIFIC GROWTH RATE
! RGL changed FI1 below to be closer to analytical solution
   IF (MINPROD == ' MINIMUM') THEN
!     Print*, 'calculating Glimited'
     DO K=1,KBM1
       DO I=1,MLOC
!RGL for analytical solution no P limit below
          FI1(I,K) = 0.23
!         P1NNF(I,K) = P1(I,K)*AMIN1(FI1(I,K),NL1NNF(I,K),PL1(I,K),SL1(I,K))  &
!	             /CCHL1(I,K)
!          IF(I.eq.11126) Print*, 'P1 before n and lt lim',P1(I,1)/CCHL1(I,K)
!         P1(I,K) = P1(I,K)*AMIN1(FI1(I,K),NL1(I,K),PL1(I,K),SL1(I,K))/CCHL1(I,K)
         P1NNF(I,K) = P1(I,K)*AMIN1(FI1(I,K),NL1NNF(I,K))  &
                    /CCHL1(I,K)
         P1(I,K) = P1(I,K)*AMIN1(FI1(I,K),NL1(I,K))/CCHL1(I,K)
! end RGL changes
         P2(I,K) = P2(I,K)*AMIN1(FI2(I,K),NL2(I,K),PL2(I,K),SL2(I,K))/CCHL2(I,K)
         P3(I,K) = P3(I,K)*AMIN1(FI3(I,K),NL3(I,K),PL3(I,K),SL3(I,K))/CCHL3(I,K)
!         IF(I.eq.11126) THEN
!           Print*, 'P1NNF =', P1NNF(I,K)
!           Print*, 'FI1 = ', FI1(I,K)
!           Print*, 'P1 after all limit= ', P1(I,K)
!         ENDIF
       END DO	 
     END DO

   ELSE

     DO K=1,KBM1
       DO I=1,MLOC
!RGL changed below to take out P limitation
!         P1NNF(I,K) = P1(I,K)*FI1(I,K)*AMIN1(NL1NNF(I,K),PL1(I,K),SL1(I,K))  &
!	             /CCHL1(I,K)
!         P1(I,K) = P1(I,K)*FI1(I,K)*AMIN1(NL1(I,K),PL1(I,K),SL1(I,K))/CCHL1(I,K)
         P1NNF(I,K) = P1(I,K)*FI1(I,K)*(NL1NNF(I,K))  &
                    /CCHL1(I,K)
         P1(I,K) = P1(I,K)*FI1(I,K)*(NL1(I,K))/CCHL1(I,K)

         P2(I,K) = P2(I,K)*FI2(I,K)*AMIN1(NL2(I,K),PL2(I,K),SL2(I,K))/CCHL2(I,K)
         P3(I,K) = P3(I,K)*FI3(I,K)*AMIN1(NL3(I,K),PL3(I,K),SL3(I,K))/CCHL3(I,K)
       END DO
     END DO  

   END IF

! RATE OF CHANGE DUE TO PRODUCTION, RESPIRATION, PREDATION

   DO K=1,KBM1
     DO I=1,MLOC
       
       NETP1 = (P1(I,K)*(1.-PRSP1)-BM1(I,K))*B1(I,K)
!       IF(I.eq.11126) Print*,'Gnet', P1(I,K)*(1.-PRSP1)-BM1(I,K)
       NETP2 = (P2(I,K)*(1.-PRSP2)-BM2(I,K))*B2(I,K)
       NETP3 = (P3(I,K)*(1.-PRSP3)-BM3(I,K))*B3(I,K)
       GPP1 = P1(I,K)*B1(I,K)
       GPP2 = P2(I,K)*B2(I,K)
       GPP3 = P3(I,K)*B3(I,K)
       
! PRIMARY PRODUCTION IN GM C/M**2/DAY

       GPP(I,K) = (GPP1+GPP2+GPP3)*D(I)*DZ(K)
       NPP(I,K) = (NETP1+NETP2+NETP3)*D(I)*DZ(K)
!       IF(I.eq.11126) THEN
!          Print*, 'GPP', 'Biomass', GPP(I,K),B1(I,K)
!          Print*, 'NPP = ', NPP(I,K)
!       ENDIF
!       DTB1(I,K) = (NETP1-PR1(I,K)-B1SZ(I,K)-B1LZ(I,K))/86400.
!rgl changed above for analytical solution
       DTB1(I,K) = (NETP1)/86400
!       IF(I.eq.11126) Print*, 'change algae pre-settle', DTB1(I,K)
       DTB2(I,K) = (NETP2-PR2(I,K)-B2SZ(I,K)-B2LZ(I,K))/86400.
       DTB3(I,K) = (NETP3-PR3(I,K)-B3SZ(I,K)-B3LZ(I,K))/86400.

     END DO
   END DO  

! NITROGEN FIXATION IN SURFACE BOXES (G N/G CHL/DAY)

   DO I=1,MLOC
     SNFIX(I) = (P1(I,1)-P1NNF(I,1))*ANC1*CCHL1(I,1)
   END DO

! RATE OF CHANGE DUE TO SETTLING
          
   DO I=1,MLOC
!     DTB1(I,1) = DTB1(I,1)-WS1(I,1)*B1(I,1)/(D(I)*DZ(1))/86400.
! RGl changed above for analytical solution
!     IF(I.eq.11126) Print*, 'change algae post settle', DTB1(I,k)
     DTB2(I,1) = DTB2(I,1)-WS2(I,1)*B2(I,1)/(D(I)*DZ(1))/86400.
     DTB3(I,1) = DTB3(I,1)-WS3(I,1)*B3(I,1)/(D(I)*DZ(1))/86400.
   END DO

   DO K=2,KBM1
     DO I=1,MLOC
!       DTB1(I,K) = DTB1(I,K)+(WS1(I,K-1)*B1(I,K-1)-WS1(I,K)*B1(I,K))  &
!                  /(D(I)*DZ(K))/86400.
! RGl changed above for analytical solution
       DTB2(I,K) = DTB2(I,K)+(WS2(I,K-1)*B2(I,K-1)-WS2(I,K)*B2(I,K))  &
                  /(D(I)*DZ(K))/86400.
       DTB3(I,K) = DTB3(I,K)+(WS3(I,K-1)*B3(I,K-1)-WS3(I,K)*B3(I,K))  &
                  /(D(I)*DZ(K))/86400.
     END DO
   END DO  

! SETTLING FLUX FOR MASS BALANCE

   DO K=1,KBM1
     DO I=1,MLOC
       FLXS1(I,K) = WS1(I,K)*B1(I,K)*V2(I,K)/(D(I)*DZ(K)*86400.)
       FLXS2(I,K) = WS2(I,K)*B2(I,K)*V2(I,K)/(D(I)*DZ(K)*86400.)
       FLXS3(I,K) = WS3(I,K)*B3(I,K)*V2(I,K)/(D(I)*DZ(K)*86400.)
     END DO
   END DO  
        
!  RESUSPENSION

   IF (SEDIMENT_CALC) THEN 
            
     TREC=1.0/86400.
     DO I=1,MLOC 
       XL(I)=TREC/(D(I)*DZ(KBM1))     
     END DO

!     DO I=1,MLOC 
!RGL CHANGED FOR Analytical solution
!       DTB1(I,KBM1) = DTB1(I,KBM1)+(WS1(I,KBM1)-WS1NET(I))*B1(I,KBM1)*XL(I)
!     END DO
     DO I=1,MLOC 
       DTB2(I,KBM1) = DTB2(I,KBM1)+(WS2(I,KBM1)-WS2NET(I))*B2(I,KBM1)*XL(I)
     END DO
     DO I=1,MLOC 
       DTB3(I,KBM1) = DTB3(I,KBM1)+(WS3(I,KBM1)-WS3NET(I))*B3(I,KBM1)*XL(I)
     END DO
   END IF 

   DEALLOCATE(XL,STOX1,STOX2,STOX3,NL1NNF,ITBOT)


   RETURN
   END SUBROUTINE ALGAE

   END MODULE ALGAL
