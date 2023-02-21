!************************************************************************
!**                                                                    **
!**       Submerged Aquatic Vegetation Subroutines for CE-QUAL-ICM     **
!**                                                                    **
!**                          St Johns River                            **
!**                           March 4, 2005                            **
!**                                                                    **
!**                    Water Quality Modeling Group                    **
!**                    U.S. Army Corps of Engineers                    **
!**                    Waterways Experiment Station                    **
!**                    Vicksburg, Mississippi 39180                    **
!**                                                                    **
!************************************************************************


!************************************************************************
!**             S U B R O U T I N E   S A V _ R E A D                  **
!************************************************************************

   SUBROUTINE SAV_READ
   USE WQM
   USE FILE_INFO
   USE ALL_VARS
   
   IMPLICIT NONE
   CHARACTER(LEN=72) :: TITLE(6)
   CHARACTER(LEN=8)  :: TIMVAR, PRINTIT
   REAL :: KTPS1, KTPS2, KTBMSAV, KTPE1, KTPE2, KTBME, KTPRE

! vjp 8/23/05   added these declarations
   INTEGER :: I, J, N, NSAV, NDPH
   REAL    :: TRPM, TRBMSAV, TRPE, TRBME, TRPRE, TLOOK

! TITLE CARDS

   READ(SVI,1000)
   READ(SVI,1010) (TITLE(J),J=1,6)

! READ SAV PROPERTIES

   READ(SVI,1040) SPNAM
   READ(SVI,1030) PMSAV,PRSPSAVB,BMSAV,BMTBRREF,FDOSR,KHSTOX,SALMAX
   READ(SVI,1030) ACDWSAV,ANDWSAV,APDWSAV

! COMPUTE ADDITIONAL STOICHIOMETRY OF SAV

   ANCSAV = ANDWSAV/ACDWSAV
   APCSAV = APDWSAV/ACDWSAV
        
   READ(SVI,1030) KHNLEAF,KHNPSAV,KHNROOT,KHPLEAF,KHPROOT
   READ(SVI,1030) ALPHSAV,KESAV,ALAC,ACAN,BCAN
   READ(SVI,1030) TRPM,TRBMSAV
   READ(SVI,1030) KTPS1,KTPS2,KTBMSAV

! READ PIECEWISE FUNCTIONS DESCRIBING PARTITIONING OF PRODUCTION,
! TRANSFER FROM TUBERS TO LEAVES
! VALUES ARE ENTERED AT INTERVALS OF 10 DEGREE DAYS FROM ZERO TO
! SCENESCENCE

   READ(SVI,1042) NDDI, PRINTIT
   READ(SVI,1032) (FPLEAF(J), FPSTEM(J),                     &
                   FPROOT(J), FPTUBER(J), TRTBRLF(J),J=0,NDDI-1)
          
! READ DISTRIBUTION OF SAV BIOMASS AFTER MORTALITY

   READ(SVI,1030) FNISAV,FNLDSAV,FNRDSAV,FNLPSAV,FNRPSAV
   READ(SVI,1030) FRNSAV(1),FRNSAV(2),FRNSAV(3)
   READ(SVI,1030) FPISAV,FPLDSAV,FPRDSAV,FPLPSAV,FPRPSAV
   READ(SVI,1030) FRPSAV(1),FRPSAV(2),FRPSAV(3)
   READ(SVI,1030) FDOSAV,FCLDSAV,FCRDSAV,FCLPSAV,FCRPSAV
   READ(SVI,1030) FRCSAV(1),FRCSAV(2),FRCSAV(3)

! READ EFFECT OF SAV ON NET SETTLING TO SEDIMENTS

   READ(SVI,1030) WSSSAV,WSLSAV,WSRSAV,WS1SAV,WS2SAV,WS3SAV,WSUSAV      

! READ EPIPHYTE NAME AND PROPERTIES

   READ(SVI,1040) EPINAM
   READ(SVI,1030) PMEPI,PRSPEPI,BMEPI,PREPI
   READ(SVI,1030) CCHLEPI,ADWCEPI,ANCEPI,APCEPI
   READ(SVI,1030) KHNEPI,KHNPEPI,KHPEPI
   READ(SVI,1030) ALPHEPI,KEEPI
   READ(SVI,1030) TRPE,TRBME,TRPRE
   READ(SVI,1030) KTPE1,KTPE2,KTBME,KTPRE
   READ(SVI,1030) KHEP

! READ DISTRIBUTION OF EPIPHYTE BIOMASS AFTER MORTALITY

   READ(SVI,1030) FNIEPI,FNLDEPI,FNRDEPI,FNLPEPI,FNRPEPI
   READ(SVI,1030) FNIPEP,FNLDPEP,FNRDPEP,FNLPPEP,FNRPPEP
   READ(SVI,1030) FPIEPI,FPLDEPI,FPRDEPI,FPLPEPI,FPRPEPI
   READ(SVI,1030) FPIPEP,FPLDPEP,FPRDPEP,FPLPPEP,FPRPPEP
   READ(SVI,1030) FCLDEPI,FCRDEPI
   READ(SVI,1030) FCLDPEP,FCRDPEP,FCLPPEP,FCRPPEP
      
! READ NUMBER OF CELLS WITH SAV, MAXIMUM NUMBER OF DEPTH INCREMENTS

   READ(SVI,1020) NSAVCELL, NSAVDPHM
      
! READ NUMBER OF DEPTH INCREMENTS, CHARACTERISTIC WIDTHS, AND DEPTHS

!vjp modified 11/3/2005
   DO I=1, MLOC
     SAVCELL(I) = 0
     NSAVDPH(I) = 0
   END DO
   DO J=1, NSINC
     DO I=1, MLOC
       SAVWDH(I,J) = 0.0
       SAVDPH(I,J) = 0.0
     END DO
   ENDDO

   READ(SVI,1006)
   DO I=1,NSAVCELL
     READ(SVI,*) NSAV, B,  NDPH
     SAVCELL(I) = B
     NSAVDPH(B) = NDPH
     READ(SVI,*) (SAVWDH(B,J),J=1,NSAVDPH(B))
     READ(SVI,*) (SAVDPH(B,J),J=1,NSAVDPH(B))
   END DO

!***** Input FORMAT statements

1000 FORMAT(///)
1006 FORMAT(/)
1010 FORMAT(A72)
1020 FORMAT(//(8X,9I8))
1030 FORMAT(//(8X,9F8.0))
1032 FORMAT(//(8X,5F8.0))
1040 FORMAT(//8X,2A8)
1042 FORMAT(//8X,I8,A8)
1050 FORMAT(//(8X,A8,F8.0))

! OUTPUT WHAT WAS INPUT

     OPEN (SVO,FILE=SVOFN)
     WRITE(SVO,2010) (TITLE(J),J=1,6)

! WRITE SAV SPECIES NAME AND PROPERTIES

     WRITE(SVO,2040) SPNAM
     WRITE(SVO,2050) PMSAV,TRPM,KTPS1,KTPS2
     WRITE(SVO,2052) PMSAV/ACDWSAV,TRPM
     WRITE(SVO,2056) ACDWSAV,ANDWSAV,APDWSAV
     WRITE(SVO,4010) KHNLEAF,KHNPSAV,KHNROOT,KHPLEAF,KHPROOT
     WRITE(SVO,2060) BMSAV,TRBMSAV,KTBMSAV
     WRITE(SVO,2058) PRSPSAVB,SALMAX
     WRITE(SVO,3070) FDOSR
     WRITE(SVO,2090) KHSTOX
     WRITE(SVO,3000) ALPHSAV
     WRITE(SVO,3010) KESAV,ALAC,ACAN,BCAN
     IF (PRINTIT == '     ALL') THEN
       WRITE(SVO,3080)
       WRITE(SVO,3090) (J,FPLEAF(J),FPSTEM(J),FPROOT(J),              &
                        FPTUBER(J),TRTBRLF(J),J=1,NDDI)
     END IF

! WRITE DISTRIBUTION OF BIOMASS UPON MORTALITY

     WRITE(SVO,4020)
     WRITE(SVO,4030) FNISAV,FNLDSAV,FNRDSAV,FNLPSAV,FNRPSAV
     WRITE(SVO,4040) FPISAV,FPLDSAV,FPRDSAV,FPLPSAV,FPRPSAV
     WRITE(SVO,4050) FDOSAV,FCLDSAV,FCRDSAV,FCLPSAV,FCRPSAV
     WRITE(SVO,4080)
     WRITE(SVO,4090) FRNSAV(1),FRNSAV(2),FRNSAV(3)
     WRITE(SVO,5000) FRPSAV(1),FRPSAV(2),FRPSAV(3)
     WRITE(SVO,5010) FRCSAV(1),FRCSAV(2),FRCSAV(3)

! WRITE EFFECT OF SAV ON NET SETTLING TO SEDIMENTS
     WRITE(SVO,5060) WSSSAV,WSLSAV,WSRSAV,WS1SAV,WS2SAV,WS3SAV,WSUSAV      

! WRITE EPIPHYTE PROPERTIES

     WRITE(SVO,2040) EPINAM
     WRITE(SVO,2051) PMEPI,TRPE,KTPE1,KTPE2
     WRITE(SVO,2052) PMEPI/CCHLEPI,TRPE
     WRITE(SVO,2062) CCHLEPI,ADWCEPI,ANCEPI,APCEPI
     WRITE(SVO,5020) KHNEPI,KHNPEPI,KHPEPI
     WRITE(SVO,2065) BMEPI,TRBME,KTBME
     WRITE(SVO,3020) PREPI,TRPRE,KTPRE
     WRITE(SVO,3002) ALPHEPI
     WRITE(SVO,3030) KEEPI
     WRITE(SVO,4000) KHEP

! WRITE DISTRIBUTION OF BIOMASS UPON MORTALITY

     WRITE(SVO,5030)
     WRITE(SVO,4030) FNIEPI,FNLDEPI,FNRDEPI,FNLPEPI,FNRPEPI
     WRITE(SVO,4040) FPIEPI,FPLDEPI,FPRDEPI,FPLPEPI,FPRPEPI
     WRITE(SVO,5050) FCLDEPI,FCRDEPI
     WRITE(SVO,5040)
     WRITE(SVO,4030) FNIPEP,FNLDPEP,FNRDPEP,FNLPPEP,FNRPPEP
     WRITE(SVO,4040) FPIPEP,FPLDPEP,FPRDPEP,FPLPPEP,FPRPPEP
     WRITE(SVO,4070) FCLDPEP,FCRDPEP,FCLPPEP,FCRPPEP
      
! WRITE NUMBER OF SAV CELLS, SAV WIDTH AND DEPTH INCREMENTS

!vjp modified 11/3/2005
     WRITE(SVO,5070) NSAVCELL, NSAVDPHM
     IF (PRINTIT == '     ALL') THEN
       DO I=1,NSAVCELL
         B = SAVCELL(I)
         WRITE(SVO,5080) I, B, NSAVDPH(B)
         WRITE(SVO,5082) (SAVWDH(B,J),J=1,NSAVDPH(B))
         WRITE(SVO,5090) (SAVDPH(B,J),J=1,NSAVDPH(B))
       END DO
     END IF
      
! COMPUTE FRACTIONAL AREA OF EACH SAV DEPTH INCREMENT

!vjp modified 11/3/2005
     DO I=1,NSAVCELL
       B=SAVCELL(I)
       DO N=1,NSAVDPH(B)
!JQI         SAVFRAC(B,N)=SAVWDH(B,N)/BL(B,2)
       END DO
     END DO

! CREATE LOOKUP TABLE OF TEMPERATURE EFFECTS

     DO J = -50,400
       TLOOK = FLOAT(J)/10.

       IF (TLOOK < TRPM) THEN
         FTPSAV(J) = EXP(-KTPS1*(TLOOK-TRPM)**2)
       ELSE
         FTPSAV(J) = EXP(-KTPS2*(TLOOK-TRPM)**2)
       END IF

       FTRSAV(J) = EXP(KTBMSAV*(TLOOK-TRBMSAV))

     END DO

     DO J = -50,400
       TLOOK = FLOAT(J)/10.

       IF (TLOOK < TRPE) THEN
         FTPEP(J) = EXP(-KTPE1*(TLOOK-TRPE)**2)
       ELSE
         FTPEP(J) = EXP(-KTPE2*(TLOOK-TRPE)**2)
       END IF
        
       FTREP(J)  = EXP(KTBME*(TLOOK-TRBME))
       FTPREP(J) = EXP(KTPRE*(TLOOK-TRPRE))

     END DO
      
! INITIALIZE DEGREE DAYS TO ZERO

     DO I=1,MLOC
       DGRDAYS(I)=0
     END DO
      
! ZERO OUT ALL COUNTERS FOR EFECTS ON WATER COLUMN AND SEDIMENTS

     DO I=1,MLOC

       DOSAV(I)   = 0.
       LDOCSAV(I) = 0.
       RDOCSAV(I) = 0.
       LPOCSAV(I) = 0.
       RPOCSAV(I) = 0.

       DOEPI(I)   = 0.
       LDOCEPI(I) = 0.
       RDOCEPI(I) = 0.
       LPOCEPI(I) = 0.
       RPOCEPI(I) = 0.

       NH4SAVW(I)  = 0.
       NO3SAVW(I)  = 0.
       LDONSAVW(I) = 0.
       RDONSAVW(I) = 0.
       LPONSAVW(I) = 0.
       RPONSAVW(I) = 0.

       NH4EPI(I)  = 0.
       NO3EPI(I)  = 0.
       RDONEPI(I) = 0.
       LDONEPI(I) = 0.
       LPONEPI(I) = 0.
       RPONEPI(I) = 0.

       PO4SAVW(I)  = 0.
       LDOPSAVW(I) = 0.
       RDOPSAVW(I) = 0.
       LPOPSAVW(I) = 0.
       RPOPSAVW(I) = 0.
        
       PO4EPI(I)  = 0.
       LDOPEPI(I) = 0.
       RDOPEPI(I) = 0.
       LPOPEPI(I) = 0.
       RPOPEPI(I) = 0.
        
     END DO

     DO I=1,MLOC

       SEDDOSAV(I) = 0.
       SEDCSAV(I)  = 0. 
       SEDNH4SAV(I) = 0.
       SEDPO4SAV(I) = 0.
       SEDNSAV(I)   = 0.
       SEDPSAV(I)   = 0.

     END DO
    
!***** Output FORMAT statements

2010 FORMAT(1X,A72)
2040 FORMAT(/1X,A8)
2050 FORMAT(' PRODUCTION = ',F8.3,' GM C/GM DW/DAY AT ',F8.2,' C.'/         &
            ' KT1 = ',F8.3,' KT2 = ',F8.3,' PER DEGREE**2')
2051 FORMAT(' PRODUCTION = ',F8.3,' GM C/GM CHL/DAY AT ',F8.2,' C.'/        &
            ' KT1 = ',F8.3,' KT2 = ',F8.3,' PER DEGREE**2')
2052 FORMAT(' CARBON SPECIFIC GROWTH RATE = ',F8.3,' PER DAY AT ',F8.2,' C.')
2056 FORMAT(' CARBON TO DRY WEIGHT RATIO = ',F8.3/                          &
            ' NITROGEN TO DRY WEIGHT RATIO = ',F8.3/                        &
            ' PHOSPHORUS TO DRY WEIGHT RATIO = ',F8.3)
2058 FORMAT(' PHOTORESPIRATION = ',F8.3,' * PRODUCTION @ ZERO SALT'/        &
            ' PHOTORESPIRATION = 1.0 WHEN SALINITY > ',F8.3)     
2060 FORMAT(/' PLANT RESPIRATION = ',F8.3,' PER DAY AT ',F8.2,              &
            ' C.  KT = ',F8.3,' PER DEGREE')
2062 FORMAT(' CARBON TO CHLOROPHYLL RATIO = ',F8.3/                         &
            ' DRY WEIGHT TO CARBON RATIO = ',F8.3/                          &
            ' NITROGEN TO CARBON RATIO = ',F8.3/                            &
            ' PHOSPHORUS TO CARBON RATIO = ',F8.3)
2065 FORMAT(/' RESPIRATION = ',F8.3,' PER DAY AT ',F8.2,                    &
            ' C.  KT = ',F8.3,' PER DEGREE')
2090 FORMAT(' SULFIDE TOXICITY EFFECT = ',F8.3,' PER MOL HS')
3000 FORMAT(/' ALPHA = ',F8.3,' (GM C/GM DW) / (E/M**2)')
3002 FORMAT(/' ALPHA = ',F8.3,' (GM C/GM CHL) / (E/M**2)')
3010 FORMAT(' LIGHT ATTENUATION = ',F8.3,' M**2/GM SHOOT C',/               &
            ' M**2 LEAF AREA PER GM LEAF CARBON  = ',F8.3,/                 &
            ' CANOPY HEIGHT = ',F8.3,' + ',F8.3,' * SHOOTS (M)')
3020 FORMAT(' PREDATION = ',F8.3,' PER DAY AT ',F8.2,                       &
            ' C.  KT = ',F8.3,' PER DEGREE')
3030 FORMAT(' LIGHT ATTENUATION = ',F8.3,' M**2 LEAF AREA PER GM EPIPHYTE C'/)
3040 FORMAT(' SPECIES SPECIFIED INCORRECTLY IN CELL ',I5,' AS ',A8)
3050 FORMAT(/3(' CELL SPECIES CODE BLSAV'))
3060 FORMAT(3(I5,1X,A8,I4,F8.2))
3070 FORMAT(' FRACTION DO TRANSFERRED FROM SHOOTS TO ROOTS =',F8.3)
3080 FORMAT(/'  DGR DAY  FPLEAF   FPSTEM   FPROOT  FPTUBER TRTBRLF'/        &
             '           0<  <1   0<  <1   0<  <1  0<  <1  1/DAY')
3090 FORMAT(I7,3X,5F10.3)
4000 FORMAT(/' EPIPHYTE DENSITY AT WHICH GROWTH IS HALVED = ',F8.3,' GM C/GM C')
4010 FORMAT(/' NITROGEN HALF-SATURATION CONC (SHOOTS) = ',F8.3,             &
            ' GM/M**3'/                                                     &
            ' NH4 HALF-SATURATION CONC (PREFERENCE) = ',F8.3,' GM/M**3'/    &
            ' NITROGEN HALF-SATURATION CONC (ROOTS) = ',F8.3,' GM/M**3'/    &
            ' PHOSPHORUS HALF-SATURATION CONC (SHOOTS) = ',F8.3,            &
            ' GM/M**3'/                                                     &
            ' PHOSPHORUS HALF-SATURATION CONC (ROOTS) = ',F8.3,             &
            ' GM/M**3')
4020 FORMAT(/' DISTRIBUTION OF SAV UPON MORTALITY'/                         &
            ' SHOOT RESPIRATION  DIS INORG  LAB DISS  REF DISS  ',          &
            'LAB PART  REF PART')
4030 FORMAT(' NITROGEN          ',5F10.3)
4040 FORMAT(' PHOSPHORUS        ',5F10.3)
4050 FORMAT(' CARBON            ',5F10.3)
4070 FORMAT(' CARBON            ',10X,4F10.3)
4080 FORMAT(/' ROOT MORTALITY   LAB PART  REF PART     INERT')
4090 FORMAT(' NITROGEN       ',3F10.3)
5000 FORMAT(' PHOSPHORUS     ',3F10.3)
5010 FORMAT(' CARBON         ',3F10.3)
5020 FORMAT(/' NITROGEN HALF-SATURATION CONC = ',F8.3,' GM/M**3'/           &
            ' NH4 HALF-SATURATION CONC (PREFERENCE) = ',F8.3,' GM/M**3'/    &
            ' PHOSPHORUS HALF-SATURATION CONC = ',F8.3,' GM/M**3')
5030 FORMAT(/' DISTRIBUTION OF EPIPHYTES UPON MORTALITY'/                   &
            ' RESPIRATION        DIS INORG  LAB DISS  REF DISS  ',          &
            'LAB PART  REF PART')
5040 FORMAT(' PREDATION')
5050 FORMAT(' CARBON            ',10X,2F10.3)
5060 FORMAT(/'EFFECT OF SAV ON NET SETTLING TO SEDIMENTS ',                 &
            '(M**3/GM C/DAY)'/'  SOLIDS  LABILE REFRACT      B1      B2',   &
            '      B3     PBS'/7F8.2)
5070 FORMAT(/' NUMBER OF SAV CELLS = ',I8,/' MAXIMUM NUMBER OF DEPTHS = ',I8)
5080 FORMAT(/' SAV CELL ',I6,', WQM CELL ',I6, ', # DEPTH INCR ',I6)
5082 FORMAT(' WIDTH (M) ',10F8.1)
5090 FORMAT(' DEPTH (M) ',10F8.2)
 
     RETURN
     END SUBROUTINE SAV_READ

!************************************************************************
!**                    S U B R O U T I N E   S A V                     **
!************************************************************************

   SUBROUTINE SAV_COMP
   USE WQM
   USE ALL_VARS
   IMPLICIT NONE
   INTEGER :: IDGD(MLOC)
   REAL :: DENLIM(MLOC,NSINC), PRSPSAV(MLOC)
   REAL :: IK, IATCNPY, IAVSH, IAVEP, NSED, NWAT, LFOLD
   REAL :: NPRSAV(MLOC), NPREPI(MLOC)
!vjp 9/20/05 added declarations
   INTEGER :: ITEMP, J, N, I
   REAL    :: ATN, PSED, DF, PWAT, HSMOL, STEST, GLIMSAV, GLIMEPI, DELLF
   REAL    :: DLTDY, TDIFF, ZMEAN, ZTOCNPY

! CONVERT MODEL TIME STEP TO DAYS, DETERMINE JULIAN DAY

   DLTDY = DLT/86400.
   J     = 1.0 + AMOD(JDAY,365.25)
      
! COMPUTE DEGREE DAYS FOR EACH CELL, DISCRETE ARRAY ELEMENT FOR
! TRANSFERS ETC

!vjp modified 11/3/2005
   DO I=1,NSAVCELL
     B = SAVCELL(I)
     TDIFF = MAX(T(B,1)-3.,0.)
     DGRDAYS(B) = DGRDAYS(B)+TDIFF*DLTDY
     IDGD(B) = DGRDAYS(B)/10.
   END DO
      
! ZERO OUT AFFECTS OF SAV CELLS ON WATER COLUMN AND SEDIMENTS

!vjp modified 11/3/2005
   DO I=1,NSAVCELL
     B = SAVCELL(I)      
     DOSAV(B)   = 0.
     LDOCSAV(B) = 0.
     RDOCSAV(B) = 0.
     LPOCSAV(B) = 0.
     RPOCSAV(B) = 0.

     DOEPI(B)   = 0.
     LDOCEPI(B) = 0.
     RDOCEPI(B) = 0.
     LPOCEPI(B) = 0.
     RPOCEPI(B) = 0.

     NH4SAVW(B)  = 0.
     NO3SAVW(B)  = 0.
     LDONSAVW(B) = 0.
     RDONSAVW(B) = 0.
     LPONSAVW(B) = 0.
     RPONSAVW(B) = 0.

     NH4EPI(B)  = 0.
     NO3EPI(B)  = 0.
     RDONEPI(B) = 0.
     LDONEPI(B) = 0.
     LPONEPI(B) = 0.
     RPONEPI(B) = 0.

     PO4SAVW(B)  = 0.
     LDOPSAVW(B) = 0.
     RDOPSAVW(B) = 0.
     LPOPSAVW(B) = 0.
     RPOPSAVW(B) = 0.
        
     PO4EPI(B)  = 0.
     LDOPEPI(B) = 0.
     RDOPEPI(B) = 0.
     LPOPEPI(B) = 0.
     RPOPEPI(B) = 0.
        
   END DO

!vjp modified 11/3/2005
   DO I=1,NSAVCELL
     B = SAVCELL(I)      
     SEDDOSAV(B)  = 0.
     SEDCSAV(B)   = 0. 
     SEDNH4SAV(B) = 0.
     SEDPO4SAV(B) = 0.
     SEDNSAV(B)   = 0.
     SEDPSAV(B)   = 0.
   END DO

! LIGHT EFFECTS

!vjp modified 11/3/2005
   DO I=1,NSAVCELL
     B = SAVCELL(I)
     ITEMP = 10. * T(B,1) + 0.05
     DO N=1,NSAVDPH(B)
        
! COMPUTE IRRADIANCE AT CANOPY HEIGHT

!JQI       ZMEAN   = SAVDPH(B,N)  
!JQI       HCAN = ACAN + BCAN*(LEAF(B,N)+STEM(B,N)) 
!JQI       HCAN = MIN(ZMEAN,HCAN)
!JQI       ZTOCNPY = ZMEAN-HCAN
!JQI       WATATN(B,N) = EXP(-KESS(B)*ZTOCNPY)
!JQI       IATCNPY = I0*(1.-SREFLECT/100.)*WATATN(B,N)

! COMPUTE ATTENUATION BY EPIPHYTES AND SELF SHADING

!JQI       EPATN(B,N) = EXP(-KEEPI*ADWCEPI*EP(B,N))
!JQI       ATN = KESS(B)*HCAN+KESAV*(LEAF(B,N)+STEM(B,N))
!JQI       SAVATN(B,N) = (1.-EXP(-ATN))/(ATN+1.0E-6)
!JQI       IAVEP = IATCNPY*SAVATN(B,N)
!JQI       IAVSH = IAVEP*EPATN(B,N)

! COMPUTE LIGHT LIMITATIONS TO GROWTH

!JQI       IK = PMSAV*FTPSAV(ITEMP)/ALPHSAV
!JQI       FISH(B,N) = IAVSH/(SQRT(IK*IK+IAVSH*IAVSH)+1.0E-30)
!JQI       IK = PMEPI*FTPEP(ITEMP)/ALPHEPI
!JQI       FIEP(B,N) = IAVEP/(SQRT(IK*IK+IAVEP*IAVEP)+1.0E-30)
          
     END DO

   END DO

! COMPUTE NUTRIENT LIMITATIONS TO GROWTH, SAV FIRST

!vjp modified 11/3/2005
   DO I=1,NSAVCELL
     B = SAVCELL(I)
!JQI     NSED = MAX(0.,NH4T2TM1S(B)/1000.)
!JQI      NWAT = MAX(0.,NH4(B)+NO3(B))
!JQI      PSED = MAX(0.,PO4T2TM1S(B)/1000.)
!JQI      DF = 1./(1.+KADPO4*SSI(B))
!JQI      PWAT = MAX(0.,DF*PO4(B))
        
        
!JQI      NLSAV(B) = (KHNROOT*NWAT+KHNLEAF*NSED)/                 &
!JQI                 (KHNLEAF*KHNROOT+KHNROOT*NWAT+KHNLEAF*NSED)
!JQI      FNSEDSAV(B) = (KHNLEAF*NSED)/                           &
!JQI                    (KHNROOT*NWAT+KHNLEAF*NSED+1.0E-6)
!JQI      NPRSAV(B) = NH4(B)/(KHNPSAV+NO3(B)+1.E-6)*              &
!JQI                 (NO3(B)/(KHNPSAV+NH4(B)+1.E-6)               &
!JQI                 +KHNPSAV/(1.E-6+NH4(B)+NO3(B)))

!JQI      PLSAV(B) = (KHPROOT*PWAT+KHPLEAF*PSED)/                 &
!JQI                 (KHPLEAF*KHPROOT+KHPROOT*PWAT+KHPLEAF*PSED)  
!JQI      FPSEDSAV(B) = (KHPLEAF*PSED)/(KHPROOT*PWAT+KHPLEAF*PSED+1.0E-6)

! NOW EPIPHYTES

!JQI      NLEPI(B) = NWAT/(KHNEPI+NWAT+1.0E-6)
!JQI      NPREPI(B) = NH4(B)/(KHNPEPI+NO3(B)+1.E-6)*              &
!JQI                 (NO3(B)/(KHNPEPI+NH4(B)+1.E-6)               &
!JQI                 +KHNPEPI/(1.E-6+NH4(B)+NO3(B)))
!JQI      PLEPI(B) = PWAT/(KHPEPI+PWAT+1.0E-6)
      
! COMPUTE SULFIDE TOXICITY EFECT ON SAV
  
!JQI      HSMOL = HST2TM1S(B)/(1.+M2*PIE2S)/64000.
!JQI      FHS(B) = EXP(-KHSTOX*HSMOL)

! COMPUTE EPIPHYTE DENSITY LIMITATION

!JQI      DO N=1,NSAVDPH(B)
!JQI        DENLIM(B,N) = KHEP/(KHEP+EP(B,N)+1.0E-6)
!JQI      END DO
    END DO

!vjp modified 11/3/2005
    DO I=1,NSAVCELL
      B = SAVCELL(I)
!JQ      ITEMP = 10. * T(B) + 0.05
        
! COMPUTE RESPIRATION

!JQI     STEST    = MAX(SALT(B),0.0)
!JQI     PRSPSAV(B) = PRSPSAVB + (1.-PRSPSAVB) * 0.5 * (1.+TANH(STEST-SALMAX))
!JQI     BMLEAF(B)= BMSAV*FTRSAV(ITEMP)
!JQI     BMSTEM(B)= BMSAV*FTRSAV(ITEMP)
!JQI     BMROOT(B)= BMSAV*FTRSAV(ITEMP)
!JQI     BMTUBER(B)= BMTBRREF*FTRSAV(ITEMP)
!JQI     SLSH(B)  = BMSAV*FTRSAV(ITEMP)*(FCLPSAV+FCRPSAV)

! DETERMINE GROWTH FOR INDIVIDUAL DEPTH INCREMENTS

!JQI     DO N=1,NSAVDPH(B)

!JQI       GLIMSAV = MIN(NLSAV(B),PLSAV(B),FISH(B,N),FHS(B))
!JQI       GLIMEPI = MIN(NLEPI(B),PLEPI(B),FIEP(B,N))
!JQI       PLEAF(B,N) = PMSAV*FTPSAV(ITEMP)*GLIMSAV/ACDWSAV
!JQI       NPPSAV(B,N) = (PLEAF(B,N)-BMLEAF(B))*LEAF(B,N)

! EPIPHYTE GROWTH AND RESPIRATION
! PHOTORESPIRATION IS NOT CONSISTENT WITH SAV
! FOR NOW LEAVE IT ALONE

!JQI       PEP(B,N) = PMEPI*FTPEP(ITEMP)*GLIMEPI*DENLIM(B,N)/CCHLEPI
!JQI       BMEP(B,N) = BMEPI*FTREP(ITEMP)+PRSPEPI*PEP(B,N)
!JQI       PREP(B,N) = PREPI*FTPREP(ITEMP)*EP(B,N)
!JQI       NPPEPI(B,N) = (PEP(B,N)-BMEP(B,N))*EP(B,N)
!JQI     END DO
   END DO

! INTERACTIONS WITH WATER COLUMN

!vjp modified 11/3/2005
   DO I=1,NSAVCELL
     B = SAVCELL(I)
     DO N=1,NSAVDPH(B)
      
! DISSOLVED OXYGEN AND CARBON

!JQI       DOSAV(B) = DOSAV(B)+SAVFRAC(B,N)*                         &
!JQI                 (AOCR*(1.-FDOSR)*PLEAF(B,N)*LEAF(B,N)           &
!JQI                 -AOCR*((BMLEAF(B)+PLEAF(B,N)*PRSPSAV(B))        &
!JQI                 *LEAF(B,N)+BMSTEM(B)*STEM(B,N))*FDOSAV)
!JQI       LDOCSAV(B) = LDOCSAV(B)+SAVFRAC(B,N)*                     &
!JQI                  ((LEAF(B,N)*(BMLEAF(B)+PLEAF(B,N)*PRSPSAV(B))  &
!JQI                  +STEM(B,N)*BMSTEM(B))*FCLDSAV)
 !JQI      RDOCSAV(B) = RDOCSAV(B)+SAVFRAC(B,N)*                     &
!JQI                   ((LEAF(B,N)*(BMLEAF(B)+PLEAF(B,N)*PRSPSAV(B)) &
!JQI                   +STEM(B,N)*BMSTEM(B))*FCRDSAV)
!JQI       LPOCSAV(B) = LPOCSAV(B)+SAVFRAC(B,N)*                     &
!JQI                   ((LEAF(B,N)*(BMLEAF(B)+PLEAF(B,N)*PRSPSAV(B)) &
!JQI                   +STEM(B,N)*BMSTEM(B))*FCLPSAV)
!JQI       RPOCSAV(B) = RPOCSAV(B)+SAVFRAC(B,N)*                     &
!JQI                   ((LEAF(B,N)*(BMLEAF(B)+PLEAF(B,N)*PRSPSAV(B)) &
!JQI                   +STEM(B,N)*BMSTEM(B))*FCRPSAV)

!JQI       DOEPI(B) = DOEPI(B)+SAVFRAC(B,N)*EP(B,N)*LEAF(B,N)*ALAC   &
!JQI                 *AOCR*(PEP(B,N)-BMEP(B,N)*(1.-FCLDEPI-FCRDEPI))
!JQI       LDOCEPI(B) = LDOCEPI(B)+SAVFRAC(B,N)*                     &
!JQI                   (EP(B,N)*LEAF(B,N)*ALAC*BMEP(B,N)*FCLDEPI     &
!JQI                   + EP(B,N)*LEAF(B,N)*ALAC*(PREP(B,N)+SLSH(B))*FCLDPEP)
!JQI       RDOCEPI(B) = RDOCEPI(B)+SAVFRAC(B,N)*                     &
!JQI                   (EP(B,N)*LEAF(B,N)*ALAC*BMEP(B,N)*FCRDEPI     &
!JQI                   + EP(B,N)*LEAF(B,N)*ALAC*(PREP(B,N)+SLSH(B))*FCRDPEP)
 !JQI      LPOCEPI(B) = LPOCEPI(B)+SAVFRAC(B,N)*                     &
!JQI                   (EP(B,N)*LEAF(B,N)*ALAC*(PREP(B,N)+SLSH(B))*FCLPPEP)
!JQI       RPOCEPI(B) = RPOCEPI(B)+SAVFRAC(B,N)*                     &
!JQI                   (EP(B,N)*LEAF(B,N)*ALAC*(PREP(B,N)+SLSH(B))   &
!JQI                   *(1.-FCLDPEP-FCRDPEP-FCLPPEP))

! NITROGEN AND PHOSPHORUS

!JQI       NH4SAVW(B) = NH4SAVW(B)+SAVFRAC(B,N)*                     &
!JQI                   (ANCSAV*FNISAV*((BMLEAF(B)+PLEAF(B,N)         &
 !JQI                  *PRSPSAV(B))*LEAF(B,N)+BMSTEM(B)*STEM(B,N))-  &
!JQI                   (1.-FNSEDSAV(B))*NPRSAV(B)*PLEAF(B,N)*ANCSAV*LEAF(B,N))
!JQI       NO3SAVW(B) = NO3SAVW(B)-SAVFRAC(B,N)*                     &
!JQI                   (1.-FNSEDSAV(B))*(1.-NPRSAV(B))*PLEAF(B,N)*   &
!JQI                    ANCSAV*LEAF(B,N)
!JQI       LDONSAVW(B) = LDONSAVW(B)+SAVFRAC(B,N)*FNLDSAV*ANCSAV     &
!JQI                    *((BMLEAF(B)+PLEAF(B,N)*PRSPSAV(B))          &
!JQI                    *LEAF(B,N)+BMSTEM(B)*STEM(B,N))
!JQI       RDONSAVW(B) = RDONSAVW(B)+SAVFRAC(B,N)*FNRDSAV*ANCSAV     &
!JQI                    *((BMLEAF(B)+PLEAF(B,N)*PRSPSAV(B))          &
!JQI                    *LEAF(B,N)+BMSTEM(B)*STEM(B,N))
!JQI       LPONSAVW(B) = LPONSAVW(B)+SAVFRAC(B,N)*FNLPSAV*ANCSAV     &
!JQI                    *((BMLEAF(B)+PLEAF(B,N)*PRSPSAV(B))          &
!JQI                    *LEAF(B,N)+BMSTEM(B)*STEM(B,N))
!JQI       RPONSAVW(B) = RPONSAVW(B)+SAVFRAC(B,N)*FNRPSAV*ANCSAV     &
!JQI                    *((BMLEAF(B)+PLEAF(B,N)*PRSPSAV(B))          &
!JQI                    *LEAF(B,N)+BMSTEM(B)*STEM(B,N))
!JQI
!JQI       NH4EPI(B) = NH4EPI(B)+SAVFRAC(B,N)*                       &
!JQI                  (FNIEPI*BMEP(B,N)+FNIPEP*(PREP(B,N)+SLSH(B))   &
!JQI                  -NPREPI(B)*PEP(B,N))*ANCEPI*EP(B,N)*LEAF(B,N)*ALAC
 !JQI      NO3EPI(B) = NO3EPI(B)-SAVFRAC(B,N)*(1.-NPREPI(B))*PEP(B,N) &
!JQI                  *ANCEPI*EP(B,N)*LEAF(B,N)*ALAC
 !JQI      RDONEPI(B) = RDONEPI(B)+SAVFRAC(B,N)*                     &
!JQI                   (FNRDEPI*BMEP(B,N)+FNRDPEP*(PREP(B,N)+SLSH(B)))  &
!JQI                   *ANCEPI*EP(B,N)*LEAF(B,N)*ALAC
!JQI       LDONEPI(B) = LDONEPI(B)+SAVFRAC(B,N)*                     &
!JQI                   (FNLDEPI*BMEP(B,N)+FNLDPEP*(PREP(B,N)+SLSH(B)))  &
!JQI                   *ANCEPI*EP(B,N)*LEAF(B,N)*ALAC
!JQI       LPONEPI(B) = LPONEPI(B)+SAVFRAC(B,N)*                     &
!JQI                   (FNLPEPI*BMEP(B,N)+FNLPPEP*(PREP(B,N)+SLSH(B)))  &
!JQI                   *ANCEPI*EP(B,N)*LEAF(B,N)*ALAC
!JQI       RPONEPI(B) = RPONEPI(B)+SAVFRAC(B,N)*                     &
!JQI                   (FNRPEPI*BMEP(B,N)+FNRPPEP*(PREP(B,N)+SLSH(B)))  &
!JQI                   *ANCEPI*EP(B,N)*LEAF(B,N)*ALAC

!JQI       PO4SAVW(B) = PO4SAVW(B)+SAVFRAC(B,N)*(APCSAV*FPISAV*      &
!JQI                   ((BMLEAF(B)+PLEAF(B,N)*PRSPSAV(B))            &
!JQI                   *LEAF(B,N)+BMSTEM(B)*STEM(B,N))               &
!JQI                   -(1.-FPSEDSAV(B))*PLEAF(B,N)*APCSAV*LEAF(B,N))
!JQI       LDOPSAVW(B) = LDOPSAVW(B)+SAVFRAC(B,N)*FPLDSAV*APCSAV     &
!JQI                    *((BMLEAF(B)+PLEAF(B,N)*PRSPSAV(B))          &
!JQI                    *LEAF(B,N)+BMSTEM(B)*STEM(B,N))
!JQI       RDOPSAVW(B) = RDOPSAVW(B)+SAVFRAC(B,N)*FPRDSAV*APCSAV     &
!JQI                    *((BMLEAF(B)+PLEAF(B,N)*PRSPSAV(B))          &
!JQI                    *LEAF(B,N)+BMSTEM(B)*STEM(B,N))
!JQI       LPOPSAVW(B) = LPOPSAVW(B)+SAVFRAC(B,N)*FPLPSAV*APCSAV     &
!JQI                    *((BMLEAF(B)+PLEAF(B,N)*PRSPSAV(B))          &
!JQI                    *LEAF(B,N)+BMSTEM(B)*STEM(B,N))
!JQI       RPOPSAVW(B) = RPOPSAVW(B)+SAVFRAC(B,N)*FPRPSAV*APCSAV     &
!JQI                    *((BMLEAF(B)+PLEAF(B,N)*PRSPSAV(B))          &
!JQI                    *LEAF(B,N)+BMSTEM(B)*STEM(B,N))

 !JQI      PO4EPI(B) = PO4EPI(B)+SAVFRAC(B,N)*                       &
!JQI                  (FPIEPI*BMEP(B,N)+FPIPEP*(PREP(B,N)+SLSH(B))   &
!JQI                  -PEP(B,N))*APCEPI*EP(B,N)*LEAF(B,N)*ALAC
!JQI       LDOPEPI(B) = LDOPEPI(B)+SAVFRAC(B,N)*                     &
!JQI!JQI                   (FPLDEPI*BMEP(B,N)+FPLDPEP*(PREP(B,N)+SLSH(B)))  &
!JQI                   *APCEPI*EP(B,N)*LEAF(B,N)*ALAC
!JQI       RDOPEPI(B) = RDOPEPI(B)+SAVFRAC(B,N)*                     &
!JQI                   (FPRDEPI*BMEP(B,N)+FPRDPEP*(PREP(B,N)+SLSH(B)))  &
!JQI                   *APCEPI*EP(B,N)*LEAF(B,N)*ALAC
!JQI       LPOPEPI(B) = LPOPEPI(B)+SAVFRAC(B,N)*                     &
!JQI!JQI                   (FPLPEPI*BMEP(B,N)+FPLPPEP*(PREP(B,N)+SLSH(B)))  &
!JQI                   *APCEPI*EP(B,N)*LEAF(B,N)*ALAC
!JQI       RPOPEPI(B) = RPOPEPI(B)+SAVFRAC(B,N)*                     &
 !JQI                  (FPRPEPI*BMEP(B,N)+FPRPPEP*(PREP(B,N)+SLSH(B)))  &
!JQI                   *APCEPI*EP(B,N)*LEAF(B,N)*ALAC

     END DO
      
   END DO

! INTERACTIONS WITH SEDIMENTS

!vjp modified 11/3/2005
   DO I=1,NSAVCELL
     B = SAVCELL(I)
     DO N=1,NSAVDPH(B)
      
! DISSOLVED OXYGEN AND CARBON
  
!JQI       SEDDOSAV(B) = SEDDOSAV(B)+SAVFRAC(B,N)*                    &
!JQI                    (LEAF(B,N)*AOCR*FDOSR*PLEAF(B,N)-             &
!JQI                    (ROOT(B,N)*BMROOT(B)+TUBER(B,N)*BMTUBER(B))*AOCR*FDOSAV)
!JQI       SEDCSAV(B) = SEDCSAV(B)+SAVFRAC(B,N)*                      &
!JQI                   (ROOT(B,N)*BMROOT(B)+TUBER(B,N)*BMTUBER(B))*   &
!JQI                   (1.-FDOSAV) 

! NITROGEN AND PHOSPHORUS

!JQI       SEDNH4SAV(B) = SEDNH4SAV(B)+SAVFRAC(B,N)*                  &
!JQI                      FNSEDSAV(B)*PLEAF(B,N)*ANCSAV*LEAF(B,N)
!JQI       SEDPO4SAV(B) = SEDPO4SAV(B)+SAVFRAC(B,N)*                  &
!JQI                      FPSEDSAV(B)*PLEAF(B,N)*APCSAV*LEAF(B,N)
!JQI       SEDNSAV(B)   = SEDNSAV(B)+SAVFRAC(B,N)*                    &
!JQI                     (ROOT(B,N)*BMROOT(B)+TUBER(B,N)*BMTUBER(B))*ANCSAV
!JQI       SEDPSAV(B)   = SEDPSAV(B)+SAVFRAC(B,N)*                    &
!JQI                     (ROOT(B,N)*BMROOT(B)+TUBER(B,N)*BMTUBER(B))*APCSAV

     END DO
   END DO

! COMPUTE NEW SHOOT, ROOT, AND EPIPHYTE BIOMASS

!vjp modified 11/3/2005
   DO I=1,NSAVCELL
     B = SAVCELL(I)

! DETERMINE DEGREE DAY INTERVAL.  SET MAXIMUM TO OCCURANCE OF SCENESCENCE
! TO PREVENT ARRAY OVERFLOW      
     J = MIN(IDGD(B),NDDI-1)
          
     DO N=1,NSAVDPH(B)
        
!JQI       LFOLD = LEAF(B,N)
!JQI       DELLF = (PLEAF(B,N)*(1.-PRSPSAV(B))*FPLEAF(J)-BMLEAF(B))     &
!JQI               *LEAF(B,N) + TRTBRLF(J)*TUBER(B,N)                   
!JQI       ROOT(B,N) = ROOT(B,N) + DLTDY*FPROOT(J)*PLEAF(B,N)*          &
!JQI                   (1.-PRSPSAV(B))*LEAF(B,N) - DLTDY*BMROOT(B)*ROOT(B,N)
!JQI       STEM(B,N) = STEM(B,N) + DLTDY*FPSTEM(J)*PLEAF(B,N)*          &
!JQI                   (1.-PRSPSAV(B))*LEAF(B,N) - DLTDY*BMSTEM(B)*STEM(B,N)
!JQI       LEAF(B,N) = LEAF(B,N) + DLTDY*DELLF
!JQI       TUBER(B,N) = TUBER(B,N) + DLTDY*FPTUBER(J)*PLEAF(B,N)*       &
!JQI                    (1.-PRSPSAV(B))*LFOLD                           &
!JQI                    - DLTDY*(BMTUBER(B)+TRTBRLF(J))*TUBER(B,N)
!JQI       EP(B,N) = EP(B,N)*LFOLD*(1.+DLTDY*(PEP(B,N)-BMEP(B,N)        &
!JQI                 -PREP(B,N)-SLSH(B)))/(LEAF(B,N)+1.0E-10)

     END DO
      
   END DO

! TEMPORARILY SET MINIMUM BIOMASS

!vjp modified 11/3/2005
   DO I=1,NSAVCELL
     B = SAVCELL(I)
     DO N=1,NSAVDPH(B)
!JQI       ROOT(B,N) = MAX(ROOT(B,N),0.01)
!JQI       LEAF(B,N) = MAX(LEAF(B,N),0.01)
!JQI       STEM(B,N) = MAX(STEM(B,N),0.01)
!JQI       TUBER(B,N) = MAX(TUBER(B,N),0.01)
!JQI       EP(B,N) = MAX(EP(B,N),0.01)
     END DO
   END DO
      
   RETURN
   END SUBROUTINE SAV_COMP



