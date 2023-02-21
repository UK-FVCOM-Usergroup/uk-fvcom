!************************************************************************
!**                  S U B R O U T I N E   T V D S                     **
!************************************************************************

    SUBROUTINE TVDS (NXTVD)
    USE WQM
    USE FILE_INFO
#   if defined (MULTIPROCESSOR)
    USE MOD_PAR
#   endif        
    SAVE
    REAL :: S2LNX, KTNX,  NXTVD
    DIMENSION S1LNX(NS1P,NCP), S2LNX(NS2P,NCP), S3LNX(NS3P,NCP),    &
                  BFLUXNX(MTLOC,NCP),BFLUXNX_GL(MGL,NCP),BFLUX(MTLOC,NCP)

!******* Meteorologic data

    NXTVD = TMEND
    DO WHILE(JDAY >= NXMET)
      KT = KTNX
      FD = FDNX

! CONVERT LANGLEYS TO EINSTEINS/M**2
! Commented conversion out since SJRWMD data already in einsteins/m**2
!         IT = 0.093*ITNX
      IT = ITNX
          
! TIME TO SUNRISE (SECONDS)
      TTSS = 86400.*(1-FD)/2.
          
      TE = MAX(TENX,0.0)
      WMS = WMSNX
      READ (MET,1010,END=11) NXMET,KTNX,TENX,ITNX,FDNX,WMSNX
      NXMET = (METPTR-1)*FILGTH+NXMET
    END DO 
    GO TO 12

!******* Open next data file

11  CONTINUE
    METPTR = METPTR+1
    IF (DIAGNOSTICS) WRITE (DIA,*) 'Opening meteorologic file ',    &
                                    METPTR,' at day ',JDAY
    CLOSE (MET)
    OPEN  (MET,FILE=METFN(METPTR),STATUS='OLD')
    READ  (MET,1000)

! INITIALIZE DEGREE DAYS TO ZERO

    DO I=1,MLOC
      DGRDAYS(I)=0
    END DO

    READ  (MET,1010) NXMET,KTNX,TENX,ITNX,FDNX,WMSNX
    NXMET = (METPTR-1)*FILGTH+NXMET
12  CONTINUE
    NXTVD = MIN(NXTVD,NXMET)

!******* Boundary inflow concentrations

!JQI    IF (BOUNDARY_CONC) THEN
!JQI      DO WHILE(JDAY >= NXCBC)THEN
!JQI        OLDNXCBC = NXCBC

!*********** Reduce/increase concentrations

!JQI        DO JC=4,12
!JQI          DO JCB=1,NCB(JC)
!JQI            CBNX(JCB,JC) = CBNX(JCB,JC)*REDCBC
!JQI	  END DO
!JQI        END DO    

!JQI        DO JC=13,19
!JQI          DO JCB=1,NCB(JC)
!JQI            CBNX(JCB,JC) = CBNX(JCB,JC)*REDCBN
!JQI          END DO
!JQI        END DO    
!JQI
!JQI        DO JC=20,25
!JQI          DO JCB=1,NCB(JC)
!JQI            CBNX(JCB,JC) = CBNX(JCB,JC)*REDCBP
!JQI          END DO
!JQI        END DO    

!JQI        DO JC=30,32
!JQI          DO JCB=1,NCB(JC)
!JQI            CBNX(JCB,JC) = CBNX(JCB,JC)*REDCBP
!JQI          END DO
!JQI        END DO
!JQI
!JQI        DO JC=1,NCP
!JQI          DO JCB=1,NCB(JC)
!JQI            CBOLD(JCB,JC) = CBNX(JCB,JC)
!JQI            CB(JCB,JC)    = CBNX(JCB,JC)
!JQI          END DO
!JQI        END DO    

!******* Read in next set of boundary conditions
!JQI
!JQI        DO JC=1,NCP
!JQI          READ (CBC,1020,END=16) NXCBC,(CBNX(JCB,JC),JCB=1,NCB(JC))
!JQI        END DO
!JQI        NXCBC = (CBCPTR-1)*FILGTH+NXCBC
!JQI      END DO
!JQI      GO TO 18

!********* Open next data file

!JQI16    CONTINUE
!JQI      CBCPTR = CBCPTR+1
!JQI      IF (DIAGNOSTICS) WRITE (DIA,*) 'Opening boundary concentra',   &
!JQI                                     'tion file ',CBCPTR,' at ',     &
!JQI                                     'day ',JDAY
!JQI      CLOSE (CBC)
!JQI      OPEN  (CBC,FILE=CBCFN(CBCPTR),STATUS='OLD')
!JQI      READ  (CBC,1080)
!JQI      READ  (CBC,1030) (NCB(JC),JC=1,NCP)
!JQI      READ  (CBC,1080)
!JQI      IF(NXCBC >= OLDNXCBC) THEN
!JQI        OLDNXCBC = NXCBC
!JQI      END IF    
!JQI      DO JC=1,NCP
!JQI        READ  (CBC,1020) NXCBC,(CBNX(JCB,JC),JCB=1,NCB(JC))
!JQI      END DO	
!JQI      NXCBC = (CBCPTR-1)*FILGTH+NXCBC
!JQI18    CONTINUE
!JQI      NXTVD = MIN(NXTVD,NXCBC)

!JQI    END IF

!******* Source One loads              !MNOEL   1-25-93

!JQI    IF (SOURCE_ONE) THEN
!JQI      DO WHILE(JDAY >= NXS1)
!JQI        DO JC=1,NCP
!JQI          DO JS1=1,S1LN(JC)
!JQI            S1L(JS1,JC) = S1LNX(JS1,JC)/86.4
!JQI	  END DO
!JQI	END DO    
! 
!*********** Reduce/increase concentrations

!JQI        DO JC=4,12
!JQI          DO JS1=1,S1LN(JC)
!JQI            S1L(JS1,JC) = S1L(JS1,JC)*REDS1C
!JQI	  END DO
!JQI	END DO    
!JQI        DO JC=13,19
!JQI          DO JS1=1,S1LN(JC)
!JQI            S1L(JS1,JC) = S1L(JS1,JC)*REDS1N
!JQI	  END DO
!JQI	END DO    
!JQI        DO JC=20,25
!JQI          DO JS1=1,S1LN(JC)
!JQI            S1L(JS1,JC) = S1L(JS1,JC)*REDS1P
!JQI	  END DO
!JQI	END DO    
!JQI        DO JC=30,32
!JQI          DO JS1=1,S1LN(JC)
!JQI            S1L(JS1,JC) = S1L(JS1,JC)*REDS1P
!JQI          END DO
!JQI        END DO

!*********** Read in next set of loads 

!JQI        DO JC=1,NCP
!JQI          READ (S1,1020,END=112) NXS1,(S1LNX(JS1,JC),JS1=1,S1LN(JC))
!JQI	END DO  
!JQI        NXS1 = (S1PTR-1)*FILGTH+NXS1
!JQI      END DO
!JQI      GO TO 115

!********* Open next data file

!JQI112   CONTINUE
!JQI      S1PTR = S1PTR+1
!JQI      IF (DIAGNOSTICS) WRITE (DIA,*) 'Opening Source One file ',    &
!JQI                                      S1PTR,' at day ',JDAY
!JQI      CLOSE (S1)
!JQI      OPEN  (S1,FILE=S1FN(S1PTR),STATUS='OLD')
!JQI      READ  (S1,1080)
!JQI      READ  (S1,1030) (S1LN(JC),JC=1,NCP)
!JQI      DO JC=1,NCP
!JQI        READ (S1,1030) (S1LB(JP,JC),JP=1,S1LN(JC))
!JQI      END DO	
!JQI      READ (S1,1080)
!JQI      DO JC=1,NCP
!JQI        READ (S1,1020) NXS1,(S1LNX(JS1,JC),JS1=1,S1LN(JC))
!JQI      END DO	
!JQI      NXS1 = (S1PTR-1)*FILGTH+NXS1
!JQI115   CONTINUE
!JQI      NXTVD = MIN(NXTVD,NXS1)
!JQI    END IF

!******* Source Two loads              !MNOEL   1-25-93

!JQI    IF (SOURCE_TWO) THEN
!JQI      DO WHILE(JDAY >= NXS2)
!JQI        DO JC=1,NCP
!JQI          DO JS2=1,S2LN(JC)
!JQI            S2L(JS2,JC) = S2LNX(JS2,JC)/86.4
!JQI	  END DO
!JQI	END DO    

!*********** Reduce/increase concentrations

!JQI        DO JC=4,12
!JQI          DO JS2=1,S2LN(JC)
!JQI            S2L(JS2,JC) = S2L(JS2,JC)*REDS2C
!JQI	  END DO
!JQI	END DO    
!JQI        DO JC=13,19
!JQI          DO JS2=1,S2LN(JC)
!JQI            S2L(JS2,JC) = S2L(JS2,JC)*REDS2N
!JQI	  END DO
!JQI	END DO    
!JQI        DO JC=20,25
!JQI          DO JS2=1,S2LN(JC)
!JQI            S2L(JS2,JC) = S2L(JS2,JC)*REDS2P
!JQI	  END DO
!JQI	END DO    
!JQI        DO JC=30,32
!JQI          DO JS2=1,S2LN(JC)
!JQI            S2L(JS2,JC) = S2L(JS2,JC)*REDS2P
!JQI          END DO
!JQI        END DO

!*********** Read in next set of loads 

!JQI        DO JC=1,NCP
!JQI          READ (S2,1020,END=119) NXS2,(S2LNX(JS2,JC),JS2=1,S2LN(JC))
!JQI	END DO  
!JQI        NXS2 = (S2PTR-1)*FILGTH+NXS2
!JQI      END DO
!JQI      GO TO 122

!********* Open next data file

!JQI119   CONTINUE
!JQI      S2PTR = S2PTR+1
!JQI      IF (DIAGNOSTICS) WRITE (DIA,*) 'Opening Source Two ',        &
!JQI                                     'file ',S2PTR,' at day ',     &
!JQI                                      JDAY
!JQI      CLOSE (S2)
!JQI      OPEN  (S2,FILE=S2FN(S2PTR),STATUS='OLD')
!JQI      READ  (S2,1080)
!JQI      READ  (S2,1030) (S2LN(JC),JC=1,NCP)
!JQI      DO JC=1,NCP
!JQI        READ (S2,1030) (S2LB(JNP,JC),JNP=1,S2LN(JC))
!JQI      END DO	
!JQI      READ (S2,1080)
!JQI      DO JC=1,NCP
!JQI        READ (S2,1020) NXS2,(S2LNX(JS2,JC),JS2=1,S2LN(JC))
!JQI      END DO	
!JQI      NXS2 = (S2PTR-1)*FILGTH+NXS2
!JQI122   CONTINUE
!JQI      NXTVD = MIN(NXTVD,NXS2)
!JQI    END IF

!******* Source Three loads              

!JQI    IF (SOURCE_THR) THEN
!JQI      DO WHILE(JDAY >= NXS3)
!JQI        DO JC=1,NCP
!JQI          DO JS3=1,S3LN(JC)
!JQI            S3L(JS3,JC) = S3LNX(JS3,JC)/86.4
!JQI          END DO
!JQI        END DO
            
!*********** Reduce/increase concentrations

!JQI        DO JC=4,12
!JQI          DO JS3=1,S3LN(JC)
!JQI            S3L(JS3,JC) = S3L(JS3,JC)*REDS3C
!JQI          END DO
!JQI        END DO
!JQI        DO JC=13,19
!JQI          DO JS3=1,S3LN(JC)
!JQI            S3L(JS3,JC) = S3L(JS3,JC)*REDS3N
!JQI          END DO
!JQI        END DO
!JQI        DO JC=20,25
!JQI          DO JS3=1,S3LN(JC)
!JQI            S3L(JS3,JC) = S3L(JS3,JC)*REDS3P
!JQI          END DO
!JQI        END DO
!JQI        DO JC=30,32
!JQI          DO JS3=1,S3LN(JC)
!JQI            S3L(JS3,JC) = S3L(JS3,JC)*REDS3P
!JQI          END DO
!JQI        END DO

!*********** Read in next set of loads 

!JQI        DO JC=1,NCP
!JQI          READ (S3,1020,END=124) NXS3,(S3LNX(JS3,JC),JS3=1,S3LN(JC))
!JQI        END DO
!JQI        NXS3 = (S3PTR-1)*FILGTH+NXS3
!JQI      END DO	
!JQI      GO TO 125

!********* Open next data file

!JQI124   CONTINUE
!JQI      S3PTR = S3PTR+1
!JQI      IF (DIAGNOSTICS) WRITE (DIA,*) 'Opening Source Three ',     &
!JQI                                     'file ',S3PTR,' at day ',    &
!JQI                                      JDAY
!JQI      CLOSE (S3)
!JQI      OPEN  (S3,FILE=S3FN(S3PTR),STATUS='OLD')
!JQI      READ  (S3,1080)
!JQI      READ  (S3,1030) (S3LN(JC),JC=1,NCP)
!JQI      DO JC=1,NCP
!JQI        READ (S3,1030) (S3LB(JNP,JC),JNP=1,S3LN(JC))
!JQI      END DO
!JQI      READ (S3,1080)
!JQI      DO JC=1,NCP
!JQI        READ (S3,1020) NXS3,(S3LNX(JS3,JC),JS3=1,S3LN(JC))
!JQI      END DO
!JQI      NXS3 = (S3PTR-1)*FILGTH+NXS3
!JQI125   CONTINUE
!JQI      NXTVD = MIN(NXTVD,NXS3)
!JQI    END IF


!******* Benthic fluxes

    IF (BENTHIC_FLUXES) THEN
      DO WHILE(JDAY >= NXBFI)
        DO JC=1,9
          DO BB=1,MLOC
            BFLUX(BB,JC) = BFLUXNX(BB,JC)
	  END DO
	END DO  
	
	BENDOC = BFLUX(:,1)
	BENNH4 = BFLUX(:,2)
	BENNO3 = BFLUX(:,3)
	BENDON = BFLUX(:,4)
	BENPO4 = BFLUX(:,5)
	BENDOP = BFLUX(:,6)
	BENCOD = BFLUX(:,7)
	BENDO  = BFLUX(:,8)
	BENSA  = BFLUX(:,9)
	  
        DO JC=1,9
          READ (BFI,1020,END=127) NXBFI,(BFLUXNX_GL(BB,JC),BB=1,MGL)
	END DO  
	
	IF(SERIAL) BFLUXNX = BFLUXNX_GL
	
#   if defined (MULTIPROCESSOR)
        IF(PAR)THEN
          DO JC = 1,9
	    DO I=1,MLOC
	      BFLUXNX(I,JC) = BFLUXNX_GL(NGID(I),JC)
	    END DO
	    DO I=1,NHN
	      BFLUXNX(I+MLOC,JC) = BFLUXNX_GL(HN_LST(I),JC)
	    END DO
	  END DO  
	END IF
#   endif	      	
        DO BB=1,MLOC
!JQI          BFLUXB(BB,1) = BFLUX(BB,1)                              ! DOC
!JQI          BFLUXB(BB,2) = BFLUX(BB,2)                              ! NH4
!JQI          BFLUXB(BB,3) = BFLUX(BB,3)                              ! NO3
!JQI          BFLUXB(BB,4) = BFLUX(BB,5)                              ! PO4
!JQI          BFLUXB(BB,5) = BFLUX(BB,7)                              ! COD
!JQI          BFLUXB(BB,6) = BFLUX(BB,8)                              ! DO
!JQI          BFLUXB(BB,7) = BFLUX(BB,9)                              ! SA
          BENDOCB(BB) = BFLUX(BB,1)                              ! DOC
          BENNH4B(BB) = BFLUX(BB,2)                              ! NH4
          BENNO3B(BB) = BFLUX(BB,3)                              ! NO3
          BENPO4B(BB) = BFLUX(BB,5)                              ! PO4
          BENCODB(BB) = BFLUX(BB,7)                              ! COD
          BENDOB(BB)  = BFLUX(BB,8)                              ! DO
          BENSAB(BB)  = BFLUX(BB,9)                              ! SA
	END DO  
        NXBFI = (BFIPTR-1)*FILGTH+NXBFI
      END DO
      GO TO 128

!********* Open next data file

127   CONTINUE
      BFIPTR = BFIPTR+1
      IF (DIAGNOSTICS) WRITE (DIA,*) 'Opening benthic flux file ',   &
                                      BFIPTR,' at day ',JDAY
      CLOSE (BFI)
      OPEN  (BFI,FILE=BFIFN(BFIPTR),STATUS='OLD')
      READ  (BFI,1000)
      READ  (BFI,1050)  KSDOC,KSNH4,KSNO3,KSPO4,KSO,KSSA
      READ  (BFI,1050)  TRSDOC,TRSNH4,TRSNO3,TRSPO4,TRSO,TRSSA
      READ  (BFI,1050)  MTCNO3, SEDNO3, KHSO  
      READ  (BFI,1020)  NXBFI,(BFLUXNX_GL(BB,JC),BB=1,MGL)
      NXBFI = (BFIPTR-1)*FILGTH+NXBFI

      IF(SERIAL) BFLUXNX(:,JC) = BFLUXNX_GL(:,JC)
	
#     if defined (MULTIPROCESSOR)
      IF(PAR)THEN
        DO I=1,MLOC
	  BFLUXNX(I,JC) = BFLUXNX_GL(NGID(I),JC)
        END DO
        DO I=1,NHN
	  BFLUXNX(I+MLOC,JC) = BFLUXNX_GL(HN_LST(I),JC)
        END DO
      END IF
#   endif	      	
128   CONTINUE
      NXTVD = MIN(NXTVD,NXBFI)
    END IF


!******* Atmospheric Loads

    IF (ATMOS_LOADS) THEN
      DO WHILE(JDAY >= NXATM)
        PRECIP = PRECIPNX/8640000.
        ATMNH4 = ANH4NX
        ATMNO3 = ANO3NX
        ATMLDON = ALDONNX
        ATMRDON = ARDONNX
        ATMPO4 = APO4NX
        ATMLDOP = ALDOPNX
        ATMRDOP = ARDOPNX
        READ (ATM,1010,END=134) NXATM,PRECIPNX,ANH4NX,ANO3NX,      &
              ALDONNX,ARDONNX,APO4NX,ALDOPNX,ARDOPNX
        NXATM = (ATMPTR-1)*FILGTH+NXATM
      END DO
      GO TO 135

!********* Open next data file

134   CONTINUE
      ATMPTR = ATMPTR+1
      IF (DIAGNOSTICS) WRITE (DIA,*) 'Opening atmospheric load',   &
                                     'ing file ',ATMPTR,' at ',    &
                                     'day ',JDAY
      CLOSE (ATM)
      OPEN  (ATM,FILE=ATMFN(ATMPTR),STATUS='OLD')
      READ  (ATM,1000)
      READ  (ATM,1010) NXATM,PRECIPNX,ANH4NX,ANO3NX,ALDONNX,       &
                       ARDONNX,APO4NX,ALDOPNX,ARDOPNX
      NXATM = (ATMPTR-1)*FILGTH+NXATM
135   CONTINUE
      NXTVD = MIN(NXTVD,NXATM)
    END IF

!******* Submerged Aquatic Vegetation

    IF (SAV_LOADS) THEN
      DO WHILE(JDAY >= NXSAV)
        DO B=1,MLOC
          FACTOR = SAVAREA(B)/SFA(B)
          LDOCSAV(B) = SDOCNX * FACTOR
          LPOCSAV(B) = SLPOCNX * FACTOR 
          RPOCSAV(B) = SRPOCNX * FACTOR
          DOSAV(B) = SDONX * FACTOR
	END DO  
        READ (SVI,1010,END=137) NXSAV, SDOCNX, SLPOCNX, SRPOCNX, SDONX
        NXSAV = (SAVPTR-1)*FILGTH+NXSAV
      END DO
      GO TO 138

!********* Open next data file

137   CONTINUE
      SAVPTR = SAVPTR+1
      IF (DIAGNOSTICS) WRITE (DIA,*) 'Opening aquatic vegetation',   &
                                     ' file ',SAVPTR,' at day ',     &
                                      JDAY
      CLOSE (SVI)
      OPEN  (SVI,FILE=SVIFN(SAVPTR),STATUS='OLD')
      READ  (SVI,1080)
      READ  (SVI,1026) (SAVAREA(B),B=1,NBB)
      READ  (SVI,1080)
      READ  (SVI,1010) NXSAV, SDOCNX, SLPOCNX, SRPOCNX, SDONX
      NXSAV = (SAVPTR-1)*FILGTH+NXSAV
138   CONTINUE
      NXTVD = MIN(NXTVD,NXSAV)
    END IF


!******* Input FORMAT's

1000 FORMAT(///)
1010 FORMAT(10F8.0,:/(:8X,9F8.0))
1020 FORMAT(8X,9F8.0,:/(:16X,8F8.0))
1025 FORMAT(16X,F8.0)
1026 FORMAT(//(8X,9F8.0))
1030 FORMAT(//(8X,9I8))
1040 FORMAT(/)
1050 FORMAT(://8X,9F8.0)
1060 FORMAT(8X,9F8.0)
1070 FORMAT(//(:8X,6F8.0))
1080 FORMAT(/)
1082 FORMAT(//8X,2A8//)
1100 FORMAT(/)

   END SUBROUTINE TVDS
 
