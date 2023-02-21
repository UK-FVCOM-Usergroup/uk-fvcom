      SUBROUTINE RCA10(SVALS,BVALS,NDIM,NO,NXT,IFILE,IPWLOPT,SCALE)
! 
!     RCA10 UPDATES THE FORCING FUNCTIONS
! 
      USE ALL_VARS, ONLY : MSR,MYID
      USE MOD_RCA
      IMPLICIT NONE
      SAVE
!      INCLUDE 'RCACM' 
      INTEGER :: NDIM
      REAL    :: NXT,SVALS(NDIM,NOSYS),BVALS(NDIM,NOSYS),SCALE(NOSYS)
      INTEGER :: NO(NOSYS),IBINOPT
      INTEGER :: IFILE,ISCALE,IPWLOPT,NXTSECS
      REAL    :: OLDTIME,SVALC,BVALC,TOTLD
      INTEGER :: I

      IF(MSR) WRITE(OUT,'(//)')
      IF(IFILE == 33) THEN   !POINT SOURCE
        IBINOPT = IBNRYRDOPTS(2)
        ISCALE = ISCALPS
      ELSEIF(IFILE == 34) THEN   !Non-point source
        IBINOPT = IBNRYRDOPTS(3)
        ISCALE = ISCALNPS
      ELSEIF(IFILE == 35) THEN   !River
        IBINOPT = IBNRYRDOPTS(4)
        ISCALE = ISCALFL
      ELSEIF(IFILE == 36) THEN   !Atmosphere
        IBINOPT = IBNRYRDOPTS(5)
        ISCALE = ISCALATM
      ELSE
        IF(MSR) PRINT *,'*** Error in rca10.f - Invalid file number: IFILE = ',IFILE
        STOP
      ENDIF

      IF(IPWLOPT == 1) THEN
        OLDTIME=NXT
        IF(IBINOPT == 0) THEN
         READ(IFILE,'(10X,I10)',ERR=950,END=970)  NXTSECS
        ELSE
         READ(IFILE,ERR=960,END=970)  NXTSECS
        ENDIF
        NXTSECS=ISCALE*NXTSECS
        NXT=NXTSECS/86400.
      ENDIF

      DO 100 ISYS=1,NOSYS 
       IF(NO(ISYS) /= 0) THEN
        IF(IFILE == 33) THEN
         IF(IPWLOPT == 0) THEN
          IF(MSR) WRITE(OUT,2601)   ISYS,NXT
 2601     FORMAT(30X,'POINT SOURCE LOADINGS FOR SYSTEM',I4,' AT TIME =',F8.2,' DAYS')
          IF(LIST(3) == 1 .AND. MSR) WRITE(OUT,2701)
 2701     FORMAT(/12X,8(4X,'PS(T)',4X))
         ELSE
          IF(INITB /= 0) THEN
            IF(MSR) WRITE(OUT,2601)   ISYS,OLDTIME
            IF(LIST(3) == 1 .AND. MSR) WRITE(OUT,2701)
          ENDIF
         ENDIF
        ENDIF
        IF(IFILE == 34) THEN
         IF(IPWLOPT == 0) THEN
          IF(MSR) WRITE(OUT,2602)   ISYS,NXT
 2602     FORMAT(30X,'NONPOINT SOURCE LOADINGS FOR SYSTEM',I4,' AT TIME =',F8.2,' DAYS')
          IF(LIST(3) == 1 .AND. MSR) WRITE(OUT,2702)
 2702     FORMAT(/12X,8(4X,'NPS(T)',3X))
         ELSE
          IF(INITB /= 0) THEN
            IF(MSR) WRITE(OUT,2602)   ISYS,OLDTIME
            IF(LIST(3) == 1 .AND. MSR) WRITE(OUT,2702)
          ENDIF
         ENDIF
        ENDIF
        IF(IFILE == 35) THEN
         IF(IPWLOPT == 0) THEN
          IF(MSR) WRITE(OUT,2603)   ISYS,NXT
 2603     FORMAT(30X,'FALL-LINE LOADINGS FOR SYSTEM',I4,' AT TIME =',F8.2,' DAYS')
          IF(LIST(3) == 1 .AND. MSR) WRITE(OUT,2703)
 2703     FORMAT(/12X,8(4X,'FL(T)',4X))
         ELSE
          IF(INITB /= 0) THEN
            IF(MSR) WRITE(OUT,2603)   ISYS,OLDTIME
            IF(LIST(3) == 1 .AND. MSR) WRITE(OUT,2703)
          ENDIF
         ENDIF
        ENDIF
        IF(IFILE == 36) THEN
         IF(IPWLOPT == 0) THEN
          IF(MSR) WRITE(OUT,2604)   ISYS,NXT
 2604     FORMAT(30X,'ATMOSPHERIC LOADINGS FOR SYSTEM',I4,' AT TIME =',F8.2,' DAYS')
         ELSE
          IF(INITB /= 0) THEN
            IF(MSR) WRITE(OUT,2604)   ISYS,OLDTIME
          ENDIF
         ENDIF
        ENDIF

        IF(IPWLOPT == 1) THEN
         IF(IFILE /= 36) THEN 
          DO I=1,NO(ISYS)
           SVALS(I,ISYS)=BVALS(I,ISYS)
          ENDDO
         ELSE
          SVALC=0.0
          DO IXY=1,NXY
           SVALS(IXY,ISYS) = BVALS(IXY,ISYS)
           IF(BVALS(IXY,ISYS) /= 0.0) SVALC=BVALS(IXY,ISYS)
          ENDDO
         ENDIF
        ENDIF

        IF(IBINOPT == 0) THEN
         IF(IFILE /= 36) THEN
          READ(IFILE,2750,ERR=980,END=970) (BVALS(I,ISYS),I=1,NO(ISYS))
 2750     FORMAT(10X,7F10.0)
         ELSE
          IF(NO(ISYS) == 1) THEN
           READ(IFILE,2750,ERR=960,END=970)  BVALC
           DO IXY=1,NXY
            BVALS(IXY,ISYS) = BVALC
           ENDDO
          ELSE
           READ(IFILE,2750,ERR=990,END=970)  (BVALS(IXY,ISYS),IXY=1,NXY)
           DO IXY=1,NXY
            IF(FSM(IXY) /= 1.)  BVALS(IXY,ISYS)=0.0
           ENDDO
          ENDIF
         ENDIF

        ELSE

         IF(IFILE /= 36) THEN
          READ(IFILE,ERR=960,END=970) (BVALS(I,ISYS),I=1,NO(ISYS))
         ELSE
          IF(NO(ISYS) == 1) THEN
           READ(IFILE,ERR=960,END=970)  BVALC
           DO IXY=1,NXY
            BVALS(IXY,ISYS) = BVALC
           ENDDO
          ELSE
           READ(IFILE,ERR=960,END=970)  (BVALS(IXY,ISYS),IXY=1,NXY)
           DO IXY=1,NXY
            IF(FSM(IXY) /= 1.)  BVALS(IXY,ISYS)=0.0
           ENDDO
          ENDIF
         ENDIF

        ENDIF

        IF(LIST(3) == 1) THEN
         IF(IFILE /= 36) THEN
          IF(IPWLOPT == 0) THEN
           IF(MSR) WRITE(OUT,2800) (BVALS(I,ISYS),I=1,NO(ISYS))
 2800      FORMAT(10X,8E13.3)
          ELSE
           IF(INITB /= 0 .AND. MSR)                                     &
             WRITE(OUT,2800) ((SVALS(I,ISYS)/1000.),I=1,NO(ISYS))
          ENDIF
         ELSE
          IF(NO(ISYS) == 1) THEN
           IF(IPWLOPT == 0 .AND. MSR) WRITE(OUT,2800)  BVALC
           IF(IPWLOPT == 1 .AND. INITB /= 0)THEN
	    IF(MSR) WRITE(OUT,2800) SVALC/1000.
	   END IF 
          ELSE
           IF(IPWLOPT == 0 .AND. MSR)                                  &
             WRITE(OUT,2850)  (BVALS(IXY,ISYS),IXY=1,NXY)
           IF(IPWLOPT == 1 .AND. INITB /= 0)THEN
            IF(MSR) WRITE(OUT,2850) ((SVALS(IXY,ISYS)/1000.),IXY=1,NXY)
           END IF
 2850      FORMAT(8E13.3/(5X,8E13.3))
          ENDIF
         ENDIF
        ENDIF

!     Multiply by scale factor and convert to MG*M^3/DAY-L

        TOTLD = 0.0
        IF(IFILE /= 36) THEN
         DO 40 I=1,NO(ISYS)
          BVALS(I,ISYS) = SCALE(ISYS)*BVALS(I,ISYS)*1000.
          IF(IPWLOPT == 0) THEN
           TOTLD = TOTLD + BVALS(I,ISYS)
          ELSE
           TOTLD = TOTLD + SVALS(I,ISYS)
          ENDIF
   40    CONTINUE
        ELSE
         IF(IFILE == 36) THEN
          TOTLD = 0.0
          DO 50 IXY=1,NXY
           BVALS(IXY,ISYS) = 1000.*SCALE(ISYS)*BVALS(IXY,ISYS)
           IF(IPWLOPT == 0) THEN
            ! TOTLD = TOTLD + XAZ(IXY)*BVALS(IXY,ISYS)
             TOTLD = TOTLD + XAZ_TMP(IXY)*BVALS(IXY,ISYS)	     
           ELSE
             !TOTLD = TOTLD + XAZ(IXY)*SVALS(IXY,ISYS)
             TOTLD = TOTLD + XAZ_TMP(IXY)*SVALS(IXY,ISYS)	     
           ENDIF
   50     CONTINUE
         ENDIF
        ENDIF
  
        IF(IPWLOPT == 0 .OR. INITB /= 0)THEN
	 WRITE(OUT,2900)  ISYS,TOTLD/1000.
	ENDIF 
 2900   FORMAT(35X,'TOTAL LOADING FOR SYSTEM',I3,' =',E13.4,' KG/DAY'/)

        IF(IPWLOPT == 1) THEN
         IF(IFILE /= 36) THEN
          DO I=1,NO(ISYS)
           SVALS(I,ISYS)=(SVALS(I,ISYS)-BVALS(I,ISYS))/(OLDTIME-NXT)
          ENDDO
         ELSE
          DO IXY=1,NXY
           SVALS(IXY,ISYS)=(SVALS(IXY,ISYS)-BVALS(IXY,ISYS))/(OLDTIME-NXT)
          ENDDO
         ENDIF
        ENDIF
       END IF
  100 CONTINUE

      IF(IPWLOPT == 0) THEN
       IF(IBINOPT == 0) THEN
        READ(IFILE,'(10X,I10)',ERR=950,END=970)  NXTSECS
       ELSE
        READ(IFILE,ERR=960,END=970)  NXTSECS
       ENDIF
       NXTSECS=ISCALE*NXTSECS
       NXT=NXTSECS/86400.
      ENDIF

      RETURN

  950 IN=IFILE
      IF(MSR) WRITE(OUT,9500)
 9500 FORMAT(///20X,'ERROR ... INPUT ERROR ENCOUNTERED WHILE ATTEMPTING ',   &
            'TO READ NEXT TIME FOR LOADING FILE'/20X,'RCA TERMINATED'//)
      CALL FMTER
!      CALL EXIT
      CALL PSTOP('rca10_239 ')
  960 IF(MSR) WRITE(OUT,9600)  IFILE,ISYS,ITIMESECS
 9600 FORMAT(///20X,'ERROR READING BINARY FILE NUMBER ',I3,' FOR SYSTEM',    &
            I3,' AT TIME =',I10,' SECONDS (',E10.3,' DAYS)')
!      CALL EXIT
      CALL PSTOP('rca10_244 ')
  970 IF(MSR) WRITE(OUT,9700)
 9700 FORMAT(//5X,'END OF FILE ENCOUNTERED WHILE READING LOADING FILE'//)
      INPCHCK=2
      RETURN
  980 IF(MSR) WRITE(OUT,9800)  ISYS
 9800 FORMAT(///20X,'ERROR ... INPUT ERROR ENCOUNTERED WHILE ATTEMPTING ',   &
            'TO READ LOADS FOR SYSTEM',I3/20X,'RCA TERMINATED')
      CALL FMTER
!      CALL EXIT
      CALL PSTOP('rca10_254 ')
  990 IF(MSR) WRITE(OUT,9900)  ISYS
 9900 FORMAT(///20X,'ERROR ... INPUT ERROR ENCOUNTERED WHILE ATTEMPTING ',   &
            'TO READ ATMOSPHERIC LOADS FOR SYSTEM',I3/20X,'RCA TERMINATED')
      CALL FMTER
!      CALL EXIT
      CALL PSTOP('rca10_260 ')
      END SUBROUTINE RCA10

!======================================================================      
      SUBROUTINE GLOB2LOC10
      USE ALL_VARS,ONLY : PAR,M,MT
      USE MOD_RCA
# if defined (MULTIPROCESSOR)
      USE MOD_PAR
# endif
      IMPLICIT NONE
      INTEGER :: I,I1,NCNT
      REAL,ALLOCATABLE,DIMENSION(:)    :: TEMP4,TEMP5
      REAL,ALLOCATABLE,DIMENSION(:,:)  :: TEMP6,TEMP7
      
# if defined (MULTIPROCESSOR)
       IF(PAR)THEN
        DO ISYS = 1,NOSYS
         ALLOCATE(TEMP4(NOPS_GL(ISYS))); TEMP4 = 0.0
         ALLOCATE(TEMP5(NOPS_GL(ISYS))); TEMP5 = 0.0
         NCNT = 0
         !!SET UP LOCAL POINT SOURCE NODES
         DO I=1,NOPS_GL(ISYS)
          I1 = NLID(IPS_GL(1,I,ISYS))
          IF(I1 /= 0)THEN
           NCNT = NCNT + 1
	   TEMP4(NCNT) = BPS_GL(I,ISYS)
	   TEMP5(NCNT) = SPS_GL(I,ISYS)
          END IF
         END DO
         IF(NCNT > 0)THEN
	  BPS(1:NCNT,ISYS)   = TEMP4(1:NCNT) 
	  SPS(1:NCNT,ISYS)   = TEMP5(1:NCNT) 
	 END IF        
         DEALLOCATE(TEMP4,TEMP5)

         ALLOCATE(TEMP4(NONPS(ISYS))); TEMP4 = 0.0
         ALLOCATE(TEMP5(NONPS(ISYS))); TEMP5 = 0.0
         NCNT = 0
         !!SET UP LOCAL NON POINT SOURCE NODES
         DO I=1,NONPS_GL(ISYS)
          I1 = NLID(INPS_GL(1,I,ISYS))
          IF(I1 /= 0)THEN
           NCNT = NCNT + 1
	   TEMP4(NCNT) = BNPS_GL(I,ISYS)
	   TEMP5(NCNT) = SNPS_GL(I,ISYS)
          END IF
         END DO
         IF(NCNT > 0)THEN
	  BNPS(1:NCNT,ISYS)   = TEMP4(1:NCNT) 
	  SNPS(1:NCNT,ISYS)   = TEMP5(1:NCNT) 
	 END IF        
         DEALLOCATE(TEMP4,TEMP5)

         ALLOCATE(TEMP4(NOFL(ISYS))); TEMP4 = 0.0
         ALLOCATE(TEMP5(NOFL(ISYS))); TEMP5 = 0.0
         NCNT = 0
         !!SET UP LOCAL FALL-LINE NODES
         DO I=1,NOFL_GL(ISYS)
          I1 = NLID(IFL_GL(1,I,ISYS))
          IF(I1 /= 0)THEN
           NCNT = NCNT + 1
	   TEMP4(NCNT) = BFL_GL(I,ISYS)
	   TEMP5(NCNT) = SFL_GL(I,ISYS)
          END IF
         END DO
         IF(NCNT > 0)THEN
	  BFL(1:NCNT,ISYS)   = TEMP4(1:NCNT) 
	  SFL(1:NCNT,ISYS)   = TEMP5(1:NCNT) 
	 END IF        
         DEALLOCATE(TEMP4,TEMP5)
	END DO 
	 
        ALLOCATE(TEMP6(MT,NOSYS)); TEMP6 = 0.0
        ALLOCATE(TEMP7(MT,NOSYS)); TEMP7 = 0.0
        DO ISYS = 1,NOSYS
         DO IXY=1,M
          TEMP6(IXY,ISYS) = BATM_GL(NGID(IXY),ISYS) 
          TEMP7(IXY,ISYS) = SATM_GL(NGID(IXY),ISYS) 
	 END DO
	 DO IXY=1,NHN
	  TEMP6(IXY+M,ISYS) = BATM_GL(HN_LST(IXY),ISYS)
	  TEMP7(IXY+M,ISYS) = SATM_GL(HN_LST(IXY),ISYS)
         END DO
	END DO 

      !  ALLOCATE(BATM(MT,NOSYS))  ;BATM = 0.0
      !  ALLOCATE(SATM(MT,NOSYS)) ; SATM = 0.0
	
        BATM(1:MT,:) =  TEMP6(1:MT,:)
	SATM(1:MT,:) =  TEMP7(1:MT,:)
	
	DEALLOCATE(TEMP6,TEMP7)
       END IF 
# endif
      RETURN
      END SUBROUTINE GLOB2LOC10
      
      
