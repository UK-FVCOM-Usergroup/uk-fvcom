      SUBROUTINE RCA01
!
!     RCA01 IS THE INITILIZATION ROUTINE
!
      USE ALL_VARS, ONLY : MSR,myid
      USE MOD_RCA

      IMPLICIT NONE
      SAVE
      CHARACTER(LEN=80) :: COMMENT,TITLE
      CHARACTER(LEN=15) :: TYPE
      INTEGER(2) :: SYSGDP(40)
      INTEGER :: I
!      INCLUDE  'RCACM'

!     INITIAL -SYSBY- SETTINGS
      SYSGDP = 1

!     DEFINE I/O DEVICES
      IN  = 5
      OUT = 6
      
      OPEN(IN,FILE='mbfull3_fvcom.inp2006')  
 
!     THE -LIST- ARRAY CONTROLS THE LISTING OF THE USERS INPUT DECK
!      LIST(#)        CONTROLS THE LISTING OF
!           1   GEOMETRY...EXCHANGES, VOLUMES AND FLOWS
!           2   BOUNDARY CONDITIONS
!           3   LOADINGS
!           4   PARAMETERS, CONSTANTS AND MISC TIME FNS
!           5   INITIAL CONDITIONS
!     LIST = 1   LIST INPUT
!     LIST = 0   DO NOT LIST INPUT
!

      READ(IN,'(A)') COMMENT           
      READ(IN,'(8I10)',ERR=950) CYCLE,LIST,IDIAGDT,INPCHCK
      IF(MSR) WRITE(OUT,1500)  NXY,EXY,NZ,NOSYS
 1500 FORMAT( 1H1/1X,120('*')/1X,120('*')////                           &
         25X,'R C A 3 D   - -  R O W / C O L U M N   A E S O P'/        &
         25X,'                  ( 3 - D I M E N S I O N A L )'//        &
         70X,'Version 3.00  Released June 30,2004'/                     &
         70X,'DEVELOPED BY HYDROQUAL INC.  MAHWAH, NJ'/                 &
         70X,'THIS EXECUTABLE COMPILED FOR:  NXY =',I5/                 &
         93X,'        EXY =',I5/                                        &
         94X,'        NZ =',I5/98X,' NOSYS =',I5/                       &
         ////1X,120('*')/1X,120('*')//)
      IF(CYCLE == 0) THEN
       IF(MSR) WRITE(OUT,                                                &
       '(15X,"INITIAL CONDITIONS WILL BE READ FROM USER INPUT DECK"//)')
      ELSE
       IF(MSR) WRITE(OUT,                                                &
        '(15X,"INITIAL CONDITIONS WILL BE READ FROM RCAFIC"//)')
      ENDIF

      READ(IN,'(A)') COMMENT
   10 READ(IN,'(A)') TITLE
      IF(TITLE(1:3) /= 'END' .AND. TITLE(1:3) /= 'end') THEN
       IF(MSR) WRITE(OUT,'(10X,A80)') TITLE
       GO TO 10
      ENDIF
      IF(MSR) WRITE(OUT,'(////1X,120("*")/1X,120("*")////)')
!     READ STATE VARIABLE NAMES
      READ(IN,'(A)') COMMENT
      READ(IN,'(10A8)',ERR=950)  (SYNAME(I),I=1,NOSYS)
      IF(MSR) WRITE(OUT,2250)  (I,SYNAME(I),I=1,NOSYS)
 2250 FORMAT(5X,'THE FOLLOWING STATE-VARIABLES ARE BEING MODELED'/ 20X,   &
         'SYSTEM     VARIABLE'/20X,6('-'),5X,8('-')/(20X,I4,'.',6X,A8))

!     System Bypass Options
      READ(IN,'(A)') COMMENT
      READ(IN,'(40I2)',ERR=950)   (SYSBY(I),I=1,NOSYS)
      IF(MSR) WRITE(OUT,4000)    NOSYS,(SYSBY(I),I=1,NOSYS)
 4000 FORMAT(15X,'SYSTEM BYPASS OPTIONS FOR SYSTEMS 1 TO',I3,' ARE',/15X,50I2)

!     DETERMINE SYSTEM NUMBER OF LAST ACTIVE SYSTEM
      DO ISYS=1,NOSYS
       SYSGDP(ISYS) = SYSBY(ISYS)
       IF(SYSBY(ISYS) == 0) MXACTS = ISYS
      END DO 

      IF(MSR)THEN
       WRITE(10)   NXY,EXY,NZ,NOSYS
       WRITE(10)   SYNAME
       WRITE(10)   SYSGDP
       WRITE(OUT,'(////1X,119("*")/1X,119("*"))')
      END IF 

!     OTHER INITIALIZATION
      TIME  = 0.0
      INITB = 0
      IREC  = 0

!JQI      CDARAY = 0.
!JQI      CARAY  = 0.

!JQI      BATM = 0.
!JQI      SATM = 0.

      BBC = 0.
      SBC = 0.

      BPS  = 0.
      SPS  = 0.
      BNPS = 0.
      SNPS = 0.
      BFL  = 0.
      SFL  = 0.

!JQI      DO IZ=1,NZ+1
!JQI       DO IXY=1,NXY
!JQI        IF(IZ <= NZ) THEN
!JQI         VDER(IXY,IZ) = 0.
!JQI         QX(IXY,IZ) = 0.
!JQI         QY(IXY,IZ) = 0.
!JQI         RX(IXY,IZ) = 0.
!JQI         RY(IXY,IZ) = 0.
!JQI        ENDIF
!JQI        QZ(IXY,IZ) = 0.
!JQI        RZ(IXY,IZ) = 0.
!JQI       END DO
!JQI      END DO 	

      RETURN

 950  CALL FMTER
!      CALL EXIT
      CALL PSTOP
      END SUBROUTINE RCA01
