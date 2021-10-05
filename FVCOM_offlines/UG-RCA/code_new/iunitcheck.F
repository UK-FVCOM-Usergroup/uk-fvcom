      FUNCTION IUNITCHECK(TWARP,NAME)

      USE ALL_VARS,ONLY : MSR
      USE MOD_RCA,ONLY : IN,OUT
      IMPLICIT NONE
      SAVE
      CHARACTER(LEN=4) :: TWARP
      CHARACTER(LEN=8) :: NAME
      INTEGER  :: IUNITCHECK
!JQI      INTEGER  :: IN,OUT
!JQI      COMMON  IN,OUT
      IF(TWARP /= 'SECS' .AND. TWARP /= 'secs' .AND.                &
         TWARP /= 'MINS' .AND. TWARP /= 'mins' .AND.                &
         TWARP /= 'HRS ' .AND. TWARP /= 'hrs ' .AND.                &
         TWARP /= ' HRS' .AND. TWARP /= ' hrs' .AND.                &
         TWARP /= 'DAYS' .AND. TWARP /= 'days')  THEN
       IF(MSR) WRITE(OUT,9000)  NAME,TWARP
 9000  FORMAT(/5X,'THE UNITS CHOSEN FOR ',A8,1X,A4,' ARE NOT VALID'/    &
         5X,'RCA TERMINATED')
!JQI       CALL EXIT
       CALL PSTOP
      ENDIF
      IF(TWARP == 'SECS'.OR.TWARP == 'secs')  IUNITCHECK=1
      IF(TWARP == 'MINS'.OR.TWARP == 'mins')  IUNITCHECK=60
      IF(TWARP == 'HRS '.OR.TWARP == 'hrs ')  IUNITCHECK=3600
      IF(TWARP == ' HRS'.OR.TWARP == ' hrs')  IUNITCHECK=3600
      IF(TWARP == 'DAYS'.OR.TWARP == 'days')  IUNITCHECK=86400

      RETURN
      END FUNCTION IUNITCHECK
