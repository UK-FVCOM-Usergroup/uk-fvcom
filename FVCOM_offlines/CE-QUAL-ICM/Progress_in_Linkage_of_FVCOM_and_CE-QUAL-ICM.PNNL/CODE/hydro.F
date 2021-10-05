!************************************************************************
!**                  S U B R O U T I N E   H Y D R O                   **
!************************************************************************

    SUBROUTINE HYDRO ()
    USE WQM
    USE FILE_INFO
    IMPLICIT NONE
    SAVE
    INTEGER   F, SB, L, JC, I, J
    LOGICAL   END_OF_FILE
    REAL(4)   NXDAY, TDUM
    REAL(8)   MASS(0:MTLOC,KBM1,NCP)
    CHARACTER(LEN=100) :: NCFILE
!************************************************************************
!**                              Inputs                                **
!************************************************************************

    UNC1  = UNC2
    VNC1  = VNC2
    WNC1  = WNC2
    WTSNC1 = WTSNC2
    UARD_OBCNNC1=UARD_OBCNNC2
    XFLUX_OBCNC1=XFLUX_OBCNC2
    DTFANC1=DTFANC2
    KHNC1 = KHNC2
    ELNC1 = ELNC2
    TNC1  = TNC2
    SNC1  = SNC2
!--HOURLY READING OF VELOCITY FIELDS IN NETCDF FILE
    WRITE(FILENUMBER,'(I4.4)') DAY
    NCFILE = TRIM(PREFF)//TRIM(FILENUMBER)//'.nc'
!    IF(HOUR1 == 24)HOUR1 = 0
! RGL added print
    Print*, 'calling ncd_read in hydro.F'
    CALL NCD_READ(NCFILE,UNC2,VNC2,WTSNC2,UARD_OBCNNC2,XFLUX_OBCNC2,DTFANC2,KHNC2,ELNC2,TNC2,SNC2,TYHOUR1)
!    IF(HOUR1 == 0)HOUR1 = 24
!tykim
!    HOUR1=HOUR1+INT(AHMDLT/3600.)
    TYHOUR1=TYHOUR1+1    
    IF (MOD(TYHOUR1,(86400/INT(AHMDLT))) == 0)then
!    IF (HOUR1 > 24) THEN
      TYHOUR1=1
      DAY=DAY+1
    ENDIF
   
!tykim
     NXHYD = (DAY-1)*86400+TYHOUR1*INT(AHMDLT)-INT(AHMDLT)
!    NXHYD = (DAY-1)*86400+HOUR1*3600-AHMDLT

    RETURN
    END SUBROUTINE HYDRO

 
