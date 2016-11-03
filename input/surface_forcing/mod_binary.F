MODULE MOD_binary
  USE MOD_BULK
  USE MOD_SURFACEFORCE
  USE MOD_SET_TIME
  implicit none
  
  TYPE bin_data
     type(TIME) :: dtm
     REAL(SP), POINTER :: data(:)
  END TYPE bin_data
  
  TYPE(bin_data), POINTER :: SWR_NEXT, SWR_PREV
  TYPE(bin_data), POINTER :: NHF_NEXT, NHF_PREV
  TYPE(bin_data), POINTER :: WNDX_NEXT, WNDX_PREV
  TYPE(bin_data), POINTER :: WNDY_NEXT, WNDY_PREV
  TYPE(bin_data), POINTER :: EVP_NEXT, EVP_PREV
  TYPE(bin_data), POINTER :: PRC_NEXT, PRC_PREV

  TYPE(bin_data), POINTER :: AIP_NEXT, AIP_PREV
  TYPE(bin_data), POINTER :: SAT_NEXT, SAT_PREV
  TYPE(bin_data), POINTER :: SPQ_NEXT, SPQ_PREV
  TYPE(bin_data), POINTER :: CLD_NEXT, CLD_PREV

  !  STORAGE FOR OUTPUT OF BINARY FILES
  REAL(SP), POINTER :: WNDYGL(:), WNDXGL(:)
  REAL(SP), POINTER :: SWRGL(:),  NHFGL(:)
  REAL(SP), POINTER :: PRCGL(:),  EVPGL(:)

  REAL(SP), POINTER :: AIPGL(:), SATGL(:), SPQGL(:), CLDGL(:)

  LOGICAL ::SAT_ON, SPQ_ON, CLD_ON
  
CONTAINS

  SUBROUTINE UPDATE_BINARY(NOW)
    IMPLICIT NONE
    TYPE(TIME) :: NOW
    
    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "START: UPDATE_BINARY"

    IF(PRECIPITATION_ON) THEN
       CALL UPDATE_EVP(NOW,EVP,PRC)
    END IF

    IF(WIND_ON) THEN

       IF(WIND_TYPE == 'stress') THEN
          CALL UPDATE_WND(NOW,STRX,STRY)
       ELSE IF(WIND_TYPE == 'speed') THEN
          CALL UPDATE_WND(NOW,SPDX,SPDY)
          CALL PSIMPLE_DRAG(SPDX,SPDY,STRX,STRY)
       END IF
       
!    WRITE(IPT,*) "MIN/MAX(SPDX)::",minval(spdx),maxval(spdx)
!    WRITE(IPT,*) "MIN/MAX(SPDy)::",minval(spdy),maxval(spdy)

!    WRITE(IPT,*) "MIN/MAX(STRX)::",minval(strx),maxval(strx)
!    WRITE(IPT,*) "MIN/MAX(STRy)::",minval(stry),maxval(stry)

    END IF

    IF(HEATING_ON) THEN
       CALL UPDATE_HFX(NOW,SWR,NHF)
    END IF

    IF(AIRPRESSURE_ON) THEN
       CALL UPDATE_AIP(NOW,AIP)
    END IF

    IF(ICE_MODEL) THEN
       CALL UPDATE_ICE(NOW,SAT,SPQ,CLD)
    END IF

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END: UPDATE_BINARY"
    
  END SUBROUTINE UPDATE_BINARY


  FUNCTION NEW_DATA(dims)
    IMPLICIT NONE
    TYPE(bin_data), POINTER :: NEW_DATA
    integer, intent(IN) :: DIMS
    INTEGER :: STATUS

    ALLOCATE(NEW_DATA,stat=status)
    IF(status /=0) CALL FATAL_ERROR("NEW_DATA: COULD NOT ALLOCATE TYPE POINTER?")
    
    ALLOCATE(NEW_DATA%DATA(0:DIMS), STAT=STATUS)
    IF(status /=0) CALL FATAL_ERROR("NEW_DATA: COULD NOT ALLOCATE DATA POINTER?")
    
  END FUNCTION NEW_DATA


  SUBROUTINE LOAD_BINARY(WND,HFX,EVP,AIPF,SATF,SPQF,CLDF)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: WND,HFX,EVP,AIPF,SATF,SPQF,CLDF
    INTEGER :: STATUS
    INTEGER(ITIME) :: dummy
    CHARACTER(LEN=4) :: FLAG

    
    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "START:LOAD_BINARY"
    
    IF(DBG_SET(DBG_LOG)) WRITE(IPT,*) "Looking for Wind Stress file:"&
         &//TRIM(WND)
    inquire(file=trim(WND),exist=WIND_ON)
    IF(WIND_ON) THEN
       
       IF(WIND_TYPE /= 'stress' .and. WIND_TYPE/='speed')THEN
          CALL FATAL_ERROR("To convert a binary wind file,",&
               &"you must specify 'binary speed' or 'binary stress'")
       END IF
       
       IF(DBG_SET(DBG_LOG)) WRITE(IPT,*) "FOUND WIND FILE: OPEN AND READ"
       
       IF(MSR) CALL FOPEN(WNDUNIT,WND,'cur')
       
       WNDX_NEXT => NEW_DATA(N)
       WNDX_PREV => NEW_DATA(N)
       WNDY_NEXT => NEW_DATA(N)
       WNDY_PREV => NEW_DATA(N)
       
       CALL READ_WND(WNDX=WNDX_PREV,WNDY=WNDY_PREV)
       CALL READ_WND(WNDX=WNDX_NEXT,WNDY=WNDY_NEXT)
       
       IF(DBG_SET(DBG_LOG)) THEN
          CALL PRINT_REAL_TIME(WNDX_PREV%dtm,IPT,"FIRST TIME POINT",timezone)
          CALL PRINT_REAL_TIME(WNDX_NEXT%dtm,IPT,"SECOND TIME POINT",timezone)
       END IF
       
       IF(DBG_SET(DBG_LOG)) WRITE(IPT,*) "FOUND WIND FILE: READ FIRST DATA POINTS"
       
    ELSE
       
       IF(DBG_SET(DBG_LOG)) WRITE(IPT,*) "! NO WIND FILE FOUND"
       
    END IF
    
    IF(DBG_SET(DBG_LOG)) WRITE(IPT,*) "Looking for HeatFlux file:"&
         &//TRIM(HFX)
    inquire(file=trim(HFX),exist=HEATING_ON)
    IF(HEATING_ON) THEN
       
       IF(DBG_SET(DBG_LOG)) WRITE(IPT,*) "FOUND HEATING FILE: OPEN AND READ"
       
       IF(MSR) CALL FOPEN(HFXUNIT,HFX,'cur')
       
       SWR_NEXT => NEW_DATA(M)
       SWR_PREV => NEW_DATA(M)
       NHF_NEXT => NEW_DATA(M)
       NHF_PREV => NEW_DATA(M)

       CALL READ_HFX(SWR=SWR_PREV,NHF=NHF_PREV)
       CALL READ_HFX(SWR=SWR_NEXT,NHF=NHF_NEXT)

       IF(DBG_SET(DBG_LOG)) THEN
          CALL PRINT_REAL_TIME(SWR_PREV%dtm,IPT,"FIRST TIME POINT",timezone)
          CALL PRINT_REAL_TIME(SWR_NEXT%dtm,IPT,"SECOND TIME POINT",timezone)
       END IF
       
       IF(DBG_SET(DBG_LOG)) WRITE(IPT,*) "FOUND HEATING FILE: READ FIRST DATA POINTS"
    ELSE
       
       IF(DBG_SET(DBG_LOG)) WRITE(IPT,*) "! NO HEATING FILE FOUND"
       
       
    END IF
    
    IF(DBG_SET(DBG_LOG)) WRITE(IPT,*) "Looking for Evaporation file:"&
         &//TRIM(EVP)
    inquire(file=trim(EVP),exist=PRECIPITATION_ON)
    IF(PRECIPITATION_ON) THEN
 
       IF(DBG_SET(DBG_LOG)) WRITE(IPT,*) "FOUND PRECIPITATION FILE: OPEN AND READ"
       
       IF(MSR) CALL FOPEN(EVPUNIT,EVP,'cur')

       EVP_NEXT => NEW_DATA(M)
       EVP_PREV => NEW_DATA(M)
       PRC_NEXT => NEW_DATA(M)
       PRC_PREV => NEW_DATA(M)

       CALL READ_EVP(EVP=EVP_PREV,PRC=PRC_PREV)
       CALL READ_EVP(EVP=EVP_NEXT,PRC=PRC_NEXT)
       
       IF(DBG_SET(DBG_LOG)) THEN
          CALL PRINT_REAL_TIME(SWR_PREV%dtm,IPT,"FIRST TIME POINT",timezone)
          CALL PRINT_REAL_TIME(SWR_NEXT%dtm,IPT,"SECOND TIME POINT",timezone)
       END IF
       
       IF(DBG_SET(DBG_LOG)) WRITE(IPT,*) "FOUND PRECIPITATION FILE: READ FIRST DATA POINTS"
       
    ELSE
       
       IF(DBG_SET(DBG_LOG)) WRITE(IPT,*) "! NO PRECIPITATION FILE FOUND"
       
    END IF


# if defined (AIR_PRESSURE)
    IF(DBG_SET(DBG_LOG)) WRITE(IPT,*) "Looking for Air Pressure file:"&
         &//TRIM(AIPF)
    inquire(file=trim(AIPF),exist=AIRPRESSURE_ON)
    IF(AIRPRESSURE_ON) THEN
 
       IF(DBG_SET(DBG_LOG)) WRITE(IPT,*) "FOUND AIR PRESSURE FILE: OPEN AND READ"
       
       IF(MSR) CALL FOPEN(AIPUNIT,AIPF,'cur')

       AIP_NEXT => NEW_DATA(M)
       AIP_PREV => NEW_DATA(M)

       CALL READ_AIP(AIP=AIP_PREV)
       CALL READ_AIP(AIP=AIP_NEXT)
       
       IF(DBG_SET(DBG_LOG)) THEN
          CALL PRINT_REAL_TIME(AIP_PREV%dtm,IPT,"FIRST TIME POINT",timezone)
          CALL PRINT_REAL_TIME(AIP_NEXT%dtm,IPT,"SECOND TIME POINT",timezone)
       END IF
       
       IF(DBG_SET(DBG_LOG)) WRITE(IPT,*) "FOUND AIR PRESSURE FILE: READ FIRST DATA POINTS"
       
    ELSE
       
       IF(DBG_SET(DBG_LOG)) WRITE(IPT,*) "! NO AIR PRESSURE FILE FOUND"
       
    END IF
# endif


# if defined (ICE)
    IF(DBG_SET(DBG_LOG)) WRITE(IPT,*) "Looking for Air Temperature file:"&
         &//TRIM(SATF)
    inquire(file=trim(SATF),exist=SAT_ON)
    IF(DBG_SET(DBG_LOG)) WRITE(IPT,*) "Looking for Specific Humidity file:"&
         &//TRIM(SPQF)
    inquire(file=trim(SPQF),exist=SPQ_ON)
    IF(DBG_SET(DBG_LOG)) WRITE(IPT,*) "Looking for Cloud Cover file:"&
         &//TRIM(CLDF)
    inquire(file=trim(CLDF),exist=CLD_ON)
    
    IF(SAT_ON .AND. SPQ_ON .AND. CLD_ON) ICE_MODEL = .TRUE.
    
    IF(ICE_MODEL) THEN
 
       IF(DBG_SET(DBG_LOG)) WRITE(IPT,*) "FOUND ICE MODEL FILES: OPEN AND READ"
       
       IF(MSR) CALL FOPEN(SATUNIT,SATF,'cur')
       IF(MSR) CALL FOPEN(SPQUNIT,SPQF,'cur')
       IF(MSR) CALL FOPEN(CLDUNIT,CLDF,'cur')

       SAT_NEXT => NEW_DATA(M)
       SAT_PREV => NEW_DATA(M)
       SPQ_NEXT => NEW_DATA(M)
       SPQ_PREV => NEW_DATA(M)
       CLD_NEXT => NEW_DATA(M)
       CLD_PREV => NEW_DATA(M)

       CALL READ_ICE(SAT=SAT_PREV,SPQ=SPQ_PREV,CLD=CLD_PREV)
       CALL READ_ICE(SAT=SAT_NEXT,SPQ=SPQ_NEXT,CLD=CLD_NEXT)
       
       IF(DBG_SET(DBG_LOG)) THEN
          CALL PRINT_REAL_TIME(SAT_PREV%dtm,IPT,"FIRST TIME POINT",timezone)
          CALL PRINT_REAL_TIME(SAT_NEXT%dtm,IPT,"SECOND TIME POINT",timezone)
       END IF
       
       IF(DBG_SET(DBG_LOG)) WRITE(IPT,*) "FOUND ICE MODEL FILES: READ FIRST DATA POINTS"
       
    ELSE
       
       IF(DBG_SET(DBG_LOG)) WRITE(IPT,*) "! NO ICE MODEL FILES FOUND OR SOME OF FILES MISSING"
       
    END IF

# endif

    IF (.not. PRECIPITATION_ON .and. .not. HEATING_ON .and. &
      & .not. WIND_ON .and. .not. AIRPRESSURE_ON .and. .not. ICE_MODEL) &
      &  CALL FATAL_ERROR("FOUND NO BINARY FORCING INPUT FILES?")
    
    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END:LOAD_BINARY"
    
  END SUBROUTINE LOAD_BINARY
!------------------------------------------------------------------
  SUBROUTINE IOERROR(IOS,MSG)
    IMPLICIT NONE
    INTEGER IOS
    CHARACTER(LEN=*) MSG
    CHARACTER(LEN=4) IOSC
    
    IF(IOS ==0) RETURN

    WRITE(IOSC,'(I4)') IOS
    
    CALL FATAL_ERROR("ERROR DURING FILE IO:"//TRIM(IOSC),&
         TRIM(MSG))
  
  END SUBROUTINE IOERROR

!-------------------------------------------------------------------
  SUBROUTINE READ_WND(WNDX,WNDY)
    IMPLICIT NONE
    TYPE(bin_data) :: WNDX,WNDY
    REAL(SP) :: hour
    integer :: i, SOURCE, ios
    Real(SP), POINTER :: WNDYGL(:),WNDXGL(:)
   
    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "START: READ_WND"


    IF(MSR) THEN
 
       IF(PAR) THEN
          ALLOCATE(WNDYGL(NGL))
          ALLOCATE(WNDXGL(NGL))
       ELSE
          WNDXGL => WNDX%DATA(1:NGL)
          WNDYGL => WNDY%DATA(1:NGL)
       END IF


       READ(UNIT=wndunit,IOSTAT=ios) hour
       CALL IOERROR(IOS,"Can't read hour from wind file")
       READ(UNIT= wndunit,IOSTAT=ios)(WNDXGL(i),WNDYGL(i),i=1,ngl)
       CALL IOERROR(IOS,"Can't read data from wind file")


       IF(DBG_SET(DBG_IO)) THEN
          WRITE(IPT,*) "MIN/MAX/MEAN(WNDX)::",MINVAL(WNDXgl),maxval(WNDXgl),sum(WNDXgl)/real(Ngl)
          WRITE(IPT,*) "MIN/MAX/MEAN(WNDY)::",MINVAL(WNDYgl),maxval(WNDYgl),sum(WNDYgl)/real(Ngl)
       END IF

    END IF


    ! IF NOT PAR, VARIABLES ARE ALREADY POINTED CORRECTLY
    IF (PAR)THEN
# if defined(MULTIPROCESSOR)
       
       SOURCE = MSRID -1

       CALL MPI_BCAST(hour,1,MPI_F,SOURCE,MPI_FVCOM_GROUP)
       CALL PDEAL(MYID,MSRID,NPROCS,EMAP,WNDXGL,WNDX%DATA)
       CALL PDEAL(MYID,MSRID,NPROCS,EMAP,WNDYGL,WNDY%DATA)

       IF(MSR) THEN
          DEALLOCATE(WNDXGL)
          DEALLOCATE(WNDYGL)
       END IF
# endif
    END IF

    NULLIFY(WNDXGL,WNDYGL)

    WNDX%dtm = seconds2time(hour*3600.0_SP) + ZEROTIME

    WNDY%dtm = WNDX%dtm

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END: READ_WND"
  END SUBROUTINE READ_WND

!-------------------------------------------------------------------
  SUBROUTINE READ_HFX(SWR,NHF)
    IMPLICIT NONE
    TYPE(bin_data) :: SWR,NHF
    REAL(SP) :: hour
    integer :: i, SOURCE, ios
    Real(SP), POINTER :: NHFGL(:),SWRGL(:)
    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "START: READ_HFX"
    

    IF(MSR) THEN
       IF(PAR) THEN
          ALLOCATE(NHFGL(MGL))
          ALLOCATE(SWRGL(MGL))
       ELSE
          SWRGL => SWR%DATA(1:MGL)
          NHFGL => NHF%DATA(1:MGL)
       END IF



       READ(UNIT=hfxunit,IOSTAT=ios) hour
       CALL IOERROR(IOS,"Can't read hour from heatflux file")
       READ(UNIT=hfxunit,IOSTAT=ios)(NHFGL(i),SWRGL(i),i=1,mgl)
       CALL IOERROR(IOS,"Can't read data from heatflux file")

       IF(DBG_SET(DBG_IO)) THEN
          WRITE(IPT,*) "MIN/MAX/MEAN(SWR)::",MINVAL(swrgl),maxval(swrgl),sum(swrgl)/real(Mgl)
          WRITE(IPT,*) "MIN/MAX/MEAN(NHF)::",MINVAL(NHFgl),maxval(NHFgl),sum(NHFgl)/real(Mgl)
       END IF


    END IF


    ! IF NOT PAR, VARIABLES ARE ALREADY POINTED CORRECTLY
    IF (PAR)THEN
# if defined(MULTIPROCESSOR)
       
       SOURCE = MSRID -1

       CALL MPI_BCAST(hour,1,MPI_F,SOURCE,MPI_FVCOM_GROUP)
       CALL PDEAL(MYID,MSRID,NPROCS,NMAP,SWRGL,SWR%DATA)
       CALL PDEAL(MYID,MSRID,NPROCS,NMAP,NHFGL,NHF%DATA)

       IF(MSR) THEN
          DEALLOCATE(SWRGL)
          DEALLOCATE(NHFGL)
       END IF
# endif
    END IF

    NULLIFY(SWRGL,NHFGL)

    SWR%dtm = seconds2time(hour*3600.0_SP) + ZEROTIME

    NHF%dtm = SWR%dtm
    
    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END: READ_HFX"

  END SUBROUTINE READ_HFX

!-------------------------------------------------------------------
  SUBROUTINE READ_EVP(EVP,PRC)
    IMPLICIT NONE
    TYPE(bin_data) :: EVP,PRC
    REAL(SP) :: hour
    integer :: i, SOURCE,ios
    Real(SP), POINTER :: EVPGL(:),PRCGL(:)
  
    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "START: READ_EVP"

    IF(MSR) THEN

         IF(PAR) THEN
          ALLOCATE(EVPGL(MGL))
          ALLOCATE(PRCGL(MGL))
       ELSE
          EVPGL => EVP%DATA(1:MGL)
          PRCGL => PRC%DATA(1:MGL)
       END IF

       READ(UNIT=evpunit,IOSTAT=ios) hour
       CALL IOERROR(IOS,"Can't read hour from evap/prec file")
       READ(UNIT=evpunit,IOSTAT=ios)(evpgl(i),prcgl(i),i=1,mgl)
       CALL IOERROR(IOS,"Can't read data from evap/prec file")

       IF(DBG_SET(DBG_IO)) THEN
          WRITE(IPT,*) "MIN/MAX/MEAN(EVAP)::",MINVAL(evpgl),maxval(evpgl),sum(evpgl)/real(Mgl)
          WRITE(IPT,*) "MIN/MAX/MEAN(PREC)::",MINVAL(prcgl),maxval(prcgl),sum(prcgl)/real(Mgl)
       END IF

    END IF


    ! IF NOT PAR, VARIABLES ARE ALREADY POINTED CORRECTLY
    IF (PAR)THEN
# if defined(MULTIPROCESSOR)
       
       SOURCE = MSRID -1
       CALL MPI_BCAST(hour,1,MPI_F,SOURCE,MPI_FVCOM_GROUP)
       CALL PDEAL(MYID,MSRID,NPROCS,NMAP,prcgl,PRC%DATA)
       CALL PDEAL(MYID,MSRID,NPROCS,NMAP,evpgl,EVP%DATA)
       
       IF(MSR) THEN
          DEALLOCATE(PRCGL)
          DEALLOCATE(EVPGL)
       END IF

# endif
    END IF

    NULLIFY(EVPGL,PRCGL)
    
    PRC%dtm = seconds2time(hour*3600.0_SP) + ZEROTIME

    EVP%dtm = PRC%dtm

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END: READ_EVP"

  END SUBROUTINE READ_EVP

  SUBROUTINE READ_AIP(AIP)
    IMPLICIT NONE
    TYPE(bin_data) :: AIP
    REAL(SP) :: hour
    integer :: i, SOURCE, ios
    Real(SP), POINTER :: AIPGL(:)
    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "START: READ_AIP"
    

    IF(MSR) THEN
       IF(PAR) THEN
          ALLOCATE(AIPGL(MGL))
       ELSE
          AIPGL => AIP%DATA(1:MGL)
       END IF

       READ(UNIT=aipunit,IOSTAT=ios) hour
       CALL IOERROR(IOS,"Can't read hour from air pressure file")
       READ(UNIT=aipunit,IOSTAT=ios)(AIPGL(i),i=1,mgl)
       CALL IOERROR(IOS,"Can't read data from air pressure file")

       IF(DBG_SET(DBG_IO)) THEN
          WRITE(IPT,*) "MIN/MAX/MEAN(AIP)::",MINVAL(aipgl),maxval(aipgl),sum(aipgl)/real(Mgl)
       END IF
    END IF

    ! IF NOT PAR, VARIABLES ARE ALREADY POINTED CORRECTLY
    IF (PAR)THEN
# if defined(MULTIPROCESSOR)
       
       SOURCE = MSRID -1

       CALL MPI_BCAST(hour,1,MPI_F,SOURCE,MPI_FVCOM_GROUP)
       CALL PDEAL(MYID,MSRID,NPROCS,NMAP,AIPGL,AIP%DATA)

       IF(MSR) THEN
          DEALLOCATE(AIPGL)
       END IF
# endif
    END IF

    NULLIFY(AIPGL)

    AIP%dtm = seconds2time(hour*3600.0_SP) + ZEROTIME

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END: READ_AIP"

  END SUBROUTINE READ_AIP

  SUBROUTINE READ_ICE(SAT,SPQ,CLD)
    IMPLICIT NONE
    TYPE(bin_data) :: SAT,SPQ,CLD
    REAL(SP) :: hour
    integer :: i, SOURCE, ios
    Real(SP), POINTER :: SATGL(:),SPQGL(:),CLDGL(:)
    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "START: READ_ICE"
    

    IF(MSR) THEN
       IF(PAR) THEN
          ALLOCATE(SATGL(MGL))
          ALLOCATE(SPQGL(MGL))
          ALLOCATE(CLDGL(MGL))
       ELSE
          SATGL => SAT%DATA(1:MGL)
          SPQGL => SPQ%DATA(1:MGL)
          CLDGL => CLD%DATA(1:MGL)
       END IF

       READ(UNIT=satunit,IOSTAT=ios) hour
       CALL IOERROR(IOS,"Can't read hour from air temperature file")
       READ(UNIT=satunit,IOSTAT=ios)(SATGL(i),i=1,mgl)
       CALL IOERROR(IOS,"Can't read data from air temperature file")
 
       READ(UNIT=spqunit,IOSTAT=ios) hour
       CALL IOERROR(IOS,"Can't read hour from specific humidity file")
       READ(UNIT=spqunit,IOSTAT=ios)(SPQGL(i),i=1,mgl)
       CALL IOERROR(IOS,"Can't read data from specific humidity file")

       READ(UNIT=cldunit,IOSTAT=ios) hour
       CALL IOERROR(IOS,"Can't read hour from cloud cover file")
       READ(UNIT=cldunit,IOSTAT=ios)(CLDGL(i),i=1,mgl)
       CALL IOERROR(IOS,"Can't read data from cloud cover file")

       IF(DBG_SET(DBG_IO)) THEN
          WRITE(IPT,*) "MIN/MAX/MEAN(SAT)::",MINVAL(satgl),maxval(satgl),sum(satgl)/real(Mgl)
          WRITE(IPT,*) "MIN/MAX/MEAN(SPQ)::",MINVAL(spqgl),maxval(spqgl),sum(spqgl)/real(Mgl)
          WRITE(IPT,*) "MIN/MAX/MEAN(CLD)::",MINVAL(cldgl),maxval(cldgl),sum(cldgl)/real(Mgl)
       END IF


    END IF


    ! IF NOT PAR, VARIABLES ARE ALREADY POINTED CORRECTLY
    IF (PAR)THEN
# if defined(MULTIPROCESSOR)
       
       SOURCE = MSRID -1

       CALL MPI_BCAST(hour,1,MPI_F,SOURCE,MPI_FVCOM_GROUP)
       CALL PDEAL(MYID,MSRID,NPROCS,NMAP,SATGL,SAT%DATA)
       CALL PDEAL(MYID,MSRID,NPROCS,NMAP,SPQGL,SPQ%DATA)
       CALL PDEAL(MYID,MSRID,NPROCS,NMAP,CLDGL,CLD%DATA)

       IF(MSR) THEN
          DEALLOCATE(SATGL)
          DEALLOCATE(SPQGL)
          DEALLOCATE(CLDGL)
       END IF
# endif
    END IF

    NULLIFY(SATGL,SPQGL,CLDGL)

    SAT%dtm = seconds2time(hour*3600.0_SP) + ZEROTIME

    SPQ%dtm = SAT%dtm
    CLD%dtm = SAT%dtm
    
    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END: READ_ICE"

  END SUBROUTINE READ_ICE

  SUBROUTINE UPDATE_WND(NOW,WNDX,WNDY)
    IMPLICIT NONE
    TYPE(TIME) :: NOW
    REAL(SP), POINTER :: WNDX(:), WNDY(:)
    TYPE(BIN_DATA), POINTER :: A, B
    REAL(DP)     :: denom, numer
    REAL(SP)     :: fw, bw

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "START:UPDATE_WND "

    DO       
       IF(NOW .LT. WNDX_PREV%dtm) THEN

          CALL PRINT_REAL_TIME(NOW,IPT,"OUTPUT TIME",timezone)
          CALL PRINT_REAL_TIME(WNDX_PREV%dtm,IPT,"DATA TIME",timezone)

          CALL FATAL_ERROR("CAN NOT REWIND BINARY FILES",&
               & "SOMETHING IS WRONG WITH TIME IN THE WIND FILE")
          
       ELSE IF(NOW .gt. WNDX_NEXT%dtm)THEN
          
          A=> WNDX_PREV
          WNDX_PREV => WNDX_NEXT
          
          B => WNDY_PREV
          WNDY_PREV => WNDY_NEXT
          
          CALL READ_WND(WNDX=A,WNDY=B)

          WNDX_NEXT => A
          WNDY_NEXT => B
          
       ELSE
          
          EXIT
          
       END IF
       
    END DO

    NUMER = SECONDS(NOW - WNDX_PREV%dtm)

    DENOM = SECONDS(WNDY_NEXT%dtm - WNDX_PREV%dtm)
    
    fw = NUMER/DENOM
    bw = 1.0_DP - NUMER/DENOM

    WNDX= WNDX_NEXT%data *fw + WNDX_PREV%data *bw 

    WNDY= WNDY_NEXT%data *fw + WNDY_PREV%data *bw 

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END:UPDATE_WND "

  END SUBROUTINE UPDATE_WND

  SUBROUTINE UPDATE_HFX(NOW,SWR,NHF)
    IMPLICIT NONE
    TYPE(TIME) :: NOW
    REAL(SP), POINTER :: SWR(:), NHF(:)
    TYPE(BIN_DATA), POINTER :: A, B
    REAL(DP)     :: denom, numer
    REAL(SP)     :: fw, bw

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "START:UPDATE_HFX "

    DO       
       IF(NOW .LT. SWR_PREV%dtm) THEN

          CALL PRINT_REAL_TIME(NOW,IPT,"OUTPUT TIME",timezone)
          CALL PRINT_REAL_TIME(SWR_PREV%dtm,IPT,"DATA TIME",timezone)

          CALL FATAL_ERROR("CAN NOT REWIND BINARY FILES",&
               & "SOMETHING IS WRONG WITH TIME IN THE HEATING FILE")
          
       ELSE IF(NOW .gt. SWR_NEXT%dtm)THEN
          
          A=> SWR_PREV
          SWR_PREV => SWR_NEXT
          
          B => NHF_PREV
          NHF_PREV => NHF_NEXT
          
          CALL READ_HFX(SWR=A,NHF=B)
          SWR_NEXT => A
          NHF_NEXT => B
          
       ELSE
          
          EXIT
          
       END IF
       
    END DO
       
    NUMER = SECONDS(NOW - SWR_PREV%dtm)

    DENOM = SECONDS(SWR_NEXT%dtm - SWR_PREV%dtm)
    
    fw = NUMER/DENOM
    bw = 1.0_DP - NUMER/DENOM

    SWR= SWR_NEXT%data *fw + SWR_PREV%data *bw 

    NHF= NHF_NEXT%data *fw + NHF_PREV%data *bw 


    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END:UPDATE_HFX "

  END SUBROUTINE UPDATE_HFX

  SUBROUTINE UPDATE_EVP(NOW,EVP,PRC)
    IMPLICIT NONE
    TYPE(TIME) :: NOW
    REAL(SP), POINTER :: EVP(:), PRC(:)
    TYPE(BIN_DATA), POINTER :: A, B
    REAL(DP)     :: denom, numer
    REAL(SP)     :: fw, bw
    
    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "START:UPDATE_EVP "

    DO       
       IF(NOW .LT. EVP_PREV%dtm) THEN

          CALL PRINT_REAL_TIME(NOW,IPT,"OUTPUT TIME",timezone)
          CALL PRINT_REAL_TIME(EVP_PREV%dtm,IPT,"DATA TIME",timezone)


          CALL FATAL_ERROR("CAN NOT REWIND BINARY FILES",&
               & "SOMETHING IS WRONG WITH TIME IN THE EVAP/PREC FILE")
          
       ELSE IF(NOW .gt. EVP_NEXT%dtm)THEN
          
          A=> EVP_PREV
          EVP_PREV => EVP_NEXT
          
          B => PRC_PREV
          PRC_PREV => PRC_NEXT
          
          CALL READ_EVP(EVP=A,PRC=B)
          EVP_NEXT => A
          PRC_NEXT => B
          
       ELSE
          
          EXIT
          
       END IF
       
    END DO
       
    NUMER = SECONDS(NOW - EVP_PREV%dtm)

    DENOM = SECONDS(PRC_NEXT%dtm - EVP_PREV%dtm)
    
    fw = NUMER/DENOM
    bw = 1.0_DP - NUMER/DENOM

    EVP= EVP_NEXT%data *fw + EVP_PREV%data *bw 

    PRC= PRC_NEXT%data *fw + PRC_PREV%data *bw 

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END:UPDATE_EVP "

  END SUBROUTINE UPDATE_EVP


  SUBROUTINE UPDATE_AIP(NOW,AIP)
    IMPLICIT NONE
    TYPE(TIME) :: NOW
    REAL(SP), POINTER :: AIP(:)
    TYPE(BIN_DATA), POINTER :: A
    REAL(DP)     :: denom, numer
    REAL(SP)     :: fw, bw
    
    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "START:UPDATE_AIP "

    DO       
       IF(NOW .LT. AIP_PREV%dtm) THEN

          CALL PRINT_REAL_TIME(NOW,IPT,"OUTPUT TIME",timezone)
          CALL PRINT_REAL_TIME(AIP_PREV%dtm,IPT,"DATA TIME",timezone)


          CALL FATAL_ERROR("CAN NOT REWIND BINARY FILES",&
               & "SOMETHING IS WRONG WITH TIME IN THE AIR PRESSURE FILE")
          
       ELSE IF(NOW .gt. AIP_NEXT%dtm)THEN
          
          A=> AIP_PREV
          AIP_PREV => AIP_NEXT
          
          CALL READ_AIP(AIP=A)
          AIP_NEXT => A
          
       ELSE
          
          EXIT
          
       END IF
       
    END DO
       
    NUMER = SECONDS(NOW - AIP_PREV%dtm)

    DENOM = SECONDS(AIP_NEXT%dtm - AIP_PREV%dtm)
    
    fw = NUMER/DENOM
    bw = 1.0_DP - NUMER/DENOM

    AIP= AIP_NEXT%data *fw + AIP_PREV%data *bw 

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END:UPDATE_AIP "

  END SUBROUTINE UPDATE_AIP


  SUBROUTINE UPDATE_ICE(NOW,SAT,SPQ,CLD)
    IMPLICIT NONE
    TYPE(TIME) :: NOW
    REAL(SP), POINTER :: SAT(:),SPQ(:),CLD(:)
    TYPE(BIN_DATA), POINTER :: A,B,C
    REAL(DP)     :: denom, numer
    REAL(SP)     :: fw, bw
    
    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "START:UPDATE_ICE "

    DO       
       IF(NOW .LT. SAT_PREV%dtm) THEN

          CALL PRINT_REAL_TIME(NOW,IPT,"OUTPUT TIME",timezone)
          CALL PRINT_REAL_TIME(SAT_PREV%dtm,IPT,"DATA TIME",timezone)


          CALL FATAL_ERROR("CAN NOT REWIND BINARY FILES",&
               & "SOMETHING IS WRONG WITH TIME IN THE ICE MODEL FILES")
          
       ELSE IF(NOW .gt. SAT_NEXT%dtm)THEN
          
          A=> SAT_PREV
          SAT_PREV => SAT_NEXT
          B=> SPQ_PREV
          SPQ_PREV => SPQ_NEXT
          C=> CLD_PREV
          CLD_PREV => CLD_NEXT
          
          CALL READ_ICE(SAT=A,SPQ=B,CLD=C)
          SAT_NEXT => A
          SPQ_NEXT => B
          CLD_NEXT => C
          
       ELSE
          
          EXIT
          
       END IF
       
    END DO
       
    NUMER = SECONDS(NOW - SAT_PREV%dtm)

    DENOM = SECONDS(SAT_NEXT%dtm - SAT_PREV%dtm)
    
    fw = NUMER/DENOM
    bw = 1.0_DP - NUMER/DENOM

    SAT= SAT_NEXT%data *fw + SAT_PREV%data *bw 
    SPQ= SPQ_NEXT%data *fw + SPQ_PREV%data *bw 
    CLD= CLD_NEXT%data *fw + CLD_PREV%data *bw 

    IF(DBG_SET(DBG_SBR)) WRITE(IPT,*) "END:UPDATE_ICE "

  END SUBROUTINE UPDATE_ICE






END MODULE MOD_BINARY
