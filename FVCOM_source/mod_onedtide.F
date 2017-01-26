!/===========================================================================/
! Copyright (c) 2007, The University of Massachusetts Dartmouth 
! Produced at the School of Marine Science & Technology 
! Marine Ecosystem Dynamics Modeling group
! All rights reserved.
!
! FVCOM has been developed by the joint UMASSD-WHOI research team. For 
! details of authorship and attribution of credit please see the FVCOM
! technical manual or contact the MEDM group.
!
! 
! This file is part of FVCOM. For details, see http://fvcom.smast.umassd.edu 
! The full copyright notice is contained in the file COPYRIGHT located in the 
! root directory of the FVCOM code. This original header must be maintained
! in all distributed versions.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
! AND ANY EXPRESS OR  IMPLIED WARRANTIES, INCLUDING,  BUT NOT  LIMITED TO,
! THE IMPLIED WARRANTIES OF MERCHANTABILITY AND  FITNESS FOR A PARTICULAR
! PURPOSE ARE DISCLAIMED.  
!
!/---------------------------------------------------------------------------/
! CVS VERSION INFORMATION
! $Id$
! $Name$
! $Revision$
!/===========================================================================/

MODULE MOD_ONEDTIDE
#  if defined (ONE_D_MODEL)
   USE MOD_TYPES
   USE BCS
   
   IMPLICIT NONE
   SAVE
   
   INTEGER, PARAMETER :: NUMTIDES_MAX = 8
   INTEGER  :: NUMTIDES
   REAL(SP) :: UMAX(NUMTIDES_MAX),PMAX(NUMTIDES_MAX)

   NAMELIST /NML_ONEDTIDE/      &
        & NUMTIDES,             &
	& UMAX,                 &
	& PMAX
	   
   CONTAINS
!============================================================================!
!
!============================================================================!
   SUBROUTINE GET_UMAX
   USE ALL_VARS
   USE MOD_UTILS
   
   IMPLICIT NONE
   integer :: ios, i
   Character(Len=120):: FNAME
   if(DBG_SET(dbg_sbr)) &
        & write(IPT,*) "Subroutine Begins: get_umax;"

    ios = 0

    FNAME = "./"//trim(casename)//"_run.nml"

    if(DBG_SET(dbg_io)) &
         & write(IPT,*) "Get_umax: File: ",trim(FNAME)

    CALL FOPEN(NMLUNIT,trim(FNAME),'cfr')

    !READ NAME LIST FILE

    ! Read name list nml_onedtide
    READ(UNIT=NMLUNIT, NML=NML_ONEDTIDE,IOSTAT=ios)
    if(ios .NE. 0 ) then
       if(DBG_SET(dbg_log)) write(UNIT=IPT,NML=NML_ONEDTIDE)
       Call Fatal_Error("Can Not Read NameList NML_ONEDTIDE from file: "//trim(FNAME))
    end if

    REWIND(NMLUNIT)

    if(DBG_SET(dbg_scl)) &
         & write(IPT,*) "Read_Name_List:"

    if(DBG_SET(dbg_scl)) &
         & write(UNIT=IPT,NML=NML_ONEDTIDE)


    IF(NUMTIDES > nTideComps)THEN
      if(nTideComps==0)then
        nTideComps = NUMTIDES
      else
        CALL FATAL_ERROR("NUMBER OF TIDAL CONSTITUTES IS GREATER THAN nTideComps IN 1D MODULE")
      endif
    END IF
      
    CLOSE(NMLUNIT)

   RETURN
   END SUBROUTINE GET_UMAX
!===========================================================================!
!
!===========================================================================!        
   SUBROUTINE ONEDTIDE(PSTX_TM,PSTY_TM)
     USE ALL_VARS
     USE BCS
     IMPLICIT NONE
     TYPE(TIME):: TIME_ELAPSED,TIME_PREVIOUS
     INTEGER :: I,J,K
     REAL(SP) :: PSTX_TM(0:NT,KB),PSTY_TM(0:NT,KB),TIME1
     REAL(DP):: TIME_FLT,TIME_PRV
     REAL(SP),dimension(8) :: tide_period 

     DATA tide_period /44712, 43200, 45570, 43082, 86164, 86637, 92950, 96726/
     !**************     EXECUTABLE     ***************
     !   TIME1 = TIME * 86400.0_SP
     TIME_ELAPSED   = IntTime - SpecTime
     TIME_PREVIOUS  = IntTime - SpecTime - IMDTI
     PSTX_TM = 0.0_SP
     PSTY_TM = 0.0_SP
     !***************  INITIALIZATION   ***************
     IF (IINT == 1) THEN
        ALLOCATE(PERIOD(NTIDECOMPS))
        PERIOD(1:nTideComps)=tide_period(1:nTideComps)
        DO K=1,KBM1
           DO I=1,NT
              DO J=1,nTideComps    !6
                 TIME_FLT = SECONDS(TIME_ELAPSED * PI2/PERIOD(J))
                 PSTX_TM(I,K) = PSTX_TM(I,K)-UMAX(J)/DTI*  &
                      SIN(TIME_FLT-PMAX(J))
                 PSTY_TM(I,K) = PSTY_TM(I,K)-UMAX(J)/DTI*  &
                      COS(TIME_FLT-PMAX(J))
                 PSTX_TM(I,K) = PSTX_TM(I,K)*(D1(I)*DZ1(I,K))
                 PSTY_TM(I,K) = PSTY_TM(I,K)*(D1(I)*DZ1(I,K))
              END DO
           END DO
        END DO
     ELSE
        !***************  COMPUTATION   ***************
        DO K=1,KBM1
           DO I=1,NT
              DO J=1,nTideComps    !6
                 TIME_FLT = SECONDS(TIME_ELAPSED * PI2/PERIOD(J))
                 TIME_PRV = SECONDS(TIME_PREVIOUS * PI2/PERIOD(J))
                 PSTX_TM(I,K) = PSTX_TM(I,K)-UMAX(J)/DTI*  &
                      (SIN(TIME_FLT-PMAX(J))-SIN(TIME_PRV-PMAX(J)))
                 PSTY_TM(I,K) = PSTY_TM(I,K)-UMAX(J)/DTI*  &
                      (COS(TIME_FLT-PMAX(J))-COS(TIME_PRV-PMAX(J)))
                 PSTX_TM(I,K) = PSTX_TM(I,K)*(D1(I)*DZ1(I,K))
                 PSTY_TM(I,K) = PSTY_TM(I,K)*(D1(I)*DZ1(I,K))
              END DO
           END DO
        END DO
     END IF



     RETURN
   END SUBROUTINE ONEDTIDE
!===========================================================================!
!
!===========================================================================!   
   SUBROUTINE NAME_LIST_INITIALIZE_ONED
   USE CONTROL
   
   IMPLICIT NONE
   
   !--Parameters in NameList NML_ONEDTIDE
   NUMTIDES = 0
   UMAX     = 0.0
   PMAX     = 0.0

   RETURN
   END SUBROUTINE NAME_LIST_INITIALIZE_ONED
!===========================================================================!
!
!===========================================================================!   
   SUBROUTINE NAME_LIST_PRINT_ONED
   USE CONTROL
   
   IMPLICIT NONE
   
   write(UNIT=IPT,NML=NML_ONEDTIDE)
   
   RETURN
   END SUBROUTINE NAME_LIST_PRINT_ONED
!===========================================================================!   
      
#  endif
END MODULE MOD_ONEDTIDE   
   
    
