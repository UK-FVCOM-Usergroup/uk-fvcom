!==============================================================================|
!   Begin Restart Run From Specified Time                                      |
!==============================================================================|

   SUBROUTINE STARTUP             
!------------------------------------------------------------------------------|

   USE ALL_VARS
   USE BCS
   USE MOD_BIO_3D

   IMPLICIT NONE
!==============================================================================|
!
!--Set Water Depth-Using Bathymetry and Free Surface Elevation-----------------!
!
   CALL WATER_DEPTH
!
!--Set up Temperature, Salinity, and Turbulence Quantity Fields----------------!
! 
   IF(RESTART == 'cold_start')THEN
    IF(MSR)WRITE(IPT,*)  '!  STARTUP TYPE          :    COLD_START'
    CALL BIO_INITIAL 
   ELSE IF(RESTART == 'hot_start') THEN
     IF(MSR)WRITE(IPT,*)  '!  STARTUP TYPE          :    HOT_START'
     CALL BIO_HOT_START
     IF(MSR)WRITE(IPT,*)  '!  RESTART DATA          :    READ     '
   ELSE
     PRINT*,'RESTAR AND S_TYPE DEFINITION NOT CORRECT'
     PRINT*,'RESTAR==',RESTART
     CALL PSTOP
   END IF

   RETURN
   END SUBROUTINE STARTUP
!==============================================================================|
