!==============================================================================|
!     ARCHIVE THE MODEL RESULTS                                                |
!==============================================================================|

   SUBROUTINE ARCHIVE

!==============================================================================|

   USE MOD_NCDOUT, ONLY : CDF_INT,CDF_OUT,OUT_NETCDF

   USE CONTROL, ONLY : IINT  

   IMPLICIT NONE
!
!--NETCDF OUTPUT---------------------------------------------------------------!
!
   IF(CDF_INT /= 0 .AND. CDF_OUT)THEN
     IF(MOD(IINT,CDF_INT)==0) CALL OUT_NETCDF
   END IF

   RETURN
   END SUBROUTINE ARCHIVE
!==============================================================================|
