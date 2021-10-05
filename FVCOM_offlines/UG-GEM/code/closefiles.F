 
!==============================================================================!
!   CLOSE UP FILES                                                             !
!==============================================================================!

   SUBROUTINE CLOSEFILES
   USE ALL_VARS
   IMPLICIT NONE
   REAL(SP) ::  COMT(80)

   CLOSE(IOPRT)
   CLOSE(IOTSR)
   
   RETURN
   END SUBROUTINE CLOSEFILES
!==============================================================================!




