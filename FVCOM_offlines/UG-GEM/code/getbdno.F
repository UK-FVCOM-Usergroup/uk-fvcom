   SUBROUTINE GETBDNO

!==============================================================================|
   USE ALL_VARS
   USE MOD_OBCS
   IMPLICIT NONE
   INTEGER :: J,IOS,LMAX
!==============================================================================|

   IF(MSR)WRITE(IPT,*)'!            GLOBAL INFORMATION                        !'
   IF(MSR)WRITE(IPT,*)'!                                                                !'
   IF(MSR)WRITE(IPT,*)'!  # OF PROCESSORS       :',NPROCS

!----------------Determine Number of Nodes on Outer Boundary-------------------!
!
   READ(INOBC,*)
   LMAX = 0
   DO WHILE(.TRUE.)
    READ(INOBC,*,IOSTAT=IOS)J
    IF(IOS < 0)EXIT
    LMAX = J
   END DO
   REWIND(INOBC)
   IOBCN_GL = LMAX
!
!----------------Determine Number of Groundwater Flux Points ------------------!
!
   READ(INBFW,*)
   READ(INBFW,*) IBFW_GL
   REWIND(INBFW)
!
!----------------Determine Number of Freshwater Flux Points--------------------!
!
   READ(INRIV,*)
   READ(INRIV,*) NUMQBC_GL
   REWIND(INRIV)
!
!----------------Report--------------------------------------------------------!
!
   IF(MSR)WRITE(IPT,*)'!  # OF OPEN BNDRY NODES :',IOBCN_GL
   IF(MSR)WRITE(IPT,*)'!  # OF BOTTOM FLUX PTS  :',IBFW_GL
   IF(MSR)WRITE(IPT,*)'!  # OF FRESH WATER PTS  :',NUMQBC_GL

   RETURN
   END SUBROUTINE GETBDNO
!==============================================================================|
