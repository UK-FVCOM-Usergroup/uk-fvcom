   SUBROUTINE BRACKET(TMAP,STIME,L1,L2,FACT,BACT,IERR)             
!==============================================================================|
!  DETERMINE DATA INTERVAL IN WHICH CURRENT TIME LIES                          |
!									       | 
!  L1:  DATA INTERVAL PROCEEDING TIME				               |
!  L2:  DATA INTERVAL AFTER TIME                                               |
!  FACT: LINEAR INTERPOLATION COEFFICIENT (0->1)                               |
!     FACT = .5  : STIME LIES EXACTLY BETWEEN TWO DATA TIMES                   |
!     FACT = 1.  : STIME OCCURS AT SECOND DATA TIME                            |
!  BACT  = 1.-FACT
!  IERR: RETURNS INTEGER ERROR                                                 |
!     IERR = 0   : NO ERROR, TIME IS BRACKETED BY DATA TIMES                   |
!     IERR =-1   : STIME PROCEEDS ALL DATA TIMES                               |
!     IERR = 1   : STIME IS GREATER THAN ALL DATA TIMES                        |
!                                                                              |
!  IF STIME PROCEEDS DATA, IERR IS SET TO -1, L1 TO 1, AND FACT TO 0.          !
!  IF STIME SUPERCEEDS DATA, IERR IS SET TO -1, L2 TO LMAX, AND FACT TO 1.     !
!==============================================================================|
   USE MOD_TYPES 
   IMPLICIT NONE
!------------------------------------------------------------------------------!
   TYPE(BC), INTENT(IN)  :: TMAP
   REAL(SP), INTENT(IN)  :: STIME
   INTEGER,  INTENT(OUT) :: L1,L2
   REAL(SP), INTENT(OUT) :: FACT,BACT
   INTEGER,  INTENT(OUT) :: IERR
!------------------------------------------------------------------------------!
   REAL(SP)  T1,T2
   INTEGER I,NTMAX
!==============================================================================|

   NTMAX = TMAP%NTIMES
   IF(STIME < TMAP%TIMES(1))THEN
     FACT = 0.0_SP
     BACT = 1.0_SP
     L1   = 1
     L2   = 1
     IERR = -1
     RETURN
   END IF

   IF(STIME > TMAP%TIMES(NTMAX))THEN
     FACT = 1.0_SP
     BACT = 0.0_SP
     L1   = NTMAX
     L2   = NTMAX
     IERR = 1
     RETURN
   END IF

   IF(NTMAX == 1)THEN
     FACT = 1.0_SP
     BACT = 0.0_SP
     L1   = 1
     L2   = 1
     IERR = 0
     RETURN
   END IF
   

   DO I=2,TMAP%NTIMES
     T1 = TMAP%TIMES(I-1)
     T2 = TMAP%TIMES(I)
     IF(STIME >= T1 .AND. STIME <= T2)THEN  
       L1   = I-1
       L2   = I
       IERR = 0
       FACT = (STIME-T1)/(T2-T1)
       BACT = 1.0_SP-FACT
     END IF
   END DO
   
     
   RETURN
   END SUBROUTINE BRACKET
!==============================================================================|
