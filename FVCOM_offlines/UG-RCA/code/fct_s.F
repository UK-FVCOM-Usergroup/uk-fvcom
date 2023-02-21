!==============================================================================|
!  FLUX CONTROL FOR SALINITY                                                        |
!==============================================================================|

   SUBROUTINE FCT_S(SF1,S1,ISSOURCE)

!==============================================================================|
   USE ALL_VARS
   IMPLICIT NONE
   REAL :: SMAX,SMIN
   INTEGER :: I,J,K
   REAL :: S1(0:MT,KBM1),SF1(0:MT,KB)
   INTEGER :: ISSOURCE(MT)
!==============================================================================|

   DO I=1,M
    if(issource(i) == 1) go to 200     
     DO K=1,KBM1
       SMAX = MAXVAL(S1(NBSN(I,1:NTSN(I)),K))
       SMIN = MINVAL(S1(NBSN(I,1:NTSN(I)),K))

       IF(K == 1)THEN
         SMAX = MAX(SMAX,(S1(I,K)*DZ(I,K+1)+S1(I,K+1)*DZ(I,K))/  &
                (DZ(I,K)+DZ(I,K+1)))
         SMIN = MIN(SMIN,(S1(I,K)*DZ(I,K+1)+S1(I,K+1)*DZ(I,K))/  &
                (DZ(I,K)+DZ(I,K+1)))
       ELSE IF(K == KBM1)THEN
         SMAX = MAX(SMAX,(S1(I,K)*DZ(I,K-1)+S1(I,K-1)*DZ(I,K))/  &
                (DZ(I,K)+DZ(I,K-1)))
         SMIN = MIN(SMIN,(S1(I,K)*DZ(I,K-1)+S1(I,K-1)*DZ(I,K))/  &
                (DZ(I,K)+DZ(I,K-1)))
       ELSE
         SMAX = MAX(SMAX,(S1(I,K)*DZ(I,K-1)+S1(I,K-1)*DZ(I,K))/  &
                (DZ(I,K)+DZ(I,K-1)),                             &
                (S1(I,K)*DZ(I,K+1)+S1(I,K+1)*DZ(I,K))/           &
                (DZ(I,K)+DZ(I,K+1)))
         SMIN = MIN(SMIN,(S1(I,K)*DZ(I,K-1)+S1(I,K-1)*DZ(I,K))/  &
                (DZ(I,K)+DZ(I,K-1)),                             &
                (S1(I,K)*DZ(I,K+1)+S1(I,K+1)*DZ(I,K))/           &
                (DZ(I,K)+DZ(I,K+1)))
       END IF

       IF(SMIN-SF1(I,K) > 0.0)SF1(I,K) = SMIN
       IF(SF1(I,K)-SMAX > 0.0)SF1(I,K) = SMAX

     END DO
200 CONTINUE
   END DO

100 CONTINUE
   RETURN
   END SUBROUTINE FCT_S
!==============================================================================|


