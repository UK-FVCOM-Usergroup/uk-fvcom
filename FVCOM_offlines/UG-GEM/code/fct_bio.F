!==============================================================================|
!  FLUX CONTROL FOR SALINITY                                                        |
!==============================================================================|

   SUBROUTINE FCT_BIO

!==============================================================================|
   USE ALL_VARS
   USE BCS
   USE MOD_OBCS
   USE MOD_BIO_3D

   IMPLICIT NONE
   REAL(SP):: SMAX,SMIN
   INTEGER :: I,J,K,N1
!==============================================================================|

   DO N1=1,NTT
!JQI   IF(H_TYPE == 'body_h') GO TO 100
   DO I=1,M
     IF(IOBCN > 0)THEN
       DO J=1,IOBCN
         IF(I == I_OBC_N(J))GO TO 200
       END DO
     END IF 
     IF(IBFW > 0)THEN
      DO J=1,IBFW
       IF(I == NODE_BFW(J))GO TO 200
      END DO 
     END IF 	 
     IF(NUMQBC > 0)THEN
       DO J=1,NUMQBC
         IF(INFLOW_TYPE == 'node')THEN
	   IF(I == INODEQ(J))GO TO 200
	 END IF  
         IF(INFLOW_TYPE == 'edge')THEN
	   IF(I == N_ICELLQ(J,1) .OR. I == N_ICELLQ(J,2))GO TO 200
	 END IF  
       END DO
     END IF
     DO K=1,KBM1
       SMAX = MAXVAL(BIO_ALL(NBSN(I,1:NTSN(I)),K,N1))
       SMIN = MINVAL(BIO_ALL(NBSN(I,1:NTSN(I)),K,N1))

       IF(K == 1)THEN
         SMAX = MAX(SMAX,(BIO_ALL(I,K,N1)*DZ(I,K+1)+BIO_ALL(I,K+1,N1)*DZ(I,K))/  &
                (DZ(I,K)+DZ(I,K+1)))
         SMIN = MIN(SMIN,(BIO_ALL(I,K,N1)*DZ(I,K+1)+BIO_ALL(I,K+1,N1)*DZ(I,K))/  &
                (DZ(I,K)+DZ(I,K+1)))
       ELSE IF(K == KBM1)THEN
         SMAX = MAX(SMAX,(BIO_ALL(I,K,N1)*DZ(I,K-1)+BIO_ALL(I,K-1,N1)*DZ(I,K))/  &
                (DZ(I,K)+DZ(I,K-1)))
         SMIN = MIN(SMIN,(BIO_ALL(I,K,N1)*DZ(I,K-1)+BIO_ALL(I,K-1,N1)*DZ(I,K))/  &
                (DZ(I,K)+DZ(I,K-1)))
       ELSE
         SMAX = MAX(SMAX,(BIO_ALL(I,K,N1)*DZ(I,K-1)+BIO_ALL(I,K-1,N1)*DZ(I,K))/  &
                (DZ(I,K)+DZ(I,K-1)),                             &
                (BIO_ALL(I,K,N1)*DZ(I,K+1)+BIO_ALL(I,K+1,N1)*DZ(I,K))/           &
                (DZ(I,K)+DZ(I,K+1)))
         SMIN = MIN(SMIN,(BIO_ALL(I,K,N1)*DZ(I,K-1)+BIO_ALL(I,K-1,N1)*DZ(I,K))/  &
                (DZ(I,K)+DZ(I,K-1)),                             &
                (BIO_ALL(I,K,N1)*DZ(I,K+1)+BIO_ALL(I,K+1,N1)*DZ(I,K))/           &
                (DZ(I,K)+DZ(I,K+1)))
       END IF

       IF(SMIN-BIO_F(I,K,N1) > 0.0_SP) BIO_F(I,K,N1) = SMIN
       IF(BIO_F(I,K,N1)-SMAX > 0.0_SP) BIO_F(I,K,N1) = SMAX

     END DO
200 CONTINUE
   END DO
   END DO

100 CONTINUE
   RETURN
   END SUBROUTINE FCT_BIO
!==============================================================================|


