!==============================================================================|
!  FLUX CONTROL FOR NUTRIENTS                                                        |
!==============================================================================|

   SUBROUTINE FCT_NUT
!#  if defined (WET_DRY)

!==============================================================================|
   USE ALL_VARS
   USE BCS
   USE WQM, ONLY : C2,C2F,NAC,AC
   USE MOD_OBCS
   IMPLICIT NONE
   REAL(SP):: NUTMAX,NUTMIN
   INTEGER :: I,J,K,JC,II
!==============================================================================|
  DO JC=1,NAC
   II=AC(JC)
   DO I=1,MLOC
      IF(IOBCN > 0)THEN
        DO J=1,IOBCN
          IF(I == I_OBC_N(J)) GO TO 200
       ENDDO
      ENDIF

      IF(NUMQBC > 0 )then
        DO J=1,NUMQBC
           IF(INFLOW_TYPE == 'node') THEN
             IF (I == INODEQ(J)) GO TO 200
           ENDIF
        ENDDO
      ENDIF

      DO K=1,KBM1
       NUTMAX = MAXVAL(C2(NBSN(I,1:NTSN(I)),K,II))
       NUTMIN = MINVAL(C2(NBSN(I,1:NTSN(I)),K,II))

       IF(K == 1)THEN
         NUTMAX = MAX(NUTMAX,(C2(I,K,II)*DZ(K+1)+C2(I,K+1,II)*DZ(K))/(DZ(K)+DZ(K+1)))
         NUTMIN = MIN(NUTMIN,(C2(I,K,II)*DZ(K+1)+C2(I,K+1,II)*DZ(K))/(DZ(K)+DZ(K+1)))
       ELSE IF(K == KBM1)THEN
         NUTMAX = MAX(NUTMAX,(C2(I,K,II)*DZ(K-1)+C2(I,K-1,II)*DZ(K))/(DZ(K)+DZ(K-1)))
         NUTMIN = MIN(NUTMIN,(C2(I,K,II)*DZ(K-1)+C2(I,K-1,II)*DZ(K))/(DZ(K)+DZ(K-1)))
       ELSE
         NUTMAX = MAX(NUTMAX,(C2(I,K,II)*DZ(K-1)+C2(I,K-1,II)*DZ(K))/(DZ(K)+DZ(K-1)), &
                 (C2(I,K,II)*DZ(K+1)+C2(I,K+1,II)*DZ(K))/(DZ(K)+DZ(K+1)))
         NUTMIN = MIN(NUTMIN,(C2(I,K,II)*DZ(K-1)+C2(I,K-1,II)*DZ(K))/(DZ(K)+DZ(K-1)), &
                 (C2(I,K,II)*DZ(K+1)+C2(I,K+1,II)*DZ(K))/(DZ(K)+DZ(K+1)))
       END IF

!tykim
!       IF(NUTMIN-C2F(I,K,II) >  0.0_SP ) C2F(I,K,II) = NUTMIN
!       IF(C2F(I,K,II)-NUTMAX >  0.0_SP ) C2F(I,K,II) = NUTMAX

       IF(NUTMIN-C2F(I,K,II) >  C2F(I,K,II)*0.001 ) C2F(I,K,II) = NUTMIN
       IF(C2F(I,K,II)-NUTMAX >  C2F(I,K,II)*0.001 ) C2F(I,K,II) = NUTMAX
      ENDDO

 200 CONTINUE

   ENDDO
   ENDDO
   RETURN
!#  endif
   END SUBROUTINE FCT_NUT
!==============================================================================|


