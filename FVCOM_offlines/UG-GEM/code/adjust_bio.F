
!==============================================================================|
!     ADJUST BIO_VARIABLES NEAR RIVER MOUTHS USING ADJACENT NODES                |
!     ADJUST BIO_VARIABLES AT RIVER MOUTHS
!==============================================================================|

   SUBROUTINE ADJUST_BIO(WDIS)

!==============================================================================|
   USE ALL_VARS
   USE MOD_BIO_3D
!!   use mod_par
   USE BCS
   IMPLICIT NONE
   REAL(SP) :: TAVE(NTT),TAVE1(NTT),TAVE2(NTT)
   INTEGER :: I,K,JJ,I1,J,J1,J2,NUM_TAVE,NUM_TAVE1,NUM_TAVE2
   REAL(SP) :: WDIS(NUMQBC,NTT)
!==============================================================================|

   
   IF(NUMQBC > 0)THEN   

    IF(INFLOW_TYPE == 'node')THEN
     DO K=1,KBM1
      DO I=1,NUMQBC
       JJ=INODEQ(I)
       TAVE = 0.0_SP
       NUM_TAVE = 0
       DO J=2,NTSN(JJ)-1
        I1=NBSN(JJ,J)
        IF(NUMQBC == 1)THEN
         NUM_TAVE = NUM_TAVE + 1
         TAVE(:) = TAVE(:) + BIO_F(I1,K,:)
        ELSE
         IF(I == 1)THEN
          IF(I1 /= INODEQ(I+1))THEN 
           NUM_TAVE = NUM_TAVE + 1
           TAVE(:) = TAVE(:) + BIO_F(I1,K,:)
          END IF
         ELSE IF(I == NUMQBC)THEN
          IF(I1 /= INODEQ(I-1))THEN	 
           NUM_TAVE = NUM_TAVE + 1
           TAVE(:) = TAVE(:) + BIO_F(I1,K,:)
          END IF
         ELSE IF(I1 /= INODEQ(I-1) .AND. I1 /= INODEQ(I+1))THEN
          NUM_TAVE = NUM_TAVE + 1
          TAVE(:) = TAVE(:) + BIO_F(I1,K,:)
         END IF	 
        END IF
       END DO
       BIO_F(JJ,K,:) = TAVE(:)/FLOAT(NUM_TAVE)
      END DO
     END DO
    ELSE IF(INFLOW_TYPE == 'edge')THEN
     DO K=1,KBM1
      DO I=1,NUMQBC
       J1=N_ICELLQ(I,1)
       J2=N_ICELLQ(I,2)
       TAVE1 = 0.0_SP
       TAVE2 = 0.0_SP
       NUM_TAVE1 = 0
       NUM_TAVE2 = 0

       DO J=2,NTSN(J1)-1
        I1=NBSN(J1,J)
        IF(NUMQBC == 1)THEN
         IF(I1 /= J2)THEN
          NUM_TAVE1 = NUM_TAVE1 + 1
          TAVE1(:) = TAVE1(:) + BIO_F(I1,K,:)
         END IF
        ELSE IF(I == 1)THEN
         IF(I1 /= J2 .AND. I1 /= N_ICELLQ(I+1,1) .AND. &
            I1 /= N_ICELLQ(I+1,2))THEN
          NUM_TAVE1 = NUM_TAVE1 + 1
          TAVE1(:) = TAVE1(:) + BIO_F(I1,K,:)
         END IF
        ELSE IF(I == NUMQBC)THEN
         IF(I1 /= J2 .AND. I1 /= N_ICELLQ(I-1,1) .AND. &
            I1 /= N_ICELLQ(I-1,2))THEN
          NUM_TAVE1 = NUM_TAVE1 + 1
          TAVE1(:) = TAVE1(:) + BIO_F(I1,K,:)
         END IF
        ELSE IF(I1 /= J2 .AND. &
	        I1 /= N_ICELLQ(I-1,1) .AND. I1 /= N_ICELLQ(I-1,2) .AND.  &
		I1 /= N_ICELLQ(I+1,1) .AND. I1 /= N_ICELLQ(I+1,2))THEN
         NUM_TAVE1 = NUM_TAVE1 + 1
         TAVE1(:) = TAVE1(:) + BIO_F(I1,K,:)
        END IF
       END DO
       BIO_F(J1,K,:) = TAVE1(:)/FLOAT(NUM_TAVE1)

       DO J=2,NTSN(J2)-1
        I1=NBSN(J2,J)
        IF(NUMQBC == 1)THEN
         IF(I1 /= J1)THEN
          NUM_TAVE2 = NUM_TAVE2 + 1
          TAVE2(:) = TAVE2(:) + BIO_F(I1,K,:)
         END IF
        ELSE IF(I == 1)THEN
         IF(I1 /= J1 .AND. I1 /= N_ICELLQ(I+1,1) .AND. &
            I1 /= N_ICELLQ(I+1,2))THEN
          NUM_TAVE2 = NUM_TAVE2 + 1
          TAVE2(:) = TAVE2(:) + BIO_F(I1,K,:)
         END IF
        ELSE IF(I == NUMQBC)THEN  
         IF(I1 /= J1 .AND. I1 /= N_ICELLQ(I-1,1) .AND. &
            I1 /= N_ICELLQ(I-1,2))THEN
          NUM_TAVE2 = NUM_TAVE2 + 1
          TAVE2(:) = TAVE2(:) + BIO_F(I1,K,:)
         END IF
        ELSE IF(I1 /= J1 .AND. &
                I1 /= N_ICELLQ(I-1,1) .AND. I1 /= N_ICELLQ(I-1,2) .AND.  &
                I1 /= N_ICELLQ(I+1,1) .AND. I1 /= N_ICELLQ(I+1,2))THEN
         NUM_TAVE2 = NUM_TAVE2 + 1
         TAVE2(:) = TAVE2(:) + BIO_F(I1,K,:)
        END IF
       END DO
       BIO_F(J2,K,:) = TAVE2(:)/FLOAT(NUM_TAVE2)
   
      END DO
     END DO	 
    END IF
   
    DO JJ=1,NNN
     DO I=1,NUMQBC
       IF(INFLOW_TYPE == 'node')THEN
         J = INODEQ(I)
         DO K=1,KBM1
           BIO_F(J,K,JJ+INN-1) = WDIS(I,JJ+INN-1)
         END DO
       ELSE IF(INFLOW_TYPE == 'edge')THEN
         J1 = N_ICELLQ(I,1)
         J2 = N_ICELLQ(I,2)
         DO K=1,KBM1
           BIO_F(J1,K,JJ+INN-1) = WDIS(I,JJ+INN-1)
           BIO_F(J2,K,JJ+INN-1) = WDIS(I,JJ+INN-1)
         END DO
       END IF
     END DO
    END DO 

!!     CALL N2E3D(T1,T)
!!     CALL N2E3D(S1,S)

   END IF

   RETURN
   END SUBROUTINE ADJUST_BIO
!==============================================================================|

