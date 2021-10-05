!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!==============================================================================|
   SUBROUTINE VDIF_WQM(F)
 
   USE ALL_VARS
   USE WQM,ONLY : NAC,AC,DLT,NCP,ZDFBCK
   IMPLICIT NONE
   REAL, DIMENSION(0:MTLOC,KBM1,NCP) :: F,FF
   REAL, DIMENSION(MLOC,KB,NCP)    :: VHF, VHPF
   REAL, DIMENSION(MLOC,NCP)       :: AF, CF, RAD
   REAL, DIMENSION(MLOC,NCP)       :: BENFLUX,WFSURF
   REAL, DIMENSION(MLOC)          :: SOURCE1,SOURCE2,SOURCE3
   REAL, DIMENSION(MLOC)          :: TBOT
   REAL :: FKH,UMOLPR
   REAL :: TEMPWUVBOT,TMP
   INTEGER  :: I,K,J,KI,JC,II

   UMOLPR = ZDFBCK*1.E0

!----------------------------------------------------------------
!                                                                
!  the following section solves the equation               
!  dti*(kh*f')' -f=-fb
!                                                                
!----------------------------------------------------------------

   DO K = 2, KBM1
     DO I = 1, MLOC
       IF(D(I) > 0.0)THEN
         FKH=KH(I,K)

         AF(I,K-1)=-DLT*(FKH+UMOLPR)/(DZ(K-1)*DZZ(K-1)*D(I)*D(I))
         CF(I,K)=-DLT*(FKH+UMOLPR)/(DZ(K)*DZZ(K-1)*D(I)*D(I))
       END IF
     END DO
   END DO

   WFSURF = 0.0

!------------------------------------------------
!  Surface BCs; WFSURF
!----------------------------------------------- 

   DO JC=1,NAC
     II = AC(JC)
     DO I = 1, MLOC
       IF (D(I) > 0.0) THEN
         VHF(I,1,II) = AF(I,1) / (AF(I,1)-1.)
         VHPF(I,1,II) = -DLT * WFSURF(I,II) / (-DZ(1)*D(I)) - F(I,1,II)
         VHPF(I,1,II) = VHPF(I,1,II) / (AF(I,1)-1.)
       END IF
     END DO
   END DO
       
   DO JC=1,NAC
     II = AC(JC)
     DO K = 2, KBM2
       DO I = 1, MLOC
         IF(D(I) > 0.0) THEN
           VHPF(I,K,II)=1./ (AF(I,K)+CF(I,K)*(1.-VHF(I,K-1,II))-1.)
           VHF(I,K,II) = AF(I,K) * VHPF(I,K,II)
           VHPF(I,K,II) = (CF(I,K)*VHPF(I,K-1,II)-          &
                          DBLE(F(I,K,II)))*VHPF(I,K,II)
         END IF
       END DO
     END DO
   END DO

   DO JC=1,NAC
     II = AC(JC)
     DO K = 1, KBM1 
       DO I = 1, MLOC
         If (D(I) > 0.0)THEN
           FF(I,K,II) = F(I,K,II)
         END IF
       END DO
     END DO
   END DO

   DO JC=1,NAC
      II=AC(JC)
     DO I = 1, MLOC
       IF (D(I) > 0.0 .AND. ISONB(I) /= 2) THEN
         FF(I,KBM1,II) = (CF(I,KBM1)*VHPF(I,KBM2,II)-FF(I,KBM1,II)   &
                         -DLT*BENFLUX(I,II)/(D(I)*DZ(KBM1)))/  &
                         (CF(I,KBM1)*(1.-VHF(I,KBM2,II))-1.)
       END IF
     END DO
   END DO

   DO JC=1,NAC
     II = AC(JC)
     DO K = 2, KBM1
       KI = KB - K
       DO I = 1, MLOC
         IF (D(I) > 0.0 .AND. ISONB(I) /= 2) THEN
           FF(I,KI,II) = (VHF(I,KI,II)*FF(I,KI+1,II)+VHPF(I,KI,II))
         END IF
       END DO
     END DO
   END DO

   DO JC=1,NAC
     II = AC(JC)
     DO I = 1, MLOC
       IF(D(I) > 0.0)THEN
         DO K = 1, KBM1
           F(I,K,II) = FF(I,K,II)
         END DO
       END IF
     END DO
   END DO

   RETURN
   END SUBROUTINE VDIF_WQM
!==============================================================================!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
 
