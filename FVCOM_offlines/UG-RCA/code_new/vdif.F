!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!==============================================================================|
      SUBROUTINE VDIF(F) 
 
      USE ALL_VARS
      USE MOD_RCA
      IMPLICIT NONE
!      INCLUDE 'RCACM'

      REAL, DIMENSION(0:MT,NZ+1) :: F
      
!      REAL, DIMENSION(M,NZ+1)   :: FF, VHF, VHPF
      REAL, DIMENSION(0:MT,NZ+1)   :: FF, VHF, VHPF  !Tian
            
      REAL, DIMENSION(M,NZ+1)      :: AF, CF   !, RAD
      REAL, DIMENSION(M)      :: BENFLUX,WFSURF
!JQI      REAL, DIMENSION(NXY)      :: SOURCE1,SOURCE2,SOURCE3
!JQI      REAL, DIMENSION(NXY)      :: TBOT
      REAL :: FKH,UMOLPR
!JQI      REAL :: TEMPWUVBOT,TMP
      INTEGER  :: I,K,J,KI,JC,II

!JQI      UMOLPR = EZ*1.E0
      UMOLPR = 1.000E-4        !1.E0

!----------------------------------------------------------------
!                                                                
!  the following section solves the equation               
!  dti*(kh*f')' -f=-fb
!                                                                
!----------------------------------------------------------------
 
      DO IZ = 2, NZ
       DO I = 1, M
         FKH=EZ(I,IZ)
	 
!JQI         AF(I,IZ-1)=-DT*(FKH+UMOLPR)/(DZ(I,IZ-1)*DZZ(I,IZ-1)*D(I)*D(I))
!JQI         CF(I,IZ)=-DT*(FKH+UMOLPR)/(DZ(I,IZ)*DZZ(I,IZ-1)*D(I)*D(I))
         AF(I,IZ-1)=-DT*86400.*(FKH+UMOLPR)/(DZ(I,IZ-1)*DZZ(I,IZ-1)*D(I)*D(I))
         CF(I,IZ)=-DT*86400.*(FKH+UMOLPR)/(DZ(I,IZ)*DZZ(I,IZ-1)*D(I)*D(I))
       END DO
      END DO

      BENFLUX = 0.0
      WFSURF = 0.0

!------------------------------------------------
!  Surface BCs; WFSURF
!----------------------------------------------- 

      DO I = 1,M
        VHF(I,1) = AF(I,1) / (AF(I,1)-1.)
!JQI        VHPF(I,1) = -DT * WFSURF(I) / (-DZ(I,1)*D(I)) - F(I,1)
        VHPF(I,1) = -DT*86400. * WFSURF(I) / (-DZ(I,1)*D(I)) - F(I,1)
        VHPF(I,1) = VHPF(I,1) / (AF(I,1)-1.)
      END DO
       
      DO IZ = 2, NZ-1        !KBM2
       DO I = 1, M
         VHPF(I,IZ)=1./ (AF(I,IZ)+CF(I,IZ)*(1.-VHF(I,IZ-1))-1.)
         VHF(I,IZ) = AF(I,IZ) * VHPF(I,IZ)
         VHPF(I,IZ) = (CF(I,IZ)*VHPF(I,IZ-1)-DBLE(F(I,IZ)))*VHPF(I,IZ)
       END DO
      END DO

      FF = F

      DO I = 1, M
       IF(ISONB(I) /= 2) THEN
!JQI         FF(I,NZ) = (CF(I,NZ)*VHPF(I,NZ-1)-FF(I,NZ)-DT*BENFLUX(I)/(D(I)*DZ(I,NZ)))/  &
         FF(I,NZ) = (CF(I,NZ)*VHPF(I,NZ-1)-FF(I,NZ)-DT*86400.*BENFLUX(I)/(D(I)*DZ(I,NZ)))/  &
                    (CF(I,NZ)*(1.-VHF(I,NZ-1))-1.)
       END IF
      END DO

      DO IZ = 2, NZ
       KI = NZ+1 - IZ
       DO I = 1, M
        IF(ISONB(I) /= 2) THEN
          FF(I,KI) = (VHF(I,KI)*FF(I,KI+1)+VHPF(I,KI))
        END IF
       END DO
      END DO
 
      F = FF

      RETURN
      END SUBROUTINE VDIF
!==============================================================================!

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
 
