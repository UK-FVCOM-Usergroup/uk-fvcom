      SUBROUTINE RCAMPROF(CSIGMA,CSLAVE)
!
!        RCAMPROF COMPUTES DOMAIN-AVERAGED WATER QUALITY CONCENTRATIONS
!                 FOR USE IN SIGMA-COORDINATE CORRECTION
!

      USE ALL_VARS,ONLY : H,ZZ
      USE MOD_RCA
      IMPLICIT NONE
      SAVE
!      INCLUDE 'RCACM'
      REAL  CSIGMA(NXY,NZ),CSLAVE(NXY,NZ)
      REAL  ZM(NZ),CI(NZ+1),CA(NSLC),CSL(NXY,NSLC)
      REAL  SRFAREA

!        CSIGMA - INPUT ARRAY OF SIGMA-LEVEL CONCENTRATIONS
!        CSLAVE - OUTPUT ARRAY OF SIGMA-LEVEL CONCENTRATIONS
!                   MINUS THE DOMAIN-AVERAGED CONCNETRATIONS,
!                   I.E. THE SIGMA-LEVEL RESIDULALS

!        INTERPOLATE CSIGMA ONTO STANDARD LEVELS

! c$doacross local(iz,iy,ix) , share(ci,csl,zm)
      DO 40 IXY=1,NXY
          IF (FSM(IXY).NE.0.0) THEN
            DO 10 IZ=1,NZ
              ZM(IZ) = -ZZ(IXY,IZ) * HBAR(IXY)
              CI(IZ) = CSIGMA(IXY,IZ)
   10       CONTINUE
            CALL SINTER(ZM,CI,SLCDPTH,CA,NZ,NOSLC)
            DO 20 IZ=1,NOSLC
              CSL(IXY,IZ) = CA(IZ)
   20       CONTINUE
          END IF
   40 CONTINUE

!        FIND MEAN CONC AT EACH STANDARD LEVEL, BY AREALLY
!        INTEGRATING AND DIVIDING BY THE AREA AT THAT DEPTH

! c$doacross local(iz,iy,ix,srfarea) , share(ca)
      DO 70 IZ=1,NOSLC
        CA(IZ) = 0.0
        SRFAREA = 0.0
        DO 60 IXY=1,NXY
            IF (FSM(IXY).NE.0.0) THEN
              IF(-H(IXY).LE.SLCDPTH(IZ)) THEN
                SRFAREA = SRFAREA + XAZ(IXY)
                CA(IZ) = CA(IZ) + CSL(IXY,IZ)*XAZ(IXY)
              END IF
            END IF
   60   CONTINUE
        IF (SRFAREA.GT.0.0)  CA(IZ) = CA(IZ)/SRFAREA
   70 CONTINUE

!        INTERPOLATE DOMAIN-AVERAGED PROFILE BACK ONTO SIGMA GRID
  
! c$doacross local(iz,iy,ix) , share(cslave)
      DO 140 IXY=1,NXY
          IF (FSM(IXY).NE.0.0) THEN
            DO 110 IZ=1,NZ
              ZM(IZ) = -ZZ(IXY,IZ) * HBAR(IXY)
  110       CONTINUE
            CALL SINTER(SLCDPTH,CA,ZM,CI,NOSLC,NZ)
            DO 120 IZ=1,NZ
              CSLAVE(IXY,IZ) = CI(IZ)
  120       CONTINUE
          END IF
  140 CONTINUE

!     call exit
      RETURN
      END
