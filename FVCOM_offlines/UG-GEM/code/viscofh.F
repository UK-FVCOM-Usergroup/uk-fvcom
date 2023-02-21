!==============================================================================|
!  Calculate Advection and Horizontal Diffusion Terms for Temperature          |
!==============================================================================|

   SUBROUTINE VISCOF_H               

!------------------------------------------------------------------------------|

   USE ALL_VARS
#  if defined (MULTIPROCESSOR)
   USE MOD_PAR
#  endif
#  if defined (SPHERICAL)
   USE MOD_SPHERICAL
#  endif   

   IMPLICIT NONE
   REAL(SP), DIMENSION(M) :: PUPX,PUPY,PVPX,PVPY  
   REAL(SP), DIMENSION(M) :: VISCOFF
   REAL(SP) :: X11,Y11,X22,Y22,X33,Y33,TMP1,TMP2
   INTEGER  :: I,I1,IA,IB,J,J1,J2,K,JTMP
#  if defined (SPHERICAL)
   REAL(DP) :: TXPI,TYPI
   REAL(DP) :: XTMP1,XTMP
   REAL(DP) :: X1_DP,Y1_DP,X2_DP,Y2_DP
   REAL(DP) :: X11_TMP,Y11_TMP,X33_TMP,Y33_TMP
#  endif

   IF(HORZMIX == 'constant') THEN
    VISCOFH =0.0_SP
    RETURN
   END IF
!
!--Calculate the Advection and Horizontal Diffusion Terms----------------------!
!

   DO K=1,KBM1
    DO I=1,M
     PUPX(I)=0.0_SP
     PUPY(I)=0.0_SP
     PVPX(I)=0.0_SP
     PVPY(I)=0.0_SP
     J=1
     I1=NBVE(I,J)
     JTMP=NBVT(I,J)
     J1=JTMP+1-(JTMP+1)/4*3
     J2=JTMP+2-(JTMP+2)/4*3
     X11=0.5_SP*(VX(I)+VX(NV(I1,J1)))
     Y11=0.5_SP*(VY(I)+VY(NV(I1,J1)))
     X22=XC(I1)
     Y22=YC(I1)
     X33=0.5_SP*(VX(I)+VX(NV(I1,J2)))
     Y33=0.5_SP*(VY(I)+VY(NV(I1,J2)))

#    if defined (SPHERICAL)
     X1_DP=VX(I)
     Y1_DP=VY(I)
     X2_DP=VX(NV(I1,J1))
     Y2_DP=VY(NV(I1,J1))

     X11=XCA(I)
     Y11=YCA(I)
     X2_DP=VX(NV(I1,J2))
     Y2_DP=VY(NV(I1,J2))

     X33=XCB(I)
     Y33=YCB(I)

     XTMP  = X33*TPI-X11*TPI
     XTMP1 = X33-X11
     IF(XTMP1 >  180.0_SP)THEN
      XTMP = -360.0_SP*TPI+XTMP
     ELSE IF(XTMP1 < -180.0_SP)THEN
      XTMP =  360.0_SP*TPI+XTMP
     END IF	 
     TXPI=XTMP*COS(DEG2RAD*VY(I))
     TYPI=(Y11-Y33)*TPI

     PUPX(I)=PUPX(I)+U(I1,K)*TYPI
     PUPY(I)=PUPY(I)+U(I1,K)*TXPI
     PVPX(I)=PVPX(I)+V(I1,K)*TYPI
     PVPY(I)=PVPY(I)+V(I1,K)*TXPI
#    else
     PUPX(I)=PUPX(I)+U(I1,K)*(Y11-Y33)
     PUPY(I)=PUPY(I)+U(I1,K)*(X33-X11)
     PVPX(I)=PVPX(I)+V(I1,K)*(Y11-Y33)
     PVPY(I)=PVPY(I)+V(I1,K)*(X33-X11)
#    endif

     IF(ISONB(I) /= 0) THEN
#     if defined (SPHERICAL)
      XTMP  = X11*TPI-VX(I)*TPI
      XTMP1 = X11-VX(I)
      IF(XTMP1 >  180.0_SP)THEN
       XTMP = -360.0_SP*TPI+XTMP
      ELSE IF(XTMP1 < -180.0_SP)THEN
       XTMP =  360.0_SP*TPI+XTMP
      END IF  
      TXPI=XTMP*COS(DEG2RAD*VY(I))
      TYPI=(VY(I)-Y11)*TPI

      PUPX(I)=PUPX(I)+U(I1,K)*TYPI
      PUPY(I)=PUPY(I)+U(I1,K)*TXPI
      PVPX(I)=PVPX(I)+V(I1,K)*TYPI
      PVPY(I)=PVPY(I)+V(I1,K)*TXPI
#     else
      PUPX(I)=PUPX(I)+U(I1,K)*(VY(I)-Y11)
      PUPY(I)=PUPY(I)+U(I1,K)*(X11-VX(I))
      PVPX(I)=PVPX(I)+V(I1,K)*(VY(I)-Y11)
      PVPY(I)=PVPY(I)+V(I1,K)*(X11-VX(I))
#     endif
     END IF

     DO J=2,NTVE(I)-1
      I1=NBVE(I,J)
      JTMP=NBVT(I,J)
      J1=JTMP+1-(JTMP+1)/4*3
      J2=JTMP+2-(JTMP+2)/4*3
      X11=0.5_SP*(VX(I)+VX(NV(I1,J1)))
      Y11=0.5_SP*(VY(I)+VY(NV(I1,J1)))
      X22=XC(I1)
      Y22=YC(I1)
      X33=0.5_SP*(VX(I)+VX(NV(I1,J2)))
      Y33=0.5_SP*(VY(I)+VY(NV(I1,J2)))

#     if defined (SPHERICAL)
      X1_DP=VX(I)
      Y1_DP=VY(I)
      X2_DP=VX(NV(I1,J1))
      Y2_DP=VY(NV(I1,J1))

      X11=XCC(I,J)
      Y11=YCC(I,J)
      X2_DP=VX(NV(I1,J2))
      Y2_DP=VY(NV(I1,J2))

      X33=XCD(I,J)
      Y33=YCD(I,J)

      XTMP  = X33*TPI-X11*TPI
      XTMP1 = X33-X11
      IF(XTMP1 >  180.0_SP)THEN
       XTMP = -360.0_SP*TPI+XTMP
      ELSE IF(XTMP1 < -180.0_SP)THEN
       XTMP =  360.0_SP*TPI+XTMP
      END IF  
      TXPI=XTMP*COS(DEG2RAD*VY(I))
      TYPI=(Y11-Y33)*TPI
      PUPX(I)=PUPX(I)+U(I1,K)*TYPI
      PUPY(I)=PUPY(I)+U(I1,K)*TXPI
      PVPX(I)=PVPX(I)+V(I1,K)*TYPI
      PVPY(I)=PVPY(I)+V(I1,K)*TXPI
#     else
      PUPX(I)=PUPX(I)+U(I1,K)*(Y11-Y33)
      PUPY(I)=PUPY(I)+U(I1,K)*(X33-X11)
      PVPX(I)=PVPX(I)+V(I1,K)*(Y11-Y33)
      PVPY(I)=PVPY(I)+V(I1,K)*(X33-X11)
#     endif
     END DO
     J=NTVE(I)
     I1=NBVE(I,J)
     JTMP=NBVT(I,J)
     J1=JTMP+1-(JTMP+1)/4*3
     J2=JTMP+2-(JTMP+2)/4*3
     X11=0.5_SP*(VX(I)+VX(NV(I1,J1)))
     Y11=0.5_SP*(VY(I)+VY(NV(I1,J1)))
     X22=XC(I1)
     Y22=YC(I1)
     X33=0.5_SP*(VX(I)+VX(NV(I1,J2)))
     Y33=0.5_SP*(VY(I)+VY(NV(I1,J2)))

#    if defined (SPHERICAL)
     X1_DP=VX(I)
     Y1_DP=VY(I)
     X2_DP=VX(NV(I1,J1))
     Y2_DP=VY(NV(I1,J1))

     X11=XCE(I)
     Y11=YCE(I)
     X2_DP=VX(NV(I1,J2))
     Y2_DP=VY(NV(I1,J2))

     X33=XCF(I)
     Y33=YCF(I)

     XTMP  = X33*TPI-X11*TPI
     XTMP1 = X33-X11
     IF(XTMP1 >  180.0_SP)THEN
      XTMP = -360.0_SP*TPI+XTMP
     ELSE IF(XTMP1 < -180.0_SP)THEN
      XTMP =  360.0_SP*TPI+XTMP
     END IF
     TXPI=XTMP*COS(DEG2RAD*VY(I))
     TYPI=(Y11-Y33)*TPI

     PUPX(I)=PUPX(I)+U(I1,K)*TYPI
     PUPY(I)=PUPY(I)+U(I1,K)*TXPI
     PVPX(I)=PVPX(I)+V(I1,K)*TYPI
     PVPY(I)=PVPY(I)+V(I1,K)*TXPI
#    else
     PUPX(I)=PUPX(I)+U(I1,K)*(Y11-Y33)
     PUPY(I)=PUPY(I)+U(I1,K)*(X33-X11)
     PVPX(I)=PVPX(I)+V(I1,K)*(Y11-Y33)
     PVPY(I)=PVPY(I)+V(I1,K)*(X33-X11)
#    endif

     IF(ISONB(I) /= 0) THEN
#     if defined (SPHERICAL)
      XTMP  = VX(I)*TPI-X11*TPI
      XTMP1 = VX(I)-X11
      IF(XTMP1 >  180.0_SP)THEN
       XTMP = -360.0_SP*TPI+XTMP
      ELSE IF(XTMP1 < -180.0_SP)THEN
       XTMP =  360.0_SP*TPI+XTMP
      END IF  
      TXPI=XTMP*COS(DEG2RAD*VY(I))
      TYPI=(Y11-VY(I))*TPI
	 
      PUPX(I)=PUPX(I)+U(I1,K)*TYPI
      PUPY(I)=PUPY(I)+U(I1,K)*TXPI
      PVPX(I)=PVPX(I)+V(I1,K)*TYPI
      PVPY(I)=PVPY(I)+V(I1,K)*TXPI
#     else
      PUPX(I)=PUPX(I)+U(I1,K)*(Y11-VY(I))
      PUPY(I)=PUPY(I)+U(I1,K)*(VX(I)-X11)
      PVPX(I)=PVPX(I)+V(I1,K)*(Y11-VY(I))
      PVPY(I)=PVPY(I)+V(I1,K)*(VX(I)-X11)
#     endif
     END IF
     PUPX(I)=PUPX(I)/ART1(I)
     PUPY(I)=PUPY(I)/ART1(I)
     PVPX(I)=PVPX(I)/ART1(I)
     PVPY(I)=PVPY(I)/ART1(I)
     TMP1=PUPX(I)**2+PVPY(I)**2
     TMP2=0.5_SP*(PUPY(I)+PVPX(I))**2
     VISCOFF(I)=SQRT(TMP1+TMP2)*ART1(I)
       
     VISCOFH(I,K) = VISCOFF(I)

    END DO
   END DO  
    
#  if defined (MULTIPROCESSOR)
   IF(PAR) CALL EXCHANGE(NC,MT,KB,MYID,NPROCS,VISCOFH)
#  endif

   RETURN
   END SUBROUTINE VISCOF_H
!==============================================================================|
