!==============================================================================|
!   Calculate Advection and Horizontal Diffusion Terms for Temperature         |
!==============================================================================|

   SUBROUTINE VISCOF_H               

!------------------------------------------------------------------------------|

   USE ALL_VARS
# if defined (MULTIPROCESSOR)
   USE MOD_PAR
# endif
   USE MOD_OBCS

   IMPLICIT NONE
   REAL(SP), DIMENSION(MLOC)            :: PUPX,PUPY,PVPX,PVPY  
   REAL(SP), DIMENSION(MLOC)            :: VISCOFF
   REAL(SP) :: X11,Y11,X22,Y22,X33,Y33,TMP1,TMP2
   INTEGER  :: I,I1,IA,IB,J,J1,J2,K,JTMP

!
!--Calculate the Advection and Horizontal Diffusion Terms----------------------!
!

   DO K=1,KBM1
     DO I=1,MLOC
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

       PUPX(I)=PUPX(I)+UU(I1,K)*(Y11-Y33)
       PUPY(I)=PUPY(I)+UU(I1,K)*(X33-X11)
       PVPX(I)=PVPX(I)+VV(I1,K)*(Y11-Y33)
       PVPY(I)=PVPY(I)+VV(I1,K)*(X33-X11)

       IF(ISONB(I) /= 0) THEN
         PUPX(I)=PUPX(I)+UU(I1,K)*(VY(I)-Y11)
         PUPY(I)=PUPY(I)+UU(I1,K)*(X11-VX(I))
         PVPX(I)=PVPX(I)+VV(I1,K)*(VY(I)-Y11)
         PVPY(I)=PVPY(I)+VV(I1,K)*(X11-VX(I))
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

         PUPX(I)=PUPX(I)+UU(I1,K)*(Y11-Y33)
         PUPY(I)=PUPY(I)+UU(I1,K)*(X33-X11)
         PVPX(I)=PVPX(I)+VV(I1,K)*(Y11-Y33)
         PVPY(I)=PVPY(I)+VV(I1,K)*(X33-X11)
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

       PUPX(I)=PUPX(I)+UU(I1,K)*(Y11-Y33)
       PUPY(I)=PUPY(I)+UU(I1,K)*(X33-X11)
       PVPX(I)=PVPX(I)+VV(I1,K)*(Y11-Y33)
       PVPY(I)=PVPY(I)+VV(I1,K)*(X33-X11)

       IF(ISONB(I) /= 0) THEN
         PUPX(I)=PUPX(I)+UU(I1,K)*(Y11-VY(I))
         PUPY(I)=PUPY(I)+UU(I1,K)*(VX(I)-X11)
         PVPX(I)=PVPX(I)+VV(I1,K)*(Y11-VY(I))
         PVPY(I)=PVPY(I)+VV(I1,K)*(VX(I)-X11)
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
    
   RETURN
   END SUBROUTINE VISCOF_H
!==============================================================================|
