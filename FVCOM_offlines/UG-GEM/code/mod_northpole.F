MODULE MOD_NORTHPOLE
#  if defined (SPHERICAL) && (NORTHPOLE)
   USE ALL_VARS 
   USE MOD_SPHERICAL

   IMPLICIT NONE
   SAVE
   INTEGER :: NODE_NORTHPOLE            !Node index at the north pole point
   INTEGER :: MP,NP,NPE,NPCV
   INTEGER, ALLOCATABLE :: NODE_NORTHAREA(:)
   INTEGER, ALLOCATABLE :: CELL_NORTHAREA(:)   
   INTEGER, ALLOCATABLE :: NPEDGE_LST(:)   
   INTEGER, ALLOCATABLE :: NCEDGE_LST(:)   
   INTEGER, ALLOCATABLE :: MP_LST(:),NP_LST(:)   
   REAL(DP), ALLOCATABLE :: A1U_XY(:,:),A2U_XY(:,:)
   REAL(DP), ALLOCATABLE :: AW0_XY(:,:),AWX_XY(:,:),AWY_XY(:,:)

   CONTAINS
!==============================================================================|
     SUBROUTINE FIND_NORTHPOLE
     IMPLICIT NONE
     INTEGER :: I,ITMP,NODE_NORTHPOLE_GL
     INTEGER,ALLOCATABLE :: TMP(:)
     
     NODE_NORTHPOLE = 0
     NODE_NORTHPOLE_GL = 0
     
     DO I = 1,MGL
       IF(ABS(YG(I)-90.0_SP) < 1.0E-4_SP)THEN
         NODE_NORTHPOLE_GL = I
       END IF
     END DO
     
     IF(NODE_NORTHPOLE_GL == 0)THEN
       PRINT*,"NO NODE POINT ON THE NORTH POLE."
       PRINT*,"PLEASE MOVE ONE NODE TO THE NORTH POLE."
       PRINT*,"STOP RUNNING......"
       CALL PSTOP
     END IF  
     
     DO I = 1,MT
       IF(ABS(VY(I)-90.0_SP) < 1.0E-4_SP)THEN
         NODE_NORTHPOLE = I
       END IF
     END DO
!     print*,'NORTH POLE = ',NODE_NORTHPOLE
     
     
     ALLOCATE(NODE_NORTHAREA(0:MT)); NODE_NORTHAREA = 0
     ALLOCATE(CELL_NORTHAREA(0:NT)); CELL_NORTHAREA = 0

     IF(NODE_NORTHPOLE >0) THEN
       ITMP = NODE_NORTHPOLE
       ALLOCATE(TMP(MT)); TMP = 0     
       MP = 0
       DO I=1,NTSN(ITMP)-1
	 MP = MP + 1
	 TMP(MP) = NBSN(ITMP,I)
         NODE_NORTHAREA(NBSN(ITMP,I)) = 1
       END DO  
       MP = MP + 1
       TMP(MP) = ITMP
       NODE_NORTHAREA(ITMP) = 1

       ALLOCATE(MP_LST(MP))
       MP_LST(1:MP) = TMP(1:MP)
       DEALLOCATE(TMP)
       
       ALLOCATE(TMP(NT)); TMP = 0          
       NP = 0 
       DO I=1,NTVE(ITMP)
         NP = NP + 1
	 TMP(NP) = NBVE(ITMP,I)
         CELL_NORTHAREA(NBVE(ITMP,I)) = 1
       END DO
     
       ALLOCATE(NP_LST(NP))
       NP_LST(1:NP) = TMP(1:NP)
       DEALLOCATE(TMP)
       
     ENDIF
     
     RETURN
     END SUBROUTINE FIND_NORTHPOLE
!==============================================================================|
     
!==============================================================================|

     SUBROUTINE FIND_CELLSIDE
     
     IMPLICIT NONE
     INTEGER  ::  I,IA,IB
     INTEGER, ALLOCATABLE :: TEMP(:)
     
     ALLOCATE(TEMP(NE));  TEMP = ZERO
     NPE = 0
     
     DO I=1,NE
       IA = IEC(I,1)
       IB = IEC(I,2)
       IF(CELL_NORTHAREA(IA) == 1 .OR. CELL_NORTHAREA(IB) == 1)THEN
         NPE = NPE + 1
	 TEMP(NPE) = I
       END IF
     END DO
     
     ALLOCATE(NPEDGE_LST(NPE))
     NPEDGE_LST(1:NPE) = TEMP(1:NPE)
     DEALLOCATE(TEMP)
     
     ALLOCATE(TEMP(NCV));  TEMP = ZERO
     NPCV = 0
     
     DO I=1,NCV
       IA = NIEC(I,1)
       IB = NIEC(I,2)
       IF(IA == NODE_NORTHPOLE .OR. IB == NODE_NORTHPOLE)THEN
         NPCV = NPCV + 1
	 TEMP(NPCV) = I
       END IF
     END DO
     
     ALLOCATE(NCEDGE_LST(NPCV))
     NCEDGE_LST(1:NPCV) = TEMP(1:NPCV)
     DEALLOCATE(TEMP)
     
     RETURN
     END SUBROUTINE FIND_CELLSIDE
       	 
!==============================================================================|
!==============================================================================|

   SUBROUTINE VERTVL_EDGE_XY(XFLUX,CETA)         

!------------------------------------------------------------------------------|
   IMPLICIT NONE 
   REAL(SP) :: XFLUX(MT,KBM1)
   REAL(SP) :: DIJ,UIJ,VIJ,UN,EXFLUX,TMP1,DIJ1,UIJ1,VIJ1
   INTEGER  :: I,K,IA,IB,I1 ,J,JJ,J1,J2,II

   REAL(SP) :: UIJ_TMP,VIJ_TMP,VX1_TMP,VY1_TMP,VX2_TMP,VY2_TMP,UIJ1_TMP,VIJ1_TMP
   REAL(SP) :: DLTXE_TMP,DLTYE_TMP,EXFLUX_TMP

   REAL(SP) :: CETA
!------------------------------------------------------------------------------|

!----------------------INITIALIZE FLUX-----------------------------------------!

   DO K=1,KBM1
     DO II=1,NPCV
       I = NCEDGE_LST(II)
       IA  = NIEC(I,1)
       IB  = NIEC(I,2)
       IF(IA == NODE_NORTHPOLE)THEN
         XFLUX(IA,K) = 0.0_SP
       END IF
       IF(IB == NODE_NORTHPOLE)THEN  
         XFLUX(IB,K) = 0.0_SP
       END IF  
     END DO  
   END DO
!----------------------ACCUMULATE FLUX-----------------------------------------!

   DO II=1,NPCV
     I=NCEDGE_LST(II)
     I1=NTRG(I)
     IA=NIEC(I,1)
     IB=NIEC(I,2)
     DO K=1,KBM1
       DIJ=D1(I1)*DZ1(I1,K)
       UIJ=U(I1,K)
       VIJ=V(I1,K)

       IF(IA == NODE_NORTHPOLE .OR. IB == NODE_NORTHPOLE)THEN
         UIJ_TMP = -VIJ*COS(XC(I1)*DEG2RAD)-UIJ*SIN(XC(I1)*DEG2RAD)
         VIJ_TMP = -VIJ*SIN(XC(I1)*DEG2RAD)+UIJ*COS(XC(I1)*DEG2RAD)

       VX1_TMP = REARTH * COS(YIJE(I,1)*DEG2RAD) * COS(XIJE(I,1)*DEG2RAD)&
                 * 2._SP /(1._SP+sin(YIJE(I,1)*DEG2RAD))
       VY1_TMP = REARTH * COS(YIJE(I,1)*DEG2RAD) * SIN(XIJE(I,1)*DEG2RAD)&
                 * 2._SP /(1._SP+sin(YIJE(I,1)*DEG2RAD))

       VX2_TMP = REARTH * COS(YIJE(I,2)*DEG2RAD) * COS(XIJE(I,2)*DEG2RAD)&
                 * 2._SP /(1._SP+sin(YIJE(I,2)*DEG2RAD))
       VY2_TMP = REARTH * COS(YIJE(I,2)*DEG2RAD) * SIN(XIJE(I,2)*DEG2RAD)&
                 * 2._SP /(1._SP+sin(YIJE(I,2)*DEG2RAD))

         DLTXE_TMP = VX2_TMP-VX1_TMP
         DLTYE_TMP = VY2_TMP-VY1_TMP
       
         EXFLUX_TMP = DIJ*(-UIJ_TMP*DLTYE_TMP+VIJ_TMP*DLTXE_TMP)
       END IF  
     
       IF(IA == NODE_NORTHPOLE)THEN
         XFLUX(IA,K) = XFLUX(IA,K)-EXFLUX_TMP
       ELSE IF(IB == NODE_NORTHPOLE)THEN
         XFLUX(IB,K) = XFLUX(IB,K)+EXFLUX_TMP
       END IF
     END DO
   END DO

   RETURN
   END SUBROUTINE VERTVL_EDGE_XY
!==============================================================================|

!==============================================================================|
   SUBROUTINE ADV_S_XY(XFLUX,XFLUX_ADV,PSPX,PSPY,PSPXD,PSPYD,VISCOFF,K,CETA)               

!------------------------------------------------------------------------------|

   IMPLICIT NONE
   INTEGER, INTENT(IN) :: K

   REAL(SP), DIMENSION(0:MT,KB)     :: XFLUX,XFLUX_ADV
   REAL(SP), DIMENSION(M)           :: PSPX,PSPY,PSPXD,PSPYD,VISCOFF
!   REAL(SP), DIMENSION(3*(NT))      :: DTIJ 
   REAL(SP), DIMENSION(3*(NT),KBM1)      :: DTIJ 
   REAL(SP) :: XI,YI
   REAL(SP) :: DXA,DYA,DXB,DYB,FIJ1,FIJ2 
   REAL(SP) :: TXX,TYY,FXX,FYY,VISCOF   
   REAL(SP) :: FACT,FM1
   INTEGER  :: I,I1,I2,IA,IB,J,J1,J2,JTMP,JJ,II
   REAL(SP) :: TXPI,TYPI

   REAL(SP) :: VX_TMP,VY_TMP,VX1_TMP,VY1_TMP,VX2_TMP,VY2_TMP,VX3_TMP,VY3_TMP
   REAL(SP) :: XI_TMP,YI_TMP,VXA_TMP,VYA_TMP,VXB_TMP,VYB_TMP
   REAL(SP) :: UIJ_TMP,VIJ_TMP,DLTXE_TMP,DLTYE_TMP,UVN_TMP,EXFLUX_TMP
   REAL(SP) :: PUPX_TMP,PUPY_TMP,PVPX_TMP,PVPY_TMP
   REAL(SP) :: PSPX_TMP,PSPY_TMP,PSPXD_TMP,PSPYD_TMP
   REAL(SP) :: U_TMP,V_TMP
   REAL(SP) :: X11,Y11,X22,Y22,X33,Y33,TMP1,TMP2

   REAL(SP) :: XIJE1_TMP,YIJE1_TMP,XIJE2_TMP,YIJE2_TMP
   REAL(SP) :: S1MIN, S1MAX, S2MIN, S2MAX

   REAL(SP) :: CETA
!------------------------------------------------------------------------------!

   FACT = 0.0_SP
   FM1  = 1.0_SP
   IF(HORZMIX == 'closure') THEN
     FACT = 1.0_SP
     FM1  = 0.0_SP
   END IF

!
!--Initialize Fluxes-----------------------------------------------------------!
!
   DO II=1,NPCV
     I = NCEDGE_LST(II)
     IA = NIEC(I,1)
     IB = NIEC(I,2)
     IF(IA == NODE_NORTHPOLE)THEN
       XFLUX(IA,K) = 0.0_SP
       XFLUX_ADV(IA,K) = 0.0_SP
     ELSE IF(IB == NODE_NORTHPOLE)THEN  
       XFLUX(IB,K) = 0.0_SP
       XFLUX_ADV(IB,K) = 0.0_SP
     END IF  
   END DO  
     
!
!--Loop Over Control Volume Sub-Edges And Calculate Normal Velocity------------!
!
   DO II=1,NPCV
     I = NCEDGE_LST(II)
     I1=NTRG(I)
     DTIJ(I,K)=D1(I1)*DZ1(I1,K)
   END DO

!
!--Calculate the Advection and Horizontal Diffusion Terms----------------------!
!
   I = NODE_NORTHPOLE

   IF(I==0)  RETURN

   PUPX_TMP=0.0_SP
   PUPY_TMP=0.0_SP
   PVPX_TMP=0.0_SP
   PVPY_TMP=0.0_SP

   DO J=1,NTVE(I)
     I1=NBVE(I,J)
     JTMP=NBVT(I,J)
     J1=JTMP+1-(JTMP+1)/4*3
     J2=JTMP+2-(JTMP+2)/4*3
       
     VX_TMP = REARTH * COS(VY(I)*DEG2RAD) * COS(VX(I)*DEG2RAD) &
                     * 2._SP /(1._SP+SIN(VY(I)*DEG2RAD))
     VY_TMP = REARTH * COS(VY(I)*DEG2RAD) * SIN(VX(I)*DEG2RAD) &
                     * 2._SP /(1._SP+SIN(VY(I)*DEG2RAD))
		     
     VX1_TMP= REARTH * COS(VY(NV(I1,J1))*DEG2RAD) * COS(VX(NV(I1,J1))*DEG2RAD) &
                     * 2._SP /(1._SP+SIN(VY(NV(I1,J1))*DEG2RAD))
     VY1_TMP= REARTH * COS(VY(NV(I1,J1))*DEG2RAD) * SIN(VX(NV(I1,J1))*DEG2RAD) &
                     * 2._SP /(1._SP+SIN(VY(NV(I1,J1))*DEG2RAD))
		     
     VX2_TMP= REARTH * COS(YC(I1)*DEG2RAD) * COS(XC(I1)*DEG2RAD) &
                     * 2._SP /(1._SP+SIN(YC(I1)*DEG2RAD))
     VY2_TMP= REARTH * COS(YC(I1)*DEG2RAD) * SIN(XC(I1)*DEG2RAD) &
                     * 2._SP /(1._SP+SIN(YC(I1)*DEG2RAD))
		     
     VX3_TMP= REARTH * COS(VY(NV(I1,J2))*DEG2RAD) * COS(VX(NV(I1,J2))*DEG2RAD) &
                     * 2._SP /(1._SP+SIN(VY(NV(I1,J2))*DEG2RAD))
     VY3_TMP= REARTH * COS(VY(NV(I1,J2))*DEG2RAD) * SIN(VX(NV(I1,J2))*DEG2RAD) &
                    * 2._SP /(1._SP+SIN(VY(NV(I1,J2))*DEG2RAD))
		     
     X11=0.5_SP*(VX_TMP+VX1_TMP)
     Y11=0.5_SP*(VY_TMP+VY1_TMP)
     X22=VX2_TMP
     Y22=VX2_TMP
     X33=0.5_SP*(VX_TMP+VX3_TMP)
     Y33=0.5_SP*(VY_TMP+VY3_TMP)
     
     U_TMP = -V(I1,K)*COS(XC(I1)*DEG2RAD)-U(I1,K)*SIN(XC(I1)*DEG2RAD)
     V_TMP = -V(I1,K)*SIN(XC(I1)*DEG2RAD)+U(I1,K)*COS(XC(I1)*DEG2RAD)

     PUPX_TMP=PUPX_TMP+U_TMP*(Y11-Y33)
     PUPY_TMP=PUPY_TMP+U_TMP*(X33-X11)
     PVPX_TMP=PVPX_TMP+V_TMP*(Y11-Y33)
     PVPY_TMP=PVPY_TMP+V_TMP*(X33-X11)
   END DO

   PUPX_TMP=PUPX_TMP/ART1(I)
   PUPY_TMP=PUPY_TMP/ART1(I)
   PVPX_TMP=PVPX_TMP/ART1(I)
   PVPY_TMP=PVPY_TMP/ART1(I)
   TMP1=PUPX_TMP**2+PVPY_TMP**2
   TMP2=0.5_SP*(PUPY_TMP+PVPX_TMP)**2
   VISCOFF(I)=SQRT(TMP1+TMP2)*ART1(I)

   DO II=1,NPCV
     I = NCEDGE_LST(II)
     I1=NTRG(I)
     IA=NIEC(I,1)
     IB=NIEC(I,2)
     
     IF((IA <= M .AND. IB <= M) .AND. I1 <= N)THEN
!       XI=0.5_SP*(XIJE(I,1)+XIJE(I,2))
!       YI=0.5_SP*(YIJE(I,1)+YIJE(I,2))
       XIJE1_TMP = REARTH * COS(YIJE(I,1)*DEG2RAD) * COS(XIJE(I,1)*DEG2RAD) &
                  * 2._SP /(1._SP+sin(YIJE(I,1)*DEG2RAD))
       YIJE1_TMP = REARTH * COS(YIJE(I,1)*DEG2RAD) * SIN(XIJE(I,1)*DEG2RAD) &
                  * 2._SP /(1._SP+sin(YIJE(I,1)*DEG2RAD))

       XIJE2_TMP = REARTH * COS(YIJE(I,2)*DEG2RAD) * COS(XIJE(I,2)*DEG2RAD) &
                  * 2._SP /(1._SP+sin(YIJE(I,2)*DEG2RAD))
       YIJE2_TMP = REARTH * COS(YIJE(I,2)*DEG2RAD) * SIN(XIJE(I,2)*DEG2RAD) &
                  * 2._SP /(1._SP+sin(YIJE(I,2)*DEG2RAD))
       XI_TMP =0.5_SP*(XIJE1_TMP+XIJE2_TMP)
       YI_TMP =0.5_SP*(YIJE1_TMP+YIJE2_TMP)

       IF(IA == NODE_NORTHPOLE .OR. IB == NODE_NORTHPOLE)THEN
!         XI_TMP = REARTH * COS(YI*DEG2RAD) * COS(XI*DEG2RAD) &
!                  * 2._SP /(1._SP+sin(YI*DEG2RAD))
!         YI_TMP = REARTH * COS(YI*DEG2RAD) * SIN(XI*DEG2RAD) &
!                  * 2._SP /(1._SP+sin(YI*DEG2RAD))

         VXA_TMP = REARTH * COS(VY(IA)*DEG2RAD) * COS(VX(IA)*DEG2RAD) &
                   * 2._SP /(1._SP+sin(VY(IA)*DEG2RAD))
         VYA_TMP = REARTH * COS(VY(IA)*DEG2RAD) * SIN(VX(IA)*DEG2RAD) &
                   * 2._SP /(1._SP+sin(VY(IA)*DEG2RAD))

         VXB_TMP = REARTH * COS(VY(IB)*DEG2RAD) * COS(VX(IB)*DEG2RAD) &
                   * 2._SP /(1._SP+sin(VY(IB)*DEG2RAD))
         VYB_TMP = REARTH * COS(VY(IB)*DEG2RAD) * SIN(VX(IB)*DEG2RAD) &
                   * 2._SP /(1._SP+sin(VY(IB)*DEG2RAD))

!         IF(IA == NODE_NORTHPOLE)THEN
         DXA=XI_TMP-VXA_TMP
         DYA=YI_TMP-VYA_TMP
!         ELSE IF(IB == NODE_NORTHPOLE)THEN
         DXB=XI_TMP-VXB_TMP
         DYB=YI_TMP-VYB_TMP
!	 END IF
!       END IF

        IF(IA == NODE_NORTHPOLE)THEN
	  PSPX_TMP=-PSPY(IB)*COS(VX(IB)*DEG2RAD)-PSPX(IB)*SIN(VX(IB)*DEG2RAD)
          PSPY_TMP=-PSPY(IB)*SIN(VX(IB)*DEG2RAD)+PSPX(IB)*COS(VX(IB)*DEG2RAD)
   
	  PSPXD_TMP=-PSPYD(IB)*COS(VX(IB)*DEG2RAD)-PSPXD(IB)*SIN(VX(IB)*DEG2RAD)
          PSPYD_TMP=-PSPYD(IB)*SIN(VX(IB)*DEG2RAD)+PSPXD(IB)*COS(VX(IB)*DEG2RAD)
   
          FIJ1=S1(IA,K)+DXA*PSPX(IA)+DYA*PSPY(IA)
          FIJ2=S1(IB,K)+DXB*PSPX_TMP+DYB*PSPY_TMP

          VISCOF=HORCON*(FACT*(VISCOFF(IA)+VISCOFF(IB))*0.5_SP + FM1)

          TXX=0.5_SP*(PSPXD(IA)+PSPXD_TMP)*VISCOF
          TYY=0.5_SP*(PSPYD(IA)+PSPYD_TMP)*VISCOF
        ELSE IF(IB == NODE_NORTHPOLE)THEN
	  PSPX_TMP=-PSPY(IA)*COS(VX(IA)*DEG2RAD)-PSPX(IA)*SIN(VX(IA)*DEG2RAD)
          PSPY_TMP=-PSPY(IA)*SIN(VX(IA)*DEG2RAD)+PSPX(IA)*COS(VX(IA)*DEG2RAD)
   
	  PSPXD_TMP=-PSPYD(IA)*COS(VX(IA)*DEG2RAD)-PSPXD(IA)*SIN(VX(IA)*DEG2RAD)
          PSPYD_TMP=-PSPYD(IA)*SIN(VX(IA)*DEG2RAD)+PSPXD(IA)*COS(VX(IA)*DEG2RAD)
   
          FIJ1=S1(IA,K)+DXA*PSPX_TMP+DYA*PSPY_TMP
          FIJ2=S1(IB,K)+DXB*PSPX(IB)+DYB*PSPY(IB)

          VISCOF=HORCON*(FACT*(VISCOFF(IA)+VISCOFF(IB))*0.5_SP + FM1)

          TXX=0.5_SP*(PSPXD_TMP+PSPXD(IB))*VISCOF
          TYY=0.5_SP*(PSPYD_TMP+PSPYD(IB))*VISCOF
        END IF

       S1MIN=MINVAL(S1(NBSN(IA,1:NTSN(IA)-1),K))
       S1MIN=MIN(S1MIN, S1(IA,K))
       S1MAX=MAXVAL(S1(NBSN(IA,1:NTSN(IA)-1),K))
       S1MAX=MAX(S1MAX, S1(IA,K))
       S2MIN=MINVAL(S1(NBSN(IB,1:NTSN(IB)-1),K))
       S2MIN=MIN(S2MIN, S1(IB,K))
       S2MAX=MAXVAL(S1(NBSN(IB,1:NTSN(IB)-1),K))
       S2MAX=MAX(S2MAX, S1(IB,K))
       IF(FIJ1 < S1MIN) FIJ1=S1MIN
       IF(FIJ1 > S1MAX) FIJ1=S1MAX
       IF(FIJ2 < S2MIN) FIJ2=S2MIN
       IF(FIJ2 > S2MAX) FIJ2=S2MAX

!       IF(IA == NODE_NORTHPOLE .OR. IB == NODE_NORTHPOLE)THEN
         UIJ_TMP = -V(I1,K)*COS(XC(I1)*DEG2RAD)-U(I1,K)*SIN(XC(I1)*DEG2RAD)
         VIJ_TMP = -V(I1,K)*SIN(XC(I1)*DEG2RAD)+U(I1,K)*COS(XC(I1)*DEG2RAD)
       
         VX1_TMP = REARTH * COS(YIJE(I,1)*DEG2RAD) * COS(XIJE(I,1)*DEG2RAD)
         VY1_TMP = REARTH * COS(YIJE(I,1)*DEG2RAD) * SIN(XIJE(I,1)*DEG2RAD)

         VX2_TMP = REARTH * COS(YIJE(I,2)*DEG2RAD) * COS(XIJE(I,2)*DEG2RAD)
         VY2_TMP = REARTH * COS(YIJE(I,2)*DEG2RAD) * SIN(XIJE(I,2)*DEG2RAD)

         DLTXE_TMP = VX2_TMP-VX1_TMP
         DLTYE_TMP = VY2_TMP-VY1_TMP
       
         FXX=-DTIJ(I,K)*TXX*DLTYE_TMP
         FYY= DTIJ(I,K)*TYY*DLTXE_TMP

         UVN_TMP = VIJ_TMP*DLTXE_TMP - UIJ_TMP*DLTYE_TMP  
         EXFLUX_TMP = -UVN_TMP*DTIJ(I,K)*((1.0_SP+SIGN(1.0_SP,UVN_TMP))*FIJ2+   &
                      (1.0_SP-SIGN(1.0_SP,UVN_TMP))*FIJ1)*0.5_SP

         IF(IA == NODE_NORTHPOLE)THEN
           XFLUX(IA,K)=XFLUX(IA,K)+EXFLUX_TMP+FXX+FYY
           XFLUX_ADV(IA,K)=XFLUX_ADV(IA,K)+EXFLUX_TMP
         ELSE IF(IB == NODE_NORTHPOLE)THEN
           XFLUX(IB,K)=XFLUX(IB,K)-EXFLUX_TMP-FXX-FYY
           XFLUX_ADV(IB,K)=XFLUX_ADV(IB,K)-EXFLUX_TMP
         END IF
       END IF
     END IF  
   END DO

   RETURN
   END SUBROUTINE ADV_S_XY
!==============================================================================|
!==============================================================================|
#  endif
END MODULE MOD_NORTHPOLE

