!==============================================================================|
!  MODULE CONTAINING SUBROUTINES USED TO SET UP SPHERICAL COORDINATE SYSTEM    |
!  FLUXES                                                                      |
!==============================================================================|

MODULE MOD_SPHERICAL
# if defined (SPHERICAL)
   USE CONTROL
   USE MOD_PREC
   IMPLICIT NONE
   SAVE
   REAL(SP), ALLOCATABLE :: DLTXNE(:,:)
   REAL(SP), ALLOCATABLE :: DLTYNE(:,:)
   
   REAL(SP), ALLOCATABLE :: DELTUX(:,:)
   REAL(SP), ALLOCATABLE :: DELTUY(:,:)
   REAL(SP), ALLOCATABLE :: SITAU(:,:)
!===================================================================================|
   CONTAINS   !!INCLUDED SUBROUTINES FOLLOW
!===================================================================================|
   
   SUBROUTINE ARC(XX1,YY1,XX2,YY2,ARCL)
!----------------------------------------------------------------------------
!      function:
!           calculate the arc lenth for given two point on the spherical plane
!      input:
!           xx1,yy1,xx2,yy2 :are longitude and latitude of two points
!      output:
!           arcl :  arc lenth of two points in spherical plane
!-----------------------------------------------------------------------------       
       
!  solve the arc length through the earth center
   IMPLICIT NONE
   REAL(DP) :: X1,Y1,X2,Y2,XA,YA,ZA,XB,YB,ZB,AB,AOB,ARCL
   REAL(DP) :: XX1,YY1,XX2,YY2

   X1=XX1*DEG2RAD
   Y1=YY1*DEG2RAD

   X2=XX2*DEG2RAD
   Y2=YY2*DEG2RAD

   XA=COS(Y1)*COS(X1)
   YA=COS(Y1)*SIN(X1)
   ZA=SIN(Y1)

   XB=COS(Y2)*COS(X2)
   YB=COS(Y2)*SIN(X2)
   ZB=SIN(Y2)

   AB=SQRT((XB-XA)**2+(YB-YA)**2+(ZB-ZA)**2)
   AOB=(2.-AB*AB)/2.
   AOB=ACOS(AOB)
   ARCL=REARTH*AOB

   RETURN
   END SUBROUTINE ARC



   SUBROUTINE AREA(SIDE1,SIDE2,SIDE3,AREA1)
!--------------------------------------------------------------------
!      function:
!           calculate the area of a triangle on a spherical plane
!      input:
!           side1,side2 and side3: are 3 arc lenth for one triangle
!      output:
!           areal: is area of a triangle on a spherical plane
!--------------------------------------------------------------------
   IMPLICIT NONE
   REAL(DP) :: SIDE1,SIDE2,SIDE3,AREA1
   REAL(DP) :: PSUM,PM,QMJC

   SIDE1=SIDE1/REARTH
   SIDE2=SIDE2/REARTH
   SIDE3=SIDE3/REARTH
   IF(SIDE1 == 0. .OR. SIDE2 == 0. .OR. SIDE3 == 0.)THEN
     AREA1=0.
   ELSE
     PSUM=0.5*(SIDE1+SIDE2+SIDE3)
     PM=SIN(PSUM)*SIN(PSUM-SIDE1)*SIN(PSUM-SIDE2)*SIN(PSUM-SIDE3)
     PM=SQRT(PM)/(2.0*COS(SIDE1*0.5)*COS(SIDE2*0.5)*COS(SIDE3*0.5))
     QMJC = 2.0*ASIN(PM)

     AREA1=REARTH*REARTH*QMJC

   END IF

   RETURN
   END SUBROUTINE AREA

   SUBROUTINE ARCC(XX1,YY1,XX2,YY2,XXC,YYC)
   IMPLICIT NONE
   REAL(DP) :: XXC,YYC,XX1,YY1,XX2,YY2
   REAL(DP) :: X1,Y1,X2,Y2

   X1=XX1*DEG2RAD
   Y1=YY1*DEG2RAD

   X2=XX2*DEG2RAD
   Y2=YY2*DEG2RAD

   IF(ABS(YY1-90.0_SP) < 1.0E-6_SP)THEN
     XXC = XX2
   ELSE IF(ABS(YY2-90.0_SP) < 1.0E-6_SP)THEN
     XXC = XX1
   ELSE
   XXC=DCOS(Y1)*DSIN(X1)+DCOS(Y2)*DSIN(X2)
!   XXC=XXC/(COS(Y1)*COS(X1)+COS(Y2)*COS(X2))
!   XXC=ATAN(XXC)
   XXC=DATAN2(XXC,(DCOS(Y1)*DCOS(X1)+DCOS(Y2)*DCOS(X2)))
   XXC=XXC/DEG2RAD

!   IF(XXC .LT. 0.0) XXC=180.0+XXC
     IF(XXC < 0.0) XXC=360.0+XXC
   END IF

   YYC=DCOS(Y1)*DCOS(Y1)+DCOS(Y2)*DCOS(Y2)+2.*DCOS(Y1)*DCOS(Y2)*DCOS(X1-X2)
!   YYC=SQRT(YYC)/(SIN(Y1)+SIN(Y2))
   YYC=DATAN2(DSQRT(YYC),(DSIN(Y1)+DSIN(Y2)))
!   YYC=ATAN(YYC)
   YYC=90.-YYC/DEG2RAD

   RETURN
   END SUBROUTINE ARCC


   SUBROUTINE ARCX(XX1,YY1,XX2,YY2,ARCX1)

   IMPLICIT NONE
   INTEGER I,NX
   PARAMETER(NX=500)
   REAL(DP) :: XX1,YY1,XX2,YY2,ARCX1
   REAL(DP) :: X1,Y1,X2,Y2,TY
   REAL(DP) :: XTMP	      

   IF(XX1 == XX2)THEN
     ARCX1=0.
   ELSE
     X1=XX1*DEG2RAD
     Y1=YY1*DEG2RAD

     X2=XX2*DEG2RAD
     Y2=YY2*DEG2RAD

     XTMP  = X2-X1
     IF(XTMP >  PI)THEN
       XTMP = -2*PI+XTMP
     ELSE IF(XTMP < -PI)THEN
       XTMP =  2*PI+XTMP
     END IF  

     TY=0.5*(Y2+Y1)
     ARCX1=REARTH*COS(TY)*XTMP
   END IF
   
   RETURN
   END SUBROUTINE ARCX

   SUBROUTINE ARCX_BACK(XX1,YY1,XX2,YY2,ARCX1)

   IMPLICIT NONE
   INTEGER I,NX
   PARAMETER(NX=500)
   REAL(DP) :: XX1,YY1,XX2,YY2,ARCX1
   REAL(DP) :: X1,Y1,X2,Y2,TY,A1,A2,B1,B2,C1,C2,A,B,C,X(NX+1),Y(NX+1)
   REAL(DP) :: XTMP	      

   IF(XX1 == XX2)THEN
     ARCX1=0.
   ELSE
     X1=XX1*DEG2RAD
     Y1=YY1*DEG2RAD

     X2=XX2*DEG2RAD
     Y2=YY2*DEG2RAD

     X(1)=X1
     Y(1)=Y1
     X(NX+1)=X2
     Y(NX+1)=Y2

     XTMP=X(NX+1)-X(1)
     IF(XTMP >  PI)THEN
       XTMP = -2*PI+XTMP
     ELSE IF(XTMP < -PI)THEN
       XTMP =  2*PI+XTMP
     END IF  

     DO I=2,NX
       X(I)=X(I-1)+XTMP/FLOAT(NX)
!       x(i)=x(i-1)+(x(nx+1)-x(1))/float(nx)
     END DO

     A1=COS(Y(1))*COS(X(1))
     A2=COS(Y(NX+1))*COS(X(NX+1))

     B1=COS(Y(1))*SIN(X(1))
     B2=COS(Y(NX+1))*SIN(X(NX+1))

     C1=SIN(Y(1))
     C2=SIN(Y(NX+1))

     A=A1*B2-A2*B1
     B=B1*C2-B2*C1
     C=A2*C1-A1*C2

     DO I=2,NX
       Y(I)=-B*COS(X(I))-C*SIN(X(I))
       Y(I)=Y(I)/A
       Y(I)=ATAN(Y(I))
     END DO

     ARCX1=0.
     DO I=1,NX
       TY=0.5*(Y(I)+Y(I+1))
       XTMP=X(I+1)-X(I)
       IF(XTMP >  PI)THEN
         XTMP = -2*PI+XTMP
       ELSE IF(XTMP < -PI)THEN
         XTMP =  2*PI+XTMP
       END IF  
       ARCX1=ARCX1+REARTH*COS(TY)*XTMP
!       arcx1=arcx1+rearth*cos(ty)*(x(i+1)-x(i))
     END DO
   END IF

   RETURN
   END SUBROUTINE ARCX_BACK

   SUBROUTINE ALLOC_SPHERE_VARS
   USE LIMS
   INTEGER NCT
   NCT = NT*3
   ALLOCATE(DLTXNE(NCT,2))       ;DLTXNE    = ZERO
   ALLOCATE(DLTYNE(NCT,2))       ;DLTYNE    = ZERO
   ALLOCATE(DELTUX(NT,3))        ;DELTUX    = ZERO
   ALLOCATE(DELTUY(NT,3))        ;DELTUY    = ZERO
   ALLOCATE(SITAU(NT,3))         ;SITAU     = ZERO
   RETURN
   END SUBROUTINE ALLOC_SPHERE_VARS

   SUBROUTINE CAL_CENTER
   USE ALL_VARS
   IMPLICIT NONE
   REAL(SP) :: X11,Y11,X22,Y22,X33,Y33,TMP1,TMP2,XI,YI
   REAL(DP) :: TXPI,TYPI
   REAL(DP) :: XTMP1,XTMP
   REAL(DP) :: X1_DP,Y1_DP,X2_DP,Y2_DP
   REAL(DP) :: X11_TMP,Y11_TMP,X33_TMP,Y33_TMP,XII,YII
   INTEGER  :: I,I1,IA,IB,J,J1,J2,K,JTMP

     DO I=1,M
!-----Calculate the Cosine Value of the Latitude of Nodes----------------------
        VAL_COS_VY(I)=COS(DEG2RAD*VY(I))
!-------------------------------------------------------------------------------
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

       X1_DP=VX(I)
       Y1_DP=VY(I)
       X2_DP=VX(NV(I1,J1))
       Y2_DP=VY(NV(I1,J1))
       CALL ARCC(X2_DP,Y2_DP,X1_DP,Y1_DP,X11_TMP,Y11_TMP)
       XCA(I)=X11_TMP
	 YCA(I)=Y11_TMP
       X11=X11_TMP
       Y11=Y11_TMP
       X2_DP=VX(NV(I1,J2))
       Y2_DP=VY(NV(I1,J2))
       CALL ARCC(X2_DP,Y2_DP,X1_DP,Y1_DP,X33_TMP,Y33_TMP)
	 XCB(I)=X33_TMP
	 YCB(I)=Y33_TMP
       X33=X33_TMP
       Y33=Y33_TMP

       XTMP  = X33*TPI-X11*TPI
       XTMP1 = X33-X11
       IF(XTMP1 >  180.0_SP)THEN
         XTMP = -360.0_SP*TPI+XTMP
       ELSE IF(XTMP1 < -180.0_SP)THEN
         XTMP =  360.0_SP*TPI+XTMP
       END IF	 
       TXPI=XTMP*COS(DEG2RAD*VY(I))
       TYPI=(Y11-Y33)*TPI

       IF(ISONB(I) /= 0) THEN
         XTMP  = X11*TPI-VX(I)*TPI
         XTMP1 = X11-VX(I)
         IF(XTMP1 >  180.0_SP)THEN
	   XTMP = -360.0_SP*TPI+XTMP
         ELSE IF(XTMP1 < -180.0_SP)THEN
	   XTMP =  360.0_SP*TPI+XTMP
         END IF  
         TXPI=XTMP*COS(DEG2RAD*VY(I))
         TYPI=(VY(I)-Y11)*TPI
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

         X1_DP=VX(I)
         Y1_DP=VY(I)
         X2_DP=VX(NV(I1,J1))
         Y2_DP=VY(NV(I1,J1))
         CALL ARCC(X2_DP,Y2_DP,X1_DP,Y1_DP,X11_TMP,Y11_TMP)
	   XCC(I,J)=X11_TMP
	   YCC(I,J)=Y11_TMP
	   X11=X11_TMP
	   Y11=Y11_TMP
         X2_DP=VX(NV(I1,J2))
         Y2_DP=VY(NV(I1,J2))
         CALL ARCC(X2_DP,Y2_DP,X1_DP,Y1_DP,X33_TMP,Y33_TMP)
	   XCD(I,J)=X33_TMP
	   YCD(I,J)=Y33_TMP
	   X33=X33_TMP
	   Y33=Y33_TMP
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

       X1_DP=VX(I)
       Y1_DP=VY(I)
       X2_DP=VX(NV(I1,J1))
       Y2_DP=VY(NV(I1,J1))
       CALL ARCC(X2_DP,Y2_DP,X1_DP,Y1_DP,X11_TMP,Y11_TMP)
       XCE(I)=X11_TMP
       YCE(I)=Y11_TMP
       X11=X11_TMP
       Y11=Y11_TMP
       X2_DP=VX(NV(I1,J2))
       Y2_DP=VY(NV(I1,J2))
       CALL ARCC(X2_DP,Y2_DP,X1_DP,Y1_DP,X33_TMP,Y33_TMP)
       XCF(I)=X33_TMP
       YCF(I)=Y33_TMP
     END DO 

     ALLOCATE(XCG2(NCV_I))         ;XCG2      = ZERO
     ALLOCATE(YCG2(NCV_I))         ;YCG2      = ZERO
      
     DO I=1,NCV_I
       IA=NIEC(I,1)
       IB=NIEC(I,2)
       XI=0.5_SP*(XIJE(I,1)+XIJE(I,2))
       YI=0.5_SP*(YIJE(I,1)+YIJE(I,2))
       X1_DP=XIJE(I,1)
       Y1_DP=YIJE(I,1)
       X2_DP=XIJE(I,2)
       Y2_DP=YIJE(I,2)
       CALL ARCC(X2_DP,Y2_DP,X1_DP,Y1_DP,XII,YII)
       XCG2(I)=XII
       YCG2(I)=YII
     END DO
   RETURN
   END SUBROUTINE CAL_CENTER
   
# endif   
END MODULE  MOD_SPHERICAL
