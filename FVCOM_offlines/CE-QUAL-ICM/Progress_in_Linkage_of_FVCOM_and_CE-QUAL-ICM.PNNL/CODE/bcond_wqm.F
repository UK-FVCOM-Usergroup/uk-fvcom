!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

   SUBROUTINE BCOND_WQM     
!==============================================================================|
!   Set Boundary Conditions on Water Quality                                   |
!==============================================================================|

!------------------------------------------------------------------------------|
   USE SIZES
   USE ALL_VARS
   USE BCS
   USE WQM, ONLY : C2,C2F,NAC,AC
   USE MOD_OBCS
   IMPLICIT NONE
   INTEGER  :: I,J,K,J1,J11,J22,NCON2,JC,II
   REAL     :: UFACT,FACT
   REAL     :: T2D,T2D_NEXT,T2D_OBC,XFLUX2D,TMP,TMAX,TMIN
   INTEGER  :: L1,L2,IERR
   REAL(SP) :: CC,CP,PERT,PERT_NEXT
   REAL(SP), ALLOCATABLE :: TTMP(:,:)
!------------------------------------------------------------------------------|


!
!--SET CONDITIONS FOR FRESH WATER INFLOW---------------------------------------|
!
!   Print*, 'calling fresh water inflow in bcond_wqm'
!   Print*, 'NUMQBC', NUMQBC
   IF(NUMQBC > 0)THEN
     ALLOCATE(WDIS(NUMQBC,NCP))     ;WDIS   = ZERO
!    Print*, 'allocating wdis'
     CALL BRACKET(QBC_TM,THOUR,L1,L2,FACT,UFACT,IERR)
      WDIS(:,:) = DWDIS(:,:,L1)      !UFACT*DWDIS(:,L1) + FACT*DWDIS(:,L2)
   END IF
!   Print*, 'after fresh water inflow'
   IF(POINT_ST_TYPE == 'specified') THEN
     IF(NUMQBC > 0) THEN
       IF(INFLOW_TYPE == 'node') THEN
         DO JC =1, NAC
	   II = AC(JC)
	   DO K=1,KBM1
	     DO I=1,NUMQBC
               J11=INODEQ(I)
               C2F(J11,K,II) = WDIS(I,II)
             END DO
           END DO
         END DO
       ELSE IF(INFLOW_TYPE == 'edge') THEN
         DO JC = 1,NAC
	   II = AC(JC)
	   DO K=1,KBM1
	     DO I=1,NUMQBC
               J11=N_ICELLQ(I,1)
               J22=N_ICELLQ(I,2)
               C2F(J11,K,II)=WDIS(I,II)
               C2F(J22,K,II)=WDIS(I,II)
             END DO
           END DO
         END DO
       END IF
     END IF
   END IF
!   Print*, 'just after inflow type end'
      DEALLOCATE(WDIS)
!   Print*, 'IOBCN before water quality at bound', IOBCN    
   IF(IOBCN > 0) THEN
!
!  SET WATER QUALITY CONDITIONS ON OUTER BOUNDARY
!
! RGl added print
!      Print*, 'nobt, iobcn in bcond_wqm', nobty, iobcn
     DO JC=1,NAC
       II = AC(JC)
       ALLOCATE(TTMP(NOBTY,KBM1)); TTMP=0.0_SP
       DO I=1,IOBCN
         J=I_OBC_N(I)
         J1=NEXT_OBC(I)
         T2D=0.0_SP
         T2D_NEXT=0.0_SP
         XFLUX2D=0.0_SP
         DO K=1,KBM1
            T2D=T2D+C2(J,K,II)*DZ(K)
            T2D_NEXT=T2D_NEXT+C2F(J1,K,II)*DZ(K)
            XFLUX2D=XFLUX2D+XFLUX_OBC(I,K)*DZ(K)
         ENDDO

!         if(II.eq.27) write(2238,*)I,UARD_OBCN(I)
         IF(UARD_OBCN(I) > 0.0) THEN
            TMP=XFLUX2D+T2D*UARD_OBCN(I)
            T2D_OBC=(T2D*DT(J)-TMP*DTI/ART1(J))/D(J)
!          if(II.eq.27) write(2239,*)J,J1,DT(J),(DZ(K),K=1,KBM1),D(J),DTI
!             DTI=DTE*FLOAT(ISPLIT) -- In FVCOM 

!            CALL BCOND_NUT_PERTURBATION(T2D_NEXT,T2D,TTMP,I,J,J1)
! Perturbation case 1 TTMP(I,K) = TF1(J1,K) - T2D_NEXT
            DO K=1,KBM1
                C2F(J,K,II)=T2D_OBC+(C2F(J1,K,II)-T2D_NEXT)
             ENDDO
! Perturbation case 3
!            CC = SQRT(GRAV*H(J))*DTI/DLTN_OBC(I)
!            CP = CC + 1.0_SP
!            DO K=1,KBM1
!               PERT_NEXT = C2F(J1,K,II) - T2D_NEXT
!               PERT      = C2(J,K,II)  - T2D
!               C2F(J,K,II)= T2D_OBC + (CC*PERT_NEXT + PERT*(1.0_SP - DTI/10800.0_SP))/CP
!            ENDDO
! The end of Perturbation

            DO K=1,KBM1
               TMAX = MAXVAL(C2(NBSN(J,1:NTSN(J)),K,II))
               TMIN = MINVAL(C2(NBSN(J,1:NTSN(J)),K,II))

           IF(K == 1)THEN
            TMAX = MAX(TMAX,(C2(J,K,II)*DZ(K+1)+C2(J,K+1,II)*DZ(K))/(DZ(K)+DZ(K+1)))
            TMIN = MIN(TMIN,(C2(J,K,II)*DZ(K+1)+C2(J,K+1,II)*DZ(K))/(DZ(K)+DZ(K+1)))
           ELSE IF(K == KBM1)THEN
            TMAX = MAX(TMAX,(C2(J,K,II)*DZ(K-1)+C2(J,K-1,II)*DZ(K))/(DZ(K)+DZ(K-1)))
            TMIN = MIN(TMIN,(C2(J,K,II)*DZ(K-1)+C2(J,K-1,II)*DZ(K))/(DZ(K)+DZ(K-1)))
           ELSE
            TMAX = MAX(TMAX,(C2(J,K,II)*DZ(K-1)+C2(J,K-1,II)*DZ(K))/(DZ(K)+DZ(K-1)), &
                   (C2(J,K,II)*DZ(K+1)+C2(J,K+1,II)*DZ(K))/(DZ(K)+DZ(K+1)))
            TMIN = MIN(TMIN,(C2(J,K,II)*DZ(K-1)+C2(J,K-1,II)*DZ(K))/(DZ(K)+DZ(K-1)), &
                   (C2(J,K,II)*DZ(K+1)+C2(J,K+1,II)*DZ(K))/(DZ(K)+DZ(K+1)))
           END IF

           IF(TMIN-C2F(J,K,II) > 0.0_SP) C2F(J,K,II) = TMIN
           IF(C2F(J,K,II)-TMAX > 0.0_SP) C2F(J,K,II) = TMAX

!           IF(TMIN-C2F(J,K,II) > C2F(J,K,II)*0.0001) C2F(J,K,II) = TMIN
!           IF(C2F(J,K,II)-TMAX > C2F(J,K,II)*0.0001) C2F(J,K,II) = TMAX

           END DO
	 
        ELSE
           DO K=1,KBM1
              C2F(J,K,II) = C2(J,K,II)
           ENDDO

!#          if defined (TS_OBC)
!              IF(IERR.NE.-1)THEN
!                if(II.eq.27) then
!                   C2F(J,K,II)=C2(J,K,II)-0.014*1.0*(C2(J,K,II)-7.0)
!                   write(2249,*)J,K,II,IERR
!                endif
!!                C2F(J,K,II)=C2(J,K,II)-ALPHA_SERIES_OBC*(C2(J,K,II)-NUT_OBC(I,K))
!              ELSE
!                C2F(J,K,II) = C2(J,K,II)
!              ENDIF
!#          else 
!              if(II.eq.27) C2F(J,K,II) = C2(J,K,II) - 0.014*1.0*(C2(J,K,II)-7.0)
!#          endif


        END IF
       END DO
       DEALLOCATE(TTMP)
      END DO !!OUTER LOOP OVER WQ VARIABLES
   END IF

!
!--SET BOUNDARY CONDITIONS-----------------------------------------------------|
!
   C2F(0,:,:) = 0.0

   RETURN
   END SUBROUTINE BCOND_WQM

!==============================================================================|
 
!==============================================================================|
   SUBROUTINE BCOND_NUT_PERTURBATION(T2D_NEXT,T2D,TTMP,I,J,J1)
!==============================================================================|
! Calculate the OBC for temperature perturbation                               |
!==============================================================================|
   USE SIZES
   USE ALL_VARS
   IMPLICIT NONE

!   INTEGER :: I1,I2,J,JN
   INTEGER :: I,J,J1,J2,K
   REAL(SP):: CC,CP,MU,CL
   REAL(SP):: PERT_NEXT,PERT,T2D_NEXT,T2D
   REAL(SP):: T2D_NEXT1,TM12D_NEXT2,TM12D_NEXT1,TM22D_NEXT1
   REAL(SP):: TTMP(NOBTY,KBM1)

!   SELECT CASE(TYPE_TSOBC)
   
!   CASE(1)
!     DO K=1,KBM1
!       TTMP(I,K) = TF1(J1,K) - T2D_NEXT
!     END DO  
!   CASE(2)
!     CC = SQRT(GRAV*H(J))*DTI/DLTN_OBC(I)
!     CP = CC + 1.0_SP
!     DO K=1,KBM1
!       PERT_NEXT = TF1(J1,K) - T2D_NEXT
!       PERT      = T1(J,K) - T2D
!       TTMP(I,K) = (CC*PERT_NEXT + PERT)/CP
!     END DO
!   CASE(3)
!     CC = SQRT(GRAV*H(J))*DTI/DLTN_OBC(I)
!     CP = CC + 1.0_SP
!     DO K=1,KBM1
!       PERT_NEXT = TF1(J1,K) - T2D_NEXT
!       PERT      = T1(J,K) - T2D
!       TTMP(I,K) = (CC*PERT_NEXT + PERT*(1.0_SP - DTI/10800.0_SP))/CP
!     END DO
!   CASE(4)
!     J2 = NEXT_OBC2(I)
!     T2D_NEXT1  =0.0_SP
!     TM12D_NEXT2=0.0_SP
!     TM12D_NEXT1=0.0_SP
!     TM22D_NEXT1=0.0_SP
!     DO K=1,KBM1
!       T2D_NEXT1  =T2D_NEXT1  +T1(J1,K)*DZ(K)
!       TM12D_NEXT2=TM12D_NEXT2+T1M1(J2,K)*DZ(K)
!       TM12D_NEXT1=TM12D_NEXT1+T1M1(J,K)*DZ(K)
!       TM22D_NEXT1=TM22D_NEXT1+T1M2(J1,K)*DZ(K)
!     END DO
!
!     DO K=1,KBM1
!       CL = ((T1M2(J1,K)-TM22D_NEXT1)-(T1(J1,K)-T2D_NEXT1))/   &
!            ((T1(J1,K)-T2D_NEXT1)+(T1M2(J1,K)-TM22D_NEXT1)     &
!	     -2.0*(T1M1(J2,K)-TM12D_NEXT2))
!       IF(CL >= 1.0)THEN
!         MU = 1.0
!       ELSE IF(CL > 0.0 .AND. CL < 1.0)THEN
!         MU = CL
!       ELSE
!         MU = 0.0
!       END IF

!       TTMP(I,K)=((T1M1(J,K)-TM12D_NEXT1)*(1.0-MU)     &
!                 +2.0*MU*(T1(J1,K)-T2D_NEXT1))/(1.0+MU)
!     END DO

!   END SELECT
     
   RETURN
   END SUBROUTINE BCOND_NUT_PERTURBATION
!========================================================================
