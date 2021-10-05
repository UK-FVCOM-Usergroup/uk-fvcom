!==============================================================================|
!   READ IN STATIC WATER DEPTH AND CALCULATE RELATED QUANTITIES                |
!                                                                              |
!   INPUTS: H(NNODE) BATHYMETRIC DEPTH AT NODES				       |
!   INITIALIZES: D(NNODE) DEPTH AT NODES				       |
!   INITIALIZES: DT(NNODE) ???					               |
!   INITIALIZES: H1(NNODE) BATHYMETRIC DEPTH AT ELEMENTS		       |
!   INITIALIZES: D1(NNODE) DEPTH AT NODES                		       | 
!   INITIALIZES: DT1(NNODE) ??                                   	       |
!==============================================================================|

   SUBROUTINE WATER_DEPTH         

!------------------------------------------------------------------------------|
   USE ALL_VARS
   USE MOD_OBCS
   IMPLICIT NONE
   REAL(SP) :: TEMP
   INTEGER  :: I,K,J1,J2
!------------------------------------------------------------------------------|

!
!  ADJUST STATIC HEIGHT AND CALCULATE DYNAMIC DEPTHS (D) AND (DT)
!
   D  = H + EL

!
!  ADJUST HEIGHT ON OUTER BOUNDARY
!
   IF(IOBCN > 0) THEN
     DO I = 1,IOBCN
       J1 = I_OBC_N(I)
       J2 = NEXT_OBC(I)
       H(J1) = H(J2)
       D(J1) = D(J2)
       
       DO K = 1,KB
         Z(J1,K)   = Z(J2,K)
         ZZ(J1,K)  = ZZ(J2,K)
         DZ(J1,K)  = DZ(J2,K)
         DZZ(J1,K) = DZZ(J2,K)
       END DO	 
     END DO
   END IF
          
! 
!  CALCULATE FACE-CENTERED VALUES OF BATHYMETRY AND DEPTH
!
   DO I=1,NT    
     H1(I)  = (H(NV(I,1))+H(NV(I,2))+H(NV(I,3)))/3.0_SP
     D1(I)  = H1(I)+EL1(I)
     
     DO K = 1,KB
       Z1(I,K)=(Z(NV(I,1),K)+Z(NV(I,2),K)+Z(NV(I,3),K))/3.0_SP
     END DO  
   END DO

!-----COMPUTE SIGMA DERIVATIVES AND INTRA SIGMA LEVELS AGAIN ON CELL----------!
   DO K=1,KB-1
     DO I=1,NT
       DZ1(I,K)  = Z1(I,K)-Z1(I,K+1)
       ZZ1(I,K)  = .5_SP*(Z1(I,K)+Z1(I,K+1))
     END DO
   END DO

   DO I=1,NT
     ZZ1(I,KB) = 2.0_SP*ZZ1(I,KB-1)-ZZ1(I,KB-2)
   END DO

   DO K=1,KBM2
     DO I=1,NT
       DZZ1(I,K) = ZZ1(I,K)-ZZ1(I,K+1)
     END DO
   END DO
   
   DZZ1(:,KBM1) = 0.0_SP
   DZ1(:,KB)    = 0.0_SP

   RETURN
   END SUBROUTINE WATER_DEPTH
!==============================================================================|
