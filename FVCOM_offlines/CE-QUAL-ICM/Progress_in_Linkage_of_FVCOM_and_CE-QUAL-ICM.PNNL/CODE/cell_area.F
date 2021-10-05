!==============================================================================|
!    This subroutine is used to calculate the area of individual               !
!    triangle based on the three vertex coordinates and also calculate         !
!    the sigma-surface area of individual control volume consisted of          !
!    triangles with a common node point                                        !
!									       !
! calculates: art(ntri)   = area of element (triangle) 			       !
! calculates: art1(nnode) = area of interior cv (for node value integration)   !
! calculates: art2(nnode) = sum area of all cells around node		       !
!==============================================================================|

   SUBROUTINE CELL_AREA 

!==============================================================================!
   USE ALL_VARS
   IMPLICIT NONE
   REAL(SP), ALLOCATABLE :: XX(:),YY(:) 
   REAL(SP) :: ARTMAX,ARTTOT,ARTMIN
   INTEGER  :: I,J,II,J1,J2,MAX_NBRE
!==============================================================================!

!---------------INITIALIZE ARRAYS----------------------------------------------!

   ALLOCATE(ART(0:NTLOC))           ;ART  = ZERO   !!AREA OF ELEMENT
   ALLOCATE(ART1(0:MTLOC))          ;ART1 = ZERO   !!AREA OF NODE-BASE CONTROl VOLUME
   ALLOCATE(ART2(MTLOC))            ;ART2 = ZERO   !!AREA OF ELEMENTSAROUND NODE
   
   MAX_NBRE = MAXVAL(NTVE)+1
   ALLOCATE(XX(2*MAX_NBRE+1),YY(2*MAX_NBRE+1))
   XX = 0.0_SP ; YY = 0.0_SP

!---------------COMPUTE AREA OF TRIANGLES USING CROSS PRODUCT------------------!
  
    DO I=1,NTLOC 
    ART(I) = (VX(NV(I,2)) - VX(NV(I,1))) * (VY(NV(I,3)) - VY(NV(I,1))) - &
             (VX(NV(I,3)) - VX(NV(I,1))) * (VY(NV(I,2)) - VY(NV(I,1)))
    END DO
   ART    = ABS(.5_SP*ART)

!---------------COMPUTE MESH STATISTICS----------------------------------------!

   ARTMIN = MINVAL(ART(1:NLOC))
   ARTMAX = MAXVAL(ART(1:NLOC))
   ARTTOT =    SUM(ART(1:NLOC))

!-------COMPUTE CONTROL VOLUME ART1: CV FOR FLUXES OF NODAL BASED VALUES-------!

   DO I=1,MLOC
     IF(ISONB(I) == 0) THEN
       DO J=1,NTVE(I)
         II=NBVE(I,J)
         J1=NBVT(I,J)
         J2=J1+1-INT((J1+1)/4)*3
         XX(2*J-1)=(VX(NV(II,J1))+VX(NV(II,J2)))*0.5_SP-VX(I)
         YY(2*J-1)=(VY(NV(II,J1))+VY(NV(II,J2)))*0.5_SP-VY(I)
         XX(2*J)=XC(II)-VX(I)
         YY(2*J)=YC(II)-VY(I)
       END DO
       XX(2*NTVE(I)+1)=XX(1)
       YY(2*NTVE(I)+1)=YY(1)

       DO J=1,2*NTVE(I)
          ART1(I)=ART1(I)+0.5_SP*(XX(J+1)*YY(J)-XX(J)*YY(J+1))
       END DO
       ART1(I)=ABS(ART1(I))
     ELSE
       DO J=1,NTVE(I)
         II=NBVE(I,J)
         J1=NBVT(I,J)
         J2=J1+1-INT((J1+1)/4)*3
         XX(2*J-1)=(VX(NV(II,J1))+VX(NV(II,J2)))*0.5_SP-VX(I)
         YY(2*J-1)=(VY(NV(II,J1))+VY(NV(II,J2)))*0.5_SP-VY(I)
         XX(2*J)=XC(II)-VX(I)
         YY(2*J)=YC(II)-VY(I)
       END DO
       J=NTVE(I)+1
       II=NBVE(I,J-1)
       J1=NBVT(I,NTVE(I))
       J2=J1+2-INT((J1+2)/4)*3

       XX(2*J-1)=(VX(NV(II,J1))+VX(NV(II,J2)))*0.5_SP-VX(I)
       YY(2*J-1)=(VY(NV(II,J1))+VY(NV(II,J2)))*0.5_SP-VY(I)

       XX(2*J)=VX(I)-VX(I)
       YY(2*J)=VY(I)-VY(I)

       XX(2*J+1)=XX(1)
       YY(2*J+1)=YY(1)

       DO J=1,2*NTVE(I)+2
        ART1(I)=ART1(I)+0.5_SP*(XX(J+1)*YY(J)-XX(J)*YY(J+1))
       END DO
       ART1(I)=ABS(ART1(I))
     END IF
   ENDDO

!---COMPUTE AREA OF CONTROL VOLUME ART2(I) = SUM(ALL TRIS SURROUNDING NODE I)--!

   DO I=1,MLOC
     ART2(I) = SUM(ART(NBVE(I,1:NTVE(I))))
   END DO

   ART(0) = ART(1) 
   ART1(0) = ART1(1) 
!   IF(NTLOC > NLOC)ART(NLOC+1:NTLOC) = ART(NLOC)
   IF(MTLOC > MLOC)ART2(MLOC+1:MTLOC) = ART2(MLOC)
   IF(MTLOC > MLOC)ART1(MLOC+1:MTLOC) = ART1(MLOC)
   DEALLOCATE(XX,YY)


   RETURN
   END SUBROUTINE CELL_AREA
!==============================================================================|
