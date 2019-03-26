MODULE MOD_TVD

   USE MOD_PREC
   IMPLICIT NONE
   SAVE
!
!--Parameters for TVD advection                 
!
   REAL(SP), ALLOCATABLE :: DELF(:)  

   REAL(SP), ALLOCATABLE :: Anear_node(:), Bnear_node(:)
   REAL(SP), ALLOCATABLE :: YUAdist(:), YUBdist(:), XUAdist(:), XUBdist(:)

CONTAINS

!==============================================================================!
  SUBROUTINE SETUP_TVD
    USE ALL_VARS
    USE LIMS
    IMPLICIT NONE
    REAL(SP), DIMENSION(1:M)             :: dstA,dstB
    REAL(SP), DIMENSION(1:NCV)        :: XUA,XUB,YUA,YUB
    REAL(SP) :: LX,LY,minA,minB
    INTEGER  :: I,I2,IA,IB

    ALLOCATE(Anear_node(1:NCV))   ; Anear_node     = ZERO
    ALLOCATE(Bnear_node(1:NCV))   ; Bnear_node     = ZERO
    ALLOCATE(XUAdist(1:NCV))      ; XUAdist    = ZERO
    ALLOCATE(YUAdist(1:NCV))      ; YUAdist    = ZERO
    ALLOCATE(XUBdist(1:NCV))      ; XUBdist    = ZERO
    ALLOCATE(YUBdist(1:NCV))      ; YUBdist    = ZERO



  ! --------------------- Calculate TVD parameters ----------------------------- !
  DO I=1,NCV
      IA = NIEC(I,1) ! Noden til venstre for kontrollvolum-kanten
      IB = NIEC(I,2) ! Noden til høyre for kontrollvolum-kanten

      LX = VX(IB)-VX(IA) ! X-avstanden mellom IA og IB
      LY = VY(IB)-VY(IA) ! Y-avstanden mellom IA og IB

  ! Om A er oppstrøm
      XUA(I) = VX(IA)-LX ! x-lokasjonen oppstrøm for IA
      YUA(I) = VY(IA)-LY ! y-lokasjonen oppstrøm for IA

  ! Om B er oppstrøm
      XUB(I) = VX(IB)+LX ! x-lokasjonen oppstrøm for IB
      YUB(I) = VY(IB)+LY ! y-lokasjonen oppstrøm for IB
 
  ! Finner avstanden mellom oppstrøms-punktene og alle nodene i domenet
         DO I2=1,M
          dstA(I2)=SQRT((VX(I2)-XUA(I))**2.0_SP+(VY(I2)-YUA(I))**2.0_SP)
          dstB(I2)=SQRT((VX(I2)-XUB(I))**2.0_SP+(VY(I2)-YUB(I))**2.0_SP)
         END DO

         minA = MINVAL(dstA)
         minB = MINVAL(dstB)

         DO I2=1,M
           if (dstA(I2).EQ.minA) then
              Anear_node(I) = I2
           end if
           if (dstB(I2).EQ.minB) then
              Bnear_node(I) = I2
           end if
         END DO

  ! Lagrer avstanden fra oppstrømsnoden til den nærmeste noden
  ! ------------------------------------------------------------
  ! A-noden oppstrøm
      XUAdist(I) = XUA(I) - VX(Anear_node(I))
      YUAdist(I) = YUA(I) - VY(Anear_node(I))

  ! B-noden oppstrøm
      XUBdist(I) = XUB(I) - VX(Bnear_node(I))
      YUBdist(I) = YUB(I) - VY(Bnear_node(I))
  END DO 

   RETURN
   END SUBROUTINE SETUP_TVD

END MODULE MOD_TVD
