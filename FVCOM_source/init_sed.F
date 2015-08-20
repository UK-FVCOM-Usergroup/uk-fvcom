!=======================================================================
! FVCOM Sediment Initialization Routine (User Defined)
!   Configuration:  default    
!=======================================================================
  Subroutine Init_Sed  
# if defined (SEDIMENT)
  USE MOD_UTILS
  USE MOD_NCTOOLS
  USE MOD_INPUT
  USE ALL_VARS
  USE EQS_OF_STATE
  
  Use Mod_Prec 
  Use Mod_Sed
  Use Lims, only: m,kbm1,kb
  implicit none 
  integer :: i,k,ised
  real(sp) :: bed_thickness
  real(sp), allocatable :: tmp(:)

   TYPE(NCVAR),  POINTER :: VAR
   TYPE(NCDIM),  POINTER :: DIM
   LOGICAL :: FOUND


  if(SEDIMENT_PARAMETER_TYPE/=CNSTNT)then
    do i=1,nsed
      call LOAD_SEDIMENT_PARAMETER(sed(i)%t_cd,sed(i)%t_ce,sed(i)%rate)
    end do
  else
    do i=1,nsed
      sed(i)%t_cd = sed(i)%tau_cd
      sed(i)%t_ce = sed(i)%tau_ce
      sed(i)%rate = sed(i)%erate
    end do
  end if

  !set up the bedflag 
  ! bedflag = 0. => deactive erosion/deposition
  ! bedflag = 1. => normal erosion/deposition
  if(BEDFLAG_TYPE/=CNSTNT)then
    allocate(tmp(0:mt)) ; tmp = 0.0
    call LOAD_BEDFLAG(tmp) 
    bottom(0:mt,bflag) = tmp(0:mt) 
    deallocate(tmp) 
  else
    do i=1,m   
      bottom(i,bflag) = 1.0
    end do
  endif;


  !--------------------------------------------------
  !Initialize critical shear stress for erosion
  !--------------------------------------------------
 ! do i=1,nsed
 !   if(sed(i)%stype=='cohesive')then
 !     VAR => FIND_VAR(NC_START,'tce',FOUND)
 !     IF(.not. FOUND) CALL FATAL_ERROR("COULD NOT FIND VARIABLE 'tce'&
 !          & IN THE STARTUP FILE OBJECT")
 !     CALL NC_CONNECT_AVAR(VAR, sed(i)%t_ce)
 !     CALL NC_READ_VAR(VAR)
 !     CALL NC_DISCONNECT(VAR)
 !   end if
 ! end do
  !--------------------------------------------------
  !Initialize critical shear stress for deposition
  !--------------------------------------------------
 ! do i=1,nsed
 !   if(sed(i)%stype=='cohesive')then
 !     VAR => FIND_VAR(NC_START,'tcd',FOUND)
 !     IF(.not. FOUND) CALL FATAL_ERROR("COULD NOT FIND VARIABLE 'tcd'&
 !        & IN THE STARTUP FILE OBJECT")
 !     CALL NC_CONNECT_AVAR(VAR, sed(i)%t_cd)
 !     CALL NC_READ_VAR(VAR)
 !     CALL NC_DISCONNECT(VAR)
 !   end if
 ! end do
  !--------------------------------------------------
  !Initialize surface erosion rate
  !--------------------------------------------------
 ! do i=1,nsed
 !   VAR => FIND_VAR(NC_START,'ero',FOUND)
 !   if(sed(i)%stype=='cohesive')then
 !     IF(.not. FOUND) CALL FATAL_ERROR("COULD NOT FIND VARIABLE 'ero'&
 !        & IN THE STARTUP FILE OBJECT")
 !     CALL NC_CONNECT_AVAR(VAR, sed(i)%rate)
 !     CALL NC_READ_VAR(VAR)
 !     CALL NC_DISCONNECT(VAR)
 !   end if
 ! end do
  !--------------------------------------------------
  !Initialize Bed Properties
  !--------------------------------------------------

  Do k=1,Nbed
    Do i=1,m
       bed(i,k,iaged) = 0.00
       bed(i,k,ithck) = init_bed_thickness(k) 
       bed(i,k,iporo) = init_bed_porosity
     End Do
   End do

  !--------------------------------------------------
  !Initialize Bed_Frac properties
  !--------------------------------------------------

  !Do k=1,Nbed
  !  Do i=1,m
  !     Do ised=1,Nsed
  !       sed(ised)%frac(i,k)=1.0/float(nsed)   
  !     End Do
  !   End Do
  !End Do
  !--------------------------------------------------
  !Initialize Bed_Frac properties
  !Version 2.0 Skagit Sediment Setup
  !Mostly fine sand (dominant size class on flats)
  !--------------------------------------------------

  Do k=1,Nbed
    Do i=1,m
       Do ised=1,Nsed
         sed(ised)%frac(i,k)=init_bed_fraction(ised)  !1.0/float(nsed)   
       End Do
     End Do
  End Do


  !--------------------------------------------------
  !Initialize Sediment Concentrations
  !--------------------------------------------------
  Do k=1,Kb
    Do i=1,m
      Do ised=1,Nsed
        sed(ised)%conc(i,k) = 0.0 
        sed(ised)%cnew(i,k) = sed(ised)%conc(i,k) 
      End Do
    End do
  End do

  Return
# endif
  End Subroutine Init_Sed
!=======================================================================
