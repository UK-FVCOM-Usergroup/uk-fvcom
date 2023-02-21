
!==============================================================================|
   SUBROUTINE PRINT_VALS          

!------------------------------------------------------------------------------|

   USE ALL_VARS
   USE MOD_RCA

#  if defined (MULTIPROCESSOR)
   USE MOD_PAR
#  endif

   IMPLICIT NONE
   INTEGER :: I,K,IOUTTMP,ierr
   REAL, ALLOCATABLE, DIMENSION(:,:)   :: T1TMP,S1TMP

!==============================================================================|
   
   if(mod(ITIMESECS,300) == 0)then

    IF(SERIAL)THEN

        write(350+isys,*) time
        do ixy = 1,m
	  write(350+isys,'(2f10.2, <NZ>f12.7)') vx(ixy),vy(ixy),(caray(ixy,iz,isys),iz = 1,nz)
	end do
        write(350+isys,*) 

    END IF

#  if defined (MULTIPROCESSOR)
    IF(PAR)THEN
     ALLOCATE(T1TMP(0:MT,NZ))     ;T1TMP = 0.0
     T1TMP(1:MT,1:NZ) = caray(1:MT,1:NZ,isys)
     ALLOCATE(S1TMP(NXY,NZ))
     CALL GATHER(LBOUND(t1tmp,1),  UBOUND(t1tmp,1),  M,NXY,NZ,MYID,NPROCS,NMAP,t1tmp,S1TMP)

      if(msr) then
        do ixy = 1,nxy
         write(350+isys,'(2f10.2, <NZ>f12.7)') xg(ixy),yg(ixy),(s1tmp(ixy,iz),iz = 1,nz)
	end do
        write(350+isys,*) 
      end if

      DEALLOCATE(T1TMP,S1TMP)

   END IF
#  endif

   end if	  
 
    END
