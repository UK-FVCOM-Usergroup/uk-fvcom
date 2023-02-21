!==============================================================================|
!     UTILITIES FILE                                                           |
!      PSTOP:   HALTS PROGRAM CORRECTLY                                        |
!      WRITE_BANNER: WRITE FVCOM BANNER TO OUTPUT                              |
!==============================================================================|


!==============================================================================|
   SUBROUTINE PSTOP               
!==============================================================================|
#if defined (MULTIPROCESSOR)
  USE MPI
  INTEGER IERR, ecode
  
  ecode = -1
  CALL MPI_ABORT(MPI_COMM_WORLD,ecode,IERR)

  CALL MPI_FINALIZE(IERR)
#endif 

  STOP
  END SUBROUTINE PSTOP
!==============================================================================|

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

!==============================================================================|
   SUBROUTINE WRITE_BANNER(IUNIT) 
!==============================================================================|
   INTEGER, INTENT(IN) :: IUNIT
!------------------------------------------------------------------------------|

   WRITE(IUNIT,*)'!===========================================================!'
   WRITE(IUNIT,*)'!   ##### #   #  ####  ###  #   #        #### ##### #   #   !'
   WRITE(IUNIT,*)'!   #     #   # #     #   # ## ##       #     #     ## ##   !'
   WRITE(IUNIT,*)'!   ####  #   # #     #   # # # # ##### #  ## ####  # # #   !'
   WRITE(IUNIT,*)'!   #      # #  #     #   # #   #       #   # #     #   #   !'
   WRITE(IUNIT,*)'!   #       #    ####  ###  #   #        #### ##### #   #   |'
   WRITE(IUNIT,*)'!===========================================================!'
   WRITE(IUNIT,*)'!======Copyright 2009, University of Massachusett Dartmouth=!'
   WRITE(IUNIT,*)'!                                                           !'
   WRITE(IUNIT,*)'!========DOMAIN DECOMPOSITION USING: METIS 4.0.1 ===========!'
   WRITE(IUNIT,*)'!======Copyright 1998, Regents of University of Minnesota===!'
   WRITE(IUNIT,*)'!                                                           !'

   RETURN
   END SUBROUTINE WRITE_BANNER
!==============================================================================|

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

!==============================================================================|
   SUBROUTINE N2E3D(NVAR,EVAR)
!==============================================================================|
   USE ALL_VARS
   IMPLICIT NONE 
   REAL(SP), DIMENSION(0:MT,1:KB), INTENT(IN)  :: NVAR  
   REAL(SP), DIMENSION(0:NT,1:KB), INTENT(OUT) :: EVAR  
   INTEGER I,K
!------------------------------------------------------------------------------|
                                                                                                                           
   DO K=1,KB
     DO I = 1, N
       EVAR(I,K) = ONE_THIRD*(NVAR(NV(I,1),K)+NVAR(NV(I,2),K)+NVAR(NV(I,3),K))
     END DO
   END DO

   RETURN
   END SUBROUTINE N2E3D 
!==============================================================================|

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                                                                                                           
!==============================================================================|
   SUBROUTINE N2E2D(NVAR,EVAR)
!==============================================================================|
   USE ALL_VARS
   IMPLICIT NONE
   REAL(SP), DIMENSION(0:MT), INTENT(IN)  :: NVAR 
   REAL(SP), DIMENSION(0:NT), INTENT(OUT) :: EVAR  
   INTEGER I,K
!------------------------------------------------------------------------------|
                                                                                                                           
   DO I = 1, N
     EVAR(I) = ONE_THIRD*(NVAR(NV(I,1))+NVAR(NV(I,2))+NVAR(NV(I,3)))
   END DO
                                                                                                                           
   RETURN
   END SUBROUTINE N2E2D
!==============================================================================|

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                                                                                                        
                                                                                                                        
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

!==============================================================================|
   SUBROUTINE E2N2D(EVAR,NVAR)
!==============================================================================|
   USE ALL_VARS
   IMPLICIT NONE
   REAL(SP), DIMENSION(0:NT), INTENT(IN ) :: EVAR  
   REAL(SP), DIMENSION(0:MT), INTENT(OUT) :: NVAR 
   INTEGER I,K
!------------------------------------------------------------------------------|
                                                                                                                           
   DO I=1,M
     NVAR(I) = SUM(EVAR(NBVE(I,1:NTVE(I))))/FLOAT(NTVE(I))
   END DO
                                                                                                                           
   RETURN
   END SUBROUTINE E2N2D
!==============================================================================|

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

!==============================================================================|
   SUBROUTINE E2N3D(EVAR,NVAR)
!==============================================================================|
   USE ALL_VARS
   IMPLICIT NONE
   REAL(SP), DIMENSION(0:NT,KB), INTENT(IN ) :: EVAR  
   REAL(SP), DIMENSION(0:MT,KB), INTENT(OUT) :: NVAR 
   INTEGER I,K
!------------------------------------------------------------------------------|
                                                                                                                           
   DO K=1,KB
     DO I=1,M
       NVAR(I,K) = SUM(EVAR(NBVE(I,1:NTVE(I)),K))/FLOAT(NTVE(I))
     END DO
   END DO
                                                                                                                           
   RETURN
   END SUBROUTINE E2N3D
!==============================================================================|
                                                                                                                        
!==============================================================================|
   SUBROUTINE GET_CASENAME(STRINGIN)
!==============================================================================|
   USE ALL_VARS
   IMPLICIT NONE
   CHARACTER(LEN=80) :: STRINGIN,TEMPSTR
   INTEGER IERR
   LOGICAL FEXIST
!------------------------------------------------------------------------------|

   IF(MSR)THEN
     CALL GETARG(1,TEMPSTR)
     
     IF(LEN_TRIM(TEMPSTR) == 0)THEN
       WRITE(IPT,*)'PLEASE PROVIDE CASENAME ON COMMAND LINE'
       WRITE(IPT,*)'STOPPING...'
       CALL PSTOP
     END IF
     STRINGIN = ADJUSTL(TEMPSTR)
     INQUIRE(FILE=TRIM(STRINGIN)//"_run.dat",EXIST=FEXIST)
     IF(.NOT.FEXIST)THEN
       WRITE(IPT,*)'FILE ',TRIM(STRINGIN)//"_run.dat",' DOES NOT EXIST'
       WRITE(IPT,*)'STOPPING...'
       CALL PSTOP
     END IF
   END IF
                                                                                                                        
# if defined (MULTIPROCESSOR)
   CALL MPI_BCAST(STRINGIN,80,MPI_CHARACTER,0,MPI_COMM_WORLD,IERR)
# endif

  RETURN
  END SUBROUTINE GET_CASENAME

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                                                                                                        
                                                                                                                        
!==============================================================================|
   SUBROUTINE REPORT_SIMTIME 
!==============================================================================|
!  REPORT TIME AT BEGINNING AND END OF CALCULATION                             |
!==============================================================================|

   USE ALL_VARS
   USE MOD_CLOCK
   IMPLICIT NONE
   CHARACTER(LEN=13) :: TSTRING
!------------------------------------------------------------------------------|


   WRITE(IPT,*)'!'
   WRITE(IPT,*)'!             STARTING CALCULATION   '
   WRITE(IPT,*)'!'
   CALL GETTIME(TSTRING,INT(DTI*(ISTART-1)))
   WRITE(IPT,7000)'!  SIMULATION TIME BEGIN :  ',TSTRING
   CALL GETTIME(TSTRING,INT(DTI*IEND))
   WRITE(IPT,7000)'!  SIMULATION TIME ENDS  :  ',TSTRING
   WRITE(IPT,*)'!'
   IF(IEND < ISTART)THEN
     WRITE(IPT,*)'END TIME IS BEFORE BEGIN TIME'
     WRITE(IPT,*)'CHECK RESTART AND INPUT DATASETS'
     CALL PSTOP
   END IF

   RETURN
7000 FORMAT(1X,A28,A13)  
   END SUBROUTINE REPORT_SIMTIME

