      SUBROUTINE NCD_READ_GRID(INFILE)
!---------------------------------------------------------------------
! READ DIMENSIONS IN A NETCDF FILES
!---------------------------------------------------------------------
      USE MOD_NCD
      USE ALL_VARS
      USE MOD_RCA
      IMPLICIT NONE
!      INCLUDE 'RCACM'
!----------------------------------------------------------------------------!
      CHARACTER(LEN=100), INTENT(IN) :: INFILE
!----------------------------------------------------------------------------!
      INTEGER :: IERR
      INTEGER :: N_ELEMS,N_NODES,N_SIG_M1,N_SIG
      REAL, ALLOCATABLE, DIMENSION(:,:) :: TEMP
!----------------------------------------------------------------------------!

!--Open NetCDF DATA FILE
      IERR = NF90_OPEN(TRIM(INFILE),NF90_NOWRITE,NC_FID)
      IF(IERR /= NF90_NOERR)THEN
       WRITE(*,*)'ERROR READING ',TRIM(INFILE)
       WRITE(*,*)TRIM(NF90_STRERROR(IERR))
       CALL PSTOP('ncdio23   ')
      END IF

!--Get Model Dimensions
      N_ELEMS   = GETDIM(NC_FID,LEN_TRIM('nele'),'nele')
      N_NODES   = GETDIM(NC_FID,LEN_TRIM('node'),'node')
      N_SIG_M1  = GETDIM(NC_FID,LEN_TRIM('siglay'),'siglay')

      IF(N_ELEMS /= EXY)THEN
       IF(MSR)THEN 
        WRITE(*,*) 'EXY is inconsistent with the # of ELEMENTS ',       &
                   'in NetCDF file'
        WRITE(*,*) 'EXY should be equal to',N_ELEMS
        CALL PSTOP('ncdio36   ')
       END IF
      END IF    
      IF(N_NODES /= NXY)THEN
       IF(MSR)THEN 
        WRITE(*,*) 'NXY is inconsistent with the # of NODES in NetCDF file'
        WRITE(*,*) 'NXY should be equal to',N_NODES
        CALL PSTOP('ncdio43   ')
       END IF
      END IF    
      IF(N_SIG_M1 /= NZ)THEN
       IF(MSR)THEN 
        WRITE(*,*) 'NZ is inconsistent with the # of SIGMA LAYERS in NetCDF file'
        WRITE(*,*) 'NZ should be equal to',N_SIG_M1
        CALL PSTOP('ncdio50   ')
       END IF
      END IF    
  
      N = EXY
      M = NXY
      NT  = N
      MT  = M
      KB   = NZ+1
      KBM1 = NZ
      KBM2 = KBM1-1
  
!--close file
      IERR = NF90_CLOSE(NC_FID)

      RETURN
      END SUBROUTINE NCD_READ_GRID

!==============================================================================|

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!==============================================================================|

      SUBROUTINE NCD_READ_SHAPE(INFILE)
!---------------------------------------------------------------------
! READ BATHYMETRY, SIGMA LEVELS AND GRID COEFFICIENTS IN A NETCDF FILES
!---------------------------------------------------------------------
      USE MOD_NCD
      USE ALL_VARS
      USE MOD_RCA
      IMPLICIT NONE
!      INCLUDE 'RCACM'
!----------------------------------------------------------------------------!
      CHARACTER(LEN=100), INTENT(IN) :: INFILE
!----------------------------------------------------------------------------!
      INTEGER :: IERR
      INTEGER :: K
      REAL, ALLOCATABLE, DIMENSION(:,:) :: TEMP

!--OPEN NETCDF DATA FILE
      IERR = NF90_OPEN(TRIM(INFILE),NF90_NOWRITE,NC_FID)
      IF(IERR /= NF90_NOERR)THEN
       WRITE(*,*)'ERROR READING ',TRIM(INFILE)
       WRITE(*,*)TRIM(NF90_STRERROR(IERR))
       STOP
      END IF

!--Get Node Coordinates
      ALLOCATE(XG(0:NXY),YG(0:NXY)) ; XG = 0.0 ; YG = 0.0
  
      ALLOCATE(TEMP(NXY,1))
      CALL GETSVAR(NC_FID,LEN_TRIM('x'),'x',NXY,1,TEMP)
      XG(1:NXY) = TEMP(1:NXY,1)
      DEALLOCATE(TEMP)

      ALLOCATE(TEMP(NXY,1))
      CALL GETSVAR(NC_FID,LEN_TRIM('y'),'y',NXY,1,TEMP)
      YG(1:NXY) = TEMP(1:NXY,1)
      DEALLOCATE(TEMP)

!--Get Node Numbering
      ALLOCATE(NVG(0:EXY,4)); NVG = 0

      ALLOCATE(TEMP(EXY,3))
      CALL GETSVAR(NC_FID,LEN_TRIM('nv'),'nv',EXY,3,TEMP)
      NVG(1:EXY,1:3) = TEMP(1:EXY,1:3)
      DEALLOCATE(TEMP)
      NVG(:,4) = NVG(:,1)

!--Get Bathymetry
      ALLOCATE(HG(0:NXY))  ; HG = 0.0

      ALLOCATE(TEMP(NXY,1))
      CALL GETSVAR(NC_FID,LEN_TRIM('h'),'h',NXY,1,TEMP)
      HG(1:NXY) = TEMP(1:NXY,1)
      DEALLOCATE(TEMP)

!--Get Sigma levels
      ALLOCATE(ZG(0:NXY,KB))  ; ZG = 0.0    !!SIGMA COORDINATE VALUE 
      ALLOCATE(TEMP(NXY,KB))
      CALL GETSVAR(NC_FID,LEN_TRIM('siglev'),'siglev',NXY,KB,TEMP)
      ZG(1:NXY,1:KB) = TEMP(1:NXY,1:KB)
      DEALLOCATE(TEMP)

!--Close file
      IERR = NF90_CLOSE(NC_FID)

      RETURN
      END SUBROUTINE NCD_READ_SHAPE

!==============================================================================|

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!==============================================================================|

      SUBROUTINE NCD_READ(INFILE,UL,VL,UAL,VAL,KHL,ELL,SL,TL,HO,N_HOUR)
!---------------------------------------------------------------------
! READ DATA FROM DAILY NETCDF FILES
!---------------------------------------------------------------------

      USE MOD_NCD
      USE ALL_VARS        !, ONLY : MTLOC, NTLOC
      USE MOD_RCA
# if defined (MULTIPROCESSOR)
      USE MOD_PAR
# endif  
      IMPLICIT NONE
!----------------------------------------------------------------------------!
!      INCLUDE 'RCACM'
      REAL, DIMENSION(0:EXY,KB) :: UL_GL,VL_GL
      REAL, DIMENSION(0:EXY)    :: UAL_GL,VAL_GL
      REAL, DIMENSION(0:NXY,KB) :: KHL_GL,TL_GL,SL_GL
      REAL, DIMENSION(0:NXY)    :: ELL_GL
      REAL, DIMENSION(0:NT,KB),INTENT(OUT)   :: UL,VL
      REAL, DIMENSION(0:NT)   ,INTENT(OUT)   :: UAL,VAL
      REAL, DIMENSION(0:MT,KB),INTENT(OUT)   :: KHL,TL,SL
      REAL, DIMENSION(0:MT),INTENT(OUT)      :: ELL
      INTEGER, INTENT(IN)                         :: HO
      INTEGER, INTENT(OUT)                         :: N_HOUR      
      CHARACTER(LEN=100), INTENT(IN)              :: INFILE
!----------------------------------------------------------------------------!
      INTEGER            :: IERR
      INTEGER            :: HT
      REAL, ALLOCATABLE, DIMENSION(:,:) :: TEMP
      REAL, ALLOCATABLE, DIMENSION(:)   :: TEMP1
      INTEGER            :: I,K

!--Adjustement to read in Netcdf file
      HT=HO+1
      
      ELL  = 0.0
    
!--Open NetCDF Datafile 
      IERR = NF90_OPEN(TRIM(INFILE),NF90_NOWRITE,NC_FID)
      IF(IERR /=NF90_NOERR)THEN
       WRITE(*,*)'ERROR READING ',TRIM(INFILE)
       WRITE(*,*)TRIM(NF90_STRERROR(IERR))
       STOP
      END IF
!#################  READ TOTAL TIME STEP IN HOUR
      N_HOUR   = GETDIM(NC_FID,LEN_TRIM('time'),'time')   !Tian Temporal

!---------------------------------------------------------------------
! Read Data from file INFILE at time level ht
!---------------------------------------------------------------------

!--free surface elevation
      ALLOCATE(TEMP(NXY,1))
      CALL GETDVAR(NC_FID,LEN_TRIM('zeta'),'zeta',NXY,1,TEMP,HT)
      ELL_GL(1:NXY) = TEMP(1:NXY,1)
      DEALLOCATE(TEMP)

      IF(SERIAL) ELL(1:NXY) = ELL_GL(1:NXY)

# if defined (MULTIPROCESSOR)
      IF(PAR)THEN
       DO I=1,M
        ELL(I) = ELL_GL(NGID(I))
       END DO

       DO I=1,NHN
        ELL(I+M) = ELL_GL(HN_LST(I))
       END DO
      END IF
# endif

!--salinity
      ALLOCATE(TEMP(NXY,KBM1))
      CALL GETDVAR(NC_FID,LEN_TRIM('salinity'),'salinity',           &
                   NXY,KBM1,TEMP,HT)
      SL_GL(1:NXY,1:KBM1) = TEMP(1:NXY,1:KBM1)
      DEALLOCATE(TEMP)

      IF(SERIAL) SL = SL_GL

# if defined (MULTIPROCESSOR)
      IF(PAR)THEN
       DO K=1,KBM1
        DO I=1,M
         SL(I,K) = SL_GL(NGID(I),K)
        END DO

        DO I=1,NHN
         SL(I+M,K) = SL_GL(HN_LST(I),K)
        END DO
       END DO
      END IF
# endif

!--temperature
      ALLOCATE(TEMP(NXY,KBM1))
      CALL GETDVAR(NC_FID,LEN_TRIM('temp'),'temp',NXY,KBM1,TEMP,HT)
      TL_GL(1:NXY,1:KBM1) = TEMP(1:NXY,1:KBM1)
      DEALLOCATE(TEMP)

      IF(SERIAL) TL = TL_GL

# if defined (MULTIPROCESSOR)
      IF(PAR)THEN
       DO K=1,KBM1
        DO I=1,M
         TL(I,K) = TL_GL(NGID(I),K)
        END DO

        DO I=1,NHN
         TL(I+M,K) = TL_GL(HN_LST(I),K)
        END DO
       END DO
      END IF
# endif

!--U velocity 
      ALLOCATE(TEMP(EXY,KBM1))
      CALL GETDVAR(NC_FID,LEN_TRIM('u'),'u',EXY,KBM1,TEMP,HT)
      UL_GL(1:EXY,1:KBM1) = TEMP(1:EXY,1:KBM1)
      DEALLOCATE(TEMP)

      IF(SERIAL) UL = UL_GL

# if defined (MULTIPROCESSOR)
      IF(PAR)THEN
       DO K=1,KBM1
        DO I=1,N
         UL(I,K) = UL_GL(EGID(I),K)
        END DO

        DO I=1,NHE
         UL(I+N,K) = UL_GL(HE_LST(I),K)
        END DO
       END DO
      END IF
# endif

!--V velocity
      ALLOCATE(TEMP(EXY,KBM1))
      CALL GETDVAR(NC_FID,LEN_TRIM('v'),'v',EXY,KBM1,TEMP,HT)
      VL_GL(1:EXY,1:KBM1) = TEMP(1:EXY,1:KBM1)
      DEALLOCATE(TEMP)

      IF(SERIAL) VL = VL_GL

# if defined (MULTIPROCESSOR)
      IF(PAR)THEN
       DO K=1,KBM1
        DO I=1,N
         VL(I,K) = VL_GL(EGID(I),K)
        END DO

        DO I=1,NHE
         VL(I+N,K) = VL_GL(HE_LST(I),K)
        END DO
       END DO
      END IF
# endif

!--KH
      !ALLOCATE(TEMP(NXY,KBM1))
      ALLOCATE(TEMP(NXY,KB))   !Tian       
     ! CALL GETDVAR(NC_FID,LEN_TRIM('kh'),'kh',NXY,KBM1,TEMP,HT)
      CALL GETDVAR(NC_FID,LEN_TRIM('kh'),'kh',NXY,KB,TEMP,HT)  !Tian Temporal    
!      CALL GETDVAR(NC_FID,LEN_TRIM('km'),'km',NXY,KBM1,TEMP,HT)
      KHL_GL(1:NXY,1:KBM1) = TEMP(1:NXY,1:KBM1)
      DEALLOCATE(TEMP)

      IF(SERIAL) KHL = KHL_GL

# if defined (MULTIPROCESSOR)
      IF(PAR)THEN
       DO K=1,KBM1
        DO I=1,M
         KHL(I,K) = KHL_GL(NGID(I),K) 
        END DO

        DO I=1,NHN
         KHL(I+M,K) = KHL_GL(HN_LST(I),K)
        END DO
       END DO
      END IF
# endif
	
!--UA velocity 
      ALLOCATE(TEMP1(EXY))
      CALL GETDVAR(NC_FID,LEN_TRIM('ua'),'ua',EXY,1,TEMP1,HT)
      UAL_GL(1:EXY) = TEMP1(1:EXY)
      DEALLOCATE(TEMP1)

      IF(SERIAL) UAL = UAL_GL

# if defined (MULTIPROCESSOR)
      IF(PAR)THEN
        DO I=1,N
         UAL(I) = UAL_GL(EGID(I))
        END DO

        DO I=1,NHE
         UAL(I+N) = UAL_GL(HE_LST(I))
        END DO
      END IF
# endif

!--VA velocity
      ALLOCATE(TEMP1(EXY))
      CALL GETDVAR(NC_FID,LEN_TRIM('va'),'va',EXY,1,TEMP1,HT)
      VAL_GL(1:EXY) = TEMP1(1:EXY)
      DEALLOCATE(TEMP1)

      IF(SERIAL) VAL = VAL_GL

# if defined (MULTIPROCESSOR)
      IF(PAR)THEN
        DO I=1,N
         VAL(I) = VAL_GL(EGID(I))
        END DO

        DO I=1,NHE
         VAL(I+N) = VAL_GL(HE_LST(I))
        END DO
      END IF
# endif

!--Close file
      IERR = NF90_CLOSE(NC_FID)
  
      RETURN
      END SUBROUTINE NCD_READ


