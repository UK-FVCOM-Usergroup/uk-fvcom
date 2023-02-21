SUBROUTINE NCD_READ_GRID(INFILE)
  !---------------------------------------------------------------------
  ! READ DIMENSIONS IN A NETCDF FILES
  !---------------------------------------------------------------------
  USE MOD_NCD
  USE LIMS
  USE CONTROL
  USE SIZES
  USE WQM
  IMPLICIT NONE
  !----------------------------------------------------------------------------!
  CHARACTER(LEN=100), INTENT(IN) :: INFILE
  !----------------------------------------------------------------------------!
  INTEGER            :: IERR
  INTEGER            :: N_ELEMS,N_NODES,N_SIG_M1,N_SIG
  REAL(SP), ALLOCATABLE, DIMENSION(:,:) :: TEMP
  !----------------------------------------------------------------------------!

  !--Open NetCDF DATA FILE
  IERR = NF90_OPEN(TRIM(INFILE),NF90_NOWRITE,NC_FID)
  Print*, 'INFILE', INFILE
  IF(IERR /=NF90_NOERR)THEN
     WRITE(*,*)'ERROR READING ',TRIM(INFILE)
     WRITE(*,*)TRIM(NF90_STRERROR(IERR))
     CALL PSTOP
  END IF

  !--Get Model Dimensions
  N_ELEMS   = GETDIM(NC_FID,LEN_TRIM('nele'),'nele')
  N_NODES   = GETDIM(NC_FID,LEN_TRIM('node'),'node')
  N_SIG_M1  = GETDIM(NC_FID,LEN_TRIM('siglay'),'siglay')
  N_SIG     = GETDIM(NC_FID,LEN_TRIM('siglev'),'siglev')
  Print*, 'getdim in netcdf ok'
  IF(N_ELEMS /= NGL)THEN
    IF(MSR)THEN 
      WRITE(*,*) 'NGL is inconsistent with the # of ELEMENTS in NetCDF file'
      WRITE(*,*) 'NGL should be equal to',N_ELEMS,NGL
      CALL PSTOP
    END IF
  END IF    
  IF(N_NODES /= MGL)THEN
    IF(MSR)THEN 
      WRITE(*,*) 'MGL IS inconsistent with the # of NODES in NetCDF file'
      WRITE(*,*) 'MGL should be equal to',N_NODES
      CALL PSTOP
    END IF
  END IF    
  IF(N_SIG /= KB)THEN
    IF(MSR)THEN 
      WRITE(*,*) 'KB IS inconsistent with the # of SIGMA LAYERS in NetCDF file'
      WRITE(*,*) 'KB should be equal to',N_SIG
      CALL PSTOP
    END IF
  END IF    
  
  MLOC=N_NODES
  NLOC=N_ELEMS
  MTLOC = MLOC
  NTLOC = NLOC
  
  KB=N_SIG    
  KBM1=N_SIG_M1
  KBM2=KB-2

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
  USE WQM
  IMPLICIT NONE
  !----------------------------------------------------------------------------!
  CHARACTER(LEN=100), INTENT(IN) :: INFILE
  !----------------------------------------------------------------------------!
  INTEGER            :: IERR
  INTEGER            :: I,K
  REAL(SP), ALLOCATABLE, DIMENSION(:,:) :: TEMP

  !--OPEN NETCDF DATA FILE
  IERR = NF90_OPEN(TRIM(INFILE),NF90_NOWRITE,NC_FID)
  IF(IERR /= NF90_NOERR)THEN
    WRITE(*,*)'ERROR READING ',TRIM(INFILE)
    WRITE(*,*)TRIM(NF90_STRERROR(IERR))
    STOP
  END IF

  !--Get Node Coordinates
  ALLOCATE(XG(0:MGL),YG(0:MGL)) ; XG = 0.0_SP ; YG = 0.0_SP
  
  ALLOCATE(TEMP(MGL,1))
  CALL GETSVAR(NC_FID,LEN_TRIM('x'),'x',MGL,1,TEMP)
  XG(1:MGL) = TEMP(1:MGL,1)
  DEALLOCATE(TEMP)

  ALLOCATE(TEMP(MGL,1))
  CALL GETSVAR(NC_FID,LEN_TRIM('y'),'y',MGL,1,TEMP)
  YG(1:MGL) = TEMP(1:MGL,1)
  DEALLOCATE(TEMP)

  !--Get Node Numbering
  ALLOCATE(NVG(0:NGL,4)); NVG = 0

  ALLOCATE(TEMP(NGL,3))
  CALL GETSVAR(NC_FID,LEN_TRIM('nv'),'nv',NGL,3,TEMP)
  NVG(1:NGL,1:3) = TEMP(1:NGL,1:3)
  DEALLOCATE(TEMP)
  NVG(:,4) = NVG(:,1)

  !--Get Bathymetry
  ALLOCATE(HG(0:MGL))  ; HG = 0.0_SP

  ALLOCATE(TEMP(MGL,1))
  CALL GETSVAR(NC_FID,LEN_TRIM('h'),'h',MGL,1,TEMP)
  HG(1:MGL) = TEMP(1:MGL,1)
  DEALLOCATE(TEMP)

  ALLOCATE(Z(KB))  ; Z   = 0.0_SP    !!SIGMA COORDINATE VALUE 
  ALLOCATE(ZZ(KB)) ; ZZ  = 0.0_SP    !!INTRA LEVEL SIGMA VALUE
  ALLOCATE(DZ(KB)) ; DZ  = 0.0_SP    !!DELTA-SIGMA VALUE
  ALLOCATE(DZZ(KB)); DZZ = 0.0_SP    !!DELTA OF INTRA LEVEL SIGMA 

  !--Get Sigma levels
  ALLOCATE(TEMP(KB,1))
  CALL GETSVAR(NC_FID,LEN_TRIM('siglev'),'siglev',KB,1,TEMP)
  Z(1:KB) = TEMP(1:KB,1)
  DEALLOCATE(TEMP)
      do K=1,KB
         Z(K)=0.0-(K-1)*(1.0/(KB*1.0-1.0))
      enddo
  !--Compute derivative and intra-sigma levels
  DO K=1,KBM1
    ZZ(K)=0.5_SP*(Z(K)+Z(K+1))
    DZ(K)=Z(K)-Z(K+1)
  END DO
  ZZ(KB)=2.0_SP*ZZ(KBM1)-ZZ(KBM2)

  DO K=1,KBM2
    DZZ(K)=ZZ(K)-ZZ(K+1)
  END DO
  DZZ(KB-1)=0.0
  DZ(KB)=0.0

  !--Close file
  IERR = NF90_CLOSE(NC_FID)
  Print*, 'closed netcdf file after read in grid info'
  RETURN
END SUBROUTINE NCD_READ_SHAPE

!==============================================================================|

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!==============================================================================|

SUBROUTINE NCD_READ(INFILE,UL,VL,WTSL,UARD_OBCNL,XFLUX_OBCL,DTFAL,KHL,ELL,TL,SL,HO)
  !---------------------------------------------------------------------
  ! READ DATA FROM DAILY NETCDF FILES
  !---------------------------------------------------------------------

  USE MOD_NCD
  USE LIMS
  USE WQM
  USE ALL_VARS, ONLY : MTLOC, NTLOC
  USE MOD_OBCS
# if defined (MULTIPROCESSOR)
  USE MOD_PAR
# endif  
  IMPLICIT NONE
  !----------------------------------------------------------------------------!
  REAL, DIMENSION(0:NGL,KB) :: UL_GL,VL_GL,WWL_GL
  REAL, DIMENSION(0:MGL,KB) :: KHL_GL,TL_GL,SL_GL,WTSL_GL
  REAL, DIMENSION(0:MGL)    :: ELL_GL,DTFAL_GL
  REAL, DIMENSION(0:NOBTY+1) :: UARD_OBCN_GL
  REAL, DIMENSION(0:NOBTY,KBM1) :: XFLUX_OBC_GL

  REAL, DIMENSION(0:NTLOC,KB),INTENT(OUT)   :: UL,VL     !,WWL
  REAL, DIMENSION(0:MTLOC,KB),INTENT(OUT)   :: KHL,TL,SL,WTSL
  REAL, DIMENSION(0:MTLOC),INTENT(OUT)      :: ELL,DTFAL
  REAL, DIMENSION(0:NOBTY+1),INTENT(OUT)    :: UARD_OBCNL
  REAL, DIMENSION(0:NOBTY,KBM1),INTENT(OUT) :: XFLUX_OBCL
  INTEGER, INTENT(IN)                         :: HO
  CHARACTER(LEN=100), INTENT(IN)              :: INFILE
  !----------------------------------------------------------------------------!
  INTEGER            :: IERR
  INTEGER            :: HT
  REAL, ALLOCATABLE, DIMENSION(:,:) :: TEMP
  INTEGER            :: I,K

  !--Adjustement to read in Netcdf file
  Print*, 'HO', HO
  HT=HO+1
    
  !--Open NetCDF Datafile 
  IERR = NF90_OPEN(TRIM(INFILE),NF90_NOWRITE,NC_FID)
  IF(IERR /=NF90_NOERR)THEN
     WRITE(*,*)'ERROR READING ',TRIM(INFILE)
     WRITE(*,*)TRIM(NF90_STRERROR(IERR))
     STOP
  END IF
  
  !---------------------------------------------------------------------
  ! Read Data from file INFILE at time level ht
  !---------------------------------------------------------------------

  !--free surface elevation
  ALLOCATE(TEMP(MGL,1))
!RGL added prints
  Print*, 'MGL', MGL
      Print*, 'HT', HT
  Print*, 'called getdvar for elevation'
  CALL GETDVAR(NC_FID,LEN_TRIM('zeta'),'zeta',MGL,1,TEMP,HT)
  ELL_GL(1:MGL) = TEMP(1:MGL,1)
  DEALLOCATE(TEMP)

  IF(SERIAL) ELL = ELL_GL

# if defined (MULTIPROCESSOR)
  IF(PAR)THEN
    DO I=1,MLOC
      ELL(I) = ELL_GL(NGID(I))
    END DO

    DO I=1,NHN
      ELL(I+MLOC) = ELL_GL(HN_LST(I))
    END DO
  END IF
# endif

  !--salinity
  ALLOCATE(TEMP(MGL,KBM1))
  CALL GETDVAR(NC_FID,LEN_TRIM('salinity'),'salinity',MGL,KBM1,TEMP,HT)
  SL_GL(1:MGL,1:KBM1) = TEMP(1:MGL,1:KBM1)
  DEALLOCATE(TEMP)
  Print*, 'called getdvar for salinity'
  IF(SERIAL) SL = SL_GL

# if defined (MULTIPROCESSOR)
  IF(PAR)THEN
    DO K=1,KBM1
      DO I=1,MLOC
        SL(I,K) = SL_GL(NGID(I),K)
      END DO

      DO I=1,NHN
        SL(I+MLOC,K) = SL_GL(HN_LST(I),K)
      END DO
    END DO
  END IF
# endif

  !--temperature
   ALLOCATE(TEMP(MGL,KBM1))
   CALL GETDVAR(NC_FID,LEN_TRIM('temp'),'temp',MGL,KBM1,TEMP,HT)
   TL_GL(1:MGL,1:KBM1) = TEMP(1:MGL,1:KBM1)
   DEALLOCATE(TEMP)
   Print*, 'called getdvar for temperature'
  IF(SERIAL) TL = TL_GL

# if defined (MULTIPROCESSOR)
  IF(PAR)THEN
    DO K=1,KBM1
      DO I=1,MLOC
        TL(I,K) = TL_GL(NGID(I),K)
      END DO

      DO I=1,NHN
        TL(I+MLOC,K) = TL_GL(HN_LST(I),K)
      END DO
    END DO
  END IF
# endif
! tykim00
       TL(:,:) = 20.0
       SL(:,:) = 20.0
!
  !--U velocity 
  ALLOCATE(TEMP(NGL,KBM1))
  CALL GETDVAR(NC_FID,LEN_TRIM('u'),'u',NGL,KBM1,TEMP,HT)
  UL_GL(1:NGL,1:KBM1) = TEMP(1:NGL,1:KBM1)
  DEALLOCATE(TEMP)
  Print*, 'call getdvar for u vel'
  IF(SERIAL) UL = UL_GL

# if defined (MULTIPROCESSOR)
  IF(PAR)THEN
    DO K=1,KBM1
      DO I=1,NLOC
        UL(I,K) = UL_GL(EGID(I),K)
      END DO

      DO I=1,NHE
        UL(I+NLOC,K) = UL_GL(HE_LST(I),K)
      END DO
    END DO
  END IF
# endif

  !--V velocity
  ALLOCATE(TEMP(NGL,KBM1))
  CALL GETDVAR(NC_FID,LEN_TRIM('v'),'v',NGL,KBM1,TEMP,HT)
  VL_GL(1:NGL,1:KBM1) = TEMP(1:NGL,1:KBM1)
  DEALLOCATE(TEMP)
  Print*, 'call getdvar for v velocity'
  IF(SERIAL) VL = VL_GL

# if defined (MULTIPROCESSOR)
  IF(PAR)THEN
    DO K=1,KBM1
      DO I=1,NLOC
        VL(I,K) = VL_GL(EGID(I),K)
      END DO

      DO I=1,NHE
        VL(I+NLOC,K) = VL_GL(HE_LST(I),K)
      END DO
    END DO
  END IF
# endif

  !--WTS velocity
  ALLOCATE(TEMP(MGL,KB))
  CALL GETDVAR(NC_FID,LEN_TRIM('wts'),'wts',MGL,KB,TEMP,HT)
  WTSL_GL(1:MGL,1:KB) = TEMP(1:MGL,1:KB)
  DEALLOCATE(TEMP)
  Print*, 'call getdvar for wts'
  IF(SERIAL) WTSL = WTSL_GL

# if defined (MULTIPROCESSOR)
  IF(PAR)THEN
    DO K=1,KB
      DO I=1,MLOC
        WTSL(I,K) = WTSL_GL(EGID(I),K)
      END DO

      DO I=1,NHE
        WTSL(I+MLOC,K) = WTSL_GL(HN_LST(I),K)
      END DO
    END DO
  END IF
# endif
! tykim
!      WTSL(:,KB)=0.0
!       UL=0.02
!       VL=0.0
!      WTSL=0.

  !--UARD_OBCN
!RGl changed nobty+1 to nobty, below
  ALLOCATE(TEMP(NOBTY,1))
  TEMP = 0.0
  CALL GETDVAR(NC_FID,LEN_TRIM('uard_obcn'),'uard_obcn',NOBTY,1,TEMP,HT)
  UARD_OBCN_GL(1:NOBTY) = TEMP(1:NOBTY,1)
  DEALLOCATE(TEMP)
  Print*, 'call getdvar for uard_obcn'
  Print*, 'NOBTY', NOBTY
   UARD_OBCN_GL(0) = 0.0
  IF(SERIAL) UARD_OBCNL = UARD_OBCN_GL
      
# if defined (MULTIPROCESSOR)
  IF(PAR)THEN
      DO I=1,IOBCN+1
        UARD_OBCNL(I) = UARD_OBCN_GL(EGID(I))
      END DO

      DO I=1,NHE
        UARD_OBCNL(I+IOBCN+1) = UARD_OBCN_GL(HN_LST(I))
      END DO
  END IF
# endif
  Print*, 'UARD_OBCNL', UARD_OBCNL(:)       
  !--XFLUX_OBCN
  ALLOCATE(TEMP(NOBTY,KBM1))
  CALL GETDVAR(NC_FID,LEN_TRIM('xflux_obc'),'xflux_obc',NOBTY,KBM1,TEMP,HT)
  XFLUX_OBC_GL(1:NOBTY,1:KBM1) = TEMP(1:NOBTY,1:KBM1)
  XFLUX_OBC_GL(0,:) = 0.0
  DEALLOCATE(TEMP)
  Print*, 'call getdvar for xflux_obc'
  IF(SERIAL) XFLUX_OBCL = XFLUX_OBC_GL

# if defined (MULTIPROCESSOR)
  IF(PAR)THEN
    DO K=1,KBM1
      DO I=1,IOBCN
        XFLUX_OBCL(I,K) = XFLUX_OBC_GL(EGID(I),K)
      END DO

      DO I=1,NHE
        XFLUX_OBCL(I+IOBCN,K) = XFLUX_OBC_GL(HE_LST(I),K)
      END DO
    END DO
  END IF
# endif
  Print*, 'XFLUX_OBCL',XFLUX_OBCL(:,1) 
  !--free surface elevation
  ALLOCATE(TEMP(MGL,1))
  CALL GETDVAR(NC_FID,LEN_TRIM('dtfa'),'dtfa',MGL,1,TEMP,HT)
  DTFAL_GL(1:MGL) = TEMP(1:MGL,1)
  DEALLOCATE(TEMP)
  Print*, 'call getdvar for dtfa'
  IF(SERIAL) DTFAL = DTFAL_GL

# if defined (MULTIPROCESSOR)
  IF(PAR)THEN
    DO I=1,MLOC
      DTFAL(I) = DTFAL_GL(NGID(I))
    END DO

    DO I=1,NHN
      DTFAL(I+MLOC) = DTFAL_GL(HN_LST(I))
    END DO
  END IF
# endif


!       write(2247,*) JDAY,4,MGL,(TL_GL(I,1),i=1,MGL), &
!                             (SL_GL(I,1),i=1,MGL), &
!                             (ELL_GL(I),i=1,MGL), &
!                             (ELL_GL(I),i=1,MGL), &
!                             NGL,(UL(I,1),I=1,NGL)
       
!       write(2248,*) JDAY,(UL(I,1),i=1,NGL),(VL(I,1),i=1,NGL),&
!       (UL(I,2),i=1,NGL),(VL(I,2),i=1,NGL),&
!       (UL(I,3),i=1,NGL),(VL(I,3),i=1,NGL)


  !--KH
  ALLOCATE(TEMP(MGL,KBM1))
  CALL GETDVAR(NC_FID,LEN_TRIM('kh'),'kh',MGL,KBM1,TEMP,HT)
  KHL_GL(1:MGL,1:KBM1) = TEMP(1:MGL,1:KBM1)
  DEALLOCATE(TEMP)
  Print*, 'call getdvar for KH'
  IF(SERIAL) KHL = KHL_GL

# if defined (MULTIPROCESSOR)
  IF(PAR)THEN
    DO K=1,KBM1
      DO I=1,MLOC
        KHL(I,K) = KHL_GL(NGID(I),K)
      END DO

      DO I=1,NHN
        KHL(I+MLOC,K) = KHL_GL(HN_LST(I),K)
      END DO
    END DO
  END IF
# endif

  !--Close file
  IERR = NF90_CLOSE(NC_FID)
  
  RETURN
END SUBROUTINE NCD_READ


