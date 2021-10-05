      SUBROUTINE NCD_READ_GRID(INFILE)
!---------------------------------------------------------------------
! READ DIMENSIONS IN A NETCDF FILES
!---------------------------------------------------------------------
      USE MOD_NCDIN
      USE ALL_VARS
      IMPLICIT NONE
!----------------------------------------------------------------------------!
      CHARACTER(LEN=80), INTENT(IN) :: INFILE
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
       CALL PSTOP
      END IF

!--Get Model Dimensions
      N_ELEMS   = GETDIM(NC_FID,LEN_TRIM('nele'),'nele')
      N_NODES   = GETDIM(NC_FID,LEN_TRIM('node'),'node')
      N_SIG_M1  = GETDIM(NC_FID,LEN_TRIM('siglay'),'siglay')

      IF(N_ELEMS /= NGL)THEN
       IF(MSR)THEN 
        WRITE(*,*) 'NGL is inconsistent with the # of ELEMENTS ',       &
                   'in NetCDF file'
        WRITE(*,*) 'NGL should be equal to',N_ELEMS
        CALL PSTOP
       END IF
      END IF    
      IF(N_NODES /= MGL)THEN
       IF(MSR)THEN 
        WRITE(*,*) 'MGL is inconsistent with the # of NODES in NetCDF file'
        WRITE(*,*) 'MGL should be equal to',N_NODES
        CALL PSTOP
       END IF
      END IF    
      IF(N_SIG_M1 /= KBM1)THEN
       IF(MSR)THEN 
        WRITE(*,*) 'KBM1 is inconsistent with the # of SIGMA LAYERS in NetCDF file'
        WRITE(*,*) 'KBM1 should be equal to',N_SIG_M1
        CALL PSTOP
       END IF
      END IF    
  
      N = NGL
      M = MGL
      NT  = N
      MT  = M
      KB   = KBM1+1
      KBM2 = KBM1-1
  
!--close file
      IERR = NF90_CLOSE(NC_FID)

      RETURN
      END SUBROUTINE NCD_READ_GRID

!==============================================================================|

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
      SUBROUTINE NCD_READ_TIME(INFILE)
!---------------------------------------------------------------------
! READ DIMENSIONS OF TIME IN A NETCDF FILES
!---------------------------------------------------------------------
      USE MOD_NCDIN
      USE ALL_VARS
      IMPLICIT NONE
!----------------------------------------------------------------------------!
      CHARACTER(LEN=80), INTENT(IN) :: INFILE
!----------------------------------------------------------------------------!
      INTEGER :: IERR
      INTEGER :: N_TIMES
!----------------------------------------------------------------------------!

!--Open NetCDF DATA FILE
      IERR = NF90_OPEN(TRIM(INFILE),NF90_NOWRITE,NC_FID)
      IF(IERR /= NF90_NOERR)THEN
       WRITE(*,*)'ERROR READING ',TRIM(INFILE)
       WRITE(*,*)TRIM(NF90_STRERROR(IERR))
       CALL PSTOP
      END IF

!--Get Model Dimensions
      N_TIMES   = GETDIM(NC_FID,LEN_TRIM('time'),'time')
      ITIME = N_TIMES

!--close file
      IERR = NF90_CLOSE(NC_FID)

      RETURN
      END SUBROUTINE NCD_READ_TIME

!==============================================================================|

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!==============================================================================|

      SUBROUTINE NCD_READ_SHAPE(INFILE)
!---------------------------------------------------------------------
! READ BATHYMETRY, SIGMA LEVELS AND GRID COEFFICIENTS IN A NETCDF FILES
!---------------------------------------------------------------------
      USE MOD_NCDIN
      USE ALL_VARS
      IMPLICIT NONE
!----------------------------------------------------------------------------!
      CHARACTER(LEN=80), INTENT(IN) :: INFILE
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
      ALLOCATE(XG(0:MGL),YG(0:MGL)) ; XG = 0.0 ; YG = 0.0
  
      ALLOCATE(TEMP(MGL,1))
# if defined (SPHERICAL)
      CALL GETSVAR(NC_FID,LEN_TRIM('lon'),'lon',MGL,1,TEMP)
# else
      CALL GETSVAR(NC_FID,LEN_TRIM('x'),'x',MGL,1,TEMP)
# endif
      XG(1:MGL) = TEMP(1:MGL,1)
      DEALLOCATE(TEMP)

      ALLOCATE(TEMP(MGL,1))
# if defined (SPHERICAL)
      CALL GETSVAR(NC_FID,LEN_TRIM('lat'),'lat',MGL,1,TEMP)
# else
      CALL GETSVAR(NC_FID,LEN_TRIM('y'),'y',MGL,1,TEMP)
# endif
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
      ALLOCATE(HG(0:MGL))  ; HG = 0.0

      ALLOCATE(TEMP(MGL,1))
      CALL GETSVAR(NC_FID,LEN_TRIM('h'),'h',MGL,1,TEMP)
      HG(1:MGL) = TEMP(1:MGL,1)
      DEALLOCATE(TEMP)

!--Get Sigma levels
      ALLOCATE(ZG(0:MGL,KB))  ; ZG = 0.0    !!SIGMA COORDINATE VALUE 
      ALLOCATE(TEMP(MGL,KB))
      CALL GETSVAR(NC_FID,LEN_TRIM('siglev'),'siglev',MGL,KB,TEMP)
      ZG(1:MGL,1:KB) = TEMP(1:MGL,1:KB)
      DEALLOCATE(TEMP)

!--Close file
      IERR = NF90_CLOSE(NC_FID)

      RETURN
      END SUBROUTINE NCD_READ_SHAPE

!==============================================================================|

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!==============================================================================|

      SUBROUTINE NCD_READ(INFILE,UL,VL,UAL,VAL,KHL,ELL,SL,TL,SWL,HO)
!---------------------------------------------------------------------
! READ DATA FROM DAILY NETCDF FILES
!---------------------------------------------------------------------

      USE MOD_NCDIN
      USE ALL_VARS        !, ONLY : MTLOC, NTLOC
# if defined (MULTIPROCESSOR)
      USE MOD_PAR
# endif  
      IMPLICIT NONE
!----------------------------------------------------------------------------!
      REAL, DIMENSION(0:NGL,KB) :: UL_GL,VL_GL
      REAL, DIMENSION(0:NGL)    :: UAL_GL,VAL_GL
      REAL, DIMENSION(0:MGL,KB) :: KHL_GL,TL_GL,SL_GL
      REAL, DIMENSION(0:MGL)    :: ELL_GL,SWRAD_GL
      REAL, DIMENSION(0:NT,KB),INTENT(OUT)   :: UL,VL
      REAL, DIMENSION(0:NT)   ,INTENT(OUT)   :: UAL,VAL
      REAL, DIMENSION(0:MT,KB),INTENT(OUT)   :: KHL,TL,SL
      REAL, DIMENSION(0:MT),INTENT(OUT)      :: ELL,SWL
      INTEGER, INTENT(IN)                         :: HO
      CHARACTER(LEN=80), INTENT(IN)              :: INFILE
!----------------------------------------------------------------------------!
      INTEGER            :: IERR
      INTEGER            :: HT
      REAL, ALLOCATABLE, DIMENSION(:,:) :: TEMP
      REAL, ALLOCATABLE, DIMENSION(:)   :: TEMP1
      INTEGER            :: I,K
!JQI
      real(sp) :: tempp1,tempp2      
!JQI      

!--Adjustement to read in Netcdf file
      HT=HO+1
      
      ELL  = 0.0
      SWL  = 0.0
      UL = 0.0
      VL = 0.0
      UAL = 0.0 
      VAL = 0.0
      KHL = 0.0 
      TL = 0.0 
      SL = 0.0       

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
      CALL GETDVAR(NC_FID,LEN_TRIM('zeta'),'zeta',MGL,1,TEMP,HT)
      ELL_GL(1:MGL) = TEMP(1:MGL,1)
      DEALLOCATE(TEMP)

      IF(SERIAL) ELL(1:MGL) = ELL_GL(1:MGL)

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
      ALLOCATE(TEMP(MGL,KBM1))
      CALL GETDVAR(NC_FID,LEN_TRIM('salinity'),'salinity',           &
                   MGL,KBM1,TEMP,HT)
      SL_GL(1:MGL,1:KBM1) = TEMP(1:MGL,1:KBM1)
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

!JQI
    !  do i=1,mt
    !   tempp1 = 0.0
    !   tempp2 = 0.0
    !   if(h_50(i) > 0.0)then
    !    do k=1,h_50(i)
    !     tempp1 = tempp1 + sl(i,k)*dz(i,k)      
    !      tempp2 = tempp2 + dz(i,k)
!	end do
!	sl(i,1) = tempp1/tempp2
!	do k=2,h_50(i)
!	 sl(i,k) = sl(i,1)
!	end do
!       end if
!      end do
!JQI       	  

!--temperature
      ALLOCATE(TEMP(MGL,KBM1))
      CALL GETDVAR(NC_FID,LEN_TRIM('temp'),'temp',MGL,KBM1,TEMP,HT)
      TL_GL(1:MGL,1:KBM1) = TEMP(1:MGL,1:KBM1)
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

!JQI       do i=1,mt
!JQI        if(h_index(i) == 1)then
!JQI	 tl(i,1) = sum(tl(i,1:12)*dz(i,1:12))/sum(dz(i,1:12))
!JQI	 tl(i,2:12) = tl(i,1)
!JQI	end if  
!JQI      end do	
!JQI

!      do i=1,mt
!       tempp1 = 0.0
!       tempp2 = 0.0
!       if(h_50(i) > 0.0)then
!        do k=1,h_50(i)
!         tempp1 = tempp1 + tl(i,k)*dz(i,k)      
!	 tempp2 = tempp2 + dz(i,k)
!	end do
!	tl(i,1) = tempp1/tempp2
!	do k=2,h_50(i)
!	 tl(i,k) = tl(i,1)
!	end do
!       end if
!      end do
!JQI       	  

!--U velocity 
      ALLOCATE(TEMP(NGL,KBM1))
      CALL GETDVAR(NC_FID,LEN_TRIM('u'),'u',NGL,KBM1,TEMP,HT)
      UL_GL(1:NGL,1:KBM1) = TEMP(1:NGL,1:KBM1)
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

!JQI
!      do i=1,nt
!       tempp1 = 0.0
!       tempp2 = 0.0
!       if(h1_50(i) > 0.0)then
!        do k=1,h1_50(i)
!         tempp1 = tempp1 + ul(i,k)*dz1(i,k)      
!	 tempp2 = tempp2 + dz1(i,k)
!	end do
!	ul(i,1) = tempp1/tempp2
!	do k=2,h1_50(i)
!	 ul(i,k) = ul(i,1)
!	end do
!       end if
!      end do
!JQI       	  

!--V velocity
      ALLOCATE(TEMP(NGL,KBM1))
      CALL GETDVAR(NC_FID,LEN_TRIM('v'),'v',NGL,KBM1,TEMP,HT)
      VL_GL(1:NGL,1:KBM1) = TEMP(1:NGL,1:KBM1)
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

!JQI
!      do i=1,nt
!       tempp1 = 0.0
!       tempp2 = 0.0
!       if(h1_50(i) > 0.0)then
!        do k=1,h1_50(i)
!         tempp1 = tempp1 + vl(i,k)*dz1(i,k)      
!	 tempp2 = tempp2 + dz1(i,k)
!	end do
!	vl(i,1) = tempp1/tempp2
!	do k=2,h1_50(i)
!	 vl(i,k) = vl(i,1)
!	end do
!       end if
!      end do
!JQI       	  

!!--KH
!      ALLOCATE(TEMP(MGL,KBM1))
!      CALL GETDVAR(NC_FID,LEN_TRIM('kh'),'kh',MGL,KBM1,TEMP,HT)
!!      CALL GETDVAR(NC_FID,LEN_TRIM('km'),'km',MGL,KBM1,TEMP,HT)
!      KHL_GL(1:MGL,1:KBM1) = TEMP(1:MGL,1:KBM1)
!      DEALLOCATE(TEMP)

!--KH
      ALLOCATE(TEMP(MGL,KBM1))
      CALL GETDVAR(NC_FID,LEN_TRIM('kh'),'kh',MGL,KBM1,TEMP,HT)
!      CALL GETDVAR(NC_FID,LEN_TRIM('km'),'km',MGL,KBM1,TEMP,HT)
      KHL_GL(1:MGL,1:KBM1) = TEMP(1:MGL,1:KBM1)
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

!------- J. Ge deleted this special treatment--------
!!JQIJQI      KHL = KHL/2.0
!       do i=1,mt
!        if(h_index(i) == 1) then 
!          khl(i,:) = khl(i,:)*0.05
!        end if
!!        khl(i,h_50(i):kb) = 0.00001
!       end do	
!       where (kh > 0.01) kh = 0.01    !Temporal
!-----------------------------------------------------


!JQI
!      do i=1,mt
!       tempp1 = 0.0
!       tempp2 = 0.0
!       if(h_50(i) > 0.0)then
!        do k=1,h_50(i)
!         tempp1 = tempp1 + khl(i,k)*dz(i,k)      
!	 tempp2 = tempp2 + dz(i,k)
!	end do
!	khl(i,1) = tempp1/tempp2
!	do k=2,h_50(i)
!	 khl(i,k) = khl(i,1)
!	end do
!       end if
!      end do
!JQI       	  
	
!--UA velocity 
      ALLOCATE(TEMP1(NGL))
      CALL GETDVAR(NC_FID,LEN_TRIM('ua'),'ua',NGL,1,TEMP1,HT)
      UAL_GL(1:NGL) = TEMP1(1:NGL)
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
      ALLOCATE(TEMP1(NGL))
      CALL GETDVAR(NC_FID,LEN_TRIM('va'),'va',NGL,1,TEMP1,HT)
      VAL_GL(1:NGL) = TEMP1(1:NGL)
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

!--short wave heat
      ALLOCATE(TEMP(MGL,1))
      CALL GETDVAR(NC_FID,LEN_TRIM('short_wave'),'short_wave',MGL,1,TEMP,HT)
      SWRAD_GL(1:MGL) = TEMP(1:MGL,1)
      DEALLOCATE(TEMP)

      IF(SERIAL) SWL(1:MGL) = SWRAD_GL(1:MGL)

# if defined (MULTIPROCESSOR)
      IF(PAR)THEN
       DO I=1,M
        SWL(I) = SWRAD_GL(NGID(I))
       END DO

       DO I=1,NHN
        SWL(I+M) = SWRAD_GL(HN_LST(I))
       END DO
      END IF
# endif

!--Close file
      IERR = NF90_CLOSE(NC_FID)
  
      RETURN
      END SUBROUTINE NCD_READ


