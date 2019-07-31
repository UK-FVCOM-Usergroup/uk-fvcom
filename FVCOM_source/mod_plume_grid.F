MODULE MOD_PLUME_GRID
      USE MOD_PREC
      IMPLICIT NONE
      SAVE
      
      REAL(SP), ALLOCATABLE ::    ART1_G(:), D_G(:), GRAV_GN(:)
      INTEGER,  ALLOCATABLE ::           NBSN_G(:,:), NTSN_G(:)
      REAL(SP), ALLOCATABLE ::            DZZ_G(:,:), ZZ_G(:,:)

      ! 
      ! -- Storing the global grid once to avoid having to do it every single iteration
      !

      CONTAINS
      
      ! =================================================================================
      SUBROUTINE SETUP_PLUME_GRID
        USE ALL_VARS
        USE MOD_UTILS
        USE MOD_PAR
        USE LIMS
        IMPLICIT NONE
        INTEGER, ALLOCATABLE  ::                              NBSNg(:,:)
        INTEGER               ::                                       I

# if defined (MULTIPROCESSOR)
        IF (SERIAL) THEN
           IF (MSR) THEN
              ALLOCATE(ART1_G,  SOURCE = ART1)
              ALLOCATE(GRAV_GN, SOURCE = GRAV_N)
              ALLOCATE(ZZ_G,    SOURCE = ZZ)
              ALLOCATE(D_G,     SOURCE = H) ! Assuming that it's ok to use constant depth (neglecting tides),
                                            ! we do this so speed up the model.
              ALLOCATE(DZZ_G,   SOURCE = DZZ)
              ALLOCATE(NBSN_G,  SOURCE = NBSN)
              ALLOCATE(NTSN_G,  SOURCE = NTSN)
           END IF

        ELSE IF (PAR) THEN
         ! Make room for the variables   
           ALLOCATE(ART1_G(0:MGL));              ART1_G    = ZERO
           ALLOCATE(GRAV_GN(0:MGL));             GRAV_GN   = ZERO
           ALLOCATE(ZZ_G(0:MGL,KB));             ZZ_G      = ZERO
           ALLOCATE(D_G(0:MGL));                 D_G       = ZERO
           ALLOCATE(DZZ_G(0:MGL,KB));            DZZ_G     = ZERO
           ALLOCATE(NBSN_G(0:MGL,SIZE(NBSN,2))); NBSN_G    = ZERO
           ALLOCATE(NTSN_G(0:MGL));              NTSN_G    = ZERO

         ! Physical variables   
           CALL ACOLLECT(MYID,MSRID,NPROCS,NMAP,GRAV_N,GRAV_GN)
           CALL ACOLLECT(MYID,MSRID,NPROCS,NMAP,H,D_G)

         ! Depth/sigma layer information
           CALL ACOLLECT(MYID,MSRID,NPROCS,NMAP,ZZ,ZZ_G)
           CALL ACOLLECT(MYID,MSRID,NPROCS,NMAP,DZZ,DZZ_G)

         ! Grid IDs and connectivity
           CALL ACOLLECT(MYID,MSRID,NPROCS,NMAP,ART1,ART1_G)
           CALL ACOLLECT(MYID,MSRID,NPROCS,NMAP,NTSN,NTSN_G)
           ALLOCATE(NBSNg(SIZE(NBSN,1),SIZE(NBSN,2)));     NBSNg  = ZERO
           DO I = 1,M
              NBSNg(I,:) = NGID_X(NBSN(I,:))
           END DO
           CALL ACOLLECT(MYID,MSRID,NPROCS,NMAP,NBSNg,NBSN_G)
           DEALLOCATE(NBSNg)
# endif
        END IF
        RETURN
      END SUBROUTINE SETUP_PLUME_GRID

END MODULE MOD_PLUME_GRID
