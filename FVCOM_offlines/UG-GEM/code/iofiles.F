!==============================================================================!
!   Open Input Files for Model Parameters and Output Files for Results         !
!==============================================================================!

   SUBROUTINE IOFILES
   USE ALL_VARS
   USE MOD_UTILS

   IMPLICIT NONE
   LOGICAL:: CHECK 
   CHARACTER(LEN=80)  :: TEMP,ISTR,OSTR
   CHARACTER(LEN=100) :: MKOUTDIR,MKSMSDIR,MKMDMDIR,MKRESDIR,MKTSODIR,MKNCDDIR
   INTEGER IERR,ISTAT

!==============================================================================!
!                Definitions of input files                                    !
!                                                                              !
! inrun: casename_run.dat: input data and parameter file controlling           !
!                          the model run                                       !
! inobc: casename_obn.dat: input data for open boundary triangles              !
!                          nodes                                               !            
!==============================================================================!
         
!==============================================================================!
!                  Definitions of output files                                 !
!                                                                              !
! ioprt: casename_prt.dat: the file printing all input parametes, data         !
!                          and forcings                                        !
! ioplt: casename_plt.dat: the archive file including averaged field           !
!                          of currents, temperature, salinity,etc for          !
!                          a specified time interval (such as atidal           !
!                          cycle)                                              !
! iotsr: casename_tsr.dat: the archive file including time series of           !
!                           selected variables at selected locations           !            
!                                                                              !
!  there are also some output files in subroutines of "out_binary" and         !
!  "out_binary_residual" for standard model binary output. All these           !
!  files could be directly used for graphics system developed by the           !
!  ocean ecosystem modeling group at SMAST/UMASSD.                             !
!==============================================================================!

   IF(MSR)WRITE(IPT,*)'!                                                                !'
   IF(MSR)WRITE(IPT,*)'!                  OPENING FILES                                 !'
   IF(MSR)WRITE(IPT,*)'!                                                                !'

   INOBC = 13

   IOPRT = 41
   IOTSR = 43
   INRIV = 15 
   INBFW = 27
!
!-----------------CHECK FOR EXISTENCE/CREATE DIRECTORIES-----------------------!
!

#  if !defined (DOS)
   MKOUTDIR = "mkdir -p "//TRIM(OUTDIR)
   MKRESDIR = "mkdir -p "//TRIM(OUTDIR)//"/out"
   MKSMSDIR = "mkdir -p "//TRIM(OUTDIR)//"/sms"
   MKMDMDIR = "mkdir -p "//TRIM(OUTDIR)//"/medm"
   MKTSODIR = "mkdir -p "//TRIM(OUTDIR)//"/timeseries"
   MKNCDDIR = "mkdir -p "//TRIM(OUTDIR)//"/netcdf"


   IF(MSR)THEN
#    if !defined (CRAY) 
     CALL SYSTEM( TRIM(MKRESDIR) )
     CALL SYSTEM( TRIM(MKOUTDIR) )
     CALL SYSTEM( TRIM(MKSMSDIR) )
     CALL SYSTEM( TRIM(MKMDMDIR) )
     CALL SYSTEM( TRIM(MKTSODIR) )
     CALL SYSTEM( TRIM(MKNCDDIR) )

#    endif
#    if defined (CRAY)
     CALL CRAY_SYSTEM_CALL(TRIM(MKRESDIR))
     CALL CRAY_SYSTEM_CALL(TRIM(MKOUTDIR))
     CALL CRAY_SYSTEM_CALL(TRIM(MKSMSDIR))
     CALL CRAY_SYSTEM_CALL(TRIM(MKMDMDIR))
     CALL CRAY_SYSTEM_CALL(TRIM(MKTSODIR))
     CALL CRAY_SYSTEM_CALL(TRIM(MKNCDDIR))

#    endif
   END IF
#  endif

# if defined (MULTIPROCESSOR)
   CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
# endif
   IF(MSR)WRITE(IPT,*)
!
!---------CHECK EXISTENCE OF STANDARD FILES-AND OPEN---------------------------!
!
   ISTR = "./"//TRIM(INPDIR)//"/"//trim(casename)
   OSTR = "./"//TRIM(OUTDIR)//"/"//"out/"//trim(casename)
   CALL FOPEN(IOPRT, TRIM(OSTR)//'_prt.dat',"ofr")
   CALL FOPEN(INOBC, TRIM(ISTR)//'_obc.dat',"cfr")
   CALL FOPEN(INRIV, TRIM(ISTR)//'_riv.dat',"cfr")
   CALL FOPEN(INBFW, TRIM(ISTR)//'_bfw.dat',"cfr")

!
!-----------------------FILES FOR ARCHIVING------------------------------------!
!
   CALL FOPEN(IOTSR,TRIM(OSTR)//'_tsr.dat' ,"ofr")

   RETURN
   END SUBROUTINE IOFILES
!==============================================================================!
