MODULE mod_ncdout
!==============================================================================!
!  NetCDF Output for FVCOM using CF Metadata Convention                        !
!                                                                              !
!    see: http://www.cgd.ucar.edu/cms/eaton/cf-metadata/ for info              !
!                                                                              !
!    current time dependent variables set up                                   !
!         el:    surface elevation                                             !
!          u:    x-velocity. In spherical coordinate,lon-velocity              !                         
!          v:    y-velocity. In spherical coordinate,lat-velocity              !                        
!         ww:    z-velocity                                                    !
!         kh:    turbulent diffusivity                                         !
!         km:    turbulent viscosity                                           !
!         t1:    temperature                                                   !
!         s1:    salinity                                                      !
!         ua:    vertically-averaged x-velocity                                !
!                In spherical coordinate,vertically-averaged lon-velocity      !
!         va:    vertically-averaged y-velocity                                !
!                In spherical coordinate,vertically-averaged lat-velocity      !
!          d:    depth at nodes                                                !
!        dye:    dye at nodes                                                  !
!       aice:    ice concentration on nodes
!       vice:    ice thichness on nodes
!      uuice:    ice x-velocity 
!      vvice:    ice y-velocity   
!     uuwind:    wind speed in x direction
!     vvwind:    wind speed in y direction

!       wd:      wet/dry flag (0 or 1)
!                                                                              !
!    to add additional variables:                                              !
!      1.) add to list above                                                   !
!      2.) add *_vid to variables vid in section "new variable vid"            !
!      3.) go to definition section "new variable definition"                  !
!      4.) add output section "new variable output"                            !
!==============================================================================!

   USE MOD_PREC
   USE NETCDF
   USE MOD_1D

   IMPLICIT NONE
   SAVE

!--Control Variables----------------------------------------------!
   LOGICAL,PUBLIC :: CDF_OUT            !!true to activate netcdf input/output
   integer,private :: nout_vars          !!number of variables to output
   integer,public :: cdf_int            !!output every cdf_int iterations
   integer,private :: cdf_stk            !!cdf_stk outputs are put in each file
   !!CDF_STK=0: ALL OUTPUTS IN SINGLE FILE
   integer,private :: stck_cnt           !!counts number of outputs in each file
   integer,private :: out_cnt            !!counts number of outputs
   character(len=120),private :: cdfname !!netcdf file name
   character(len=80),private, allocatable, dimension(:) :: cdf_vdp

!--NetCDF IDs----------------------------------------------------!

   !--NetCDF File 
   integer,private :: nc_ofid

   !--Dimensions
   integer,private :: node_did,nele_did
   integer,private :: scl_did,siglay_did,siglev_did
   integer,private :: three_did,four_did
   integer,private :: time_did

   !--Grid Variables
   integer,private :: nprocs_vid,partition_vid
   integer,private :: idens_vid
   integer,private :: x_vid,y_vid,lat_vid,lon_vid
   integer,private :: nv_vid,nbe_vid
!JQI   integer,private :: aw0_vid,awx_vid,awy_vid
!JQI   integer,private :: a1u_vid,a2u_vid
   integer,private :: siglay_vid,siglev_vid,siglay_shift_vid
  
   !--Flow Variables 
   integer,private :: time_vid
   integer,private :: iint_vid
   integer,private :: u_vid
   integer,private :: v_vid
   integer,private :: wd_vid
   integer,private :: ww_vid
   integer,private :: s1_vid
   integer,private :: t1_vid
   integer,private :: el_vid
   integer,private :: h_vid
   integer,private :: km_vid
   integer,private :: kh_vid
   integer,private :: ua_vid
   integer,private :: va_vid
   integer,private :: d_vid
   ! new variable vid  
   ! add *_vid here, e.g. for rho, add rho_vid

   CHARACTER(LEN=120) :: fldnam    !,CDFNAME_BIO   !BioGen
   INTEGER :: bio_ofid,ii,mm,stck_bio          !BioGen

   integer,private :: uuwind_vid,vvwind_vid


   !--Info Variables
   character(len=120),public :: institution
   character(len=120),public :: netcdf_timestring 

   INTEGER, ALLOCATABLE :: trcsid(:)


   INTERFACE PUTVAR
     MODULE PROCEDURE PUTVAR1D_INT
     MODULE PROCEDURE PUTVAR1D_REAL
     MODULE PROCEDURE PUTVAR2D_INT
     MODULE PROCEDURE PUTVAR2D_REAL
   END INTERFACE


   CONTAINS !------------------------------------------------------------------!
            ! handle_ncerr        :   deal with netcdf error                   !
            ! set_ncd_io          :   read assimilation parameters from input  !
            ! write_netcdf_setup  :   set up dimensioning and write grid       !
            ! out_netcdf          :   write time dependent data                ! 
            ! putvar              :   collect variable to global and dump      ! 
            ! -----------------------------------------------------------------!

!==============================================================================|
!==============================================================================|

!------------------------------------------------------------------------------|
!  CHECK NETCDF ERROR AND WRITE MESSAGE TO FILE UNIT IPT                       |
!------------------------------------------------------------------------------|
   SUBROUTINE HANDLE_NCERR(STATUS,ERRMSGE,IPT)
   INTEGER, INTENT(IN) :: STATUS,IPT
   CHARACTER(LEN=*)    :: ERRMSGE
   IF(STATUS /=NF90_NOERR)THEN
     WRITE(IPT,*)TRIM(ERRMSGE)
     WRITE(IPT,*)TRIM(NF90_STRERROR(STATUS))
     CALL PSTOP
   END IF
   END SUBROUTINE HANDLE_NCERR

!==============================================================================|

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!

!==============================================================================|
!  READ IN PARAMETERS CONTROLLING INPUT/OUTPUT FROM RUNTIME PARAM FILE         |
!==============================================================================|
   SUBROUTINE set_ncd_io   
   use mod_prec
   use all_vars
   use mod_inp
   use netcdf
   implicit none
!--Local Vars----------------------------|
   real(sp)           :: realvec(150)
   integer            :: intvec(150)
   integer            :: iscan
   character(len=120) :: fname
   character(len=80), dimension(100) :: charvec
   integer            :: i
!----------------------------------------|


   out_cnt = 0

   fname = "./"//trim(casename)//"_run.dat"

!------------------------------------------------------------------------------|
!   cdf_out: netcdf activation flag        
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(TRIM(FNAME),"CDF_OUT",LVAL = CDF_OUT)
   if(iscan /= 0)then
     write(ipt,*)'error reading cdf_out: ',iscan
     if(iscan == -2)then
       write(ipt,*)'variable not found in input file: ',trim(fname)
     end if
     call pstop
   end if

!------------------------------------------------------------------------------|
!  cdf_int: output is performed every cdf_int iterations
!------------------------------------------------------------------------------|

   ISCAN = SCAN_FILE(TRIM(FNAME),"CDF_INT",ISCAL = CDF_INT)
   if(iscan /= 0)then
     write(ipt,*)'error reading cdf_int: ',iscan
     if(iscan == -2)then
       write(ipt,*)'variable not found in input file: ',trim(fname)
     end if
     call pstop
   end if

!------------------------------------------------------------------------------|
!  cdf_stk: # dumps / file                                
!------------------------------------------------------------------------------|

   ISCAN = SCAN_FILE(TRIM(FNAME),"CDF_STK",ISCAL = CDF_STK)
   if(iscan /= 0)then
     write(ipt,*)'error reading cdf_stk: ',iscan
     if(iscan == -2)then
       write(ipt,*)'variable not found in input file: ',trim(fname)
     end if
     call pstop
   end if
   

!------------------------------------------------------------------------------|
!     cdf_vdp: list of variables to write to output file
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(TRIM(FNAME),"CDF_VDP",CVEC = CHARVEC,NSZE = NOUT_VARS)
   if(iscan /= 0)then
     write(ipt,*)'error reading cdf_vdp: ',iscan
     call pstop
   end if
   if(nout_vars <= 0)then
     write(ipt,*)'incorrect number of netcdf cdf_vdp variables specified'
     write(ipt,*)'in input file',nout_vars
     call pstop
   end if

   allocate(cdf_vdp(nout_vars))
   cdf_vdp(1:nout_vars)= charvec(1:nout_vars)

!------------------------------------------------------------------------------|
!            SCREEN REPORT OF SET VARIABLES                                    !
!------------------------------------------------------------------------------|
   if(msr)then
     write(ipt,*)''
     write(ipt,*)'!        netcdf parameters                  '
     if(cdf_out)then
       write(ipt,*)'!  netcdf i/o            :  active'
       write(ipt,*)'!  output every # its    : ',cdf_int
       write(ipt,*)'!  # dumps / file        : ',cdf_stk
       write(ipt,*)'!  # variables to write  : ',nout_vars
       do i=1,nout_vars
         write(ipt,999)i,trim(cdf_vdp(i))
       end do
     else
       write(ipt,*)'!  # netcdf i/o          :  not active'
     end if
   end if


   return
   999 format(' !  variable #',i4,'        :',a13)
   END SUBROUTINE set_ncd_io  
!==============================================================================|

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|

!==============================================================================|
!  Write NetCDF Header and Static Variables                                    |
!==============================================================================|
   SUBROUTINE write_netcdf_setup(filecnt) 

   use all_vars

#  if defined (MULTIPROCESSOR)
   use mod_par 
#  endif
   use netcdf
   use mod_types
   use mod_utils
   implicit none
   integer, intent(in)   :: filecnt
   integer, dimension(3) :: dynm3de_lev,dynm3de_lay
   integer, dimension(3) :: dynm3dn_lev,dynm3dn_lay
   integer, dimension(2) :: stat3de_lev,stat3de_lay 
   integer, dimension(2) :: stat3dn_lev,stat3dn_lay 
   integer, dimension(2) :: specdim
   integer, dimension(2) :: dynm2de,dynm2dn
   integer, dimension(1) :: stat2de,stat2dn
   integer, dimension(1) :: stat_lev,stat_lay,dynmtime ,stat_scl
   character(len=100)    :: netcdf_convention
   character(len=100)    :: timestamp ,temp ,time_bio
   integer               :: i,j,ierr,i1,i2
   integer               :: maxnode,maxnodep,maxelem,maxelemp,itmp
   real(sp), allocatable :: tmp(:,:),tvec(:)
   integer, allocatable  :: tmpint(:,:)
   character(len=4)      :: nchar
   integer               :: ii,mm
   
   REAL(SP), ALLOCATABLE :: VXY_TMP(:)

!==============================================================================|

!==============================================================================|
!  Set up Constants and Initialize Counters                                    |
!==============================================================================|

!--Initialize Stack Count
   stck_cnt = 1

!--NetCDF Convention String
   netcdf_convention = 'CF-1.0'

!--Time Stamp for History
   call get_timestamp(temp)
   timestamp = 'model started at: '//trim(temp)

   ALLOCATE (trcsid(ntt))

!==============================================================================|
!  OPEN FILE AND DEFINE VARIABLES                                              |
!==============================================================================|
   if(msr)then

!--Define NetCDF Output Filename 
   write(nchar,'(I4)')filecnt
   if(filecnt < 10)then
     cdfname = trim(outdir)//"/netcdf/"//trim(casename)//'_000'//trim(adjustl(nchar))//'.nc'
   elseif(filecnt < 100)then
     cdfname = trim(outdir)//"/netcdf/"//trim(casename)//'_00'//trim(adjustl(nchar))//'.nc'
   elseif(filecnt < 1000)then
     cdfname = trim(outdir)//"/netcdf/"//trim(casename)//'_0'//trim(adjustl(nchar))//'.nc'
   elseif(filecnt < 10000)then
     cdfname = trim(outdir)//"/netcdf/"//trim(casename)//'_'//trim(adjustl(nchar))//'.nc'
   else
     write(*,*)'error in netcdf module'
     write(*,*)'# history files > 10000'
     stop
   endif

!--Create File 
   ierr = nf90_create(path=cdfname,cmode=nf90_clobber,ncid=nc_ofid)
   if(ierr /= nf90_eexist)then
     call handle_ncerr(ierr,"file creation error",ipt)
   else
     write(ipt,*)'file :',cdfname,' already exists'
     write(ipt,*)'exiting'
     stop
   end if

!--Description of File Contents
   ierr = nf90_put_att(nc_ofid,nf90_global,"title"      ,trim(casetitle))
   ierr = nf90_put_att(nc_ofid,nf90_global,"institution",trim(institution))
   ierr = nf90_put_att(nc_ofid,nf90_global,"source"     ,trim(fvcom_version))
   ierr = nf90_put_att(nc_ofid,nf90_global,"history"    ,trim(timestamp))
   ierr = nf90_put_att(nc_ofid,nf90_global,"references" ,trim(fvcom_website))
   ierr = nf90_put_att(nc_ofid,nf90_global,"Conventions",trim(netcdf_convention))
#  if defined (SPHERICAL)
   ierr = nf90_put_att(nc_ofid,nf90_global,"CoordinateSystem","GeoReferenced")
#endif



!--Define Fixed Model Dimensions 
   ierr = nf90_def_dim(nc_ofid,"scalar" ,1      ,scl_did    )        
   ierr = nf90_def_dim(nc_ofid,"node"   ,mgl    ,node_did   )        
   ierr = nf90_def_dim(nc_ofid,"nele"   ,ngl    ,nele_did   )
   ierr = nf90_def_dim(nc_ofid,"siglay" ,kbm1   ,siglay_did )
   ierr = nf90_def_dim(nc_ofid,"siglev" ,kb     ,siglev_did )
   ierr = nf90_def_dim(nc_ofid,"three"  ,3      ,three_did  )
   ierr = nf90_def_dim(nc_ofid,"four"   ,4      ,four_did   )

!--Define Unlimited Model Dimension
   ierr = nf90_def_dim(nc_ofid,"time"   ,nf90_unlimited,time_did)

!--Set Up Data Dimensioning - Static Vars
   stat_scl     = (/scl_did/)             !!scalar variable               
   stat_lay     = (/siglay_did/)          !!vertical variables at layers
   stat_lev     = (/siglev_did/)          !!vertical variables at levels
   stat2de      = (/nele_did/)            !!2d element vars
   stat2dn      = (/node_did/)            !!2d nodal vars
   stat3de_lay  = (/nele_did,siglay_did/) !!3d element vars at layers
   stat3de_lev  = (/nele_did,siglev_did/) !!3d element vars at levels
   stat3dn_lay  = (/node_did,siglay_did/) !!3d node    vars at layers
   stat3dn_lev  = (/node_did,siglev_did/) !!3d node    vars at levels

!--Set Up Data Dimensioning - Dynamic Vars 
   dynm2de      = (/nele_did,time_did/)            !!2d element vars
   dynm2dn      = (/node_did,time_did/)            !!2d nodal vars
   dynm3de_lay  = (/nele_did,siglay_did,time_did/) !!3d elem vars at layers
   dynm3de_lev  = (/nele_did,siglev_did,time_did/) !!3d elem vars at levels
   dynm3dn_lay  = (/node_did,siglay_did,time_did/) !!3d node vars at layers
   dynm3dn_lev  = (/node_did,siglev_did,time_did/) !!3d node vars at levels
   dynmtime     = (/time_did/)   

!--Define Coordinate Variables and Attributes
   !!====NPROCS: Number of Processors=======================!
   ierr = nf90_def_var(nc_ofid,"nprocs",nf90_int,stat_scl,nprocs_vid)
   ierr = nf90_put_att(nc_ofid,nprocs_vid,"long_name","number of processors")

   !!====PARTITION: Partion Number of Element===============!
   ierr = nf90_def_var(nc_ofid,"partition",nf90_int,stat2de,partition_vid)
   ierr = nf90_put_att(nc_ofid,partition_vid,"long_name","partition")
   ierr = nf90_put_att(nc_ofid,partition_vid,"grid","TWOD_MESH")

!JQI   !!====Initial Density (Used for Constructing 3D Domain)==!
!   ierr = nf90_def_var(nc_ofid,"Initial_Density",nf90_float,stat3dn_lay,idens_vid)
!   ierr = nf90_put_att(nc_ofid,idens_vid,"long_name","Initial Density")
!JQI  ierr = nf90_put_att(nc_ofid,idens_vid,"grid","SigmaLayer_Mesh")

   !!====X Grid Coordinate at Nodes (VX) (Meters)===========!
   ierr = nf90_def_var(nc_ofid,"x",nf90_float,stat2dn,x_vid)
   ierr = nf90_put_att(nc_ofid,x_vid,"long_name","nodal x-coordinate")
   ierr = nf90_put_att(nc_ofid,x_vid,"units","meters")
   ierr = nf90_put_att(nc_ofid,x_vid,"grid","TWOD_MESH")

   !!====Y Grid Coordinate at Nodes (VY) (Meters)===========!
   ierr = nf90_def_var(nc_ofid,"y",nf90_float,stat2dn,y_vid)
   ierr = nf90_put_att(nc_ofid,y_vid,"long_name","nodal y-coordinate")
   ierr = nf90_put_att(nc_ofid,y_vid,"units","meters")
   ierr = nf90_put_att(nc_ofid,y_vid,"grid","TWOD_MESH")

   !!====Longitudinal Coordinate at Nodes (LON) (degrees)===!
   ierr = nf90_def_var(nc_ofid,"lon",nf90_float,stat2dn,lon_vid)
   ierr = nf90_put_att(nc_ofid,lon_vid,"long_name","Longitude")
   ierr = nf90_put_att(nc_ofid,lon_vid,"standard_name","longitude")
   ierr = nf90_put_att(nc_ofid,lon_vid,"units","degrees_east")
   ierr = nf90_put_att(nc_ofid,lon_vid,"grid","Bathymetry_Mesh")

   !!====Latitudinal  Coordinate at Nodes (LAT) (degrees)===!
   ierr = nf90_def_var(nc_ofid,"lat",nf90_float,stat2dn,lat_vid)
   ierr = nf90_put_att(nc_ofid,lat_vid,"long_name","Latitude")
   ierr = nf90_put_att(nc_ofid,lat_vid,"standard_name","latitude")
   ierr = nf90_put_att(nc_ofid,lat_vid,"units","degrees_north")
   ierr = nf90_put_att(nc_ofid,lat_vid,"grid","Bathymetry_Mesh")

   !!====Sigma Coordinate for Sigma Layers (ZZ)  (-)========!
   ierr = nf90_def_var(nc_ofid,"siglay",nf90_float,stat3dn_lay,siglay_vid)
   ierr = nf90_put_att(nc_ofid,siglay_vid,"long_name","Sigma Layers")
   ierr = nf90_put_att(nc_ofid,siglay_vid,"standard_name","ocean_sigma/general_coordinate")
   ierr = nf90_put_att(nc_ofid,siglay_vid,"positive","up")
   ierr = nf90_put_att(nc_ofid,siglay_vid,"valid_min","-1")
   ierr = nf90_put_att(nc_ofid,siglay_vid,"valid_max","0")
   ierr = nf90_put_att(nc_ofid,siglay_vid,"formula_terms","siglay:siglay eta:zeta depth:depth")

   !!====Shifted Sigma Layer Coordinate for Viz ============!
   ierr = nf90_def_var(nc_ofid,"siglay_shift",nf90_float,stat3dn_lay,siglay_shift_vid)
   ierr = nf90_put_att(nc_ofid,siglay_shift_vid,"long_name","Shifted Sigma Layers")

   !!====Sigma Coordinate for Sigma Levels (Z)   (-)========!
   ierr = nf90_def_var(nc_ofid,"siglev",nf90_float,stat3dn_lev,siglev_vid)
   ierr = nf90_put_att(nc_ofid,siglev_vid,"long_name","Sigma Levels")
   ierr = nf90_put_att(nc_ofid,siglev_vid,"standard_name","ocean_sigma/general_coordinate")
   ierr = nf90_put_att(nc_ofid,siglev_vid,"positive","up")
   ierr = nf90_put_att(nc_ofid,siglev_vid,"valid_min","-1")
   ierr = nf90_put_att(nc_ofid,siglev_vid,"valid_max","0")
   ierr = nf90_put_att(nc_ofid,siglev_vid,"formula_terms","siglev:siglev eta:zeta depth:depth")



!--Define Mesh Relevant Variables and Attributes

   !!====Bathymetry at Nodes (H) (meters)===================!
   ierr = nf90_def_var(nc_ofid,"h",nf90_float,stat2dn,h_vid)
   ierr = nf90_put_att(nc_ofid,h_vid,"long_name","Bathymetry")   
   ierr = nf90_put_att(nc_ofid,h_vid,"units","meters")
   ierr = nf90_put_att(nc_ofid,h_vid,"positive","down")
   ierr = nf90_put_att(nc_ofid,h_vid,"standard_name","depth")
   ierr = nf90_put_att(nc_ofid,h_vid,"grid","fvcom_grid")

   !!====Nodes surrounding each Element (NV)================!
   specdim = (/nele_did,three_did/) 
   ierr = nf90_def_var(nc_ofid,"nv",nf90_int,specdim,nv_vid)
   ierr = nf90_put_att(nc_ofid,nv_vid,"long_name","nodes surrounding element")     

!JQI   !!====Momentum Stencil Interpolation Coefficients========!
!JQI   specdim = (/nele_did,four_did/) 
!JQI   ierr = nf90_def_var(nc_ofid,"a1u",nf90_float,specdim,a1u_vid)
!JQI   ierr = nf90_put_att(nc_ofid,a1u_vid,"long_name","a1u")
!JQI   ierr = nf90_def_var(nc_ofid,"a2u",nf90_float,specdim,a2u_vid)
!JQI   ierr = nf90_put_att(nc_ofid,a2u_vid,"long_name","a2u")

!JQI   !!====Element Based Interpolation Coefficients===========!
!JQI   specdim = (/nele_did,three_did/) 
!JQI   ierr = nf90_def_var(nc_ofid,"aw0",nf90_float,specdim,aw0_vid)
!JQI   ierr = nf90_put_att(nc_ofid,aw0_vid,"long_name","aw0")
!JQI   ierr = nf90_def_var(nc_ofid,"awx",nf90_float,specdim,awx_vid)
!JQI   ierr = nf90_put_att(nc_ofid,awx_vid,"long_name","awx")
!JQI   ierr = nf90_def_var(nc_ofid,"awy",nf90_float,specdim,awy_vid)
!JQI   ierr = nf90_put_att(nc_ofid,awy_vid,"long_name","awy")

!--Define Model Time Variables and Attributes    
   ierr = nf90_def_var(nc_ofid,"time",nf90_float,dynmtime,time_vid)
   ierr = nf90_put_att(nc_ofid,time_vid,"long_name","Time")
   ierr = nf90_put_att(nc_ofid,time_vid,"units",trim(netcdf_timestring))
   ierr = nf90_put_att(nc_ofid,time_vid,"calendar","none")
   ierr = nf90_def_var(nc_ofid,"iint",nf90_int,dynmtime,iint_vid)
   ierr = nf90_put_att(nc_ofid,iint_vid,"long_name","internal mode iteration number")

!--Define Time Dependent Flow Variables (selected by user from input file)
   IERR = NF90_DEF_VAR(NC_OFID,"zeta",NF90_FLOAT,DYNM2DN,EL_VID)
   IERR = NF90_PUT_ATT(NC_OFID,EL_VID,"long_name","Sea Level")
   IERR = NF90_PUT_ATT(NC_OFID,EL_VID,"units","meters")
   IERR = NF90_PUT_ATT(NC_OFID,EL_VID,"grid","fvcom_grid")
   IERR = NF90_PUT_ATT(NC_OFID,EL_VID,"type","data")

   DO MM=1,NTT
     fldnam=TRIM(BIO_NAME(mm,1))
     ierr = nf90_def_var(nc_ofid,fldnam,nf90_float,dynm3dn_lay,ii)	  
     trcsid(mm)=ii
     ierr = nf90_put_att(nc_ofid,ii,"unit",TRIM(BIO_NAME(mm,2)))
     ierr = nf90_put_att(nc_ofid,ii,"long_name",TRIM(BIO_NAME(mm,3)))
     ierr = nf90_put_att(nc_ofid,ii,"grid","fvcom_grid")
     ierr = nf90_put_att(nc_ofid,ii,"type","data")
     print*,fldnam,trcsid(mm),nc_ofid,TRIM(BIO_NAME(mm,2)),TRIM(BIO_NAME(mm,3))
   end do

!ex  case("var") !!===============var====================================!
!ex  ierr = nf90_def_var(nc_ofid,"truevar",nf90_float,dimensions,var_vid)
!ex  ierr = nf90_put_att(nc_ofid,var_vid,"long_name","A Good Descriptive Name")
!ex  ierr = nf90_put_att(nc_ofid,var_vid,"units","UDUNITS compatible units")
!ex  ierr = nf90_put_att(nc_ofid,var_vid,"standard_name","CF-convention standard name")
!ex  ierr = nf90_put_att(nc_ofid,var_vid,"type","data")

     !    new variable definition
     !1.) add new definition above here by copying example above and modifying
     !2.) copy dimensions from variable which has same dimensions as var 
     !3.) change variable name if necessary to something more descriptive
     !   e.g. model name for temperature is t1, use temp instead 
     !4.) give the variable a reasonable "long_name"
     !5.) look up the variables standard_name from the cf-convention standard_name list
     !   http://www.cgd.ucar.edu/cms/eaton/cf-metadata/standard_name.html
     !   if it does not exist, do not provide a standard name attribute
     !6.) set variable units conforming to udunits standard   
     !   http://my.unidata.ucar.edu/content/software/udunits/index.html
      
!--Exit Define Mode
   ierr = nf90_enddef(nc_ofid)
   ierr = nf90_close(nc_ofid)

   end if !(msr)

!==============================================================================|
!  WRITE VARIABLES TO FILE                                                     |
!==============================================================================|
   if(msr)then
     ierr = nf90_open(cdfname,nf90_write,nc_ofid)
     if(ierr /= nf90_noerr)then
       call handle_ncerr(ierr,"file open error",ipt)
     end if
   end if
   
   !!====Longitude at Nodes (LON) ==========================!
   ALLOCATE(VXY_TMP(0:MT))
   VXY_TMP = VX + VXMIN
   i1 = lbound(vxy_tmp,1) ; i2 = ubound(vxy_tmp,1)
   call putvar(i1,i2,m,mgl,1,1,"n",vxy_tmp,nc_ofid,lon_vid,myid&
        &,nprocs,ipt, stck_cnt)
   DEALLOCATE(VXY_TMP)

   !!====Latitude  at Nodes (LAT) ==========================!
   ALLOCATE(VXY_TMP(0:MT))
   VXY_TMP = VY + VYMIN
   i1 = lbound(vxy_tmp,1) ; i2 = ubound(vxy_tmp,1)
   call putvar(i1,i2,m,mgl,1,1,"n",vxy_tmp,nc_ofid,lat_vid,myid&
        &,nprocs,ipt, stck_cnt) 
   DEALLOCATE(VXY_TMP)

   !!====Number of Processors (NPROCS) =====================!
   if(msr)then 
   ierr = nf90_put_var(nc_ofid,nprocs_vid,nprocs)
   if(ierr /= nf90_noerr)then
     call handle_ncerr(ierr,"error writing nprocs variable to netcdf",ipt)
   end if
#  if defined (MULTIPROCESSOR)
   ierr = nf90_put_var(nc_ofid,partition_vid,el_pid)
   if(ierr /= nf90_noerr)then
     call handle_ncerr(ierr,"error writing el_pid variable to netcdf",ipt)
   end if
#  endif
   end if

   !!====X Grid Coordinate at Nodes (VX)====================!
   ALLOCATE(VXY_TMP(0:MT))
   VXY_TMP = VX + VXMIN
   i1 = lbound(vxy_tmp,1) ; i2 = ubound(vxy_tmp,1)
   call putvar(i1,i2,m,mgl,1,1,"n",vxy_tmp,nc_ofid,x_vid,myid,nprocs&
        &,ipt, stck_cnt) 
   DEALLOCATE(VXY_TMP)

   !!====Y Grid Coordinate at Nodes (VY)====================!
   ALLOCATE(VXY_TMP(0:MT))
   VXY_TMP = VY + VYMIN
   i1 = lbound(vxy_tmp,1) ; i2 = ubound(vxy_tmp,1)
   call putvar(i1,i2,m,mgl,1,1,"n",vxy_tmp,nc_ofid,y_vid,myid,nprocs&
        &,ipt, stck_cnt) 
   DEALLOCATE(VXY_TMP)

   !!====Bathymetry at Nodes (H)============================!
   i1 = lbound(h,1) ; i2 = ubound(h,1)
   call putvar(i1,i2,m,mgl,1,1,"n",h,nc_ofid,h_vid,myid,nprocs,ipt,&
        & stck_cnt) 

   !!====Nodes surrounding each Element (NV)================!
   allocate(tmpint(0:nt,3))
   if(serial)then
     tmpint(0:nt,1:3) = nv(0:nt,1:3) 
   end if
#  if defined (MULTIPROCESSOR)
   if(par)then
   do j=1,3
   do i=1,n
     tmpint(i,j) = ngid(nv(i,j))
   end do
   end do
   end if
#  endif
   i1 = lbound(tmpint,1) ; i2 = ubound(tmpint,1)
   call putvar(i1,i2,n,ngl,3,3,"e",tmpint,nc_ofid,nv_vid,myid,nprocs&
        &,ipt, stck_cnt) 
   deallocate(tmpint)

   !!====Sigma Layers (z)==================================!
   i1 = lbound(zz,1) ; i2 = ubound(zz,1)
   call putvar(i1,i2,m,mgl,kb-1,kb-1,"n",zz,nc_ofid,siglay_vid,myid&
        &,nprocs,ipt, stck_cnt) 

   !!====Sigma Layers Shift(zz)==================================!
   allocate(tmp(0:mt,kbm1))
   tmp(:,1:kbm1) = z(:,2:kb)
   i1 = lbound(tmp,1) ; i2 = ubound(tmp,1)
   call putvar(i1,i2,m,mgl,kb-1,kb-1,"n",tmp,nc_ofid,siglay_shift_vid&
        &,myid,nprocs,ipt, stck_cnt) 
   deallocate(tmp)

   !!====Sigma Levels (z)==================================!
   i1 = lbound(z,1) ; i2 = ubound(z,1)
   call putvar(i1,i2,m,mgl,kb,kb,"n",z,nc_ofid,siglev_vid,myid,nprocs&
        &,ipt, stck_cnt) 

   
!==============================================================================|
!  close the file                                                              |
!==============================================================================|

   if(msr) ierr = nf90_close(nc_ofid)

   return
   end subroutine write_netcdf_setup
!==============================================================================|


!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%|


   subroutine out_netcdf 
!==============================================================================|
!   Write Time Dependent NetCDF Data to File                                   |
!==============================================================================|

   use all_vars
   use netcdf
   USE MOD_BIO_3D

   implicit none
   integer :: i,ierr,i1,i2,k,icheck     !JQI,ii
   integer :: dims(1)
   real(sp), allocatable :: ftemp(:)
   REAL(SP), ALLOCATABLE :: BIO_ALL_TMP(:,:)
!==============================================================================|
   

!--Update Counter
   out_cnt = out_cnt + 1
   stck_cnt = stck_cnt + 1 

!--Write Header information if first output of file
   if(cdf_stk == 0)then
     if(out_cnt == 1) call write_netcdf_setup(1)
   else
     icheck = mod(out_cnt-1,cdf_stk)
     if(icheck ==0 .or. out_cnt==1)call write_netcdf_setup((out_cnt-1)/cdf_stk+1)
   endif

   dims(1) = stck_cnt

!--Open File
   if(msr)then
     ierr = nf90_open(cdfname,nf90_write,nc_ofid)
     if(ierr /= nf90_noerr)then
       call handle_ncerr(ierr,"file open error",ipt)
     end if

!--Dump Time/IINT to File
      ierr    = nf90_put_var(nc_ofid,iint_vid,iint,START=dims)
      ierr    = nf90_put_var(nc_ofid,time_vid,thour*3600.,START=dims)
   end if

!--Dump EL to File
   I1 = LBOUND(EL,1) ; I2 = UBOUND(EL,1)
   CALL PUTVAR(I1,I2,M,MGL,1,1,"n",EL,NC_OFID,EL_VID,MYID,NPROCS,IPT,STCK_CNT)

!--Write Variables to File
   if(msr) write(ipt,*)'dumping to netcdf file: ',trim(cdfname),stck_cnt

   do ii=1,ntt
     ALLOCATE(BIO_ALL_TMP(0:MT,KB))
     BIO_ALL_TMP(:,:) = BIO_ALL(:,:,II)
     I1 = LBOUND(BIO_ALL_TMP,1) ; I2 = UBOUND(BIO_ALL_TMP,1)
     mm=trcsid(ii)
     CALL putvar(i1,i2,m,mgl,kb,kb-1,"n",BIO_ALL_TMP,nc_ofid,mm,MYID &
          &,NPROCS,IPT,stck_cnt)
     DEALLOCATE(BIO_ALL_TMP)	  
   end do

!ex     case("s1") !!===============S1======================================!
!ex       i1 = lbound(s1,1) ; i2 = ubound(s1,1)
!ex       call putvar(i1,i2,m,mgl,kb,kb-1,"n",s1,nc_ofid,s1_vid,myid
       !,nprocs,ipt, stck_cnt) 

     !new variable output - add a new variable (e.g. 'var') to output
     !1.) copy example section above
     !2.) modify case for your variable 'case("var")' 
     !3.) modify bounds for your variable
     !4.) modify putvar for your variable by finding a putvar for a variable
     !    with same dimensions and type ("e" or "n")
     !5.) modify variable vid with your variables vid (e.g. "var_vid")

!==============================================================================|
!  CLOSE THE FILE                                                              |
!==============================================================================|

   if(msr) ierr = nf90_close(nc_ofid)
   return
 end subroutine out_netcdf
 
 
 !==============================================================================|
 !  Collect Data to Global Array and Write to Netcdf File                       |
 !==============================================================================|
 
 
 ! David added interface to putvar to select the right subroutine for
 ! the data type.                                       
 
 SUBROUTINE PUTVAR1D_REAL(i1,i2,n1,n1gl,kt,k1,map_type,var,nc_fid,vid&
      &,myid,nprocs,ipt,stk)
   
   !------------------------------------------------------------------------------|
   implicit none
   integer, intent(in) :: i1,i2,n1,n1gl,kt,k1,nc_fid,vid,myid,nprocs&
        &,ipt,stk
   character(len=*),intent(in)   :: map_type
   real(sp), dimension(i1:i2) :: var
   
   real(sp), allocatable, dimension(:,:) :: temp

   allocate(temp(i1:i2,kt))
   temp(i1:i2,1)=var

   CALL PUTVAR2D_REAL(i1,i2,n1,n1gl,kt,k1,map_type,temp,nc_fid,vid&
        &,myid,nprocs,ipt,stk)
   
   deallocate(temp)

 END SUBROUTINE PUTVAR1D_REAL
   

 subroutine PUTVAR2D_REAL(i1,i2,n1,n1gl,kt,k1,map_type,var,nc_fid,vid&
      &,myid,nprocs,ipt,stk)
!------------------------------------------------------------------------------|

#  if defined (MULTIPROCESSOR)
   use mod_par
#  endif
   use mod_types
   implicit none
   integer, intent(in) :: i1,i2,n1,n1gl,kt,k1,nc_fid,vid,myid,nprocs&
        &,ipt, stk
   character(len=*),intent(in)   :: map_type
   real(sp), dimension(i1:i2,kt) :: var

   real(sp), allocatable, dimension(:,:) :: temp,gtemp
   integer :: ierr,k1m1
   integer, allocatable :: dims(:)
   

   k1m1 = k1 
   if(k1m1 == 1)then
     allocate(dims(2))
     dims(1) = 1 
     dims(2) = stk
   else
     allocate(dims(3))
     dims(1) = 1 
     dims(2) = 1 
     dims(3) = stk      
   end if
     

   if(map_type(1:1) /= "e" .and. map_type(1:1) /= "n")then
     write(ipt,*)'map_type input to putvar should be "e" OR "n"'
     call pstop
   end if

   if(nprocs==1)then
     allocate(temp(n1,k1m1))  ; temp(1:n1,1:k1m1) = var(1:n1,1:k1m1)
   end if

#  if defined (MULTIPROCESSOR)
   if(nprocs > 1)then
     allocate(gtemp(n1gl,kt))
     if(map_type(1:1) == "e")then
       call gather(i1,i2,n1,n1gl,kt,myid,nprocs,emap,var,gtemp)
     else 
       call gather(i1,i2,n1,n1gl,kt,myid,nprocs,nmap,var,gtemp)
     end if
     allocate(temp(n1gl,k1m1))  ; temp(1:n1gl,1:k1m1) = gtemp(1:n1gl,1:k1m1)
     deallocate(gtemp)
   end if
#  endif

!   if(myid /= 1) return
   if(myid == 1) then
     ierr = nf90_put_var(nc_fid,vid,temp,START=dims)
     if(ierr /= nf90_noerr)then
       call handle_ncerr(ierr,"error writing variable to netcdf",ipt)
     end if
   end if  
   deallocate(dims,temp)

   return
 end subroutine PUTVAR2D_REAL
!==============================================================================|

 SUBROUTINE PUTVAR1D_INT(i1,i2,n1,n1gl,kt,k1,map_type,var,nc_fid,vid&
      &,myid,nprocs,ipt,stk )
   
   !------------------------------------------------------------------------------|
   implicit none
   integer, intent(in) :: i1,i2,n1,n1gl,kt,k1,nc_fid,vid,myid,nprocs&
        &,ipt, stk
   character(len=*),intent(in)   :: map_type
   INTEGER, dimension(i1:i2) :: var
   
   INTEGER, allocatable, dimension(:,:) :: temp
   
   allocate(temp(i1:i2,kt))
   temp(i1:i2,kt)= var
   
   call PUTVAR2D_INT(i1,i2,n1,n1gl,kt,k1,map_type,temp,nc_fid,vid&
        &,myid,nprocs,ipt, stk)
   
   deallocate(temp)
   
 END SUBROUTINE PUTVAR1D_INT
 
 subroutine PUTVAR2D_INT(i1,i2,n1,n1gl,kt,k1,map_type,var,nc_fid,vid&
      &,myid,nprocs,ipt, stk)
   
   !------------------------------------------------------------------------------|
   
#  if defined (MULTIPROCESSOR)
   use mod_par
#  endif
!   use mod_types
   implicit none
   integer, intent(in) :: i1,i2,n1,n1gl,kt,k1,nc_fid,vid,myid,nprocs&
        &,ipt,stk
   character(len=*),intent(in)   :: map_type
   INTEGER, dimension(i1:i2,kt) :: var

   INTEGER, allocatable, dimension(:,:) :: temp,gtemp
   integer :: ierr,k1m1
   integer, allocatable :: dims(:)
   

   k1m1 = k1 
   if(k1m1 == 1)then
     allocate(dims(2))
     dims(1) = 1 
     dims(2) = stk
   else
     allocate(dims(3))
     dims(1) = 1 
     dims(2) = 1 
     dims(3) = stk 
   end if
     

   if(map_type(1:1) /= "e" .and. map_type(1:1) /= "n")then
     write(ipt,*)'map_type input to putvar should be "e" OR "n"'
     call pstop
   end if

   if(nprocs==1)then
     allocate(temp(n1,k1m1))  ; temp(1:n1,1:k1m1) = var(1:n1,1:k1m1)
   end if

#  if defined (MULTIPROCESSOR)
   if(nprocs > 1)then
     allocate(gtemp(n1gl,kt))
     if(map_type(1:1) == "e")then
       call igather(i1,i2,n1,n1gl,kt,myid,nprocs,emap,var,gtemp)
     else 
       call igather(i1,i2,n1,n1gl,kt,myid,nprocs,nmap,var,gtemp)
     end if
     allocate(temp(n1gl,k1m1))  ; temp(1:n1gl,1:k1m1) = gtemp(1:n1gl,1:k1m1)
     deallocate(gtemp)
   end if
#  endif

!   if(myid /= 1) return
   if(myid == 1) then
     ierr = nf90_put_var(nc_fid,vid,temp,START=dims)
     if(ierr /= nf90_noerr)then
       call handle_ncerr(ierr,"error writing variable to netcdf",ipt)
     end if
   end if  
   deallocate(dims,temp)

   return
 end subroutine PUTVAR2D_INT
!==============================================================================|

   END MODULE mod_ncdout
