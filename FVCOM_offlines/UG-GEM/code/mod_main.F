!==============================================================================|
!   GLOBAL LIMITS AND ARRAY SIZING PARAMETERS                                  !
!==============================================================================|

MODULE LIMS
   USE MOD_PREC
   IMPLICIT NONE
   SAVE
 
   INTEGER NGL                !!GLOBAL NUMBER OF ELEMENTS
   INTEGER MGL                !!GLOBAL NUMBER OF NODES
   INTEGER IBFW_GL            !!GLOBAL NUMBER OF GROUNDWATER NODES
   INTEGER NUMQBC_GL          !!GLOBAL NUMBER OF FRESHWATER INFLOW NODES

   INTEGER N                  !!LOCAL NUMBER OF ELEMENTS 
   INTEGER M                  !!LOCAL NUMBER OF NODES
   INTEGER IBFW               !!LOCAL NUMBER OF GROUNDWATER NODES
   INTEGER NUMQBC             !!LOCAL NUMBER OF FRESHWATER INFLOW NODES
   INTEGER NISBCE_1           !!LOCAL NUMBER OF ELEMENTS WITH ISBCE = 1
   INTEGER NISBCE_2           !!LOCAL NUMBER OF ELEMENTS WITH ISBCE = 2
   INTEGER NISBCE_3           !!LOCAL NUMBER OF ELEMENTS WITH ISBCE = 3

   INTEGER KB                 !!NUMBER OF SIGMA LEVELS
   INTEGER KBM1               !!NUMBER OF SIGMA LEVELS-1
   INTEGER KBM2               !!NUMBER OF SIGMA LEVELS-2
   INTEGER MYID               !!UNIQUE PROCESSOR ID (1 => NPROCS)
   INTEGER KSL                !!NUMBER OF STANDARD SEA LEVELS 
   INTEGER NPROCS             !!NUMBER OF PROCESSORS
   INTEGER NE                 !!NUMBER OF UNIQUE EDGES (LOCAL DOMAIN ONLY)
   INTEGER NCV                !!NUMBER OF INTERNAL CONTROL VOLUMES (EXTENDED LOCAL ONLY)
   
   INTEGER NCV_I              !!NUMBER OF INTERNAL CONTROL VOLUMES (LOCAL ONLY)
   INTEGER NT                 !!TOTAL OF LOCAL INTERNAL + HALO ELEMENTS
   INTEGER MT                 !!TOTAL OF LOCAL INTERNAL + HALO NODES
   INTEGER MX_NBR_ELEM        !!MAX NUMBER OF ELEMENTS SURROUNDING A NODE

END MODULE LIMS



!==============================================================================|
!   CONTROL VARIABLES                                                          |
!==============================================================================|

MODULE CONTROL
   USE MOD_PREC
   IMPLICIT NONE
   SAVE

   LOGICAL SERIAL                  !!TRUE IF SINGLE PROCESSOR
   LOGICAL MSR                     !!TRUE IF MASTER PROCESSOR (MYID==1)
   LOGICAL PAR                     !!TRUE IF MULTIPROCESSOR RUN

   CHARACTER(LEN=80) FVCOM_VERSION !!STRING DESCRIBING VERSION
   CHARACTER(LEN=80) FVCOM_WEBSITE !!STRING DESCRIBING WEBSITE FOR FVCOM INFO 
   CHARACTER(LEN=80) CASENAME      !!LETTER ACRONYM SPECIFYING CASE IDENTITY (MAX 80 CHARS)
   CHARACTER(LEN=120) CASETITLE    !!CASE TITLE                                 
   CHARACTER(LEN=80) OUTDIR        !!PARENT OUTPUT DIRECTORY
   CHARACTER(LEN=80) INPDIR        !!MAIN   INPUT DIRECTORY
   CHARACTER(LEN=80) INFOFILE      !!INFO  FILE            
   CHARACTER(LEN=80) INFLOW_TYPE   !!SPECIFIED RIVER INFLOW TYPE (edge/node) 
   CHARACTER(LEN=80) POINT_ST_TYPE !!(calculated/specified)
   CHARACTER(LEN=80) HORZMIX       !!CONTROLS HORIZONTAL DIFFUSION COEF CALC (constant/closure)
   CHARACTER(LEN=80) VERTMIX       !!CONTROLS VERTICAL DIFFUSION COEF CALC [UNUSED]
   CHARACTER(LEN=80) RESTART       !!CONTROLS RESTART TYPE (cold_start/hot_cold_s/hot_start)
   CHARACTER(LEN=80) NCFILE        !!VAR OF NETCDF FILE            

!--Constants-------------------------------------------------------------------!
   REAL(SP), PARAMETER :: GRAV      = 9.81_SP
   REAL(SP), PARAMETER :: PI        = 3.141592653_SP
   REAL(SP), PARAMETER :: PI2       = 6.283185307_SP
   REAL(SP), PARAMETER :: ZERO      = 0.0_SP 
   REAL(SP), PARAMETER :: ONE_THIRD = 1.0_SP/3.0_SP 
   REAL(SP), PARAMETER :: REARTH    = 6371.0E03_SP   !!Earth Radius in Meters
   REAL(SP), PARAMETER :: DEG2RAD   = PI2/360.0_SP   !!Radians/Degree
   REAL(SP), PARAMETER :: TPI       = DEG2RAD*REARTH !TPI=pi*rearth/180.=3.14159265/180.0*6371.*1000.

!--File Unit Specifiers (see iofiles.f for details)----------------------------!
   INTEGER  INRUN,INOBC,IOPRT,IPT,IOTSR,INBFW,INRIV

!--Temp/Salinity Flux Enhancement Parameters-----------------------------------!
   LOGICAL  :: BIO_FCT      !!TRUE IF BIO_VARIABLES ADJUSTMENT IS ON

!--Diffusion Coefficients/Viscosities------------------------------------------!
   REAL(SP) :: UMOL         !!VERTICAL DIFFUSION COEFFICIENT
   REAL(SP) :: HORCON       !!HORIZONTAL DIFFUSION COEFFICIENT
   REAL(SP) :: VPRNU        !!VERTICAL KINEMATIC VISCOSITY
   REAL(SP) :: HPRNU        !!HORIZONTAL KINEMATIC VISCOSITY

!--Parameters Controlling Time/Time Stepping-----------------------------------!
   REAL(SP) :: DTI        !!INTERNAL TIME STEP (Seconds)
   REAL(SP) :: TIME       !!SIMULATION TIME IN DAYS
   REAL(SP) :: THOUR      !!SIMULATION TIME AT CURRENT INTERNAL STEP (IINT) IN HOURS
!JQI   REAL(SP) :: THOUR1     !!SIMULATION TIME AT END OF CURRENT EXTERNAL STEP (IEXT) IN HOURS
   REAL(SP) :: DAYS       !!TOTAL NUMBER OF DAYS IN SIMULATION
   INTEGER  :: IINT       !!INTERNAL TIME STEP ITERATION NUMBER (ISTART => IEND)
   INTEGER  :: ISTART     !!STARTING INTERNAL TIME STEP ITERATION NUMBER
   INTEGER  :: IEND       !!ENDING INTERNAL TIME STEP ITERATION NUMBER
   INTEGER  :: NSTEPS     !!ITERATION NUMBER AT END OF SIMULATION = IEND

!--Parameters Controlling Output
   INTEGER IREPORT        !!INTERVAL (IINT) FOR REPORTING OF FLOWFIELD STATISTICS
   INTEGER IRESTART       !!INTERVAL (IINT) FOR RESTART FILE DUMPS

!----
   INTEGER :: NOHYDFILNA       
   CHARACTER(LEN=80), ALLOCATABLE :: HYDFILNA(:)  !!NAMES OF NETCDF FILE     
   REAL(SP) :: INTERVAL_HYD                       !!INTERVAL TIME OF HYDRA FIELDS IN HOURS
   INTEGER  :: I_HYD,IFILE,ITIME


END MODULE CONTROL

!==============================================================================|

MODULE ALL_VARS
   USE MOD_PREC
   USE LIMS
   USE CONTROL 
   IMPLICIT NONE
   SAVE

!--------------------------Temporary Array------------------------------------------!

  INTEGER, ALLOCATABLE :: NVG(:,:)

!--------------------------Global Grid Variables------------------------------------!

  REAL(SP), ALLOCATABLE :: XG(:)               !!GLOBAL X-COORD AT NODE 
  REAL(SP), ALLOCATABLE :: YG(:)               !!GLOBAL X-COORD AT NODE 
  REAL(SP), ALLOCATABLE :: HG(:)               !!GLOBAL DEPTH AT NODE 
  REAL(SP), ALLOCATABLE :: XCG(:)              !!GLOBAL X-COORD AT FACE CENTER 
  REAL(SP), ALLOCATABLE :: YCG(:)              !!GLOBAL X-COORD AT FACE CENTER 

!--------------------------Grid Metrics---------------------------------------------!

   REAL(SP)              :: VXMIN,VYMIN,VXMAX,VYMAX
   REAL(SP), ALLOCATABLE :: XC(:)               !!X-COORD AT FACE CENTER 
   REAL(SP), ALLOCATABLE :: YC(:)               !!Y-COORD AT FACE CENTER
   REAL(SP), ALLOCATABLE :: VX(:)               !!X-COORD AT GRID POINT
   REAL(SP), ALLOCATABLE :: VY(:)               !!Y-COORD AT GRID POINT
   REAL(SP), ALLOCATABLE :: ART(:)              !!AREA OF ELEMENT
   REAL(SP), ALLOCATABLE :: ART1(:)             !!AREA OF NODE-BASE CONTROl VOLUME
   REAL(SP), ALLOCATABLE :: ART2(:)             !!AREA OF ELEMENTS AROUND NODE
   
!----------------Node, Boundary Condition, and Control Volume-----------------------!

   INTEGER, ALLOCATABLE :: NV(:,:)             !!NODE NUMBERING FOR ELEMENTS
   INTEGER, ALLOCATABLE :: NBE(:,:)            !!INDICES OF ELMNT NEIGHBORS
   INTEGER, ALLOCATABLE :: NTVE(:)      
   INTEGER, ALLOCATABLE :: NTSN(:)      
   INTEGER, ALLOCATABLE :: ISONB(:)            !!NODE MARKER = 0,1,2
   INTEGER, ALLOCATABLE :: ISBC(:)     
   INTEGER, ALLOCATABLE :: ISBCE(:)     
   INTEGER, ALLOCATABLE :: IEC(:,:)
   INTEGER, ALLOCATABLE :: IENODE(:,:)
   INTEGER, ALLOCATABLE :: NBSN(:,:)
   INTEGER, ALLOCATABLE :: NIEC(:,:)
   INTEGER, ALLOCATABLE :: NTRG(:)
   INTEGER, ALLOCATABLE :: NBVE(:,:)
   INTEGER, ALLOCATABLE :: NBVT(:,:)
   INTEGER, ALLOCATABLE :: LISBCE_1(:)          !!LIST OF ELEMENTS WITH ISBCE=1
   INTEGER, ALLOCATABLE :: LISBCE_2(:)          !!LIST OF ELEMENTS WITH ISBCE=2
   INTEGER, ALLOCATABLE :: LISBCE_3(:)          !!LIST OF ELEMENTS WITH ISBCE=3
   REAL(SP),ALLOCATABLE :: DLTXC(:)
   REAL(SP),ALLOCATABLE :: DLTYC(:)
   REAL(SP),ALLOCATABLE :: DLTXYC(:)
   REAL(SP),ALLOCATABLE :: DLTXE(:)
   REAL(SP),ALLOCATABLE :: DLTYE(:)
   REAL(SP),ALLOCATABLE :: DLTXYE(:)
   REAL(SP),ALLOCATABLE :: SITAC(:) 
   REAL(SP),ALLOCATABLE :: SITAE(:) 
   REAL(SP),ALLOCATABLE :: XIJC(:) 
   REAL(SP),ALLOCATABLE :: YIJC(:)
   REAL(SP),ALLOCATABLE :: XIJE(:,:) 
   REAL(SP),ALLOCATABLE :: YIJE(:,:) 
   REAL(SP),ALLOCATABLE :: EPOR(:)            !!ELEMENT FLUX POROSITY (=0. IF ISBCE = 2)
   INTEGER, ALLOCATABLE :: IBCGEO(:)        !!LOCAL GEOSTROPHIC FRICTION CORRECTION NODES
   INTEGER, ALLOCATABLE :: N_ICELLQ(:,:)    !!FLUX ANGLE 

!----------------2-d arrays for the general vertical coordinate -------------------------------!

   REAL(SP), ALLOCATABLE :: Z(:,:)                    !!SIGMA COORDINATE VALUE 
   REAL(SP), ALLOCATABLE :: ZZ(:,:)                   !!INTRA LEVEL SIGMA VALUE
   REAL(SP), ALLOCATABLE :: DZ(:,:)                   !!DELTA-SIGMA VALUE
   REAL(SP), ALLOCATABLE :: DZZ(:,:)                  !!DELTA OF INTRA LEVEL SIGMA 
   REAL(SP), ALLOCATABLE :: Z1(:,:)                   !!SIGMA COORDINATE VALUE 
   REAL(SP), ALLOCATABLE :: ZZ1(:,:)                  !!INTRA LEVEL SIGMA VALUE
   REAL(SP), ALLOCATABLE :: DZ1(:,:)                  !!DELTA-SIGMA VALUE
   REAL(SP), ALLOCATABLE :: DZZ1(:,:)                 !!DELTA OF INTRA LEVEL SIGMA 
   REAL(SP), ALLOCATABLE :: DPTHSL(:)               !!Z-DEPTHS FOR SALINITY/TEMP ICs
   REAL(SP), ALLOCATABLE :: ZG(:,:)                    !!SIGMA COORDINATE VALUE 


!---------------2-d flow variable arrays at elements-------------------------------!

   REAL(SP), ALLOCATABLE :: UA(:)            !!VERTICALLY AVERAGED X-VELOC
   REAL(SP), ALLOCATABLE :: VA(:)            !!VERTICALLY AVERAGED Y-VELOC
   REAL(SP), ALLOCATABLE :: H1(:)            !!BATHYMETRIC DEPTH   
   REAL(SP), ALLOCATABLE :: D1(:)            !!CURRENT DEPTH
   REAL(SP), ALLOCATABLE :: EL1(:)           !!CURRENT SURFACE ELEVATION
   REAL(SP), ALLOCATABLE :: ELF1(:)          !!SURFACE ELEVATION STORAGE FOR RK INT

!---------------2-d flow variable arrays at nodes----------------------------------!

   REAL(SP), ALLOCATABLE :: H(:)             !!BATHYMETRIC DEPTH   
   REAL(SP), ALLOCATABLE :: D(:)             !!CURRENT DEPTH   
   REAL(SP), ALLOCATABLE :: DF(:)            !!DEPTH AT PREVIOUS TIME STEP
   REAL(SP), ALLOCATABLE :: EL(:)            !!CURRENT SURFACE ELEVATION
   REAL(SP), ALLOCATABLE :: ELF(:)           !!SURFACE ELEVATION STORAGE FOR RK INT

!---------------surface/bottom boundary conditions---------------------------------!

   REAL(SP), ALLOCATABLE :: SWRAD(:)         !!SURFACE INCIDENT RADIATION

!---------------- internal mode   arrays-(element based)----------------------------!

   REAL(SP), ALLOCATABLE :: U(:,:)         !X-VELOCITY
   REAL(SP), ALLOCATABLE :: V(:,:)         !Y-VELOCITY

   REAL(SP), ALLOCATABLE :: KH(:,:)        !TURBULENT DIFFUSIVITY FOR SALINITY/TEMP 

  !-----------------------3d variable arrays-(node based)-----------------------------!

  REAL(SP), ALLOCATABLE :: T1(:,:)         !!TEMPERATURE AT NODES               
  REAL(SP), ALLOCATABLE :: S1(:,:)         !!SALINITY AT NODES               
  REAL(SP), ALLOCATABLE :: WTS(:,:)        !!VERTICAL VELOCITY IN SIGMA SYSTEM

  REAL(SP), ALLOCATABLE :: VISCOFH(:,:)


# if defined (SPHERICAL)
  REAL(DP), ALLOCATABLE :: XCA(:)       !Longitude of center point of arc
  REAL(DP), ALLOCATABLE :: YCA(:)       !Latitude of center point of arc
  REAL(DP), ALLOCATABLE :: XCB(:)       !Longitude of center point of arc
  REAL(DP), ALLOCATABLE :: YCB(:)       !Latitude of center point of arc

  REAL(DP), ALLOCATABLE :: XCC(:,:)       !Longitude of center point of arc
  REAL(DP), ALLOCATABLE :: YCC(:,:)       !Latitude of center point of arc
  REAL(DP), ALLOCATABLE :: XCD(:,:)       !Longitude of center point of arc
  REAL(DP), ALLOCATABLE :: YCD(:,:)       !Latitude of center point of arc

  REAL(DP), ALLOCATABLE :: XCE(:)       !Longitude of center point of arc
  REAL(DP), ALLOCATABLE :: YCE(:)       !Latitude of center point of arc
  REAL(DP), ALLOCATABLE :: XCF(:)       !Longitude of center point of arc
  REAL(DP), ALLOCATABLE :: YCF(:)       !Latitude of center point of arc

  REAL(DP), ALLOCATABLE :: XCG2(:)       !Longitude of center point of arc
  REAL(DP), ALLOCATABLE :: YCG2(:)       !Latitude of center point of arc
  
  REAL(SP), ALLOCATABLE :: VAL_COS_VY(:) !THE COSINE VALUE OF LATITUDE
# endif  
  
END MODULE ALL_VARS

MODULE BCS
   USE MOD_TYPES
   USE MOD_PREC
   IMPLICIT NONE
   SAVE

!----------------boundary conditions: ground water----------------------------------!

   INTEGER, ALLOCATABLE :: NODE_BFW(:)         !!LOCAL GROUNDWATER NODES
   INTEGER, ALLOCATABLE :: BFW_GL2LOC(:)       !!GLOBAL TO LOCAL MAPPING OF GWATER NODES
   REAL(SP),  ALLOCATABLE :: BFWQDIS(:,:)      !!GROUNDWATER FRESH WATER FLUX DATA
   REAL(SP),  ALLOCATABLE :: BFWQWDIS(:,:,:)   !!GROUNDWATER WATER BIOLOGICAL DATA
   REAL(SP), ALLOCATABLE :: BFWDIS3(:)         !!GROUNDWATER FLUX AT CURRENT TIME FOR INT
   REAL(SP), ALLOCATABLE :: BFWWDIS3(:,:)      !!GROUNDWATER BIO AT CURRENT TIME FOR INT

   TYPE(BC)      :: BFW_TM                  !!TIME MAP FOR GROUNDWATER DATA
!----------------boundary conditions: fresh water discharge-------------------------!

   INTEGER, ALLOCATABLE  :: INODEQ(:)        !!LOCAL FRESH WATER INFLOW NODES
   INTEGER, ALLOCATABLE  :: ICELLQ(:)        !!LOCAL FRESH WATER INFLOW ELEMENTS 
   INTEGER, ALLOCATABLE  :: RIV_GL2LOC(:)    !!GLOBAL TO LOCAL MAP OF FW NODES
   TYPE(BC)              :: QBC_TM           !!TIME MAP FOR RIVER DISCHARGE DATA 
!JQI   REAL(SP), ALLOCATABLE  :: VQDIST(:,:)     !!DISCHARGE VERTICAL DISTRIBUTION
!JQI   REAL(SP), ALLOCATABLE  :: DQDIS(:,:)      !!WATER FLUX DISCHARGE DATA
!JQI   REAL(SP), ALLOCATABLE  :: DTDIS(:,:)      !!WATER TEMPERATURE DISCHARGE DATA
!JQI   REAL(SP), ALLOCATABLE  :: DSDIS(:,:)      !!WATER SALINITY DISCHARGE DATA

!==============================================================================|

END MODULE  BCS
