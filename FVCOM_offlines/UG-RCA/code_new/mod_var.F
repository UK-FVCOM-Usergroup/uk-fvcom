!==============================================================================|
!   GLOBAL LIMITS AND ARRAY SIZING PARAMETERS                                  !
!==============================================================================|

      MODULE LIMS
      IMPLICIT NONE
      SAVE
   
      INTEGER M
      INTEGER N
      INTEGER MT
      INTEGER NT
!      INTEGER EXY               !!NUMBER OF ELEMENTS 
      INTEGER NISBCE_1           !!LOCAL NUMBER OF ELEMENTS WITH ISBCE = 1
      INTEGER NISBCE_2           !!LOCAL NUMBER OF ELEMENTS WITH ISBCE = 2
      INTEGER NISBCE_3           !!LOCAL NUMBER OF ELEMENTS WITH ISBCE = 3

      INTEGER KB                 !!NUMBER OF SIGMA LEVELS
      INTEGER KBM1               !!NUMBER OF SIGMA LEVELS-1
      INTEGER KBM2               !!NUMBER OF SIGMA LEVELS-2
      INTEGER MYID               !!UNIQUE PROCESSOR ID (1 => NPROCS)
      INTEGER NPROCS             !!NUMBER OF PROCESSORS
      INTEGER NE                 !!NUMBER OF UNIQUE EDGES
      INTEGER NCV                !!NUMBER OF INTERNAL CONTROL VOLUMES (EXTENDED LOCAL ONLY)
   
      INTEGER NCV_I              !!NUMBER OF INTERNAL CONTROL VOLUMES (LOCAL ONLY)
      INTEGER ETXY                 !!TOTAL OF LOCAL INTERNAL + HALO ELEMENTS
      INTEGER NTXY                 !!TOTAL OF LOCAL INTERNAL + HALO NODES
      INTEGER NCT
      INTEGER MX_NBR_ELEM        !!MAX NUMBER OF ELEMENTS SURROUNDING A NODE

      INTEGER NUMQBC_GL
      INTEGER NUMQBC
      END MODULE LIMS

!==============================================================================|
!   CONTROL VARIABLES                                                          |
!==============================================================================|

      MODULE CONTROL

      IMPLICIT NONE
      SAVE

      LOGICAL SERIAL                  !!TRUE IF SINGLE PROCESSOR
      LOGICAL MSR                     !!TRUE IF MASTER PROCESSOR (MYID==1)
      LOGICAL PAR                     !!TRUE IF MULTIPROCESSOR RUN

      CHARACTER(LEN=80) CASENAME      !!LETTER ACRONYM SPECIFYING CASE IDENTITY (MAX 80 CHARS)
      CHARACTER(LEN=120) CASETITLE    !!CASE TITLE                                 

      REAL :: HMAX         !!GLOBAL MAXIMUM DEPTH
      REAL :: HMIN         !!GLOBAL MINIMUM DEPTH
      REAL :: UMOL         !!VERTICAL DIFFUSION COEFFICIENT
      REAL :: HORCON       !!HORIZONTAL DIFFUSION COEFFICIENT

      CHARACTER(LEN=80) HORZMIX       !!CONTROLS HORIZONTAL DIFFUSION COEF CALC (constant/closure)
      CHARACTER(LEN=100) :: FILENUMBER,PREFF,INPDIR,GEOAREA,NCFILE
      CHARACTER(LEN=80) INFLOW_TYPE   !!SPECIFIED RIVER INFLOW TYPE (edge/node) 
      CHARACTER(LEN=80) POINT_ST_TYPE !!(calculated/specified)
      INTEGER :: DAY
   
      END MODULE CONTROL
!==============================================================================|
!  VARIABLES                                                                   |
!==============================================================================|

      MODULE ALL_VARS

      USE LIMS
      USE CONTROL
!      USE MOD_TYPES
      IMPLICIT NONE
      SAVE

!--Constants-------------------------------------------------------------------!

      REAL, PARAMETER :: GRAV      = 9.81
      REAL, PARAMETER :: PI        = 3.141592653
      REAL, PARAMETER :: PI2       = 6.283185307
      REAL, PARAMETER :: ZERO      = 0.0 
      REAL, PARAMETER :: ONE_THIRD = 1.0/3.0 

!--------------------------Temporary Array------------------------------------------!

      INTEGER, ALLOCATABLE :: NVG(:,:)

!--------------------------Global Grid Variables------------------------------------!

      REAL, ALLOCATABLE :: XG(:)               !!GLOBAL X-COORD AT NODE 
      REAL, ALLOCATABLE :: YG(:)               !!GLOBAL X-COORD AT NODE 
      REAL, ALLOCATABLE :: HG(:)               !!GLOBAL DEPTH AT NODE 
      REAL, ALLOCATABLE :: XCG(:)              !!GLOBAL X-COORD AT FACE CENTER 
      REAL, ALLOCATABLE :: YCG(:)              !!GLOBAL X-COORD AT FACE CENTER 

!--------------------------Grid Metrics---------------------------------------------!

      REAL              :: VXMIN,VYMIN,VXMAX,VYMAX
      REAL, ALLOCATABLE :: XC(:)               !!X-COORD AT FACE CENTER 
      REAL, ALLOCATABLE :: YC(:)               !!Y-COORD AT FACE CENTER
      REAL, ALLOCATABLE :: VX(:)               !!X-COORD AT GRID POINT
      REAL, ALLOCATABLE :: VY(:)               !!Y-COORD AT GRID POINT
      REAL, ALLOCATABLE :: ART(:)              !!AREA OF ELEMENT
      REAL, ALLOCATABLE :: ART1(:)             !!AREA OF NODE-BASE CONTROl VOLUME
      REAL, ALLOCATABLE :: ART2(:)             !!AREA OF ELEMENTS AROUND NODE

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
      REAL,ALLOCATABLE :: DLTXC(:)
      REAL,ALLOCATABLE :: DLTYC(:)
      REAL,ALLOCATABLE :: DLTXYC(:)
      REAL,ALLOCATABLE :: DLTXE(:)
      REAL,ALLOCATABLE :: DLTYE(:)
      REAL,ALLOCATABLE :: DLTXYE(:)
      REAL,ALLOCATABLE :: SITAC(:) 
      REAL,ALLOCATABLE :: SITAE(:) 
      REAL,ALLOCATABLE :: XIJC(:) 
      REAL,ALLOCATABLE :: YIJC(:)
      REAL,ALLOCATABLE :: XIJE(:,:) 
      REAL,ALLOCATABLE :: YIJE(:,:) 
      REAL,ALLOCATABLE :: EPOR(:)            !!ELEMENT FLUX POROSITY (=0. IF ISBCE = 2)
      INTEGER, ALLOCATABLE :: IBCGEO(:)        !!LOCAL GEOSTROPHIC FRICTION CORRECTION NODES
      INTEGER, ALLOCATABLE :: N_ICELLQ(:,:)    !!FLUX ANGLE 

!----------------2-d arrays for the sigma coordinate -------------------------------!

      REAL, ALLOCATABLE :: ZG(:,:)                    !!SIGMA COORDINATE VALUE 
      REAL, ALLOCATABLE :: Z(:,:)                    !!SIGMA COORDINATE VALUE 
      REAL, ALLOCATABLE :: DZ1(:,:)                   !!DELTA-SIGMA VALUE
      REAL, ALLOCATABLE :: ZZ(:,:)                   !!INTRA LEVEL SIGMA VALUE
      REAL, ALLOCATABLE :: DZ(:,:)                   !!DELTA-SIGMA VALUE
      REAL, ALLOCATABLE :: DZZ(:,:)                  !!DELTA OF INTRA LEVEL SIGMA 

!---------------2-d flow variable arrays at elements-------------------------------!

      REAL, ALLOCATABLE :: HC(:)            !!BATHYMETRIC DEPTH   

!---------------2-d flow variable arrays at nodes----------------------------------!

      REAL, ALLOCATABLE :: H(:)            !!BATHYMETRIC DEPTH   
      REAL, ALLOCATABLE :: D(:)            !!CURRENT DEPTH   
      REAL, ALLOCATABLE :: D1(:)            !!CURRENT DEPTH   
!      REAL, ALLOCATABLE :: DT(:)            !!DEPTH AT PREVIOUS TIME STEP
      REAL, ALLOCATABLE :: DT1(:)           !!DEPTH AT PREVIOUS TIME STEP
      REAL, ALLOCATABLE :: EL(:)           !!CURRENT SURFACE ELEVATION
      REAL, ALLOCATABLE :: ET(:)           !!SURFACE ELEVATION AT PREVIOUS TIME STEP
      REAL, ALLOCATABLE :: DTFA(:)          !!ADJUSTED DEPTH FOR MASS CONSERVATION

!---------------- internal mode   arrays-(element based)----------------------------!

      REAL, ALLOCATABLE :: UUA(:)         !X-VELOCITY
      REAL, ALLOCATABLE :: VVA(:)         !Y-VELOCITY
      REAL, ALLOCATABLE :: UU(:,:)         !X-VELOCITY
      REAL, ALLOCATABLE :: VV(:,:)         !Y-VELOCITY
      REAL, ALLOCATABLE :: UUT(:,:)        !X-VELOCITY FROM PREVIOUS TIMESTEP
      REAL, ALLOCATABLE :: VVT(:,:)        !Y-VELOCITY FROM PREVIOUS TIMESTEP
      REAL, ALLOCATABLE :: WWT(:,:)       !Z-VELOCITY FROM PREVIOUS TIMESTEP

!-----------------------3d variable arrays-(node based)-----------------------------!

      REAL, ALLOCATABLE :: WTS(:,:)        !!VERTICAL VELOCITY IN SIGMA SYSTEM       
      REAL, ALLOCATABLE :: WTTS(:,:)        !!VERTICAL VELOCITY IN SIGMA SYSTEM       
      REAL, ALLOCATABLE :: KH(:,:)         !!TURBULENT DIFFUSIVITY

      REAL, ALLOCATABLE :: CARAY(:,:,:)
      REAL, ALLOCATABLE :: CDARAY(:,:,:)
      

!------------shape coefficient arrays and control volume metrics--------------------!

      REAL, ALLOCATABLE :: A1U(:,:)      
      REAL, ALLOCATABLE :: A2U(:,:)     
      REAL, ALLOCATABLE :: AWX(:,:)   
      REAL, ALLOCATABLE :: AWY(:,:)  
      REAL, ALLOCATABLE :: AW0(:,:) 

      REAL, ALLOCATABLE :: VISCOFH(:,:)

      REAL, ALLOCATABLE :: UNC1(:,:)
      REAL, ALLOCATABLE :: VNC1(:,:)
      REAL, ALLOCATABLE :: WNC1(:,:)
      REAL, ALLOCATABLE :: KHNC1(:,:)
      REAL, ALLOCATABLE :: TNC1(:,:)
      REAL, ALLOCATABLE :: SNC1(:,:)
      REAL, ALLOCATABLE :: ELNC1(:)
      REAL, ALLOCATABLE :: UNC2(:,:)
      REAL, ALLOCATABLE :: VNC2(:,:)
      REAL, ALLOCATABLE :: WNC2(:,:)
      REAL, ALLOCATABLE :: KHNC2(:,:)
      REAL, ALLOCATABLE :: TNC2(:,:)
      REAL, ALLOCATABLE :: SNC2(:,:)
      REAL, ALLOCATABLE :: ELNC2(:)
  
      INTEGER :: HOUR1,HOUR
  
!      TYPE(BC) :: TIME_MAP
      REAL :: THOUR1     !!SIMULATION TIME AT END OF CURRENT EXTERNAL STEP (IEXT) IN HOURS
      REAL :: THOUR

      END MODULE ALL_VARS

      MODULE BCS
!      USE MOD_TYPES
      IMPLICIT NONE
      SAVE

!----------------boundary conditions: fresh water discharge-------------------------!

      INTEGER, ALLOCATABLE  :: INODEQ(:)        !!LOCAL FRESH WATER INFLOW NODES
      INTEGER, ALLOCATABLE  :: ICELLQ(:)        !!LOCAL FRESH WATER INFLOW ELEMENTS 
      INTEGER, ALLOCATABLE  :: RIV_GL2LOC(:)    !!GLOBAL TO LOCAL MAP OF FW NODES
!      TYPE(BC)              :: QBC_TM           !!TIME MAP FOR RIVER DISCHARGE DATA 
      REAL, ALLOCATABLE  :: VQDIST(:,:)     !!DISCHARGE VERTICAL DISTRIBUTION
      REAL, ALLOCATABLE  :: DQDIS(:,:)      !!WATER FLUX DISCHARGE DATA
      REAL, ALLOCATABLE :: DWDIS(:,:,:)  !!WATER QUALITY DISCHARGE DATA
      REAL, ALLOCATABLE :: WDIS(:,:)     !!FRESH WATER QUALITY AT CURRENT TIME

!==============================================================================|

      END MODULE  BCS
