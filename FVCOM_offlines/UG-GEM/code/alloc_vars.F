!==============================================================================|
!    Allocate and Initialize Most Arrays                                       !
!==============================================================================|

   SUBROUTINE ALLOC_VARS

!==============================================================================!
   USE ALL_VARS
   IMPLICIT NONE

   INTEGER NCT,NDB,IERR
   REAL(SP) :: MEMTOT,MEMCNT
!==============================================================================!

   NDB = 1       !!GWC BASE THIS ON KIND
   NCT = NT*3
   MEMCNT = 0.

   IF(MSR)WRITE(IPT,*  )'!'
   IF(MSR)WRITE(IPT,*)'!                ALLOCATING MEMORY    '
   IF(MSR)WRITE(IPT,*  )'!'

!==============================================================================!
!  ALLOCATE:                                                                   !
!==============================================================================!

!--------------------------Grid Metrics---------------------------------------------!

   ALLOCATE(XC(0:NT))            ;XC   = ZERO   !!X-COORD AT FACE CENTER 
   ALLOCATE(YC(0:NT))            ;YC   = ZERO   !!Y-COORD AT FACE CENTER
   ALLOCATE(VX(0:MT))            ;VX   = ZERO   !!X-COORD AT GRID POINT
   ALLOCATE(VY(0:MT))            ;VY   = ZERO   !!Y-COORD AT GRID POINT
   ALLOCATE(ART(0:NT))           ;ART  = ZERO   !!AREA OF ELEMENT
   ALLOCATE(ART1(0:MT))          ;ART1 = ZERO   !!AREA OF NODE-BASE CONTROl VOLUME
   ALLOCATE(ART2(MT))            ;ART2 = ZERO   !!AREA OF ELEMENTS AROUND NODE

                  MEMCNT = MT*7*NDB + MEMCNT

!----------------Node, Boundary Condition, and Control Volume-----------------------!

   ALLOCATE(NV(0:NT,4))          ;NV       = 0  !!NODE NUMBERING FOR ELEMENTS
   ALLOCATE(NBE(0:NT,3))         ;NBE      = 0  !!INDICES OF ELMNT NEIGHBORS
   ALLOCATE(NTVE(0:MT))          ;NTVE     = 0 
   ALLOCATE(NTSN(MT))            ;NTSN     = 0 
   ALLOCATE(ISONB(0:MT))         ;ISONB    = 0  !!NODE MARKER = 0,1,2
   ALLOCATE(ISBCE(0:NT))         ;ISBCE    = 0 
   ALLOCATE(NIEC(NCT,2))         ;NIEC     = 0
   ALLOCATE(NTRG(NCT))           ;NTRG     = 0
   ALLOCATE(DLTXE(NCT))          ;DLTXE    = ZERO
   ALLOCATE(DLTYE(NCT))          ;DLTYE    = ZERO
   ALLOCATE(DLTXYE(NCT))         ;DLTXYE   = ZERO
   ALLOCATE(SITAE(NCT))          ;SITAE    = ZERO
   ALLOCATE(XIJE(NCT,2))         ;XIJE     = ZERO
   ALLOCATE(YIJE(NCT,2))         ;YIJE     = ZERO 
   ALLOCATE(N_ICELLQ(NUMQBC,2))  ;N_ICELLQ = 0

                 MEMCNT = NT*8 + MT*3  + NCT*3 + NCT*7*NDB  + MEMCNT

!----------------2-d arrays for the general vertical coordinate -------------------------------!

   ALLOCATE(Z(0:MT,KB))               ; Z    = ZERO    !!SIGMA COORDINATE VALUE 
   ALLOCATE(ZZ(0:MT,KB))              ; ZZ   = ZERO    !!INTRA LEVEL SIGMA VALUE
   ALLOCATE(DZ(0:MT,KB))              ; DZ   = ZERO    !!DELTA-SIGMA VALUE
   ALLOCATE(DZZ(0:MT,KB))             ; DZZ  = ZERO    !!DELTA OF INTRA LEVEL SIGMA 
   ALLOCATE(Z1(0:NT,KB))              ; Z1   = ZERO    !!SIGMA COORDINATE VALUE 
   ALLOCATE(ZZ1(0:NT,KB))             ; ZZ1  = ZERO    !!INTRA LEVEL SIGMA VALUE
   ALLOCATE(DZ1(0:NT,KB))             ; DZ1  = ZERO    !!DELTA-SIGMA VALUE
   ALLOCATE(DZZ1(0:NT,KB))            ; DZZ1 = ZERO    !!DELTA OF INTRA LEVEL SIGMA 
                     MEMCNT = MT*KB*4*NDB + NT*KB*4*NDB +MEMCNT

!---------------2-d flow variable arrays at elements-------------------------------!

   ALLOCATE(UA(0:NT))            ;UA        = ZERO  !!VERTICALLY AVERAGED X-VELOC
   ALLOCATE(VA(0:NT))            ;VA        = ZERO  !!VERTICALLY AVERAGED Y-VELOC
   ALLOCATE(H1(0:NT))            ;H1        = ZERO  !!BATHYMETRIC DEPTH   
   ALLOCATE(D1(0:NT))            ;D1        = ZERO  !!DEPTH
   ALLOCATE(EL1(0:NT))           ;EL1       = ZERO  !!SURFACE ELEVATION
   ALLOCATE(ELF1(0:NT))          ;ELF1      = ZERO  !!SURFACE ELEVATION
                 MEMCNT = NT*17*NDB + MEMCNT

!---------------2-d flow variable arrays at nodes----------------------------------!

   ALLOCATE(H(0:MT))             ;H    = ZERO       !!BATHYMETRIC DEPTH   
   ALLOCATE(D(0:MT))             ;D    = ZERO       !!DEPTH   
   ALLOCATE(DF(0:MT))            ;DF   = ZERO       !!DEPTH   
   ALLOCATE(EL(0:MT))            ;EL   = ZERO       !!SURFACE ELEVATION
   ALLOCATE(ELF(0:MT))           ;ELF  = ZERO       !!SURFACE ELEVATION
               MEMCNT = MT*8*NDB + MEMCNT

   ALLOCATE(SWRAD(0:MT))         ;SWRAD   = ZERO     !!SURFACE INCIDENT RADIATION

!---------------- internal mode   arrays-(element based)----------------------------!

   ALLOCATE(U(0:NT,KB))          ;U       = ZERO   !!X-VELOCITY
   ALLOCATE(V(0:NT,KB))          ;V       = ZERO   !!Y-VELOCITY

!-----------------------3d variable arrays-(node based)-----------------------------!

   ALLOCATE(T1(0:MT,KB))         ;T1     = ZERO  !!TEMPERATURE AT NODES               
   ALLOCATE(S1(0:MT,KB))         ;S1     = ZERO  !!SALINITY AT NODES               
   ALLOCATE(WTS(0:MT,KB))        ;WTS    = ZERO  !!VERTICAL VELOCITY IN SIGMA SYSTEM
   ALLOCATE(KH(0:MT,KB))         ;KH    = ZERO   !!TURBULENT QUANTITY

   ALLOCATE(VISCOFH(0:MT,KB))    ;VISCOFH = ZERO
               MEMCNT = MT*KB*NDB + MEMCNT

# if defined (SPHERICAL)
!---------------Coordinates of Center Pionts around the Nodes-----------------------!
   ALLOCATE(XCA(M))             ;XCA        = ZERO
   ALLOCATE(YCA(M))             ;YCA        = ZERO
   ALLOCATE(XCB(M))             ;XCB        = ZERO
   ALLOCATE(YCB(M))             ;YCB        = ZERO
   ALLOCATE(XCC(M,10))          ;XCC        = ZERO !ASSUMING THE MAXIUM OF NEIGHBORING ELEMENTS IS NOT MORE THAN 10
   ALLOCATE(YCC(M,10))          ;YCC        = ZERO !ASSUMING THE MAXIUM OF NEIGHBORING ELEMENTS IS NOT MORE THAN 10
   ALLOCATE(XCD(M,10))          ;XCD        = ZERO !ASSUMING THE MAXIUM OF NEIGHBORING ELEMENTS IS NOT MORE THAN 10
   ALLOCATE(YCD(M,10))          ;YCD        = ZERO !ASSUMING THE MAXIUM OF NEIGHBORING ELEMENTS IS NOT MORE THAN 10
   ALLOCATE(XCE(M))             ;XCE        = ZERO
   ALLOCATE(YCE(M))             ;YCE        = ZERO
   ALLOCATE(XCF(M))             ;XCF        = ZERO
   ALLOCATE(YCF(M))             ;YCF        = ZERO
   ALLOCATE(VAL_COS_VY(M))      ;VAL_COS_VY = ZERO
# endif
!---------------report approximate memory usage-------------------------------------!

   MEMTOT = MEMCNT*4
#  if defined (MULTIPROCESSOR)
   IF(PAR)CALL MPI_REDUCE(MEMCNT,MEMTOT,1,MPI_F,MPI_SUM,0,MPI_COMM_WORLD,IERR)
#  endif
   IF(MSR)WRITE(IPT,*)'!  # MBYTES TOTAL MEM    :',MEMTOT/1E+6
   IF(MSR .AND. .NOT.SERIAL )WRITE(IPT,*)'!  # AVERAGE MBYTES/PROC :',MEMTOT/(1E+6*NPROCS)
   

   RETURN
   END SUBROUTINE ALLOC_VARS
!==============================================================================|
