!==============================================================================|
!    Allocate and Initialize RCA Arrays                                       !
!==============================================================================|

      SUBROUTINE ALLOCATE_RCA
      USE ALL_VARS   !,ONLY : MT,NT,CARAY,CDARAY,UUA,VVA,WTS
      USE MOD_RCA
      IMPLICIT NONE
      
      ALLOCATE(BVOL(0:MT,NZ))    ; BVOL = 0.0
      ALLOCATE(VDER(0:MT,NZ))    ; VDER = 0.0

      ALLOCATE(PARAM2D(0:MT,MXPARM2D)) ; PARAM2D = 0.0
      ALLOCATE(PARAM3D(0:MT,NZ,MXPARM3D)) ; PARAM3D = 0.0
      ALLOCATE(PARAM2D_GL(0:NXY,MXPARM2D)) ; PARAM2D_GL = 0.0
      ALLOCATE(PARAM3D_GL(0:NXY,NZ,MXPARM3D)) ; PARAM3D_GL = 0.0
 
!      ALLOCATE(BATM(MT,NOSYS))  ;BATM = 0.0
!      ALLOCATE(SATM(MT,NOSYS)) ; SATM = 0.0
      
      ALLOCATE(QX(0:NT,NZ))         ;QX = 0.0
      ALLOCATE(QY(0:NT,NZ))         ;QY = 0.0
      ALLOCATE(QZ(0:MT,NZ+1))       ;QZ = 0.0
      ALLOCATE(RX(0:MT,NZ))           ;RX = 0.0
      ALLOCATE(RY(0:MT,NZ))           ;RY = 0.0
      ALLOCATE(RZ(0:MT,NZ+1))         ;RZ = 0.0
      ALLOCATE(AVECT(0:MT,0:NZ+1,6))  ;AVECT = 0.0
      ALLOCATE(DIAG(0:MT,NZ))         ;DIAG = 0.0
      ALLOCATE(EX(0:MT,NZ))         ;EX = 0.0
      ALLOCATE(EY(0:MT,NZ))         ;EY = 0.0
      ALLOCATE(EZ(0:MT,NZ+1))       ;EZ = 0.0

      ALLOCATE(UU(0:NT,NZ))         ;UU = 0.0
      ALLOCATE(VV(0:NT,NZ))         ;VV = 0.0
      ALLOCATE(WTS(0:MT,NZ+1))      ;WTS = 0.0
      ALLOCATE(VISCOFH(0:MT,NZ))    ;VISCOFH = 0.0

      ALLOCATE(FSMSPLT(MT,NZ))      ;FSMSPLT = 0.0
      
      ALLOCATE(IWTRNDX(MT))         ;IWTRNDX = 0.0
      ALLOCATE(JWTRNDX(MT))         ;JWTRNDX = 0.0

      ALLOCATE(FSM(0:MT))             ;FSM = 0.0
      ALLOCATE(DETA(0:MT))          ;DETA = 0.0
      ALLOCATE(ETA(0:MT))           ;ETA = 0.0
      ALLOCATE(HBAR(0:MT))            ;HBAR = 0.0
      ALLOCATE(DX(MT))              ;DX = 0.0
      ALLOCATE(DY(MT))              ;DY = 0.0
      ALLOCATE(XAX(0:MT,NZ))          ;XAX = 0.0
      ALLOCATE(XAY(0:MT,NZ))          ;XAY = 0.0
      ALLOCATE(XAZ(0:MT))             ;XAZ = 0.0
      ALLOCATE(DETA1(NT))           ;DETA1 = 0.0
      ALLOCATE(ETA1(NT))            ;ETA1 = 0.0
      
      ALLOCATE(HYDSAL(0:MT,NZ))           ;HYDSAL = 0.0
      ALLOCATE(HYDTEMP(0:MT,NZ))          ;HYDTEMP = 0.0 
      ALLOCATE(CKINARRAY(0:MT,NZ,MXKINSYS)) ;CKINARRAY = 0.0

      ALLOCATE(SCRATCH_TRAN(0:MT,NZ,15))    ;SCRATCH_TRAN = 0.0
      ALLOCATE(SCRATCH_KIN(0:MT,NZ,25))     ;SCRATCH_KIN = 0.0
      ALLOCATE(SCRATCH_ARRAY(0:MT,NZ,40))   ;SCRATCH_ARRAY = 0.0

      ALLOCATE(CARAY(0:MT,NZ,NOSYS))        ;CARAY = 0.0
      ALLOCATE(CDARAY(0:MT,NZ,NOSYS))       ;CDARAY = 0.0

      ALLOCATE(UUA(0:NT))            ;UUA = 0.0
      ALLOCATE(VVA(0:NT))            ;VVA = 0.0
      
      RETURN
      END SUBROUTINE ALLOCATE_RCA
