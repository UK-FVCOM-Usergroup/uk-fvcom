!
!        CONFIGURATION, REAL AND INTEGER FOR RCA
!                                        Version 3.0

!*               Copyright (C) 1980, HydroQual Inc.                       
!*                                                                        
!*  These coded instructions, statements, and computer programs contain   
!*  unpublished proprietary  information of HydroQual, Inc., and are     
!*  protected by Federal copyright law.  No part of this program and/or
!*  modifications of this program may be reproduced in any form by print
!*  photocopy, microfilm, magnetic- or optical-media or by any other
!*  means, without written permission of HydroQual, Inc.

!*	Author: James Fitzpatrick, HydroQual, Inc.

      MODULE MOD_RCA

      IMPLICIT NONE
      SAVE
      INTEGER,PARAMETER :: NXY=4577,EXY=8172,NZ=45,NOSYS=26
      INTEGER,PARAMETER :: MXPARM2D=10,MXPARM3D=5,MXCONS=250
      INTEGER,PARAMETER :: MXFUNC=20,MXFUNCT=400
!      INTEGER,PARAMETER :: MXBC=750,MXWK=750,MXKINSYS=5,MXKINFILES=5
      INTEGER,PARAMETER :: MXBC=1660,MXWK=750,MXKINSYS=5,MXKINFILES=5
      INTEGER,PARAMETER :: MXHYDFILES=36,MXDIFF=750,NHYD=15000
      INTEGER,PARAMETER :: NSL=100,NSLC=100
      INTEGER,PARAMETER :: MXSEGSPLT=(NXY*NZ)/5

      CHARACTER(LEN=8) :: SYNAME(NOSYS)
      CHARACTER(LEN=40) ::                                   &
        HYDFILNA(MXHYDFILES),DIFFILNA(MXHYDFILES)            &
       ,BCFILNA,PSFILNA,NPSFILNA,FLFILNA,ATMFILNA            &
       ,PCFILNA,ICFILNA,KINFILNA(MXKINFILES)
      CHARACTER(LEN=4) ::                                    &
        TWARP,TWARPP,TWARPBC,TWARPPS,TWARPNPS                &
       ,TWARPFL,TWARPATM,TWARPTVF,TWARPMB
      INTEGER ::                                             &
        NXHYDTSECS,NXBCTSECS,NXPSTSECS,NXNPSTSECS            &
       ,NXFLTSECS,NXATMTSECS,NXFUNTSECS(MXFUNC)
      REAL ::                                                    &
        NXHYDT,NXBCT,NXPST,NXNPST,NXFLT,NXATMT,NXFUNT(MXFUNC)    &
       ,NXCALL13T
      INTEGER ::                                                 &
        OUT,CYCLE,SYSBY(NOSYS),RBY(NOSYS),QBY(NOSYS),HYDCYCOPT   &
       ,DDMPSBY(NOSYS),SLCOPT

      INTEGER ::                                                 &
          IN,MXACTS,NOKINSYS,ISYS,IXY,IZ,IDISK,IPRNTGSECS        &
         ,NXPRTG,IGDOPT,IAVGGDCNTR,IPRNTDSECS,NXPRTD,IDDOPT      &
         ,IAVGDDCNTR,NDMPS,IREC,MASSBAL,IPRNTMBSECS,NXPRTMB      &
         ,IMBDOPT,ISMBSECS,IEMBSECS,IAVGMBCNTR,NEGSLN            &
         ,INTGRTYP,ISMOLAR,ISMOLBCOPT,IDIAGDT,INPCHCK            
      INTEGER ::  LIST(5),IDUMP(8,2),IFDMPS(999,2)
      REAL    ::  CMAX(NOSYS),CMIN(NOSYS)

      INTEGER ::                                                 &
          INITB,IDTSECS,ITIMESECS,IDTWQSECS,ITIMEWQSECS,ISCALT
      REAL ::                                                    &
          TZERO,DT,TIME,DTWQ,TIMEWQ,SCALT,TEND

      INTEGER :: STEP
      INTEGER :: NSTEP,ISTEP(400)
      REAL :: TBRK(400)

      INTEGER :: IHYDFILE,IBNRYRDOPTS(5)

      INTEGER :: ISCALBC,ISCALPS,ISCALNPS,ISCALFL,ISCALATM,ISCALTVF

      INTEGER :: IHYDDTSECS,IDIFFOPT,IECOMVER
      REAL    :: HYDDT,TMID0
      REAL, ALLOCATABLE, DIMENSION(:,:) :: BVOL,VDER

      REAL :: MFUNC(MXFUNC),CONST(MXCONS)
      REAL :: BFUNC(MXFUNC),VALMTF(MXFUNC,MXFUNCT),TIMEMTF(MXFUNC,MXFUNCT)
      REAL,ALLOCATABLE :: PARAM2D(:,:),PARAM3D(:,:,:)
      REAL,ALLOCATABLE :: PARAM2D_GL(:,:),PARAM3D_GL(:,:,:)

      INTEGER :: NOCONS,NOFUNC,ITVFPWLOPT,ITIMF(MXFUNC)

      INTEGER ::                                                  &
          IBCOPT,IBCPWLOPT,NOBC(NOSYS),IBC(2,MXBC,NOSYS)          &
         ,NOBCALL,IBCALL(2,MXBC),NOBCSL(MXBC,NOSYS)
      INTEGER ::                                                  &
          NOBC_GL(NOSYS),IBC_GL(2,MXBC,NOSYS),NOBCALL_GL,IBCALL_GL(2,MXBC)  &
         ,NOBCSL_GL(MXBC,NOSYS),IBCS_GL(MXBC)
      REAL ::                                                     &
          SCALBC(NOSYS),BBC(MXBC,NOSYS),SBC(MXBC,NOSYS)           &
         ,SLDEPTH(NSL,NOSYS)
      REAL ::                                                     &
          BBC_GL(MXBC,NOSYS),SBC_GL(MXBC,NOSYS)
 
      INTEGER ::                                                  &   !PS
          IPSOPT,IPSPWLOPT,NOPS(NOSYS),IPSTABL(2,MXWK)            &
         ,IPS(3,MXWK,NOSYS),INPSOPT,INPSPWLOPT,NONPS(NOSYS)       &
         ,INPSTABL(2,MXWK),INPS(3,MXWK,NOSYS),IFLOPT              &
         ,IFLPWLOPT,NOFL(NOSYS),IFLTABL(2,MXWK)                   &
         ,IFL(3,MXWK,NOSYS),IATMOPT,IATMPWLOPT,NOATM(NOSYS)       
      INTEGER ::                                                  &
          NOPS_GL(NOSYS)            &  !PS
         ,IPS_GL(3,MXWK,NOSYS),NONPS_GL(NOSYS)       &
         ,INPS_GL(3,MXWK,NOSYS)              &
         ,NOFL_GL(NOSYS)                  &
         ,IFL_GL(3,MXWK,NOSYS),NOATM_GL(NOSYS)       
      REAL ::                                                     &
          ZFRACPS(NZ,MXWK),SCALPS(NOSYS),BPS(MXWK,NOSYS)          &
         ,SPS(MXWK,NOSYS),ZFRACNPS(NZ,MXWK),SCALNPS(NOSYS)        &
         ,BNPS(MXWK,NOSYS),SNPS(MXWK,NOSYS),ZFRACFL(NZ,MXWK)      &
         ,SCALFL(NOSYS),BFL(MXWK,NOSYS),SFL(MXWK,NOSYS)           &
         ,SCALATM(NOSYS)
      REAL ::                                                     &
          BPS_GL(MXWK,NOSYS),SPS_GL(MXWK,NOSYS),ZFRACPS_GL(NZ,MXWK)   &
         ,ZFRACNPS_GL(NZ,MXWK),ZFRACFL_GL(NZ,MXWK)        &
         ,BNPS_GL(MXWK,NOSYS),SNPS_GL(MXWK,NOSYS)     &
         ,BFL_GL(MXWK,NOSYS),SFL_GL(MXWK,NOSYS)           
      REAL,ALLOCATABLE,DIMENSION(:,:) :: BATM,SATM
      REAL,ALLOCATABLE,DIMENSION(:,:) :: BATM_GL,SATM_GL

!JQI      INTEGER,DIMENSION(NOSYS) :: NOPS_GL,NONPS_GL,NOFL_GL,NOATM_GL

      INTEGER ::                                                  &
!JQI     .    IXS(NY),IXE(NY)
          NODIFF,IDD(MXDIFF),JDD(MXDIFF)
      REAL ::                                                     &
        SCALRX,SCALRY,SCALRZ,VDDIST(MXDIFF,NZ),QDIFF(MXDIFF)
      REAL,ALLOCATABLE,DIMENSION(:,:) ::                          &
          QX,QY,QZ,RX,RY,RZ,DIAG,EX,EY,EZ
      REAL,ALLOCATABLE :: AVECT(:,:,:)

      INTEGER :: IDTSLCSECS,NOSLC
      REAL :: DTSLC,SLCDPTH(NSLC)

      INTEGER ::                                                  &
          IDTSPLITSECS,IDTFULLSECS                                &
         ,NSEGSPLT,ISEGSPLT(MXSEGSPLT,3)
      REAL ::                                                     &
          DTSPLIT,DTFULL                                          &
         ,AVECTSPLT(MXSEGSPLT,12),DIAGSPLT(MXSEGSPLT)             &
         ,VDERSPLT(MXSEGSPLT)
      REAL,ALLOCATABLE :: FSMSPLT(:,:)

      INTEGER ::                                                 &
!JQI     .    ICOLLOPT,LNDWTROPT
          ICOLLOPT,IWTRCNT
      INTEGER,ALLOCATABLE :: IWTRNDX(:),JWTRNDX(:)

      REAL,ALLOCATABLE,DIMENSION(:) ::                           &
          FSM,DETA,ETA,HBAR,DX,DY,XAZ,XAZ_TMP,DETA1,ETA1
      REAL,ALLOCATABLE,DIMENSION(:,:) :: XAX,XAY
 
      REAL,ALLOCATABLE,DIMENSION(:,:) :: HYDSAL,HYDTEMP

      REAL,ALLOCATABLE :: CKINARRAY(:,:,:)

      REAL,ALLOCATABLE,DIMENSION(:,:,:) ::                       &
          SCRATCH_TRAN,SCRATCH_KIN,SCRATCH_ARRAY

!      EQUIVALENCE
!     .     (SCRATCH_ARRAY(1,1,1),SCRATCH_TRAN(1,1,1))
!     .   , (SCRATCH_ARRAY(1,1,16),SCRATCH_KIN(1,1,1))
      
      END MODULE MOD_RCA
