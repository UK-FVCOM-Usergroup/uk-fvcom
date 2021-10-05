MODULE MOD_PAR
# if defined (MULTIPROCESSOR)
   USE MOD_TYPES
   IMPLICIT NONE
   SAVE
!
!--Global Information
!
   INTEGER, ALLOCATABLE :: EL_PID(:)         !!PROCESSOR OWNER OF GLOBAL ELEMENT
   INTEGER, ALLOCATABLE :: ELID(:)           !!LOCAL VALUE OF GLOBAL ELEMENT
   INTEGER, ALLOCATABLE :: NLID(:)           !!LOCAL VALUE OF GLOBAL NODE 
   INTEGER, ALLOCATABLE :: ELID_X(:)         !!LOCAL VALUE OF GLOBAL ELEMENT INCLUDING HALOS
   INTEGER, ALLOCATABLE :: NLID_X(:)         !!LOCAL VALUE OF GLOBAL NODE INCLUDING HALOS 
!
!--Global to Local Boundary Condition Transforms
!
   INTEGER, ALLOCATABLE :: OBN_GL2LOC(:)    !!GIVES GLOBAL IDENTITY OF LOCAL BC ARRAY
!
!--Internal Information (Local)
!
   INTEGER, ALLOCATABLE :: EGID(:)         !!GLOBAL ID OF LOCAL ELEMENT
   INTEGER, ALLOCATABLE :: NGID(:)         !!GLOBAL ID OF LOCAL NODE 
!
!--Boundary Information: Halo Elements          
!
   INTEGER              :: NHE             !!NUMBER OF HALO ELEMENTS
   INTEGER, ALLOCATABLE :: HE_LST(:)       !!GLOBAL IDENTITIES OF HALO ELEMENTS
   INTEGER, ALLOCATABLE :: HE_OWN(:)       !!OWNER OF HALO ELEMENTS

!
!--Boundary Information: Internal Boundary Nodes
!
   INTEGER :: NBN                                 !!NUMBER OF BOUNDARY NODES
   INTEGER :: MX_MLT                              !!MAX MULTIPLICITY OF BOUNDARY NODES
   INTEGER, ALLOCATABLE, DIMENSION(:)   :: BN_LST !!GLOBAL IDENTITY OF BOUNDARY NODES
   INTEGER, ALLOCATABLE, DIMENSION(:)   :: BN_LOC !!LOCAL IDENTITY OF BOUNDARY NODES
   INTEGER, ALLOCATABLE, DIMENSION(:)   :: BN_MLT !!MULTIPLICITY OF BOUNDARY NODES
   INTEGER, ALLOCATABLE, DIMENSION(:,:) :: BN_NEY !!NODE OWNER LIST
   INTEGER, ALLOCATABLE, DIMENSION(:)   :: NDE_ID !! = 0 IF INTERNAL, 1 IF ON INTERNAL BOUNDARY

!
!--Boundary Information: Halo Nodes
!
   INTEGER :: NHN  !!NUMBER OF HALO NODES
   INTEGER, ALLOCATABLE, DIMENSION(:) :: HN_LST !!LIST OF HALO NODES 
   INTEGER, ALLOCATABLE, DIMENSION(:) :: HN_OWN !!PRIMARY OWNER OF HALO NODES

!
!--Communication Objects    [SIZE: NPROCS] 
!
   TYPE(COMM), ALLOCATABLE, DIMENSION(:) :: EC,NC,BNC 

!
!--Maps for Global Array Reconstruction   [SIZE: NPROCS]
!
   TYPE(GMAP), ALLOCATABLE, DIMENSION(:) :: EMAP,NMAP 

!
!--Statistics Calculation (Valid in MASTER Only)   [SIZE: NPROCS]
!
   INTEGER, ALLOCATABLE :: PNE(:)        !!NUMBER OF ELEMENTS IN EACH PROC
   INTEGER, ALLOCATABLE :: PNN(:)        !!NUMBER OF NODES IN EACH PROC
   INTEGER, ALLOCATABLE :: PNHE(:)       !!NUMBER OF HALO ELEMENTS IN EACH PROC
   INTEGER, ALLOCATABLE :: PNBN(:)       !!NUMBER OF INTERNAL BOUNDARY NODES IN EACH PROC
   INTEGER, ALLOCATABLE :: PMBM(:)       !!MAX MULTIPLICITY OF INTERNAL BOUNDARY NODES
   INTEGER, ALLOCATABLE :: PNHN(:)       !!NUMBER OF HALO NODES IN EACH PROC

!===================================================================================|
   CONTAINS   !!INCLUDED SUBROUTINES FOLLOW
!===================================================================================|

   SUBROUTINE INIT_MPI_ENV(MYID,NPROCS,SERIAL,PAR,MSR) 
!===================================================================================|
!  INITIALIZE MPI ENVIRONMENT                                                       |
!===================================================================================|
   INTEGER, INTENT(OUT) :: MYID,NPROCS
   LOGICAL, INTENT(OUT) :: SERIAL,PAR,MSR
   INTEGER IERR
   CALL MPI_INIT(IERR)
   CALL MPI_COMM_RANK(MPI_COMM_WORLD,MYID,IERR)
   CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NPROCS,IERR)
   MYID = MYID + 1
   IF(NPROCS > 1) SERIAL=.FALSE.
   IF(NPROCS > 1) PAR   =.TRUE.
   IF(MYID /=  1) MSR   =.FALSE.

   RETURN
   END SUBROUTINE INIT_MPI_ENV
   



   SUBROUTINE EXCHANGE(CM,NT,KT,MYID,NPROCS,A,B,C) 
!===================================================================================|
!    PASS ELEMENT/NODE INFORMATION AMONG PROCESSORS                                 |
!===================================================================================|

!------------------------------------------------------------------------------
   USE MOD_TYPES
   IMPLICIT NONE
!   include "mpif.h"
!------------------------------------------------------------------------------
   INTEGER, INTENT(IN)             :: NT,KT,MYID,NPROCS
   TYPE(COMM), INTENT(IN)          :: CM(NPROCS)
   REAL(SP), INTENT(INOUT)           :: A(0:NT,KT)
   REAL(SP), INTENT(INOUT), OPTIONAL :: B(0:NT,KT)
   REAL(SP), INTENT(INOUT), OPTIONAL :: C(0:NT,KT)
!------------------------------------------------------------------------------
   LOGICAL             :: BYES,CYES
   INTEGER               ::IREQR(NPROCS),IREQS(NPROCS)
   REAL(SP), ALLOCATABLE :: RBUF(:),SBUF(:)
   INTEGER   STAT(MPI_STATUS_SIZE),ISTATR(MPI_STATUS_SIZE,NPROCS),IERR,J,N1,N2,NCNT
   INTEGER   I,IFROM,ITO,ISTAG,IRTAG,TRCV,TSND,NVARS,LBUF,LP,NMSG,INDX,LPROC,NSZE
!------------------------------------------------------------------------------

   NVARS = 1 ; BYES = .FALSE. ; CYES = .FALSE.
   IF(PRESENT(B)) THEN
     NVARS = NVARS + 1
     BYES  = .TRUE.
   END IF
      
   IF(PRESENT(C)) THEN
     NVARS = NVARS + 1
     CYES = .TRUE. 
   END IF

   ALLOCATE(RBUF(NVARS*SUM(CM(1:NPROCS)%NRCV*KT)))
   ALLOCATE(SBUF(NVARS*SUM(CM(1:NPROCS)%NSND*KT)))

!===================================================================================|
!    POST NON-BLOCKING RECEIVES FROM NEIGHBORS                                      |
!===================================================================================|
   TRCV = 0
   DO I=1,NPROCS

     IF(CM(I)%NRCV > 0)THEN
       IFROM = I-1
       IRTAG = I*1000
       TRCV  = TRCV + 1
       LP    = CM(I)%RCPT*NVARS*KT + 1
       LBUF  = NVARS * CM(I)%NRCV *KT
       CALL MPI_IRECV(RBUF(LP),LBUF,MPI_F,IFROM,IRTAG,MPI_COMM_WORLD,IREQR(TRCV),IERR)
     END IF

   END DO

!===================================================================================|
!    SEND DATA TO NEIGHBORS                                                         |
!===================================================================================|
   TSND = 0
   NCNT = 0
   DO I=1,NPROCS
     LBUF = CM(I)%NSND
     IF(LBUF > 0)THEN
       NSZE = LBUF*KT*NVARS
!       ALLOCATE(SBUF(NSZE))
       N2 = NCNT 
       DO J=1,KT
         N1 = N2+1  ; N2 = N1 + LBUF -1
         SBUF(N1:N2) = A(CM(I)%SNDP(:),J)
         IF(BYES)THEN
           N1 = N2+1 ; N2 = N1 + LBUF -1
           SBUF(N1:N2) = B(CM(I)%SNDP(:),J)
         END IF
         IF(CYES)THEN
           N1 = N2+1 ; N2 = N1 + LBUF -1
           SBUF(N1:N2) = C(CM(I)%SNDP(:),J)
         END IF
       END DO
       TSND  = TSND + 1
       ITO   = I-1
       ISTAG = MYID*1000
       CALL MPI_ISEND(SBUF(NCNT+1),NSZE,MPI_F,ITO,ISTAG,MPI_COMM_WORLD,IREQS(TSND),IERR)
       NCNT = NCNT + LBUF*KT*NVARS 
!       DEALLOCATE(SBUF)
     END IF
   END DO


!===================================================================================|
!    LOOP OVER PROCS UNTIL A MESSAGE IS RECIEVED AND UNPACK                         |
!===================================================================================|
   DO NMSG = 1,TRCV 
     CALL MPI_WAITANY(TRCV,IREQR,INDX,STAT,IERR)
     LPROC = STAT(MPI_SOURCE) +1 
     LP    = CM(LPROC)%RCPT*NVARS*KT 
     LBUF  = CM(LPROC)%NRCV
     N2 = LP
     DO J=1,KT
       N1 = N2+1 ; N2 = N1 + LBUF -1
       A(CM(LPROC)%RCVP(:),J) = RBUF(N1:N2) 
       IF(BYES)THEN
         N1 = N2+1; N2 = N1 + LBUF -1
         B(CM(LPROC)%RCVP(:),J) = RBUF(N1:N2) 
       END IF
       IF(CYES)THEN
         N1 = N2+1 ; N2 = N1 + LBUF -1
         C(CM(LPROC)%RCVP(:),J) = RBUF(N1:N2) 
       END IF
     END DO
   END DO

!===================================================================================|
!    WAIT FOR COMPLETION OF NON-BLOCKING SENDS                                      |
!===================================================================================|

   CALL MPI_WAITALL(TSND,IREQS,ISTATR,IERR)
   DEALLOCATE(RBUF,SBUF)

   RETURN
   END SUBROUTINE EXCHANGE
!===================================================================================|


   SUBROUTINE NODE_MATCH(IMATCH,NBN,BN_MLT,BN_LOC,CM,NT,KT,MYID,NPROCS,A,B,C) 
!===================================================================================|
! IMATCH=1:   ENFORCE AGREEMENT OF A,B,C ON BOUNDARY NODES                          |
! IMATCH=0:   ACCUMULATE VALUES OF A,B,C AT BOUNDARY NODES                          |
!===================================================================================|

!------------------------------------------------------------------------------
   USE MOD_TYPES
   IMPLICIT NONE
!   include "mpif.h"
!------------------------------------------------------------------------------
   INTEGER, INTENT(IN)             :: IMATCH
   INTEGER, INTENT(IN)             :: NBN
   INTEGER, INTENT(IN)             :: BN_MLT(NBN)
   INTEGER, INTENT(IN)             :: BN_LOC(NBN)
   INTEGER, INTENT(IN)             :: NT,KT,MYID,NPROCS
   TYPE(COMM), INTENT(IN)          :: CM(NPROCS)
   REAL(SP), INTENT(INOUT)           :: A(0:NT,KT)
   REAL(SP), INTENT(INOUT), OPTIONAL :: B(0:NT,KT)
   REAL(SP), INTENT(INOUT), OPTIONAL :: C(0:NT,KT)
!------------------------------------------------------------------------------
   LOGICAL             :: BYES,CYES
   INTEGER               ::IREQR(NPROCS),IREQS(NPROCS)
   REAL(SP), ALLOCATABLE :: RBUF(:),SBUF(:)
   INTEGER   STAT(MPI_STATUS_SIZE),ISTATR(MPI_STATUS_SIZE,NPROCS),IERR,J,N1,N2,NCNT
   INTEGER   I,IFROM,ITO,ISTAG,IRTAG,TRCV,TSND,NVARS,LBUF,LP,NMSG,INDX,LPROC,NSZE
!------------------------------------------------------------------------------


   NVARS = 1 ; BYES = .FALSE. ; CYES = .FALSE.
   IF(PRESENT(B)) THEN
     NVARS = NVARS + 1
     BYES  = .TRUE.
   END IF
      
   IF(PRESENT(C)) THEN
     NVARS = NVARS + 1
     CYES = .TRUE. 
   END IF

   ALLOCATE(RBUF(NVARS*SUM(CM(1:NPROCS)%NRCV*KT)))
   ALLOCATE(SBUF(NVARS*SUM(CM(1:NPROCS)%NSND*KT)))

!===================================================================================|
!    POST NON-BLOCKING RECEIVES FROM NEIGHBORS                                      |
!===================================================================================|
   TRCV = 0
   DO I=1,NPROCS

     IF(CM(I)%NRCV > 0)THEN
       IFROM = I-1
       IRTAG = I*1000
       TRCV  = TRCV + 1
       LP    = CM(I)%RCPT*NVARS*KT + 1
       LBUF  = NVARS * CM(I)%NRCV *KT
       CALL MPI_IRECV(RBUF(LP),LBUF,MPI_F,IFROM,IRTAG,MPI_COMM_WORLD,IREQR(TRCV),IERR)
     END IF

   END DO

!===================================================================================|
!    SEND DATA TO NEIGHBORS                                                         |
!===================================================================================|
   TSND = 0
   NCNT = 0
   DO I=1,NPROCS
     LBUF = CM(I)%NSND
     IF(LBUF > 0)THEN
       NSZE = LBUF*KT*NVARS
!       ALLOCATE(SBUF(NSZE))
       N2 = NCNT 
       DO J=1,KT
         N1 = N2+1  ; N2 = N1 + LBUF -1
         SBUF(N1:N2) = A(CM(I)%SNDP(:),J)
         IF(BYES)THEN
           N1 = N2+1 ; N2 = N1 + LBUF -1
           SBUF(N1:N2) = B(CM(I)%SNDP(:),J)
         END IF
         IF(CYES)THEN
           N1 = N2+1 ; N2 = N1 + LBUF -1
           SBUF(N1:N2) = C(CM(I)%SNDP(:),J)
         END IF
       END DO
       TSND  = TSND + 1
       ITO   = I-1
       ISTAG = MYID*1000
       CALL MPI_ISEND(SBUF(NCNT+1),NSZE,MPI_F,ITO,ISTAG,MPI_COMM_WORLD,IREQS(TSND),IERR)
       NCNT = NCNT + LBUF*KT*NVARS 
!       DEALLOCATE(SBUF)
     END IF
   END DO


!===================================================================================|
!    LOOP OVER PROCS UNTIL A MESSAGE IS RECIEVED AND UNPACK                         |
!===================================================================================|
   DO NMSG = 1,TRCV 
     CALL MPI_WAITANY(TRCV,IREQR,INDX,STAT,IERR)
     LPROC = STAT(MPI_SOURCE) +1 
     LP    = CM(LPROC)%RCPT*NVARS*KT 
     LBUF  = CM(LPROC)%NRCV
     N2 = LP
     DO J=1,KT
       N1 = N2+1 ; N2 = N1 + LBUF -1
       A(CM(LPROC)%RCVP(:),J) = RBUF(N1:N2) + A(CM(LPROC)%RCVP(:),J) 
       IF(BYES)THEN
         N1 = N2+1; N2 = N1 + LBUF -1
         B(CM(LPROC)%RCVP(:),J) = RBUF(N1:N2) + B(CM(LPROC)%RCVP(:),J)
       END IF
       IF(CYES)THEN
         N1 = N2+1 ; N2 = N1 + LBUF -1
         C(CM(LPROC)%RCVP(:),J) = RBUF(N1:N2) + C(CM(LPROC)%RCVP(:),J) 
       END IF
     END DO
   END DO

!===================================================================================|
!    WAIT FOR COMPLETION OF NON-BLOCKING SENDS                                      |
!===================================================================================|

   CALL MPI_WAITALL(TSND,IREQS,ISTATR,IERR)
   DEALLOCATE(RBUF,SBUF)

!===================================================================================|
!  USE MULTIPLICITY OF NODES TO COMPUTE TRUE AVERAGE VALUE                          |
!===================================================================================|
   IF(IMATCH /=1)RETURN

   DO J=1,KT
     DO I=1,NBN
       A( BN_LOC(I),J) = A( BN_LOC(I),J)/FLOAT(BN_MLT(I))
       IF(BYES)B( BN_LOC(I),J) = B( BN_LOC(I),J)/FLOAT(BN_MLT(I))
       IF(CYES)C( BN_LOC(I),J) = C( BN_LOC(I),J)/FLOAT(BN_MLT(I))
     END DO
   END DO


   RETURN
   END SUBROUTINE NODE_MATCH
!===================================================================================|

!::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

!===================================================================================|
   SUBROUTINE GATHER(N1,N2,NT,NTG,KT,MYID,NPROCS,GM,A,AG) 
!===================================================================================|
!    GATHER ARRAY/VECTOR A INTO GLOBAL ARRAY/VECTOR                                 |
!    LOCAL ARRAYS A(0:NT,KT) --> AG(0:NTG,KT) BY MAPPING GM                         |
!    UPON COMPLETION ONLY MASTER PROCESSOR HAS AG                                   |
!===================================================================================|
!   Make this an allocatable array-valued function GWC

!------------------------------------------------------------------------------
   USE MOD_TYPES
   IMPLICIT NONE
!   include "mpif.h"
!------------------------------------------------------------------------------
   INTEGER,   INTENT(IN)           :: N1,N2,NT,NTG,KT,MYID,NPROCS
   TYPE(GMAP), INTENT(IN)           :: GM(NPROCS)
   REAL(SP),    INTENT(IN)           :: A(N1:N2,KT)
   REAL(SP),    INTENT(OUT)          :: AG(NTG,KT)
!------------------------------------------------------------------------------
   REAL(SP), ALLOCATABLE :: SBUF(:,:),RBUF(:,:)
   INTEGER   STAT(MPI_STATUS_SIZE),IERR,I,IP,J,ISND,IRCV,NSZE,INC
!------------------------------------------------------------------------------

!===================================================================================|
!     CONFINE ARRAY TO INTERIOR ELEMENTS/NODES                                      |
!===================================================================================|
   IF(MYID /=1)THEN
     ALLOCATE(SBUF(NT,KT))
     DO I=1,NT
     DO J=1,KT
       SBUF(I,J) = A(I,J) 
     END DO
     END DO
   END IF

!===================================================================================|
!     SEND INFORMATION TO MASTER PROCESSOR                                          |
!===================================================================================|
   IF(MYID /= 1)THEN
     ISND = MYID+100
     CALL MPI_SEND(SBUF,NT*KT,MPI_F,0,ISND,MPI_COMM_WORLD,IERR)
     DEALLOCATE(SBUF)
     RETURN
   END IF
!===================================================================================|
!     COPY MASTER PROC DATA INTO GLOBAL ARRAY                                       |
!===================================================================================|

   DO I=1,GM(MYID)%NSIZE
     DO J=1,KT
       AG(GM(MYID)%LOC_2_GL(I),J) = A(I,J)
     END DO
   END DO
!===================================================================================|
!     RECEIVE SLAVE DATA AND UNPACK INTO GLOBAL ARRAY                               |
!===================================================================================|

   DO IP=2,NPROCS
     NSZE = (GM(IP)%NSIZE)
     ALLOCATE(RBUF(NSZE,KT))
     IRCV = IP+100
     CALL MPI_RECV(RBUF,NSZE*KT,MPI_F,IP-1,IRCV,MPI_COMM_WORLD,STAT,IERR)
     DO I=1,GM(IP)%NSIZE
       DO J=1,KT
         AG(GM(IP)%LOC_2_GL(I),J) = RBUF(I,J)
       END DO
     END DO
     DEALLOCATE(RBUF)
   END DO
   
   

   RETURN
   END SUBROUTINE GATHER 
!===================================================================================|

!::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


!===================================================================================|
   SUBROUTINE IGATHER(N1,N2,NT,NTG,KT,MYID,NPROCS,GM,A,AG) 
!===================================================================================|
!    INTEGER GATHER							            |
!    GATHER ARRAY/VECTOR A INTO GLOBAL ARRAY/VECTOR                                 |
!    LOCAL ARRAYS A(0:NT,KT) --> AG(0:NTG,KT) BY MAPPING GM                         |
!    UPON COMPLETION ONLY MASTER PROCESSOR HAS AG                                   |
!===================================================================================|
!   Make this an allocatable array-valued function GWC

!------------------------------------------------------------------------------
   USE MOD_TYPES
   IMPLICIT NONE
!   include "mpif.h"
!------------------------------------------------------------------------------
   INTEGER,   INTENT(IN)           :: N1,N2,NT,NTG,KT,MYID,NPROCS
   TYPE(GMAP), INTENT(IN)           :: GM(NPROCS)
   INTEGER,   INTENT(IN)           :: A(N1:N2,KT)
   INTEGER,   INTENT(OUT)          :: AG(NTG,KT)
!------------------------------------------------------------------------------
   INTEGER, ALLOCATABLE :: SBUF(:,:),RBUF(:,:)
   INTEGER   STAT(MPI_STATUS_SIZE),IERR,I,IP,J,ISND,IRCV,NSZE,INC
!------------------------------------------------------------------------------

!===================================================================================|
!     CONFINE ARRAY TO INTERIOR ELEMENTS/NODES                                      |
!===================================================================================|
   IF(MYID /=1)THEN
     ALLOCATE(SBUF(NT,KT))
     DO I=1,NT
     DO J=1,KT
       SBUF(I,J) = A(I,J) 
     END DO
     END DO
   END IF

!===================================================================================|
!     SEND INFORMATION TO MASTER PROCESSOR                                          |
!===================================================================================|
   IF(MYID /= 1)THEN
     ISND = MYID+100
     CALL MPI_SEND(SBUF,NT*KT,MPI_INTEGER,0,ISND,MPI_COMM_WORLD,IERR)
     DEALLOCATE(SBUF)
     RETURN
   END IF
!===================================================================================|
!     COPY MASTER PROC DATA INTO GLOBAL ARRAY                                       |
!===================================================================================|

   DO I=1,GM(MYID)%NSIZE
     DO J=1,KT
       AG(GM(MYID)%LOC_2_GL(I),J) = A(I,J)
     END DO
   END DO
!===================================================================================|
!     RECEIVE SLAVE DATA AND UNPACK INTO GLOBAL ARRAY                               |
!===================================================================================|

   DO IP=2,NPROCS
     NSZE = (GM(IP)%NSIZE)
     ALLOCATE(RBUF(NSZE,KT))
     IRCV = IP+100
     CALL MPI_RECV(RBUF,NSZE*KT,MPI_INTEGER,IP-1,IRCV,MPI_COMM_WORLD,STAT,IERR)
     DO I=1,GM(IP)%NSIZE
       DO J=1,KT
         AG(GM(IP)%LOC_2_GL(I),J) = RBUF(I,J)
       END DO
     END DO
     DEALLOCATE(RBUF)
   END DO
   
   

   RETURN
   END SUBROUTINE IGATHER 
!===================================================================================|

!::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

!===================================================================================|
   SUBROUTINE SORT(N,M,SZE)
!===================================================================================|
!    SORT ELEMENTS IN N AND RETURN ORDER IN M                                       |
!===================================================================================|

!------------------------------------------------------------------------------
   IMPLICIT NONE
   INTEGER, INTENT(IN)             :: SZE
   INTEGER, INTENT(INOUT)          :: N(SZE)
   INTEGER, INTENT(OUT)            :: M(SZE)
!------------------------------------------------------------------------------
   INTEGER I,LAST,J(1)
   REAL(SP)  VALM
   INTRINSIC MINLOC
!------------------------------------------------------------------------------

   M = 0
   LAST = 0
   DO I=1,SZE
     J = MINLOC(N,MASK=N > LAST)
     M(I) = J(1)
     LAST = N(J(1))
   END DO

   RETURN
   END SUBROUTINE SORT
!===================================================================================|


!::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

   INTEGER FUNCTION GETLOC(GLOC,GMAP,N)
!===================================================================================|
!    DETERMINE LOCAL IDENTITY OF ELEMENT/NODE I USING GMAP                           |
!===================================================================================|

!------------------------------------------------------------------------------
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: GLOC,N
   INTEGER, INTENT(IN) :: GMAP(N)
   INTEGER  I,FOUND
!------------------------------------------------------------------------------
   FOUND = 0
   DO I=1,N
     IF(GMAP(I)==GLOC) FOUND = I
   END DO

   GETLOC = FOUND
   RETURN 
   END FUNCTION GETLOC
!===================================================================================|

!::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


!==============================================================================|
!     READ IN REAL GLOBAL ARRAY AND DECOMPOSE TO LOCAL                         |
!==============================================================================|

   SUBROUTINE PREAD(IUNIT,VAR,I1,I2,IT,GI2,K2,GID,IRS,VNAME)  

!==============================================================================|
   USE MOD_UTILS
   IMPLICIT NONE
   INTEGER, INTENT(IN)  :: IUNIT,I1,I2,IT,GI2,K2,IRS
   INTEGER, INTENT(IN)  :: GID(IT) 
   CHARACTER(LEN=*),INTENT(IN)     :: VNAME
   REAL(SP), INTENT(OUT), DIMENSION(I1:I2,K2) :: VAR 
   REAL(SP), DIMENSION(0:GI2,K2) :: TEMP 
   INTEGER :: I,J
   INTEGER IERR 
!==============================================================================|

!
!--READ IN GLOBAL ARRAY
!
   READ(IUNIT,IOSTAT=IERR) ((TEMP(I,J),J=1,K2),I=IRS,GI2)
!
!--ERROR CHECK
!
   IF(IERR /= 0)  CALL PERROR(6,"ERROR READING IN HOT START: ",VNAME) 
     
!
!--DECOMPOSE
!
   IF(I1 == 0) VAR(0,:) = TEMP(0,:)
   DO I=1,IT
     DO J=1,K2
     VAR(I,J) = TEMP(GID(I),J)
     END DO
   END DO

   RETURN
   END SUBROUTINE PREAD
!==============================================================================|

!==============================================================================|
!     GATHER LOCAL ARRAY TO GLOBAL AND WRITE TO FILE                           |
!==============================================================================|

   SUBROUTINE PWRITE(IUNIT,MYID,NPROCS,VAR,I1,I2,IT,GI2,K2,MP,IRS,VNAME) 

!==============================================================================|
   IMPLICIT NONE
   INTEGER, INTENT(IN)  :: IUNIT,I1,I2,IT,GI2,K2,IRS,MYID,NPROCS
   TYPE(GMAP), INTENT(IN)  :: MP(NPROCS) 
   CHARACTER(LEN=*),INTENT(IN)     :: VNAME
   REAL(SP), INTENT(IN), DIMENSION(I1:I2,K2) :: VAR
   REAL(SP), DIMENSION(GI2,K2) :: TEMP
   REAL(SP), DIMENSION(0:GI2,K2) :: TEMP2
   INTEGER :: I,J
!==============================================================================|

!
!  GATHER                
!
   CALL GATHER(I1,I2,IT,GI2,K2,MYID,NPROCS,MP,VAR,TEMP)  

!
!  WRITE TO FILE
!
   IF(MYID==1)THEN
     TEMP2(0,:) = 0.0_SP
     TEMP2(1:GI2,1:K2) = TEMP(1:GI2,1:K2)
     WRITE(IUNIT) ((TEMP2(I,J),J=1,K2),I=IRS,GI2)
   END IF

   RETURN
   END SUBROUTINE PWRITE
!==============================================================================|
!==============================================================================|

   SUBROUTINE PPRINT(IUNIT,LB1,UB1,UB2,VARP,VART,ILOC,K1,K2,REF,IPT)

!==============================================================================|
!  WRITE OUT VARIABLE INFORMATION TO LOCAL FILES                               |
!									       |
!  USAGE EXAMPLES                                                              |
!									       |
!  write u velocity at surface in triangle 256 to file fort.306 with iteration |
!  I1 = LBOUND(U,1) ; I2 = UBOUND(U,1)                                         |
!  CALL PPRINT(306,I1,I2,KB,U,"element",256,1,1,FLOAT(IINT))                   |
!									       |
!  I1 = LBOUND(EL,1) ; I2 = UBOUND(EL,1)                                       |
!  write surface elevation at node 233 to file fort.409 with time in hours     |
!  CALL PPRINT(406,I1,I2,1,EL,"node",233,1,1,THOUR)                            |
!									       |
!  I1 = LBOUND(T1,1) ; I2 = UBOUND(T1,1)                                       |
!  write vertical distribution of salinity at node 422 to file fort.433        |
!  CALL PPRINT(433,I1,I2,KB,T1,"node",422,1,KBM1,THOUR)                        |
!									       |
!  ARGUMENT LIST                                                               |
!       PPRINT(IUNIT,LB1,UB1,UB2,VARP,VART,ILOC,K1,K2,REF)                     |
!    1.) IUNIT - UNIT NUMBER FOR OUTPUT FILE (MUST BE >= 300)                  |
!    2.) LB1   - LOWER BOUND OF 1ST ARGUMENT OF ARRAY TO PRINT (USUAlLY 0)     |
!    3.) LB2   - UPPER BOUND OF 1ST ARRAY DIMENSION (USUALLY NT OR MT)         |
!        NOTE: LB1/LB2 CAN BE DETERMINE AUTOMATICALLY WITH LBOUND/UBOUND       |
!    4.) UB2   - UPPER BOUND OF SECOND ARRAY DIMENSION                         | 
!        UB2   = 1 FOR SURFACE ARRAYS LIKE EL,UA                               |
!        UB2   = KB FOR 3D ARRAYS LIKE U/V                                     |
!    5.) VARP  = VARIABLE TO PRINT (ARRAY NAME = U,V,WW,EL,T1,RHO1, etc)       |
!    6.) VART  = VARIABLE LOCATION ("element" or "node")                       |
!    7.) ILOC  = INDEX OF ELEMENT/NODE TO PRINT                                | 
!    8.) K1    = LOWER RANGE OF SIGMA LEVEL TO PRINT                           |
!    9.) K2    = UPPER RANGE OF SIGMA LEVEL TO PRINT                           |
!        K1 = 1,K2 = 1 FOR SURFACE VALUES ONLY                                 |
!        K1 = 1,K2 = KBM1 FOR ALL LEVELS                                       |
!   10.) REF   = REFERENCE VALUE FOR DATA (MUST BE FLOAT)                      |
!        REF = THOURS FOR CALCULATION TIME IN HOURS                            |
!        REF = FLOAT(IINT) FOR ITERATION NUMBER                                |
!   11.) IPT = UNIT TO WRITE ERRORS TO (USE IPT)                               |
!==============================================================================|

   USE LIMS
   IMPLICIT NONE
   INTEGER,  INTENT(IN) :: IUNIT,LB1,UB1,UB2,ILOC,K1,K2,IPT
   REAL(SP), INTENT(IN) :: VARP(LB1:UB1,UB2)
   REAL(SP), INTENT(IN) :: REF 
   CHARACTER(LEN=*), INTENT(IN) :: VART

   INTEGER :: I,J,K,PROCMAX,II,IBND
   LOGICAL :: PRINT_PROC

!==============================================================================|
   
!------------------------------------------------------------------------------|
!  Process Iunit for Errors                                                    |
!------------------------------------------------------------------------------|
   IF(IUNIT < 300)THEN
     WRITE(IPT,*)'ERROR IN PPRINT'
     WRITE(IPT,*)'FILE UNIT < 300 RESERVED FOR FVCOM I/O'
     WRITE(IPT,*)'PLEASE INCREASE IUNIT TO 300+'
     CALL PSTOP
   END IF

!------------------------------------------------------------------------------|
!  Process Vartype for Errors                                                  |
!------------------------------------------------------------------------------|
   IF(VART /= "element" .AND. VART /= "node")THEN
     WRITE(IPT,*)'VART IN PPRINT NOT CORRECT',TRIM(VART)
     WRITE(IPT,*)'SHOULD BE "element" or "node"'
     CALL PSTOP
   END IF
    
!------------------------------------------------------------------------------|
!  Single Processor Case                                                       |
!------------------------------------------------------------------------------|
   IF(NPROCS == 1)THEN
     WRITE(IUNIT,*)REF,(VARP(ILOC,K),K=K1,K2)
     RETURN
   END IF

!------------------------------------------------------------------------------|
!  Multi Processor Case with Element Based Variable (u,v,ww, etc)              |
!      Transform to Local Element ID with "ELID"                               |
!------------------------------------------------------------------------------|

#  if defined (MULTIPROCESSOR)
   IF(NPROCS /= 1 .AND. VART == "element")THEN
     IF(ELID(ILOC) /= 0)THEN
       WRITE(IUNIT,*)REF,(VARP(ELID(ILOC),K),K=K1,K2)
     END IF
     RETURN
   END IF
#  endif
   
!------------------------------------------------------------------------------|
!  Multi Processor Case with Node Based Variable (s1,t1,rho1,e1, etc)          |
!      Transform to Local Node ID with "NLID"                                  |
!      If Node is Interprocessor Boundary Node, Choose Processor with Highest  |
!      ID Number to Write Values to File                                       |
!------------------------------------------------------------------------------|

#  if defined (MULTIPROCESSOR)
   IF(NPROCS /= 1 .AND. VART == "node" .AND. NLID(ILOC) > 0)THEN

     IF(NDE_ID(NLID(ILOC)) == 1)THEN   !!BOUNDARY NODE
       PRINT_PROC = .TRUE.

       DO II=1,NBN
         IF(BN_LST(II) == ILOC) IBND = II
       END DO

       PROCMAX = 10000
       DO J=1,NPROCS
         IF(BN_NEY(IBND,J)==1) THEN
           IF(J < PROCMAX) PROCMAX = J
         END IF
       END DO
                                                                                                                            
       IF(PROCMAX /=  MYID) PRINT_PROC = .FALSE.  !!NOT RESPONSIBLE FOR OUTPUT 
     END IF

     IF(PRINT_PROC)THEN
       WRITE(IUNIT,*)REF,(VARP(NLID(ILOC),K),K=K1,K2)
     END IF

   END IF
   RETURN
#  endif

   RETURN
   END SUBROUTINE PPRINT
!==============================================================================|
# endif
END MODULE MOD_PAR  
