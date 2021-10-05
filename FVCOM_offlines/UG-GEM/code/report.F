
!==============================================================================|
!     REPORT INITIAL INFORMATION                                               |
!==============================================================================|

   SUBROUTINE REPORT(INFO_STRING)

!==============================================================================|
   USE ALL_VARS

   IMPLICIT NONE
   CHARACTER(LEN=*) :: INFO_STRING     !!INFORMATION STRING
   INTEGER :: E3TOT,ESTOT,IERR
   REAL(DP), DIMENSION(17) :: SBUF,RBUF1,RBUF2,RBUF3
   
   REAL(SP), ALLOCATABLE :: KH1(:,:),KQ1(:,:)
   INTEGER :: I,J,K
!==============================================================================|

   ALLOCATE(KH1(1:N,KBM1));   KH1   = 0.0_SP
   
   DO K=1,KBM1
     DO I=1,N
       DO J=1,3
         KH1(I,K)  = KH1(I,K)+KH(NV(I,J),K)
       END DO
       KH1(I,K)  = KH1(I,K)/3.0_SP
     END DO
   END DO    	    
      
   SBUF(1)  = SUM(DBLE(UA(1:N)))
   SBUF(2)  = SUM(DBLE(VA(1:N)))
   SBUF(3)  = SUM(DBLE(EL1(1:N)))
   SBUF(4)  = SUM(DBLE(H1(1:N)))
   SBUF(5)  = SUM(DBLE(U(1:N,1:KBM1)))
   SBUF(6)  = SUM(DBLE(V(1:N,1:KBM1)))
   SBUF(14) = SUM(DBLE(KH1(1:N,1:KBM1)))
   SBUF(16) = SUM(DBLE(D1(1:N)))
   SBUF(17) = FLOAT(N)


   RBUF1 = SBUF
# if defined (MULTIPROCESSOR)
   IF(PAR)CALL MPI_REDUCE(SBUF,RBUF1,17,MPI_DP,MPI_SUM,0,MPI_COMM_WORLD,IERR)
# endif

   SBUF(1)  = MAXVAL(UA(1:N))
   SBUF(2)  = MAXVAL(VA(1:N))
   SBUF(3)  = MAXVAL(EL(1:M))
   SBUF(4)  = MAXVAL(H(1:M))
   SBUF(5)  = MAXVAL(U(1:N,1:KBM1))
   SBUF(6)  = MAXVAL(V(1:N,1:KBM1))
   SBUF(7)  = MAXVAL(S1(1:M,1:KBM1))
   SBUF(8)  = MAXVAL(T1(1:M,1:KBM1))
   SBUF(14) = MAXVAL(KH(1:M,1:KBM1))
   SBUF(16) = MAXVAL(D(1:M))

   RBUF2 = SBUF
# if defined (MULTIPROCESSOR)
   IF(PAR)CALL MPI_REDUCE(SBUF,RBUF2,16,MPI_DP,MPI_MAX,0,MPI_COMM_WORLD,IERR)
# endif

   SBUF(1)  = MINVAL(UA(1:N))
   SBUF(2)  = MINVAL(VA(1:N))
   SBUF(3)  = MINVAL(EL(1:M))
   SBUF(4)  = MINVAL(H(1:M))
   SBUF(5)  = MINVAL(U(1:N,1:KBM1))
   SBUF(6)  = MINVAL(V(1:N,1:KBM1))
   SBUF(7)  = MINVAL(S1(1:M,1:KBM1))
   SBUF(8)  = MINVAL(T1(1:M,1:KBM1))
   SBUF(14) = MINVAL(KH(1:M,1:KBM1))
   SBUF(16) = MINVAL(D(1:M))

   RBUF3 = SBUF
# if defined (MULTIPROCESSOR)
   IF(PAR)CALL MPI_REDUCE(SBUF,RBUF3,16,MPI_DP,MPI_MIN,0,MPI_COMM_WORLD,IERR)
# endif

   IF(MSR)THEN
   IF(LEN_TRIM(INFO_STRING) /= 0)THEN
     WRITE(IPT,*  )'===================',TRIM(INFO_STRING),'======================'
   END IF
   RBUF1(15) = (RBUF1(15)+NGL*KBM1)*1000.
   RBUF2(15) = (RBUF2(15)+1.)*1000.
   RBUF3(15) = (RBUF3(15)+1.)*1000.
   E3TOT = DBLE(NGL*KBM1)
   ESTOT = DBLE(NGL)
   WRITE(IPT,*  )'!  QUANTITY              :     AVG           MAX         MIN'
   WRITE(IPT,100)'!  EXTERNAL UVEL         :',RBUF1(1)/ESTOT,RBUF2(1),RBUF3(1)
   WRITE(IPT,100)'!  EXTERNAL VVEL         :',RBUF1(2)/ESTOT,RBUF2(2),RBUF3(2)
   WRITE(IPT,100)'!  FREE SURFACE          :',RBUF1(3)/ESTOT,RBUF2(3),RBUF3(3)
   WRITE(IPT,100)'!  BATH DEPTH            :',RBUF1(4)/ESTOT,RBUF2(4),RBUF3(4)
   WRITE(IPT,100)'!  INTERNAL UVEL         :',RBUF1(5)/E3TOT,RBUF2(5),RBUF3(5)
   WRITE(IPT,100)'!  INTERNAL VVEL         :',RBUF1(6)/E3TOT,RBUF2(6),RBUF3(6)
   WRITE(IPT,100)'!  SALINITY              :',RBUF1(7)/E3TOT,RBUF2(7),RBUF3(7)
   WRITE(IPT,100)'!  TEMPERATURE           :',RBUF1(8)/E3TOT,RBUF2(8),RBUF3(8)
   WRITE(IPT,100)'!  KH                    :',RBUF1(14)/E3TOT,RBUF2(14),RBUF3(14)
   WRITE(IPT,100)'!  DEPTH                 :',RBUF1(16)/ESTOT,RBUF2(16),RBUF3(16)

   END IF

   DEALLOCATE(KH1)
   
   RETURN
   100 FORMAT(1X,A26,3F12.6)
   101 FORMAT(1X,A26,2I12,F12.6)
   END SUBROUTINE REPORT 
!==============================================================================|

