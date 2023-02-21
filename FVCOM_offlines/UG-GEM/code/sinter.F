!==============================================================================|
!                                                                	       |
!  this subroutine linearly interpolates and extrapolates an     	       | 
!  array b.                                                      	       |
!                                                                	       |
!  x(m1) must be descending                                                    |
!  a(x) given function                                           	       |
!  b(y) found by linear interpolation and extrapolation          	       |	
!  y(n1) the desired depths                                      	       |	
!  m1   the number of points in x and a                          	       |	
!  n1   the number of points in y and b                         	       |	
!                                                                              |	
!  a special case of interp ....no extrapolation below data                    |	
!==============================================================================|

   SUBROUTINE SINTER(X,A,Y,B,M1,N1)              

!==============================================================================|
   USE MOD_PREC
   IMPLICIT NONE
   INTEGER, INTENT(IN)   :: M1,N1
   REAL(SP), INTENT(IN)  :: X(M1),A(M1),Y(N1)
   REAL(SP), INTENT(OUT) :: B(N1) 
   INTEGER   I,J,NM
!==============================================================================|

!
!  EXTRAPOLATION
!
   DO I=1,N1
       IF (Y(I) > X(1 )) B(I) = A(1) + ((A(1)-A(2))/(X(1)-X(2))) * (Y(I)-X(1))
       IF (Y(I) < X(M1)) B(I) = A(M1)
   END DO

!
!  INTERPOLATION
!
   NM = M1 - 1
   DO I=1,N1
     DO J=1,NM
        IF (Y(I) <=  X(J) .AND. Y(I) >= X(J+1)) &
              B(I) = A(J) - (A(J)- A(J+1)) * (X(J)-Y(I)) / (X(J)-X(J+1))
     END DO
   END DO

   RETURN
   END SUBROUTINE SINTER
!==============================================================================|

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

!==============================================================================|
   SUBROUTINE SINTER_P(X,A,Y,B,M1,N1)              
!==============================================================================|
!                                                                              |
!  for baroclinic interpolation                                                |
!                          changed by hedong liu                               |
!                           july 12, 2002                                      |
!                                                           	               |
!                                                                	       |
!  this subroutine linearly interpolates and extrapolates an     	       | 
!  array b.                                                      	       |
!                                                                	       |
!  x(m1) must be descending                                                    |
!  a(x) given function                                           	       |
!  b(y) found by linear interpolation and extrapolation          	       |	
!  y(n1) the desired depths                                      	       |	
!  m1   the number of points in x and a                          	       |	
!  n1   the number of points in y and b                         	       |	
!                                                                              |	
!  a special case of interp ....no extrapolation below data                    |	
!==============================================================================|


!==============================================================================|
   USE MOD_PREC
   IMPLICIT NONE
   INTEGER, INTENT(IN)  :: M1,N1
   REAL(SP),  INTENT(IN)  :: X(M1),A(M1),Y(N1)
   REAL(SP),  INTENT(OUT) :: B(N1) 
   INTEGER :: I,J,NM
!==============================================================================|

!
!  EXTRAPOLATION 
!
   DO I=1,N1
     IF(Y(I) > X(1 )) B(I) = A(1)
     IF(Y(I) < X(M1)) B(I)=A(M1)+(A(M1-1)-A(M1))*(Y(I)-X(M1))/(X(M1-1)-X(M1))
   END DO

!
!  INTERPOLATION
!
   NM = M1 - 1
   DO I=1,N1
     DO J=1,NM
       IF (Y(I)<=X(J).AND.Y(I)>=X(J+1)) &
          B(I) = A(J) - (A(J)- A(J+1)) *(X(J)-Y(I)) / (X(J)-X(J+1))
     END DO
   END DO

   RETURN
   END SUBROUTINE SINTER_P 
!==============================================================================|


!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

!==============================================================================|
   SUBROUTINE SINTER_TS(X,A,Y,B,M1,N1)              
!==============================================================================|
!  for t&s obc interpolation                                                   |
!                          changed by hedong liu                               |
!                           july 12, 2002                                      |
!                                                                              |
!                                                                	       |
!  this subroutine linearly interpolates and extrapolates an     	       | 
!  array b.                                                      	       |
!                                                                	       |
!  x(m1) must be descending                                                    |
!  a(x) given function                                           	       |
!  b(y) found by linear interpolation and extrapolation          	       |	
!  y(n1) the desired depths                                      	       |	
!  m1   the number of points in x and a                          	       |	
!  n1   the number of points in y and b                         	       |	
!                                                                              |	
!  a special case of interp ....no extrapolation below data                    |	
!==============================================================================|

   USE MOD_PREC
   IMPLICIT NONE
   INTEGER, INTENT(IN)  :: M1,N1
   REAL(SP),  INTENT(IN)  :: X(M1),A(M1),Y(N1)
   REAL(SP),  INTENT(OUT) :: B(N1) 
   INTEGER :: I,J,NM
!==============================================================================|


!
!   EXTRAPOLATION
!
   DO I=1,N1
     IF (Y(I) > X(1 )) B(I) = A(1)
     IF (Y(I) < X(M1)) B(I) = A(M1)
   END DO

!
!  INTERPOLATION
!
   NM = M1 - 1
   DO I=1,N1
     DO J=1,NM
       IF (Y(I) <= X(J).AND.Y(I) >= X(J+1))  &
       B(I) = A(J) - (A(J)- A(J+1)) * (X(J)-Y(I)) / (X(J)-X(J+1))
     END DO
   END DO

   RETURN
   END SUBROUTINE SINTER_TS
!==============================================================================|


