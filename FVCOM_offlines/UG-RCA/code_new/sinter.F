      SUBROUTINE SINTER(X,A,Y,B,M,N)
      IMPLICIT NONE
      SAVE
!
!-----------------------------------------------------------------------
!        A SPECIAL CASE OF INTERP ....NO EXTRAPOLATION BELOW AND ABOVE DATA
!        THIS ROUTINE LINEARLY INTERPOLATES AN ARRAY B
!        X(M) MUST BE DESCENDING
!        A(X) GIVEN FUNCTION
!        B(Y) FOUND BY LINEAR INTERPOLATION AND EXTRAPOLATION
!        Y(N) THE DESIRED DEPTHS
!        M    THE NUMBER OF POINTS IN X AND A
!        N    THE NUMBER OF POINTS IN Y AND B
!-----------------------------------------------------------------------
!
      INTEGER I,M,N,NM,J
      REAL X(M), A(M), Y(N), B(N)
!
!-------- EXTRAPOLATION CASES ------------------------------------------
      DO I = 1, N
        IF(Y(I) > X(1)) B(I) = A(1) 
        IF(Y(I) < X(M)) B(I) = A(M)
      END DO
!
!-------- INTERPOLATION CASES ------------------------------------------
      NM = M - 1
      DO I = 1, N
       DO J = 1, NM
        IF(Y(I) <= X(J) .AND. Y(I) >= X(J+1))                    &
	  B(I) = A(J) - (A(J)- A(J+1)) * (X(J)-Y(I)) / (X(J)-X(J+1))
       END DO
      END DO
!
      RETURN
      
      END SUBROUTINE SINTER
