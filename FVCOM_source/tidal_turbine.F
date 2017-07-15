!===============================================================================|
!   UPDATE THE TIDAL TURBINE THRUST COEFFICIENT, TO BE CALLED EVERY TIME STEP   |
!===============================================================================|
SUBROUTINE TT_THRUST_UPDATE
	USE ALL_VARS
	IMPLICIT NONE
	REAL(SP) :: TC(2,11)
	REAL(SP) :: CUR_SPEED(0:NT)
	INTEGER :: II
	
	TC(1,1:11) = (/ 0.00, 0.99, 1.00, 2.50, 2.750, 3.00, 3.250, 3.500, 3.75, 4.000, 4.01 /)
	TC(2,1:11) = (/ 0.00, 0.00, 0.85, 0.85, 0.635, 0.49, 0.385, 0.308, 0.25, 0.205, 0.00 /)

	CUR_SPEED  = 0.0_SP
	CT = 0.0_SP
	CUR_SPEED = SQRT(UA**2+VA**2)
	
	DO II=2,11
		WHERE( (CUR_SPEED>TC(1,II-1)) .AND. (CUR_SPEED<=TC(1,II)) .AND. (NTT>0.0) )
		    ! LINEAR INTERPOLATION
			CT = TC(2,II-1) +  ( (CUR_SPEED-TC(1,II-1))*(TC(2,II)-TC(2,II-1))/(TC(1,II)-TC(1,II-1)) )
		END WHERE
	END DO
	
!	WHERE(UA**2 + VA**2 < 1)
!		CT = 0.0;
!	ELSEWHERE
!		CT = 1.0;
!    END WHERE

END SUBROUTINE TT_THRUST_UPDATE
