!     
! File:   Matriz.f90
! Author: David Reyes
!
! Created on 25 de septiembre de 2015, 01:20 PM
!

MODULE Matriz
    contains
SUBROUTINE Gsselm(AA,row,X,SINGULAR)
	implicit none
	INTEGER, INTENT(IN) :: row
	REAL*8 , INTENT(IN)  ::  AA(ROW,ROW+1) 	!Assume shape (:)
	REAL*8 , DIMENSION(row),INTENT(out) :: X

	INTEGER i,j,k
	REAL*8 :: swap_ik(row+1),A(ROW,ROW+1)
	REAL*8  :: tmp ,EPS=3.0D-15
        LOGICAL, OPTIONAL :: SINGULAR
        SINGULAR=.FALSE.
        A=AA
! 	Initialise
	swap_ik(:) = 0.0d0            ! Whole vector initialized to zero
	tmp = 0.0d0

! Check dimensions of input matrix

!/*   Gaussian Elimination - Row Reduction of matrix  */
	do k=1, row-1                             ! total of row-1 operations

!/*  Pivotal strategy - SWAP rows to make pivotal element a[k][k] have the
!    greatest magnitude in its column. This prevents unnecessary division by
!    a small number.                           */
   	do i = k+1, row
      	if ( (dabs(a(i,k))-dabs(a(k,k))).gt.eps  ) then
         	do j = k, row+1                     !/* If pivotal element is not */
            	swap_ik(j) = a(k,j)              !/* the highest then  */
          	   a(k,j) = a(i,j)                  !/* swap i'th and k'th rows */
            	a(i,j) = swap_ik(j)
         	end do 		!j-loop
         end if
   	end do 				!i-loop


!/*   If the Matrix is SINGULAR then EXIT program      */
  		IF ( dabs(a(k,k)) < EPS ) then
                SINGULAR=.TRUE.
                RETURN
!   	   call break()
	   END if
   


!/*      Perform row-reduction with pivotal element a[k][k]     */
		do i = k+1, row
			do j = row+1, k, -1			!/* starting from end of column */
	     	 	a(i,j) = a(i,j) - a(k,j) / a(k,k) * a(i,k)
			end DO 							!/* end of j loop     */
		end do 	 							!/* end of 2nd i loop */

	end DO 									!/* end of k loop     */
!  At this point, the bottom triangle is Zero


!/*   Back Substitution - Solutions of equations   */
	X(row) = a(row,row+1) / a(row,row)
	do k = row-1, 1, -1
   	tmp = 0.0d0
   	do j = k+1, row
      	tmp = tmp + a(k,j)*X(j)
	   end do 							!j-loop
   	X(k) = ( a(k,row+1) - tmp ) / a(k,k)
	end do 								!k-loop

END SUBROUTINE Gsselm
END MODULE Matriz
