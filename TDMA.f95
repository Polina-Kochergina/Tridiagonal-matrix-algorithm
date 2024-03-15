program Tridiagonal_matrix_algorithm
use mymod_tdma
implicit none
		
integer :: n, k, i, err, io
real(8), allocatable, dimension(:) :: alpha, beta, gamma, B, X
CHARACTER(LEN=30) :: name

! name of input files with koef
name = "input3.txt"

! opening input and count lines = n
n=0
open(1, file = name)
do
	read(1,*,iostat=io)
	if (io/=0) EXIT
	n = n + 1
end do
close (1)
   
write(*,"(1x, 'dimension = ', i1)") n

! allocate memory for our arrays, we use dinamic arrays
allocate(alpha(n-1),beta(n), gamma(n-1), B(n), X(n), stat=err) !since we start from x[1] (not x[0])
if (err /= 0) print *, "arrays: Allocation request denied"

! reading alpha, beta, gamma and B
! 	|beta(1) gamma(1) 0 ..................... 0 | 
! 	|alpha(1) beta(2) gamma(2) 0 ............ 0 | 
! A=|.......................................... |,    A*X = B
! 	|0 .......0 alpha(n-2) beta(n-1) gamma(n-1) | 
! 	|0 ....................0 alpha(n-1) beta(n) | 

open(1, file = name)

read(1,*) beta(1), gamma(1), B(1)
	do k = 2, n-1
		read(1,*) alpha(k-1), beta(k), gamma(k), B(k)
	enddo
read(1,*) alpha(n-1), beta(n), B(n)

! found X, we use the module where the sweep method is implemented 
X = TDMASolve(alpha, beta, gamma, B, n)


! write x to file .txt
call write_to_file(X, "output.txt")


! don't forget to free up memory
deallocate(alpha, beta, gamma, B, X, stat=err)
if (err /= 0) print *, "arrays: Deallocation request denied"


end program
