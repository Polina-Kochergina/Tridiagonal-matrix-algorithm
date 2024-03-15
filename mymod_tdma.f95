module mymod_tdma
implicit none
    
contains 

subroutine det(alpha, beta, gamma, n)
    ! checking the existence of a solution
    ! if determinant = 0 then we have infinite number of solutions

    implicit none
    real(8), dimension(0:2) :: determinant
    integer i,n
    real(8) :: eps
    real(8) :: alpha(:), beta(:), gamma(:)

    determinant(0) = 0
    determinant(1) = 1

    eps = 1.e-6

    ! we use formul: |f_n| = beta(n)*|f_n-1| - alpha(n-1)*gamma(n-1)*|f_n-2|
    do i = 1, n
        if (i>1) then
            determinant(2) = beta(i)*determinant(1) - alpha(i-1)*gamma(i-1)*determinant(0)
            determinant(0) = determinant(1)
            determinant(1) = determinant(2)
        else
            determinant(2) = beta(i)*determinant(1)
            determinant(0) = determinant(1)
            determinant(1) = determinant(2)
        endif
    end do

! we cannot compare real(8) with 0
    if (abs(determinant(2)) < eps) then
        
        print*, "ERROR: There are an infinite number of solutions,&
         use the sweep method it is forbidden. The job has been forced to complete."
        stop 
    end if

    
end subroutine

subroutine write_to_file(X, name)

real(8) :: x(:)
CHARACTER :: name
CHARACTER(LEN=30) :: Format
! format output in fortran
10 format(f10.3)
100 format(5x, a)

! output the results to a file and to the screen
    open(2, file = name)
    write(2, 100) ' X '
    write(*, 100) ' X '
    write(2, 10)  x
    write(*, 10)  x

end subroutine


function TDMASolve(alpha, beta, gamma, B, n) result(x)
    real(8), dimension(:) :: alpha, beta, gamma, B
    real(8), dimension(n) :: u, v, x
    integer :: i, n



    call det(alpha, beta, gamma, n)
    print*, "solve is exist"

    ! first step: (прямой ход = forward stroke)
    ! find v, u are the double sweep coefficients. They are consecutively calculated 
    ! using the following recurrent formulas.

    v(1) = -gamma(1)/beta(1)
    v(n) = 0
    u(1) = B(1)/beta(1)

    do i = 2, n-1
        v(i) = gamma(i)/(-beta(i) - alpha(i-1)*v(i-1))
        u(i) = (alpha(i-1)*u(i-1) - B(i))/(-beta(i) - alpha(i-1)*v(i-1))

    enddo
    
    ! second step: (обратный ход = reverse stroke)
    ! we find X
    x(n) = (alpha(n-1)*u(n-1) - B(n))/(-beta(n) - alpha(n-1)*v(n-1))

    do i = n, 2, -1
        x(i-1) = v(i-1)*x(i) + u(i-1)
    enddo

end function TDMASolve

end module

