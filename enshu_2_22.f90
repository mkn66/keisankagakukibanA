program main
    implicit none
    real(8) , allocatable :: a(:,:), b(:,:)
    integer n, i, j
    n = 3
    allocate(a(1:n, 1:n), b(1:n-1, 1:n-1))
    call random_seed
    call random_number(a(1:n, 1:n))
    call random_number(b(1:n-1, 1:n-1))
    do i=1, n
        do j=1, n
            b(1:i-1, 1:j-1) = a(1:i-1,1:j-1)
            b(1:i-1, j:n-1) = a(1:i-1, j+1:n)
            b(i:n-1, 1:j-1) = a(i+1:n, 1:j-1)
            b(i:n-1, j:n-1) = a(i+1:n, j+1:n)
            write(*,'(3e10.4)') a(:,:)
            write(*,'(2e10.4)') b(:,:)
        enddo
    enddo

end program main