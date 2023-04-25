program main
    implicit none
    real(8) , allocatable :: a(:,:), b(:,:), c(:,:), cc(:,:)
    integer i, j, k, n
    n=5
    allocate(a(n, n), b(n, n), c(n,n), cc(n,n))
    c(n,n) = 0.0d0
    call random_seed
    call random_number(a(1:n, 1:n))
    call random_number(b(1:n, 1:n))
    do j=1,n
        do i=1,n
            do k=1,n
                c(i,j) = c(i, j) + a(i,k) * b(k, j)
            enddo
        enddo
    enddo
    cc(1:n,1:n) = matmul(a(1:n,1:n), b(1:n,1:n))
    write(*,'(5e10.5)') c(1:n,1:n), cc(1:n, 1:n)
end program main