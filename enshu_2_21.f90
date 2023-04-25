program main
    implicit none
    real(8) , allocatable :: a(:,:), b(:,:), c(:,:), cc(:,:), at(:,:), bt(:,:)
    real(8) x
    integer n, i, j, k
    n = 3
    allocate(a(n, n), b(n, n), c(n,n), cc(n,n), at(n,n), bt(n,n))
    call random_seed
    call random_number(a(1:n, 1:n))
    call random_number(b(1:n, 1:n))
    bt(1:n,1:n) = b(1:n,1:n)
    at(1:n,1:n) = a(1:n,1:n)
    c(1:n,1:n) = 0.0d0
    cc(1:n,1:n) = 0.0d0
    do j=1,n
        do i=1,n
            do k=1,n
                c(i,j) = c(i, j) + a(i,k) * b(k, j)
            enddo
        enddo
    enddo
    do i = 1, n
        do j=i+1, n
            x = c(i, j)
            c(i,j) = c(j,i)
            c(j,i) = x
            x = at(i,j)
            at(i,j) = at(j,i)
            at(j,i) = x
            x = bt(i,j)
            bt(i,j) = bt(j,i)
            bt(j,i) = x
        enddo
    enddo
    do j=1,n
        do i=1,n
            do k=1,n
                cc(i,j) = cc(i, j) + bt(i,k) * at(k, j)
            enddo
        enddo
    enddo
    write(*,'(3e10.5)') c(:,:), cc(:,:)
    c(1:n,1:n) = transpose(matmul(a(1:n,1:n),b(1:n,1:n)))
    cc(1:n,1:n) = matmul(bt(1:n,1:n),at(1:n,1:n))
    write(*,*) ""
    write(*,'(3e10.5)') c(:,:), cc(:,:)
end program main