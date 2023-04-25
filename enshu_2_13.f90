program main
    implicit none
    real(8), allocatable :: r(:, :)
    integer n, i, j
    n = 5
    if(n <1) stop "stop n < 1"
    allocate(r(n, n))
    call random_seed
    call random_number(r(1:n,1:n))
    do i=1,n
        do j=i+1, n
            r(i,j) = r(j, i)
        enddo
    enddo
    write(*,'(5e10.5)') r
end program main