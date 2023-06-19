program main
    implicit none
    real(8), allocatable ::  x(:), y(:)
    real(8) r, c, x0, d
    integer m, n,i, fi
    x0 = -10.0d0
    n = 3
    m = 30
    c = 1.0
    d = 20.0d0 / dble(m-1)
    fi = 10
    open(fi, file='output.d')
    allocate(x(m), y(m))
    call random_seed
    call random_number(r)
    r = -1.0d0 + 2.0d0 * r
    do i = 1, m
        x(i) = x0 + (i-1)*d
        y(i) = 0.1d0 * x(i)**3 + 0.2d0 * x(i)**2 + 0.5d0 * x(i) + 1.0d0 + r
        write(fi, '(2e12.4)') x(i), y(i)
    enddo
    close(fi)
end program main