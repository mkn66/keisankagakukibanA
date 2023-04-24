program main
    implicit none
    integer n, r, i, nn, nr, rr
    nn = 1
    rr = 1
    nr = 1
    do
        write(*,*) 'input n and r (0 <= r <= n <= 10, n is natural number)'
        read(*,*) n, r
        if (n > 10 .or. n < r .or. r < 0 .or. n==0) cycle 
    enddo
    if (r == 0) then
        write(*,*) 'nPr = ', 1, 'nCr = ', 1
        stop
    endif
    do i = 1, n
        nn = nn * i
    enddo
    do i = 1, r
        rr = rr * i
    enddo
    do i = 1, n-r
        nr = nr * i
    enddo
    write(*,*) 'nPr = ', nn/nr, 'nCr = ', nn/rr/nr

end program main