program main
    implicit none
    real(8) s, er,x, e
    integer n, kai
    x = 1.0d0
    s = 1.0d0
    e = exp(1.0d0) 
    kai = 1
    do n = 1 , 10
        kai = kai * n
        s = s + x**n / dble(kai)
        er = abs(s-e)
        write(*,*) 'n=', n, 'e=', s, 'er=', er
    enddo
end program main