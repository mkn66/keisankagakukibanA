program main
    implicit none
    real(8) x1, x2, a, er, er0
    integer k,  km, i
    er0=1.0d0-6
    km = 100

    write(*,*) 'input a and k  (2 <= k):'
    read(*,*) a, k
    
    x1 = a
    do i = 1, km
        x2 = x1 - (x1**k-dble(a))/(dble(k)*x1**(k-1))
        er = abs(x2 - x1)
        if (er < er0) exit
        x1 = x2
    enddo
    write(*,*) 'kai=', x2, 'er=', er 
end program main