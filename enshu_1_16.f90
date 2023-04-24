program main
    implicit none
    real(8) :: L1, L2, er, er0=1.0d0-6, g = 9.8d0, T=5.0d0, h=2.5d0, pi
    integer :: k, km=100
    pi = acos(-1.0)
    L1 = h
    do k = 1, km
        L2 = T*sqrt(g * L1 * tanh(2.0d0 * pi * h / L1) / (2.0d0 * pi))
        er = abs(L1 - L2)
        if (er < er0) exit
        L1 = L2
    enddo
    write(*,*) 'L=', L2, 'er=', er
end program main