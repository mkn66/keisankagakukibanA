program main
    implicit none
    real(8) dx, x, y, s, pi
    integer i, n, a, b
    pi = acos(-1.0)
    write(*,*) 'input n a b (a<b):'
    read(*,*) n, a, b
    if(n<1) stop 'stop, n<1'
    dx = dble(b-a) / dble(n)
    s = 0.0d0
    do i = 0, n
        x = dx*dble(i)
        y = exp(-x*x/2.0d0) / sqrt(2.0d0*pi)
        if (i == 0 .or. i == n) then
            s = s+0.5d0*y
        else 
            s = s+ y
        endif
    enddo
    s = s * dx
    write(*,*) s
end program main