program main
    implicit none
    integer :: i, step, fo=10
    real(8) an, r,a1
    open(fo, file='output_1_18.d')
    step = 10
    r=0.8d0
    a1=16.0d0
    do i = 1, step
        an = a1*r**(i-1)
        write(fo, *) i, an
    enddo
    close(fo)
end program main