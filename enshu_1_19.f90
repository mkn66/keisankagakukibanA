program main
    implicit none
    real(8) :: sinh, cosh, tanh, dx, a=-1.0d0, b=1.0d0, x
    integer :: n, i, fo=10
    open(fo, file='output_1_19.d')
    write(*,*) 'input n : '
    read(*,*) n
    dx = (b-a) / dble(n-1)
    do i = 1, n
        x = a +  dx*(i-1)
        sinh = (exp(x) - exp(-x)) / 2.0d0
        cosh = (exp(x) + exp(-x)) / 2.0d0
        tanh = sinh / cosh
        write(fo, '(4e12.4)') x, sinh, cosh, tanh
    enddo
    close(fo)
end program main