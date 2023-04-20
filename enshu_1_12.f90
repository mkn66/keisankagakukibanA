program main
    implicit none
    integer i, step
    real(8) an, sum, r,a1, summ
    step = 10
    sum= 0.0d0
    r=0.8d0
    a1=16.0d0
    do i = 1, step
        an = a1*r**(i-1)
        sum = sum+an
        write(*,*)'n=',i,'an=',an
    enddo
    summ = a1*(1.0-r**step) / (1.0 - r)

    write(*,*) sum, summ
end program main