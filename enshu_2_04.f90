program main
    implicit none
    real(8) :: naiseki, dot, u(1:3) = (/1.0, 2.0, 3.0/)
    naiseki = sqrt(u(1)**2 + u(2)**2 + u(3)**2)
    dot = sqrt(dot_product(u(1:3), u(1:3)))
    write(*,*) naiseki, dot
end program main