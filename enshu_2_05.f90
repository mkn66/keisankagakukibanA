program main
    implicit none
    real :: u(1:3) = (/1.0,2.0,3.0/),l
    l = sqrt(dot_product(u(1:3), u(1:3)))
    if(l == 0.0d0) stop "Lengh is 0"
    write(*,*) u(1:3) / l
end program main