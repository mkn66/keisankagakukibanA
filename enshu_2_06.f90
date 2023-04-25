program main
    implicit none
    real(8) :: u(1:3) = (/1.0,2.0,3.0/), c(1:3), sum
    c(1:3) = u(1:3) / sqrt(dot_product(u, u))
    sum = dot_product(c,c)
    write(*,*) '方向余弦', c(1:3), 'sum', sum

end program main