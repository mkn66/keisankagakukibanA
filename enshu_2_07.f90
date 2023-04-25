program main
    implicit none
    integer i
    real(8) :: u(1:3)=(/1.0,2.0,3.0/), v(1:3) = (/4.0,5.0,6.0/), gaiseki(1:3), u1(3),u2(3),v1(3),v2(3)
    ! write(*,*) cshift(u, 1)
    ! write(*,*) cshift(v, 2)
    u1=cshift(u,1)
    u2=cshift(u,2)
    v1=cshift(v,1)
    v2=cshift(v,2)
    do i=1,3
        gaiseki(i) = u1(i)*v2(i) - u2(i)*v1(i)
    enddo
    write(*,*) gaiseki(:)

end program main