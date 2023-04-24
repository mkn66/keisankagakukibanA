program main
    implicit none
    integer an, a1, a2, i
    write(*, *)  1
    write(*, *)  2
    a1 = 1
    a2 = 2
    do i = 3, 10
        an = a1 + a2
        a1 = a2
        a2 = an
        write(*, *)  an
    enddo

end program main