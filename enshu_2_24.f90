program main
    implicit none
    real(8) a(1:3), b(1:3), c(1:2, 1:3), ab(1:3), det, bu(1:2, 1:2)
    integer i
    call random_seed
    call random_number(a(1:3))
    call random_number(b(1:3))
    c(1,1:3) = a(1:3)
    c(2,1:3) = b(1:3)

    
    do i = 1, 3
        c(1:2, 1:3) =  cshift(c,i-1, 2)

        bu(1:2, 1:2) = c(1:2, 2:3)

        det = bu(1,1)*bu(2,2) - bu(1,2)*bu(2,1)
        ab(i) = det
    enddo
    write(*, '(3e10.4)') ab(1:3)
        
end program main
