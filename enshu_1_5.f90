program main
    implicit none
    integer wa, n, i, m
    do 
        write(*,*) 'input interger n and m (m <= n)'
        read(*,*) n, m
        if (m > n) then
            write(*,*) 'not m <= n'
            cycle
        else
            exit
        endif
    enddo
    wa = 0
    do i = m, n
        wa = wa + i
    enddo
    write(*,*) wa

end program main