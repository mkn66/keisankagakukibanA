program main
    implicit none
    integer wa, n, i, m, s

    write(*,*) 'input interger n and m'
    read(*,*) n, m
    if (n > m) then 
        s = n
        n = m 
        m = s
    endif
    wa = 0
    do i = n, m
        wa = wa + i
    enddo
    write(*,*) wa

end program main