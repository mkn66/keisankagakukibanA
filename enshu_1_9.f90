program main
    implicit none
    integer n, m, l, k
    write(*,*) 'input natural numbers n and m'
    read(*,*) n, m
    if ( n < m) then
        l = n
        n = m
        m = l
    endif
    k = mod(n, m)
    do 
        if (k == 0) then 
            write(*,*) m
            exit
        endif
        n = m
        m = k
        k = mod(n, m)
    enddo
end program main