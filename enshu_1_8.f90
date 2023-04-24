program main
    implicit none
    integer n, i

    
    write(*,*) 'input natural number less than 1 milliion'
    read(*,*) n
    if(n <= 0 .or. n > 1000000) stop 'please input natural number less than 1 million'
    do i = 2, int(sqrt(dble(n)))
        if (mod(n, i) == 0) then 
            write(*,*) 'not prime number'
            stop
        endif
    enddo
    write(*,*) 'prime number'
end program main
