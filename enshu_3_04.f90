module add_1
    implicit none
    
contains
    subroutine add
        integer, save :: ic = 1
        integer f
        open(f, file='./output.d')
        if (ic >  10) then 
            close(f)
            stop
        else
            write(f,*) ic
            close(f)
        endif
        ic = ic +1
        end subroutine add 
end module add_1

program main
    use add_1
    implicit none
    integer :: i
    do i =1, 12
        call add
    enddo
end program main