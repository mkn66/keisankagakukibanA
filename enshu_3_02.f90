module add
    implicit none
    
contains
    subroutine wa(i, j ,k)
        integer i , j, k
        k = i + j
    end subroutine wa
end module add



program main
    use add
    implicit none
    integer :: i = 1, j = 2, k
    call wa(i, j, k)
    write(*,*) k
end program main

