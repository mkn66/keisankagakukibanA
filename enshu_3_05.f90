module add
    implicit none
    
contains
    subroutine wa(i, j ,k)
        integer, intent(in) :: i, j
        integer, intent(out) ::k
        k = i + j
    end subroutine wa
end module add

module subprog
    implicit none
    
contains
    subroutine swap(a,b)
        integer, intent(inout) :: a, b
        integer tmp
        tmp = a
        a = b
        b = tmp
    end subroutine swap 
end module subprog

program main
    use add
    use subprog
    implicit none
    integer :: a = 1, b = 2, k
    call swap(a, b)
    call wa(a, b, k)
    write(*,*) a, b, k
    
end program main