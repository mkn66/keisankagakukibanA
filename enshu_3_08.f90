module sequence
    implicit none
    
contains
    function tohi_sequence(a, r, n) result(wa)
        real(8), intent(in) :: a, r
        integer, intent(in) :: n
        real(8) wa
        wa = a*(1.0-r**n) / (1.0 - r)
    end function tohi_sequence
end module sequence

program main
    use sequence
    implicit none
    real(8) :: a = 2.0d0,  r = 3.0d0
    integer :: n = 10
    write(*,*) tohi_sequence(a, r, n)
end program main