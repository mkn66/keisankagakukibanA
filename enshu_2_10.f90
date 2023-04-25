program main
    implicit none
    real(8), allocatable :: r(:)
    real(8) :: av, s
    integer n
    write(*,*) 'input n : '
    read(*,*) n
    if(n <1) stop "stop n < 1"
    allocate(r(n))
    call random_seed
    call random_number(r(1:n))
    r(:)=2.0d0 * r(:) - 1.0d0
    av=sum(r) / dble(n)
    s=sqrt(sum((r(:)-av)**2)/dble(n))
    write(*,*) 'average', av, 'standard deviation', s
end program main