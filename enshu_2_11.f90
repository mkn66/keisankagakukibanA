program main
    implicit none
    real(8), allocatable :: r(:)
    integer, allocatable :: nn(:)
    integer n
    write(*,*) 'input n : '
    read(*,*) n
    if(n <1) stop "stop n < 1"
    allocate(r(n))
    allocate(nn(n))
    call random_seed
    call random_number(r(1:n))
    nn(:) = int(r(:)*10.0d0)
    write(*,*) nn
end program main