program main
    implicit none
    real(8) , allocatable :: a(:,:), b(:,:)
    real(8) y
    integer i, j
    write(*,*) 'input i and j : '
    read(*,*) i,j
    allocate(a(1:3, 1:3), b(1:2, 1:2))
    call random_seed
    call random_number(a(1:3, 1:3))
    call random_number(b(1:2, 1:2))

    b(1:i-1, 1:j-1) = a(1:i-1,1:j-1)
    b(1:i-1, j:2) = a(1:i-1, j+1:3)
    b(i:2, 1:j-1) = a(i+1:3, 1:j-1)
    b(i:2, j:2) = a(i+1:3, j+1:3)
    y = (-1.0d0)**(i+j)*(b(1,1)*b(2,2) - b(1,2)*(2,1)) !警告が出た -1の累乗はcomplex
    write(*,*) y

end program main