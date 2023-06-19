module subprogs
    implicit none
    
contains
    subroutine gauss_jordan(a0, x, b, n)
        integer, intent(in) :: n
        real(8), intent(in) :: a0(n,n), b(n, n)
        real(8), intent(out) :: x(n, n)
        integer i, k
        real(8) ar, a(n,n)
        a(:,:) = a0(:,:)
        x(:, :) = b(:, :)
        do k = 1, n
            if(a(k,k) == 0.0d0) stop "pivot = 0"
            ar = 1.0d0/a(k,k)
            a(k,k)=1.0d0
            a(k,k+1:n)=ar*a(k,k+1:n)
            x(k,:)=ar*x(k, :)
            do i=1,n
                if(i/=k) then
                    a(i,k+1:n)=a(i,k+1:n)-a(i,k)*a(k,k+1:n)
                    x(i, :)=x(i, :) -a(i,k)*x(k, :)
                    a(i,k)= 0.0d0
                endif 
            enddo
        enddo
        
        end subroutine gauss_jordan
    
    subroutine set_random_ab(a,b,x,n)
        integer, intent(out) :: n
        real(8), allocatable, intent(out) :: a(:,:), b(:, :)
        real(8), allocatable, intent(out)  :: x(:, :)
        n = 3
        allocate(a(n,n), b(n, n), x(n,n))
        call random_seed
        call random_number(a(:,:))
        call random_number(b(:, :))
    end subroutine set_random_ab
end module subprogs

program main
    use subprogs
    implicit none
    real(8), allocatable :: a(:,:), b(:, :), x(:, :)
    integer n, i, j
    call set_random_ab(a,b,x,n)
    call gauss_jordan(a,x,b,n)
    do i=1,n
        do j=1, n
            write(*,*) x(i,j)
        enddo
        write(*,*) "\n"
    enddo
    deallocate(a,b,x)
end program main