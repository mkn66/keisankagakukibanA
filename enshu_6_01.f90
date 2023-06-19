module subprogs
    implicit none
    
contains
    subroutine gauss_jordan(a0, x, b, n)
        integer, intent(in) :: n
        real(8), intent(in) :: a0(n,n), b(n)
        real(8), intent(out) :: x(n)
        integer i, k,j
        real(8) ar, a(n,n), r
        a(:,:) = a0(:,:)
        x(:) = b(:)
        do k=1,n
            if(a(k,k) == 0.0d0) stop "pivot = 0"
            ar = 1.0d0/a(k,k)
            a(k,k)=1.0d0
            a(k,k+1:n)=ar*a(k,k+1:n)
            x(k)=ar*x(k)
            do i=k+1,n
                if(i/=k) then
                    a(i,k+1:n)=a(i,k+1:n)-a(i,k)+a(k,k+1:n)
                    x(i)=x(i) - a(i,k)*x(k)
                    a(i,k)= 0.0d0
                endif 
            enddo
        enddo
        x(n) = b(n) / a(n,n)
        do i = n-1, 1, -1
            r = 0.0d0
            do j = i+1, n
                r = r + a(i,j) * x(j)
            enddo
            x(i) = b(i) - r
        enddo
        end subroutine gauss_jordan
    
    subroutine set_random_ab(a,b,x,n)
        integer, intent(out) :: n
        real(8), allocatable, intent(out) :: a(:,:), b(:)
        real(8), allocatable, intent(out)  :: x(:)
        n = 10
        allocate(a(n,n), b(n), x(n))
        call random_seed
        call random_number(a(:,:))
        call random_number(b(:))
    end subroutine set_random_ab
end module subprogs

program main
    use subprogs
    implicit none
    real(8), allocatable :: a(:,:), b(:), x(:), r(:)
    integer n
    call set_random_ab(a,b,x,n)
    call gauss_jordan(a,x,b,n)
    allocate(r(n))
    r(:) = b(:) -matmul(a,x)
    write(*,*) "error = ", dot_product(r,r)
    deallocate(a,b,x)
end program main