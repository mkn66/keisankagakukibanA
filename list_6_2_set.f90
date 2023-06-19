module subprogs
    implicit none
    
contains
    subroutine gauss_jordan_pv(a0, x, b, n)
        integer, intent(in) :: n
        real(8), intent(in) :: a0(n,n), b(n)
        real(8), intent(out) :: x(n)
        integer i,k,m
        real(8) ar, am, t, a(n,n), w(n)
        a(:,:) = a0(:,:)
        x(:) = b(:)
        do k = 1, n
            m = k
            am = abs(a(k,k))
            do i = k+1, n
                if(abs(a(i,k))  > am) then
                    am = abs(a(i,k))
                    m = i
                endif
            enddo
            if (am == 0.0d0) stop 'A is singular'
            if (k /= m) then
                w(k:n) = a(k, k:n)
                a(k, k:n) = a(m, k:n)
                a(m, k:n) = w(k:n)
                t = x(k)
                x(k) = x(m)
                x(m) = t
            end if
            ar = 1.0d0/a(k,k)
            a(k,k)=1.0d0
            a(k,k+1:n)=ar*a(k,k+1:n)
            x(k)=ar*x(k)
            do i=1,n
                if(i/=k) then
                    a(i,k+1:n)=a(i,k+1:n)-a(i,k)+a(k,k+1:n)
                    x(i)=x(i) -a(i,k)*x(k)
                    a(i,k)= 0.0d0
                endif 
            enddo
        enddo
    end subroutine gauss_jordan_pv

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