module subprogs
    implicit none
    
contains
    subroutine enshu_9(x, y, m)
        integer, intent(in) :: m
        real(8), intent(out) :: x(m), y(m)
        real(8) r, c, x0, d
        integer  i, fo
        x0 = -10.0d0
        ! n = 3
        ! m = 30
        c = 1.0
        d = 20.0d0 / dble(m-1)
        fo = 10
        open(fo, file='output.d')
        ! allocate(x(m), y(m))
        call random_seed
        call random_number(r)
        r = -1.0d0 + 2.0d0 * r
        do i = 1, m
            x(i) = x0 + (i-1)*d
            y(i) = 0.1d0 * x(i)**3 + 0.2d0 * x(i)**2 + 0.5d0 * x(i) + 1.0d0 + r
            write(fo, '(2e12.4)') x(i), y(i)
        enddo
        close(fo)
    end subroutine

    subroutine set_c_b(x,y,b,k,m,c)
        integer, intent(in) :: m, k
        real(8), intent(in) :: x(m), y(m)
        real(8), intent(out) :: c(k+1,k+1), b(k+1)
        integer i, j, n
        real(8) w
        do i = 1, k+1 !行
            do j = 1, k+1 !列
                w = 0.0d0
                do n = 1, m !シグマ
                    w = w + x(n) ** (i-1+j-1)
                enddo
                c(i,j) = w
            enddo
        enddo
        do i = 1, k+1
            w = 0.0d0
            do n = 1, m
                w = w + y(n) * x(n) ** (i-1)
            enddo
            b(i) = w
        enddo

    end subroutine set_c_b

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
end module subprogs

program main
    use subprogs
    implicit none
    integer  m, k
    real(8), allocatable ::  x(:), y(:), c(:,:), b(:), a(:)
    m = 30
    k = 3
    allocate(x(m), y(m), c(k+1, k+1), b(k+1), a(k+1))
    call enshu_9(x,y,m)
    call set_c_b(x,y,b,k,m,c)
    call gauss_jordan_pv(c,a,b,k+1)

    write(*,'(4e12.4)') a(:)
end program main


