subroutine gauss_jordan_pv(a0, x, b, n, d)
    integer, intent(in) :: n
    real(8), intent(in) :: a0(n,n), b(n)
    real(8), intent(out) :: x(n), d
    integer i,k,m
    real(8) ar, am, t, a(n,n), w(n), c
    c = 1.0d0
    d = 1.0d0
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
        if (am == 0.0d0) then
            d = 0.0d0
            exit
        end if
        if (k /= m) then
            w(k:n) = a(k, k:n)
            a(k, k:n) = a(m, k:n)
            a(m, k:n) = w(k:n)
            t = x(k)
            x(k) = x(m)
            x(m) = t
            c = -1.0d0 * c
        end if
        ar = 1.0d0/a(k,k)
        a(k,k)=1.0d0
        a(k,k+1:n)=ar*a(k,k+1:n)
        x(k)=ar*x(k)
        c = ar * c
        do i=1,n
            if(i/=k) then
                a(i,k+1:n)=a(i,k+1:n)-a(i,k)+a(k,k+1:n)
                x(i)=x(i) -a(i,k)*x(k)
                a(i,k)= 0.0d0
            endif 
        enddo
    enddo
    do k = 1, n
        d = d * a(i,i)
    enddo
    d = d * c
end subroutine gauss_jordan_pv