program main
    implicit none
    real(8) :: a(3) = (/1.0,2.0,3.0/), b(3)=(/4.0,5.0,6.0/), m, ms, gaiseki(3), a1(3), a2(3),b1(3),b2(3),s, aa, bb, cc 
    integer i
    aa=sqrt(dot_product(a,a))
    bb=sqrt(dot_product(b,b))
    cc=sqrt(dot_product(b-a, b-a))
    s=(aa+bb+cc)/2.0d0
    a1=cshift(a,1)
    a2=cshift(a,2)
    b1=cshift(b,1)
    b2=cshift(b,2)
    do i=1,3
        gaiseki(i) = a1(i)*b2(i) - a2(i)*b1(i)
    enddo
    m = sqrt(dot_product(gaiseki,gaiseki)) / 2.0d0
    ms = sqrt(s*(s-aa)*(s-bb)*(s-cc))
    write(*,'(e10.5)') ms, m

end program main