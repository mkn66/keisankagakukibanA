program main
    implicit none
    integer i, sum, summ, step
    step = 50
    summ =0
    sum=0
    do i=1, step
        sum = sum + i
    enddo
    summ = step * (step + 1) / 2
    write(*, *)   sum, summ

    sum=0
    do i=1, step
        sum = sum + i**2
    enddo
    summ = step * (step + 1) * (2 * step + 1) / 6
    write(*, *)  sum, summ

    sum=0
    do i=1, step
        sum = sum + i**3
    enddo
    summ = step ** 2 * (step + 1) **2 /4
    write(*, *)  sum, summ
end program main