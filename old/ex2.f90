program fibonacci
    implicit none ! assume no data types
    integer(kind=8):: x,y,z ! let x = x_n, y = x_(n-1), z = x_(n-2) *precision
    integer:: n=3 ! start with n = 3
    real:: phi_est ! golden ratio
    z = 1 ! initial numbers
    y = 1
2   x = y + z ! start of "loop"
    phi_est = x/(1.0*y)
    print*, x, phi_est
    z = y
    y = x
    n = n + 1 ! counter
    if (n<50) go to 2
end program fibonacci
