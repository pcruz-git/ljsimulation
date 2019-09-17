real(8) function random(nv) result(res)
    implicit none
    rea(8), intent(in):: nv
    


program pi_approximation
    implicit none
    real(8), dimension(1):: x, y, P, pi! coordinates,"P"?, and pi
    integer(8):: i, counter_in, counter_tot ! iteration and # of points
    integer, dimension(1):: seed ! for the random number generator 
    !if we want to let the user determine the number of points, uncomment the following lines
    !print*, 'Enter the maximum number of points to consider'
    !read*, counter_tot
    counter_tot = 1000000 ! Total number of points to consider, (uncomment this if the user gives the number)
    seed(1) = 12345
    counter_in = 0
    call random_seed
    do i=1, counter_tot
        call random_number(P)
        x(1) = P(1)
        call random_number(P)
        y(1) = P(1)
        if (x(1)**2 + y(1)**2 <= 1) counter_in = counter_in + 1 
        ! condition for a point inside the circle
    end do
    pi = 4*(counter_in/(1.0*counter_tot))
    print*, "The approximate value of pi is", pi
end program pi_approximation
