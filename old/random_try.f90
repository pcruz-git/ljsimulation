program random
    implicit none
    real(8), dimension(1):: x, P
    integer, dimension(1):: seed
    integer(8):: i
    seed(1) = 12345
    call random_seed
    do i=1,10
        call random_number(P)
        x = P
        print*, x
    end do
end program
