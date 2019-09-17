program computer_time_single
    implicit none
    real(8), dimension(:), allocatable  :: times
    real(8)                             :: t1, t2, t_mean, t_sd, time
    real                                :: x_real_sp
    real(8)                             :: x_real_dp
    integer                             :: i, x_int, reps=1000
    
    allocate(times(reps))
    !cpu time for allocation of variable x
    !when x is an integer (single precision
    do i=1, reps
        call cpu_time(t1)
        x_int = 12345
        call cpu_time(t2)
        times(i) = t2-t1
    end do
    t_mean = sum(times)/reps
    t_sd = sqrt((sum(times-t_mean)**2)/reps)
    write(*, '(i6, 3f12.8)') reps, t_mean, t_sd
    
    !when x is a real number (single precision)
    do i=1, reps
        call cpu_time(t1)
        x_real_sp = 12.3456_4
        call cpu_time(t2)
        times(i) = t2-t1
    end do
    t_mean = sum(times)/reps
    t_sd = sqrt((sum(times-t_mean)**2)/reps)
    write(*, '(i6, 3f12.8)') reps, t_mean, t_sd
    
    !when x is a real number (double precision)
    do i=1, reps
        call cpu_time(t1)
        x_real_dp = 12.3456_8
        call cpu_time(t2)
        times(i) = t2-t1
    end do
    t_mean = sum(times)/reps
    t_sd = sqrt((sum(times-t_mean)**2)/reps)
    write(*, '(i6, 3f12.8)') reps, t_mean, t_sd
    
    !cpu time for summation of variable (double precision)
    do i=1, reps
        call cpu_time(t1)
        x_real_dp = 12.3456_8 + 67.8910_8
        call cpu_time(t2)
        times(i) = t2-t1
    end do
    t_mean = sum(times)/reps
    t_sd = sqrt((sum(times-t_mean)**2)/reps)
    write(*, '(i6, 3f12.8)') reps, t_mean, t_sd
    
end program computer_time_single
