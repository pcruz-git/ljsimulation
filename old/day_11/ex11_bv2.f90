real(8) function time_reps(t1d,t2d,repsd) result(res)
    implicit none
    real(8), intent(in)                             :: t1d, t2d
    integer(8), intent(in)                          :: repsd
    res = (t2d-t1d) / real(repsd)
end function

program computer_time
    implicit none
    real(8)                                         :: t1, t2, time, t_mean, t_sd
    real(8), external                               :: time_reps
    real(8), dimension(:), allocatable              :: times
    integer                                         :: var_int, var_int_b, var_int_c 
    real(4)                                         :: var_sp, var_sp_b, var_sp_c 
    real(8)                                         :: var_dp, var_dp_b, var_dp_c 
    integer(8), dimension(:), allocatable           :: arr_int
    real, dimension(:), allocatable                 :: arr_sp, mat_sp
    real(8), dimension(:), allocatable              :: arr_dp, arr_dp_b, arr_dp_c
    real(8), dimension(:,:), allocatable            :: mat_dp_a, mat_dp_b, mat_dp_c
    real(8), dimension(:), allocatable              :: mat_dp, mat_dp_1, mat_dp_2
    real(8), dimension(:,:), allocatable            :: mat_arb, mat_arb_1, mat_arb_2
    integer(8)                                      :: i, j, k, n, reps
    integer                                         :: u
    
    !STORING ALL PRINTOUTS INTO A FILE time_data.tmp
    open(unit=u, file='time_data.tmp', status='replace', action='write')
    !--------------------------------------------------------------------------------------------------!
    !TIMING ASSIGNMENT #1, SINGLE NUMBERS
    
    !setting repetitions to 1E6
    reps = 1e6
   
   !ASSIGNMENT OF VALUES-------------------------------------------------------------------------------!
   !integers
    call cpu_time(t1)
    do n=1, reps
        var_int = 12345
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps) * 1e9
    write(*, '(i10, f12.6, a100)') reps, time, ">Single value, integer assignment. {reps, time}"
    
    !single precision
    call cpu_time(t1)
    do n=1, reps
        var_sp = 12.345_4
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps) * 1e9
    write(*, '(i10, f12.6, a100)') reps, time, ">Single value, float (single precision) assignment. {reps, time}"
    
    !double precision
    call cpu_time(t1)
    do n=1, reps
        var_dp = 12.345_8
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps) * 1e9
    write(*, '(i10, f12.6, a100)') reps, time, ">Single value, float (double precision) assignment. {reps, time}"   
    
    !READOUT AND ASSIGNMENT----------------------------------------------------------------------------!
    !integer
    var_int_b = 12345
    call cpu_time(t1)
    do n=1, reps
        var_int = var_int_b
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps) * 1e9
    write(*, '(i10, f12.6, a100)') reps, time, ">Single value, integer readout and assignment. {reps, time}"    
    
    !single precision
    var_sp_b = 12.345_4
    call cpu_time(t1)
    do n=1, reps
        var_sp = var_sp_b
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps) * 1e9
    write(*, '(i10, f12.6, a100)') reps, time, ">Single value, float (single precision) readout and assignment. {reps, time}"    
    
    !double precision
    var_dp_b = 12.345_8
    call cpu_time(t1)
    do n=1, reps
        var_dp = var_dp_b
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps) * 1e9
    write(*, '(i10, f12.6, a100)') reps, time, ">Single value, float (double precision) readout and assignment. {reps, time}"     
    
    !ADDITION------------------------------------------------------------------------------------------!
    !integer
    var_int_b = 12345
    var_int_c = 56789
    call cpu_time(t1)
    do n=1, reps
        var_int = var_int_b + var_int_c
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps) * 1e9
    write(*, '(i10, f12.6, a100)') reps, time, ">Single value, integer addition. {reps, time}"
    
    !single precision
    var_sp_b = 12.345_4
    var_sp_c = 56.789_4
    call cpu_time(t1)
    do n=1, reps
        var_sp = var_sp_b + var_sp_c
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps) * 1e9
    write(*, '(i10, f12.6, a100)') reps, time, ">Single value, float (single precision) addition. {reps, time}"
    
    !double precision
    var_dp_b = 12.345_4
    var_dp_c = 56.789_4
    call cpu_time(t1)
    do n=1, reps
        var_dp = var_dp_b + var_dp_c
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps) * 1e9
    write(*, '(i10, f12.6, a100)') reps, time, ">Single value, float (double precision) addition. {reps, time}"
    
    !MULTIPLICATION------------------------------------------------------------------------------------!
    !integer
    var_int_b = 12345
    var_int_c = 56789
    call cpu_time(t1)
    do n=1, reps
        var_int = var_int_b * var_int_c
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps) * 1e9
    write(*, '(i10, f12.6, a100)') reps, time, ">Single value, integer multiplication. {reps, time}"
    
    !single precision
    var_sp_b = 12.345_4
    var_sp_c = 56.789_4
    call cpu_time(t1)
    do n=1, reps
        var_sp = var_sp_b * var_sp_c
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps) * 1e9
    write(*, '(i10, f12.6, a100)') reps, time, ">Single value, float (single precision) multiplication. {reps, time}"
    
    !double precision
    var_dp_b = 12.345_4
    var_dp_c = 56.789_4
    call cpu_time(t1)
    do n=1, reps
        var_dp = var_dp_b * var_dp_c
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps) * 1e9
    write(*, '(i10, f12.6, a100)') reps, time, ">Single value, float (double precision) multiplication. {reps, time}"
    
    !--------------------------------------------------------------------------------------------------!
    !TIMING ASSIGNMENT #1, 1D AND 2D ARRAYS WITH ARRAY OPERATIONS--------------------------------------!
    !setting the dimensions of the arrays, and repetitions
    reps = 1e3
    n = 1e3
    allocate(arr_dp(n))
    allocate(arr_dp_b(n))
    allocate(arr_dp_c(n))
    allocate(mat_dp_a(n,n))
    allocate(mat_dp_b(n,n))
    allocate(mat_dp_c(n,n))
    
    !ASSIGNMENT----------------------------------------------------------------------------------------!
    !1D
    call cpu_time(t1)
    do i=1, reps
        arr_dp = 12.345_8
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n) * 1e9
    write(*, '(i10, i10, f12.6, a100)') reps, n, time, "1D array assignment. {reps, dim, time}"
    
    !2D
    call cpu_time(t1)
    do i=1, reps
        mat_dp_a = 12.345_8
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n*n) * 1e9
    write(*, '(i10, i10, f12.6, a100)') reps, n, time, "2D array assignment. {reps, dim, time}"
    
    !READOUT AND ASSIGNMENT----------------------------------------------------------------------------!
    !1D
    arr_dp_b = 12.345_8
    call cpu_time(t1)
    do i=1, reps
        arr_dp = arr_dp_b
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n) * 1e9
    write(*, '(i10, i10, f12.6, a100)') reps, n, time, "1D array readout and assignment. {reps, dim, time}"
    
    !2D
    mat_dp_b = 12.345_8
    call cpu_time(t1)
    do i=1, reps
        mat_dp_a = mat_dp_b
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n*n) * 1e9
    write(*, '(i10, i10, f12.6, a100)') reps, n, time, "2D array readout and assignment. {reps, dim, time}"
    
    !ADDITION------------------------------------------------------------------------------------------!
    !1D
    arr_dp_b = 12.345_8
    arr_dp_c = 56.789_8
    call cpu_time(t1)
    do i=1, reps
        arr_dp = arr_dp_b + arr_dp_c
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n) * 1e9
    write(*, '(i10, i10, f12.6, a100)') reps, n, time, "1D array addition. {reps, dim, time}" 
    
    !2D
    mat_dp_b = 12.345_8
    mat_dp_c = 56.789_8
    call cpu_time(t1)
    do i=1, reps
        mat_dp_a = mat_dp_b + mat_dp_c
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n*n) * 1e9
    write(*, '(i10, i10, f12.6, a100)') reps, n, time, "2D array addition. {reps, dim, time}"
    
    !MULTIPLICATION------------------------------------------------------------------------------------!
    !1D
    arr_dp_b = 12.345_8
    arr_dp_c = 56.789_8
    call cpu_time(t1)
    do i=1, reps
        arr_dp = arr_dp_b * arr_dp_c
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n) * 1e9
    write(*, '(i10, i10, f12.6, a100)') reps, n, time, "1D array multiplication. {reps, dim, time}"
    
    !2D
    mat_dp_b = 12.345_8
    mat_dp_c = 56.789_8
    call cpu_time(t1)
    do i=1, reps
        mat_dp_a = mat_dp_b * mat_dp_c
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n*n) * 1e9
    write(*, '(i10, i10, f12.6, a100)') reps, n, time, "2D array multiplication. {reps, dim, time}"
    
    
    !deallocating used matrices
    deallocate(arr_dp)
    deallocate(arr_dp_b)
    deallocate(arr_dp_c)
    deallocate(mat_dp_a)
    deallocate(mat_dp_b)
    deallocate(mat_dp_c)
    
    
    
    
    
    
    !--------------------------------------------------------------------------------------------------!
    !TIMING ASSIGNMENT #1, 1D and 2D ARRAYS WITH MANUAL ITERATION OVER ELEMENTS------------------------!
        
    !initializing the times matrix
    reps = 1e4
    allocate(times(reps))
    
    !ASSIGNMENT----------------------------------------------------------------------------------------!
    !1D INTEGER
    n=1e3
    allocate(arr_int(n))
    do i=1, reps
        call cpu_time(t1)
        do j=1, n
            arr_int(j) = 12345
        end do
        call cpu_time(t2)
        times(i) = t2-t1
    end do
    t_mean = sum(times)/real(reps) * 1e9
    t_sd = sqrt(((sum(times-t_mean))**2)/real(reps)) * 1e9
    write(*, '(i10, i10, f12.6, f12.6, a60)') reps, n, t_mean, t_sd, "1D array assignment integer."
    
    !cpu time for operations on a 1d array 
    !assigning single precision floats in an array
    n=1e5
    allocate(mat_sp(n))
    do i=1, reps
        call cpu_time(t1)
        do j=1, n
            mat_sp(j) = 12.345_4
        end do
        call cpu_time(t2)
        times(i) = t2-t1
    end do
    t_mean = sum(times)/real(reps) * 1e9
    t_sd = sqrt(((sum(times-t_mean))**2)/real(reps)) * 1e9
    write(*, '(i10, i10, f12.6, f12.6, a60)') reps, n, t_mean, t_sd, "1D array assignment (single precision) floats."
    
    !allocating double precision floats in an array
    n=1e5
    allocate(mat_dp(n))
    do i=1, reps
        call cpu_time(t1)
        do j=1, n
            mat_dp(j) = 12.3456_8
        end do
        call cpu_time(t2)
        times(i) = t2-t1
    end do
    t_mean = sum(times)/reps
    t_sd = sqrt((sum(times-t_mean)**2)/reps)
    write(*, '(i6, 2f12.6, a50)') n, t_mean, t_sd, "1d assignment real (double)"
    
    !performing matrix addition
    n=1e5
    allocate(mat_dp_1(n))
    do i=1, reps
        call cpu_time(t1)
        do j=1, n
            mat_dp_1(j) = 12.3456_8 - 65.4321_8
        end do
        call cpu_time(t2)
        times(i) = t2-t1
    end do
    t_mean = sum(times)/size(times)
    t_sd = sqrt((sum(times-t_mean)**2)/size(times))
    write(*, '(i6, 2f12.6, a50)') n, t_mean, t_sd, "1d array summation"
    
    !deallocating used matrices
    deallocate(mat_dp_1)
    deallocate(mat_dp)
    
    !performing 1d array multiplication
    n=1e5
    allocate(mat_dp(n))
    allocate(mat_dp_1(n))
    allocate(mat_dp_2(n))
    call random_number(mat_dp_1)
    call random_number(mat_dp_2)
    do i=1, reps
        call cpu_time(t1)
        mat_dp = mat_dp_1 * mat_dp_2
        call cpu_time(t2)
        times(i) = t2 - t1
    end do
    t_mean = sum(times)/size(times)
    t_sd = sqrt((sum(times-t_mean)**2)/size(times))
    write(*, '(i6, 2f12.6, a50)') n, t_mean, t_sd, "1d array multiplication"
    
    !deallocating used matrices
    deallocate(mat_dp_1)
    deallocate(mat_dp_2)
    deallocate(mat_dp)
    
    !performing matrix multiplication (columns first before rows)
    n=500
    allocate(mat_arb_1(n,n))
    allocate(mat_arb_2(n,n))
    allocate(mat_arb(n,n))
    
    call random_number(mat_arb_1)
    call random_number(mat_arb_2)
    do i=1, reps
        call cpu_time(t1)
        do j=1, n
            do k=1, n
                mat_arb(j,k) = mat_arb_1(j,k) * mat_arb_2(j,k)
            end do
        end do
        call cpu_time(t2)
        times(i) = t2-t1
    end do
    t_mean = sum(times)/size(times)
    t_sd = sqrt((sum(times-t_mean)**2)/size(times))
    write(*, '(i6, 2f12.6, a50)') n, t_mean, t_sd, "NxN array multiplication (column->row)"
    
    !performing matrix multiplication (rows first before columns)
    do i=1, reps
        call cpu_time(t1)
        do k=1, n
            do j=1, n
                mat_arb(j,k) = mat_arb_1(j,k) * mat_arb_2(j,k)
            end do
        end do
        call cpu_time(t2)
        times(i) = t2-t1
    end do
    t_mean = sum(times)/size(times)
    t_sd = sqrt((sum(times-t_mean)**2)/size(times))
    write(*, '(i6, 2f12.6, a50)') n, t_mean, t_sd, "NxN array multiplication (row->column)"
    
    !performing matrix multiplication (built-in)
    do i=1, reps
        call cpu_time(t1)
        mat_arb = mat_arb_1*mat_arb_2
        call cpu_time(t2)
        times(i) = t2-t1
    end do
    t_mean = sum(times)/size(times)
    t_sd = sqrt(sum(times-t_mean)**2/size(times))
    write(*, '(i6, 2f12.6, a50)') n, t_mean, t_sd, "NxN array multiplication (built-in)"
    
    !deallocating used matrices
    deallocate(mat_arb)
    deallocate(mat_arb_1)
    deallocate(mat_arb_2)
    !xkcd
end program computer_time
