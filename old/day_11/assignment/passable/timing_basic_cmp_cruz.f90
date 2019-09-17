real(8) function time_reps(t1d,t2d,repsd) result(res)
    implicit none
    real(8), intent(in)                             :: t1d, t2d
    integer(8), intent(in)                          :: repsd
    res = (t2d-t1d) / real(repsd)
end function

program computer_time
    implicit none
    real(8)                                         :: t1, t2, time
    real(8), external                               :: time_reps
    integer                                         :: var_int, var_int_b, var_int_c 
    real(4)                                         :: var_sp, var_sp_b, var_sp_c 
    real(8)                                         :: var_dp, var_dp_b, var_dp_c 
    integer(8), dimension(:), allocatable           :: arr_int
    real, dimension(:), allocatable                 :: arr_sp
    real(8), dimension(:), allocatable              :: arr_dp, arr_dp_b, arr_dp_c
    real(8), dimension(:,:), allocatable            :: mat_dp, mat_dp_b, mat_dp_c
    integer(8)                                      :: i, j, k, n, reps
    

    !--------------------------------------------------------------------------------------------------!
    !TIMING ASSIGNMENT #1, SINGLE NUMBERS--------------------------------------------------------------!
    !--------------------------------------------------------------------------------------------------!
    !Setting repetitions to 1E6
    reps = 1e6
   
   !Header
   print*, 'Timing of simple operations for single numbers'
   print*, 'Number of times the operation is performed : ', reps
   
   !---------------------------------------------------------------------------------------------------!
   !INTEGERS-------------------------------------------------------------------------------------------!
   print*, ''
   print*, 'For integers'
   !ASSIGNMENT OF VALUES-------------------------------------------------------------------------------!
    call cpu_time(t1)
    do n=1, reps
        var_int = 12345
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps) * 1e9
    write(*, '(a20,f12.6,a2)') "(Assignment) : ", time, 'ns'
   
   !READOUT AND ASSIGNMENT-----------------------------------------------------------------------------!
    var_int_b = 12345
    call cpu_time(t1)
    do n=1, reps
        var_int = var_int_b
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps) * 1e9
    write(*, '(a20,f12.6,a2)') "(Readout) : ", time, 'ns'
    
    !ADDITION------------------------------------------------------------------------------------------! 
    var_int_b = 12345
    var_int_c = 56789
    call cpu_time(t1)
    do n=1, reps
        var_int = var_int_b + var_int_c
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps) * 1e9
    write(*, '(a20,f12.6,a2)') '(Addition) : ', time, 'ns'
    
    !MULTIPLICATION------------------------------------------------------------------------------------!
    var_int_b = 12345
    var_int_c = 56789
    call cpu_time(t1)
    do n=1, reps
        var_int = var_int_b * var_int_c
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps) * 1e9
    write(*, '(a20,f12.6,a2)') '(Multiplication) : ', time, 'ns'
    
    !--------------------------------------------------------------------------------------------------!
    !SINGLE PRECISION FLOAT----------------------------------------------------------------------------!
    print*, ''
    print*, 'For single precision floats'
    !ASSIGNMENT OF VALUES------------------------------------------------------------------------------!
    call cpu_time(t1)
    do n=1, reps
        var_sp = 12.345_4
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps) * 1e9
    write(*, '(a20,f12.6,a2)') "(Assignment) : ", time, 'ns'
    
    !READOUT AND ASSIGNMENT----------------------------------------------------------------------------!
    var_sp_b = 12.345_4
    call cpu_time(t1)
    do n=1, reps
        var_sp = var_sp_b
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps) * 1e9
    write(*, '(a20,f12.6,a2)') "(Readout) : ", time, 'ns'
    
    !ADDITION------------------------------------------------------------------------------------------!
    var_sp_b = 12.345_4
    var_sp_c = 56.789_4
    call cpu_time(t1)
    do n=1, reps
        var_sp = var_sp_b + var_sp_c
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps) * 1e9
    write(*, '(a20,f12.6,a2)') '(Addition) : ', time, 'ns'
    
    !MULTIPLICATION------------------------------------------------------------------------------------!
    var_sp_b = 12.345_4
    var_sp_c = 56.789_4
    call cpu_time(t1)
    do n=1, reps
        var_sp = var_sp_b * var_sp_c
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps) * 1e9
    write(*, '(a20,f12.6,a2)') '(Multiplication) : ', time, 'ns'
    
    !--------------------------------------------------------------------------------------------------!
    !DOUBLE PRECISION FLOAT----------------------------------------------------------------------------!
    print*, ''
    print*, 'For double precision floats'
    !ASSIGNMENT OF VALUES------------------------------------------------------------------------------!
    call cpu_time(t1)
    do n=1, reps
        var_dp = 12.345_8
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps) * 1e9
    write(*, '(a20,f12.6,a2)') "(Assignment) : ", time, 'ns'
    
    !READOUT AND ASSIGNMENT----------------------------------------------------------------------------! 
    var_dp_b = 12.345_8
    call cpu_time(t1)
    do n=1, reps
        var_dp = var_dp_b
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps) * 1e9
    write(*, '(a20,f12.6,a2)') "(Readout) : ", time, 'ns' 
    
    !ADDITION------------------------------------------------------------------------------------------!
    var_dp_b = 12.345_8
    var_dp_c = 56.789_8
    call cpu_time(t1)
    do n=1, reps
        var_dp = var_dp_b + var_dp_c
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps) * 1e9
    write(*, '(a20,f12.6,a2)') '(Addition) : ', time, 'ns'
    
    !MULTIPLICATION------------------------------------------------------------------------------------!
    var_dp_b = 12.345_4
    var_dp_c = 56.789_4
    call cpu_time(t1)
    do n=1, reps
        var_dp = var_dp_b * var_dp_c
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps) * 1e9
    write(*, '(a20,f12.6,a2)') '(Multiplication) : ', time, 'ns'
    
    
    !--------------------------------------------------------------------------------------------------!
    !TIMING ASSIGNMENT #1, 1D AND 2D ARRAYS WITH ARRAY OPERATIONS--------------------------------------!
    !--------------------------------------------------------------------------------------------------!
    !Setting the dimensions of the arrays, and repetitions
    reps = 1e3
    n = 1e3
    allocate(arr_dp(n))
    allocate(arr_dp_b(n))
    allocate(arr_dp_c(n))
    allocate(mat_dp(n,n))
    allocate(mat_dp_b(n,n))
    allocate(mat_dp_c(n,n))
    
    !Header
    print*, ''
    print*, 'Timing of simple operations for 1D and 2D arrays using array operations'
    print*, 'Data type: Double precision floats'
    print*, 'Dimension of array: n = ', n
    print*, 'Number of times the operations is performed : ', reps
    
    !--------------------------------------------------------------------------------------------------!
    !1D ARRAY------------------------------------------------------------------------------------------!
    print*, ''
    print*, 'For 1D arrays'
    !ASSIGNMENT----------------------------------------------------------------------------------------!
    call cpu_time(t1)
    do i=1, reps
        arr_dp = 12.345_8
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n) * 1e9
    write(*, '(a20,f12.6,a2)') '(Assignment) : ', time, 'ns'
    
    !READOUT AND ASSIGNMENT----------------------------------------------------------------------------!
    arr_dp_b = 12.345_8
    call cpu_time(t1)
    do i=1, reps
        arr_dp = arr_dp_b
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n) * 1e9
    write(*, '(a20,f12.6,a2)') '(Readout) : ', time, 'ns'
    
    !ADDITION------------------------------------------------------------------------------------------!
    arr_dp_b = 12.345_8
    arr_dp_c = 56.789_8
    call cpu_time(t1)
    do i=1, reps
        arr_dp = arr_dp_b + arr_dp_c
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n) * 1e9
    write(*, '(a20,f12.6,a2)') '(Addition) : ', time, 'ns'
    
    !MULTIPLICATION------------------------------------------------------------------------------------!
    arr_dp_b = 12.345_8
    arr_dp_c = 56.789_8
    call cpu_time(t1)
    do i=1, reps
        arr_dp = arr_dp_b * arr_dp_c
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n) * 1e9
    write(*, '(a20,f12.6,a2)') '(Multiplication) : ', time, 'ns'

    !--------------------------------------------------------------------------------------------------!
    !2D ARRAY------------------------------------------------------------------------------------------!
    print*, ''
    print*, 'For 2D arrays'
    !ASSIGNMENT----------------------------------------------------------------------------------------!
    call cpu_time(t1)
    do i=1, reps
        mat_dp = 12.345_8
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n*n) * 1e9
    write(*, '(a20,f12.6,a2)') '(Assignment) : ', time, 'ns'
    
    !READOUT AND ASSIGNMENT----------------------------------------------------------------------------!
    mat_dp_b = 12.345_8
    call cpu_time(t1)
    do i=1, reps
        mat_dp = mat_dp_b
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n*n) * 1e9
    write(*, '(a20,f12.6,a2)') '(Readout) : ', time, 'ns'
    
    !ADDITION------------------------------------------------------------------------------------------!
    mat_dp_b = 12.345_8
    mat_dp_c = 56.789_8
    call cpu_time(t1)
    do i=1, reps
        mat_dp = mat_dp_b + mat_dp_c
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n*n) * 1e9
    write(*, '(a20,f12.6,a2)') '(Addition) : ', time, 'ns'
    
    !MULTIPLICATION------------------------------------------------------------------------------------!
    mat_dp_b = 12.345_8
    mat_dp_c = 56.789_8
    call cpu_time(t1)
    do i=1, reps
        mat_dp = mat_dp_b * mat_dp_c
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n*n) * 1e9
    write(*, '(a20,f12.6,a2)') '(Multiplication) : ', time, 'ns'
    
    !deallocating used matrices
    deallocate(arr_dp)
    deallocate(arr_dp_b)
    deallocate(arr_dp_c)
    deallocate(mat_dp)
    deallocate(mat_dp_b)
    deallocate(mat_dp_c)

    !--------------------------------------------------------------------------------------------------!
    !TIMING ASSIGNMENT #1, 1D and 2D ARRAYS WITH MANUAL ITERATION OVER ELEMENTS------------------------!
    !--------------------------------------------------------------------------------------------------!
        
    !setting number of repetitions
    reps = 1e3

    
    !allocating matrices
    n=1e3
    allocate(arr_dp(n))
    allocate(arr_dp_b(n))
    allocate(arr_dp_c(n))
    allocate(mat_dp(n,n))
    allocate(mat_dp_b(n,n))
    allocate(mat_dp_c(n,n))
    
    !Header
    print*, ''
    print*, 'Timing of simple operations for 1D and 2D arrays with manual iteration over elements'
    print*, 'Data type: Double precision floats'
    print*, 'Dimension of array: n = ', n
    print*, 'Number of times the operation is performed : ', reps
    
    
    !--------------------------------------------------------------------------------------------------!
    !1D ARRAY------------------------------------------------------------------------------------------!
    print*, ''
    print*, 'For 1D arrays'
    !ASSIGNMENT----------------------------------------------------------------------------------------!
    call cpu_time(t1)
    do i=1, reps
        do j=1, n
            arr_dp(j) = 12.345_8
        end do
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n) * 1e9
    write(*, '(a20,f12.6,a2)') '(Assignment) : ', time, 'ns'
    
    !READOUT AND ASSIGNMENT----------------------------------------------------------------------------!
    arr_dp_b = 12.345_8
    call cpu_time(t1)
    do i=1, reps
        do j=1, n
            arr_dp(j) = arr_dp_b(j)
        end do
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n) * 1e9
    write(*, '(a20,f12.6,a2)') '(Readout) : ', time, 'ns'
    
    !ADDITION------------------------------------------------------------------------------------------!
    arr_dp_c = 56.789_8
    call cpu_time(t1)
    do i=1, reps
        do j=1, n
            arr_dp(j) = arr_dp_b(j) + arr_dp_c(j)
        end do
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n) * 1e9
    write(*, '(a20,f12.6,a2)') '(Addition) : ', time, 'ns'
    
    !MULTIPLICATION------------------------------------------------------------------------------------!
    call cpu_time(t1)
    do i=1, reps
        do j=1, n
            arr_dp(j) = arr_dp_b(j) * arr_dp_c(j)
        end do
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n) * 1e9
    write(*, '(a20,f12.6,a2)') '(Multiplication) : ', time, 'ns'
    
    !--------------------------------------------------------------------------------------------------!
    !2D ARRAY ROW MAJOR--------------------------------------------------------------------------------!
    print*, ''
    print*, 'For 2D arrays (row major)'
    !ASSIGNMENT----------------------------------------------------------------------------------------!
    call cpu_time(t1)
    do i=1, reps
        do j=1, n
            do k=1, n
                mat_dp(j,k) = 12.345_8
            end do
        end do
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n*n) * 1e9
    write(*, '(a20,f12.6,a2)') '(Assignment) : ', time, 'ns'
    
    !READOUT AND ASSIGNMENT----------------------------------------------------------------------------!
    mat_dp_b = 12.345_8
    call cpu_time(t1)
    do i=1,reps
        do j=1, n
            do k=1, n
                mat_dp(j,k) = mat_dp_b(j,k)
            end do
        end do
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n*n) * 1e9
    write(*, '(a20,f12.6,a2)') '(Readout) : ', time, 'ns'
    
    !ADDITION------------------------------------------------------------------------------------------!
    mat_dp_c = 56.789_8
    call cpu_time(t1)
    do i=1, reps
        do j=1, n
            do k=1, n
                mat_dp(j,k) = mat_dp_b(j,k) + mat_dp_c(j,k)
            end do
        end do
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n*n) * 1e9
    write(*, '(a20,f12.6,a2)') '(Addition) : ', time, 'ns'
    
    !MULTIPLICATION------------------------------------------------------------------------------------!
    call cpu_time(t1)
    do i=1, reps
        do j=1, n
            do k=1, n
                mat_dp(j,k) = mat_dp_b(j,k) * mat_dp_c(j,k)
            end do
        end do
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n*n) * 1e9
    write(*, '(a20,f12.6,a2)') '(Multiplication) : ', time, 'ns' 

    !--------------------------------------------------------------------------------------------------!
    !2D ARRAY COLUMN MAJOR-----------------------------------------------------------------------------!
    print*, ''
    print*, 'For 2D arrays (column major)'
    !ASSIGNMENT----------------------------------------------------------------------------------------!
    call cpu_time(t1)
    do i=1, reps
        do k=1, n
            do j=1, n
                mat_dp(j,k) = 12.345_8
            end do
        end do
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n*n) * 1e9
    write(*, '(a20,f12.6,a2)') '(Assignment) : ', time, 'ns'
    
    !READOUT AND ASSIGNMENT----------------------------------------------------------------------------!
    mat_dp_b = 12.345_8
    call cpu_time(t1)
    do i=1,reps
        do k=1, n
            do j=1, n
                mat_dp(j,k) = mat_dp_b(j,k)
            end do
        end do
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n*n) * 1e9
    write(*, '(a20,f12.6,a2)') '(Readout) : ', time, 'ns'
    
    !ADDITION------------------------------------------------------------------------------------------!
    mat_dp_c = 56.789_8
    call cpu_time(t1)
    do i=1, reps
        do k=1, n
            do j=1, n
                mat_dp(j,k) = mat_dp_b(j,k) + mat_dp_c(j,k)
            end do
        end do
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n*n) * 1e9
    write(*, '(a20,f12.6,a2)') '(Addition) : ', time, 'ns'
    
    !MULTIPLICATION------------------------------------------------------------------------------------!
    call cpu_time(t1)
    do i=1, reps
        do k=1, n
            do j=1, n
                mat_dp(j,k) = mat_dp_b(j,k) * mat_dp_c(j,k)
            end do
        end do
    end do
    call cpu_time(t2)
    time = time_reps(t1,t2,reps)/real(n*n) * 1e9
    write(*, '(a20,f12.6,a2)') '(Multiplication) : ', time, 'ns'    

    !deallocating used matrices
    deallocate(arr_dp)
    deallocate(arr_dp_b)
    deallocate(arr_dp_c)
    deallocate(mat_dp)
    deallocate(mat_dp_b)
    deallocate(mat_dp_c)
    
end program computer_time
