module mean_stdev
    implicit none
    public                                  :: t_mean, t_stdev
    
contains
    real(8) function t_mean(arrd) result(res) !in ms
        real(8), dimension(:), allocatable      :: arrd
        res = sum(arrd)/real(size(arrd)) 
    end function
    
    real(8) function t_stdev(arrd) result(res) !in ms
        real(8), dimension(:), allocatable      :: arrd
        res = sqrt(sum((arrd - t_mean(arrd))**2) / real(size(arrd))) 
    end function
end module

program operation_scaling
    use mean_stdev
    implicit none
    integer                                 :: m, n, i, j, k, reps
    real(8)                                 :: t1, t2, t3
    real(8), dimension(:,:), allocatable    :: mat_a, mat_b, mat_c
    real(8), dimension(:), allocatable      :: times_row, times_column
    integer                                 :: u
    
    !Global variables
    reps = 1e3 ! Number of times the simple operation will be performed
    n = 1e3 ! Dimension of the matrix
    
    !Declaring the "times" matrix
    allocate(times_row(reps))
    allocate(times_column(reps))
    
    
    !Checking the scaling of ASSIGNMENT operation
    !(saved in assign.dat)
    open(unit=u, file='assign.dat', status='replace', action='write')
    do m=50, n, 50
        allocate(mat_a(m,m))
        do i=1, reps
            call cpu_time(t1)
            
                do j=1, m
                    do k=1, m
                        mat_a(j,k) = 12.345_8
                    end do
                end do
            
            call cpu_time(t2)
            
                do j=1, m
                    do k=1, m
                        mat_a(k,j) = 12.345_8
                    end do
                end do
            
            call cpu_time(t3)    
            times_row(i) = t2-t1
            times_column(i) = t3-t2
        end do
        write(u, '(i5, f12.3, f12.3, f12.3, f12.3)') m, (1d3)*t_mean(times_row), (1d3)*t_stdev(times_row),&
        (1d3)*t_mean(times_column), (1d3)*t_stdev(times_column)
        deallocate(mat_a)
    end do
    close(u)
    
     
    !Checking the scaling of READOUT AND ASSIGNMENT operation
    !(saved in readout.dat)
    open(unit=u, file='readout.dat', status='replace', action='write')
    do m=50, n, 50
        allocate(mat_a(m,m))
        allocate(mat_b(m,m))
        mat_b = 12.345_8
        do i=1, reps
            call cpu_time(t1)
            
                do j=1, m
                    do k=1, m
                        mat_a(j,k) = mat_b(j,k)
                    end do
                end do
                
            call cpu_time(t2)
            
                do j=1, m
                    do k=1, m
                        mat_a(k,j) = mat_b(k,j)
                    end do
                end do
            
            call cpu_time(t3)
            times_row(i) = t2-t1
            times_column(i) = t3-t2
        end do
        write(u, '(i5, f12.3, f12.3, f12.3, f12.3)') m, (1d3)*t_mean(times_row), (1d3)*t_stdev(times_row),&
        (1d3)*t_mean(times_column), (1d3)*t_stdev(times_column)
        deallocate(mat_a)
        deallocate(mat_b)
    end do
    close(u)

    !Checking the scaling of ADDITION operation
    !(saved in addition.dat)
    open(unit=u, file='addition.dat', status='replace', action='write')
    do m=50, n, 50
        allocate(mat_a(m,m))
        allocate(mat_b(m,m))
        allocate(mat_c(m,m))
        mat_b = 12.345_8
        mat_c = 56.789_8
        do i=1, reps
            call cpu_time(t1)
            
                do j=1, m
                    do k=1, m
                        mat_a(j,k) = mat_b(j,k) + mat_c(j,k)
                    end do
                end do
                
            call cpu_time(t2)
            
                do j=1, m
                    do k=1, m
                        mat_a(k,j) = mat_b(k,j) + mat_c(k,j)
                    end do
                end do
            
            call cpu_time(t3)
            times_row(i) = t2-t1
            times_column(i) = t3-t2
        end do
        write(u, '(i5, f12.3, f12.3, f12.3, f12.3)') m, (1d3)*t_mean(times_row), (1d3)*t_stdev(times_row),&
        (1d3)*t_mean(times_column), (1d3)*t_stdev(times_column)
        deallocate(mat_a)
        deallocate(mat_b)
        deallocate(mat_c)
    end do
    close(u)
    
    !Checking the scaling of MULTIPLICATION operation
    !(saved in multiplication.dat)
    open(unit=u, file='multiplication.dat', status='replace', action='write')
    do m=50, n, 50
        allocate(mat_a(m,m))
        allocate(mat_b(m,m))
        allocate(mat_c(m,m))
        mat_b = 12.345_8
        mat_c = 56.789_8
        do i=1, reps
            call cpu_time(t1)
            
                do j=1, m
                    do k=1, m
                        mat_a(j,k) = mat_b(j,k) + mat_c(j,k)
                    end do
                end do
                
            call cpu_time(t2)
            
                do j=1, m
                    do k=1, m
                        mat_a(k,j) = mat_b(k,j) + mat_c(k,j)
                    end do
                end do
            
            call cpu_time(t3)
            times_row(i) = t2-t1
            times_column(i) = t3-t2
        end do
        write(u, '(i5, f12.3, f12.3, f12.3, f12.3)') m, (1d3)*t_mean(times_row), (1d3)*t_stdev(times_row),&
        (1d3)*t_mean(times_column), (1d3)*t_stdev(times_column)
        deallocate(mat_a)
        deallocate(mat_b)
        deallocate(mat_c)
    end do
    close(u)
    
    

end program operation_scaling
