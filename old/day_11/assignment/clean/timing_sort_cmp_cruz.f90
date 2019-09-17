module mean_stdev !I used a module to calculate mean and standard deviation of arrays
    implicit none
    public                                      :: t_mean, t_stdev
    
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

subroutine pairsort(x,y)
    implicit none
    real(8), intent(inout)                      :: x, y
    real(8)                                     :: n !buffer
    if (x>y) then
        n=x
        x=y
        y=n
    end if
end subroutine pairsort

program timing_sorting !timing the "bubble sort" sorting algorithm
    use mean_stdev
    implicit none
    real(8), dimension(:), allocatable          :: array, times
    real(8)                                     :: t1, t2
    integer(8)                                  :: p, reps
    integer(8)                                  :: i, j, n, m
    !Global variables
    n=1e4 + 1
    reps = 5
    
    !Array for storing times
    allocate(times(reps))
    
    !actual sorting operation
    open(unit=42, file='sorting.dat', status='replace', action='write')
    do m=1, n, 100
        write(*, fmt='(a1, a, t45, f6.2, a)', advance='no') achar(13), & !this is a code for displaying the progress the operations
        & '(Sorting) Percent complete: ', (real(m)/real(n))*100.0, "%"
        allocate(array(m))
        do p=1, reps
            call random_number(array)
            
            call cpu_time(t1)
            
            do i=1, m
                do j=1, m-1
                    call pairsort(array(j),array(j+1))
                end do
            end do
            
            call cpu_time(t2)
            
            times(p) = t2-t1
        end do
        write(42, '(i5, 2f12.6)') m, (1d3)*t_mean(times), (1d3)*t_stdev(times)
        deallocate(array)
    end do
    close(42)
    write(*,*)
end program timing_sorting
