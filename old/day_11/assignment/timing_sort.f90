module mean_stdev !I used a module to calculate mean and standard deviation of arrays
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

subroutine !xkcd

program timing_sorting

end program timing_sorting
