!A convenient module that allows me to append elements in a list like in python
module dynamic_arrays
    implicit none
    public                                  :: append

contains
    subroutine append(list, element)
        implicit none
        integer                             :: i, imax
        real(8), intent(in)                 :: element
        real(8), allocatable, intent(inout) :: list(:)
        real(8), allocatable                :: dummy(:)
        
        if(allocated(list)) then
            imax = size(list)
            allocate(dummy(imax+1))
            do i=1, imax
                dummy(i) = list(i)
            end do
            dummy(imax+1) = element
            
            deallocate(list)
            call move_alloc(dummy, list)
            
        else
            allocate(list(1))
            list(1) = element
        
        end if
    end subroutine append

end module dynamic_arrays

!a function that returns the Boltzmann probability of the velocity
real(8) function prob(vd, md, td) result(res)
    implicit none
    real(8), intent(in)                     :: vd, md, td
    real(8)                                 :: pi = 4.0_8*atan(1.0_8)
    real(8)                                 :: kb = 1.38065E-23 !boltzmann constant in units of j/(kg*K)
    real(8)                                 :: a0 !constant in the distribution function
    a0 = sqrt(md/(2.0_8*pi*kb*td))
    res = a0*exp((-md*vd**2)/(2*kb*td))
end function

!a function that returns a random number between ad and bd
real(8) function random_range(ad, bd) result(res)
    implicit none
    real(8), intent(in)                     :: ad, bd
    res = (bd-ad)*rand(0) + ad
end function



program generate_xv
    use parameters
    use dynamic_arrays
    implicit none
    integer(8)                              :: i, j !counters
    real(8), dimension(n_atoms, 3)          :: x_i, v_i !initial arrays for the position and velocity of the atoms 
    real(8)                                 :: v_abs, v_rand, v0, v1, v2
    real(8)                                 :: pv, pv_rand, kb = 1.38065E-23
    real(8)                                 :: del = 0.2_8
    real(8), external                       :: prob, random_range
    
    !global variables
    v_abs = sqrt(kb*thermos_temp/m_atoms)
    
!     !Rejection method for velocities
!     do i=1, n_atoms
!         do j=1, 3
!         111 v_rand = random_range(-v_abs,v_abs)
!             print*, i,j, v_rand
!             pv = prob(v_rand, m_atoms, thermos_temp)
!             pv_rand = random_range(0.0_8, 1.0_8)
!             if (pv_rand<pv) then
!                 v_i(i,j) =v_rand
!                 go to 112
!             else
!                 go to 111
!         112 end if
!         end do
!     end do

    !random velocities
    do i=1, n_atoms
        do j=1, 3
            v_i(i, j) = 1e-3*random_range(-v_abs,v_abs)
        end do
    end do
    
    open(unit=42, file=inputfile)
    write(42, *) 'Test'
    write(42, *) n_atoms, 'Argon'
    do i=1, n_atoms
        write(42, '(i5, 2a5, i5, 3f8.3, 3f8.4)') i, 'Ar', 'Ar', i, random_range(0.0_8, l_unit/2.0), &
        random_range(0.0_8, l_unit/2.0), random_range(0.0_8, l_unit/2.0), v_i(i,1), v_i(i,2), v_i(i,3) 
    end do
    close(42)
end program generate_xv
