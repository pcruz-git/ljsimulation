real(8) function rand_uniform(a,b) result(res)
	implicit none
	real(8)						:: a, b, temp
	call random_number(temp)
	res = a+temp*(b-a)
end function

real(8) function rand_normal(mean,stdev) result(res)
	real(8)						:: mean, stdev, res, temp(2)
	real(8), parameter				:: PI=3.141592653589793238462
	if (stdev <= 0.0d0) then
		write(*,*) "Standard deviation is negative."
	else
		call random_number(temp)
		r=(-2.0d0*log(temp(1)))**0.5
        	theta = 2.0d0*PI*temp(2)
        	res= mean+stdev*r*sin(theta)
	end if
end function

integer(8) function rand_atom(natomsd) result(res)
	implicit none
	integer(8)					:: natomsd
	real(8)						:: temp
	call random_number(temp)
	res = 1 + floor(natomsd*temp)
end function

subroutine init_random_seed
	implicit none
	integer						:: i, nseed, clock
	integer, dimension(:), allocatable		:: seed
	
	call random_seed(size=nseed)
	allocate(seed(nseed))
	call system_clock(clock)
	
	seed = clock/2 + 37*(/ (i-1 , i=1, nseed)/)
	call random_seed(put=seed)
	
	deallocate(seed)
end subroutine


program monte_carlo
	use parameters
	use auxiliary
	implicit none
	real(8)						:: x, y, z, P, rand, dr
	real(8)						:: u_energy
	real(8), dimension(n_atoms,3) 			:: x_atoms, displace_random
	integer(8)					:: i, j, k, i_atom
	integer, dimension(1)				:: seed=(/12345812/)
	integer(8), external				:: rand_atom
	real(8), external				:: rand_uniform, rand_normal
	
	! Constructing array of atomic positions
	call read_file_position(x_atoms)	

	! Calculating initial potential energy
	call calculate_energy(x_atoms, u_energy)
	print*, "Initial potential energy = ", u_energy

	! Displacing the positions by a random amount
	call init_random_seed
	dr = l_unit/2.0
	i_atom = random_atom(n_atoms)
	do i=1,3
		x_atoms(i_atom,i) = x_atoms(i_atom,i) + dr*
	do k=1,10
		rand = rand_uniform(12.0_8,15.0_8)
		print*, rand
		i_atom = rand_atom(n_atoms)
		print*, i_atom
	end do

!next, choose a random atom, displace by a certain amount and calculate total energy, xkcd
end program


