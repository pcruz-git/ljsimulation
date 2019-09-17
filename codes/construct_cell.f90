program construct_cell
	use parameters
    	implicit none
        integer(8)                          :: i, j, k, l, m  !counters
        real(8), dimension(n_atoms, 3) 	    :: x_atoms !current positions and velocities of atoms 
        real(8)                             :: a, b !lattice parameters
        real(8), dimension(3, 4)            :: cell
        integer(8)                          :: n_base !number of basis atoms
        integer(8), dimension(3)            :: i_cell !number of cell spanned in each direction, e.g. 2 x 2 x 3
        real(8)                             :: kb = 1.38065E-23

        !Global variables
        n_base = 4
        i_cell = 6 ! since we have a cubic lattice
        b = l_unit/6.0
        a = b/2.0


   	!Global vectors
    	cell = reshape( (/ 0.0_8, 0.0_8, 0.0_8, &
                     a, a, 0.0_8, &
                     a, 0.0_8, a, &
                     0.0_8, a, a /), (/ 3, 4 /) )    


    	!Coordinates of each atoms
	m=1
    	do k=0, i_cell(3)-1
        	do j=0, i_cell(2)-1
            		do i=0, i_cell(1)-1
                		do l=1, n_base
                    			x_atoms(m,1) = i*b + cell(1, l)
                    			x_atoms(m,2) = j*b + cell(2, l)
                    			x_atoms(m,3) = k*b + cell(3, l)
                    			m = m + 1
                		end do
            		end do
        	end do
    	end do
    	x_atoms = x_atoms -(l_unit/2.0_8)

    	!Writing the configuration file
    	open(unit=1, file='initial_positions.gro', status='replace', action='write')
    	write(1, *) 'Coordinates of atoms in the fcc structure'
    	write(1, *) 'Number of atoms:', n_atoms
    	do i=1, n_atoms
        	write(1, '(i5, 2a5, i5, 3f8.3)') i, atom_name, atom_name, i, x_atoms(i, 1), &
        	x_atoms(i, 2), x_atoms(i, 3)
    	end do
    	close(1)

end program
