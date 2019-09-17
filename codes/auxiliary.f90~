module auxiliary
    implicit none
    contains

    !-----------------------------------------------------------------------------------------------------------------------!
    !Subroutine for reading files-------------------------------------------------------------------------------------------!
    subroutine read_file_position(x_atomd)
        use parameters  
        implicit none
        integer(8)                      :: i, j, k ! counters
        integer(8)                      :: i_atom ! number of the atom/ atom label
        character(len=5)                :: atom_label ! type of the atom (Ar)
        character(len=3)                :: num ! type of the atom (Ar)
        real(8), dimension(n_atoms, 3)  :: x_atomd ! positions of the atoms in the system (array)
        real(8), dimension(n_atoms, 3)  :: v_atomd ! velocities of the atoms in the system (array)
        open(unit=42, file=inputfile)
        read(42, *)
        read(42, *)
        do j=1, n_atoms
            read(42, '(i5, 2a5, i5, 3f8.3)') i_atom, atom_label, num, &
        i_atom, (x_atomd(j, k), k= 1, 3)
        end do
        close(unit=42)
        return
    end subroutine

    !-----------------------------------------------------------------------------------------------------------------------!
    !Subroutine for writing files-------------------------------------------------------------------------------------------!


    !-----------------------------------------------------------------------------------------------------------------------!
    !Subroutine for calculating potential energy----------------------------------------------------------------------------!
    subroutine calculate_energy(x_atomd, potential_d)
	use parameters
	implicit none
	integer(8)			:: i, j, k ! indices
	integer(8)			:: a, b ! atomic labels
	real(8), dimension(n_atoms,3)	:: x_atomd ! vector of positions of atoms
	real(8), dimension(3)		:: distance_ab ! vector distance between two atoms
	real(8)				:: r_abs, r_squared ! modulus/distance between two atoms
	real(8)				:: potential_d ! interaction/potential between two atoms
	real(8)				:: r6_norm, r12_norm
	
    !Generating a clean array for the interaction energy between pairs of atoms
    do a=1, n_atoms-1 
	do b=a+1, n_atoms
		do k=1, 3
			distance_ab(k) = x_atomd(a,k) - x_atomd(b,k)
			distance_ab(k) = distance_ab(k) - anint(distance_ab(k)/l_unit)*l_unit ! nearest image convention
		end do

		r_abs = 0.0_8
		r_squared = 0.0_8

		do j=1, 3
			r_squared = r_squared + distance_ab(j)**2
		end do
		
		r_abs = sqrt(r_squared)
		r6_norm = (sigma/r_abs)**6
		r12_norm = (sigma/r_abs)**12
		potential_d = potential_d + (r12_norm - r6_norm)
	end do
    end do

    !Rescaling potential energy
    potential_d = 4.0*eps*potential_d
    return
    end subroutine


    subroutine force_atoms(x_atomd, forced, force_numd, potential_d)
        use parameters
        implicit none
        integer(8)                      :: i, j, k ! counters
        integer(8)                      :: a, b ! arbitrary lables for the two atoms in a pair
        real(8), dimension(n_atoms, 3)  :: x_atomd ! position of the atoms (array)
        real(8), dimension(n_atoms, 3)  :: forced, force_numd ! array of the forces exerted on any atom (array, component wise)
        real(8), dimension(3)           :: distance_ab, distance_ab_e ! distance between two atoms a and b in each direction
        real(8)                         :: force_q ! force along a specific component
        real(8)                         :: r_abs, r_squared ! absolute distance between atoms, square of the distance
        real(8)                         :: force_abs ! magnitude of the force exerted on an atom
        real(8)                         :: potential_d, potentiald_e, delv ! interaction potential between atoms (Lennard-Jones)
        real(8)                         :: r6_norm, r12_norm ! variables in the Lennard-Jones potential sigma/r*
	real(8)				:: e_step ! step size for taking the numerical derivative
    
    !Generating a clean array for the forces felt by the atoms
    do i=1, n_atoms
        do j=1, 3
            forced(i,j) = 0.0_8
        end do
    end do
    
    !Main calculation of forces between particles (pair-wise interactions only)
    do a=1, n_atoms - 1
        do b = a + 1, n_atoms
            do i=1, 3
                distance_ab(i) = x_atomd(a, i) - x_atomd(b, i)
                distance_ab(i) = distance_ab(i) - anint(distance_ab(i)/l_unit)*l_unit !setting up the periodic boundary conditions
            end do
            
            r_abs = 0.0_8
            r_squared = 0.0_8
	    delv = 0.0_8
            do j=1,3
                r_squared = r_squared + distance_ab(j)**2
            end do


	    !Calculating forces from analytic formula
    	    r_abs = sqrt(r_squared)
	    r6_norm = (sigma/r_abs)**6
	    r12_norm = (sigma/r_abs)**12
	    force_abs = (24*eps)*(1/r_abs)*(2.0_8*r12_norm - r6_norm)
            potential_d = potential_d + (r12_norm - r6_norm)

	    !Generate force array (analytic)
	    do i=1,3
		force_q = force_abs*distance_ab(i)/r_abs
		forced(a,i) = forced(a,i) + force_q
		forced(b,i) = forced(b,i) - force_q
	    end do

	    !Calculating forces numerically 
	    do i=1,3
		distance_ab_e(i) = x_atomd(a,i) - x_atomd(b,i) + e_step
		distance_ab_e(i) = distance_ab_e(i) - anint(distance_ab(i)/l_unit)*l_unit

		r_squared = distance_ab_e(i)**2
		do j=i+1, i+2
			if (j>3) then
				k = j - 3
			else 
				k = j
			end if

			r_squared = r_squared + distance_ab(k)**2
		end do

		r_abs = sqrt(r_squared)
	    	r6_norm = (sigma/r_abs)**6
	    	r12_norm = (sigma/r_abs)**12
		potentiald_e = potentiald_e + (r12_norm - r6_norm)
		delv = potentiald_e - potential_d
		force_numd(a,i) = force_numd(a,i) -delv/e_step 
	    end do            
        end do
    end do
        
    return
    end subroutine

end module
