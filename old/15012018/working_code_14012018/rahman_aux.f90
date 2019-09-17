module auxiliary
    implicit none
    contains
    
    !-------------------------------------------------------------------------------------------------!
    !Subroutine for reading the files for the position and velocities of each particle
    subroutine read_file(x_atomd, v_atomd)
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
            read(42, '(i5, 2a5, i5, 3f8.3, 3f8.3)') i_atom, atom_label, num, &
        i_atom, (x_atomd(j, k), k= 1, 3), (v_atomd(j, k), k= 1, 3)
        end do
        close(unit=42)
        return
    end subroutine
    
    !-------------------------------------------------------------------------------------------------!
    !Subroutine for writing the output of the molecular dynamics simulation into different files
    subroutine out(numd, x_atomsd, v_atomsd, pot_end, kin_end, tempd)
        use parameters
        implicit none
        integer(8)                      :: i, j, k
        integer(8)                      :: numd
        real(8), dimension(n_atoms, 3)  :: x_atomsd, v_atomsd
        real(8)                         :: pot_end, kin_end, tempd
        real(8)                         :: t, energy
        real(8)                         :: units ! used for converting lengths
        parameter( units = 10.0_8 )
        
        ! At the first step, generate files for position/velocity, energy and temperature
        if (numd==1) then
            open(unit=15, file='position.xyz', status='replace', action='write')
            open(unit=16, file='velocity.xyz', status='replace', action='write')
            open(unit=17, file='energy.log', status='replace', action='write')
            open(unit=18, file='temperature.log', status='replace', action='write')
            write(15, *) n_atoms
            write(15, *) l_unit*units, l_unit*units, l_unit*units
        end if
        
        ! Writing on the files every update 
        if (mod((numd-1),update)==0) then
            !position
            do i=1, n_atoms
                write(15,*) atom_name, (x_atomsd(i,j)*units, j=1,3)
            end do
            
            !velocities
            do i=1, n_atoms
                write(16,*) atom_name, (v_atomsd(i,j), j=1,3)
            end do
            
            !defining the current time step
            t = t_step*(numd-1)
            
            !energies
            energy = pot_end + kin_end
            write(17, *) t, pot_end, kin_end, energy
            write(18, *) t, tempd
            
            
        end if
        
        if (numd==n_steps) then
            open(unit=19, file='argon_final.gro', status='replace', action='write')
            write(19, *) 'Final positions for the Molecular Dynamics simulation of Argon'
            write(19, *) '# of atoms', n_atoms
            ! writing the final positions of the atoms
            do i=1, n_atoms
                write(19, '(i5, 2a5, i5, 3f8.3, 3f8.3)') i, atom_name, atom_name, i, &
                (x_atomsd(i,k), k=1,3), (v_atomsd(i,k), k=1,3)
            end do
            write(19,*) l_unit, l_unit, l_unit
            
            close(15)
            close(16)
            close(17)
            close(18)
            close(19)
        end if
        
    end subroutine
    
    !-------------------------------------------------------------------------------------------------!
    !Subroutine for calculating the forces exerted between particles
    subroutine force_atoms(x_atomd, forced, potentiald)
        use parameters
        implicit none
        integer(8)                      :: i, j, k ! counters
        integer(8)                      :: a, b ! arbitrary lables for the two atoms in a pair
        real(8), dimension(n_atoms, 3)  :: x_atomd ! position of the atoms (array)
        real(8), dimension(n_atoms, 3)  :: forced ! array of the forces exerted on any atom (array, component wise)
        real(8), dimension(3)           :: distance_ab ! distance between two atoms a and b in each direction
        real(8)                         :: force_q ! force along a specific component
        real(8)                         :: r_abs, r_squared ! absolute distance between atoms, square of the distance
        real(8)                         :: force_abs ! magnitude of the force exerted on an atom
        real(8)                         :: potentiald ! interaction potential between atoms (Lennard-Jones)
        real(8)                         :: r6_norm, r12_norm ! variables in the Lennard-Jones potential sigma/r*
    
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
            do j=1,3
                r_squared = r_squared + distance_ab(j)**2
            end do
            
            !Now, we add the important condition of the cutoff length
            if (r_squared<r_cut**2) then
                r_abs = sqrt(r_squared)
                r6_norm = (sigma/r_abs)**6
                r12_norm = (sigma/r_abs)**12
                potentiald = potentiald + (r12_norm - r6_norm) !the lennard jones potential
                force_abs = (24.0_8*eps*1e9)*(1/r_abs)*(2.0_8*r12_norm - r6_norm)
                
                !Generating the forces array
                do i=1, 3
                    force_q = force_abs * distance_ab(i)/r_abs
                    forced(a, i) = forced(a, i) + force_q !signs are important to indicate direction
                    forced(b, i) = forced(b, i) - force_q !signs are important to indicate direction
                end do
            end if
        end do
    end do
    
    !Rescaling the potential energy (units)
    potentiald = 4.0*eps*potentiald
    
    return
    end subroutine
    
    !-------------------------------------------------------------------------------------------------!
    !Subroutine for integrating out the equations of motion (Leapfrog method)
    subroutine integrate(x_atomd, v_atomd, forced, kineticd, temp)
        use parameters
        implicit none
        integer(8)                      :: i, j, k
        real(8), dimension(n_atoms,3)   :: x_atomd
        real(8), dimension(n_atoms,3)   :: v_atomd
        real(8), dimension(n_atoms,3)   :: forced
        real(8)                         :: kineticd
        real(8)                         :: temp
        real(8), dimension(3)           :: distance
        real(8)                         :: accel
        real(8)                         :: v0,x_past
        real(8)                         :: temp_scale
        
        !Initializing kinetic energy term
        kineticd = 0.0_8
        
        !Leapfrog algorithm
        do i=1, n_atoms
            do j=1, 3
                v0 = v_atomd(i,j)
                accel = forced(i,j)*(1e-15 / m_atoms)
                v_atomd(i,j) = v0 + t_step*accel
                x_atomd(i,j) = x_atomd(i,j) +t_step*v_atomd(i,j)
                kineticd = kineticd + (0.5_8*(v_atomd(i,j) + v0))**2
            end do
        end do

        !Uncomment if using Verlet Algorithm
        !Verlet algorithm
!         do i=1, n_atoms
!             do j=1, 3
!                 v0 = v_atomd(i,j)
!                 x_past = x_atomd(i,j) - v0*t_step
!                 x_atomd(i,j) = 2.0_8*x_atomd(i,j) - x_past + (forced(i,j)*(1e-15/m_atoms))*t_step**2
!                 v_atomd(i,j) = (x_atomd(i,j)-x_past)/(2.0_8*t_step)
!                 kineticd = kineticd + (0.5_8*(v_atomd(i,j)+v0))**2
!             end do
!         end do
        
        !Rescaling of the velocities due to a heat bath (thermostat)
        kineticd = 0.5_8*m_atoms*1e6*kineticd
        temp = kineticd/(1.5_8*n_atoms*1.38065E-23)
        
        if (thermos .eqv. .true.) then
            temp_scale = sqrt(thermos_temp/temp)
            do i=1, n_atoms
                do j=1, 3
                    v_atomd(i,j) = v_atomd(i,j)*temp_scale
                end do
            end do
        end if
        return
    end subroutine

    !-------------------------------------------------------------------------------------------------!
    !Subroutine for the pair-correlation function
    subroutine radial_dist(numd, x_atomsd, r_histd)
        use parameters
        implicit none
        integer(8)                      :: numd
        integer(8)                      :: i, j, k, l
        real(8), dimension(n_atoms, 3)  :: x_atomsd
        real(8), dimension(n_bins)      :: r_histd
        real(8), dimension(3)           :: distance_ij
        real(8)                         :: r_bin, rho, r_distance, r_distance2
        real(8)                         :: v_min, v_max, v_shell, r_label
        real(8)                         :: pi = 4.0_8*atan(1.0_8)
        
        !Binning variables
        rho = n_atoms/((l_unit)**3) ! global density of the system
        r_bin = l_unit/(2.0_8*n_bins) ! binning width/window (resolution)
        
        
        !Initialize the pair-correlation output file and the radial distributiona array
        if (numd==1) then
            open(unit=99, file='argon_rdf.dat', status='replace', action='write')
            
            do i=1, n_bins
                r_histd(i)=0
            end do
            
        end if
        
        !Adding frequency after every update
        if (mod((numd-1), update)==0) then
            do i=1, n_atoms - 1
                do j = i+1, n_atoms
                    do k=1, 3
                        distance_ij(k) = x_atomsd(i, k) - x_atomsd(j, k)
                        distance_ij(k) = distance_ij(k) - anint(distance_ij(k)/l_unit)*l_unit ! be cautious of the periodic boundary condition
                    end do
                    
                    r_distance = 0.0_8
                    r_distance2 = 0.0_8
                    
                    do k=1, 3
                        r_distance2 = r_distance2 + distance_ij(k)**2
                    end do
                    
                    r_distance = sqrt(r_distance2) ! the distance between particles has been obtained
                    l = 1 + nint(r_distance/r_bin) ! the bin index where this distance is found
                    if(l<=n_bins) then
                        r_histd(l) = r_histd(l) + 2
                    end if
                end do
            end do
        end if
        
        !Creating the radial distribution (pair-correlation) output file
        if (numd==n_steps) then
            do i=1, n_bins
                v_min = (4.0_8/3.0_8)*pi*rho*(r_bin*(i-1))**3
                v_max = (4.0_8/3.0_8)*pi*rho*(r_bin*i)**3
                v_shell = v_max-v_min
                r_label = (r_bin*(i-1)+r_bin/2.0_8)/sigma
                write(99, *) r_label, (r_histd(i)/(v_shell*n_atoms*(n_steps/update)))
            end do
        end if
        
    end subroutine
    
end module auxiliary
