!Molecular dynamics of Argon
!-------------------------------------------------------------------------------------------------!
!Subroutine for writing the output of the molecular dynamics simulation into different files
subroutine out(numd, x_atomsd, v_atomsd, pot_end, kin_end, tempd)
    use parameters
    implicit none
    integer(8)                          :: i, j, k
    integer(8)                          :: numd
    real(8), dimension(n_atoms, 3)      :: x_atomsd, v_atomsd
    real(8)                             :: pot_end, kin_end, tempd
    real(8)                             :: t, energy
    real(8)                             :: units ! used for converting lengths
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
        open(unit=19, file='final_argon.gro', status='replace', action='write')
        write(19, *) 'Final positions for the Molecular Dynamics simulation of Argon'
        write(19, *) '# of atoms', n_atoms
        ! writing the final positions of the atoms
        do i=1, n_atoms
            write(19, '(i5, 2a5, i5, 3f8.3, 3f8.3)') i, 'argon', 'Ar', i, &
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
! !Subroutine on the velocity autocorrelation function
! subroutine vel_corr(numd, x_atoms0d, x_atomsd, vel_corrd)
!-------------------------------------------------------------------------------------------------!
!Main molecular dynamics code
program molecular_dynamics
    use parameters
    use auxiliary
    implicit none
    integer(8)                          :: i, j, k !counters
    real(8), dimension(n_atoms, 3)      :: x_atoms, v_atoms !current positions and velocities
    real(8), dimension(n_atoms, 3)      :: x_atoms0, v_atoms0 !initial positions and velocities
    real(8), dimension(n_atoms, 3)      :: forces !current forces influencing the system
    real(8)                             :: pot_en, kin_en, tot_en, temp !overall potential, kinetic and total energy of the system
    real(8), dimension(n_bins)          :: r_hist !array of frequencies for the radial distribution function

    
    !Converting initial data (position and velocity) into arrays and saving initial values for correlation function calculation
    call read_file(x_atoms, v_atoms)
    x_atoms0 = x_atoms
    v_atoms0 = v_atoms
    
    !Molecular dynamics main code
    
    !Equilibriation
    do i=1, n_equil
        call force_atoms(x_atoms, forces, pot_en)
        call leapfrog(x_atoms, v_atoms, forces, kin_en, temp)
        write(*, fmt='(a1, a, t45, f6.2, a)', advance='no') achar(13), & !this is a code for displaying the progress the operations
        & '(Equilibriation) Percent complete: ', (real(i)/real(n_equil))*100.0, "%"
    end do
    print*, !clearing the screen of status
    
    !Actual simulation being recorded/tabulated
    do i=1, n_steps
        call force_atoms(x_atoms, forces, pot_en)
        call leapfrog(x_atoms, v_atoms, forces, kin_en, temp)
        call radial_dist(i, x_atoms, r_hist)
        call out(i, x_atoms, v_atoms, pot_en, kin_en, temp)
        write(*, fmt='(a1, a, t45, f6.2, a)', advance='no') achar(13), & !this is a code for displaying the progress the operations
        & '(Simulation/Recording) Percent complete: ', (real(i)/real(n_steps))*100.0, "%"
    end do
    print*, !clearing the screen of status

end program molecular_dynamics
