!Molecular dynamics of Argon
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
    real(8)                             :: pot_en, kin_en, tot_en, temp !overall energies of the system
    real(8), dimension(n_bins)          :: r_hist !array of frequencies for the radial distribution function

    !Converting initial data (position and velocity) into arrays and saving initial values for correlation function calculation
    call read_file(x_atoms, v_atoms)
    x_atoms0 = x_atoms
    v_atoms0 = v_atoms
    
    !Molecular dynamics main code
    !Actual simulation being recorded/tabulated
    do i=1, n_steps
        call force_atoms(x_atoms, forces, pot_en)
        call integrate(x_atoms, v_atoms, forces, kin_en, temp)
        call radial_dist(i, x_atoms, r_hist)
        call out(i, x_atoms, v_atoms, pot_en, kin_en, temp)
        write(*, fmt='(a1, a, t45, f6.2, a)', advance='no') achar(13), & !this is a code for displaying the progress the operations
        & '(Simulation/Recording) Percent complete: ', (real(i)/real(n_steps))*100.0, "%"
    end do
    print*, !clearing the screen of status

end program molecular_dynamics
