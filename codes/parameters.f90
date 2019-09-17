!Module of all the parameters to be used in the simulation
module parameters
    implicit none
    
    !General parameters
    integer(8), parameter               :: n_atoms = 864 !number of atoms in the system
    integer(8), parameter               :: n_steps = 1000 !number of time steps in the simulation
    real(8), parameter                  :: t_step = 1e-2 !time step (in ps)
    real(8), parameter                  :: l_unit = 100_8 !size of the unit cell (this is 10^-9, nm)
    
    !Atomic parameters
    character(len=2), parameter         :: atom_name = 'A' !type of atom being considered, Argon
    real(8), parameter                  :: m_atoms = 0.0001_8 !mass of the atom (in kg)
    
    
    !Lennard-Jones parameters
    real(8), parameter                  :: sigma = 1.0_8 !sigma in the lennard-jones potential (this is 10^-9, nm)
    real(8), parameter                  :: eps = 100.0_8 !epsilon in the lennard-jones potential
    real(8), parameter                  :: r_cut = 0.765_8 !cutoff length to be considered
    
    !Other parameters
    logical, parameter                  :: thermos = .false. !determines if a thermocouple/thermostat will be used
    real(8), parameter                  :: thermos_temp = 94.4_8 !temperature of the heat bath/reservoir
    integer(8), parameter               :: update = 10 !freqency of update
    character(len=*), parameter         :: inputfile = 'initial_positions.gro'
    integer(8), parameter               :: n_bins = 200 ! bin windows
    
    !Note that in this simulation positions are presented in nanometer(10^-9), velocities in nm/ps (10^3m/s)
end module
