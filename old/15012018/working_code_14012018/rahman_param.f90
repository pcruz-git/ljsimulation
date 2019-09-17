module parameters
    implicit none
    
    !General parameters
    integer(8), parameter               :: n_atoms = 864 !number of atoms in the system
    integer(8), parameter               :: n_steps = 5000 !number of time steps in the simulation
    real(8), parameter                  :: t_step = 1e-2 !time step (in ps)
    real(8), parameter                  :: l_unit = 3.47786_8 !size of the unit cell (this is 10^-9, nm)
    
    !Atomic parameters
    character(len=2), parameter         :: atom_name = 'Ar' !type of atom being considered, Argon
    real(8), parameter                  :: m_atoms = 6.6335209E-26 !mass of the atom
    
    !Lennard-Jones parameters
    real(8), parameter                  :: sigma = 0.34_8 !sigma in the lennard-jones potential (this is 10^-9, nm)
    real(8), parameter                  :: eps = 1.657E-21 !epsilon in the lennard-jones potential
    real(8), parameter                  :: r_cut = 0.765_8 !cutoff length to be considered
    
    !Other parameters
    logical, parameter                  :: thermos = .false. !determines if a thermocouple/thermostat will be used
    real(8), parameter                  :: thermos_temp = 94.4_8 !temperature of the heat bath/reservoir
    integer(8), parameter               :: update = 25 !frequency of update
    character(len=*), parameter         :: inputfile = 'argon_equi.gro'
    integer(8), parameter               :: n_bins = 200 ! bin windows
end module
