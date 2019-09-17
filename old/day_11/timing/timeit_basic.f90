! Module mystuff
! contains:
!	- stream constants STDOUT, STDERR, STDIN
!   - double precision bit number
!	- function warn to print warnings
!	- function print_mat to display matrices

module mystuff
	implicit none

	integer, parameter  :: stdin=5
	integer, parameter  :: stdout=6
	integer, parameter  :: stderr=0

	integer, parameter  :: sp = kind(1.0e0)
	integer, parameter  :: dp = kind(1.0d0)

	public				:: mean, sd

contains
	real(dp) function mean(array) result(arr_mean)
		real(dp), dimension(:), allocatable  :: array

		arr_mean = sum(array) / size(array)
	end function

	real(dp) function sd(array) result(arr_stddev)
		real(dp), dimension(:), allocatable  :: array

		arr_stddev = sqrt(sum((array - mean(array))**2) / size(array))
	end function
end module

! program timeit_matmult
!   times matrix multiplication ov nxn matrices
!   for n ranging from 20 to nmax
!
program timeit_basic
	use mystuff
	implicit none

	real(sp)								:: real_a, real_b, real_c
	real(dp)								:: dbl_a, dbl_b, dbl_c
	real(dp), dimension(:), allocatable		:: arr_a, arr_b, arr_c
	real(dp), dimension(:,:), allocatable	:: mat_a, mat_b, mat_c
	real(dp)								:: time
	real(dp) 								:: t1, t2
	integer									:: rep, i, j
	integer									:: nreps, n

	nreps = 1000
	n = 1000

	allocate(arr_a(n))
	allocate(arr_b(n))
	allocate(arr_c(n))

	allocate(mat_a(n, n))
	allocate(mat_b(n, n))
	allocate(mat_c(n, n))

	call random_number(real_a)
	call random_number(real_b)
	call random_number(real_c)
	call random_number(dbl_a)
	call random_number(dbl_b)
	call random_number(dbl_c)
	call random_number(arr_a)
	call random_number(arr_b)
	call random_number(arr_c)
	call random_number(mat_a)
	call random_number(mat_b)
	call random_number(mat_c)

	write (stdout,*) 'Note:'
	write (stdout,*) '  Abbrefiations: [=] assign, [r] read, [+] sum, [*] product.'
	write (stdout,*) '  Note that in reality:'
	write (stdout,*) '   [r] = [r] + [=], i.e. we first read, then assign'
	write (stdout,*) '   [+] = 2x[r] + [+] + [=]'
	write (stdout,*) '   [*] = 2x[r] + [*] + [=]'

	nreps = 1e6
	write (stdout,*) ''
	write (stdout,*) 'Timing of simple operations'
	write (stdout, '(A,I9,A)') 'Averaging over ', nreps, ' repetitions.'

! Simple tests for real(4)
!	note that real_a = real_b + real_c contains 4 operations
!	read real_b, read real_c, sum them, assign to real_a
	write (stdout,*) ''
	write (stdout,*) 'real(4):'

	call cpu_time(t1)
	do rep = 1, nreps
		real_a = 12.345e0
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[=] : ', (t2 - t1) / real(nreps,8) * 1d9, 'ns'

	call cpu_time(t1)
	do rep = 1, nreps
		real_a = real_b
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[r] : ', (t2 - t1) / real(nreps,8) * 1d9, 'ns'

	call cpu_time(t1)
	do rep = 1, nreps
		real_a = real_b + real_c
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[+] : ', (t2 - t1) / real(nreps,8) * 1d9, 'ns'

	call cpu_time(t1)
	do rep = 1, nreps
		real_a = real_b * real_c
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[*] : ', (t2 - t1) / real(nreps,8) * 1d9, 'ns'

! Simple tests for real(8) or double
	write (stdout,*) ''
	write (stdout,*) 'real(8), aka double:'

	call cpu_time(t1)
	do rep = 1, nreps
		dbl_a = 12.345d0
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[=] : ', (t2 - t1) / real(nreps,8) * 1d9, 'ns'

	call cpu_time(t1)
	do rep = 1, nreps
		dbl_a = dbl_b
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[r] : ', (t2 - t1) / real(nreps,8) * 1d9, 'ns'

	call cpu_time(t1)
	do rep = 1, nreps
		dbl_a = dbl_b + dbl_c
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[+] : ', (t2 - t1) / real(nreps,8) * 1d9, 'ns'

	call cpu_time(t1)
	do rep = 1, nreps
		dbl_a = dbl_b * dbl_c
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[*] : ', (t2 - t1) / real(nreps,8) * 1d9, 'ns'

!-------------------------------------------------------------
! Operations on arrays (we'll stick to inly double precision)
	nreps = 1000

	write (stdout,*) ''
	write (stdout,*) 'For arrays we use double precision and write time per element...'
	write (stdout, '(A,I9,A)') 'Averaging over ', nreps, ' repetitions.'

! 1D arrays (array operations)
	write (stdout,*) ''
	write (stdout,*) '1D arrays (array operations):'

	call cpu_time(t1)
	do rep = 1, nreps
		arr_a = 12.345d0
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[=] : ', (t2 - t1) / real(nreps*n,8) * 1d9, 'ns'

	call cpu_time(t1)
	do rep = 1, nreps
		arr_a = arr_b
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[r] : ', (t2 - t1) / real(nreps*n,8) * 1d9, 'ns'

	call cpu_time(t1)
	do rep = 1, nreps
		arr_a = arr_b + arr_c
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[+] : ', (t2 - t1) / real(nreps*n,8) * 1d9, 'ns'

	call cpu_time(t1)
	do rep = 1, nreps
		arr_a = arr_b * arr_c
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[*] : ', (t2 - t1) / real(nreps*n,8) * 1d9, 'ns'

! 1D arrays (manual operations)

	write (stdout,*) ''
	write (stdout,*) '1D arrays (manual element-wise):'

	call cpu_time(t1)
	do rep = 1, nreps
		do i = 1, n
			arr_a(i) = 12.345d0
		enddo
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[=] : ', (t2 - t1) / real(nreps*n,8) * 1d9, 'ns'

	call cpu_time(t1)
	do rep = 1, nreps
		do i = 1, n
			arr_a(i) = arr_b(i)
		enddo
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[r] : ', (t2 - t1) / real(nreps*n,8) * 1d9, 'ns'

	call cpu_time(t1)
	do rep = 1, nreps
		do i = 1, n
			arr_a(i) = arr_b(i) + arr_c(i)
		enddo
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[+] : ', (t2 - t1) / real(nreps*n,8) * 1d9, 'ns'

	call cpu_time(t1)
	do rep = 1, nreps
		do i = 1, n
			arr_a(i) = arr_b(i) * arr_c(i)
		enddo
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[*] : ', (t2 - t1) / real(nreps*n,8) * 1d9, 'ns'

! 2D arrays (array operations)

	write (stdout,*) ''
	write (stdout,*) '2D arrays (array operations):'

	call cpu_time(t1)
	do rep = 1, nreps
		mat_a = 12.345d0
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[=] : ', (t2 - t1) / real(nreps*n*n,8) * 1d9, 'ns'

	call cpu_time(t1)
	do rep = 1, nreps
		mat_a = mat_b
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[r] : ', (t2 - t1) / real(nreps*n*n,8) * 1d9, 'ns'

	call cpu_time(t1)
	do rep = 1, nreps
		mat_a = mat_b + mat_c
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[+] : ', (t2 - t1) / real(nreps*n*n,8) * 1d9, 'ns'

	call cpu_time(t1)
	do rep = 1, nreps
		mat_a = mat_b * mat_c
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[*] : ', (t2 - t1) / real(nreps*n*n,8) * 1d9, 'ns'

! 2D arrays (maual element-wise column-major)

	write (stdout,*) '2D arrays (manual element-wise, column-major):'

	call cpu_time(t1)
	do rep = 1, nreps
		do i = 1, n
			do j = 1, n
				mat_a(j,i) = 12.345d0
			enddo
		enddo
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[=] : ', (t2 - t1) / real(nreps*n*n,8) * 1d9, 'ns'

	call cpu_time(t1)
	do rep = 1, nreps
		do i = 1, n
			do j = 1, n
				mat_a(j,i) = mat_b(j,i)
			enddo
		enddo
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[r] : ', (t2 - t1) / real(nreps*n*n,8) * 1d9, 'ns'

	call cpu_time(t1)
	do rep = 1, nreps
		do i = 1, n
			do j = 1, n
				mat_a(j,i) = mat_b(j,i) + mat_c(j,i)
			enddo
		enddo
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[+] : ', (t2 - t1) / real(nreps*n*n,8) * 1d9, 'ns'

	call cpu_time(t1)
	do rep = 1, nreps
		do i = 1, n
			do j = 1, n
				mat_a(j,i) = mat_b(j,i) * mat_c(j,i)
			enddo
		enddo
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[*] : ', (t2 - t1) / real(nreps*n*n,8) * 1d9, 'ns'

! 2D arrays (maual element-wise row-major)

	write (stdout,*) '2D arrays (manual element-wise, row-major):'

	call cpu_time(t1)
	do rep = 1, nreps
		do i = 1, n
			do j = 1, n
				mat_a(i,j) = 12.345d0
			enddo
		enddo
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[=] : ', (t2 - t1) / real(nreps*n*n,8) * 1d9, 'ns'

	call cpu_time(t1)
	do rep = 1, nreps
		do i = 1, n
			do j = 1, n
				mat_a(i,j) = mat_b(i,j)
			enddo
		enddo
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[r] : ', (t2 - t1) / real(nreps*n*n,8) * 1d9, 'ns'

	call cpu_time(t1)
	do rep = 1, nreps
		do i = 1, n
			do j = 1, n
				mat_a(i,j) = mat_b(i,j) + mat_c(i,j)
			enddo
		enddo
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[+] : ', (t2 - t1) / real(nreps*n*n,8) * 1d9, 'ns'

	call cpu_time(t1)
	do rep = 1, nreps
		do i = 1, n
			do j = 1, n
				mat_a(i,j) = mat_b(i,j) * mat_c(i,j)
			enddo
		enddo
	enddo
	call cpu_time(t2)
	write (stdout,'(A10,F6.3,A)') '[*] : ', (t2 - t1) / real(nreps*n*n,8) * 1d9, 'ns'

	deallocate(arr_a, arr_b, arr_c, mat_a, mat_b, mat_c)
end program
