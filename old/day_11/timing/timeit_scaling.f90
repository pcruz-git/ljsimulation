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
program timeit_matmul
	use mystuff
	implicit none

	real(dp), dimension(:,:), allocatable	:: mat_a, mat_b, mat_c
	real(dp), dimension(:), allocatable	  	:: times_ij, times_ji
	real(dp) 								:: t1, t2, t3
	integer									:: n, rep, i, j
	integer							 		:: n_min, n_max, dn, reps
	integer									:: outfile, ios
	character(len=256)						:: outfile_name

	n_min = 500
	n_max = 10000
	dn = 500
	reps = 5

	outfile = 11
	outfile_name = 'times.matmul.dat'
	open(unit=outfile, file=outfile_name, status='replace', iostat=ios)
	if (ios .ne. 0) then
		write (stderr,'(A,A,A)') 'Could not open ', &
								trim(adjustl(outfile_name)), &
								'...'
		stop
	end if

	allocate(times_ij(reps))
	allocate(times_ji(reps))

	write (outfile, '(A)') '#Matrix multoplication times in ms'
	write (outfile, '(A6,A12,A12,A12,A12)') '#    n', 'Tij', 'Tji', 'sd(Tij)', 'sd(Tji)'

	do n = n_min, n_max, dn				! loop over matrix size n
		allocate(mat_a(n,n))
		allocate(mat_b(n,n))
		allocate(mat_c(n,n))

		do rep = 1, reps				! repetition loop for statistics
			call random_number(mat_a)
			call random_number(mat_b)

			call cpu_time(t1)

			do i = 1, n
				do j = 1, n
					mat_c(i,j) = mat_a(i,j) * mat_b(i,j)
				enddo
			enddo

			call cpu_time(t2)

			do j = 1, n
				do i = 1, n
					mat_c(i,j) = mat_a(i,j) * mat_b(i,j)
				enddo
			enddo

			call cpu_time(t3)

			times_ij(rep) = t2 - t1
			times_ji(rep) = t3 - t2
		enddo

		write (outfile, '(I6,F11.6,F11.6,F11.6,F11.6)') n, &
								mean(times_ij), mean(times_ji), &
								sd(times_ij), sd(times_ji)

		deallocate(mat_a, mat_b, mat_c)
	end do
	close(outfile)
end program
