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
program timeit_matmult
	use mystuff
	implicit none

	real(dp), dimension(:,:), allocatable	:: mat_a, mat_b, mat_c
	real(dp), dimension(:), allocatable	  	:: times_ij, times_ji
	real(dp) 								:: t1, t2
	integer									:: n, t, i, j
	integer							 		:: n_min, n_max, dn
	integer									:: reps

	n_min = 20
	n_max = 2000
	dn = 2
	reps = 20

	allocate(times_ij(reps))
	allocate(times_ji(reps))

	write (stdout, '(A6,A12,A12,A12,A12)') 'n', 'Tij', 'Tji', 'sd(Tij)', 'sd(Tji)'

	do n = 20, n_max, dn
		allocate(mat_a(n,n))
		allocate(mat_b(n,n))
		allocate(mat_c(n,n))

		do t = 1, reps
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
			times_ij(t) = t2 - t1
			times_ij(t) = t3 - t2
		enddo
		write (stdout, '(I6,F12.6,F12.6,F12.6,F12.6)') n, &
								mean(times_ij), mean(times_ji), &
								sd(times_ij), sd(times_ji)

		deallocate(mat_a, mat_b, mat_c)
	end do

end program
