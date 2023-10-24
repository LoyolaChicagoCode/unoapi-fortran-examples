! Computation of the dot product of two vectors using loop parallelism
program VectorDotProduct
  implicit none
  integer :: i, n
  real :: dot_product
  real, dimension(:), allocatable :: vector1, vector2

  n = 10000  ! Number of elements in the vectors
  allocate(vector1(n), vector2(n))

  ! Initialize the vectors (example values)
  vector1 = 1.0
  vector2 = 2.0

  dot_product = 0.0

  !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(i) SHARED(n, vector1, vector2, dot_product)
  do i = 1, n
    dot_product = dot_product + vector1(i) * vector2(i)
  end do
  !$OMP END PARALLEL DO

  print *, "The dot product is:", dot_product

end program VectorDotProduct
