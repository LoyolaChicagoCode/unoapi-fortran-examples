! Computation of the area under a curve using loop parallelism
program AreaUnderCurve
  implicit none
  integer :: i, n
  real :: a, b, dx, x, area

  n = 1000  ! Number of intervals
  a = 0.0   ! Lower limit of integration
  b = 1.0   ! Upper limit of integration
  dx = (b - a) / n  ! Width of each interval

  area = 0.0

  !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(x) SHARED(a, dx, n, area)
  do i = 1, n
    x = a + (i - 0.5) * dx  ! Midpoint of the interval
    area = area + f(x) * dx
  end do
  !$OMP END PARALLEL DO

  print *, "The area under the curve is:", area

contains

  ! Function representing the curve
  function f(x)
    real, intent(in) :: x
    real :: f

    f = x**2  ! Example function: x^2
  end function f

end program AreaUnderCurve
