program example_mean
  use ieee_arithmetic, only: ieee_quiet_nan
  use stdlib_stats_mean, only: mean
  implicit none
  real :: x(1:6) = [1., 2., 3., 4., 5., 6.]
  real :: y(1:2, 1:3) = reshape([1., 2., 3., 4., 5., 6.], [2, 3])
  real :: res(3)
  print *, mean(x)                                  !returns 3.5
  if (abs(mean(x) - 3.5) > 1e-8) error stop
  print *, mean(y)                                  !returns 3.5
  if (abs(mean(y) - 3.5) > 1e-8) error stop
  res = mean(y, 1)                                 !returns [ 1.5, 3.5, 5.5 ]
  print *, mean(y, 1)                               !returns [ 1.5, 3.5, 5.5 ]
  if (abs(res(1) - 1.5) > 1e-8) error stop
  if (abs(res(2) - 3.5) > 1e-8) error stop
  if (abs(res(3) - 5.5) > 1e-8) error stop
  print *, mean(y, 1, y > 3.)                       !returns [ NaN, 4.0, 5.5 ]
end program example_mean
