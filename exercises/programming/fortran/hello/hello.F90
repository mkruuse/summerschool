program hello
  implicit none
!  integer, parameter :: nx=10, ny=10
  real :: a,b,c
  real :: sum
 ! real :: matrix(0:nx,0:ny)
  write(*,*) 'Write sides for a triangle'
  read(*,*) a,b,c
  if (a>0 .and. b >0 .and. c>0) then
   sum=a+b+c
   write (*,*) 'Triangle perimeter', sum
  else
    write (*,*) 'Negative values'
    stop
  end if
! write (*,*) 'Hello world from Fortran!'
end program hello
