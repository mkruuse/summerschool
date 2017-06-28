program loops
  implicit none
  ! TODO define parameters nx and ny
  integer, parameter :: nx=10, ny=10
  real :: matrix(0:nx+1, 0:ny+1)
  ! TODO: define real-valued array A
  integer :: i, j
 matrix=65.0 
 do i=0, nx
   matrix(i,0)=20.0
   matrix(i,ny)=70.0
  end do
  do j=0, ny
   matrix(0,j)=85.0
   matrix(i,j)=5.0
  end do
 !write (*,*) 'matrix', matrix 
  ! TODO initialize array A here



  !--------------------------------------------------
  ! Print out the array
  ! the ':' syntax means the whole row, see the Fortran arrays lecture
  do i = 0, nx+1
     write(*, '(12F6.1)') matrix(i,:)
  end do

end program loops
