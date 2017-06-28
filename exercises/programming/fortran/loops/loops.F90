program loops
  implicit none
  ! TODO define parameters nx and ny
  integer, parameter :: nx=10, ny=10, k=11, l=11
  real :: matrix(0:nx+1, 0:ny+1)
  ! TODO: define real-valued array A
  integer :: i, j
 matrix=65.0
 matrix(1:k,0)=20.0
 matrix(1:k,l)=70
 matrix(0,1:l)=85.0
 matrix(k,1:l)=5.0
! do i=0, k
!  matrix(i,0)=20.0
!  matrix(i,l)=70.0
! end do
! do j=0, l
!  matrix(0,j)=85.0
!  matrix(k,j)=5.0
! end do
!write (*,*) 'matrix', matrix 
  ! TODO initialize array A here



  !--------------------------------------------------
  ! Print out the array
  ! the ':' syntax means the whole row, see the Fortran arrays lecture
  do i = 0, nx+1
     write(*, '(12F6.1)') matrix(i,:)
  end do

end program loops
