program subroutines
  use laplacian_mod
  implicit none
! TODO: define the arrays
  integer :: nx, ny
  real, dimension(:,:), allocatable :: previous, current
  write (*,*) 'Give number of rows and columns for matrix A:'
  read (*,*) nx, ny

  ! initialize the array
  call initialize(nx, ny, previous)
  call initialize(nx, ny, current)

  call write_field(previous, nx, ny)

  ! compute the Laplacian
  call laplacian(current, previous, nx, ny)

  ! print the result array
  call write_field(current, nx, ny)
  
  deallocate(previous)
!  call finalize(previous)
  deallocate(current)
!  call finalize(previous) 
end program subroutines

