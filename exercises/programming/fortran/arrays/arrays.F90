program arrays
  implicit none
  integer :: nx, ny
  integer :: i, alloc_stat  
! TODO: define allocatable array A
  real, allocatable :: A(:,:)
  write (*,*) 'Give number of rows and columns for matrix A:'
  read (*,*) nx, ny
  allocate(A(nx,ny), stat=alloc_stat)
  if (alloc_stat /=0) call abort()
  A=100.0
  A(:,1)=20.0
  A(:,ny)=200.0
  A(1,:)=30.0
  A(nx,:)=300.0
  do i = 1, nx
     write(*, '(*(F6.1))') A(i,:) 
 end do   

  deallocate(A)

  ! TODO allocate A now that we know nx and ny


  ! TODO Use array syntax to initialize the array


end program arrays
