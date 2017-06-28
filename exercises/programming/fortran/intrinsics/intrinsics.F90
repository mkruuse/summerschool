program intrinsics
  implicit none
  integer :: nx, ny
  integer :: i, alloc_stat
  real, dimension(:,:), allocatable :: A
  real, dimension(:), allocatable :: sums_of_rows
  real, dimension(:), allocatable :: sums_of_colmns
  real :: values_smaller
  write (*,*) 'Give number of rows and columns for matrix A:'
  read (*,*) nx, ny

  allocate(A(nx,ny), stat = alloc_stat)
  if (alloc_stat /= 0) call abort()
  allocate(sums_of_rows(ny),stat=alloc_stat)
  if (alloc_stat /= 0) call abort()
  allocate(sums_of_colmns(nx), stat=alloc_stat)
  if (alloc_stat /=0) call abort()
  ! Initializing array
  A(:,:)  = 65.0 ! middle
  A(:,1)  = 20.0 ! left
  A(:,ny) = 70.0 ! right
  A(1,:)  = 85.0 ! top
  A(nx,:) = 5.0  ! bottom
  sums_of_rows=sum(A,1)
  write(*,*) 'Sum of rows'
  write (*, '(*(F6.1))') sums_of_rows
  write(*,*)'Maximum element', maxloc(A)
  write(*,*)'Minumum value', minval(A) 
! write(*,*) 'Location of minumum',  minloc(A)
  write(*,*) 'A elements which are larger than zero',  any(A>=0.0)
  write(*,*) 'A elements which are larger than 0.5', any(A>=0.05)
 !--------------------------------------------------
  ! Print out the array
  write(*,*) 'The original matrix'
  do i = 1, nx
    write(*,'(*(F6.1))') A(i,:)
  end do
  !--------------------------------------------------
  ! TODO: use array intrinsics to probe elements of A




end program intrinsics
