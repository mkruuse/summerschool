module laplacian_mod
  implicit none
  real, parameter :: dx = 0.01, dy = 0.01
  
contains
  
  subroutine initialize(inx,iny,field0)
  implicit none
  integer :: alloc_stat
  real, dimension(:,:), allocatable, intent(out) :: field0
  integer,intent(in):: inx,iny
  allocate(field0(inx,iny), stat=alloc_stat)
  if (alloc_stat/=0) call abort()
  field0=65.0
  field0(:,1)=20.0
  field0(:,iny)=70.0
  field0(1,:)=85.0
  field0(inx,:)=5
  end subroutine initialize
  
 subroutine finalize(array)
  implicit none
  real, dimension(:,:), allocatable,  intent(inout):: array
  deallocate(array)
 end subroutine finalize

  subroutine laplacian(curr, prev, inx,iny)
! TODO: insert a subroutine that computes a laplacian of the
! array "prev" and returns it as an array "curr"
  implicit none
  integer, intent(in) :: inx, iny
  real, dimension(inx,iny), intent(out) :: curr
  real, dimension(inx, iny), intent(in) :: prev
  integer :: i, j
   do i=2, inx-1
        do j=2, iny-1
                curr(i,j)=(prev(i-1,j)-2*prev(i, j)+prev(i+1,j))/(dx*dx)+(prev(i,j-1)-2*prev(i,j)+prev(i,j+1))/(dy*dy)
        end do
 end do
  end subroutine laplacian

  subroutine write_field(array, inx, iny)
! TODO: write a subroutine that prints "array" on screen
  implicit none
  integer :: i
  integer, intent(in) :: inx, iny
  real, dimension(inx,iny), intent(in) :: array
  write(*,*) "Previous array:"
  do i = 1, inx
   write(*,'(*(G10.1))') array(i,:)
  end do
 
  end subroutine write_field

end module laplacian_mod
