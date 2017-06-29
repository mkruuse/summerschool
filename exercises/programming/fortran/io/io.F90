module io

contains

  ! Reads the temperature distribution from an input file
  subroutine read_field(field, filename)
    ! TODO: implement function that will:
    ! open the file
    ! read the first header line to get nx and ny
    ! allocate matrix called field
    ! read rest of the file into field
    ! close the file

    implicit none
    integer :: i, nx, ny
    real, allocatable, dimension(:) :: temp
    real, dimension(:,:), allocatable, intent(out) :: field
    character(len=*), intent(in) :: filename
    open(unit=11, file='filename', status="old", action="read")
    read(11,fmt='(I10,I10)') nx, ny
    print*,'nx, ny', nx, ny
    allocate(temp(ny)) 
    allocate(field(nx,ny))
    read(11, fmt=*)
    do i=1, nx
     read(11, fmt=*) temp(:)
     field(i,:)=temp
    end do
    close(11)

 end subroutine read_field

  ! Output routine, saves the temperature distribution as a png image
  subroutine write_field(field, iter)
    use iso_fortran_env, only : REAL64
    use pngwriter
    implicit none
    integer, parameter :: dp = REAL64
    real, intent(in) :: field(:,:)
    integer, intent(in) :: iter

    character(len=85) :: filename
    integer :: nx, ny, stat

    nx = size(field, 1)
    ny = size(field, 2)

    write(filename,'(A5,I4.4,A4,A)')  'heat_', iter, '.png'
    stat = save_png(real(field, kind=dp), nx, ny, filename)

  end subroutine write_field

end module io
