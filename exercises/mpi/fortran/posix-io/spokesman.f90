program pario
  use mpi
  use, intrinsic :: iso_fortran_env, only : error_unit, output_unit
  implicit none

  integer, parameter :: datasize = 64, writer_id = 0
  integer :: rc, my_id, ntasks, localsize, i
  integer, dimension(:), allocatable :: localvector
  integer, dimension(datasize) :: fullvector
  integer :: funit=11
  character(10) :: fname="out.dat"
  
  call mpi_init(rc)
  call mpi_comm_size(mpi_comm_world, ntasks, rc)
  call mpi_comm_rank(mpi_comm_world, my_id, rc)

  if (ntasks > 64) then
    write(error_unit, *) 'Maximum number of tasks is 64!'
    call mpi_abort(MPI_COMM_WORLD, -1, rc)
  end if

  if (mod(datasize, ntasks) /= 0) then
    write(error_unit,*) 'Datasize (64) should be divisible by number of tasks'
    call mpi_abort(MPI_COMM_WORLD, -1, rc)
  end if

  localsize = datasize / ntasks
  allocate(localvector(localsize))

  localvector = [(i + my_id * localsize, i=1,localsize)]


  deallocate(localvector)
  funit=11

  call single_writer()
  call reader()
  call mpi_finalize(rc)

contains

!12.a)
  subroutine single_writer()
    implicit none
    ! TODO: Implement a function that writers the whole array of elements
    !       to a file so that single process is responsible for the file io 
   call mpi_gather(localvector,localsize, MPI_INTEGER, fullvector, localsize, MPI_INTEGER, 0, MPI_COMM_WORLD, rc )

   if(my_id==writer_id) then !writer_id=0
    open(funit, file="fname", form="unformatted", status="replace", access="stream")
    write(funit) fullvector
    close(funit)   
   end if
  end subroutine single_writer
  
!12.b)
  subroutine reader()
  implicit none
  
  if (my_id==writer_id) then
   open(funit, file="fname", form="unformatted", status="old", access="stream")
   read(funit, pos=1) fullvector !pos=1, position where to start the stream
   close(funit)
   write(output_unit, "(A, IO, A)") "Read", size(fullvector)
  end if
    call mpi_scatter(fullvector, localsize, mpi_integer, localvector, &
         & localsize, mpi_integer, writer_id, mpi_comm_world, rc)

  end subroutine reader

!13.
subroutine writer()
implicit none
 call mpi_file_write_at()
end subroutine reader
end program pario
