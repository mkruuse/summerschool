program datatype1
  use mpi
  implicit none

  integer, dimension(8,8) :: array
  integer :: rank, ierr
  !TODO: declare variable for datatype
  integer :: i, j
  integer :: rowtype

  call mpi_init(ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, rank ,ierr)

  ! initialize arrays
  if (rank == 0) then
     do i=1,8
        do j=1,8
           array(i,j) = i*10 + j
        end do
     end do
  else
     array(:,:) = 0
  end if

  !TODO: create datatype describing one row, use mpi_type_vector
  call mpi_type_vector(8,1,8,mpi_integer, rowtype, ierr)
  call mpi_type_commit(rowtype, ierr)
  !TODO: send first row of matrix from rank 0 to 1
  if(rank==0) then
  mpi_send(array,1, rowtype, 1, MPI_ANY_TAG, MPI_COMM_WORLD, ierr )
  end if
  ! Print out the result
  if (rank == 1) then
     do i=1,8
        write(*,'(8I3)') array(i, :)
     end do
  end if

  !TODO free datatype

  call mpi_finalize(ierr)

end program datatype1
