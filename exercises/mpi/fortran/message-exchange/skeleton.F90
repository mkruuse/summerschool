program exchange
  use mpi
  implicit none
  integer, parameter :: size = 100000
  integer :: rc, myid, ntasks, count
  integer :: status(MPI_STATUS_SIZE)
  integer :: message(size)
  integer :: receiveBuffer(size)

  call mpi_init(rc)
  call mpi_comm_rank(MPI_COMM_WORLD, myid, rc)
  call mpi_comm_size(MPI_COMM_WORLD, ntasks, rc)

  message = myid

  ! TODO: Implement sending and receiving as defined in the assignment
  if ( myid == 0 ) then
     call mpi_send(message, size, mpi_integer, myid+1, 10, MPI_COMM_WORLD, rc)
     call mpi_recv(receiveBuffer, size, mpi_integer, myid+1, MPI_ANY_TAG, MPI_COMM_WORLD, status, rc)  
     write(*,'(A10,I3,A10,I3)') 'Rank: ', myid, &
          ' received ', receiveBuffer(1)
  else if (myid == 1) then
     call mpi_recv(receiveBuffer, size, mpi_integer, myid-1, MPI_ANY_TAG, MPI_COMM_WORLD, status, rc)
     call mpi_send(message, size, mpi_integer, myid-1, 11, MPI_COMM_WORLD,rc)
     write(*,'(A10,I3,A10,I3)') 'Rank: ', myid, &
          ' received ', receiveBuffer(1)
  end if

  call mpi_finalize(rc)

end program exchange
