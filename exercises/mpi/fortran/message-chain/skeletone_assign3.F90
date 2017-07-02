program basic
  use mpi
  use iso_fortran_env, only : REAL64

  implicit none
  integer, parameter :: size = 100
  integer :: rc, myid, ntasks, count
  integer :: status(MPI_STATUS_SIZE)
  integer :: message(size)
  integer :: receiveBuffer(size)
  integer :: source, destination
  real(REAL64) :: t0, t1
  integer, dimension(2) :: variable_request
  integer :: array_of_statuses(mpi_status_size,2)
  call mpi_init(rc)
  call mpi_comm_rank(MPI_COMM_WORLD, myid, rc)
  call mpi_comm_size(MPI_COMM_WORLD, ntasks, rc)
  
  message = myid
 
  if (myid < ntasks-1) then
     destination = myid + 1
  else
     destination = MPI_PROC_NULL
  end if
  if (myid > 0) then
     source = myid - 1
  else
     source = MPI_PROC_NULL
  end if

! Start measuring the time spent in communication
  call mpi_barrier(mpi_comm_world, rc)
  t0 = mpi_wtime()
 ! TODO: Send and receive as defined in the assignment
!3. Message chain:
!  call mpi_sendrecv(message, size, MPI_INTEGER, destination, myid + 1, receiveBuffer, size, MPI_INTEGER, source, MPI_ANY_TAG, &
!       MPI_COMM_WORLD, status, rc)
!  print*, "Message chain!"
!  write(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Sender: ', myid, &
!       ' Sent elements: ', size, &
!       '. Tag: ', myid + 1, &
!       '. Receiver: ', destination

!7. Collective operators:

  call mpi_Irecv(receiveBuffer, size, mpi_integer, source, MPI_ANY_TAG, MPI_COMM_WORLD, variable_request(1), rc)
  call mpi_Isend(message, size, mpi_integer, destination, myid+1, MPI_COMM_WORLD, variable_request(2), rc)    
  
  call mpi_waitall(2,variable_request, array_of_statuses, rc) !MPI_STATUSES_IGNORE 

  print*, "Collective operators!" 
  write(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Sender: ', myid, &
       ' Sent elements: ', size, &
       '. Tag: ', myid + 1, &
       '. Receiver: ', destination    
  ! Finalize measuring the time and print it out
  t1 = mpi_wtime()
  call mpi_barrier(mpi_comm_world, rc)
  call flush(6)

  write(*, '(A20, I3, A, F6.3)') 'Time elapsed in rank', myid, ':', t1-t0

  call mpi_finalize(rc)

end program basic

