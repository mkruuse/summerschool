program hello
  use mpi
  implicit none
  integer :: rank, rc, total_ranks
  call mpi_init(rc)
  call mpi_comm_rank(MPI_COMM_WORLD, rank, rc)
  call mpi_comm_size(MPI_COMM_WORLD, total_ranks, rc)
  print*, "Hello World!", rank
  if(rank==0) print*, "Total number of processes", total_ranks
  call mpi_finalize(rc)
end program hello
