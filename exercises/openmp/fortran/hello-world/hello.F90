program hello
  use omp_lib
  use mpi
  implicit none
  integer :: active_threads
  integer :: thread_num
  
  print*, "Hello World!"
  !$OMP PARALLEL SHARED(thread_num) PRIVATE(active_threads)
  thread_num=omp_get_thread_num()
  !$OMP SINGLE
  active_threads=omp_get_num_threads()
  !$OMP END SINGLE
  !$OMP CRITICAL 
  print*, "The thread number:", thread_num
  !$OMP END CRITICAL
  print *, 'X'
  !$OMP END PARALLEL
  print*, "The number of threads", active_threads

end program hello
