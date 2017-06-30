program exer1
  use omp_lib
  implicit none
  integer :: var1, var2
  integer :: omp_rank
  var1 = 1
  var2 = 2

  ! TODO:
  !   Test different data sharing clauses here
  !$omp parallel private(omp_rank),shared(var1, var2)
!  var1=1 !if private inside parallel
!  var2=2 
  omp_rank=omp_get_thread_num()
  print *, 'Region 1:       var1=', var1, 'var2=', var2
  var1 = var1 + 1
  var2 = var2 + 1
  print*, "thread", omp_rank
  !$omp end parallel
  !end here :)
  print *, 'After region 1: var1=', var1, 'var2=', var2
  print *

end program exer1
