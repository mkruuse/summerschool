
program datatype_struct
  use mpi
  implicit none
  type particle
     real :: coords(3)
     integer :: charge
     character(len=2) :: label
  end type particle
  integer, parameter :: n = 1000
  integer :: i, ierror,  myid,  ntasks, tag
  type(particle) :: particles(n)
  
  integer, parameter :: cnt=3
  integer:: particle_mpi_type, temp_type
  integer:: types(cnt),blocklen(cnt)
  integer(KIND=MPI_ADDRESS_KIND) :: disp(cnt)
  integer(KIND=MPI_ADDRESS_KIND) :: lb, extent
  real(8) :: t1,t2
  integer :: datatype_particles
  call MPI_INIT(ierror)
  call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierror)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, ntasks, ierror)
  
  ! insert some data for the particle struct
  if(myid == 0) then
    do i = 1, n
      call random_number(particles(i)%coords)
      particles(i)%charge = 54
      particles(i)%label = 'Xe'
    end do  
  end if
  
  ! TODO: define the datatype for type particle
  !mpi_type_create_struct(count-numb of blocks, blocklens-length of each block, displs-displacements of blocks in bytes (array), types-of blocks (array))
  types=(/mpi_real, mpi_integer, mpi_character/)
  blocklen=(/3,1,2/)
  call mpi_get_address(particles(1)%coords, disp(1), ierror)
  call mpi_get_address(particles(1)%charge, disp(2), ierror)
  call mpi_get_address(particles(1)%label, disp(3), ierror)
  do i=cnt-1, i
   disp(i)=disp(i)-disp(1)
  end do 
  
  call mpi_type_create_struct(3, blocklen, disp , types, datatype_particles, ierror)
  call mpi_type_commit(datatype_particles, ierror)

  call mpi_type_get_extent(particle_mpi_type, lb, extent, ierror)
  call mpi_get_address(particles(1), disp(1), ierror)
  call mpi_get_address(particles(2), disp(2), ierror)
  if(extent/=disp(2)-disp(1)) then
   temp_type=particle_mpi_type
   lb=0
   extent=disp(2)-disp(1)
   call mpi_type_create_resized(temp_type,lb,extent,particle_mpi_type,ierror)
   call mpi_type_commit(particle_mpi_type, ierror)
   call mpi_type_free(particle_mpi_type, ierror)
  end if 
   
  ! TODO: Check extent. 
  ! (Not really neccessary on most systems.)
  ! TODO: resize the particle_mpi_type if needed
  ! TODO: resize the particle_mpi_type if needed

  t1=MPI_WTIME()
  if(myid == 0) then
     do i=1,1000
        call MPI_SEND(particles, n, particle_mpi_type, 1, i, &
             MPI_COMM_WORLD,ierror)
     end do
  else if(myid == 1) then
     do i=1, 1000
        call MPI_RECV(particles, n, particle_mpi_type, 0, i, &
             MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
     end do
  end if
  t2=MPI_WTIME()
  
  write(*,*) "Time: ", myid, (t2-t1) / 1000d0
  write(*,*) "Check:", myid, particles(n)%coords(1)

  call MPI_TYPE_free(particle_mpi_type, ierror)  
  call MPI_FINALIZE(ierror)
    

end program datatype_struct
