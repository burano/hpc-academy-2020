   program hello
   USE MPI
   integer rank, size, ierror, tag, status(MPI_STATUS_SIZE)
   
   call MPI_INIT(ierror)
   call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)
   call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
   if (rank == 0) then
       print *,'node',rank,' HELLO_MPI - Master process:'
       print *,'node',rank,' Fortran version'
       print *,'node',rank,' The number of processes is: ',size
   end if
   print*, 'node', rank, ': Hello world'
   call MPI_FINALIZE(ierror)
   if (rank == 0) then
       print *,'node',rank,' finishing normally'
   end if
   end
