   program HELLO
   USE MPI
   
   INTEGER :: rank, ierr
   INTEGER :: status(MPI_STATUS_SIZE)
   REAL, DIMENSION(10) :: a, b

   CALL MPI_INIT(ierror)
   CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)

   IF (rank .EQ. 0) THEN
      PRINT *,'node',rank,' HELLO_MPI - Send process:'
      CALL MPI_SEND(a(1), 10, MPI_REAL, 1, 0, MPI_COMM_WORLD, &
      ierr)
   ELSE IF (rank .EQ. 1) THEN
      PRINT *,'node',rank,' HELLO_MPI - Receive process:'
      CALL MPI_RECV(b(1), 10, MPI_REAL, 0, 0, MPI_COMM_WORLD, &
      status, ierr)
   END IF

END PROGRAM HELLO
