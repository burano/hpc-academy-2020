program hello_input
    implicit none
    character(len=20) :: name, age

    print*, "Reading form a file..."
    open(2, file = 'input.hello_input', status = 'old')
    read(2,*) name, age
    close(2)

    ! print*, "Enter your name and age:"
    ! read*, name, age
    ! print*, "Hello, ", name, " ", age, "!"

    print*, "Write to a file..."
    open(1, file = 'output.hello_input', status = 'new')   
    write(1,*) "Hello, ", name, " ", age, "!"
    close(1)



end program hello_input