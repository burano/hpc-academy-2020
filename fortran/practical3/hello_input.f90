program hello_input
    implicit none
    character(len=20) :: name, age
    character(len=30) :: input_file, output_file
    
    print*, "Enter input file name:"
    read*, input_file
    print*, "Enter output file name:"
    read*, output_file

    print*, "Reading form a file..."
    open(2, file = trim(input_file), status = 'old')
    read(2,*) name, age
    close(2)

    ! print*, "Enter your name and age:"
    ! read*, name, age
    ! print*, "Hello, ", name, " ", age, "!"

    print*, "Write to a file..."
    open(1, file = trim(output_file), status = 'new')   
    write(1,*) "Hello, ", trim(name), " - ", trim(age), "."
    close(1)

end program hello_input