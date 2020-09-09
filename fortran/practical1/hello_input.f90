program hello_input
    implicit none
    character(len=20) :: name, surname
    print*, "How should I call you?"
    read*, name, surname
    print*, "Hello, ", name, " ", surname, "!"
end program hello_input