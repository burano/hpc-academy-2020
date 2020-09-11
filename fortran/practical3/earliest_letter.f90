program earliest_letter
    implicit none
    character(len=:) :: input_word
    
    print*, "Enter a word in lowercase:"
    read*, input_word

    contains
    function earliest(word) result(c)
        character(len=:), intent(in) :: word
        character(len=1) :: c
        integer :: i
        
        c = word(1)
        do i= 2, len(word)
            if (word(i) < c) then
                c = word(i)
            end if
        end do
    end function earliest

    print*, "Earliest letter: ", earliest(trim(input_word))

end program earliest_letter