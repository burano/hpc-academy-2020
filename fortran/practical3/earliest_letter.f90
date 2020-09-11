program earliest_letter
    implicit none
    character(len=20) :: input_word
    
    print*, "Enter a word in lowercase:"
    read*, input_word

    print*, "Earliest letter: ", earliest(input_word)
    contains
    function earliest(word) result(c)
        character(len=20), intent(in) :: word
        character(len=1) :: c
        integer :: i
        
        c = word(1)
        do i= 2, len(word)
            if (word(i) < c) then
                c = word(i)
            end if
        end do
    end function earliest

end program earliest_letter