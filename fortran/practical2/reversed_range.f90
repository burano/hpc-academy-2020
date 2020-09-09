program reversed_range
    implicit none
    integer :: array10(10), i
    integer :: lower_idx, upper_idx

    do i = 1, 10
        array10(i) = i
    end do

    print*, array10

    print*, "Enter lower and upper bounds: "
    read*, lower_idx, upper_idx
    
    if (array10(1) < lower_idx .or. (lower_idx < array10(10)) .or. (upper_idx < array10(10)) .or. (array10(1) < upper_idx)) then
        if (lower_idx < upper_idx) then
            print*, array10(lower_idx:upper_idx)
        else
            print*, array10(lower_idx:upper_idx:-1)
        end if
    else
        print*, "Indices are out of range\nPlease enter two numbers with in [1,10] range."    
    end if

end program reversed_range   