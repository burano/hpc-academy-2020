program reversed_range
    implicit none
    integer, allocatable :: array(:)
    integer :: size, i, lower_idx, upper_idx

    print*, "Enter size of the array:"
    read*, size
    if (size <= 0) then
        print*, "Size must be greater than or equal to 1."
        call exit()
    end if

    allocate(array(size))
    do i = 1, size
        array(i) = i
    end do

    print*, array

    print*, "Enter lower and upper bounds: "
    read*, lower_idx, upper_idx
    
    if (array(1) < lower_idx .or. (lower_idx < array(10)) .or. (upper_idx < array(10)) .or. (array(1) < upper_idx)) then
        if (lower_idx < upper_idx) then
            print*, array(lower_idx:upper_idx)
        else
            print*, array(lower_idx:upper_idx:-1)
        end if
    else
        print*, "Indices are out of range\nPlease enter two numbers with in [1,10] range."
        call exit()    
    end if

end program reversed_range   