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

    print*, array10(lower_idx:upper_idx)
end program reversed_range   