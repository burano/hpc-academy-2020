program factorial
    implicit none
    integer :: n, i, fact = 1

    print*, "Please, enter factorial to calculate:"
    read*, n

    if (n < 0) then
        print*, "n must be => 0."
        call exit()
    end if

    if (n/=0) then
        do i = 1, n
            fact = fact * i
        end do
    end if
    
    print*, n, "! = ", fact

    recursive function fact_recursive(n) result(res)
        integer, intent(in) :: n
        integer :: res
        if ((n == 1) .or. (n == 2)) then
            res = 1
            return
        end if
        res = fact_recursive(n - 1) * fact_recursive(n - 2)
    end function factorial
end program factorial