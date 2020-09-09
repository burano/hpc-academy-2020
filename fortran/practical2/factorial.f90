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

    print*, ""
    print*, "Recursive factorial solution"
    print*, n, "! = ", fact_recursive(n)

    recursive function fact_recursive(a) result(res)
        integer, intent(in) :: a
        integer :: res
        if ((a == 0) .or. (a == 1)) then
            res = 1
            return
        end if
        res = a * fact_recursive(a - 1)
    end function fact_recursive
end program factorial