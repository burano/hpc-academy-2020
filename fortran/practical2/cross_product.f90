program cross_product
    implicit none
    real :: vector1(3) = [1.0, 2.0, 3.0], vector2(3) = [4.0, 5.0, 6.0], vector_product(3)

    print*, "Vector 1: ", vector1
    print*, "Vector 2: ", vector2

    print*, "Call cross product subroutine..."
    call multiply_subroutine(vector1, vector2, vector_product)
    print*, "Vector product: ", vector_product

    print*, "Call cross product function..."
    print*, "Vector product: ", multiply_function(vector1, vector2)

contains
    subroutine multiply_subroutine(x, y, z)
        real, dimension(3), intent(in) :: x, y
        real, dimension(3), intent(out) :: z

        z(1) = x(2) * y(3) - y(2) * x(3)
        z(2) = x(3) * y(1) - y(3) * x(1)
        z(3) = x(1) * y(2) - y(1) * x(2)

    end subroutine multiply_subroutine

    function multiply_function(x, y) result(z)
        real, dimension(3), intent(in) :: x, y
        real, dimension(3) :: z

        z(1) = x(2) * y(3) - y(2) * x(3)
        z(2) = x(3) * y(1) - y(3) * x(1)
        z(3) = x(1) * y(2) - y(1) * x(2)

    end function multiply_function
end program cross_product