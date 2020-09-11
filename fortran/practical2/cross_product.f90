program cross_product
    implicit none
    real :: vector1(3) = [1.0, 2.0, 3.0], vector2(3) = [4.0, 5.0, 6.0], vector_product(3)

    print*, "Vector 1: ", vector1
    print*, "Vector 2: ", vector2
    print*, "Perform cross product..."
    call multiply(vector1, vector2, vector_product)
    print*, " Vector product: ", vector_product
    
contains
    subroutine multiply(x, y, z)
        real, dimension(3) :: x, y, z
        z(1) = x(2) * y(3) - y(2) * x(3)
        z(2) = x(3) * y(1) - y(3) * x(1)
        z(3) = x(1) * y(2) - y(1) * x(2)
    end subroutine multiply
end program cross_product