module rational
    implicit none
    type :: rational_number
        real(real64) :: r
    end type rational_number

contains

    function addition(number1, number2) result(sum)
        type(rational_number), intent(in) :: number1, number2
        type(rational_number) :: sum

        sum = number1 + number2

    end function addition 
    
    function subtraction(number1, number2) result(difference)
        type(rational_number), intent(in) :: number1, number2
        type(rational_number) :: difference

        difference = number1 - number2

    end function subtraction 

    function multiplication(number1, number2) result(product)

        product = number1%rational_number / number2%rational_number

    end function multiplication
    
    function division(number1, number2) result(dividend)
        if (number1%rational_number == 0.0 .or. number2%rational_number == 0.0) then
            dividend
        dividend = number1%rational_number / number2%rational_number

    end function divison


    TYPE rational
        REAL(real64) :: x                ! To avoid conflict with REAL
    END TYPE rational

    INTERFACE operator(+)
        MODULE PROCEDURE addition
    END INTERFACE

    INTERFACE operator(-)
        MODULE PROCEDURE subtraction
    END INTERFACE

    INTERFACE operator(*)
        MODULE PROCEDURE multiplication
    END INTERFACE

    INTERFACE operator(/)
        MODULE PROCEDURE division
    END INTERFACE


   ! ====================== Implementation part ===============

    CONTAINS   

    ! -----------------------
    ! Matrix * Vector
    ! -----------------------
    FUNCTION MatVecMult(A, v) result (w)
        implicit none

        TYPE(MyReal), dimension(:,:), INTENT(IN) :: A
        TYPE(MyReal), dimension(:), INTENT(IN) :: v
        TYPE(MyReal), dimension( SIZE(A,1) ) :: w

        integer :: j
        integer :: N

        N = SIZE(A,2)

        w(:).x = 0.0       !! clear whole vector
        DO j = 1, N
        w(:).x = w(:).x + v(j).x * A( :, j ).x
        END DO
    END FUNCTION
end module rational