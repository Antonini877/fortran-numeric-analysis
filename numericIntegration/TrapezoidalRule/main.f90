PROGRAM main
    IMPLICIT NONE

    REAL :: a, b, h, result
    INTEGER :: n, i

    ! Define integration limits
    a = 0.0
    b = 1.0

    ! Number of intervals
    n = 1000

    ! Calculate step size
    h = (b - a) / REAL(n)

    ! Perform integration using the trapezoidal rule
    result = 0.5 * (f(a) + f(b))

    DO i = 1, n - 1
        result = result + f(a + i * h)
    END DO

    result = result * h

    ! Display the result
    PRINT *, "Result:", result

CONTAINS

    ! Function to be integrated
    REAL FUNCTION f(x)
        REAL, INTENT(IN) :: x
        f = x**2
    END FUNCTION

END PROGRAM main
