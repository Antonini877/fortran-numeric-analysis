
PROGRAM main
    IMPLICIT NONE
    REAL :: a, b, tol
    INTEGER :: maxIter

    a = 0.0
    b = 2.5
    tol = 1.0E-6
    maxIter = 100


    PRINT *, bisectionProcedure(a, b, tol, maxIter)

CONTAINS

    REAL FUNCTION bisectionProcedure(a, b, tol, maxIter)
        REAL, INTENT(INOUT) :: a, b
        REAL, INTENT(IN) :: tol
        INTEGER, INTENT(IN) :: maxIter
        INTEGER :: i
        REAL :: c, fa, fb, fc

        bisectionProcedure = 0.0

        IF (polynomialFunction(a) * polynomialFunction(b) > 0) THEN
            PRINT *, "The function doesn't change sign in the interval."
            RETURN
        END IF

        DO i = 1, maxIter
            c = (a + b) / 2
            fa = polynomialFunction(a)
            fb = polynomialFunction(b)
            fc = polynomialFunction(c)

            IF (ABS(fc) < tol) THEN
                bisectionProcedure = c
                RETURN
            END IF

            IF (fa * fc < 0) THEN
                b = c
            ELSE
                a = c
            END IF
        END DO

        PRINT *, "Bisection method did not converge within the specified number of iterations."
    END FUNCTION


    REAL FUNCTION polynomialFunction(x)
        REAL, INTENT(IN) :: x

        polynomialFunction =  x**3 - x**2 - 1


    END FUNCTION

END PROGRAM

