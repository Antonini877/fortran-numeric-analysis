PROGRAM main
    IMPLICIT NONE

    REAL, DIMENSION(5) :: xValues, yValues
    REAL :: x, y

    x = 2.7

    xValues = [1.1, 1.4, 1.7, 2.23, 2.7]
    yValues = [32.1, 45.2, 45.3, 49.7, 57.1]

    y = interpolate(xValues, yValues, x)

    PRINT *, y, " y value for x", x

CONTAINS

    REAL FUNCTION interpolate(xValues, yValues, x)
        REAL, DIMENSION(5), INTENT(IN) :: xValues, yValues
        REAL :: maxX, x, x0, x1, y0, y1, y
        INTEGER :: maxXIndex, i

        maxX = xValues(1)
        maxXIndex = 0

        DO i = 1, 5
            IF (xValues(i) > maxX .AND. xValues(i) < x) THEN
                maxXIndex = i
                maxX = xValues(i)
            END IF
        END DO

        x0 = xValues(maxXIndex)
        x1 = xValues(maxXIndex + 1)

        y0 = yValues(maxXIndex)
        y1 = yValues(maxXIndex + 1)

        y = y0 + (y1 - y0) * (x - x0) / (x1 - x0)

        interpolate = y


    END FUNCTION

END PROGRAM
