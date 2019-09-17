PROGRAM myfirstprogram
    IMPLICIT NONE ! no assumption about variables
    REAL :: x,y,z ! real values for x, y and z
    x = 7.9
    print*, 'the value of x is', x
    print*, 'enter another value of x'
    read*, x
    print*, 'please insert the value of y'
    read*, y
    z = x*y
    PRINT*, 'the value of z is', z
END PROGRAM MyFiRsTpRoGrAm
