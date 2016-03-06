!-----------------------------------------------------------------------------------!
!
!  PROGRAM: Projection_main.f90
!
!  PURPOSE: To solve the cavity problem, projection method was used to 
!           solve the Navier-Stokes equation with Staggered grid. 
!    
!                                                                2016.02.24 K.Noh
!
!-----------------------------------------------------------------------------------!
    
        PROGRAM Projection_main

            USE projection_module,                                                &
                ONLY : file_name, path_name
            
            USE projection_module,                                                &
                ONLY : Nx, Ny, dx, dy, U,V, Rx, Ry, Hx, Hy, Lx, Ly

            IMPLICIT NONE
            INTEGER :: i,j

            path_name = 'RESULT' 
            CALL SYSTEM('mkdir '//TRIM(path_name))
            CALL SETUP
            CALL RESI

            OPEN(100,FILE=TRIM(path_name)//'/TEST.plt',FORM='FORMATTED',STATUS='REPLACE')
            WRITE(100,*) 'VARIABLES = "x","y","Rx","Hx","Lx"'
            WRITE(100,200) Nx-1, Ny
            200 FORMAT('ZONE',2X,'I=',I3,2X,'J=',I3,2X,'F=Point')

            DO j = 1, Ny
              DO i = 1, Nx-1
                WRITE(100,*) i*dx, j*dy, Rx(i,j), Hx(i,j), Lx(i,j)
              END DO
            END DO
            CLOSE(100) 

            
        END PROGRAM Projection_main

