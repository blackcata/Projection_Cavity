!-----------------------------------------------------------------------------------!
!  PROGRAM: Projection_main.f90
!
!  PURPOSE: To solve the cavity problem, projection method was used to 
!           solve the Navier-Stokes equation with Staggered grid. 
!    
!                                                                2016.03.02 K.Noh
!
!-----------------------------------------------------------------------------------!
        PROGRAM Projection_main

            USE projection_module,                                                &
                ONLY : file_name, path_name
            
            USE projection_module,                                                &
                ONLY : Nx, Ny, dx, dy, Lx, Ly
            
            USE projection_module,                                                &
                ONLY : U, V, P

            IMPLICIT NONE
            
            INTEGER :: it, i, j
            
            path_name = 'RESULT' 
            CALL SYSTEM('mkdir '//TRIM(path_name))
            CALL SETUP()
            
            OPEN(100,FILE=TRIM(path_name)//'/U.plt',FORM='FORMATTED',STATUS='REPLACE')
            Write(100,*) 'VARIABLES = "x","y","u"'
            Write(100,200) NX,Ny
            200 Format('Zone',2X,'I=',I3,2X,'J=',I3,2X,'F=Point')
            DO j = 1,Ny
                DO i = 1,Nx
                    WRITE(100,*) i*dx, j*dy, U(i,j)
                END DO
            END DO 
            CLOSE(100) 

            WRITE(*,*) Nx, dx, Lx
            WRITE(*,*) Ny, dy, Ly
            WRITE(*,*) U 
            
        END PROGRAM Projection_main

