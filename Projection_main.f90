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
                ONLY : U, V, P, Phi, Divergence, Vhat, Gradient_phi_y

            IMPLICIT NONE
            
            INTEGER :: it, i, j
            
            path_name = 'RESULT' 
            CALL SYSTEM('mkdir '//TRIM(path_name))
            CALL SETUP
            
            CALL SOR
            CALL Divergence(P)
            CALL Gradient_phi_y(Vhat)
            
            OPEN(100,FILE=TRIM(path_name)//'/P.plt',FORM='FORMATTED',STATUS='REPLACE')
            Write(100,*) 'VARIABLES = "x","y","dv","phi"'
            Write(100,200) NX,Ny-1
200         Format('Zone',2X,'I=',I3,2X,'J=',I3,2X,'F=Point')
            
            DO j = 1,Ny-1
                DO i = 1,Nx
                    WRITE(100,*) i*dx, j*dy, vhat(i,j), phi(i,j,1)
                END DO
            END DO 
            CLOSE(100) 
            
            WRITE(*,*) Nx, dx, Lx
            WRITE(*,*) Ny, dy, Ly
            !WRITE(*,*) U 
            
        END PROGRAM Projection_main

