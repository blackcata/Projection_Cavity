!----------------------------------------------------------------------------
!
!  PROGRAM: Projection_main.f90
!
!  PURPOSE: To solve the cavity problem, projection method was used to 
!           solve the Navier-Stokes equation. 
!    
!                                                            2016.02.24 K.Noh
!
!----------------------------------------------------------------------------
    
        PROGRAM Projection_main

            USE projection_module,                                           &
                ONLY : file_name, path_name
            
            USE projection_module,                                           &
                ONLY : Nx, Ny, dx, dy, Lx, Ly
            
            USE projection_module,                                           &
                ONLY : U, V, P
            
            IMPLICIT NONE
            INTEGER :: it, i, j, k
            
            path_name = 'RESULT' 
            CALL SYSTEM('mkdir '//TRIM(path_name))
            
            OPEN(100,FILE=TRIM(path_name)//'/Test.plt',FORM='FORMATTED',STATUS='REPLACE')
            WRITE(100,*) 'Test'
            CLOSE(100) 
            
            CALL SETUP()
            
            WRITE(*,*) Nx, dx, Lx
            WRITE(*,*) Ny, dy, Ly
            WRITE(*,*) U 
            
        END PROGRAM Projection_main

