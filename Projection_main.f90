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
            
            IMPLICIT NONE
            INTEGER :: it, i, j, k
            REAL(KIND=8) :: pi
            
            pi = atan(1.0) * 4
            
            path_name = 'RESULT' 
            CALL SYSTEM('mkdir '//TRIM(path_name))
            
            OPEN(100,FILE=TRIM(path_name)//'/Test.plt',FORM='FORMATTED',STATUS='REPLACE')
            WRITE(100,*) 'Test'
            CLOSE(100) 
            
            Nx = 100
            Ny = 100
            
            Lx = 1.0
            Ly = 1.0
            
            dx = Lx / (Nx+1)
            dy = Ly / (Ny+1)
            
            WRITE(*,*) Nx, dx, Lx
            WRITE(*,*) Ny, dy, Ly
            
        END PROGRAM Projection_main

