!----------------------------------------------------------------------------
!
!  PROGRAM: Projection_main.f90
!
!  PURPOSE: Modules for Projection method
!
!   log 
!   2016.02.24 First make a module file and add file_name, path_name 
!
!----------------------------------------------------------------------------
        MODULE projection_module
            
            INTEGER :: Nx, Ny 
            REAL(KIND=8) :: Re, dx, dy, Lx, Ly
            CHARACTER(LEN=65) :: file_name, path_name
            
            SAVE
            
        END MODULE projection_module
    