!----------------------------------------------------------------------------
!
!  PROGRAM: Projection_main.f90
!
!  PURPOSE: Modules for Projection method
!
!                                                            2016.02.26 K.Noh
!
!   log 
!   2016.02.24 First make a module file and add file_name, path_name 
!   2016.03.02 Add the velocity and pressure arrays and initial velocity U0
!
!----------------------------------------------------------------------------
        MODULE projection_module
            
            INTEGER :: Nx, Ny 
            REAL(KIND=8) :: Re, dx, dy, Lx, Ly
            REAL(KIND=8) :: U0
            CHARACTER(LEN=65) :: file_name, path_name
 
            REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: U,V
            REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: P

            SAVE
            
        END MODULE projection_module
    