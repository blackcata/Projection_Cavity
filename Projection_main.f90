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
            
            IMPLICIT NONE
            INTEGER :: it, i, j, k
            REAL(KIND=8) :: pi
            
            pi = atan(1.0) * 4
            
            WRITE(*,*) pi
            
        END PROGRAM Projection_main

