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
                ONLY : Nx, Ny, dx, dy

            IMPLICIT NONE
            INTEGER :: it
            
            path_name = 'RESULT' 
            CALL SYSTEM('mkdir '//TRIM(path_name))

            CALL SETUP
            
            DO it = 1, 1
                CALL RESI
                CALL GETUH
                CALL GETVH
                CALL SOR
                CALL UVNEW
                CALL OUTPUT
            END DO
            
        END PROGRAM Projection_main

