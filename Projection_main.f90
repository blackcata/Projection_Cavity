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
            
            path_name = 'RESULT' 
            CALL SYSTEM('mkdir '//TRIM(path_name))
            CALL SETUP
            CALL RESI

            OPEN(100,FILE=TRIM(path_name)//'/TEST.plt',FORM='FORMATTED',STATUS='REPLACE')
            CLOSE(100) 

            
        END PROGRAM Projection_main

