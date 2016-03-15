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
                ONLY : Nx, Ny, TOTAL_IT, PRINT_NUM

            IMPLICIT NONE
            INTEGER :: it
            
            path_name = 'RESULT' 
            CALL SYSTEM('mkdir '//TRIM(path_name))
            
           ! Remove command - windows & unix
            CALL SYSTEM('del /q '//TRIM(path_name)//'\*')   ! Windows
           !CALL SYSTEM('rm -rf ./'//TRIM(path_name)//'/*') ! Unix
           
            CALL SETUP
            CALL OUTPUT(0)
            
            !---------- Main Loop ---------!
            
            DO it = 1, TOTAL_IT
                CALL CFL
                CALL RESI
                CALL GETUH
                CALL GETVH
                CALL SOR
                CALL UVNEW
                IF(mod(it,PRINT_NUM)==0) CALL OUTPUT(it)
            END DO
            
        END PROGRAM Projection_main

