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

            USE projection_module,                                              &
                ONLY : file_name, path_name

            USE projection_module,                                              &
                ONLY : Nx, Ny, TOTAL_IT, PRINT_NUM

            IMPLICIT NONE
            INCLUDE 'mpif.h'

            INTEGER :: it, ierr, myrank

            CALL MPI_INIT(ierr)
            CALL MPI_COMM_RANK(MPI_COMM_WORLD,myrank,ierr)

            IF (myrank == 0) THEN
              path_name = 'RESULT'
              CALL SYSTEM('mkdir '//TRIM(path_name))
              CALL SYSTEM('rm -rf ./'//TRIM(path_name)//'/*')
            END IF

            CALL SETUP
            IF( myrank==0 ) CALL OUTPUT(0)
            CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

            !---------- Main Loop ---------!

            ! DO it = 1, TOTAL_IT
            !     CALL CFL
            !     CALL RESI
            !     CALL GETUH
            !     CALL GETVH
                CALL SOR(ierr)
                ! CALL UVNEW
                it = 20
                IF(mod(it,PRINT_NUM)==0 .AND. myrank==0) CALL OUTPUT(it)
            ! END DO

            CALL MPI_FINALIZE(ierr)
        END PROGRAM Projection_main
