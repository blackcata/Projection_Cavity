!-----------------------------------------------------------------------------------!
!
!  PROGRAM: Projection_output.f90
!
!  PURPOSE: Make output files of each variables in the result folder.
!
!                                                                2016.03.09 K.Noh
!   log
!   2016.03.09 Add  output system file, this file'll make each variables for each
!              files and it will trace each variables status
!   2016.03.09 Add U,V, Uhat, Vhat, Phi, Hx, Hy, Lx, Ly, Rx, Ry output system
!   2016.03.09 Change the total number of x-grid and y-grid in V.Plt file
!   2016.03.15 Add 2 FORMAT to trace the time-series of each variables
!
!-----------------------------------------------------------------------------------!

        SUBROUTINE OUTPUT(TS)

            USE projection_module,                                                &
                ONLY : file_name, path_name

            USE projection_module,                                                &
                ONLY : Nx, Ny, dx, dy

            USE projection_module,                                                &
                ONLY : U, V, Uhat, Vhat, UNew, VNew, Phi,                         &
                       Rx, Ry, Hx, Hy, Lx, Ly

            IMPLICIT NONE
            INTEGER :: i,j
            INTEGER, INTENT(IN) :: TS


200         FORMAT('I=',I3,2X,'J=',I3,2X,'F=POINT')
300         FORMAT('STRANDID=1, SOLUTIONTIME=',I5)
400         FORMAT('ZONE T= "Timestep ',I5,'"')

            file_name = TRIM(path_name)//'/U.plt'
            OPEN(100,FILE=file_name,FORM='FORMATTED',POSITION='APPEND')
            WRITE(100,*) 'VARIABLES="x","y","U"'
            WRITE(100,400) TS
            WRITE(100,300) TS
            WRITE(100,200) Nx+1, Ny+2
            DO j = 0, Ny+1
                DO i = 0, Nx
                    WRITE(100,*) i*dx, j*dy, U(i,j)
                END DO
            END DO
            CLOSE(100)

            file_name = TRIM(path_name)//'/V.plt'
            OPEN(100,FILE=file_name,FORM='FORMATTED',POSITION='APPEND')
            WRITE(100,*)'VARIABLES="x","y","V"'
            WRITE(100,400) TS
            WRITE(100,300) TS
            WRITE(100,200) Nx+2, Ny+1
            DO j = 0, Ny
                DO i = 0, Nx+1
                    WRITE(100,*) i*dx, j*dy, V(i,j)
                END DO
            END DO
            CLOSE(100)

            file_name = TRIM(path_name)//'/Uhat.plt'
            OPEN(100,FILE=file_name,FORM='FORMATTED',POSITION='APPEND')
            WRITE(100,*)'VARIABLES="x","y","Uhat"'
            WRITE(100,400) TS
            WRITE(100,300) TS
            WRITE(100,200) Nx-1, Ny
            DO j = 1, Ny
                DO i = 1, Nx-1
                    WRITE(100,*) i*dx, j*dy, Uhat(i,j)
                END DO
            END DO
            CLOSE(100)

            file_name = TRIM(path_name)//'/Vhat.plt'
            OPEN(100,FILE=file_name,FORM='FORMATTED',POSITION='APPEND')
            WRITE(100,*)'VARIABLES="x","y","Vhat"'
            WRITE(100,400) TS
            WRITE(100,300) TS
            WRITE(100,200) Nx, Ny-1
            DO j = 1, Ny-1
                DO i = 1, Nx
                    WRITE(100,*) i*dx, j*dy, Vhat(i,j)
                END DO
            END DO
            CLOSE(100)

            file_name = TRIM(path_name)//'/Resi_x.plt'
            OPEN(100,FILE=file_name,FORM='FORMATTED',POSITION='APPEND')
            WRITE(100,*)'VARIABLES="x","y","Hx","Lx","Rx"'
            WRITE(100,400) TS
            WRITE(100,300) TS
            WRITE(100,200) Nx-1, Ny
            DO j = 1, Ny
                DO i = 1, Nx-1
                    WRITE(100,*) i*dx, j*dy, Hx(i,j), Lx(i,j), Rx(i,j)
                END DO
            END DO
            CLOSE(100)

            file_name = TRIM(path_name)//'/Resi_y.plt'
            OPEN(100,FILE=file_name,FORM='FORMATTED',POSITION='APPEND')
            WRITE(100,*)'VARIABLES="x","y","Hy","Ly","Ry"'
            WRITE(100,400) TS
            WRITE(100,300) TS
            WRITE(100,200) Nx, Ny-1
            DO j = 1, Ny-1
                DO i = 1, Nx
                    WRITE(100,*) i*dx, j*dy, Hy(i,j), Ly(i,j), Ry(i,j)
                END DO
            END DO
            CLOSE(100)

            file_name = TRIM(path_name)//'/Phi.plt'
            OPEN(100,FILE=file_name,FORM='FORMATTED',POSITION='APPEND')
            WRITE(100,*)'VARIABLES="x","y","Phi"'
            WRITE(100,400) TS
            WRITE(100,300) TS
            WRITE(100,200) Nx, Ny
            DO j = 1, Ny
                DO i = 1, Nx
                    WRITE(100,*) i*dx, j*dy, Phi(i,j)
                END DO
            END DO
            CLOSE(100)

        END SUBROUTINE OUTPUT
