!-----------------------------------------------------------------------------------!
!
!  PROGRAM: Projection_module.f90
!
!  PURPOSE: Modules for Projection method
!
!                                                                2016.02.24 K.Noh
!
!   log
!   2016.02.24 First make a module file and add file_name, path_name
!   2016.03.02 Add the velocity and pressure arrays and initial velocity U0
!   2016.03.03 Add the phi variable and tollerance and omega
!   2016.03.04 Add Divergence and Gradient subroutines in the moudule
!   2016.03.06 Add the residual & laplace x and y term and dt
!   2016.03.08 Combined two gradient_phi_x,y subroutines to one subroutine
!   2016.03.08 Modified divergence subroutine u -> uhat, v -> vhat
!   2016.03.09 Modified Divergence subroutines index and boundary condition
!   2016.03.15 Add the printing number, total iteration integer variable
!
!-----------------------------------------------------------------------------------!

        MODULE projection_module
            IMPLICIT NONE

            INTEGER :: Nx, Ny, PRINT_NUM, TOTAL_IT, ITMAX, mpi_xsize, mpi_ysize
            REAL(KIND=8) :: Re, dt, dx, dy, ALx, ALy
            REAL(KIND=8) :: U0, tol, omega
            CHARACTER(LEN=65) :: file_name, path_name

            REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE   :: U,V, Uhat, Vhat, UNew, VNew
            REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE   :: P,VOR,Stream,Phi
            REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE   :: Rx, Ry, Hx, Hy, Lx, Ly

            TYPE :: MYMPI
              INTEGER :: nprocs, myrank, nx_mpi, ny_mpi
              INTEGER :: mpisize_x, mpisize_y, mpirank_x, mpirank_y
              INTEGER,DIMENSION(0:3) :: rank_sur
            END TYPE MYMPI

            SAVE

        CONTAINS

            SUBROUTINE MPI_SETUP(mpi_xsize,mpi_ysize,mpi_info)
                IMPLICIT NONE
                INTEGER :: mpi_xsize,mpi_ysize
                TYPE(MYMPI) :: mpi_info

                mpi_info%mpisize_x = mpi_xsize
                mpi_info%mpisize_y = mpi_ysize
                mpi_info%nx_mpi    = Nx/mpi_xsize
                mpi_info%ny_mpi    = Ny/mpi_ysize
                mpi_info%mpirank_x = mpi_info%myrank/mpi_ysize
                mpi_info%mpirank_y = mod(mpi_info%myrank,mpi_ysize)

                IF (mpi_info%mpirank_x == 0) THEN
                   mpi_info%rank_sur(0) = -1;
                ELSE
                   mpi_info%rank_sur(0) = mpi_info%myrank - mpi_info%mpisize_y;
                END IF

                IF (mpi_info%mpirank_x == mpi_info%mpisize_x-1) THEN
                    mpi_info%rank_sur(1) = -1;
                ELSE
                  mpi_info%rank_sur(1) = mpi_info%myrank + mpi_info%mpisize_y;
                END IF

                IF (mpi_info%mpirank_y == 0) THEN
                   mpi_info%rank_sur(2) = -1;
                ELSE
                  mpi_info%rank_sur(2) = mpi_info%myrank - 1;
                END IF

                IF (mpi_info%mpirank_y == mpi_info%mpisize_y-1) THEN
                  mpi_info%rank_sur(3) = -1;
                ELSE
                  mpi_info%rank_sur(3) = mpi_info%myrank + 1;
                END IF

            END SUBROUTINE

            SUBROUTINE DIVERGENCE(b)

                IMPLICIT NONE
                INTEGER :: i,j
                REAL(KIND=8) :: b(1:Nx,1:Ny), duhat, dvhat

                DO j = 1,Ny
                    DO i = 1,Nx
                        IF (i==1) THEN
                            duhat = Uhat(i,j)
                        ELSEIF (i==Nx) THEN
                            duhat = -Uhat(i-1,j)
                        ELSE
                            duhat = Uhat(i,j) - Uhat(i-1,j)
                        END IF

                        IF (j==1) THEN
                            dvhat = Vhat(i,j)
                        ELSEIF (j==NY) THEN
                            dvhat = -Vhat(i,j-1)
                        ELSE
                            dvhat = Vhat(i,j) - Vhat(i,j-1)
                        END IF

                        b(i,j) = duhat / dx + dvhat / dy
                    END DO
                END DO

            END SUBROUTINE DIVERGENCE

            SUBROUTINE GRADIENT(gx,gy)

                IMPLICIT NONE
                INTEGER :: i,j
                REAL(KIND=8) :: gx(1:Nx-1,1:Ny), gy(1:Nx,1:Ny-1)

                DO j = 1,Ny
                    DO i = 1, Nx-1
                        gx(i,j) = (Phi(i+1,j) - Phi(i,j))/dx
                    END DO
                END DO

                DO j = 1,Ny-1
                    DO i = 1, Nx
                        gy(i,j) = (Phi(i,j+1) - Phi(i,j))/dy
                    END DO
                END DO

            END SUBROUTINE

        END MODULE projection_module
