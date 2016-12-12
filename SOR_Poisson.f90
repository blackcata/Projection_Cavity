!-----------------------------------------------------------------------------------!
!
!  PROGRAM: SOR_Poisson.f90
!
!  PURPOSE: To solve the poisson equation by SOR Method
!
!                                                                2016.03.04 K.Noh
!
!   log
!   2016.03.04 Add SOR Solver of poisson equation and modified to optimize to
!              cavity problem and this projection code
!   2016.03.08 Use Divergence subroutine and call divergence subroutine
!   2016.03.11 Modified the format of printing consuming time of SOR
!
!-----------------------------------------------------------------------------------!

        SUBROUTINE SOR(ierr)

            USE projection_module,                                              &
              ONLY : Nx, Ny, dx, dy, dt, tol, omega, ITMAX, MYMPI,              &
                     mpi_xsize, mpi_ysize

            USE projection_module,                                              &
                ONLY : Phi, DIVERGENCE

            IMPLICIT NONE
            INCLUDE 'mpif.h'

            INTEGER,INTENT(IN) :: ierr
            INTEGER :: i, j, it, ista,iend,jsta,jend
            REAL(KIND=8) :: beta, rms, t1, t2, SUM1, SUM2, SUM1_loc, SUM2_loc
            REAL(KIND=8),DIMENSION(:,:),ALLOCATABLE :: b, phi_new, phi_loc
            TYPE(MYMPI) :: mpi_info

            SUM1 = 0.0
            SUM2 = 0.0
            beta = dx/dy

            !------------------------------------------------------------------!
            !                            MPI Setting                           !
            !------------------------------------------------------------------!
            CALL MPI_COMM_SIZE(MPI_COMM_WORLD,mpi_info%nprocs,ierr)
            CALL MPI_COMM_RANK(MPI_COMM_WORLD,mpi_info%myrank,ierr)
            CALL MPI_SETUP(mpi_xsize,mpi_ysize,mpi_info)

            ista = mpi_info.mpirank_x*mpi_info.nx_mpi+1;
            iend = ista + mpi_info.nx_mpi-1;
            jsta = mpi_info.mpirank_y*mpi_info.ny_mpi+1;
            jend = jsta + mpi_info.ny_mpi-1;

            WRITE(*,"(A,I2)")"Myrank : ",mpi_info%myrank
            WRITE(*,"(2(A,I2))")"mpiank_x : ",mpi_info%mpirank_x, ", mpirank_y : ",mpi_info%mpirank_y
            WRITE(*,"(2(A,I2))")"mpisize_x : ",mpi_info%mpisize_x,", mpisize_y : ",mpi_info%mpisize_y
            WRITE(*,"(4(A,I2))")"e_rank : ",mpi_info%rank_sur(0),", w_rank : ",mpi_info%rank_sur(1),&
                   " ,s_rank : ",mpi_info%rank_sur(2),", n_rank : ",mpi_info%rank_sur(3)
            WRITE(*,"(A,4(I3,A))")"(",ista,",",iend,") X (",jsta,",",jend,")"
            WRITE(*,*)""
            CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

            IF( mpi_info%myrank == 0 )THEN
              WRITE(*,*) '-------------------------------------------------------'
              WRITE(*,*) '                  SOR PROCESS STARTED                  '
            END IF

            !------------------------------------------------------------------!
            !                        Memory Allocation                         !
            !------------------------------------------------------------------!
            ALLOCATE( phi_loc(0:mpi_info%nx_mpi+1,0:mpi_info%ny_mpi+1) )
            ALLOCATE( phi_new(0:mpi_info%nx_mpi+1,0:mpi_info%ny_mpi+1) )
            ALLOCATE( b(1:mpi_info%nx_mpi,1:mpi_info%ny_mpi) )

            b(1:mpi_info%nx_mpi,1:mpi_info%ny_mpi)           = 0.0
            phi_loc(0:mpi_info%nx_mpi+1,0:mpi_info%ny_mpi+1) = 0.0
            phi_new(0:mpi_info%nx_mpi+1,0:mpi_info%ny_mpi+1) = 0.0

            DO j = jsta,jend
              DO i = ista,iend
                phi_loc(i-ista+1,j-jsta+1) = Phi(i,j)
              END DO
            END DO

            ! Divergence term has to be modified to be parallelized
            ! CALL DIVERGENCE(b)
            CALL CPU_TIME(t1)

            !------------------------------------------------------------------!
            !                      Main Loop of SOR method                     !
            !------------------------------------------------------------------!
            DO it=1,ITMAX
              SUM1_loc = 0.0
              SUM2_loc = 0.0

              !----------------------------------------------------------------!
              !                           Update red nodes                     !
              !----------------------------------------------------------------!
              CALL SEND_NORTH(ierr,phi_loc,mpi_info%nx_mpi,mpi_info%ny_mpi,mpi_info)
              CALL SEND_SOUTH(ierr,phi_loc,mpi_info%nx_mpi,mpi_info%ny_mpi,mpi_info)
              CALL SEND_WEST(ierr, phi_loc,mpi_info%nx_mpi,mpi_info%ny_mpi,mpi_info)
              CALL SEND_EAST(ierr, phi_loc,mpi_info%nx_mpi,mpi_info%ny_mpi,mpi_info)

              DO j=1,mpi_info%ny_mpi
                DO i=1,mpi_info%nx_mpi
                    IF((mod(i+j,2))==0) THEN
                      phi_new(i,j) = ( phi_loc(i+1,j)+phi_loc(i-1,j)            &
                                      + beta**2*(phi_loc(i,j+1)+phi_loc(i,j-1)) &
                                      - dx*dx*b(i,j)/dt ) / (2*(1+beta**2))
                      phi_new(i,j) = phi_loc(i,j)                               &
                                   + omega*(phi_new(i,j) - phi_loc(i,j))
                    END IF
                END DO
              END DO

              !----------------------------------------------------------------!
              !                       Boundary Conditions                      !
              !----------------------------------------------------------------!
              IF (ista == 1)    phi_new(1,1:mpi_info%ny_mpi)             = 0.0
              IF (iend == Nx) phi_new(mpi_info%nx_mpi,1:mpi_info%ny_mpi) = 0.0
              IF (jsta == 1)    phi_new(1:mpi_info%nx_mpi,1)             = 1.0
              IF (jend == Ny) phi_new(1:mpi_info%nx_mpi,mpi_info%ny_mpi) = 0.0

              !----------------------------------------------------------------!
              !                         Update black nodes                     !
              !----------------------------------------------------------------!
              CALL SEND_NORTH(ierr,phi_new,mpi_info%nx_mpi,mpi_info%ny_mpi,mpi_info)
              CALL SEND_SOUTH(ierr,phi_new,mpi_info%nx_mpi,mpi_info%ny_mpi,mpi_info)
              CALL SEND_WEST(ierr, phi_new,mpi_info%nx_mpi,mpi_info%ny_mpi,mpi_info)
              CALL SEND_EAST(ierr, phi_new,mpi_info%nx_mpi,mpi_info%ny_mpi,mpi_info)

              DO j=1,mpi_info%ny_mpi
                DO i=1,mpi_info%nx_mpi
                    IF((mod(i+j,2))==1) THEN
                      phi_new(i,j) = ( phi_new(i+1,j)+phi_new(i-1,j)            &
                                      + beta**2*(phi_new(i,j+1)+phi_new(i,j-1)) &
                                      - dx*dx*b(i,j)/dt ) / (2*(1+beta**2))
                      phi_new(i,j) = phi_loc(i,j)                               &
                                   + omega*(phi_new(i,j) - phi_loc(i,j))
                    END IF
                END DO
              END DO

              !----------------------------------------------------------------!
              !                       Boundary Conditions                      !
              !----------------------------------------------------------------!
              IF (ista == 1)    phi_new(1,1:mpi_info%ny_mpi)             = 0.0
              IF (iend == Nx) phi_new(mpi_info%nx_mpi,1:mpi_info%ny_mpi) = 0.0
              IF (jsta == 1)    phi_new(1:mpi_info%nx_mpi,1)             = 1.0
              IF (jend == Ny) phi_new(1:mpi_info%nx_mpi,mpi_info%ny_mpi) = 0.0

              !----------------------------------------------------------------!
              !                       Convergence Criteria                     !
              !----------------------------------------------------------------!
              DO j = 2,mpi_info%ny_mpi-1
                DO i = 2,mpi_info%nx_mpi-1
                  SUM1_loc = SUM1_loc + abs(phi_new(i,j))
                  SUM2_loc = SUM2_loc + abs( phi_new(i+1,j)+phi_new(i-1,j)      &
                                  + beta**2*(phi_new(i,j+1)+phi_new(i,j-1))     &
                                  -(2+2*beta**2)*phi_new(i,j)- dx*dx*b(i,j)/dt )
                END DO
              END DO
              CALL MPI_ALLREDUCE(SUM1_loc,SUM1,1,MPI_DOUBLE_PRECISION,          &
                                                 MPI_SUM,MPI_COMM_WORLD,ierr)
              CALL MPI_ALLREDUCE(SUM2_loc,SUM2,1,MPI_DOUBLE_PRECISION,          &
                                                 MPI_SUM,MPI_COMM_WORLD,ierr)
              IF (mpi_info%myrank==0) WRITE(*,"(I5,2X,3(F15.10,2X))") it, SUM2/SUM1, tol
              IF ( SUM2/SUM1 < tol ) EXIT

              !----------------------------------------------------------------!
              !                               Update                           !
              !----------------------------------------------------------------!
              phi_loc(1:mpi_info%nx_mpi,1:mpi_info%nx_mpi) =                    &
                                    phi_new(1:mpi_info%nx_mpi,1:mpi_info%nx_mpi)

            END DO

            DEALLOCATE(b,phi_loc,phi_new)
            CALL CPU_TIME(t2)

            IF ( mpi_info%myrank == 0 ) THEN
              WRITE(*,*) '                   SOR PROCESS ENDED                   '
              WRITE(*,FMT='(A,I5,A,F10.7,A)')                                     &
                  'Total Iteration : ',it,', total time for SOR : ',t2-t1,'s'
              WRITE(*,*) '-------------------------------------------------------'
              WRITE(*,*) ''
            END IF

        END SUBROUTINE SOR
