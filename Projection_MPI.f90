!-------------------------------------------------------------------------------!
!
!  PROGRAM: Projection_MPI.f90
!
!  PURPOSE: MPI setting and Communication subroutines for Projection method
!
!                                                                2016.12.12 K.Noh
!
!-------------------------------------------------------------------------------!

!-------------------------------------------------------------------------------!
!                              MPI Setup function                               !
!-------------------------------------------------------------------------------!
        SUBROUTINE MPI_SETUP(mpi_xsize,mpi_ysize,mpi_info)
            USE projection_module,                                              &
              ONLY : Nx, Ny, MYMPI

            IMPLICIT NONE
            INTEGER,INTENT(IN)        :: mpi_xsize, mpi_ysize
            TYPE(MYMPI),INTENT(INOUT) :: mpi_info

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

!-------------------------------------------------------------------------------!
!                        Communication subroutines                              !
!-------------------------------------------------------------------------------!
        SUBROUTINE SEND_NORTH(ierr,Phi,nx_mpi,ny_mpi,mpi_info)
            USE projection_module,                                              &
              ONLY : MYMPI

            IMPLICIT NONE
            INCLUDE 'mpif.h'
            TYPE(MYMPI),INTENT(IN) :: mpi_info

            INTEGER :: req1, req2,                                               &
                       status1(MPI_STATUS_SIZE), status2(MPI_STATUS_SIZE)
            INTEGER,INTENT(IN) :: ierr, nx_mpi, ny_mpi
            REAL(KIND=8),INTENT(INOUT) :: Phi(0:nx_mpi+1,0:ny_mpi+1)
            REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: sendbuf, recvbuf

            ALLOCATE( sendbuf(1:nx_mpi), recvbuf(1:nx_mpi) )

            sendbuf(1:nx_mpi) = Phi(1:nx_mpi,ny_mpi)

            CALL MPI_ISEND(sendbuf,nx_mpi,MPI_DOUBLE_PRECISION,                 &
                           mpi_info%rank_sur(3),101,MPI_COMM_WORLD,req1,ierr)
            CALL MPI_IRECV(recvbuf,nx_mpi,MPI_DOUBLE_PRECISION,                 &
                           mpi_info%rank_sur(2),101,MPI_COMM_WORLD,req2,ierr)
            CALL MPI_WAIT(req1,status1,ierr)
            CALL MPI_WAIT(req2,status2,ierr)

            Phi(1:nx_mpi,0) = recvbuf(1:nx_mpi)

            DEALLOCATE( sendbuf, recvbuf )

        END SUBROUTINE

        SUBROUTINE SEND_SOUTH(ierr,Phi,nx_mpi,ny_mpi,mpi_info)
            USE projection_module,                                              &
              ONLY : MYMPI

            IMPLICIT NONE
            INCLUDE 'mpif.h'
            TYPE(MYMPI),INTENT(IN) :: mpi_info

            INTEGER :: req1, req2,                                               &
                       status1(MPI_STATUS_SIZE), status2(MPI_STATUS_SIZE)
            INTEGER,INTENT(IN) :: ierr, nx_mpi, ny_mpi
            REAL(KIND=8),INTENT(INOUT) :: Phi(0:nx_mpi+1,0:ny_mpi+1)
            REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: sendbuf, recvbuf

            ALLOCATE( sendbuf(1:nx_mpi), recvbuf(1:nx_mpi) )

            sendbuf(1:nx_mpi) = Phi(1:nx_mpi,1)

            CALL MPI_ISEND(sendbuf,nx_mpi,MPI_DOUBLE_PRECISION,                 &
                           mpi_info%rank_sur(2),102,MPI_COMM_WORLD,req1,ierr)
            CALL MPI_IRECV(recvbuf,nx_mpi,MPI_DOUBLE_PRECISION,                 &
                           mpi_info%rank_sur(3),102,MPI_COMM_WORLD,req2,ierr)
            CALL MPI_WAIT(req1,status1,ierr)
            CALL MPI_WAIT(req2,status2,ierr)

            Phi(1:nx_mpi,ny_mpi+1) = recvbuf(1:nx_mpi)

            DEALLOCATE( sendbuf, recvbuf )

        END SUBROUTINE

        SUBROUTINE SEND_WEST(ierr,Phi,nx_mpi,ny_mpi,mpi_info)
            USE projection_module,                                              &
              ONLY : MYMPI

            IMPLICIT NONE
            INCLUDE 'mpif.h'
            TYPE(MYMPI),INTENT(IN) :: mpi_info

            INTEGER :: req1, req2,                                               &
                       status1(MPI_STATUS_SIZE), status2(MPI_STATUS_SIZE)
            INTEGER,INTENT(IN) :: ierr, nx_mpi, ny_mpi
            REAL(KIND=8),INTENT(INOUT) :: Phi(0:nx_mpi+1,0:ny_mpi+1)
            REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: sendbuf, recvbuf

            ALLOCATE( sendbuf(1:ny_mpi), recvbuf(1:ny_mpi) )

            sendbuf(1:ny_mpi) = Phi(nx_mpi,1:ny_mpi)

            CALL MPI_ISEND(sendbuf,nx_mpi,MPI_DOUBLE_PRECISION,                 &
                           mpi_info%rank_sur(1),103,MPI_COMM_WORLD,req1,ierr)
            CALL MPI_IRECV(recvbuf,nx_mpi,MPI_DOUBLE_PRECISION,                 &
                           mpi_info%rank_sur(0),103,MPI_COMM_WORLD,req2,ierr)
            CALL MPI_WAIT(req1,status1,ierr)
            CALL MPI_WAIT(req2,status2,ierr)

            Phi(0,1:ny_mpi) = recvbuf(1:ny_mpi)

            DEALLOCATE( sendbuf, recvbuf )

        END SUBROUTINE

        SUBROUTINE SEND_EAST(ierr,Phi,nx_mpi,ny_mpi,mpi_info)
            USE projection_module,                                              &
              ONLY : MYMPI

            IMPLICIT NONE
            INCLUDE 'mpif.h'
            TYPE(MYMPI),INTENT(IN) :: mpi_info

            INTEGER :: req1, req2,                                               &
                       status1(MPI_STATUS_SIZE), status2(MPI_STATUS_SIZE)
            INTEGER,INTENT(IN) :: ierr, nx_mpi, ny_mpi
            REAL(KIND=8),INTENT(INOUT) :: Phi(0:nx_mpi+1,0:ny_mpi+1)
            REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: sendbuf, recvbuf

            ALLOCATE( sendbuf(1:ny_mpi), recvbuf(1:ny_mpi) )

            sendbuf(1:ny_mpi) = Phi(1,1:ny_mpi)

            CALL MPI_ISEND(sendbuf,nx_mpi,MPI_DOUBLE_PRECISION,                 &
                           mpi_info%rank_sur(0),104,MPI_COMM_WORLD,req1,ierr)
            CALL MPI_IRECV(recvbuf,nx_mpi,MPI_DOUBLE_PRECISION,                 &
                           mpi_info%rank_sur(1),104,MPI_COMM_WORLD,req2,ierr)
            CALL MPI_WAIT(req1,status1,ierr)
            CALL MPI_WAIT(req2,status2,ierr)

            Phi(nx_mpi+1,1:ny_mpi) = recvbuf(1:ny_mpi)

            DEALLOCATE( sendbuf, recvbuf )

        END SUBROUTINE
