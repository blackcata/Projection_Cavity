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

        SUBROUTINE SOR

            USE projection_module,                                              &
              ONLY : Nx, Ny, dx, dy, dt, tol, omega, ITMAX, MYMPI

            USE projection_module,                                              &
                ONLY : Phi, DIVERGENCE

            IMPLICIT NONE

            INTEGER :: i, j, it
            REAL(KIND=8) :: beta, rms, t1, t2, SUM1, SUM2
            REAL(KIND=8),DIMENSION(:,:),ALLOCATABLE :: b, phi_new
            TYPE(MYMPI) :: mpi_info

            ALLOCATE( b(1:Nx,1:Ny),phi_new(1:Nx,1:Ny) )

            b(1:Nx,1:Ny)       = 0.0
            phi_new(1:Nx,1:Ny) = 0.0
            beta = dx/dy

            WRITE(*,*) '-------------------------------------------------------'
            WRITE(*,*) '                  SOR PROCESS STARTED                  '
            CALL DIVERGENCE(b)
            CALL CPU_TIME(t1)

            !------------------------------------------------------------------!
            !                      Main Loop of SOR method                     !
            !------------------------------------------------------------------!
            DO it=1,ITMAX
              SUM1 = 0.0
              SUM2 = 0.0

              !----------------------------------------------------------------!
              !                           Update red nodes                     !
              !----------------------------------------------------------------!
              DO j=2,Ny-1
                DO i=2,Nx-1
                    IF((mod(i+j,2))==0) THEN
                      phi_new(i,j) = ( phi(i+1,j)+phi(i-1,j)                    &
                                      + beta**2*(phi(i,j+1)+phi(i,j-1))         &
                                      - dx*dx*b(i,j)/dt ) / (2*(1+beta**2))
                      phi_new(i,j) = phi(i,j) + omega*(phi_new(i,j) - phi(i,j))
                    END IF
                END DO
              END DO

              !----------------------------------------------------------------!
              !                         Update black nodes                     !
              !----------------------------------------------------------------!
              DO j=2,Ny-1
                DO i=2,Nx-1
                    IF((mod(i+j,2))==1) THEN
                      phi_new(i,j) = ( phi_new(i+1,j)+phi_new(i-1,j)            &
                                      + beta**2*(phi_new(i,j+1)+phi_new(i,j-1)) &
                                      - dx*dx*b(i,j)/dt ) / (2*(1+beta**2))
                      phi_new(i,j) = phi(i,j) + omega*(phi_new(i,j) - phi(i,j))
                    END IF
                END DO
              END DO

              !----------------------------------------------------------------!
              !                       Boundary Conditions                      !
              !----------------------------------------------------------------!
              DO j = 1,Ny
                phi_new(1,j)  = phi_new(2,j)!0.0
                phi_new(Nx,j) = phi_new(Nx-1,j)!0.0
              END DO

              Do i = 1,Nx
                phi_new(i,1)  = 1.0
                phi_new(i,Ny) = 0.0
              END DO

              !----------------------------------------------------------------!
              !                       Convergence Criteria                     !
              !----------------------------------------------------------------!
              DO j = 2,Ny-1
                DO i = 2,Nx-1
                  SUM1 = SUM1 + abs(phi_new(i,j))
                  SUM2 = SUM2 + abs( phi_new(i+1,j)+phi_new(i-1,j)              &
                                  + beta**2*(phi_new(i,j+1)+phi_new(i,j-1))     &
                                  -(2+2*beta**2)*phi_new(i,j)- dx*dx*b(i,j)/dt )
                END DO
              END DO

              ! WRITE(*,"(I5,2X,3(F15.7,2X))") it, SUM2/SUM1, tol
              IF ( SUM2/SUM1 < tol ) EXIT

              !----------------------------------------------------------------!
              !                               Update                           !
              !----------------------------------------------------------------!
              phi(1:Nx,1:Ny) = phi_new(1:Nx,1:Ny)

            END DO

            DEALLOCATE(b,phi_new)
            CALL CPU_TIME(t2)

            WRITE(*,*) '                   SOR PROCESS ENDED                   '
            WRITE(*,FMT='(A,I5,A,F10.7,A)')                                     &
                'Total Iteration : ',it,', total time for SOR : ',t2-t1,'s'
            WRITE(*,*) '-------------------------------------------------------'
            WRITE(*,*) ''

        END SUBROUTINE SOR
