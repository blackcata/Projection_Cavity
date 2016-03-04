!-----------------------------------------------------------------------------------!
!
!  PROGRAM: SOR_Poisson.f90
!
!  PURPOSE: To solve the poisson equation by SOR Method 
!
!                                                                2016.03.02 K.Noh
!
!   log 
!   2016.03.03 Add SOR Solver of poisson equation and modified to optimize to 
!              cavity problem and this projection code
!                                     
!-----------------------------------------------------------------------------------! 

        SUBROUTINE SOR

            USE projection_module,                                                          &
              ONLY : Nx, Ny, dx, dy, tol, omega

            USE projection_module,                                                          &
                ONLY : Phi
            
            IMPLICIT NONE
            
            INTEGER :: i, j, it
            REAL(KIND=8) :: beta, rms, t1, t2, SUM1, SUM2
            REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: b
            
            ALLOCATE( b(1:Nx,1:Ny) )

            beta = dx/dy

            CALL CPU_TIME(t1)
            DO it = 1,100000

              DO j = 1, Ny-1
                DO i = 1, Nx-1
                  Phi(i,j,1) = (Phi(i+1,j,0) + Phi(i-1,j,1) + &
                             (beta**2) * (Phi(i,j+1,0) + Phi(i,j-1,1)) - dx*dx*b(i,j) ) &
                                 / (2+2*beta**2)
                  Phi(i,j,1) = omega * Phi(i,j,1) + (1-omega) * Phi(i,j,0)
                END DO
              END DO

              rms = 0.0
              SUM1 = 0.0
              SUM2 = 0.0

              DO j = 1,Ny-1
                  DO i = 1,Nx-1
                      SUM1 = SUM1 + abs(Phi(i,j,1)) **2
                      SUM2 = SUM2 + abs(Phi(i+1,j,1)+Phi(i-1,j,1)+Phi(i,j+1,1) &
                                  +Phi(i,j-1,1) - 4*Phi(i,j,1)) **2
                  END DO
              END DO

              rms = sqrt(SUM2/SUM1)

              If ( rms < tol ) EXIT

              !----------------!
              !    Updating    !
              !----------------!
              Phi(:,:,0) = Phi(:,:,1)

            END DO
            CALL CPU_TIME(t2)
            print*,it,t2-t1

        END SUBROUTINE SOR
