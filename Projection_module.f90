!-----------------------------------------------------------------------------------!
!
!  PROGRAM: Projection_main.f90
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
!   2016,03.08 Modified divergence subroutine u -> uhat, v -> vhat
!-----------------------------------------------------------------------------------!

        MODULE projection_module
            
            INTEGER :: Nx, Ny
            REAL(KIND=8) :: Re, dt, dx, dy, ALx, ALy
            REAL(KIND=8) :: U0, tol, omega
            CHARACTER(LEN=65) :: file_name, path_name
 
            REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE   :: U,V, Uhat, Vhat, UNew, VNew
            REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE   :: P,VOR,Stream
            REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE   :: Rx, Ry, Hx, Hy, Lx, Ly
            REAL(KIND=8), DIMENSION(:,:,:), ALLOCATABLE :: Phi
            
            SAVE
            
        CONTAINS
    
            SUBROUTINE DIVERGENCE(b)
                
                IMPLICIT NONE
                INTEGER :: i,j    
                REAL(KIND=8) :: b(1:Nx,1:Ny)
                
                DO j = 1,Ny
                    DO i = 1,Nx
                        b(i,j) = (Uhat(i,j) - Uhat(i-1,j)) / dx + & 
                                 (Vhat(i,j) - Vhat(i,j-1)) / dy
                    END DO
                END DO
            
            END SUBROUTINE DIVERGENCE
            
            SUBROUTINE GRADIENT(gx,gy)
                
                IMPLICIT NONE
                INTEGER :: i,j
                REAL(KIND=8) :: gx(1:Nx-1,1:Ny), gy(1:Nx,1:Ny-1)
                
                DO j = 1,Ny
                    DO i = 1, Nx-1
                        gx(i,j) = (Phi(i+1,j,1) - Phi(i,j,1))/dx
                    END DO
                END DO
                
                DO j = 1,Ny-1
                    DO i = 1, Nx
                        gy(i,j) = (Phi(i,j+1,1) - Phi(i,j,1))/dy
                    END DO
                END DO
                
            END SUBROUTINE
            
        END MODULE projection_module
    
