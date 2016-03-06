!-----------------------------------------------------------------------------------!
!
!  PROGRAM: Projection_laplace.f90
!
!  PURPOSE: Calculating the laplace linear term
!
!                                                                2016.03.06 K.Noh
!                                                   
!   log 
!   2016.03.06 Add lapalce term calculating code
!
!-----------------------------------------------------------------------------------!
    
        SUBROUTINE LAPLACE
            
            USE projection_module,                                              &
                ONLY : Nx, Ny, dx, dy
            
            USE projection_module,                                              &
                ONLY : U, V, Lx, Ly
            
            IMPLICIT NONE

            INTEGER :: i, j

            DO j = 1,Ny
              DO i = 1,Nx-1
                Lx(i,j) = (U(i+1,j) - 2*U(i,j) + U(i-1,j)) / (dx**2) +          &
                          (U(i,j+1) - 2*U(i,j) + U(i,j-1)) / (dy**2) 
              END DO
            END DO

            DO j = 1,Ny-1
              DO i = 1,Nx
                Ly(i,j) = (V(i+1,j) - 2*V(i,j) + V(i-1,j)) / (dx**2) +          &
                          (V(i,j+1) - 2*V(i,j) + V(i,j-1)) / (dy**2) 
              END DO
            END DO

        END SUBROUTINE LAPLACE
    
