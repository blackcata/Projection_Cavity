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
    
        SUBROUTINE CONVEC
            
            USE projection_module,                                              &
                ONLY : Nx, Ny, dx, dy
            
            USE projection_module,                                              &
                ONLY : U, V, Hx, Hy
            
            IMPLICIT NONE

            INTEGER :: i, j

            DO j = 1,Ny
              DO i = 1,Nx-1
                
              END DO
            END DO
        END SUBROUTINE CONVEC
    
