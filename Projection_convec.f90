!-----------------------------------------------------------------------------------!
!
!  PROGRAM: Projection_convec.f90
!
!  PURPOSE: Calculating the convective term which is nonlinear term.
!
!                                                                2016.03.04 K.Noh
!                                                   
!   log 
!   2016.03.04 Add calculating convetive term code
!   2016.03.06 Add how to calculate Hx and Hy considering boundary condition
!
!   caution 
!     This code is assumed the non-slip boundary condition.
!     But at surface of upper boundary case, I cannot assure that this 
!     assumtion U = U0, V = 0 is right or not.
!
!-----------------------------------------------------------------------------------!
    
        SUBROUTINE CONVEC
            
            USE projection_module,                                              &
                ONLY : Nx, Ny, dx, dy
            
            USE projection_module,                                              &
                ONLY : U, V, Hx, Hy
            
            IMPLICIT NONE

            INTEGER :: i, j
            REAL(KIND=8) :: Ur, Ul, Vu, Vd

            DO j = 1, Ny
              DO i = 1,Nx-1
                IF (j == 1) THEN
                  Vu = (V(i,j) + V(i+1,j) + V(i,j+1) + V(i+1,j+1)) / 4
                  Vd = 0.0
                ELSEIF(j == Ny) THEN 
                  Vu = 0.0
                  Vd = (V(i,j-2) + V(i+1,j-2) + V(i,j-1) + V(i+1,j-1)) /4
                ELSE
                  Vu = (V(i,j) + V(i+1,j) + V(i,j+1) + V(i+1,j+1)) / 4
                  Vd = (V(i,j-2) + V(i+1,j-2) + V(i,j-1) + V(i+1,j-1)) /4
                END IF
                
                Hx(i,j) = - ( U(i+1,j)*U(i+1,j) - U(i-1,j)*U(i-1,j) ) / (2*dx)  &
                          - ( U(i,j+1)*Vu - U(i,j-1)*Vd) / (2*dy)
              END DO
            END DO

            DO j = 1,Ny-1
              DO i = 1,Nx
                IF (i == 1) THEN 
                  Ur = (U(i,j) + U(i,j+1) + U(i+1,j) + U(i+1,j+1)) / 4
                  Ul = 0.0
                ELSEIF (i == Nx) THEN
                  Ur = 0.0
                  Ul = (U(i-2,j) + U(i-2,j+1) + U(i-1,j) + U(i-1,j+1)) / 4
                ELSE
                  Ur = (U(i,j) + U(i,j+1) + U(i+1,j) + U(i+1,j+1)) / 4
                  Ul = (U(i-2,j) + U(i-2,j+1) + U(i-1,j) + U(i-1,j+1)) / 4
                END IF

                Hy(i,j) = - ( V(i,j+1)*V(i,j+1) - V(i,j-1)*V(i,j-1) ) / (2*dy)  &
                          - ( V(i,j+1)*Ur - V(i,j-1)*Ul ) / (2*dx) 
              END DO
            END DO
            
        END SUBROUTINE CONVEC
    
