!-----------------------------------------------------------------------------------!
!
!  PROGRAM: Projection_resi.f90
!
!  PURPOSE: Get residual term which includes convective term and laplace term.
!
!                                                                2016.03.06 K.Noh
!                                                   
!   log 
!   2016.03.06 Add residual code and it is programmed.
!
!-----------------------------------------------------------------------------------!
    
        SUBROUTINE RESI
            
            USE projection_module,                                              &
                ONLY : Nx, Ny, Re, dt
            
            USE projection_module,                                              &
                ONLY : Hx, Hy, Lx, Ly, Rx, Ry
            
            IMPLICIT NONE
            
            INTEGER :: i, j
            REAL(8) :: Hx_pr(1:Nx-1,1:Ny), Hy_pr(1:Nx,1:Ny-1)

            Hx_pr = Hx
            Hy_pr = Hy

            CALL CONVEC
            CALL LAPLACE

            Rx = dt/2 * ( 3*Hx - Hx_pr) + dt/Re * Lx
            Ry = dt/2 * ( 3*Hy - Hy_Pr) + dt/Re * Ly

        END SUBROUTINE RESI
    
