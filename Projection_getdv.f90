!-----------------------------------------------------------------------------------!
!
!  PROGRAM: Projection_getdv.f90
!
!  PURPOSE: Get du(delta u between Uhat and u)
!
!                                                                2016.03.02 K.Noh
!                                                   
!   log 
!   2016.03.04 Add getdu and getdv code for 1st process of projection method
!
!-----------------------------------------------------------------------------------!
    
        SUBROUTINE GETdv
            
            USE projection_module,                                              &
                ONLY : Nx, Ny, dx, dy
            
            USE projection_module,                                              &
                ONLY : U0, U, V, Uhat, Vhat
            
            IMPLICIT NONE
            
        END SUBROUTINE GETdv
    