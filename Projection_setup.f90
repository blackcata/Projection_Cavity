!-----------------------------------------------------------------------------------!
!  PROGRAM: Projection_setup.f90
!
!  PURPOSE: Setup the cavity problem for Projection method
!
!                                                                2016.03.02 K.Noh
!                                                   
!   log 
!   2016.03.01 First add the setup file and initialization of the U,V,P
!
!-----------------------------------------------------------------------------------!
        SUBROUTINE SETUP()
            
            USE projection_module,                                              &
                ONLY : Nx, Ny, dx, dy, Lx, Ly
            
            USE projection_module,                                              &
                ONLY : U0, U, V, P 
            
            IMPLICIT NONE

            U0 = 1.0
            
            Nx = 10
            Ny = 10
            
            Lx = 1.0
            Ly = 1.0
            
            dx = Lx / Nx
            dy = Ly / Ny
            
            ALLOCATE( U(0:Nx,0:Ny), V(0:Nx,0:Ny), P(0:Nx,0:Ny) )
            
            U = 0.0
            V = 0.0
            P = 0.0
            
            U(:,Ny) = U0 
            
        END SUBROUTINE SETUP
    