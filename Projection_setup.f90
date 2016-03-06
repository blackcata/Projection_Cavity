!-----------------------------------------------------------------------------------!
!
!  PROGRAM: Projection_setup.f90
!
!  PURPOSE: Setup the cavity problem for Projection method
!
!                                                                2016.03.02 K.Noh
!                                                   
!   log 
!   2016.03.01 First add the setup file and initialization of the U,V,P
!   2016.03.03 Setup the omega and tollerance value and allocate phi 
!   2016.03.04 Add Unew, Vnea, Uhat, Vhat, hx, hy arrays and initialized    
!
!-----------------------------------------------------------------------------------!
    
        SUBROUTINE SETUP
            
            USE projection_module,                                              &
                ONLY : Nx, Ny, dx, dy, Lx, Ly, omega, tol, Re, U0
            
            USE projection_module,                                              &
                ONLY :U, V, Uhat, Vhat, Unew, Vnew, hx, hy, P, Phi
            
            IMPLICIT NONE

            U0 = 1.0
            Re = 100
            
            Nx = 50
            Ny = 50
            
            Lx = 1.0
            Ly = 1.0
            
            dx = Lx / Nx
            dy = Ly / Ny
            
            omega = 1.85
            tol = 1e-4
            
            ALLOCATE( U(0:Nx,0:Ny+1), V(0:Nx+1,0:Ny), P(1:Nx,1:Ny) )
            ALLOCATE( Uhat(1:Nx-1,1:Ny), Vhat(1:Nx,1:Ny-1) )
            ALLOCATE( Hx(1:Nx-1,1:Ny), Hy(1:Nx,1:Ny-1) )
            ALLOCATE( Unew(0:Nx,0:Ny+1), Vnew(0:Nx+1,0:Ny), Phi(1:Nx,1:Ny,0:1) )
            
            U = 0.0
            V = 0.0
            P = 0.0
            Phi = 0.0
            
            Unew = 0.0
            Vnew = 0.0
            Uhat = 0.0
            Vhat = 0.0
            
            Hx = 0.0
            Hy = 0.0
            
            U(:,NY-1:Ny) = U0
            
        END SUBROUTINE SETUP
    