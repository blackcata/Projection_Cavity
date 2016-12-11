!-----------------------------------------------------------------------------------!
!
!  PROGRAM: Projection_setup.f90
!
!  PURPOSE: Setup the cavity problem for Projection method
!
!                                                                2016.03.01 K.Noh
!
!   log
!   2016.03.01 First add the setup file and initialization of the U,V,P
!   2016.03.03 Setup the omega and tollerance value and allocate phi
!   2016.03.04 Add Unew, Vnea, Uhat, Vhat, hx, hy arrays and initialized
!   2016.03.06 Add the residual term rx, ry and initialized and dt term
!   2016.03.11 Change the dt's value because it have to satisfy CFL condition
!
!-----------------------------------------------------------------------------------!

        SUBROUTINE SETUP

            USE projection_module,                                              &
                ONLY : Nx, Ny, dx, dy, ALx, ALy, omega, tol, Re, U0, dt,        &
                       PRINT_NUM, TOTAL_IT, ITMAX

            USE projection_module,                                              &
                ONLY : U, V, Uhat, Vhat, Unew, Vnew, P, Phi,                    &
                       Hx, Hy, Rx, Ry, Lx, Ly

            IMPLICIT NONE

            PRINT_NUM = 20
            TOTAL_IT  = 100
            ITMAX     = 100000

            U0 = 1.0
            Re = 100
            dt = 0.001

            Nx = 100
            Ny = 100

            ALx = 1.0
            ALy = 1.0

            dx = ALx / Nx
            dy = ALy / Ny

            omega = 1.0
            tol = 1e-6

            ALLOCATE( U(0:Nx,0:Ny+1), V(0:Nx+1,0:Ny), P(1:Nx,1:Ny) )
            ALLOCATE( Uhat(1:Nx-1,1:Ny), Vhat(1:Nx,1:Ny-1) )
            ALLOCATE( Hx(1:Nx-1,1:Ny), Hy(1:Nx,1:Ny-1) )
            ALLOCATE( Lx(1:Nx-1,1:Ny), Ly(1:Nx,1:Ny-1) )
            ALLOCATE( Rx(1:Nx-1,1:Ny), Ry(1:Nx,1:Ny-1) )
            ALLOCATE( Unew(0:Nx,0:Ny+1), Vnew(0:Nx+1,0:Ny), Phi(1:Nx,1:Ny) )

            U = 0.0
            V = 0.0
            P = 0.0
            Phi = 0.0
            Phi(1:Nx,1) = 1.0

            Unew = 0.0
            Vnew = 0.0
            Uhat = 0.0
            Vhat = 0.0

            Hx = 0.0
            Hy = 0.0
            Lx = 0.0
            Ly = 0.0
            Rx = 0.0
            Ry = 0.0

            U(:,Ny+1) = U0

        END SUBROUTINE SETUP
