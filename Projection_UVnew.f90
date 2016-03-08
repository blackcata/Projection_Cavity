!-----------------------------------------------------------------------------------!
!
!  PROGRAM: Projection_UVnew.f90
!
!  PURPOSE: Get U,V Update values from uhat which is calculated by gradient phi
!
!                                                                2016.03.08 K.Noh
!                                                   
!   log 
!   2016.03.08 Add UVNew subroutine and programmed it.
!
!-----------------------------------------------------------------------------------!
    
        SUBROUTINE UVNEW
            
            USE projection_module,                                              &
                ONLY : Nx, Ny, dt
            
            USE projection_module,                                              &
                ONLY : UNew, VNew, Uhat, Vhat, U, V, GRADIENT
            
            IMPLICIT NONE
            
            INTEGER :: i, j
            REAL(KIND=8) :: Gx(1:Nx-1,1:Ny), Gy(1:Nx,1:Ny-1)

            Gx = 0.0
            Gy = 0.0

            CALL GRADIENT(Gx,Gy)

            UNew = Uhat - dt*Gx
            VNew = Vhat - dt*Gy

            U = UNew
            V = VNew

        END SUBROUTINE UVNEW
    
