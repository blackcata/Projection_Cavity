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
!   2016.03.09 Modified UNew,Vnew and Uhat,Vhat's arrays size, its size is different
!
!-----------------------------------------------------------------------------------!
    
        SUBROUTINE UVNEW
            
            USE projection_module,                                              &
                ONLY : Nx, Ny, dt, U0, dx, dy, file_name, path_name
            
            USE projection_module,                                              &
                ONLY : UNew, VNew, Uhat, Vhat, U, V, GRADIENT
            
            IMPLICIT NONE
            
            INTEGER :: i, j
            REAL(KIND=8) :: Gx(1:Nx-1,1:Ny), Gy(1:Nx,1:Ny-1)

            Gx = 0.0
            Gy = 0.0

            CALL GRADIENT(Gx,Gy)
            
            UNew(1:Nx-1,1:Ny) = Uhat - dt*Gx
            VNew(1:Nx,1:Ny-1) = Vhat - dt*Gy
 
            UNew(:,Ny+1) = U0
            
            U = UNew
            V = VNew
            
        END SUBROUTINE UVNEW
    
