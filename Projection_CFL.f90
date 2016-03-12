!-----------------------------------------------------------------------------------!
!
!  PROGRAM: Projection_CFL.f90
!
!  PURPOSE: Get CFL Number of each time step
!
!                                                                2016.03.08 K.Noh
!                                                   
!   log 
!   2016.03.11 Add CFL calculation code and programmed printing cfl number.
!
!-----------------------------------------------------------------------------------!
    
        SUBROUTINE CFL
            
            USE projection_module,                                              &
                ONLY : dx, dy, dt
            
            USE projection_module,                                              &
                ONLY : U,V
            
            IMPLICIT NONE
            REAL(KIND=8) :: CFLN, U_max, V_max
            
            U_max = MAXVAL(U)
            V_max = MAXVAL(V)
            
            CFLN = U_max *dt/dx + V_max * dt/dx
            WRITE(*,FMT='(A,F10.7)') 'CFL Number is ',CFLN

        END SUBROUTINE CFL
    
