!-----------------------------------------------------------------------------------!
!
!  PROGRAM: Projection_getuh.f90
!
!  PURPOSE: Get UH(U hat which is addition between u and delta u)
!
!                                                                2016.03.04 K.Noh
!                                                   
!   log 
!   2016.03.04 Add getdu and getdv code for 1st process of projection method
!   2016.03.06 Modified the files name and alogrithm to getdu -> getuh
!
!-----------------------------------------------------------------------------------!
    
        SUBROUTINE GETUH
            
            USE projection_module,                                              &
                ONLY : Nx, Ny, dx, dy, Re, dt
            
            USE projection_module,                                              &
                ONLY : U, V, Rx
            
            IMPLICIT NONE
            INTEGER :: i,j
            REAL(KIND=8) :: c1, c2
            REAL(KIND=8) :: D(1:Nx-1,1:Ny), du(1:Nx-1,1:Ny)

            c1 = dt / (2*Re*dx**2)
            c2 = dt / (2*Re*dy**2)


            
        END SUBROUTINE GETUH
    
