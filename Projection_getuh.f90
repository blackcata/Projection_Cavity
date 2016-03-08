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
!   2016.03.08 Add the detail code and algorithm at this getuh code
!   2016.03.08 Initialized the D,du,a,b,c
!
!-----------------------------------------------------------------------------------!
    
        SUBROUTINE GETUH
            
            USE projection_module,                                              &
                ONLY : Nx, Ny, dx, dy, Re, dt
            
            USE projection_module,                                              &
                ONLY : U, Uhat, Rx
            
            IMPLICIT NONE
            INTEGER :: i,j
            REAL(KIND=8) :: c1, c2
            REAL(KIND=8) :: D(1:Nx-1,1:Ny), du(1:Nx-1,1:Ny)
            REAL(KIND=8),DIMENSION(:), ALLOCATABLE :: a,b,c
            
            D = 0.0
            du = 0.0

            c1 = dt / (2*Re*dx**2)
            c2 = dt / (2*Re*dy**2)

            ALLOCATE( a(1:Nx-1), b(1:Nx-1), c(1:Nx-1) )

            a = 0.0
            b = 0.0
            c = 0.0

            DO j = 1, Ny
              a = -c1
              b = 1 + 2*c1
              c = -c1
              
              CALL TDMA_Solver(a,b,c,Rx(:,j),D(:,j),Nx-2)
            END DO
            
            DEALLOCATE( a,b,c )
            ALLOCATE( a(1:Ny), b(1:Ny), c(1:Ny) )

            a = 0.0
            b = 0.0
            c = 0.0

            DO i = 1, Nx-1
              a = -c2
              b = 1+2*c2
              c = -c2

              CALL TDMA_Solver(a,b,c,D(i,:),du(i,:),Ny-1)
            END DO

            DEALLOCATE( a,b,c )

            Uhat = U + du 
            
        END SUBROUTINE GETUH
    
