!-----------------------------------------------------------------------------------!
!
!  PROGRAM: Projection_getvh.f90
!
!  PURPOSE: Get VH(V hat which is addition between v and dv
!
!                                                                2016.03.04 K.Noh
!                                                   
!   log 
!   2016.03.04 Add getdu and getdv code for 1st process of projection method
!   2016.03.06 Change the files name and algorithm of this subroutine
!   2016.03.08 Add the detail code and algorithm at this getuh code
!
!-----------------------------------------------------------------------------------!
    
        SUBROUTINE GETVH
            
            USE projection_module,                                              &
                ONLY : Nx, Ny, dx, dy, Re, dt
            
            USE projection_module,                                              &
                ONLY : V, Vhat, Ry
            
            IMPLICIT NONE
            INTEGER :: i,j
            REAL(KIND=8) :: c1, c2
            REAL(KIND=8) :: D(1:Nx,1:Ny-1), dv(1:Nx,1:Ny-1)
            REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: a,b,c

            c1 = dt / (2*Re*dx**2)
            c2 = dt / (2*Re*dy**2)

            ALLOCATE( a(1:Nx), b(1:Nx), c(1:Nx) )

            DO j = 1, Ny-1
              a = -c1
              b = 1 + 2*c1
              c = -c1

              CALL TDMA_Solver(a,b,c,Ry(:,j),D(:,j),Nx-1)
            END DO

            DEALLOCATE( a,b,c )
            ALLOCATE( a(1:Ny-1), b(1:Ny-1), c(1:Ny-1) )
            
            DO i = 1,Nx
              a = -c2
              b = 1+2*c2
              c = -c2

              CALL TDMA_Solver(a,b,c,D(i,:),dv(i,:),Ny-2)
            END DO
            
            DEALLOCATE( a,b,c )
            
            Vhat = V + dv

        END SUBROUTINE GETVH
    
