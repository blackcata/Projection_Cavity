!-----------------------------------------------------------------------------------!
!
!  PROGRAM: Projection_output.f90
!
!  PURPOSE: Make output files of each variables in the result folder.
!    
!                                                                2016.03.09 K.Noh
!   log 
!   2016.03.04 Add  output system file, this file'll make each variables for each
!              files and it will trace each variables status 
!
!-----------------------------------------------------------------------------------!
    
        SUBROUTINE OUTPUT

            USE projection_module,                                                &
                ONLY : file_name, path_name
            
            USE projection_module,                                                &
                ONLY : Nx, Ny, dx, dy
            
            USE projection_module,                                                &
                ONLY : U, V, Uhat, Vhat, UNew, VNew, Phi,                         &
                       Rx, Ry, Hx, Hy, Lx, Ly
            
            IMPLICIT NONE
            INTEGER :: i,j
            
            
200         FORMAT('ZONE',2X,'I=',I3,2X,'J=',I3,2X,'F=POINT')
            
            file_name = TRIM(path_name)//'/U.plt' 
            OPEN(100,FILE=file_name,FORM='FORMATTED',STATUS='REPLACE')
            WRITE(100,*)'VARIABLES="x","y","U"'
            WRITE(100,200) Nx+1, Ny+2
            DO j = 0, Ny+1
                DO i = 0, Nx
                    WRITE(100,*) i*dx, j*dy, U(i,j)
                END DO
            END DO
            CLOSE(100)
            
            file_name = TRIM(path_name)//'/V.plt' 
            OPEN(100,FILE=file_name,FORM='FORMATTED',STATUS='REPLACE')
            WRITE(100,*)'VARIABLES="x","y","V"'
            WRITE(100,200) Nx+1, Ny+2
            DO j = 0, Ny
                DO i = 0, Nx+1
                    WRITE(100,*) i*dx, j*dy, V(i,j)
                END DO
            END DO
            CLOSE(100)
            
!            ALLOCATE( U(0:Nx,0:Ny+1), V(0:Nx+1,0:Ny), P(1:Nx,1:Ny) )
!            ALLOCATE( Uhat(1:Nx-1,1:Ny), Vhat(1:Nx,1:Ny-1) )
!            ALLOCATE( Hx(1:Nx-1,1:Ny), Hy(1:Nx,1:Ny-1) )
!            ALLOCATE( Lx(1:Nx-1,1:Ny), Ly(1:Nx,1:Ny-1) )
!            ALLOCATE( Rx(1:Nx-1,1:Ny), Ry(1:Nx,1:Ny-1) )
!            ALLOCATE( Unew(0:Nx,0:Ny+1), Vnew(0:Nx+1,0:Ny), Phi(1:Nx,1:Ny,0:1) )
            
        END SUBROUTINE OUTPUT

