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
!
!-----------------------------------------------------------------------------------!
    
        SUBROUTINE GETVH
            
            USE projection_module,                                              &
                ONLY : Nx, Ny, dx, dy
            
            USE projection_module,                                              &
                ONLY : U, V, Uhat, Vhat
            
            IMPLICIT NONE
            
        END SUBROUTINE GETVH
    
