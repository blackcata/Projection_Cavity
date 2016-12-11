        !COMPILER-GENERATED INTERFACE MODULE: Mon Mar 07 12:47:32 2016
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE TDMA_SOLVER__genmod
          INTERFACE 
            SUBROUTINE TDMA_SOLVER(A,B,C,R,X,N)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: A(0:N)
              REAL(KIND=8) :: B(0:N)
              REAL(KIND=8) :: C(0:N)
              REAL(KIND=8) :: R(0:N)
              REAL(KIND=8) :: X(0:N)
            END SUBROUTINE TDMA_SOLVER
          END INTERFACE 
        END MODULE TDMA_SOLVER__genmod
