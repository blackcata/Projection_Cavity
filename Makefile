F90=ifort
FCFLAGS=-O2 -L$(MPI_HOME)/lib -I$(MPI_HOME)/include
LDFLAGS= -lmpifort

TARGET= Projection_Cavity
OBJECT= Projection_module.o Projection_main.o Projection_setup.o \
		Projection_getuh.o Projection_getvh.o \
		Projection_resi.o Projection_convec.o Projection_laplace.o  \
		Projection_UVnew.o Projection_CFL.o Projection_output.o\
		SOR_Poisson.o TDMA_Solver.o Projection_MPI.f90

all : $(TARGET)
$(TARGET) : $(OBJECT)
	$(F90) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

.SUFFIXES. : .o .f90

%.o : %.f90
	$(F90) $(FCFLAGS) -c $< $(LDFLAGS)

clean :
	rm -f *.o
	rm RESULT/*.plt
