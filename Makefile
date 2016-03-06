F90=gfortran
TARGET= Projection_Cavity
OBJECT= Projection_module.o Projection_main.o Projection_setup.o \
		Projection_getdu.o Projection_getdv.o Projection_convec.o \
		Projection_resi.o \
		SOR_Poisson.o TDMA_Solver.o 

all : $(TARGET)
$(TARGET) : $(OBJECT)
	$(F90) -o $@ $^

.SUFFIXES. : .o .f90

%.o : %.f90
	$(F90) -c $<

clean :
	rm -f *.o
