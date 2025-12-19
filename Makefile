.SUFFIXES: .o .f .f90

# Fortran 90 compiler
FC90 = gfortran

# Options for the Fortran 90 compiler 
FC90OPT = -O2 -openmp -funroll-all-loops -fall-intrinsics 


# Linker
LN = $(FC90)

# Linker options
LNOPT = 

# Override options for different DEBUG modes
ifeq ($(DEBUG),1)
    FC90OPT = -g -Wall -std=f2003 -pedantic -fall-intrinsics -fbounds-check
endif
ifeq ($(DEBUG),2)
    FC90OPT = -g -Wall -std=f2003 -pedantic -fall-intrinsics -fbounds-check
endif
ifeq ($(DEBUG),3)
    FC90OPT = -g -Wall -std=f2003 -pedantic -fall-intrinsics -fbounds-check
endif

# Library options in general
ATLASDIR = /usr/lib
LIBOPT = -L$(ATLASDIR) -L/usr/local/lib

# How to link specific libraries
LIBS = -L/usr/local/lib -L/usr/lib -llapack -lblas

# Threaded version may work as well
# LIB_BLAS = -lptf77blas -lptcblas -latlas


MODULES=   accuracy.o constants.o  message.o hall.o permu.o bztest.o groupkp.o sumsets.o projmat.o eigensolver.o intsec.o eigvec.o genera.o charac.o  subsp.o  classes.o  degen.o repres.o irrep.o  modsymprj.o orbmain.o

ALL=  $(MODULES)

	
sympw:$(ALL)
	$(LN) $(FC90OPT) -o sympw $(ALL) $(LIBS)
	
$(MODULES):%.o:%.F90
	$(FC90)  -c  $*.F90


clean:
	rm -f *.mod *.o 
