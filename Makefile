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


MODULES=   accuracy.o constants.o  message.o hall.o permu.o bztest.o groupkp.o sumsets.o projmat.o eigensolver.o intsec.o eigvec.o genera.o charac.o  subsp.o  classes.o  degen.o repres.o irrep.o  modsymprj.o vasp_reader.o orbmain.o

VASP_MODULES = accuracy.o constants.o message.o hall.o permu.o bztest.o groupkp.o sumsets.o projmat.o eigensolver.o intsec.o eigvec.o genera.o charac.o subsp.o classes.o degen.o repres.o irrep.o modsymprj.o vasp_reader.o sympw_vasp.o

ALL=  $(MODULES)


sympw:$(ALL)
	$(LN) $(FC90OPT) -o sympw $(ALL) $(LIBS)

sympw_vasp:$(VASP_MODULES)
	$(LN) $(FC90OPT) -o sympw_vasp $(VASP_MODULES) $(LIBS)

all: sympw sympw_vasp

$(MODULES):%.o:%.F90
	$(FC90)  -c  $*.F90

vasp_reader.o: vasp_reader.F90 accuracy.o constants.o genera.o
	$(FC90) -c vasp_reader.F90

sympw_vasp.o: sympw_vasp.F90 accuracy.o constants.o vasp_reader.o modsymprj.o
	$(FC90) -c sympw_vasp.F90

orbmain.o: orbmain.F90 vasp_reader.o
	$(FC90) -c orbmain.F90


clean:
	rm -f *.mod *.o 
