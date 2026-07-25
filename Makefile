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


MODULES=   accuracy.o constants.o  message.o hall.o permu.o bztest.o groupkp.o sumsets.o projmat.o eigensolver.o intsec.o eigvec.o genera.o charac.o  subsp.o  classes.o  degen.o repres.o irrep.o  modsymprj.o vasp_reader.o time_reversal.o time_reversal_optimization.o sympw_pointgroup_data.o sympw_core.o sympw_real_sh.o orbmain.o

VASP_MODULES = accuracy.o constants.o message.o hall.o permu.o bztest.o groupkp.o sumsets.o projmat.o eigensolver.o intsec.o eigvec.o genera.o charac.o subsp.o classes.o degen.o repres.o irrep.o modsymprj.o vasp_reader.o time_reversal.o time_reversal_optimization.o sympw_pointgroup_data.o sympw_core.o sympw_real_sh.o sympw_vasp.o

LIB_MODULES = accuracy.o constants.o message.o hall.o permu.o bztest.o groupkp.o sumsets.o projmat.o eigensolver.o intsec.o eigvec.o genera.o charac.o subsp.o classes.o degen.o repres.o irrep.o vasp_reader.o sympw_pointgroup_data.o sympw_core.o sympw_lib.o sympw_real_sh.o

ALL=  $(MODULES)


sympw:$(ALL)
	$(LN) $(FC90OPT) -o sympw $(ALL) $(LIBS)

sympw_vasp:$(VASP_MODULES)
	$(LN) $(FC90OPT) -o sympw_vasp $(VASP_MODULES) $(LIBS)

all: sympw sympw_vasp

%.o: %.F90
	$(FC90) -c $*.F90

vasp_reader.o: vasp_reader.F90 accuracy.o constants.o genera.o
	$(FC90) -c vasp_reader.F90

time_reversal.o: time_reversal.F90 accuracy.o constants.o
	$(FC90) -c time_reversal.F90

time_reversal_optimization.o: time_reversal_optimization.F90 accuracy.o constants.o
	$(FC90) -c time_reversal_optimization.F90

sympw_pointgroup_data.o: sympw_pointgroup_data.F90 accuracy.o constants.o genera.o
	$(FC90) -c sympw_pointgroup_data.F90

sympw_core.o: sympw_core.F90 accuracy.o constants.o groupkp.o irrep.o sumsets.o projmat.o time_reversal.o
	$(FC90) -c sympw_core.F90

sympw_vasp.o: sympw_vasp.F90 accuracy.o constants.o vasp_reader.o genera.o sympw_pointgroup_data.o sympw_core.o
	$(FC90) -c sympw_vasp.F90

orbmain.o: orbmain.F90 vasp_reader.o time_reversal.o time_reversal_optimization.o sympw_pointgroup_data.o sympw_core.o sympw_real_sh.o
	$(FC90) -c orbmain.F90

sympw_lib.o: sympw_lib.F90 accuracy.o constants.o vasp_reader.o sympw_pointgroup_data.o sympw_core.o
	$(FC90) -c sympw_lib.F90

sympw_real_sh.o: sympw_real_sh.F90 accuracy.o
	$(FC90) -c sympw_real_sh.F90

libsympw.a: $(LIB_MODULES)
	ar rcs libsympw.a $(LIB_MODULES)

TEST_LIB_MODULES = accuracy.o constants.o message.o hall.o permu.o bztest.o groupkp.o sumsets.o projmat.o eigensolver.o intsec.o eigvec.o genera.o charac.o subsp.o classes.o degen.o repres.o irrep.o modsymprj.o vasp_reader.o time_reversal.o time_reversal_optimization.o sympw_pointgroup_data.o sympw_core.o sympw_lib.o sympw_real_sh.o

test_lib: $(TEST_LIB_MODULES) test_lib.o
	$(LN) $(FC90OPT) -o test_lib $(TEST_LIB_MODULES) test_lib.o $(LIBS)

test_lib.o: test_lib.F90 accuracy.o constants.o sympw_lib.o sympw_real_sh.o
	$(FC90) -c test_lib.F90

clean:
	rm -f *.mod *.o 
