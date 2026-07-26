.SUFFIXES: .o .f .f90

# Fortran 90 compiler
FC90 = gfortran

# Options for the Fortran 90 compiler
# GCC 15 miscompiles the complex MATMUL/CONJG/TRANSPOSE paths used here
# when its Fortran frontend optimizer is enabled; backend -O2 remains active.
FC90OPT = -O2 -fno-frontend-optimize -fopenmp -funroll-all-loops -fall-intrinsics


# Linker
LN = $(FC90)

# Linker options
LNOPT = 

# Override options for different DEBUG modes
ifeq ($(DEBUG),1)
    FC90OPT = -g -Wall -std=f2008 -pedantic -fall-intrinsics -fcheck=all -fbacktrace
endif
ifeq ($(DEBUG),2)
    FC90OPT = -g -Wall -std=f2008 -pedantic -fall-intrinsics -fcheck=all -fbacktrace
endif
ifeq ($(DEBUG),3)
    FC90OPT = -g -Wall -std=f2008 -pedantic -fall-intrinsics -fcheck=all -fbacktrace
endif

# Library options in general
ATLASDIR = /usr/lib
LIBOPT = -L$(ATLASDIR) -L/usr/local/lib

# How to link specific libraries
LIBS = -L/usr/local/lib -L/usr/lib -llapack -lblas

# Threaded version may work as well
# LIB_BLAS = -lptf77blas -lptcblas -latlas


MODULES=   accuracy.o constants.o sympw_group_mode.o sympw_phase.o sympw_config.o message.o hall.o permu.o bztest.o groupkp.o sumsets.o projmat.o eigensolver.o intsec.o eigvec.o genera.o charac.o subsp.o classes.o degen.o repres.o irrep.o vasp_reader.o time_reversal.o time_reversal_optimization.o sympw_pointgroup_data.o sympw_core.o sympw_mulliken_cubic.o sympw_mulliken.o sympw_mulliken_real_pairs.o sympw_real_sh.o sympw_lib.o sympw_vasp_input.o sympw_cli_runner.o orbmain.o

VASP_MODULES = accuracy.o constants.o sympw_group_mode.o sympw_phase.o sympw_config.o message.o hall.o permu.o bztest.o groupkp.o sumsets.o projmat.o eigensolver.o intsec.o eigvec.o genera.o charac.o subsp.o classes.o degen.o repres.o irrep.o vasp_reader.o time_reversal.o time_reversal_optimization.o sympw_pointgroup_data.o sympw_core.o sympw_mulliken_cubic.o sympw_mulliken.o sympw_mulliken_real_pairs.o sympw_real_sh.o sympw_lib.o sympw_vasp_input.o sympw_cli_runner.o sympw_vasp.o

LIB_MODULES = accuracy.o constants.o sympw_group_mode.o sympw_phase.o message.o hall.o permu.o bztest.o groupkp.o sumsets.o projmat.o eigensolver.o intsec.o eigvec.o genera.o charac.o subsp.o classes.o degen.o repres.o irrep.o vasp_reader.o sympw_pointgroup_data.o sympw_core.o sympw_mulliken_cubic.o sympw_mulliken.o sympw_mulliken_real_pairs.o sympw_lib.o sympw_real_sh.o

ALL=  $(MODULES)


sympw:$(ALL)
	$(LN) $(FC90OPT) -o sympw $(ALL) $(LIBS)

sympw_vasp:$(VASP_MODULES)
	$(LN) $(FC90OPT) -o sympw_vasp $(VASP_MODULES) $(LIBS)

all: sympw sympw_vasp

%.o: %.F90
		$(FC90) $(FC90OPT) -c $*.F90

sympw_phase.o: sympw_phase.F90 accuracy.o
		$(FC90) $(FC90OPT) -c sympw_phase.F90

sumsets.o: sympw_group_mode.o

repres.o: sympw_group_mode.o

modsymprj.o: sympw_group_mode.o sumsets.o

groupkp.o: groupkp.F90 accuracy.o constants.o bztest.o sympw_phase.o
		$(FC90) $(FC90OPT) -c groupkp.F90

projmat.o: projmat.F90 accuracy.o constants.o sympw_group_mode.o sympw_phase.o
		$(FC90) $(FC90OPT) -c projmat.F90

vasp_reader.o: vasp_reader.F90 accuracy.o constants.o genera.o
		$(FC90) $(FC90OPT) -c vasp_reader.F90

time_reversal.o: time_reversal.F90 accuracy.o constants.o
		$(FC90) $(FC90OPT) -c time_reversal.F90

time_reversal_optimization.o: time_reversal_optimization.F90 accuracy.o constants.o projmat.o
		$(FC90) $(FC90OPT) -c time_reversal_optimization.F90

sympw_pointgroup_data.o: sympw_pointgroup_data.F90 accuracy.o constants.o genera.o
		$(FC90) $(FC90OPT) -c sympw_pointgroup_data.F90

sympw_core.o: sympw_core.F90 accuracy.o constants.o sympw_group_mode.o groupkp.o irrep.o sumsets.o projmat.o time_reversal.o
		$(FC90) $(FC90OPT) -c sympw_core.F90

sympw_vasp.o: sympw_vasp.F90 accuracy.o sympw_pointgroup_data.o sympw_lib.o sympw_vasp_input.o sympw_cli_runner.o
		$(FC90) $(FC90OPT) -c sympw_vasp.F90

orbmain.o: orbmain.F90 accuracy.o constants.o genera.o sympw_pointgroup_data.o sympw_lib.o sympw_vasp_input.o sympw_cli_runner.o
		$(FC90) $(FC90OPT) -c orbmain.F90

sympw_mulliken_cubic.o: sympw_mulliken_cubic.F90 accuracy.o
		$(FC90) $(FC90OPT) -c sympw_mulliken_cubic.F90

sympw_mulliken.o: sympw_mulliken.F90 accuracy.o sympw_mulliken_cubic.o
		$(FC90) $(FC90OPT) -c sympw_mulliken.F90

sympw_mulliken_real_pairs.o: sympw_mulliken_real_pairs.F90 accuracy.o
		$(FC90) $(FC90OPT) -c sympw_mulliken_real_pairs.F90

sympw_lib.o: sympw_lib.F90 accuracy.o constants.o vasp_reader.o sympw_pointgroup_data.o sympw_core.o sympw_mulliken.o sympw_mulliken_real_pairs.o sympw_real_sh.o time_reversal_optimization.o
		$(FC90) $(FC90OPT) -c sympw_lib.F90

sympw_vasp_input.o: sympw_vasp_input.F90 accuracy.o constants.o genera.o sympw_config.o sympw_lib.o vasp_reader.o
		$(FC90) $(FC90OPT) -c sympw_vasp_input.F90

sympw_cli_runner.o: sympw_cli_runner.F90 accuracy.o sympw_lib.o time_reversal.o time_reversal_optimization.o
		$(FC90) $(FC90OPT) -c sympw_cli_runner.F90

sympw_real_sh.o: sympw_real_sh.F90 accuracy.o
		$(FC90) $(FC90OPT) -c sympw_real_sh.F90

libsympw.a: $(LIB_MODULES)
	ar rcs libsympw.a $(LIB_MODULES)

TEST_LIB_MODULES = accuracy.o constants.o sympw_group_mode.o sympw_phase.o message.o hall.o permu.o bztest.o groupkp.o sumsets.o projmat.o eigensolver.o intsec.o eigvec.o genera.o charac.o subsp.o classes.o degen.o repres.o irrep.o modsymprj.o vasp_reader.o time_reversal.o time_reversal_optimization.o sympw_pointgroup_data.o sympw_core.o sympw_mulliken_cubic.o sympw_mulliken.o sympw_mulliken_real_pairs.o sympw_lib.o sympw_real_sh.o

test_lib: $(TEST_LIB_MODULES) test_lib.o
	$(LN) $(FC90OPT) -o test_lib $(TEST_LIB_MODULES) test_lib.o $(LIBS)

test_lib.o: test_lib.F90 accuracy.o constants.o sympw_lib.o sympw_real_sh.o
		$(FC90) $(FC90OPT) -c test_lib.F90

TEST_REGRESSION_MODULES = accuracy.o constants.o sympw_group_mode.o sympw_phase.o sympw_config.o message.o hall.o permu.o bztest.o groupkp.o sumsets.o projmat.o eigensolver.o intsec.o eigvec.o genera.o charac.o subsp.o classes.o degen.o repres.o irrep.o modsymprj.o vasp_reader.o time_reversal.o time_reversal_optimization.o sympw_pointgroup_data.o sympw_core.o sympw_mulliken_cubic.o sympw_mulliken.o sympw_mulliken_real_pairs.o sympw_lib.o sympw_vasp_input.o sympw_real_sh.o

test_regressions: $(TEST_REGRESSION_MODULES) test_regressions.o
	$(LN) $(FC90OPT) -o test_regressions $(TEST_REGRESSION_MODULES) test_regressions.o $(LIBS)

test_regressions.o: test_regressions.F90 accuracy.o constants.o sympw_group_mode.o sympw_phase.o sympw_config.o sympw_lib.o sympw_vasp_input.o sympw_real_sh.o sumsets.o modsymprj.o vasp_reader.o
		$(FC90) $(FC90OPT) -c test_regressions.F90

test_frontends: sympw sympw_vasp
	./sympw examples/legacy/C1.in > /dev/null
	./sympw examples/legacy/C1_cartesian_tr.in > /dev/null
	! ./sympw examples/legacy/C1_nfacto_unsupported.in > /tmp/sympw-c1-nfacto.out 2>&1
	grep -q "Legacy nfacto expansion is undefined" /tmp/sympw-c1-nfacto.out
	./sympw examples/legacy/C1_verbose.in > /tmp/sympw-c1-verbose.out
	grep -q "Group Multiplication Table" /tmp/sympw-c1-verbose.out
	rm -f /tmp/sympw-c1-nfacto.out /tmp/sympw-c1-verbose.out
	cd examples/GaAs && ../../sympw sympw.conf > /dev/null
	cd examples/GaAs && ../../sympw_vasp sympw.conf > /dev/null
	cd examples/GaAs && ../../sympw_vasp POSCAR KPOINTS 1 > /dev/null
	cd examples/GaAs && ! ../../sympw_vasp POSCAR KPOINTS 1 1 1 > /dev/null 2>&1
	cd examples/Graphene && ../../sympw POSCAR > /dev/null
	cd examples/Graphene && ../../sympw_vasp POSCAR KPOINTS 1 > /dev/null
	cd examples/Graphene && ! ../../sympw_vasp POSCAR > /dev/null 2>&1

clean:
	rm -f *.mod *.o 
