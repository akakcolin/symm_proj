program sympw_vasp
  use accuracy, only: dp
  use sympw_cli_runner, only: run_sympw_kpoint_batch
  use sympw_lib
  use sympw_pointgroup_data, only: point_group_number_to_name
  use sympw_vasp_input, only: resolve_vasp_command_line, load_vasp_crystal_input
  implicit none

  type(sympw_crystal_t) :: crystal
  type(sympw_cell_info_t) :: cell_info
  real(dp), allocatable :: kpoints(:,:)
  character(len=20), allocatable :: kpoint_names(:)
  integer, allocatable :: lmax_values(:)
  character(len=256) :: first_argument, poscar_file, kpoints_file, comment
  character(len=10) :: point_group_name
  integer :: argument_status, input_error, init_error, info_error
  logical :: batch_ok

  call get_command_argument(1, first_argument, status=argument_status)
  if (argument_status /= 0 .or. len_trim(first_argument) == 0) then
     call print_usage()
     error stop "Missing input files"
  end if

  call resolve_vasp_command_line(first_argument, .true., poscar_file, kpoints_file, &
       lmax_values, point_group_name, input_error)
  if (input_error /= 0) then
     call print_usage()
     error stop "Unable to resolve VASP input arguments"
  end if
  call load_vasp_crystal_input(poscar_file, kpoints_file, lmax_values, &
       point_group_name, crystal, kpoints, kpoint_names, comment, input_error)
  if (input_error /= 0) then
     write(*,*) "sympw_vasp: input error", input_error
     error stop "Unable to load VASP input"
  end if

  call sympw_set_verbosity(1)
  call sympw_init(crystal, init_error)
  if (init_error /= 0) then
     write(*,*) "sympw_vasp: library initialization error", init_error
     error stop "Unable to initialize symmetry analysis"
  end if
  call sympw_get_cell_info(cell_info, info_error)
  if (info_error /= 0) error stop "Unable to query canonical cell metadata"

  write(*,*)
  write(*,*) "=========================================="
  write(*,*) "Symmetry Projection with VASP Input"
  write(*,*) "=========================================="
  write(*,'(A,A)') " Structure: ", trim(comment)
  write(*,'(A,A,A,I0,A)') " Point group: ", &
       trim(point_group_number_to_name(cell_info%point_group_number)), &
       " (", cell_info%point_group_number, ")"
  write(*,'(A,L1)') " Primitive-cell reduction applied: ", cell_info%reduced
  write(*,'(A,I0)') " Internal atom count: ", sum(cell_info%nat)
  write(*,'(A,I0)') " Orbital basis dimension: ", cell_info%basis_dimension
  write(*,'(A,I0)') " K-points: ", size(kpoints, 1)

  call run_sympw_kpoint_batch(kpoints, kpoint_names, cell_info%basis_dimension, &
       .false., batch_ok)
  call sympw_finalize()
  if (.not. batch_ok) error stop "K-point symmetry analysis failed"

contains

  subroutine print_usage()
    write(*,*) "Usage:"
    write(*,*) "  sympw_vasp POSCAR KPOINTS [lmax1 lmax2 ...]"
    write(*,*) "  sympw_vasp sympw.conf"
  end subroutine print_usage

end program sympw_vasp
