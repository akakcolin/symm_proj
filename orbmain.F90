program main
  use accuracy, only: dp
  use constants, only: maxL, npgodat
  use genera, only: sym_matinv
  use sympw_cli_runner, only: run_sympw_kpoint_batch
  use sympw_lib
  use sympw_pointgroup_data, only: point_group_number_to_name
  use sympw_vasp_input, only: resolve_vasp_command_line, load_vasp_crystal_input
  implicit none

  type(sympw_crystal_t) :: crystal
  type(sympw_cell_info_t) :: cell_info
  real(dp), allocatable :: all_kpoints(:,:)
  character(len=20), allocatable :: kpoint_names(:)
  character(len=256) :: input_file
  integer :: argument_status, init_error, info_error, verbosity_level
  logical :: use_config_file, use_vasp_format, print_projectors, batch_ok

  call get_command_argument(1, input_file, status=argument_status)
  if (argument_status /= 0 .or. len_trim(input_file) == 0) then
     call print_usage()
     error stop "Missing input file"
  end if

  use_config_file = index(input_file, '.conf') > 0 .or. index(input_file, '.cfg') > 0
  use_vasp_format = use_config_file .or. index(input_file, 'POSCAR') > 0 .or. &
       index(input_file, 'CONTCAR') > 0
  if (use_vasp_format) then
     call read_vasp_input(input_file, crystal, all_kpoints, kpoint_names)
     print_projectors = .false.
     verbosity_level = 1
  else
     call read_legacy_input(input_file, crystal, all_kpoints, kpoint_names, &
          print_projectors, verbosity_level)
  end if

  if (any(crystal%lmax < 0) .or. any(crystal%lmax > maxL)) then
     error stop "LMAX values must be in the supported range"
  end if

  call sympw_set_verbosity(verbosity_level)
  call sympw_init(crystal, init_error)
  if (init_error /= 0) then
     write(*,*) "sympw: library initialization error", init_error
     error stop "Unable to initialize symmetry analysis"
  end if
  call sympw_get_cell_info(cell_info, info_error)
  if (info_error /= 0) error stop "Unable to query canonical cell metadata"

  write(*,*)
  write(*,*) "=========================================="
  write(*,*) "Symmetry Projection"
  write(*,*) "=========================================="
  write(*,'(A,A,A,I0,A)') " Point group: ", &
       trim(point_group_number_to_name(cell_info%point_group_number)), &
       " (", cell_info%point_group_number, ")"
  write(*,'(A,L1)') " Primitive-cell reduction applied: ", cell_info%reduced
  write(*,'(A,I0)') " Internal atom count: ", sum(cell_info%nat)
  write(*,'(A,I0)') " Orbital basis dimension: ", cell_info%basis_dimension
  write(*,'(A,I0)') " K-points: ", size(all_kpoints, 1)

  call run_sympw_kpoint_batch(all_kpoints, kpoint_names, cell_info%basis_dimension, &
       print_projectors, batch_ok)
  call sympw_finalize()
  if (.not. batch_ok) error stop "K-point symmetry analysis failed"

contains

  subroutine read_vasp_input(filename, output_crystal, kpoints, names)
    character(len=*), intent(in) :: filename
    type(sympw_crystal_t), intent(out) :: output_crystal
    real(dp), allocatable, intent(out) :: kpoints(:,:)
    character(len=20), allocatable, intent(out) :: names(:)

    character(len=256) :: poscar_file, kpoints_file, comment
    character(len=10) :: point_group_name
    integer, allocatable :: input_lmax(:)
    integer :: input_error

    write(*,*) "=========================================="
    write(*,*) "Using VASP format input"
    write(*,*) "=========================================="
    call resolve_vasp_command_line(filename, .false., poscar_file, kpoints_file, &
         input_lmax, point_group_name, input_error)
    if (input_error /= 0) error stop "Unable to resolve VASP input arguments"
    call load_vasp_crystal_input(poscar_file, kpoints_file, input_lmax, &
         point_group_name, output_crystal, kpoints, names, comment, input_error)
    if (input_error /= 0) then
       write(*,*) "sympw: VASP input error", input_error
       error stop "Unable to load VASP input"
    end if
  end subroutine read_vasp_input


  subroutine read_legacy_input(filename, output_crystal, kpoints, names, &
       print_projector_output, output_verbosity)
    character(len=*), intent(in) :: filename
    type(sympw_crystal_t), intent(out) :: output_crystal
    real(dp), allocatable, intent(out) :: kpoints(:,:)
    character(len=20), allocatable, intent(out) :: names(:)
    logical, intent(out) :: print_projector_output
    integer, intent(out) :: output_verbosity

    integer :: file_unit, io_status, steer(20), point_group_number, nel
    integer :: element_index, atom_index, coordinate_flag, operation_index
    integer :: order, nkpts, kpoint_index, last_flag, wavevector_flag, nfacto
    real(dp) :: lattice(3,3), inverse_lattice(3,3), cartesian_to_fractional(3,3)
    real(dp) :: coordinates(3), legacy_translation(3)

    open(newunit=file_unit, file=filename, status='old', action='read', iostat=io_status)
    if (io_status /= 0) error stop "Unable to open legacy input file"
    call read_legacy_integers(file_unit, steer, "steering flags")
    do element_index = 1, 3
       call read_legacy_reals(file_unit, lattice(element_index, :), "lattice vector")
    end do
    call read_legacy_integer(file_unit, point_group_number, "point group number")
    if (point_group_number < 1 .or. point_group_number > 36) then
       error stop "Legacy point group number must be in the range 1..36"
    end if
    call read_legacy_integer(file_unit, nel, "element count")
    if (nel < 1) error stop "Legacy element count must be positive"

    output_crystal%lattice(:, :) = lattice(:, :)
    output_crystal%nel = nel
    output_crystal%pgnr = point_group_number
    allocate(output_crystal%lmax(nel), output_crystal%nat(nel))
    do element_index = 1, nel
       call read_legacy_integer(file_unit, output_crystal%lmax(element_index), &
            "LMAX value")
    end do
    do element_index = 1, nel
       call read_legacy_integer(file_unit, output_crystal%nat(element_index), &
            "atom count")
    end do
    if (any(output_crystal%nat < 1)) error stop "Legacy atom counts must be positive"

    inverse_lattice(:, :) = lattice(:, :)
    call sym_matinv(inverse_lattice, 3)
    cartesian_to_fractional(:, :) = transpose(inverse_lattice)
    allocate(output_crystal%pos_frac(3, nel, maxval(output_crystal%nat)))
    output_crystal%pos_frac(:, :, :) = 0.0_dp
    do element_index = 1, nel
       do atom_index = 1, output_crystal%nat(element_index)
          call read_legacy_integer(file_unit, coordinate_flag, "atomic coordinate flag")
          call read_legacy_reals(file_unit, coordinates, "atomic coordinates")
          if (coordinate_flag == 1) then
             coordinates(:) = matmul(cartesian_to_fractional, coordinates)
          end if
          output_crystal%pos_frac(:, element_index, atom_index) = coordinates(:)
       end do
    end do

    order = npgodat(point_group_number)
    if (steer(20) == 0) then
       do operation_index = 1, order
          call read_legacy_integer(file_unit, coordinate_flag, "translation coordinate flag")
          call read_legacy_reals(file_unit, legacy_translation, "nonprimitive translation")
       end do
       if (order > 0) then
          write(*,*) "Legacy translation table consumed; translations are re-derived from the structure"
       end if
    else
       write(*,*) "Legacy group-mode override is ignored; the physical factor group is selected automatically"
    end if

    call read_legacy_integer(file_unit, nkpts, "number of wave vectors")
    if (nkpts < 1) error stop "Legacy input must contain at least one wave vector"
    allocate(kpoints(nkpts, 3), names(nkpts))
    do kpoint_index = 1, nkpts
       call read_legacy_integer(file_unit, last_flag, "last-wave-vector flag")
       call read_legacy_integer(file_unit, wavevector_flag, "wave-vector coordinate flag")
       call read_legacy_reals(file_unit, coordinates, "wave vector")
       if (wavevector_flag == 1) then
          coordinates(:) = matmul(transpose(lattice), coordinates)
       end if
       kpoints(kpoint_index, :) = coordinates(:)
       call read_legacy_integer(file_unit, nfacto, "same-direction factor count")
       if ((kpoint_index < nkpts .and. last_flag /= 0) .or. &
            (kpoint_index == nkpts .and. last_flag /= 1)) then
          error stop "Legacy wave-vector count and last flag are inconsistent"
       end if
       if (nfacto /= 0) then
          error stop "Legacy nfacto expansion is undefined; list all k-points explicitly"
       end if
       write(names(kpoint_index), '(A,I0)') "K", kpoint_index
    end do
    close(file_unit)
    call interpret_legacy_steer(steer, print_projector_output, output_verbosity)
  end subroutine read_legacy_input


  subroutine interpret_legacy_steer(steer, print_projector_output, output_verbosity)
    integer, intent(in) :: steer(20)
    logical, intent(out) :: print_projector_output
    integer, intent(out) :: output_verbosity

    integer :: steer_index

    print_projector_output = steer(1) /= 0
    output_verbosity = 1
    if (any(steer(6:9) /= 0) .or. steer(12) /= 0 .or. steer(18) /= 0) then
       output_verbosity = 3
    end if
    if (steer(11) /= 0) then
       write(*,*) "Legacy steer(11) is an internal validity flag and is ignored"
    end if
    do steer_index = 1, size(steer)
       select case (steer_index)
       case (1, 2, 6, 7, 8, 9, 11, 12, 18, 20)
          cycle
       case default
          if (steer(steer_index) /= 0) then
             write(*,'(A,I0,A)') " Legacy steer(", steer_index, ") is unsupported and ignored"
          end if
       end select
    end do
  end subroutine interpret_legacy_steer


  subroutine read_legacy_integer(file_unit, value, context)
    integer, intent(in) :: file_unit
    integer, intent(out) :: value
    character(len=*), intent(in) :: context
    integer :: io_status

    read(file_unit, *, iostat=io_status) value
    if (io_status /= 0) then
       write(*,*) "Unable to read legacy ", trim(context)
       error stop "Malformed legacy input"
    end if
  end subroutine read_legacy_integer


  subroutine read_legacy_integers(file_unit, values, context)
    integer, intent(in) :: file_unit
    integer, intent(out) :: values(:)
    character(len=*), intent(in) :: context
    integer :: io_status

    read(file_unit, *, iostat=io_status) values
    if (io_status /= 0) then
       write(*,*) "Unable to read legacy ", trim(context)
       error stop "Malformed legacy input"
    end if
  end subroutine read_legacy_integers


  subroutine read_legacy_reals(file_unit, values, context)
    integer, intent(in) :: file_unit
    real(dp), intent(out) :: values(:)
    character(len=*), intent(in) :: context
    integer :: io_status

    read(file_unit, *, iostat=io_status) values
    if (io_status /= 0) then
       write(*,*) "Unable to read legacy ", trim(context)
       error stop "Malformed legacy input"
    end if
  end subroutine read_legacy_reals


  subroutine print_usage()
    write(*,*) "Usage:"
    write(*,*) "  sympw legacy_input.in"
    write(*,*) "  sympw POSCAR [KPOINTS] [lmax1 lmax2 ...]"
    write(*,*) "  sympw sympw.conf"
  end subroutine print_usage

end program main
