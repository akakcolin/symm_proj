module sympw_vasp_input
  use accuracy, only: dp
  use constants, only: maxL
  use genera, only: sym_matinv
  use sympw_config, only: read_sympw_config
  use sympw_lib, only: sympw_crystal_t
  use vasp_reader, only: read_poscar, read_kpoints, point_group_name_to_number
  implicit none
  private

  public :: resolve_vasp_command_line
  public :: load_vasp_crystal_input

contains

  subroutine resolve_vasp_command_line(first_argument, require_kpoints_argument, &
       poscar_file, kpoints_file, lmax_values, point_group_name, error_code)
    character(len=*), intent(in) :: first_argument
    logical, intent(in) :: require_kpoints_argument
    character(len=256), intent(out) :: poscar_file, kpoints_file
    integer, allocatable, intent(out) :: lmax_values(:)
    character(len=10), intent(out) :: point_group_name
    integer, intent(out) :: error_code

    character(len=256) :: argument
    integer :: argument_count, lmax_count, value_index, io_status
    logical :: use_config_file

    error_code = 0
    poscar_file = ""
    kpoints_file = ""
    point_group_name = ""
    argument_count = command_argument_count()
    use_config_file = index(first_argument, '.conf') > 0 .or. &
         index(first_argument, '.cfg') > 0
    if (use_config_file) then
       call read_sympw_config(first_argument, poscar_file, kpoints_file, &
            lmax_values, point_group_name, error_code)
       return
    end if

    poscar_file = first_argument
    if (argument_count >= 2) then
       call get_command_argument(2, kpoints_file)
    else if (require_kpoints_argument) then
       write(*,*) "POSCAR and KPOINTS arguments are required"
       error_code = 2
       return
    else
       kpoints_file = "KPOINTS"
    end if

    lmax_count = max(0, argument_count - 2)
    if (lmax_count == 0) return
    allocate(lmax_values(lmax_count), stat=io_status)
    if (io_status /= 0) then
       error_code = 3
       return
    end if
    do value_index = 1, lmax_count
       call get_command_argument(2 + value_index, argument)
       read(argument, *, iostat=io_status) lmax_values(value_index)
       if (io_status /= 0) then
          write(*,*) "Invalid LMAX argument: ", trim(argument)
          deallocate(lmax_values)
          error_code = 4
          return
       end if
    end do
  end subroutine resolve_vasp_command_line


  subroutine load_vasp_crystal_input(poscar_file, kpoints_file, lmax_values, &
       point_group_name, crystal, kpoints, kpoint_names, comment, error_code)
    character(len=*), intent(in) :: poscar_file, kpoints_file
    integer, allocatable, intent(inout) :: lmax_values(:)
    character(len=*), intent(in) :: point_group_name
    type(sympw_crystal_t), intent(out) :: crystal
    real(dp), allocatable, intent(out) :: kpoints(:,:)
    character(len=20), allocatable, intent(out) :: kpoint_names(:)
    character(len=256), intent(out) :: comment
    integer, intent(out) :: error_code

    character(len=20) :: kpoint_mode
    character(len=2), allocatable :: elements(:)
    integer, allocatable :: nat_per_element(:)
    real(dp), allocatable :: positions(:,:)
    real(dp) :: lattice(3,3), inverse_lattice(3,3), reciprocal_conversion(3,3)
    real(dp) :: scale
    integer :: input_error, nel, total_atoms, nkpts
    integer :: element_index, atom_index, flat_index, kpoint_index, alloc_stat
    logical :: positions_cartesian, kpoints_cartesian

    error_code = 0
    comment = ""
    call read_poscar(poscar_file, comment, scale, lattice, elements, nat_per_element, &
         positions, positions_cartesian, nel, total_atoms, input_error)
    if (input_error /= 0) then
       error_code = 10 + input_error
       return
    end if
    call normalize_lmax(lmax_values, nel, input_error)
    if (input_error /= 0) then
       error_code = 20 + input_error
       return
    end if
    call read_kpoints(kpoints_file, kpoints, kpoint_names, nkpts, kpoint_mode, &
         kpoints_cartesian, input_error)
    if (input_error /= 0) then
       error_code = 30 + input_error
       return
    end if

    if (positions_cartesian) then
       inverse_lattice(:, :) = lattice(:, :)
       call sym_matinv(inverse_lattice, 3)
       do atom_index = 1, total_atoms
          positions(atom_index, :) = matmul(positions(atom_index, :), inverse_lattice)
       end do
    end if
    if (kpoints_cartesian) then
       ! VASP Cartesian KPOINTS are expressed in units of 2*pi/scale.
       reciprocal_conversion(:, :) = transpose(lattice) / scale
       do kpoint_index = 1, nkpts
          kpoints(kpoint_index, :) = &
               matmul(kpoints(kpoint_index, :), reciprocal_conversion)
       end do
    end if

    crystal%lattice(:, :) = lattice(:, :)
    crystal%nel = nel
    crystal%pgnr = 0
    if (len_trim(point_group_name) > 0) then
       crystal%pgnr = point_group_name_to_number(point_group_name)
    end if
    allocate(crystal%nat(nel), crystal%lmax(nel), &
         crystal%pos_frac(3, nel, maxval(nat_per_element)), stat=alloc_stat)
    if (alloc_stat /= 0) then
       if (allocated(crystal%nat)) deallocate(crystal%nat)
       if (allocated(crystal%lmax)) deallocate(crystal%lmax)
       if (allocated(crystal%pos_frac)) deallocate(crystal%pos_frac)
       error_code = 40
       return
    end if
    crystal%nat(:) = nat_per_element(:)
    crystal%lmax(:) = lmax_values(:)
    crystal%pos_frac(:, :, :) = 0.0_dp
    flat_index = 1
    do element_index = 1, nel
       do atom_index = 1, nat_per_element(element_index)
          crystal%pos_frac(:, element_index, atom_index) = positions(flat_index, :)
          flat_index = flat_index + 1
       end do
    end do
  end subroutine load_vasp_crystal_input


  subroutine normalize_lmax(lmax_values, element_count, error_code)
    integer, allocatable, intent(inout) :: lmax_values(:)
    integer, intent(in) :: element_count
    integer, intent(out) :: error_code

    integer :: configured_lmax, alloc_stat

    error_code = 0
    if (.not. allocated(lmax_values)) then
       allocate(lmax_values(element_count), stat=alloc_stat)
       if (alloc_stat /= 0) then
          error_code = 1
          return
       end if
       lmax_values(:) = 2
       write(*,*) "Using default LMAX = 2 for all elements"
    else if (size(lmax_values) == 1 .and. element_count > 1) then
       configured_lmax = lmax_values(1)
       deallocate(lmax_values)
       allocate(lmax_values(element_count), stat=alloc_stat)
       if (alloc_stat /= 0) then
          error_code = 1
          return
       end if
       lmax_values(:) = configured_lmax
    else if (size(lmax_values) /= element_count) then
       write(*,*) "LMAX count must be 1 or match the number of elements"
       error_code = 2
       return
    end if

    if (any(lmax_values < 0) .or. any(lmax_values > maxL)) then
       write(*,*) "LMAX values must be in the range 0..", maxL
       error_code = 3
    end if
  end subroutine normalize_lmax

end module sympw_vasp_input
