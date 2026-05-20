program sympw_vasp
  ! ============================================
  ! Symmetry Projection with VASP input
  ! ============================================
  ! Usage:
  !   sympw_vasp POSCAR KPOINTS [lmax1 lmax2 ...]
  !   sympw_vasp sympw.conf
  !
  ! Example:
  !   sympw_vasp POSCAR KPOINTS 2 2
  !   sympw_vasp sympw.conf
  ! ============================================

  use accuracy
  use constants
  use vasp_reader
  use modsymprj
  use genera, only: sym_matinv
  implicit none

  ! VASP input
  character(len=256) :: poscar_file, kpoints_file, comment
  real(dp) :: scale
  real(dp) :: lattice(3,3), bi(3,3), ai(3,3)
  character(len=2), allocatable :: elements(:)
  integer, allocatable :: nat_per_elem(:)
  real(dp), allocatable :: positions(:,:)
  logical :: is_cartesian
  integer :: nel, total_atoms

  ! K-points
  real(dp), allocatable :: kpoints(:,:)
  character(len=20), allocatable :: kpoint_names(:)
  integer :: nkpts
  character(len=20) :: kpt_mode

  ! Symmetry calculation
  integer :: pgnr
  character(len=10) :: pg_name = ''
  integer, allocatable :: lmax(:)
  real(dp), allocatable :: r(:,:,:)

  ! Command line
  integer :: nargs, i, ios
  character(len=256) :: arg
  logical :: use_config_file

  ! Get command line arguments
  nargs = command_argument_count()

  if (nargs < 1) then
     write(*,*) "Usage:"
     write(*,*) "  sympw_vasp POSCAR KPOINTS [lmax1 lmax2 ...]"
     write(*,*) "  sympw_vasp sympw.conf"
     write(*,*)
     write(*,*) "Examples:"
     write(*,*) "  sympw_vasp POSCAR KPOINTS 2 2"
     write(*,*) "  sympw_vasp POSCAR KPOINTS     # default lmax=2 for all"
     write(*,*)
     write(*,*) "Config file format (sympw.conf):"
     write(*,*) "  POSCAR   POSCAR"
     write(*,*) "  KPOINTS  KPOINTS"
     write(*,*) "  LMAX     2 2"
     write(*,*) "  POINTGROUP  Oh  # optional"
     error stop
  end if

  ! Check if using config file
  call get_command_argument(1, arg)
  use_config_file = (index(arg, '.conf') > 0 .or. index(arg, '.cfg') > 0)

  if (.not. use_config_file .and. nargs < 2) then
     write(*,*) "Error: Need at least POSCAR and KPOINTS files"
     write(*,*) "Usage: sympw_vasp POSCAR KPOINTS [lmax1 lmax2 ...]"
     error stop
  end if

  if (use_config_file) then
     ! Read from config file
     call read_config_file(arg, poscar_file, kpoints_file, lmax, pg_name)
     if (len_trim(poscar_file) == 0 .or. len_trim(kpoints_file) == 0) then
        write(*,*) "Error: POSCAR and KPOINTS must be specified in config file"
        error stop
     end if
  else
     ! Read from command line
     call get_command_argument(1, poscar_file)
     call get_command_argument(2, kpoints_file)
  end if

  ! Read POSCAR
  call read_poscar(poscar_file, comment, scale, lattice, elements, &
                   nat_per_elem, positions, is_cartesian, nel, total_atoms)

  ! Allocate lmax if not set
  if (.not. allocated(lmax)) then
     allocate(lmax(nel))
     if (nargs >= 2 + nel) then
        ! Read lmax from command line
        do i = 1, nel
           call get_command_argument(2 + i, arg)
           read(arg, *) lmax(i)
        end do
     else
        ! Default lmax = 2 (s, p, d)
        lmax(:) = 2
        write(*,*) "Using default lmax = 2 for all elements"
     end if
  end if

  write(*,*) "lmax for each element:", lmax

  ! Read KPOINTS
  call read_kpoints(kpoints_file, kpoints, kpoint_names, nkpts, kpt_mode)

  ! Detect or read point group
  if (len_trim(pg_name) == 0) then
     pg_name = detect_point_group(lattice)
     write(*,*) "Auto-detected point group:", trim(pg_name)
  else
     write(*,*) "Using specified point group:", trim(pg_name)
  end if
  pgnr = point_group_name_to_number(pg_name)
  write(*,*) "Point group number:", pgnr

  ! Convert positions if cartesian
  if (is_cartesian) then
     write(*,*) "Converting Cartesian to fractional coordinates..."
     ! Calculate inverse lattice
     bi = lattice
     call sym_matinv(bi, 3)
     do i = 1, total_atoms
        positions(i,:) = matmul(positions(i,:), bi)
     end do
  end if

  ! Reorganize positions by element
  allocate(r(nel, maxval(nat_per_elem), 3))
  call reorganize_positions(positions, nat_per_elem, nel, r)

  ! Calculate reciprocal lattice
  bi = lattice
  call sym_matinv(bi, 3)
  ai = transpose(lattice)

  write(*,*)
  write(*,*) "=========================================="
  write(*,*) "Starting symmetry projection calculation"
  write(*,*) "=========================================="
  write(*,*)

  ! Call main symmetry projection routine
  ! (This would call your existing modsymprj module)
  ! call symprj(...)

  write(*,*)
  write(*,*) "=========================================="
  write(*,*) "Calculation complete!"
  write(*,*) "=========================================="

contains

  subroutine read_config_file(filename, poscar, kpoints, lmax, pg)
    character(len=*), intent(in) :: filename
    character(len=*), intent(out) :: poscar, kpoints, pg
    integer, allocatable, intent(out) :: lmax(:)

    integer :: fh, ios, nlmax, i
    character(len=256) :: line, keyword
    integer, allocatable :: temp_lmax(:)

    allocate(temp_lmax(100))  ! Temporary array

    fh = 20
    open(fh, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) then
       write(*,*) "Error: Cannot open config file:", trim(filename)
       error stop
    end if

    poscar = ""
    kpoints = ""
    pg = ""
    nlmax = 0

    do
       read(fh, '(A)', iostat=ios) line
       if (ios /= 0) exit

       line = adjustl(line)
       if (len_trim(line) == 0) cycle
       if (line(1:1) == '#') cycle

       read(line, *) keyword

       select case(trim(keyword))
       case('POSCAR')
          read(line, *) keyword, poscar
       case('KPOINTS')
          read(line, *) keyword, kpoints
       case('LMAX')
          ! Read all lmax values
          temp_lmax = 0
          read(line, *, iostat=ios) keyword, temp_lmax(1:100)
          ! Count how many were read
          do i = 1, 100
             if (temp_lmax(i) /= 0) nlmax = i
          end do
       case('POINTGROUP', 'POINT_GROUP')
          read(line, *) keyword, pg
       end select
    end do

    close(fh)

    if (nlmax > 0) then
       allocate(lmax(nlmax))
       lmax = temp_lmax(1:nlmax)
    end if

    deallocate(temp_lmax)

  end subroutine read_config_file

  subroutine reorganize_positions(pos_flat, nat, nel, r)
    real(dp), intent(in) :: pos_flat(:,:)
    integer, intent(in) :: nat(:), nel
    real(dp), intent(out) :: r(:,:,:)

    integer :: iel, iat, idx

    idx = 1
    do iel = 1, nel
       do iat = 1, nat(iel)
          r(iel, iat, :) = pos_flat(idx, :)
          idx = idx + 1
       end do
    end do

  end subroutine reorganize_positions

end program sympw_vasp
