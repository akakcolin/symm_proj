! ============================================
! VASP格式读取模块
! ============================================
! 直接读取VASP的POSCAR和KPOINTS文件
!
! 使用方法:
!   ./sympw POSCAR KPOINTS
!
! 或者使用配置文件:
!   ./sympw sympw.conf
!
! sympw.conf 格式:
!   POSCAR_FILE  POSCAR
!   KPOINTS_FILE KPOINTS
!   LMAX         2 2    # 每种元素的lmax
!   POINT_GROUP  Oh     # 可选，自动检测
! ============================================

module vasp_reader
  use accuracy
  use constants, only: pi
  implicit none
  private
  public :: read_poscar, read_kpoints, detect_point_group, point_group_name_to_number

contains

  subroutine read_poscar(filename, comment, scale, lattice, elements, &
                        nat_per_elem, positions, is_cartesian, nel, total_atoms)
    character(len=*), intent(in) :: filename
    character(len=256), intent(out) :: comment
    real(dp), intent(out) :: scale
    real(dp), intent(out) :: lattice(3,3)
    character(len=2), allocatable, intent(out) :: elements(:)
    integer, allocatable, intent(out) :: nat_per_elem(:)
    real(dp), allocatable, intent(out) :: positions(:,:)
    logical, intent(out) :: is_cartesian
    integer, intent(out) :: nel, total_atoms

    integer :: fh, ios, i, j, atom_idx
    character(len=256) :: line
    character(len=1) :: coord_type

    fh = 10
    open(fh, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) then
       write(*,*) "Error: Cannot open POSCAR file: ", trim(filename)
       error stop
    end if

    ! Line 1: Comment
    read(fh, '(A)') comment

    ! Line 2: Scale factor
    read(fh, *) scale

    ! Lines 3-5: Lattice vectors
    do i = 1, 3
       read(fh, *) lattice(i, 1:3)
       lattice(i, :) = lattice(i, :) * scale
    end do

    ! Line 6: Element names
    read(fh, '(A)') line
    ! Count elements
    nel = 0
    do i = 1, len_trim(line)
       if (line(i:i) /= ' ' .and. (i == 1 .or. line(i-1:i-1) == ' ')) then
          nel = nel + 1
       end if
    end do

    allocate(elements(nel))
    read(line, *) elements(:)

    ! Line 7: Number of atoms per element
    allocate(nat_per_elem(nel))
    read(fh, *) nat_per_elem(:)

    total_atoms = sum(nat_per_elem)

    ! Line 8: Coordinate type (Selective dynamics or Direct/Cartesian)
    read(fh, '(A)') line
    line = adjustl(line)
    coord_type = line(1:1)

    ! Check if Selective dynamics
    if (coord_type == 'S' .or. coord_type == 's') then
       ! Skip selective dynamics line, read next line for coord type
       read(fh, '(A)') line
       line = adjustl(line)
       coord_type = line(1:1)
    end if

    is_cartesian = (coord_type == 'C' .or. coord_type == 'c' .or. &
                    coord_type == 'K' .or. coord_type == 'k')

    ! Read atomic positions
    allocate(positions(total_atoms, 3))
    do i = 1, total_atoms
       read(fh, *) positions(i, 1:3)
    end do

    close(fh)

    write(*,*) "=========================================="
    write(*,*) "Read POSCAR: ", trim(comment)
    write(*,*) "=========================================="
    write(*,*) "Lattice vectors (Angstrom):"
    do i = 1, 3
       write(*,'(3F12.6)') lattice(i, :)
    end do
    write(*,*) "Elements:", (trim(elements(i))//" ", i=1,nel)
    write(*,*) "Atoms per element:", nat_per_elem
    write(*,*) "Total atoms:", total_atoms
    if (is_cartesian) then
       write(*,*) "Coordinates: Cartesian"
    else
       write(*,*) "Coordinates: Fractional"
    end if
    write(*,*)

  end subroutine read_poscar

  subroutine read_kpoints(filename, kpoints, kpoint_names, nkpts, kpt_mode)
    character(len=*), intent(in) :: filename
    real(dp), allocatable, intent(out) :: kpoints(:,:)
    character(len=20), allocatable, intent(out) :: kpoint_names(:)
    integer, intent(out) :: nkpts
    character(len=20), intent(out) :: kpt_mode

    integer :: fh, ios, i
    character(len=256) :: line, comment
    character(len=1) :: coord_type

    fh = 11
    open(fh, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) then
       write(*,*) "Error: Cannot open KPOINTS file: ", trim(filename)
       error stop
    end if

    ! Line 1: Comment
    read(fh, '(A)') comment

    ! Line 2: Number of k-points (0 = automatic)
    read(fh, *) nkpts

    ! Line 3: Mode (Automatic, Gamma, Monkhorst-Pack, or Line-mode)
    read(fh, '(A)') line
    line = adjustl(line)
    kpt_mode = trim(line)

    if (nkpts == 0) then
       ! Automatic k-point generation
       write(*,*) "Warning: Automatic k-point generation not supported"
       write(*,*) "Please use explicit k-points"
       nkpts = 1
       allocate(kpoints(1, 3))
       allocate(kpoint_names(1))
       kpoints(1, :) = [0.0_dp, 0.0_dp, 0.0_dp]
       kpoint_names(1) = "Gamma"
    else
       ! Explicit k-points
       ! Line 4: Coordinate type (optional)
       read(fh, '(A)', iostat=ios) line
       if (ios == 0) then
          line = adjustl(line)
          coord_type = line(1:1)
          ! If it's a coordinate type line, read it
          if (coord_type /= 'R' .and. coord_type /= 'r' .and. &
              coord_type /= 'C' .and. coord_type /= 'c' .and. &
              coord_type /= 'K' .and. coord_type /= 'k') then
             ! It's actually the first k-point, backspace
             backspace(fh)
          end if
       else
          backspace(fh)
       end if

       allocate(kpoints(nkpts, 3))
       allocate(kpoint_names(nkpts))

       do i = 1, nkpts
          read(fh, '(A)') line
          ! Try to read k-point with optional name
          read(line, *, iostat=ios) kpoints(i, 1:3)

          ! Try to extract name from comment
          if (index(line, '!') > 0) then
             kpoint_names(i) = adjustl(line(index(line, '!')+1:))
          else
             write(kpoint_names(i), '(A,I0)') "K", i
          end if
       end do
    end if

    close(fh)

    write(*,*) "=========================================="
    write(*,*) "Read KPOINTS"
    write(*,*) "=========================================="
    write(*,*) "Number of k-points:", nkpts
    write(*,*) "K-points:"
    do i = 1, nkpts
       write(*,'(A,3F10.4,2X,A)') "  ", kpoints(i,:), trim(kpoint_names(i))
    end do
    write(*,*)

  end subroutine read_kpoints

  function detect_point_group(lattice) result(pg_name)
    real(dp), intent(in) :: lattice(3,3)
    character(len=10) :: pg_name

    real(dp) :: lengths(3), angles(3)
    real(dp) :: tol
    integer :: i, j, k

    tol = 1.0e-3_dp

    ! Calculate lattice parameters
    do i = 1, 3
       lengths(i) = sqrt(sum(lattice(i,:)**2))
    end do

    ! Calculate angles
    do i = 1, 3
       j = mod(i, 3) + 1
       k = mod(i+1, 3) + 1
       angles(i) = acos(dot_product(lattice(j,:), lattice(k,:)) / &
                       (lengths(j) * lengths(k))) * 180.0_dp / pi
    end do

    ! Detect crystal system and assign point group

    ! Cubic
    if (abs(lengths(1) - lengths(2)) < tol .and. &
        abs(lengths(2) - lengths(3)) < tol .and. &
        abs(angles(1) - 90.0_dp) < tol .and. &
        abs(angles(2) - 90.0_dp) < tol .and. &
        abs(angles(3) - 90.0_dp) < tol) then
       pg_name = "Oh"
       return
    end if

    ! Hexagonal
    if (abs(lengths(1) - lengths(2)) < tol .and. &
        abs(angles(1) - 90.0_dp) < tol .and. &
        abs(angles(2) - 90.0_dp) < tol .and. &
        abs(angles(3) - 120.0_dp) < tol) then
       pg_name = "D6h"
       return
    end if

    ! Tetragonal
    if (abs(lengths(1) - lengths(2)) < tol .and. &
        abs(angles(1) - 90.0_dp) < tol .and. &
        abs(angles(2) - 90.0_dp) < tol .and. &
        abs(angles(3) - 90.0_dp) < tol) then
       pg_name = "D4h"
       return
    end if

    ! Orthorhombic
    if (abs(angles(1) - 90.0_dp) < tol .and. &
        abs(angles(2) - 90.0_dp) < tol .and. &
        abs(angles(3) - 90.0_dp) < tol) then
       pg_name = "D2h"
       return
    end if

    ! Default to lowest symmetry
    pg_name = "C1"

  end function detect_point_group

  function point_group_name_to_number(name) result(pgnr)
    character(len=*), intent(in) :: name
    integer :: pgnr
    integer :: ios

    select case(trim(name))
    case('C1');  pgnr = 1
    case('Ci', 'S2');  pgnr = 2
    case('C2');  pgnr = 3
    case('Cs');  pgnr = 4
    case('C2h'); pgnr = 5
    case('D2');  pgnr = 6
    case('C2v'); pgnr = 7
    case('D2h'); pgnr = 8
    case('C4');  pgnr = 9
    case('S4');  pgnr = 10
    case('C4h'); pgnr = 11
    case('D4');  pgnr = 12
    case('C4v'); pgnr = 13
    case('D2d'); pgnr = 14
    case('D4h'); pgnr = 15
    case('C3');  pgnr = 16
    case('C3i', 'S6'); pgnr = 17
    case('D3');  pgnr = 18
    case('C3v'); pgnr = 20
    case('D3d'); pgnr = 22
    case('C6');  pgnr = 24
    case('C3h'); pgnr = 25
    case('C6h'); pgnr = 26
    case('D6');  pgnr = 27
    case('C6v'); pgnr = 28
    case('D3h'); pgnr = 29
    case('D6h'); pgnr = 31
    case('T');   pgnr = 32
    case('Th');  pgnr = 33
    case('O');   pgnr = 34
    case('Td');  pgnr = 35
    case('Oh');  pgnr = 36
    case default
       ! Try to read as number
       read(name, *, iostat=ios) pgnr
       if (ios /= 0) then
          write(*,*) "Warning: Unknown point group '", trim(name), "', using C1"
          pgnr = 1
       end if
    end select
  end function point_group_name_to_number

end module vasp_reader
