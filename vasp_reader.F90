! ============================================
! VASP format reader module
! ============================================
! Reads VASP POSCAR and KPOINTS files directly.
!
! Usage (via sympw_vasp frontend):
!   ./sympw_vasp POSCAR KPOINTS [lmax1 lmax2 ...]
!
! Centered conventional cells are reduced to primitive cells
! when their centering translations can be detected from the
! atomic basis.
! ============================================

module vasp_reader
  use accuracy
  use constants, only: pi
  use genera, only: sym_matinv
  implicit none
  private
  public :: read_poscar, read_kpoints, reduce_centered_cell
  public :: detect_point_group, point_group_name_to_number

contains

  subroutine read_poscar(filename, comment, scale, lattice, elements, &
                        nat_per_elem, positions, is_cartesian, nel, total_atoms, error_code)
    character(len=*), intent(in) :: filename
    character(len=256), intent(out) :: comment
    real(dp), intent(out) :: scale
    real(dp), intent(out) :: lattice(3,3)
    character(len=2), allocatable, intent(out) :: elements(:)
    integer, allocatable, intent(out) :: nat_per_elem(:)
    real(dp), allocatable, intent(out) :: positions(:,:)
    logical, intent(out) :: is_cartesian
    integer, intent(out) :: nel, total_atoms
    integer, optional, intent(out) :: error_code

    integer :: fh, ios, i
    character(len=256) :: line
    character(len=1) :: coord_type
    real(dp) :: raw_volume, scale_factor
    logical :: file_open

    if (present(error_code)) error_code = 0
    comment = ''
    scale = 0.0_dp
    lattice(:, :) = 0.0_dp
    is_cartesian = .false.
    nel = 0
    total_atoms = 0
    file_open = .false.

    open(newunit=fh, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) then
       call report_poscar_error(1, "Cannot open POSCAR file: "//trim(filename))
       return
    end if
    file_open = .true.

    ! Line 1: Comment
    read(fh, '(A)', iostat=ios) comment
    if (ios /= 0) then
       call report_poscar_error(2, "POSCAR is missing the comment line")
       return
    end if

    ! Line 2: Scale factor
    read(fh, *, iostat=ios) scale
    if (ios /= 0) then
       call report_poscar_error(3, "POSCAR scale factor is missing or invalid")
       return
    end if

    ! Lines 3-5: Lattice vectors
    do i = 1, 3
       read(fh, *, iostat=ios) lattice(i, 1:3)
       if (ios /= 0) then
          call report_poscar_error(4, "POSCAR lattice vectors are incomplete or invalid")
          return
       end if
    end do

    raw_volume = abs(lattice(1,1) * (lattice(2,2) * lattice(3,3) - lattice(2,3) * lattice(3,2)) - &
         lattice(1,2) * (lattice(2,1) * lattice(3,3) - lattice(2,3) * lattice(3,1)) + &
         lattice(1,3) * (lattice(2,1) * lattice(3,2) - lattice(2,2) * lattice(3,1)))
    if (raw_volume <= tol_equal) then
       call report_poscar_error(5, "POSCAR lattice has zero volume")
       return
    end if
    if (abs(scale) <= tol_equal) then
       call report_poscar_error(6, "POSCAR scale factor must be nonzero")
       return
    else if (scale < 0.0_dp) then
       scale_factor = (abs(scale) / raw_volume)**(1.0_dp / 3.0_dp)
    else
       scale_factor = scale
    end if
    lattice(:, :) = lattice(:, :) * scale_factor
    scale = scale_factor

    ! Line 6: Element names
    read(fh, '(A)', iostat=ios) line
    if (ios /= 0 .or. len_trim(line) == 0) then
       call report_poscar_error(7, "POSCAR element-name line is missing")
       return
    end if
    ! Count elements
    nel = 0
    do i = 1, len_trim(line)
       if (line(i:i) /= ' ' .and. (i == 1 .or. line(i-1:i-1) == ' ')) then
          nel = nel + 1
       end if
    end do

    if (nel < 1) then
       call report_poscar_error(7, "POSCAR must contain at least one element")
       return
    end if

    allocate(elements(nel))
    read(line, *, iostat=ios) elements(:)
    if (ios /= 0) then
       call report_poscar_error(7, "POSCAR element-name line is invalid")
       return
    end if

    ! Line 7: Number of atoms per element
    allocate(nat_per_elem(nel))
    read(fh, *, iostat=ios) nat_per_elem(:)
    if (ios /= 0) then
       call report_poscar_error(8, "POSCAR atom-count line is missing or invalid")
       return
    end if
    if (any(nat_per_elem <= 0)) then
       call report_poscar_error(8, "POSCAR atom counts must be positive")
       return
    end if

    total_atoms = sum(nat_per_elem)

    ! Line 8: Coordinate type (Selective dynamics or Direct/Cartesian)
    read(fh, '(A)', iostat=ios) line
    if (ios /= 0 .or. len_trim(line) == 0) then
       call report_poscar_error(9, "POSCAR coordinate mode is missing")
       return
    end if
    line = adjustl(line)
    coord_type = line(1:1)

    ! Check if Selective dynamics
    if (coord_type == 'S' .or. coord_type == 's') then
       ! Skip selective dynamics line, read next line for coord type
       read(fh, '(A)', iostat=ios) line
       if (ios /= 0 .or. len_trim(line) == 0) then
          call report_poscar_error(9, "POSCAR coordinate mode is missing after Selective dynamics")
          return
       end if
       line = adjustl(line)
       coord_type = line(1:1)
    end if

    is_cartesian = (coord_type == 'C' .or. coord_type == 'c' .or. &
                    coord_type == 'K' .or. coord_type == 'k')

    if (.not. (coord_type == 'D' .or. coord_type == 'd' .or. is_cartesian)) then
       call report_poscar_error(9, "POSCAR coordinate mode must be Direct or Cartesian")
       return
    end if

    ! Read atomic positions
    allocate(positions(total_atoms, 3))
    do i = 1, total_atoms
       read(fh, *, iostat=ios) positions(i, 1:3)
       if (ios /= 0) then
          call report_poscar_error(10, "POSCAR atomic positions are incomplete or invalid")
          return
       end if
    end do
    if (is_cartesian) positions(:, :) = positions(:, :) * scale_factor

    close(fh)
    file_open = .false.

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

  contains

    subroutine report_poscar_error(code, message)
      integer, intent(in) :: code
      character(len=*), intent(in) :: message
      integer :: close_ios

      if (file_open) then
         close(fh, iostat=close_ios)
         file_open = .false.
      end if
      if (allocated(elements)) deallocate(elements)
      if (allocated(nat_per_elem)) deallocate(nat_per_elem)
      if (allocated(positions)) deallocate(positions)
      nel = 0
      total_atoms = 0
      if (present(error_code)) then
         error_code = code
         write(*,*) "Error: ", trim(message)
      else
         write(*,*) "Error: ", trim(message)
         error stop
      end if
    end subroutine report_poscar_error

  end subroutine read_poscar

  subroutine read_kpoints(filename, kpoints, kpoint_names, nkpts, kpt_mode, &
       kpoints_are_cartesian, error_code)
    character(len=*), intent(in) :: filename
    real(dp), allocatable, intent(out) :: kpoints(:,:)
    character(len=20), allocatable, intent(out) :: kpoint_names(:)
    integer, intent(out) :: nkpts
    character(len=20), intent(out) :: kpt_mode
    logical, optional, intent(out) :: kpoints_are_cartesian
    integer, optional, intent(out) :: error_code

    integer :: fh, ios, i
    character(len=256) :: line, comment
    character(len=1) :: coord_type
    real(dp) :: coordinate_probe(3)
    logical :: cartesian_mode, file_open

    coord_type = 'R'
    cartesian_mode = .false.
    file_open = .false.
    nkpts = 0
    kpt_mode = ''
    if (present(kpoints_are_cartesian)) kpoints_are_cartesian = .false.
    if (present(error_code)) error_code = 0

    open(newunit=fh, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) then
       call report_kpoints_error(1, "Cannot open KPOINTS file: "//trim(filename))
       return
    end if
    file_open = .true.

    ! Line 1: Comment
    read(fh, '(A)', iostat=ios) comment
    if (ios /= 0) then
       call report_kpoints_error(2, "KPOINTS is missing the comment line")
       return
    end if

    ! Line 2: Number of k-points (0 = automatic)
    read(fh, *, iostat=ios) nkpts
    if (ios /= 0) then
       call report_kpoints_error(3, "KPOINTS point count is missing or invalid")
       return
    end if
    if (nkpts == 0) then
       call report_kpoints_error(3, "Automatic KPOINTS meshes are not supported; provide explicit k-points")
       return
    else if (nkpts < 0) then
       call report_kpoints_error(3, "KPOINTS point count must be positive")
       return
    end if

    ! Line 3: Coordinate type for an explicit list, or Line-mode.
    read(fh, '(A)', iostat=ios) line
    if (ios /= 0 .or. len_trim(line) == 0) then
       call report_kpoints_error(4, "KPOINTS coordinate mode is missing")
       return
    end if
    line = adjustl(line)
    kpt_mode = trim(line)
    coord_type = line(1:1)
    if (coord_type == 'L' .or. coord_type == 'l') then
       call report_kpoints_error(4, "KPOINTS Line-mode is not supported; provide explicit k-points")
       return
    else if (coord_type == 'R' .or. coord_type == 'r') then
       cartesian_mode = .false.
    else if (coord_type == 'C' .or. coord_type == 'c' .or. &
             coord_type == 'K' .or. coord_type == 'k') then
       cartesian_mode = .true.
    else if (coord_type == 'G' .or. coord_type == 'g' .or. &
             coord_type == 'M' .or. coord_type == 'm' .or. &
             coord_type == 'A' .or. coord_type == 'a') then
       call report_kpoints_error(4, "Automatic KPOINTS modes are not supported; provide explicit k-points")
       return
    else
       read(line, *, iostat=ios) coordinate_probe(:)
       if (ios /= 0) then
          call report_kpoints_error(4, &
               "KPOINTS coordinate mode must be Reciprocal or Cartesian")
          return
       end if
       kpt_mode = "Reciprocal"
       cartesian_mode = .false.
       backspace(fh, iostat=ios)
       if (ios /= 0) then
          call report_kpoints_error(5, "Unable to reread the first implicit Reciprocal k-point")
          return
       end if
    end if

    allocate(kpoints(nkpts, 3), stat=ios)
    if (ios /= 0) then
       call report_kpoints_error(6, "KPOINTS coordinate allocation failed")
       return
    end if
    allocate(kpoint_names(nkpts), stat=ios)
    if (ios /= 0) then
       call report_kpoints_error(6, "KPOINTS name allocation failed")
       return
    end if

    do i = 1, nkpts
       read(fh, '(A)', iostat=ios) line
       if (ios /= 0) then
          call report_kpoints_error(5, "KPOINTS explicit coordinate list is incomplete")
          return
       end if
       read(line, *, iostat=ios) kpoints(i, 1:3)
       if (ios /= 0) then
          call report_kpoints_error(5, "Invalid explicit KPOINTS coordinate line")
          return
       end if

       if (index(line, '!') > 0) then
          kpoint_names(i) = adjustl(line(index(line, '!')+1:))
       else
          write(kpoint_names(i), '(A,I0)') "K", i
       end if
    end do

    close(fh)
    file_open = .false.

    if (present(kpoints_are_cartesian)) then
       kpoints_are_cartesian = cartesian_mode
    end if

    write(*,*) "=========================================="
    write(*,*) "Read KPOINTS"
    write(*,*) "=========================================="
    write(*,*) "Number of k-points:", nkpts
    if (cartesian_mode) then
       write(*,*) "K-point coordinates: Cartesian"
    else
       write(*,*) "K-point coordinates: Reciprocal fractional"
    end if
    write(*,*) "K-points:"
    do i = 1, nkpts
       write(*,'(A,3F10.4,2X,A)') "  ", kpoints(i,:), trim(kpoint_names(i))
    end do
    write(*,*)

  contains

    subroutine report_kpoints_error(code, message)
      integer, intent(in) :: code
      character(len=*), intent(in) :: message
      integer :: close_ios

      if (file_open) then
         close(fh, iostat=close_ios)
         file_open = .false.
      end if
      if (allocated(kpoints)) deallocate(kpoints)
      if (allocated(kpoint_names)) deallocate(kpoint_names)
      nkpts = 0
      kpt_mode = ''
      if (present(kpoints_are_cartesian)) kpoints_are_cartesian = .false.
      if (present(error_code)) then
         error_code = code
         write(*,*) "Error: ", trim(message)
      else
         write(*,*) "Error: ", trim(message)
         error stop
      end if
    end subroutine report_kpoints_error

  end subroutine read_kpoints

  subroutine reduce_centered_cell(lattice, positions, nat_per_elem, nel, total_atoms, &
       & k_transform, reduced, verbosity)
    real(dp), intent(inout) :: lattice(3,3)
    real(dp), allocatable, intent(inout) :: positions(:,:)
    integer, allocatable, intent(inout) :: nat_per_elem(:)
    integer, intent(in) :: nel
    integer, intent(inout) :: total_atoms
    real(dp), intent(out) :: k_transform(3,3)
    logical, intent(out) :: reduced
    integer, intent(in), optional :: verbosity

    real(dp), allocatable :: translations(:,:)
    real(dp), allocatable :: new_positions(:,:)
    integer, allocatable :: new_nat(:)
    real(dp) :: candidate(3), cell_transform(3,3), new_lattice(3,3)
    real(dp) :: inv_new_lattice(3,3), ai_new(3,3), cart(3), frac_new(3)
    integer :: i, atom_index, ntrans, old_offset, new_total, new_start
    integer :: element_index, atom_in_element
    integer :: out_level
    character(len=1) :: centering

    out_level = 1
    if (present(verbosity)) out_level = max(0, verbosity)
    k_transform(:,:) = 0.0_dp
    do i = 1, 3
       k_transform(i, i) = 1.0_dp
    end do
    reduced = .false.
    if (nel < 1 .or. total_atoms <= nel) return

    allocate(translations(total_atoms, 3))
    translations(:,:) = 0.0_dp
    ntrans = 1

    do atom_index = 1, nat_per_elem(1)
       candidate(:) = canonical_fractional(positions(atom_index, :) - positions(1, :))
       if (translation_exists(candidate, translations, ntrans)) cycle
       if (translation_maps_structure(candidate, positions, nat_per_elem, nel)) then
          ntrans = ntrans + 1
          translations(ntrans, :) = candidate(:)
       end if
    end do

    if (ntrans <= 1) then
       deallocate(translations)
       return
    end if

    cell_transform(:,:) = 0.0_dp
    centering = 'P'

    if (ntrans == 4 .and. &
         translation_exists((/0.0_dp, 0.5_dp, 0.5_dp/), translations, ntrans) .and. &
         translation_exists((/0.5_dp, 0.0_dp, 0.5_dp/), translations, ntrans) .and. &
         translation_exists((/0.5_dp, 0.5_dp, 0.0_dp/), translations, ntrans)) then
       centering = 'F'
       cell_transform(1,:) = (/0.0_dp, 0.5_dp, 0.5_dp/)
       cell_transform(2,:) = (/0.5_dp, 0.0_dp, 0.5_dp/)
       cell_transform(3,:) = (/0.5_dp, 0.5_dp, 0.0_dp/)
    else if (ntrans == 2 .and. &
         translation_exists((/0.5_dp, 0.5_dp, 0.5_dp/), translations, ntrans)) then
       centering = 'I'
       cell_transform(1,:) = (/-0.5_dp,  0.5_dp,  0.5_dp/)
       cell_transform(2,:) = (/ 0.5_dp, -0.5_dp,  0.5_dp/)
       cell_transform(3,:) = (/ 0.5_dp,  0.5_dp, -0.5_dp/)
    else if (ntrans == 2 .and. &
         translation_exists((/0.5_dp, 0.5_dp, 0.0_dp/), translations, ntrans)) then
       centering = 'C'
       cell_transform(1,:) = (/ 0.5_dp, 0.5_dp, 0.0_dp/)
       cell_transform(2,:) = (/-0.5_dp, 0.5_dp, 0.0_dp/)
       cell_transform(3,:) = (/ 0.0_dp, 0.0_dp, 1.0_dp/)
    else if (ntrans == 2 .and. &
         translation_exists((/0.5_dp, 0.0_dp, 0.5_dp/), translations, ntrans)) then
       centering = 'B'
       cell_transform(1,:) = (/ 0.5_dp, 0.0_dp, 0.5_dp/)
       cell_transform(2,:) = (/ 0.0_dp, 1.0_dp, 0.0_dp/)
       cell_transform(3,:) = (/-0.5_dp, 0.0_dp, 0.5_dp/)
    else if (ntrans == 2 .and. &
         translation_exists((/0.0_dp, 0.5_dp, 0.5_dp/), translations, ntrans)) then
       centering = 'A'
       cell_transform(1,:) = (/1.0_dp,  0.0_dp, 0.0_dp/)
       cell_transform(2,:) = (/0.0_dp,  0.5_dp, 0.5_dp/)
       cell_transform(3,:) = (/0.0_dp, -0.5_dp, 0.5_dp/)
    else
       deallocate(translations)
       return
    end if

    new_lattice(:,:) = matmul(cell_transform, lattice)
    inv_new_lattice(:,:) = new_lattice(:,:)
    call sym_matinv(inv_new_lattice, 3)
    ai_new(:,:) = transpose(inv_new_lattice)

    allocate(new_positions(total_atoms, 3))
    allocate(new_nat(nel))
    new_positions(:,:) = 0.0_dp
    new_nat(:) = 0
    old_offset = 1
    new_total = 0

    do element_index = 1, nel
       new_start = new_total + 1
       do atom_in_element = 1, nat_per_elem(element_index)
          cart(:) = matmul(transpose(lattice), &
               positions(old_offset + atom_in_element - 1, :))
          frac_new(:) = canonical_fractional(matmul(ai_new, cart))
          if (.not. position_exists(frac_new, new_positions, new_start, new_total)) then
             new_total = new_total + 1
             new_nat(element_index) = new_nat(element_index) + 1
             new_positions(new_total, :) = frac_new(:)
          end if
       end do
       old_offset = old_offset + nat_per_elem(element_index)
    end do

    if (any(new_nat(1:nel) * ntrans /= nat_per_elem(1:nel))) then
       deallocate(translations, new_positions, new_nat)
       return
    end if

    lattice(:,:) = new_lattice(:,:)
    k_transform(:,:) = cell_transform(:,:)
    nat_per_elem(1:nel) = new_nat(1:nel)
    total_atoms = new_total

    deallocate(positions)
    allocate(positions(total_atoms, 3))
    positions(:,:) = new_positions(1:total_atoms, :)

    if (out_level >= 1) then
       write(*,*) "Detected ", centering, "-centered conventional cell"
       write(*,*) "Reduced to primitive cell with atoms per element:", nat_per_elem
    end if

    reduced = .true.
    deallocate(translations, new_positions, new_nat)

  contains

    function canonical_fractional(vec) result(out)
      real(dp), intent(in) :: vec(3)
      real(dp) :: out(3)
      integer :: component

      out(:) = vec(:) - floor(vec(:))
      do component = 1, 3
         if (abs(out(component) - 1.0_dp) < tol_lattice_integer) out(component) = 0.0_dp
         if (abs(out(component)) < tol_lattice_integer) out(component) = 0.0_dp
      end do
    end function canonical_fractional

    logical function translation_exists(candidate_vec, translation_list, count) result(found)
      real(dp), intent(in) :: candidate_vec(3)
      real(dp), intent(in) :: translation_list(:,:)
      integer, intent(in) :: count
      integer :: idx

      found = .false.
      do idx = 1, count
         if (fractional_equal(candidate_vec, translation_list(idx, :))) then
            found = .true.
            return
         end if
      end do
    end function translation_exists

    logical function translation_maps_structure(translation_vec, pos, nat_list, nel_in) result(ok)
      real(dp), intent(in) :: translation_vec(3)
      real(dp), intent(in) :: pos(:,:)
      integer, intent(in) :: nat_list(:)
      integer, intent(in) :: nel_in
      integer :: elem, atom, start_idx
      real(dp) :: mapped(3)

      ok = .true.
      start_idx = 1
      do elem = 1, nel_in
         do atom = 1, nat_list(elem)
            mapped(:) = canonical_fractional(pos(start_idx + atom - 1, :) + translation_vec(:))
            if (.not. position_exists(mapped, pos, start_idx, start_idx + nat_list(elem) - 1)) then
               ok = .false.
               return
            end if
         end do
         start_idx = start_idx + nat_list(elem)
      end do
    end function translation_maps_structure

    logical function position_exists(candidate_pos, pos, first_idx, last_idx) result(found)
      real(dp), intent(in) :: candidate_pos(3)
      real(dp), intent(in) :: pos(:,:)
      integer, intent(in) :: first_idx, last_idx
      integer :: idx

      found = .false.
      if (last_idx < first_idx) return
      do idx = first_idx, last_idx
         if (fractional_equal(candidate_pos, pos(idx, :))) then
            found = .true.
            return
         end if
      end do
    end function position_exists

    logical function fractional_equal(left, right) result(equal)
      real(dp), intent(in) :: left(:), right(:)
      real(dp) :: diff(3)

      if (size(left) < 3 .or. size(right) < 3) then
         equal = .false.
         return
      end if
      diff(:) = left(1:3) - right(1:3)
      equal = all(abs(diff(:) - nint(diff(:))) < tol_lattice_integer)
    end function fractional_equal

  end subroutine reduce_centered_cell

  function detect_point_group(lattice) result(pg_name)
    real(dp), intent(in) :: lattice(3,3)
    character(len=10) :: pg_name

    real(dp) :: lengths(3), angles(3), cosine_angle
    real(dp) :: tol, angle_tol
    integer :: i, j, k

    tol = tol_lattice_metric
    angle_tol = tol_lattice_angle_deg

    ! Calculate lattice parameters
    do i = 1, 3
       lengths(i) = sqrt(sum(lattice(i,:)**2))
    end do

    if (any(lengths <= tol_equal)) then
       pg_name = "C1"
       return
    end if

    ! Calculate angles
    do i = 1, 3
       j = mod(i, 3) + 1
       k = mod(i+1, 3) + 1
       cosine_angle = dot_product(lattice(j,:), lattice(k,:)) / &
            (lengths(j) * lengths(k))
       cosine_angle = max(-1.0_dp, min(1.0_dp, cosine_angle))
       angles(i) = acos(cosine_angle) * 180.0_dp / pi
    end do

    ! Detect crystal system and assign point group

    ! Cubic
    if (abs(lengths(1) - lengths(2)) < tol .and. &
        abs(lengths(2) - lengths(3)) < tol .and. &
        abs(angles(1) - 90.0_dp) < angle_tol .and. &
        abs(angles(2) - 90.0_dp) < angle_tol .and. &
        abs(angles(3) - 90.0_dp) < angle_tol) then
       pg_name = "Oh"
       return
    end if

    ! Cubic primitive cells: FCC has alpha=60 deg, BCC has alpha=109.471 deg.
    if (abs(lengths(1) - lengths(2)) < tol .and. &
        abs(lengths(2) - lengths(3)) < tol .and. &
        ((abs(angles(1) - 60.0_dp) < angle_tol .and. &
          abs(angles(2) - 60.0_dp) < angle_tol .and. &
          abs(angles(3) - 60.0_dp) < angle_tol) .or. &
         (abs(angles(1) - 109.471220634_dp) < angle_tol .and. &
          abs(angles(2) - 109.471220634_dp) < angle_tol .and. &
          abs(angles(3) - 109.471220634_dp) < angle_tol))) then
       pg_name = "Oh"
       return
    end if

    ! Hexagonal
    if (abs(lengths(1) - lengths(2)) < tol .and. &
        abs(angles(1) - 90.0_dp) < angle_tol .and. &
        abs(angles(2) - 90.0_dp) < angle_tol .and. &
        abs(angles(3) - 120.0_dp) < angle_tol) then
       pg_name = "D6h"
       return
    end if

    ! Tetragonal
    if (abs(lengths(1) - lengths(2)) < tol .and. &
        abs(angles(1) - 90.0_dp) < angle_tol .and. &
        abs(angles(2) - 90.0_dp) < angle_tol .and. &
        abs(angles(3) - 90.0_dp) < angle_tol) then
       pg_name = "D4h"
       return
    end if

    ! Orthorhombic
    if (abs(angles(1) - 90.0_dp) < angle_tol .and. &
        abs(angles(2) - 90.0_dp) < angle_tol .and. &
        abs(angles(3) - 90.0_dp) < angle_tol) then
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
       if (ios /= 0 .or. pgnr < 1 .or. pgnr > 36) then
          write(*,*) "Warning: Invalid point group '", trim(name), "', using C1"
          pgnr = 1
       end if
    end select
  end function point_group_name_to_number

end module vasp_reader
