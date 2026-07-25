program sympw_vasp
  ! ============================================
  ! Symmetry Projection with VASP input
  ! ============================================
  ! Usage:
  !   sympw_vasp POSCAR KPOINTS [lmax1 lmax2 ...]
  !   sympw_vasp sympw.conf
  ! ============================================

  use accuracy
  use constants
  use vasp_reader
  use genera, only: sym_matinv
  use sumsets, only: detect_nonprimitive_translations
  use sympw_pointgroup_data
  use sympw_core
  use time_reversal
  use time_reversal_optimization
  implicit none

  ! VASP input
  character(len=256) :: poscar_file, kpoints_file, comment
  real(dp) :: scale
  real(dp) :: lattice(3,3), bi(3,3), ai(3,3), b(3,3)
  real(dp) :: k_transform(3,3)
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

  ! Symmetry
  integer :: pgnr
  character(len=10) :: pg_name = ''
  integer, allocatable :: lmax(:)
  real(dp), allocatable :: r(:,:,:)

  ! Command line
  integer :: nargs, i, ios
  character(len=256) :: arg
  logical :: use_config_file
  logical :: auto_point_group
  logical :: kpoints_cartesian
  logical :: cell_reduced

  ! Per-calculation arrays
  integer :: order, first
  integer, dimension(100) :: npri
  integer, allocatable :: gel(:)
  integer, allocatable :: mtab(:,:)
  real(dp), allocatable :: u(:,:)
  integer, dimension(20) :: steer
  real(dp), dimension(3) :: tsk
  real(dp) :: tsmall, ttsmall, T
  real(dp) :: all_kpoints_arr(1000, 3)  ! max 1000 k-points
  integer :: number_of_wave_vectors
  integer :: matrixorder
  complex(dp), allocatable :: projmatrix(:,:,:)

  ! Point group data
  type(pg_data_t) :: pg

  ! Time-reversal
  integer, allocatable :: trim_indices(:), tr_pairs(:)
  integer :: n_trim, ik_tr
  logical :: is_trim
  logical, allocatable :: should_compute(:)
  integer, allocatable :: source_kpoint(:)
  integer :: n_computed, n_skipped

  ! K-point loop
  integer :: ikp, matrix_order_per_kpt
  logical :: kpt_success
  complex(dp), allocatable :: kpt_projmatrix(:,:)
  integer :: K1, K2

  integer :: atco

  ! ============================================
  ! Command line parsing
  ! ============================================
  nargs = command_argument_count()

  if (nargs < 1) then
     write(*,*) "Usage:"
     write(*,*) "  sympw_vasp POSCAR KPOINTS [lmax1 lmax2 ...]"
     write(*,*) "  sympw_vasp sympw.conf"
     error stop
  end if

  call get_command_argument(1, arg)
  use_config_file = (index(arg, '.conf') > 0 .or. index(arg, '.cfg') > 0)

  if (.not. use_config_file .and. nargs < 2) then
     write(*,*) "Error: Need at least POSCAR and KPOINTS files"
     error stop
  end if

  if (use_config_file) then
     call read_config_file(arg, poscar_file, kpoints_file, lmax, pg_name)
     if (len_trim(poscar_file) == 0 .or. len_trim(kpoints_file) == 0) then
        write(*,*) "Error: POSCAR and KPOINTS must be specified in config file"
        error stop
     end if
  else
     call get_command_argument(1, poscar_file)
     call get_command_argument(2, kpoints_file)
  end if

  ! ============================================
  ! Read POSCAR
  ! ============================================
  call read_poscar(poscar_file, comment, scale, lattice, elements, &
                   nat_per_elem, positions, is_cartesian, nel, total_atoms)

  if (.not. allocated(lmax)) then
     allocate(lmax(nel))
     if (nargs >= 2 + nel) then
        do i = 1, nel
           call get_command_argument(2 + i, arg)
           read(arg, *) lmax(i)
        end do
     else
        lmax(:) = 2
        write(*,*) "Using default lmax = 2 for all elements"
     end if
  end if

  write(*,*) "lmax for each element:", lmax

  ! ============================================
  ! Read KPOINTS
  ! ============================================
  call read_kpoints(kpoints_file, kpoints, kpoint_names, nkpts, kpt_mode, kpoints_cartesian)
  number_of_wave_vectors = nkpts
  do i = 1, nkpts
     all_kpoints_arr(i, :) = kpoints(i, :)
  end do

  ! ============================================
  ! Convert positions and reduce centered cells
  ! ============================================
  if (is_cartesian) then
     write(*,*) "Converting Cartesian to fractional coordinates..."
     bi = lattice
     call sym_matinv(bi, 3)
     do i = 1, total_atoms
        positions(i,:) = matmul(positions(i,:), bi)
     end do
  end if

  call reduce_centered_cell(lattice, positions, nat_per_elem, nel, &
       total_atoms, k_transform, cell_reduced, verbosity=1)
  if (cell_reduced .and. (.not. kpoints_cartesian)) then
     write(*,*) "Converting reciprocal fractional KPOINTS to primitive basis..."
     do i = 1, nkpts
        kpoints(i, :) = matmul(k_transform, kpoints(i, :))
        all_kpoints_arr(i, :) = kpoints(i, :)
     end do
  end if

  ! ============================================
  ! Detect point group
  ! ============================================
  auto_point_group = (len_trim(pg_name) == 0)
  if (auto_point_group) then
     pg_name = detect_point_group(lattice)
     write(*,*) "Lattice-only point group guess:", trim(pg_name)
  else
     write(*,*) "Using specified point group:", trim(pg_name)
  end if
  pgnr = point_group_name_to_number(pg_name)
  write(*,*) "Point group number:", pgnr

  allocate(r(3, nel, maxval(nat_per_elem)))
  call reorganize_positions(positions, nat_per_elem, nel, r)

  ! Convert positions from fractional to Cartesian (required by symmetry engine)
  do i = 1, nel
     do K2 = 1, nat_per_elem(i)
        tsk(1:3) = r(:, i, K2)
        r(:, i, K2) = matmul(transpose(lattice), tsk)
     end do
  end do
  write(*,*) "Positions converted to Cartesian coordinates."

  ! ============================================
  ! Reciprocal lattice
  ! ============================================
  ! a  = lattice (direct lattice, Cartesian)
  ! b  = a^{-1} (reciprocal lattice, used in groupkp)
  ! ai = (a^{-1})^T (inverse-transpose, used for fractional mapping)
  ! bi = a^T (transpose, used for coordinate transforms)
  T = 2*pi
  b(:,:) = lattice(:,:)
  call sym_matinv(b, 3)
  bi = transpose(lattice)
  ai = transpose(b)

  if (kpoints_cartesian) then
     write(*,*) "Converting Cartesian KPOINTS to reciprocal fractional coordinates..."
     do i = 1, nkpts
        kpoints(i, :) = matmul(kpoints(i, :), bi)
        all_kpoints_arr(i, :) = kpoints(i, :)
     end do
  end if

  ! ============================================
  ! Initialize tolerances and steer
  ! ============================================
  tsmall = tol_equal
  ttsmall = tol_projection
  steer(:) = 0
  steer(2) = 1
  steer(11) = 1
  atco = 0

  ! ============================================
  ! Initialize point group data
  ! ============================================
  write(*,*)
  write(*,*) "=========================================="
  write(*,*) "Initializing point group data..."
  write(*,*) "=========================================="
  call init_point_group_data(pg, 0)

  if (auto_point_group) then
     pgnr = detect_structure_point_group(lattice, ai, r, nel, nat_per_elem, pg)
     pg_name = point_group_number_to_name(pgnr)
     write(*,*) "Structure-verified point group:", trim(pg_name), " Number:", pgnr
  end if

  ! ============================================
  ! Per-calculation setup
  ! ============================================
  order = pg%npgo(1, pgnr)
  first = pg%npgo(2, pgnr)

  allocate(gel(order))
  gel(1:order) = nge2(first:(first+order-1))
  npri(:) = primen(:)

  allocate(u(order, 3))
  call detect_nonprimitive_translations(u, r, lattice, ai, pg%rgr3, gel, order, pgnr, nel, nat_per_elem)
  if (any(abs(u) > tol_zero)) then
     write(*,*) "Detected nonprimitive translations; factor-group path will be used on boundary k-points"
  else
     write(*,*) "No nonprimitive translations detected; using ordinary little co-group path"
  end if

  write(*,'(A,I3)') " Point group number: ", pgnr
  write(*,'(A,I3)') " Group order: ", order
  if ((pgnr >= 16) .and. (pgnr <= 31)) then
     allocate(mtab(24, 24))
     mtab(:,:) = 0
     mtab(:,:) = pg%MD6h(:,:)
     write(*,*) "Subgroup of D6h"
  else if (pgnr == 2) then
     ! Ci: uses D6h parent where element 13 is pure inversion
     allocate(mtab(24, 24))
     mtab(:,:) = 0
     mtab(:,:) = pg%MD6h(:,:)
     write(*,*) "Subgroup of D6h"
  else
     allocate(mtab(48, 48))
     mtab(:,:) = 0
     mtab(:,:) = pg%MOh(:,:)
     write(*,*) "Subgroup of Oh"
  end if

  if ((pgnr /= 31) .and. (pgnr /= 36)) then
     allocate(kpt_projmatrix(maxval(gel), 1))
     kpt_projmatrix(:,:) = 0
     do i = 1, order
        kpt_projmatrix(gel(i), 1) = cmplx(real(i, dp), 0, dp)
     end do
     do i = 1, order
        do K1 = 1, order
           mtab(i, K1) = mtab(gel(i), gel(K1))
        end do
     end do
     do i = 1, order
        do K1 = 1, order
           mtab(i, K1) = nint(real(kpt_projmatrix(mtab(i, K1), 1)))
        end do
     end do
     deallocate(kpt_projmatrix)
  end if

  ! ============================================
  ! Compute total matrix dimension
  ! ============================================
  matrixorder = 0
  do i = 1, nel
     K1 = 0
     do K2 = 0, lmax(i)
        K1 = K1 + 2*K2 + 1
     end do
     matrixorder = matrixorder + nat_per_elem(i)*K1
  end do
  write(*,'(A,I5)') " Total basis function dimension: ", matrixorder

  allocate(projmatrix(number_of_wave_vectors, matrixorder, matrixorder))
  projmatrix(:,:,:) = 0

  ! ============================================
  ! Time-reversal symmetry analysis
  ! ============================================
  write(*,*)
  write(*,*) "=========================================="
  write(*,*) "Time-Reversal Symmetry Analysis"
  write(*,*) "=========================================="

  call find_trim_points(kpoints, nkpts, trim_indices, n_trim, tsmall)
  allocate(tr_pairs(nkpts))
  call build_time_reversal_pairs(kpoints, nkpts, tr_pairs, tsmall)

  write(*,'(A,I3,A,I3)') " TRIM points: ", n_trim, " out of ", nkpts

  allocate(should_compute(nkpts))
  allocate(source_kpoint(nkpts))
  call mark_kpoints_to_compute(tr_pairs, nkpts, should_compute, source_kpoint, &
                                n_computed, n_skipped, n_trim)
  call print_tr_optimization_summary(nkpts, n_computed, n_skipped, n_trim, &
                                      should_compute, source_kpoint, tr_pairs)

  ! ============================================
  ! Main k-point loop
  ! ============================================
  write(*,*)
  write(*,*) "=========================================="
  write(*,*) "Starting symmetry projection calculation"
  write(*,*) "=========================================="

  do ikp = 1, number_of_wave_vectors
     call sympw_compute_kpoint(kpoints(ikp,:), lattice, ai, b, bi, nel, nat_per_elem, lmax, order, r, u, &
          pgnr, pg%rgr3, pg%ldrmm, mtab, gel, steer, npri, tsmall, ttsmall, &
          ikp, matrix_order_per_kpt, kpt_projmatrix, kpt_success, verbosity=1)

     if (kpt_success) then
        if (matrix_order_per_kpt == matrixorder) then
           projmatrix(ikp, :, :) = matmul(kpt_projmatrix(:, :), transpose(conjg(kpt_projmatrix(:, :))))
        end if
        deallocate(kpt_projmatrix)
     end if
  end do

  ! ============================================
  ! Summary output
  ! ============================================
  write(*,*)
  write(*,*) "=========================================="
  write(*,*) "Symmetry Projection Complete!"
  write(*,*) "=========================================="
  write(*,'(A,I3)') " K-points processed: ", number_of_wave_vectors
  write(*,'(A,I5)') " Basis dimension: ", matrixorder
  write(*,'(A,I3)') " Point group: ", pgnr
  write(*,*)

  ! ============================================
  ! Cleanup
  ! ============================================
  deallocate(r, lmax, nat_per_elem, gel, mtab, u, projmatrix)
  deallocate(kpoints, kpoint_names, positions, elements)
  deallocate(trim_indices, tr_pairs, should_compute, source_kpoint)
  call deallocate_point_group_data(pg)

contains

  subroutine read_config_file(filename, poscar, kpoints_file_out, lmax_out, pg)
    character(len=*), intent(in) :: filename
    character(len=*), intent(out) :: poscar, kpoints_file_out, pg
    integer, allocatable, intent(out) :: lmax_out(:)

    integer :: fh_cfg, ios_cfg, nlmax, i_cfg
    character(len=256) :: line, keyword
    integer, allocatable :: temp_lmax(:)

    allocate(temp_lmax(100))
    fh_cfg = 20
    open(fh_cfg, file=filename, status='old', action='read', iostat=ios_cfg)
    if (ios_cfg /= 0) then
       write(*,*) "Error: Cannot open config file:", trim(filename)
       error stop
    end if

    poscar = ""
    kpoints_file_out = ""
    pg = ""
    nlmax = 0

    do
       read(fh_cfg, '(A)', iostat=ios_cfg) line
       if (ios_cfg /= 0) exit
       line = adjustl(line)
       if (len_trim(line) == 0) cycle
       if (line(1:1) == '#') cycle
       read(line, *) keyword
       select case(trim(keyword))
       case('POSCAR')
          read(line, *) keyword, poscar
       case('KPOINTS')
          read(line, *) keyword, kpoints_file_out
       case('LMAX')
          temp_lmax = 0
          read(line, *, iostat=ios_cfg) keyword, temp_lmax(1:100)
          do i_cfg = 1, 100
             if (temp_lmax(i_cfg) /= 0) nlmax = i_cfg
          end do
       case('POINTGROUP', 'POINT_GROUP')
          read(line, *) keyword, pg
       end select
    end do
    close(fh_cfg)

    if (nlmax > 0) then
       allocate(lmax_out(nlmax))
       lmax_out = temp_lmax(1:nlmax)
    end if
    deallocate(temp_lmax)
  end subroutine read_config_file

  subroutine reorganize_positions(pos_flat, nat_arr, nel_arr, r_out)
    real(dp), intent(in) :: pos_flat(:,:)
    integer, intent(in) :: nat_arr(:), nel_arr
    real(dp), intent(out) :: r_out(:,:,:)
    integer :: iel, iat, idx
    idx = 1
    do iel = 1, nel_arr
       do iat = 1, nat_arr(iel)
          r_out(:, iel, iat) = pos_flat(idx, :)
          idx = idx + 1
       end do
    end do
  end subroutine reorganize_positions

end program sympw_vasp
