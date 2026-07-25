! ============================================
! Symmetry Projection Library Public API
! ============================================
! Provides a clean interface for external programs
! (e.g. DFTB+) to:
!   1. Initialize symmetry data from crystal structure
!   2. Analyze symmetry at individual k-points
!   3. Extract block-diagonal structure from projectors
!
! Single k-point interface: DFTB+ calls
!   sympw_analyze_kpoint() inside its own k-loop.
! ============================================

module sympw_lib
  use accuracy
  use constants
  use sympw_pointgroup_data
  use sympw_core
  use genera, only: sym_matinv
  use sumsets, only: detect_nonprimitive_translations
  use vasp_reader, only: reduce_centered_cell
  implicit none
  private

  ! ============================================
  ! Public types
  ! ============================================
  public :: sympw_crystal_t
  public :: sympw_result_t
  public :: sympw_block_t
  public :: sympw_cell_info_t

  ! ============================================
  ! Public subroutines
  ! ============================================
  public :: sympw_init
  public :: sympw_analyze_kpoint
  public :: sympw_extract_blocks
  public :: sympw_get_cell_info
  public :: sympw_set_verbosity
  public :: sympw_finalize

  ! --- Crystal structure descriptor ---
  type :: sympw_crystal_t
     real(dp) :: lattice(3,3)                ! direct lattice vectors (Cartesian, rows)
     integer  :: nel                          ! number of chemical elements
     integer, allocatable :: nat(:)           ! atoms per element
     integer, allocatable :: lmax(:)          ! max angular momentum per element
     real(dp), allocatable :: pos_frac(:,:,:) ! (3, nel, maxval(nat)) fractional coords
     integer  :: pgnr                         ! point group number (1..36)
  end type sympw_crystal_t

  ! --- Symmetry block descriptor ---
  type :: sympw_block_t
     integer :: dim                               ! block size
     integer, allocatable :: basis_indices(:)      ! indices into the full basis
  end type sympw_block_t

  ! --- Canonicalized cell metadata ---
  type :: sympw_cell_info_t
     logical :: reduced = .false.                 ! .true. if centered cell was reduced
     integer :: nel = 0                           ! number of chemical elements
     integer, allocatable :: nat(:)               ! atoms per element after reduction
     real(dp) :: lattice(3,3) = 0.0_dp             ! direct lattice used internally
     real(dp) :: k_transform(3,3) = 0.0_dp         ! k_internal = k_transform * k_input
  end type sympw_cell_info_t

  ! --- Per-k-point result ---
  type :: sympw_result_t
     integer :: matrix_order                         ! total basis dimension
     complex(dp), allocatable :: projector(:,:)      ! projection matrix (matrix_order, matrix_order)
     logical :: success                               ! .true. if computation succeeded
     real(dp) :: kpoint_input(3) = 0.0_dp             ! fractional k-point in caller basis
     real(dp) :: kpoint_internal(3) = 0.0_dp          ! fractional k-point used internally
     integer :: little_group_order = 0                ! order of the little co-group G_k
     integer :: factor_group_order = 0                ! order of represented factor/lifted group
     logical :: factor_group_used = .false.           ! .true. for nonsymmorphic G_k/T_k branch
     integer :: n_classes = 0                         ! conjugacy classes in represented group
     integer :: n_irreps = 0                          ! irrep count, equal to n_classes
     integer :: n_allowed_irreps = 0                  ! irreps passing Bloch-phase allow filter
     integer :: irrep_dimension_sum = 0               ! sum of represented irrep dimensions
     integer :: allowed_irrep_dimension_sum = 0       ! sum after allow filtering
     integer :: n_blocks                              ! number of symmetry blocks
     type(sympw_block_t), allocatable :: blocks(:)    ! block-diagonal structure
  end type sympw_result_t

  ! ============================================
  ! Module-level persistent state
  ! ============================================
  type(pg_data_t), save :: pg_data
  logical, save :: library_initialized = .false.
  integer, save :: sympw_verbosity = 0

  ! Cached per-calculation arrays (allocated once in init, reused per k-point)
  integer, save :: order
  integer, save, allocatable :: gel(:)
  integer, save, allocatable :: mtab(:, :)
  real(dp), save, allocatable :: u(:, :)
  integer, save, dimension(20) :: steer
  integer, save, dimension(100) :: npri

  ! Tolerances
  real(dp), save :: tsmall, ttsmall

  ! Crystal data (fractional → Cartesian positions cached)
  real(dp), save, allocatable :: r_cart(:, :, :)
  integer, save, allocatable :: nat_arr(:)
  integer, save, allocatable :: lmax_arr(:)
  integer, save :: nel_cached, pgnr_cached

  ! Reciprocal lattice
  real(dp), save :: a_lat(3, 3), ai_lat(3, 3), b_lat(3, 3), bi_lat(3, 3)
  real(dp), save :: k_basis_transform(3, 3)
  logical, save :: cell_was_reduced = .false.

contains

  ! ============================================
  ! Initialize the symmetry library.
  !
  ! Must be called once before any k-point
  ! analysis. Precomputes point group tables,
  ! rotation matrices, and Wigner D-matrices.
  !
  ! Input:
  !   crystal  - crystal structure descriptor
  !
  ! Output:
  !   error_code - 0 on success, nonzero on failure
  ! ============================================
  subroutine sympw_init(crystal, error_code)
    type(sympw_crystal_t), intent(in) :: crystal
    integer, intent(out) :: error_code

    integer :: i, j, first, flat_index, total_atoms
    real(dp) :: tsk(3)
    real(dp), allocatable :: positions_work(:,:)
    integer, allocatable :: nat_work(:)
    real(dp) :: lattice_work(3,3)
    real(dp) :: lattice_volume, lattice_scale

    error_code = 0

    ! --- Validate inputs before replacing any active library state ---
    if (crystal%pgnr < 1 .or. crystal%pgnr > 36) then
       write(*,*) "sympw_init: invalid point group number", crystal%pgnr
       error_code = 1
       return
    end if
    if (crystal%nel < 1) then
       write(*,*) "sympw_init: no chemical elements"
       error_code = 2
       return
    end if
    if (.not. allocated(crystal%nat)) then
       write(*,*) "sympw_init: nat array is not allocated"
       error_code = 3
       return
    end if
    if (size(crystal%nat) < crystal%nel) then
       write(*,*) "sympw_init: nat array is smaller than nel"
       error_code = 4
       return
    end if
    if (.not. allocated(crystal%lmax)) then
       write(*,*) "sympw_init: lmax array is not allocated"
       error_code = 5
       return
    end if
    if (size(crystal%lmax) < crystal%nel) then
       write(*,*) "sympw_init: lmax array is smaller than nel"
       error_code = 6
       return
    end if
    if (any(crystal%nat(1:crystal%nel) < 1)) then
       write(*,*) "sympw_init: atom counts must be positive"
       error_code = 7
       return
    end if
    if (any(crystal%lmax(1:crystal%nel) < 0)) then
       write(*,*) "sympw_init: lmax values must be nonnegative"
       error_code = 8
       return
    end if
    if (.not. allocated(crystal%pos_frac)) then
       write(*,*) "sympw_init: pos_frac array is not allocated"
       error_code = 9
       return
    end if
    if (size(crystal%pos_frac, 1) /= 3) then
       write(*,*) "sympw_init: pos_frac first dimension must be 3"
       error_code = 10
       return
    end if
    if (size(crystal%pos_frac, 2) < crystal%nel) then
       write(*,*) "sympw_init: pos_frac element dimension is smaller than nel"
       error_code = 11
       return
    end if
    if (size(crystal%pos_frac, 3) < maxval(crystal%nat(1:crystal%nel))) then
       write(*,*) "sympw_init: pos_frac atom dimension is smaller than max(nat)"
       error_code = 12
       return
    end if

    lattice_scale = sqrt(sum(crystal%lattice(1, :)**2)) * &
         sqrt(sum(crystal%lattice(2, :)**2)) * &
         sqrt(sum(crystal%lattice(3, :)**2))
    lattice_volume = abs(determinant3(crystal%lattice))
    if (lattice_scale <= tol_equal .or. &
         lattice_volume <= tol_lattice_integer * lattice_scale) then
       write(*,*) "sympw_init: lattice vectors are singular or nearly singular"
       error_code = 13
       return
    end if

    if (library_initialized) then
       call sympw_finalize()
    end if

    ! --- Cache and canonicalize crystal data ---
    nel_cached = crystal%nel
    pgnr_cached = crystal%pgnr
    total_atoms = sum(crystal%nat(:))
    allocate(nat_work(nel_cached))
    allocate(positions_work(total_atoms, 3))
    nat_work(:) = crystal%nat(:)
    lattice_work(:, :) = crystal%lattice(:, :)

    flat_index = 1
    do i = 1, nel_cached
       do j = 1, nat_work(i)
          positions_work(flat_index, :) = crystal%pos_frac(:, i, j)
          flat_index = flat_index + 1
       end do
    end do

    call reduce_centered_cell(lattice_work, positions_work, nat_work, nel_cached, &
         total_atoms, k_basis_transform, cell_was_reduced, verbosity=sympw_verbosity)

    allocate(nat_arr(nel_cached))
    allocate(lmax_arr(nel_cached))
    nat_arr(:) = nat_work(:)
    lmax_arr(:) = crystal%lmax(:)
    a_lat(:, :) = lattice_work(:, :)

    ! --- Reciprocal lattice ---
    b_lat(:, :) = a_lat(:, :)
    call sym_matinv(b_lat, 3)
    bi_lat = transpose(a_lat)
    ai_lat = transpose(b_lat)

    ! --- Convert fractional positions to Cartesian ---
    allocate(r_cart(3, nel_cached, maxval(nat_arr)))
    flat_index = 1
    do i = 1, nel_cached
       do j = 1, nat_arr(i)
          tsk(1:3) = positions_work(flat_index, :)
          r_cart(:, i, j) = matmul(transpose(a_lat), tsk)
          flat_index = flat_index + 1
       end do
    end do
    deallocate(positions_work, nat_work)

    ! --- Initialize tolerances and steer ---
    tsmall = tol_equal
    ttsmall = tol_projection
    steer(:) = 0
    call apply_verbosity_to_steer()

    ! --- Initialize point group data ---
    call init_point_group_data(pg_data, 0)

    ! --- Extract group order and elements ---
    order = pg_data%npgo(1, pgnr_cached)
    first = pg_data%npgo(2, pgnr_cached)

    allocate(gel(order))
    gel(1:order) = nge2(first:(first + order - 1))
    npri(:) = primen(:)

    allocate(u(order, 3))
    call detect_nonprimitive_translations(u, r_cart, a_lat, ai_lat, &
         pg_data%rgr3, gel, order, pgnr_cached, nel_cached, nat_arr)

    ! --- Build multiplication table ---
    if ((pgnr_cached >= 16) .and. (pgnr_cached <= 31)) then
       allocate(mtab(24, 24))
       mtab(:, :) = pg_data%MD6h(:, :)
    else if (pgnr_cached == 2) then
       allocate(mtab(24, 24))
       mtab(:, :) = pg_data%MD6h(:, :)
    else
       allocate(mtab(48, 48))
       mtab(:, :) = pg_data%MOh(:, :)
    end if

    ! Remap multiplication table if not the full parent group
    if ((pgnr_cached /= 31) .and. (pgnr_cached /= 36)) then
       call remap_multiplication_table(mtab, gel, order)
    end if

    library_initialized = .true.

  end subroutine sympw_init


  ! ============================================
  ! Analyze symmetry at a single k-point.
  !
  ! Computes the little group, irreducible
  ! representations, and the projection matrix.
  !
  ! Input:
  !   kpoint   - k-point in fractional coords
  !
  ! Output:
  !   result   - projection matrix and metadata
  ! ============================================
  subroutine sympw_analyze_kpoint(kpoint, result)
    real(dp), intent(in) :: kpoint(3)
    type(sympw_result_t), intent(out) :: result

    real(dp) :: kpoint_internal(3)
    integer :: matrix_order_per_kpt
    logical :: kpt_success
    integer :: ikp_dummy

    result%matrix_order = 0
    result%success = .false.
    result%kpoint_input(:) = kpoint(:)
    result%kpoint_internal(:) = 0.0_dp
    result%little_group_order = 0
    result%factor_group_order = 0
    result%factor_group_used = .false.
    result%n_classes = 0
    result%n_irreps = 0
    result%n_allowed_irreps = 0
    result%irrep_dimension_sum = 0
    result%allowed_irrep_dimension_sum = 0
    result%n_blocks = 0

    if (.not. library_initialized) then
       write(*,*) "sympw_analyze_kpoint: library not initialized"
       return
    end if

    ikp_dummy = 1  ! single k-point mode, use 1 as label
    if (cell_was_reduced) then
       kpoint_internal(:) = matmul(k_basis_transform, kpoint(:))
    else
       kpoint_internal(:) = kpoint(:)
    end if
    result%kpoint_internal(:) = kpoint_internal(:)

    call sympw_compute_kpoint(kpoint_internal, a_lat, ai_lat, b_lat, bi_lat, &
         nel_cached, nat_arr, lmax_arr, order, r_cart, u, &
         pgnr_cached, pg_data%rgr3, pg_data%ldrmm, mtab, gel, &
         steer, npri, tsmall, ttsmall, &
         ikp_dummy, matrix_order_per_kpt, result%projector, kpt_success, &
         verbosity=sympw_verbosity, &
         little_group_order=result%little_group_order, &
         factor_group_order=result%factor_group_order, &
         factor_group_used=result%factor_group_used, &
         n_classes=result%n_classes, n_irreps=result%n_irreps, &
         n_allowed_irreps=result%n_allowed_irreps, &
         irrep_dimension_sum=result%irrep_dimension_sum, &
         allowed_irrep_dimension_sum=result%allowed_irrep_dimension_sum)

    ! sym_projmat assembles the symmetry-adapted basis T (not the projector).
    ! The true projection matrix onto the symmetry-adapted subspace is P = T * T^H.
    if (kpt_success) then
       result%projector = matmul(result%projector, &
            transpose(conjg(result%projector)))
    end if

    result%matrix_order = matrix_order_per_kpt
    result%success = kpt_success
    if (kpt_success) then
       call extract_connected_blocks(result)
    end if

  end subroutine sympw_analyze_kpoint


  ! ============================================
  ! Extract block-diagonal structure from the
  ! projection matrix.
  !
  ! Blocks are identified as connected components
  ! in the graph defined by |P(i,j)| > tol.
  ! Each block corresponds to a symmetry-adapted
  ! subspace that can be diagonalized independently.
  !
  ! Input:
  !   projector       - N×N projection matrix
  !   matrix_order    - dimension N
  !   tol             - threshold for non-zero coupling
  !
  ! Output:
  !   n_blocks_out    - number of symmetry blocks
  !   blocks_out      - block descriptors (dim, basis_indices)
  ! ============================================
  subroutine sympw_extract_blocks(projector, matrix_order, tol, n_blocks_out, blocks_out)
    complex(dp), intent(in)  :: projector(:, :)
    integer,      intent(in)  :: matrix_order
    real(dp),     intent(in)  :: tol
    integer,      intent(out) :: n_blocks_out
    type(sympw_block_t), allocatable, intent(out) :: blocks_out(:)

    integer :: N, i, j, k
    logical, allocatable :: visited(:)
    integer, allocatable :: queue(:), component(:)
    integer :: qhead, qtail, comp_size, n_comp
    integer, allocatable :: comp_start(:), comp_size_arr(:)

    N = matrix_order
    allocate(visited(N))
    visited(:) = .false.

    ! First pass: count and measure components
    n_comp = 0
    allocate(queue(N))
    allocate(component(N))
    allocate(comp_start(N))
    allocate(comp_size_arr(N))

    do i = 1, N
       if (visited(i)) cycle

       ! BFS from node i
       n_comp = n_comp + 1
       comp_start(n_comp) = i
       qhead = 1
       qtail = 1
       queue(1) = i
       visited(i) = .true.
       comp_size = 1

       do while (qhead <= qtail)
          k = queue(qhead)
          qhead = qhead + 1
          do j = 1, N
             if (.not. visited(j)) then
                if (abs(projector(k, j)) > tol) then
                   visited(j) = .true.
                   qtail = qtail + 1
                   queue(qtail) = j
                   comp_size = comp_size + 1
                end if
             end if
          end do
       end do
       comp_size_arr(n_comp) = comp_size
    end do

    n_blocks_out = n_comp
    allocate(blocks_out(n_comp))

    ! Second pass: collect basis indices for each component
    visited(:) = .false.
    n_comp = 0
    do i = 1, N
       if (visited(i)) cycle

       n_comp = n_comp + 1
       qhead = 1
       qtail = 1
       queue(1) = i
       visited(i) = .true.
       comp_size = 1

       do while (qhead <= qtail)
          k = queue(qhead)
          qhead = qhead + 1
          do j = 1, N
             if (.not. visited(j)) then
                if (abs(projector(k, j)) > tol) then
                   visited(j) = .true.
                   qtail = qtail + 1
                   queue(qtail) = j
                   comp_size = comp_size + 1
                end if
             end if
          end do
       end do

       blocks_out(n_comp)%dim = comp_size
       allocate(blocks_out(n_comp)%basis_indices(comp_size))
       blocks_out(n_comp)%basis_indices(1:comp_size) = queue(1:comp_size)
    end do

    deallocate(visited, queue, component, comp_start, comp_size_arr)
  end subroutine sympw_extract_blocks


  ! Internal: extract connected components from the projector
  ! stored in result%projector.
  subroutine extract_connected_blocks(result)
    type(sympw_result_t), intent(inout) :: result

    call sympw_extract_blocks(result%projector, result%matrix_order, &
         tol_projection, result%n_blocks, result%blocks)
  end subroutine extract_connected_blocks


  ! ============================================
  ! Set library output verbosity.
  !
  ! Level 0 is quiet API mode. Level 1 mirrors concise CLI progress.
  ! Level 2 adds core little-group details. Level 3 enables algebra tables.
  ! ============================================
  subroutine sympw_set_verbosity(level)
    integer, intent(in) :: level

    sympw_verbosity = max(0, level)
    if (library_initialized) call apply_verbosity_to_steer()
  end subroutine sympw_set_verbosity


  ! ============================================
  ! Return the canonicalized cell state used by the library.
  !
  ! k_transform maps input fractional k-points to the internal
  ! reciprocal basis: k_internal = k_transform * k_input.
  ! ============================================
  subroutine sympw_get_cell_info(info, error_code)
    type(sympw_cell_info_t), intent(out) :: info
    integer, intent(out), optional :: error_code

    integer :: i

    if (present(error_code)) error_code = 0
    info%reduced = .false.
    info%nel = 0
    info%lattice(:, :) = 0.0_dp
    info%k_transform(:, :) = 0.0_dp
    do i = 1, 3
       info%k_transform(i, i) = 1.0_dp
    end do

    if (.not. library_initialized) then
       if (present(error_code)) error_code = 1
       return
    end if

    info%reduced = cell_was_reduced
    info%nel = nel_cached
    allocate(info%nat(nel_cached))
    info%nat(:) = nat_arr(:)
    info%lattice(:, :) = a_lat(:, :)
    info%k_transform(:, :) = k_basis_transform(:, :)
  end subroutine sympw_get_cell_info


  ! ============================================
  ! Release all internal state.
  !
  ! Call when symmetry analysis is complete.
  ! Safe to call even if not initialized.
  ! ============================================
  subroutine sympw_finalize()
    integer :: i

    if (allocated(r_cart))     deallocate(r_cart)
    if (allocated(nat_arr))    deallocate(nat_arr)
    if (allocated(lmax_arr))   deallocate(lmax_arr)
    if (allocated(gel))        deallocate(gel)
    if (allocated(mtab))       deallocate(mtab)
    if (allocated(u))          deallocate(u)

    call deallocate_point_group_data(pg_data)

    cell_was_reduced = .false.
    k_basis_transform(:, :) = 0.0_dp
    do i = 1, 3
       k_basis_transform(i, i) = 1.0_dp
    end do
    library_initialized = .false.
  end subroutine sympw_finalize


  real(dp) function determinant3(mat)
    real(dp), intent(in) :: mat(3,3)

    determinant3 = mat(1,1) * (mat(2,2) * mat(3,3) - mat(2,3) * mat(3,2)) - &
         mat(1,2) * (mat(2,1) * mat(3,3) - mat(2,3) * mat(3,1)) + &
         mat(1,3) * (mat(2,1) * mat(3,2) - mat(2,2) * mat(3,1))
  end function determinant3

  ! ============================================
  ! Map verbosity to legacy steer flags without changing their semantics.
  ! ============================================
  subroutine apply_verbosity_to_steer()
    steer(2) = 1
    steer(6) = merge(1, 0, sympw_verbosity >= 3)
    steer(7) = merge(1, 0, sympw_verbosity >= 3)
    steer(8) = merge(1, 0, sympw_verbosity >= 3)
    steer(9) = merge(1, 0, sympw_verbosity >= 3)
    steer(11) = 1
    steer(12) = merge(1, 0, sympw_verbosity >= 3)
    steer(18) = merge(1, 0, sympw_verbosity >= 3)
  end subroutine apply_verbosity_to_steer


  ! ============================================
  ! Remap parent-group multiplication table to
  ! the subgroup defined by gel(1:order).
  ! ============================================
  subroutine remap_multiplication_table(mtab_sub, gel_sub, order_sub)
    integer, intent(inout) :: mtab_sub(:, :)
    integer, intent(in) :: gel_sub(:)
    integer, intent(in) :: order_sub

    integer :: i, j
    integer, allocatable :: kkgel(:)

    allocate(kkgel(maxval(gel_sub)))
    kkgel(:) = 0
    do i = 1, order_sub
       kkgel(gel_sub(i)) = i
    end do

    do i = 1, order_sub
       do j = 1, order_sub
          mtab_sub(i, j) = mtab_sub(gel_sub(i), gel_sub(j))
       end do
    end do

    do i = 1, order_sub
       do j = 1, order_sub
          mtab_sub(i, j) = kkgel(mtab_sub(i, j))
       end do
    end do

    deallocate(kkgel)
  end subroutine remap_multiplication_table

end module sympw_lib
