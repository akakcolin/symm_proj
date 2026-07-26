! ============================================
! Symmetry Projection Library Public API
! ============================================
! Provides a clean interface for external programs
! (e.g. DFTB+) to:
!   1. Initialize symmetry data from crystal structure
!   2. Analyze symmetry at individual k-points
!   3. Access the symmetry-adapted basis T and aggregate projectors
!      in both complex and real orbital bases
!   4. Resolve Bloch-allowed irreps into T-column ranges and
!      construct their subspace projectors on demand
!   5. Expose conventional real representations assembled from
!      complex-conjugate irrep pairs without replacing the complex data
!   6. Independently audit the spinless k/-k relation
!
! Single k-point interface: DFTB+ calls
!   sympw_analyze_kpoint() inside its own k-loop.
! ============================================

module sympw_lib
  use accuracy
  use constants
  use sympw_pointgroup_data
  use sympw_core
  use sympw_mulliken, only: assign_mulliken_label, mulliken_point_group_supported, &
       mulliken_point_group_requires_pairing
  use sympw_mulliken_real_pairs, only: assign_real_view_mulliken_label
  use sympw_real_sh, only: complex_to_real_projector
  use time_reversal_optimization, only: verify_spinless_projector_pair
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
  public :: sympw_irrep_subspace_t
  public :: sympw_real_irrep_view_t

  ! ============================================
  ! Public subroutines
  ! ============================================
  public :: sympw_init
  public :: sympw_analyze_kpoint
  public :: sympw_check_spinless_time_reversal
  public :: sympw_get_irrep_projector
  public :: sympw_get_real_irrep_projector
  public :: sympw_extract_blocks
  public :: sympw_get_cell_info
  public :: sympw_set_verbosity
  public :: sympw_finalize

  integer, parameter, public :: SYMPW_MULLIKEN_STATUS_NOT_ANALYZED = 0
  integer, parameter, public :: SYMPW_MULLIKEN_STATUS_AVAILABLE = 1
  integer, parameter, public :: SYMPW_MULLIKEN_STATUS_PROJECTIVE = 2
  integer, parameter, public :: SYMPW_MULLIKEN_STATUS_NOT_FULL_POINT_GROUP = 3
  ! Complex irreps retain fingerprint labels; conventional labels, when resolved,
  ! are exposed separately through sympw_result_t%real_irreps.
  integer, parameter, public :: SYMPW_MULLIKEN_STATUS_COMPLEX_PAIR = 4
  integer, parameter, public :: SYMPW_MULLIKEN_STATUS_UNSUPPORTED_POINT_GROUP = 5
  integer, parameter, public :: SYMPW_MULLIKEN_STATUS_RESOLUTION_FAILED = 6

  ! --- Crystal structure descriptor ---
  type :: sympw_crystal_t
     real(dp) :: lattice(3,3)                ! direct lattice vectors (Cartesian, rows)
     integer  :: nel                          ! number of chemical elements
     integer, allocatable :: nat(:)           ! atoms per element
     integer, allocatable :: lmax(:)          ! max angular momentum per element
     real(dp), allocatable :: pos_frac(:,:,:) ! (3, nel, maxval(nat)) fractional coords
     integer  :: pgnr                         ! point group number (1..36), or 0 for automatic detection
  end type sympw_crystal_t

  ! --- Projector-connectivity component descriptor ---
  type :: sympw_block_t
     integer :: dim                               ! block size
     integer, allocatable :: basis_indices(:)      ! indices into the full basis
  end type sympw_block_t

  ! --- One Bloch-allowed group irrep and its orbital-space occurrence ---
  type :: sympw_irrep_subspace_t
     integer :: group_index = 0                    ! internal irrep index, not a Mulliken label
     integer :: dimension = 0                      ! dimension of one irrep copy
     integer :: multiplicity = 0                   ! copies present in the orbital representation
     integer :: column_start = 0                   ! first column in symmetry_basis, or 0 if absent
     integer :: column_end = 0                     ! last column in symmetry_basis, or 0 if absent
     character(len=24) :: label = ""                ! group-order-scoped character fingerprint
     character(len=16) :: mulliken_label = ""       ! optional conventional label for identified ordinary groups
     complex(dp), allocatable :: characters(:)      ! character for each represented-group element
  end type sympw_irrep_subspace_t

  ! --- Conventional real representation assembled from one real irrep or a complex pair ---
  type :: sympw_real_irrep_view_t
     integer :: dimension = 0                         ! real representation dimension
     integer :: multiplicity = 0                      ! copies in the orbital representation
     character(len=16) :: label = ""                  ! conventional real Mulliken label
     integer, allocatable :: member_irrep_positions(:) ! positions in sympw_result_t%irreps
     complex(dp), allocatable :: characters(:)         ! summed, numerically real characters
  end type sympw_real_irrep_view_t

  ! --- Canonicalized cell metadata ---
  type :: sympw_cell_info_t
     logical :: reduced = .false.                 ! .true. if centered cell was reduced
     integer :: nel = 0                           ! number of chemical elements
     integer :: point_group_number = 0            ! detected or requested crystallographic point group
     integer :: basis_dimension = 0               ! orbital basis size after cell canonicalization
     integer, allocatable :: nat(:)               ! atoms per element after reduction
     integer, allocatable :: lmax(:)              ! maximum angular momentum per element
     real(dp) :: lattice(3,3) = 0.0_dp             ! direct lattice used internally
     real(dp) :: k_transform(3,3) = 0.0_dp         ! k_internal = k_transform * k_input
  end type sympw_cell_info_t

  ! --- Per-k-point result ---
  type :: sympw_result_t
     integer :: matrix_order                         ! total basis dimension
     complex(dp), allocatable :: symmetry_basis(:,:) ! symmetry-adapted basis T in CSH basis
     complex(dp), allocatable :: projector(:,:)      ! legacy aggregate P in CSH basis
     complex(dp), allocatable :: projector_real(:,:) ! aggregate P in real orbital basis
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
     integer :: mulliken_status = SYMPW_MULLIKEN_STATUS_NOT_ANALYZED ! result-level label availability
     type(sympw_irrep_subspace_t), allocatable :: irreps(:) ! all Bloch-allowed group irreps
     logical :: real_irrep_view_available = .false.     ! paired conventional view is populated
     type(sympw_real_irrep_view_t), allocatable :: real_irreps(:)
     integer :: n_blocks                              ! number of projector graph components
     type(sympw_block_t), allocatable :: blocks(:)    ! threshold-dependent connectivity components
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

    integer :: i, j, first, flat_index, total_atoms, alloc_stat
    logical :: translation_ok, auto_point_group
    real(dp) :: tsk(3)
    real(dp), allocatable :: positions_work(:,:)
    integer, allocatable :: nat_work(:)
    real(dp) :: lattice_work(3,3)
    real(dp) :: lattice_volume, lattice_scale

    error_code = 0

    ! --- Validate inputs before replacing any active library state ---
    auto_point_group = crystal%pgnr == 0
    if (crystal%pgnr < 0 .or. crystal%pgnr > 36) then
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
    if (any(crystal%lmax(1:crystal%nel) > maxL)) then
       write(*,*) "sympw_init: lmax exceeds supported maximum", maxL
       error_code = 14
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
    total_atoms = sum(crystal%nat(1:nel_cached))
    allocate(nat_work(nel_cached), positions_work(total_atoms, 3), stat=alloc_stat)
    if (alloc_stat /= 0) then
       call fail_init_allocation("input canonicalization workspace")
       return
    end if
    nat_work(:) = crystal%nat(1:nel_cached)
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

    allocate(nat_arr(nel_cached), lmax_arr(nel_cached), stat=alloc_stat)
    if (alloc_stat /= 0) then
       call fail_init_allocation("cached element metadata")
       return
    end if
    nat_arr(:) = nat_work(:)
    lmax_arr(:) = crystal%lmax(1:nel_cached)
    a_lat(:, :) = lattice_work(:, :)

    ! --- Reciprocal lattice ---
    b_lat(:, :) = a_lat(:, :)
    call sym_matinv(b_lat, 3)
    bi_lat = transpose(a_lat)
    ai_lat = transpose(b_lat)

    ! --- Convert fractional positions to Cartesian ---
    allocate(r_cart(3, nel_cached, maxval(nat_arr)), stat=alloc_stat)
    if (alloc_stat /= 0) then
       call fail_init_allocation("Cartesian position cache")
       return
    end if
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

    if (auto_point_group) then
       pgnr_cached = detect_structure_point_group(a_lat, ai_lat, r_cart, &
            nel_cached, nat_arr, pg_data)
       if (pgnr_cached < 1 .or. pgnr_cached > 36) then
          write(*,*) "sympw_init: automatic point-group detection failed"
          call sympw_finalize()
          error_code = 17
          return
       end if
       if (sympw_verbosity >= 1) then
          write(*,'(A,I3)') " Automatically detected point group:", pgnr_cached
       end if
    end if

    ! --- Extract group order and elements ---
    order = pg_data%npgo(1, pgnr_cached)
    first = pg_data%npgo(2, pgnr_cached)

    allocate(gel(order), stat=alloc_stat)
    if (alloc_stat /= 0) then
       call fail_init_allocation("point-group element table")
       return
    end if
    gel(1:order) = nge2(first:(first + order - 1))
    npri(:) = primen(:)

    allocate(u(order, 3), stat=alloc_stat)
    if (alloc_stat /= 0) then
       call fail_init_allocation("nonprimitive translation table")
       return
    end if
    call detect_nonprimitive_translations(u, r_cart, a_lat, ai_lat, &
         pg_data%rgr3, gel, order, pgnr_cached, nel_cached, nat_arr, &
         success=translation_ok)
    if (.not. translation_ok) then
       write(*,*) "sympw_init: atomic structure is inconsistent with requested point group"
       call sympw_finalize()
       error_code = 16
       return
    end if

    ! --- Build multiplication table ---
    if ((pgnr_cached >= 16) .and. (pgnr_cached <= 31)) then
       allocate(mtab(24, 24), stat=alloc_stat)
       if (alloc_stat /= 0) then
          call fail_init_allocation("D6h multiplication table")
          return
       end if
       mtab(:, :) = pg_data%MD6h(:, :)
    else if (pgnr_cached == 2) then
       allocate(mtab(24, 24), stat=alloc_stat)
       if (alloc_stat /= 0) then
          call fail_init_allocation("Ci multiplication table")
          return
       end if
       mtab(:, :) = pg_data%MD6h(:, :)
    else
       allocate(mtab(48, 48), stat=alloc_stat)
       if (alloc_stat /= 0) then
          call fail_init_allocation("Oh multiplication table")
          return
       end if
       mtab(:, :) = pg_data%MOh(:, :)
    end if

    ! Remap multiplication table if not the full parent group
    if ((pgnr_cached /= 31) .and. (pgnr_cached /= 36)) then
       call remap_multiplication_table(mtab, gel, order)
    end if

    library_initialized = .true.

  contains

    subroutine fail_init_allocation(context)
      character(len=*), intent(in) :: context

      write(*,*) "sympw_init: memory allocation failed for ", trim(context)
      if (allocated(positions_work)) deallocate(positions_work)
      if (allocated(nat_work)) deallocate(nat_work)
      call sympw_finalize()
      error_code = 15
    end subroutine fail_init_allocation

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

    real(dp) :: kpoint_canonical(3), kpoint_internal(3)
    integer :: matrix_order_per_kpt
    integer :: real_projector_stat
    integer, allocatable :: irrep_indices(:), irrep_dimensions(:)
    integer, allocatable :: irrep_column_start(:), irrep_column_end(:)
    complex(dp), allocatable :: irrep_characters(:,:)
    logical :: kpt_success, irrep_metadata_ok
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
    result%mulliken_status = SYMPW_MULLIKEN_STATUS_NOT_ANALYZED
    result%real_irrep_view_available = .false.
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
    call snap_fractional_kpoint(kpoint_internal, kpoint_canonical)
    kpoint_internal = kpoint_canonical
    result%kpoint_internal(:) = kpoint_internal(:)

    call sympw_compute_kpoint(kpoint_internal, a_lat, ai_lat, b_lat, bi_lat, &
         nel_cached, nat_arr, lmax_arr, order, r_cart, u, &
         pgnr_cached, pg_data%rgr3, pg_data%ldrmm, mtab, gel, &
         steer, npri, tsmall, ttsmall, &
         ikp_dummy, matrix_order_per_kpt, result%symmetry_basis, kpt_success, &
         verbosity=sympw_verbosity, &
         little_group_order=result%little_group_order, &
         factor_group_order=result%factor_group_order, &
         factor_group_used=result%factor_group_used, &
         n_classes=result%n_classes, n_irreps=result%n_irreps, &
         n_allowed_irreps=result%n_allowed_irreps, &
         irrep_dimension_sum=result%irrep_dimension_sum, &
         allowed_irrep_dimension_sum=result%allowed_irrep_dimension_sum, &
         projector_out=result%projector, &
         allowed_irrep_indices_out=irrep_indices, &
         allowed_irrep_dimensions_out=irrep_dimensions, &
         allowed_irrep_column_start_out=irrep_column_start, &
         allowed_irrep_column_end_out=irrep_column_end, &
         allowed_irrep_characters_out=irrep_characters)

    result%matrix_order = matrix_order_per_kpt
    result%success = kpt_success
    if (kpt_success) then
       call populate_irrep_metadata(result, irrep_indices, irrep_dimensions, &
            irrep_column_start, irrep_column_end, irrep_characters, irrep_metadata_ok)
       if (.not. irrep_metadata_ok) then
          write(*,*) "sympw_analyze_kpoint: invalid irrep column metadata"
          if (allocated(result%projector)) deallocate(result%projector)
          if (allocated(result%symmetry_basis)) deallocate(result%symmetry_basis)
          result%mulliken_status = SYMPW_MULLIKEN_STATUS_RESOLUTION_FAILED
          result%success = .false.
          return
       end if
       allocate(result%projector_real(matrix_order_per_kpt, matrix_order_per_kpt), &
            stat=real_projector_stat)
       if (real_projector_stat /= 0) then
          write(*,*) "sympw_analyze_kpoint: failed to allocate real-basis projector"
          if (allocated(result%projector)) deallocate(result%projector)
          if (allocated(result%symmetry_basis)) deallocate(result%symmetry_basis)
          if (allocated(result%irreps)) deallocate(result%irreps)
          result%mulliken_status = SYMPW_MULLIKEN_STATUS_NOT_ANALYZED
          result%success = .false.
          return
       end if
       call complex_to_real_projector(result%projector, lmax_arr, nat_arr, result%projector_real)
       call extract_connected_blocks(result)
    end if

  end subroutine sympw_analyze_kpoint


  subroutine populate_irrep_metadata(result, indices, dimensions, column_start, &
       column_end, characters, success)
    type(sympw_result_t), intent(inout) :: result
    integer, allocatable, intent(in) :: indices(:), dimensions(:)
    integer, allocatable, intent(in) :: column_start(:), column_end(:)
    complex(dp), allocatable, intent(in) :: characters(:,:)
    logical, intent(out) :: success

    integer :: position, other_position, column_count, expected_column, alloc_stat
    complex(dp) :: character_inner_product

    success = .false.
    if (.not. allocated(indices) .or. .not. allocated(dimensions) .or. &
         .not. allocated(column_start) .or. .not. allocated(column_end)) return
    if (.not. allocated(characters)) return
    if (size(indices) /= result%n_allowed_irreps .or. &
         size(dimensions) /= size(indices) .or. &
         size(column_start) /= size(indices) .or. size(column_end) /= size(indices) .or. &
         size(characters, 1) /= size(indices) .or. &
         size(characters, 2) /= result%factor_group_order) return

    allocate(result%irreps(size(indices)), stat=alloc_stat)
    if (alloc_stat /= 0) return

    expected_column = 1
    do position = 1, size(indices)
       result%irreps(position)%group_index = indices(position)
       result%irreps(position)%dimension = dimensions(position)
       result%irreps(position)%column_start = column_start(position)
       result%irreps(position)%column_end = column_end(position)
       allocate(result%irreps(position)%characters(result%factor_group_order), stat=alloc_stat)
       if (alloc_stat /= 0) then
          deallocate(result%irreps)
          return
       end if
       result%irreps(position)%characters = characters(position, :)
       result%irreps(position)%label = make_irrep_label(dimensions(position), &
            result%irreps(position)%characters)
       if (indices(position) < 1 .or. indices(position) > result%n_irreps .or. &
            dimensions(position) < 1) then
          deallocate(result%irreps)
          return
       end if
       if (column_start(position) == 0 .and. column_end(position) == 0) cycle
       column_count = column_end(position) - column_start(position) + 1
       if (column_start(position) /= expected_column .or. column_count < 1 .or. &
            mod(column_count, dimensions(position)) /= 0) then
          deallocate(result%irreps)
          return
       end if
       result%irreps(position)%multiplicity = column_count / dimensions(position)
       expected_column = column_end(position) + 1
    end do

    do position = 1, size(result%irreps)
       if (len_trim(result%irreps(position)%label) == 0 .or. &
            abs(result%irreps(position)%characters(1) - &
            result%irreps(position)%dimension) > tol_irrep_phase) then
          deallocate(result%irreps)
          return
       end if
       character_inner_product = sum(conjg(result%irreps(position)%characters) * &
            result%irreps(position)%characters) / real(result%factor_group_order, dp)
       if (abs(character_inner_product - cmplx(1.0_dp, 0.0_dp, dp)) > &
            tol_irrep_phase) then
          deallocate(result%irreps)
          return
       end if
       do other_position = 1, position - 1
          if (trim(result%irreps(position)%label) == &
               trim(result%irreps(other_position)%label)) then
             deallocate(result%irreps)
             return
          end if
          character_inner_product = sum(conjg(result%irreps(other_position)%characters) * &
               result%irreps(position)%characters) / real(result%factor_group_order, dp)
          if (abs(character_inner_product) > tol_irrep_phase) then
             deallocate(result%irreps)
             return
          end if
       end do
    end do

    if (expected_column /= result%matrix_order + 1) then
       deallocate(result%irreps)
       return
    end if
    success = .true.
    call populate_mulliken_labels(result)
  end subroutine populate_irrep_metadata


  subroutine populate_mulliken_labels(result)
    type(sympw_result_t), intent(inout) :: result

    character(len=16), allocatable :: candidate_labels(:)
    integer :: position, other_position, alloc_stat
    logical :: label_ok

    result%mulliken_status = SYMPW_MULLIKEN_STATUS_RESOLUTION_FAILED
    if (.not. allocated(result%irreps)) return
    if (result%factor_group_used) then
       result%mulliken_status = SYMPW_MULLIKEN_STATUS_PROJECTIVE
       return
    end if
    if (result%little_group_order /= order .or. result%factor_group_order /= order .or. &
         result%n_allowed_irreps /= result%n_irreps .or. &
         any(abs(result%kpoint_internal - nint(result%kpoint_internal)) > &
         tol_kpoint_snap)) then
       result%mulliken_status = SYMPW_MULLIKEN_STATUS_NOT_FULL_POINT_GROUP
       return
    end if
    if (.not. mulliken_point_group_supported(pgnr_cached)) then
       if (mulliken_point_group_requires_pairing(pgnr_cached)) then
          result%mulliken_status = SYMPW_MULLIKEN_STATUS_COMPLEX_PAIR
          call populate_real_irrep_view(result, label_ok)
          if (.not. label_ok) then
             result%mulliken_status = SYMPW_MULLIKEN_STATUS_RESOLUTION_FAILED
          end if
       else
          result%mulliken_status = SYMPW_MULLIKEN_STATUS_UNSUPPORTED_POINT_GROUP
       end if
       return
    end if
    if (.not. allocated(gel)) return
    if (size(gel) < order) return

    allocate(candidate_labels(size(result%irreps)), stat=alloc_stat)
    if (alloc_stat /= 0) return
    candidate_labels = ""
    do position = 1, size(result%irreps)
       call assign_mulliken_label(pgnr_cached, gel(1:order), pg_data%rgr3, &
            result%irreps(position)%characters, candidate_labels(position), label_ok)
       if (.not. label_ok) then
          deallocate(candidate_labels)
          return
       end if
       do other_position = 1, position - 1
          if (trim(candidate_labels(position)) == trim(candidate_labels(other_position))) then
             deallocate(candidate_labels)
             return
          end if
       end do
    end do

    do position = 1, size(result%irreps)
       result%irreps(position)%mulliken_label = candidate_labels(position)
    end do
    result%mulliken_status = SYMPW_MULLIKEN_STATUS_AVAILABLE
    deallocate(candidate_labels)
  end subroutine populate_mulliken_labels


  subroutine populate_real_irrep_view(result, success)
    type(sympw_result_t), intent(inout) :: result
    logical, intent(out) :: success

    integer, allocatable :: partner(:)
    logical, allocatable :: assigned(:)
    integer :: irrep_position, partner_position, search_position
    integer :: view_position, view_count, member_count, element_index, alloc_stat
    complex(dp) :: character_value
    logical :: label_ok

    success = .false.
    result%real_irrep_view_available = .false.
    if (allocated(result%real_irreps)) deallocate(result%real_irreps)
    if (.not. allocated(result%irreps) .or. .not. allocated(gel)) return
    if (size(gel) < order .or. size(result%irreps) < 1) return

    allocate(partner(size(result%irreps)), assigned(size(result%irreps)), stat=alloc_stat)
    if (alloc_stat /= 0) return
    partner = 0
    assigned = .false.
    view_count = 0
    do irrep_position = 1, size(result%irreps)
       if (assigned(irrep_position)) cycle
       if (maxval(abs(aimag(result%irreps(irrep_position)%characters))) <= &
            tol_irrep_phase) then
          partner(irrep_position) = irrep_position
          assigned(irrep_position) = .true.
          view_count = view_count + 1
          cycle
       end if

       partner_position = 0
       do search_position = irrep_position + 1, size(result%irreps)
          if (assigned(search_position)) cycle
          if (result%irreps(search_position)%dimension /= &
               result%irreps(irrep_position)%dimension .or. &
               result%irreps(search_position)%multiplicity /= &
               result%irreps(irrep_position)%multiplicity) cycle
          if (maxval(abs(result%irreps(search_position)%characters - &
               conjg(result%irreps(irrep_position)%characters))) <= tol_irrep_phase) then
             partner_position = search_position
             exit
          end if
       end do
       if (partner_position == 0) then
          deallocate(partner, assigned)
          return
       end if
       partner(irrep_position) = partner_position
       partner(partner_position) = irrep_position
       assigned(irrep_position) = .true.
       assigned(partner_position) = .true.
       view_count = view_count + 1
    end do

    allocate(result%real_irreps(view_count), stat=alloc_stat)
    if (alloc_stat /= 0) then
       deallocate(partner, assigned)
       return
    end if

    view_position = 0
    do irrep_position = 1, size(result%irreps)
       if (partner(irrep_position) < irrep_position) cycle
       view_position = view_position + 1
       member_count = merge(1, 2, partner(irrep_position) == irrep_position)
       allocate(result%real_irreps(view_position)%member_irrep_positions(member_count), &
            result%real_irreps(view_position)%characters(result%factor_group_order), &
            stat=alloc_stat)
       if (alloc_stat /= 0) then
          deallocate(result%real_irreps, partner, assigned)
          return
       end if
       result%real_irreps(view_position)%member_irrep_positions(1) = irrep_position
       result%real_irreps(view_position)%dimension = result%irreps(irrep_position)%dimension
       result%real_irreps(view_position)%multiplicity = &
            result%irreps(irrep_position)%multiplicity
       result%real_irreps(view_position)%characters = &
            result%irreps(irrep_position)%characters
       if (member_count == 2) then
          partner_position = partner(irrep_position)
          result%real_irreps(view_position)%member_irrep_positions(2) = partner_position
          result%real_irreps(view_position)%dimension = &
               result%real_irreps(view_position)%dimension + &
               result%irreps(partner_position)%dimension
          result%real_irreps(view_position)%characters = &
               result%real_irreps(view_position)%characters + &
               result%irreps(partner_position)%characters
       end if

       do element_index = 1, result%factor_group_order
          character_value = result%real_irreps(view_position)%characters(element_index)
          if (abs(aimag(character_value)) > tol_irrep_phase) then
             deallocate(result%real_irreps, partner, assigned)
             return
          end if
          if (abs(real(character_value, dp)) < tol_character_cleanup) then
             character_value = cmplx(0.0_dp, 0.0_dp, dp)
          else
             character_value = cmplx(real(character_value, dp), 0.0_dp, dp)
          end if
          result%real_irreps(view_position)%characters(element_index) = character_value
       end do

       call assign_real_view_mulliken_label(pgnr_cached, gel(1:order), pg_data%rgr3, &
            result%real_irreps(view_position)%dimension, &
            result%real_irreps(view_position)%characters, &
            result%real_irreps(view_position)%label, label_ok)
       if (.not. label_ok) then
          deallocate(result%real_irreps, partner, assigned)
          return
       end if
       do search_position = 1, view_position - 1
          if (trim(result%real_irreps(search_position)%label) == &
               trim(result%real_irreps(view_position)%label)) then
             deallocate(result%real_irreps, partner, assigned)
             return
          end if
       end do
    end do

    deallocate(partner, assigned)
    result%real_irrep_view_available = .true.
    success = .true.
  end subroutine populate_real_irrep_view


  subroutine sympw_get_real_irrep_projector(result, view_position, projector, success)
    type(sympw_result_t), intent(in) :: result
    integer, intent(in) :: view_position
    complex(dp), allocatable, intent(out) :: projector(:,:)
    logical, intent(out) :: success

    complex(dp), allocatable :: member_projector(:,:)
    integer :: member_index, irrep_position, alloc_stat
    logical :: member_ok

    success = .false.
    if (.not. result%success .or. .not. result%real_irrep_view_available .or. &
         .not. allocated(result%real_irreps)) return
    if (view_position < 1 .or. view_position > size(result%real_irreps)) return
    if (result%real_irreps(view_position)%multiplicity < 1 .or. &
         .not. allocated(result%real_irreps(view_position)%member_irrep_positions)) return

    allocate(projector(result%matrix_order, result%matrix_order), stat=alloc_stat)
    if (alloc_stat /= 0) return
    projector = cmplx(0.0_dp, 0.0_dp, dp)
    do member_index = 1, size(result%real_irreps(view_position)%member_irrep_positions)
       irrep_position = result%real_irreps(view_position)%member_irrep_positions(member_index)
       call sympw_get_irrep_projector(result, irrep_position, member_projector, member_ok)
       if (.not. member_ok) then
          if (allocated(member_projector)) deallocate(member_projector)
          deallocate(projector)
          return
       end if
       projector = projector + member_projector
       deallocate(member_projector)
    end do
    success = .true.
  end subroutine sympw_get_real_irrep_projector


  character(len=24) function make_irrep_label(dimension, characters) result(label)
    integer, intent(in) :: dimension
    complex(dp), intent(in) :: characters(:)

    integer(kind=8), parameter :: hash_modulus = 2147483647_8
    integer(kind=8), parameter :: hash_base = 1000003_8
    real(dp), parameter :: fingerprint_scale = 1.0e5_dp
    integer(kind=8) :: hash_value, value_code
    integer :: element_index

    hash_value = modulo(int(dimension, 8)*hash_base + int(size(characters), 8), &
         hash_modulus)
    do element_index = 1, size(characters)
       value_code = nint(real(characters(element_index), dp)*fingerprint_scale, kind=8)
       hash_value = modulo(hash_value*hash_base + modulo(value_code, hash_modulus), &
            hash_modulus)
       value_code = nint(aimag(characters(element_index))*fingerprint_scale, kind=8)
       hash_value = modulo(hash_value*hash_base + modulo(value_code, hash_modulus), &
            hash_modulus)
    end do
    write(label, '("g",I0,"-d",I0,"-",Z8.8)') size(characters), dimension, hash_value
  end function make_irrep_label


  subroutine sympw_get_irrep_projector(result, irrep_position, projector, success)
    type(sympw_result_t), intent(in) :: result
    integer, intent(in) :: irrep_position
    complex(dp), allocatable, intent(out) :: projector(:,:)
    logical, intent(out) :: success

    integer :: column_start, column_end

    success = .false.
    if (.not. result%success .or. .not. allocated(result%symmetry_basis) .or. &
         .not. allocated(result%irreps)) return
    if (irrep_position < 1 .or. irrep_position > size(result%irreps)) return
    if (result%irreps(irrep_position)%multiplicity < 1) return
    column_start = result%irreps(irrep_position)%column_start
    column_end = result%irreps(irrep_position)%column_end
    if (column_start < 1 .or. column_end < column_start .or. &
         column_end > size(result%symmetry_basis, 2)) return

    call sympw_form_projector(result%symmetry_basis(:, column_start:column_end), &
         projector, success)
  end subroutine sympw_get_irrep_projector


  ! Independently compute k and -k and check spinless TR in the real orbital basis.
  subroutine sympw_check_spinless_time_reversal(kpoint, minus_kpoint, tol, is_valid, max_diff)
    real(dp), intent(in) :: kpoint(3), minus_kpoint(3)
    real(dp), intent(in) :: tol
    logical, intent(out) :: is_valid
    real(dp), intent(out) :: max_diff

    type(sympw_result_t) :: result_k, result_minus_k

    is_valid = .false.
    max_diff = huge(1.0_dp)
    if (.not. library_initialized .or. tol < 0.0_dp) return

    call sympw_analyze_kpoint(kpoint, result_k)
    call sympw_analyze_kpoint(minus_kpoint, result_minus_k)
    if (.not. result_k%success .or. .not. result_minus_k%success) return
    if (.not. allocated(result_k%projector_real) .or. &
         .not. allocated(result_minus_k%projector_real)) return

    call verify_spinless_projector_pair(result_k%projector_real, result_minus_k%projector_real, &
         tol, is_valid, max_diff)
  end subroutine sympw_check_spinless_time_reversal


  ! ============================================
  ! Extract connectivity components from the
  ! projection matrix.
  !
  ! Blocks are identified as connected components
  ! in the graph defined by |P(i,j)| > tol.
  ! These components describe the sparsity graph of one
  ! projector. They are not irreducible-representation
  ! labels and can depend on basis gauge and tolerance.
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

    n_blocks_out = 0
    N = matrix_order
    if (N < 0 .or. tol < 0.0_dp .or. size(projector, 1) /= N .or. &
         size(projector, 2) /= N) then
       allocate(blocks_out(0))
       return
    end if

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

    integer :: i, alloc_stat

    if (present(error_code)) error_code = 0
    info%reduced = .false.
    info%nel = 0
    info%point_group_number = 0
    info%basis_dimension = 0
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
    info%point_group_number = pgnr_cached
    allocate(info%nat(nel_cached), info%lmax(nel_cached), stat=alloc_stat)
    if (alloc_stat /= 0) then
       if (allocated(info%nat)) deallocate(info%nat)
       if (allocated(info%lmax)) deallocate(info%lmax)
       if (present(error_code)) error_code = 2
       return
    end if
    info%nat(:) = nat_arr(:)
    info%lmax(:) = lmax_arr(:)
    do i = 1, nel_cached
       info%basis_dimension = info%basis_dimension + &
            info%nat(i)*(info%lmax(i) + 1)**2
    end do
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
