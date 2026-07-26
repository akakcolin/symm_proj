! ============================================
! Core Symmetry Projection Computation Module
! ============================================
! Encapsulates the k-point symmetry analysis pipeline:
!   little group → irrep → sumsets → projmat
!
! Extracted from orbmain.F90 to provide a reusable
! computation entry point for all frontends.
! ============================================

module sympw_core
  use accuracy
  use constants
  use sympw_group_mode, only: projective_factor_group_active
  use groupkp
  use irrep
  use sumsets
  use projmat
  implicit none
  private

  public :: sympw_compute_kpoint
  public :: sympw_form_projector

contains

  ! Compute the symmetry projection matrix for a single k-point.
  !
  ! This is the main computation pipeline:
  !   1. Determine the little group of k
  !   2. Compute irreducible representations
  !   3. Build summation sets (atom mappings under symmetry)
  !   4. Construct the projection matrix
  !
  ! Input:
  !   rk       - k-point coordinates (fractional, in units of 2*pi/a)
  !   a, ai    - direct lattice and its inverse
  !   b, bi    - reciprocal lattice and its inverse
  !   r        - atomic positions (3, nel, maxnat) in Cartesian
  !   u        - nonprimitive translations (order, 3) in fractional coordinates
  !   nel      - number of chemical elements
  !   nat      - number of atoms per element
  !   lmax     - maximum angular momentum per element
  !   pgnr     - point group number (1..36)
  !   rgr3     - rotation matrices (3, 3, 72)
  !   ldrmm    - Wigner D-matrix elements (72, 84)
  !   mtab     - group multiplication table (order, order)
  !   gel      - group element indices (order)
  !   order    - group order
  !   steer    - control flags (20)
  !   npri     - prime numbers array (100)
  !   tsmall   - tolerance for equality checks
  !   ttsmall  - tolerance for projection checks
  !   ikp      - k-point index (for output messages)
  !   np_inout  - work array: number of symmetry ops mapping atom pairs
  !   nvec_inout - work array: lattice vectors for atom mappings
  !   verbosity   - optional output level: 0 quiet, 1 summary, 2 details, 3 tables
  !
  ! Output:
  !   projmatrix_out - legacy output containing the symmetry-adapted basis T
  !   matrixorder    - total basis function dimension
  !   success        - .true. if computation completed
  ! Optional output metadata:
  !   little_group_order       - order of little co-group G_k
  !   factor_group_order       - order of represented factor/lifted group
  !   factor_group_used        - .true. for nonsymmorphic G_k/T_k branch
  !   n_classes, n_irreps      - conjugacy-class count and irrep count
  !   n_allowed_irreps         - irreps passing the Bloch-phase allow filter
  !   *_dimension_sum          - sums over represented/allowed irrep dimensions
  !   allowed_irrep_*_out      - allowed irrep indices, dimensions, and T-column ranges

  subroutine sympw_compute_kpoint(rk, a, ai, b, bi, nel, nat, lmax, order, r, u, &
       pgnr, rgr3, ldrmm, mtab, gel, steer, npri, tsmall, ttsmall, &
       ikp, matrixorder, projmatrix_out, success, verbosity, &
       little_group_order, factor_group_order, factor_group_used, &
       n_classes, n_irreps, n_allowed_irreps, &
       irrep_dimension_sum, allowed_irrep_dimension_sum, projector_out, &
       allowed_irrep_indices_out, allowed_irrep_dimensions_out, &
       allowed_irrep_column_start_out, allowed_irrep_column_end_out, &
       allowed_irrep_characters_out)
    real(dp), intent(in) :: rk(3)
    real(dp), intent(in) :: a(3,3), ai(3,3), b(3,3), bi(3,3)
    integer, intent(in) :: nel
    integer, intent(in) :: nat(:)
    integer, intent(in) :: lmax(:)
    integer, intent(in) :: order
    real(dp), intent(in) :: r(:, :, :)
    real(dp), intent(in) :: u(order, 3)
    integer, intent(in) :: pgnr
    real(dp), intent(in) :: rgr3(3, 3, 72)
    complex(dp), intent(in) :: ldrmm(72, 84)
    integer, intent(in) :: mtab(:,:)
    integer, intent(in) :: gel(:)
    integer, intent(inout) :: steer(20)
    integer, intent(in) :: npri(100)
    real(dp), intent(in) :: tsmall, ttsmall
    integer, intent(in) :: ikp
    integer, intent(out) :: matrixorder
    complex(dp), allocatable, intent(out) :: projmatrix_out(:,:)
    logical, intent(out) :: success
    integer, intent(in), optional :: verbosity
    integer, intent(out), optional :: little_group_order
    integer, intent(out), optional :: factor_group_order
    logical, intent(out), optional :: factor_group_used
    integer, intent(out), optional :: n_classes
    integer, intent(out), optional :: n_irreps
    integer, intent(out), optional :: n_allowed_irreps
    integer, intent(out), optional :: irrep_dimension_sum
    integer, intent(out), optional :: allowed_irrep_dimension_sum
    complex(dp), allocatable, intent(out), optional :: projector_out(:,:)
    integer, allocatable, intent(out), optional :: allowed_irrep_indices_out(:)
    integer, allocatable, intent(out), optional :: allowed_irrep_dimensions_out(:)
    integer, allocatable, intent(out), optional :: allowed_irrep_column_start_out(:)
    integer, allocatable, intent(out), optional :: allowed_irrep_column_end_out(:)
    complex(dp), allocatable, intent(out), optional :: allowed_irrep_characters_out(:,:)

    ! Local variables - operator-ID-indexed rotation matrices
    real(dp), allocatable :: rgr(:,:,:)

    ! Local variables - k-point group
    integer :: kg, kgord, k2gord
    integer, allocatable :: kgel(:), kkgel(:)
    complex(dp), allocatable :: sil(:)
    real(dp), allocatable :: til(:,:)
    integer, allocatable :: listp(:)
    integer, allocatable :: nopi(:)
    integer :: nopi1, ksym, ibz
    integer, allocatable :: nopli(:,:), nopli1(:)

    ! Local variables - irreps
    complex(dp), allocatable :: jpdd(:,:,:)
    integer, allocatable :: laj(:), allow(:)
    integer :: ncl
    integer :: IV

    ! Local variables - atom mappings and projections
    integer, allocatable :: np(:,:,:)
    real(dp), allocatable :: nvec(:,:,:,:,:)
    integer, allocatable :: npl(:,:,:,:)
    integer :: nal, nblock, nup, nip, nallowed, ichem, L, N
    integer, allocatable :: nalr(:)
    integer, allocatable :: irrep_column_start_all(:), irrep_column_end_all(:)

    ! Other locals
    real(dp) :: ark(3), srk(3), rk_phase(3)
    integer :: I, J, K, K1, K2
    integer :: allowed_position, column_count, covered_columns, metadata_alloc_stat
    complex(dp) :: character_value
    integer :: factor_capacity
    logical :: projective_mode
    integer, allocatable :: mtab2(:, :)
    integer :: out_level
    integer :: saved_steer(20)
    logical :: factor_group_ok, mapping_ok, override_verbosity, irrep_ok, projection_ok
    logical :: projector_ok, metadata_ok

    success = .false.
    if (present(little_group_order)) little_group_order = 0
    if (present(factor_group_order)) factor_group_order = 0
    if (present(factor_group_used)) factor_group_used = .false.
    if (present(n_classes)) n_classes = 0
    if (present(n_irreps)) n_irreps = 0
    if (present(n_allowed_irreps)) n_allowed_irreps = 0
    if (present(irrep_dimension_sum)) irrep_dimension_sum = 0
    if (present(allowed_irrep_dimension_sum)) allowed_irrep_dimension_sum = 0
    out_level = 1
    override_verbosity = present(verbosity)
    if (override_verbosity) out_level = max(0, verbosity)

    if (nel < 1 .or. size(nat) < nel .or. size(lmax) < nel) then
       write(*,*) "Invalid crystal metadata for k-point projection"
       return
    end if
    if (pgnr < 1 .or. pgnr > 36) then
       write(*,*) "Invalid point-group number for k-point projection:", pgnr
       return
    end if
    if (any(lmax(1:nel) < 0) .or. any(lmax(1:nel) > maxL)) then
       write(*,*) "K-point projection lmax must be in the range 0..", maxL
       return
    end if
    if (order < 1 .or. order > 48) then
       write(*,*) "Invalid point-group order for k-point projection:", order
       return
    end if
    if (size(rk) < 3 .or. size(r, 1) < 3 .or. size(r, 2) < nel .or. &
         size(r, 3) < maxval(nat(1:nel))) then
       write(*,*) "Crystal position arrays are too small for k-point projection"
       return
    end if
    if (size(mtab, 1) < order .or. size(mtab, 2) < order .or. size(gel) < order .or. &
         size(steer) < 20 .or. size(npri) < 100) then
       write(*,*) "Group workspace arrays are too small for k-point projection"
       return
    end if
    call snap_fractional_kpoint(rk(1:3), ark(1:3))
    srk(1:3) = ark(1:3)
    rk_phase(1:3) = ark(1:3) * 2*pi
    IV = 1
    ibz = 1
    nopi1 = 1
    ksym = 1
    factor_group_ok = .true.
    ! Crystallographic screw/glide translations can produce Bloch phases
    ! of order up to 12 (for example a 6_1 screw at a zone boundary).
    ! Keep the extension finite, but do not reject those valid cases solely
    ! because the workspace was sized for a fourfold phase.
    factor_capacity = max(100, max_projective_phase_order*order)

    ! Determine total basis dimension
    matrixorder = 0
    do ichem = 1, nel
       N = 0
       do L = 0, lmax(ichem)
          N = N + 2*L + 1
       end do
       matrixorder = matrixorder + nat(ichem)*N
    end do

    if (out_level >= 1) then
       write(*,*)
       write(*,*) "------------------------------------------"
       write(*,'(A,I3,A,3F10.5)') " K-point ", ikp, ": ", rk(1:3)
       write(*,*) "------------------------------------------"
    end if

    ! Build position-indexed rotation matrices from raw rgr3 table.
    ! rgr3 is indexed by rotation_table_index (= operator_id + parent_offset).
    ! Subroutines access rgr by position index (kgel values are positions),
    ! so we build rgr(3, 3, order) indexed by position in the point group.
    allocate(rgr(3, 3, order))
    do I = 1, order
       rgr(1:3, 1:3, I) = rgr3(1:3, 1:3, &
            rotation_table_index(gel(I), pgnr))
    end do

    allocate(kgel(order))
    allocate(kkgel(order))
    allocate(sil(factor_capacity))
    allocate(til(factor_capacity, 3))
    allocate(nopi(order))
    allocate(nopli1(factor_capacity))
    allocate(nopli(order, factor_capacity))
    allocate(listp(factor_capacity))
    allocate(mtab2(factor_capacity, factor_capacity))
    sil(:) = cmplx(0, 0, dp)
    til(:,:) = 0.0_dp
    nopi(:) = 0
    nopli1(:) = 1
    nopli(:,:) = 0
    listp(:) = 0
    mtab2(:,:) = 0

    ! Determine little group
    if (all(abs(rk(1:3) - nint(rk(1:3))) < tsmall)) then
       ! Gamma point: little group = full point group
       if (out_level >= 1) then
          write(*,*) "This is the Gamma point (k=0)"
          write(*,*) "The little group equals the full point group"
       end if
       kgord = order
       mtab2(1:kgord, 1:kgord) = mtab(1:kgord, 1:kgord)
       do I = 1, kgord
          kgel(I) = I
          kkgel(I) = gel(I)
       end do
       ibz = 1
       kg = kgord
       ! Gamma point has no non-symmorphic structure; initialize to safe defaults.
       nopi(:) = 0
       nopli1(:) = 1
       nopli(:,:) = 0
       listp(:) = 0
    else
       if (out_level >= 1) write(*,*) "Computing the little group..."
       sil(1) = cmplx(1, 0, dp)
       kgel(1) = 1
       kgord = 1

       nopli1(:) = 1

       call sym_groupkp(kg, kgord, k2gord, kgel, kkgel, mtab2, ibz, listp, &
            nopi, nopi1, nopli, nopli1, sil, til, ksym, rk_phase, ark, a, ai, b, bi, u, order, pgnr, &
            rgr, mtab, gel, steer, tsmall, factor_group_ok)
       if (out_level >= 1) write(*,'(A,I3,A)') " Little group order: ", kg, " elements"
    end if

    if (.not. factor_group_ok) then
       if (out_level >= 1) then
          write(*,*) "Projective phases do not close within the supported finite factor group"
          write(*,*) "Projection skipped for this k-point"
       end if
       if (present(little_group_order)) little_group_order = kg
       if (present(factor_group_order)) factor_group_order = kg
       deallocate(kgel, kkgel, sil, til)
       deallocate(rgr, nopi, nopli1, nopli, listp, mtab2)
       return
    end if

    if (out_level >= 1) then
       write(*,'(A,3F10.6)') " Wave vector k: ", srk(1:3)
       write(*,'(A,I4,A)') " Little group has ", kg, " symmetry operators"
    end if
    if (out_level >= 2) then
       write(*,*) "Operator indices in the full point group:"
       do I = 1, kg, 12
          write(*,'(12I6)') kkgel(I:min(I+11, kg))
       end do
    end if

    projective_mode = projective_factor_group_active(steer(20), ksym, ibz)
    if (present(little_group_order)) little_group_order = kg
    if (present(factor_group_order)) factor_group_order = kgord
    if (present(factor_group_used)) factor_group_used = projective_mode
    if (projective_mode) then
       if (out_level >= 1) then
          write(*,'(A,I4,A)') " Factor group Gk/Tk active with ", kgord, " lifted elements"
       end if
       if (out_level >= 2) then
          write(*,*) "Factor group Gk/Tk (for nonsymmorphic space groups):"
          do I = 1, kgord
             write(*,'(A,I3,A,I4)') "  Element ", I, ": Point group operator ", kkgel(listp(I))
             write(*,'(A,3F8.4,A,2F8.4,A)') "    Translation: (", til(I, 1:3), "), Phase factor: ", sil(I), ")"
          end do
       end if
    end if

    if (out_level >= 1) write(*,*) "Computing irreducible representations of the little group..."

    if ((IV <= 2) .or. .not. projective_mode) then
       allocate(jpdd(kgord, maxdim, kgord))
       allocate(laj(kgord))
       allocate(allow(kgord))
       jpdd(:,:,:) = 0

       saved_steer(:) = steer(:)
       steer(11) = 1  ! reset per-k-point; sym_irrep sets to 0 only if table invalid
       if (override_verbosity) then
          steer(2) = 1
          steer(6) = merge(1, 0, out_level >= 3)
          steer(7) = merge(1, 0, out_level >= 3)
          steer(8) = merge(1, 0, out_level >= 3)
          steer(9) = merge(1, 0, out_level >= 3)
          steer(12) = merge(1, 0, out_level >= 3)
          steer(18) = merge(1, 0, out_level >= 3)
       end if
       call sym_irrep(jpdd, allow, ncl, laj, kgord, mtab2, npri, steer, ibz, ksym, nopi1, nopli1, sil)
       irrep_ok = (steer(11) /= 0)
       if (override_verbosity) then
          steer(:) = saved_steer(:)
          if (.not. irrep_ok) steer(11) = 0
       end if

       if (out_level >= 1) write(*,'(A,I3,A)') " Found ", ncl, " conjugacy classes"
       if (present(n_classes)) n_classes = ncl
       if (present(n_irreps)) n_irreps = ncl

       allocate(nalr(ncl))
       if (.not. irrep_ok) then
          deallocate(jpdd, laj, allow, nalr, kgel, kkgel, sil, til)
          deallocate(rgr, nopi, nopli1, nopli, listp, mtab2)
          success = .false.
          return
       end if

       nup = 0
       nip = 0
       nallowed = 0
       do I = 1, ncl
          nip = nip + laj(I)
          if (allow(I) .ne. 0) then
             nup = nup + laj(I)
             nallowed = nallowed + 1
          end if
       end do

       if (out_level >= 1) then
          write(*,'(A,I3,A)') " Irreducible representations: ", ncl
          write(*,'(A,I3,A)') " Allowed representations: ", nallowed
          write(*,'(A,I3)') " Irrep dimension sum: ", nip
          write(*,'(A,I3)') " Allowed irrep dimension sum: ", nup
       end if
       if (present(n_allowed_irreps)) n_allowed_irreps = nallowed
       if (present(irrep_dimension_sum)) irrep_dimension_sum = nip
       if (present(allowed_irrep_dimension_sum)) allowed_irrep_dimension_sum = nup

       if (nip .ne. nup) then
          nal = 0
          do I = 1, ncl
             if (allow(I) .ne. 0) then
                nal = nal + 1
                nalr(nal) = I
             end if
          end do
       end if

       nblock = 0
       do ichem = 1, nel
          nblock = nblock + lmax(ichem) + 1
       end do

       if (out_level >= 1) then
          write(*,*) "Constructing projection matrices..."
          write(*,'(A,I3,A,I3)') " Matrix dimension: ", matrixorder, " x ", matrixorder
       end if

       ! Allocate work arrays for atom mappings
       allocate(np(nel, maxval(nat), maxval(nat)))
       allocate(nvec(nel, maxval(nat), maxval(nat), factor_capacity, 3))
       allocate(npl(nel, maxval(nat), maxval(nat), factor_capacity))
       np(:,:,:) = 0
       nvec(:,:,:,:,:) = 0
       npl(:,:,:,:) = 0

       call sym_sumsets(np, nvec, npl, til, kgord, kgel, rgr, listp, &
            a, ai, b, r, u, nel, nat, ksym, ibz, steer, success=mapping_ok)
       if (.not. mapping_ok) then
          deallocate(laj, allow, jpdd, np, nvec, npl, nalr)
          deallocate(kgel, kkgel, sil, til)
          deallocate(rgr, nopi, nopli1, nopli, listp, mtab2)
          return
       end if

       allocate(projmatrix_out(matrixorder, matrixorder))
       projmatrix_out(:,:) = 0

       allocate(irrep_column_start_all(ncl), irrep_column_end_all(ncl), &
            stat=metadata_alloc_stat)
       if (metadata_alloc_stat /= 0) then
          deallocate(projmatrix_out)
          deallocate(laj, allow, jpdd, np, nvec, npl, nalr)
          deallocate(kgel, kkgel, sil, til)
          deallocate(rgr, nopi, nopli1, nopli, listp, mtab2)
          return
       end if

       call sym_projmat(laj, kgord, allow, jpdd, projmatrix_out, nvec, nat, lmax, np, nel, ncl, npl, &
            kgel, kkgel, listp, steer, ksym, ibz, pgnr, ldrmm, rk_phase, u, tsmall, ttsmall, &
            projection_ok, irrep_column_start_all, irrep_column_end_all)

       if (.not. projection_ok) then
          deallocate(projmatrix_out, irrep_column_start_all, irrep_column_end_all)
          deallocate(laj, allow, jpdd, np, nvec, npl, nalr)
          deallocate(kgel, kkgel, sil, til)
          deallocate(rgr, nopi, nopli1, nopli, listp, mtab2)
          return
       end if

       metadata_ok = .true.
       covered_columns = 0
       do I = 1, ncl
          if (allow(I) == 0) cycle
          if (irrep_column_start_all(I) == 0 .and. irrep_column_end_all(I) == 0) cycle
          column_count = irrep_column_end_all(I) - irrep_column_start_all(I) + 1
          if (irrep_column_start_all(I) < 1 .or. &
               irrep_column_end_all(I) > matrixorder .or. column_count < 1 .or. &
               mod(column_count, laj(I)) /= 0) then
             metadata_ok = .false.
             exit
          end if
          covered_columns = covered_columns + column_count
       end do
       if (covered_columns /= matrixorder) metadata_ok = .false.
       if (.not. metadata_ok) then
          write(*,*) "Irrep column metadata is inconsistent with the symmetry basis"
          deallocate(projmatrix_out, irrep_column_start_all, irrep_column_end_all)
          deallocate(laj, allow, jpdd, np, nvec, npl, nalr)
          deallocate(kgel, kkgel, sil, til)
          deallocate(rgr, nopi, nopli1, nopli, listp, mtab2)
          return
       end if

       if (present(allowed_irrep_indices_out)) then
          allocate(allowed_irrep_indices_out(nallowed), stat=metadata_alloc_stat)
          if (metadata_alloc_stat /= 0) metadata_ok = .false.
       end if
       if (present(allowed_irrep_dimensions_out)) then
          allocate(allowed_irrep_dimensions_out(nallowed), stat=metadata_alloc_stat)
          if (metadata_alloc_stat /= 0) metadata_ok = .false.
       end if
       if (present(allowed_irrep_column_start_out)) then
          allocate(allowed_irrep_column_start_out(nallowed), stat=metadata_alloc_stat)
          if (metadata_alloc_stat /= 0) metadata_ok = .false.
       end if
       if (present(allowed_irrep_column_end_out)) then
          allocate(allowed_irrep_column_end_out(nallowed), stat=metadata_alloc_stat)
          if (metadata_alloc_stat /= 0) metadata_ok = .false.
       end if
       if (present(allowed_irrep_characters_out)) then
          allocate(allowed_irrep_characters_out(nallowed, kgord), stat=metadata_alloc_stat)
          if (metadata_alloc_stat /= 0) metadata_ok = .false.
       end if
       if (.not. metadata_ok) then
          if (present(allowed_irrep_indices_out)) then
             if (allocated(allowed_irrep_indices_out)) deallocate(allowed_irrep_indices_out)
          end if
          if (present(allowed_irrep_dimensions_out)) then
             if (allocated(allowed_irrep_dimensions_out)) deallocate(allowed_irrep_dimensions_out)
          end if
          if (present(allowed_irrep_column_start_out)) then
             if (allocated(allowed_irrep_column_start_out)) deallocate(allowed_irrep_column_start_out)
          end if
          if (present(allowed_irrep_column_end_out)) then
             if (allocated(allowed_irrep_column_end_out)) deallocate(allowed_irrep_column_end_out)
          end if
          if (present(allowed_irrep_characters_out)) then
             if (allocated(allowed_irrep_characters_out)) deallocate(allowed_irrep_characters_out)
          end if
          deallocate(projmatrix_out, irrep_column_start_all, irrep_column_end_all)
          deallocate(laj, allow, jpdd, np, nvec, npl, nalr)
          deallocate(kgel, kkgel, sil, til)
          deallocate(rgr, nopi, nopli1, nopli, listp, mtab2)
          return
       end if

       allowed_position = 0
       do I = 1, ncl
          if (allow(I) == 0) cycle
          allowed_position = allowed_position + 1
          if (present(allowed_irrep_indices_out)) allowed_irrep_indices_out(allowed_position) = I
          if (present(allowed_irrep_dimensions_out)) allowed_irrep_dimensions_out(allowed_position) = laj(I)
          if (present(allowed_irrep_column_start_out)) then
             allowed_irrep_column_start_out(allowed_position) = irrep_column_start_all(I)
          end if
          if (present(allowed_irrep_column_end_out)) then
             allowed_irrep_column_end_out(allowed_position) = irrep_column_end_all(I)
          end if
          if (present(allowed_irrep_characters_out)) then
             do K = 1, kgord
                character_value = conjg(sum(jpdd(I, 1:laj(I), K)))
                if (abs(real(character_value, dp)) < tol_character_cleanup) then
                   character_value = cmplx(0.0_dp, aimag(character_value), dp)
                end if
                if (abs(aimag(character_value)) < tol_character_cleanup) then
                   character_value = cmplx(real(character_value, dp), 0.0_dp, dp)
                end if
                allowed_irrep_characters_out(allowed_position, K) = character_value
             end do
          end if
       end do

       if (present(projector_out)) then
          call sympw_form_projector(projmatrix_out, projector_out, projector_ok)
          if (.not. projector_ok) then
             if (allocated(projector_out)) deallocate(projector_out)
             if (present(allowed_irrep_indices_out)) then
                if (allocated(allowed_irrep_indices_out)) deallocate(allowed_irrep_indices_out)
             end if
             if (present(allowed_irrep_dimensions_out)) then
                if (allocated(allowed_irrep_dimensions_out)) deallocate(allowed_irrep_dimensions_out)
             end if
             if (present(allowed_irrep_column_start_out)) then
                if (allocated(allowed_irrep_column_start_out)) deallocate(allowed_irrep_column_start_out)
             end if
             if (present(allowed_irrep_column_end_out)) then
                if (allocated(allowed_irrep_column_end_out)) deallocate(allowed_irrep_column_end_out)
             end if
             if (present(allowed_irrep_characters_out)) then
                if (allocated(allowed_irrep_characters_out)) deallocate(allowed_irrep_characters_out)
             end if
             deallocate(projmatrix_out, irrep_column_start_all, irrep_column_end_all)
             deallocate(laj, allow, jpdd, np, nvec, npl, nalr)
             deallocate(kgel, kkgel, sil, til)
             deallocate(rgr, nopi, nopli1, nopli, listp, mtab2)
             return
          end if
       end if

       if (out_level >= 1) write(*,*) "Projection matrix construction complete."
       success = .true.

       deallocate(irrep_column_start_all, irrep_column_end_all)
       deallocate(laj, allow, jpdd, np, nvec, npl, nalr)
    end if

    deallocate(kgel, kkgel, sil, til)
    deallocate(rgr, nopi, nopli1, nopli, listp, mtab2)

  end subroutine sympw_compute_kpoint


  ! Form the projector P = T*T^H from one or more orthonormal basis columns.
  subroutine sympw_form_projector(symmetry_basis, projector, success)
    complex(dp), intent(in) :: symmetry_basis(:,:)
    complex(dp), allocatable, intent(out) :: projector(:,:)
    logical, intent(out) :: success

    integer :: alloc_stat

    success = .false.
    if (size(symmetry_basis, 1) < 1 .or. size(symmetry_basis, 2) < 1 .or. &
         size(symmetry_basis, 2) > size(symmetry_basis, 1)) return

    allocate(projector(size(symmetry_basis, 1), size(symmetry_basis, 1)), stat=alloc_stat)
    if (alloc_stat /= 0) return
    projector = matmul(symmetry_basis, transpose(conjg(symmetry_basis)))
    success = .true.
  end subroutine sympw_form_projector

end module sympw_core
