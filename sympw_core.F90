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
  use groupkp
  use irrep
  use sumsets
  use projmat
  implicit none
  private

  public :: sympw_compute_kpoint

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
  !   projmatrix_out - projection matrix (matrixorder, matrixorder)
  !   matrixorder    - total basis function dimension
  !   success        - .true. if computation completed

  subroutine sympw_compute_kpoint(rk, a, ai, b, bi, nel, nat, lmax, order, r, u, &
       pgnr, rgr3, ldrmm, mtab, gel, steer, npri, tsmall, ttsmall, &
       ikp, matrixorder, projmatrix_out, success, verbosity)
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
    integer :: nal, nblock, nup, nip, ichem, L, N
    integer, allocatable :: nalr(:)

    ! Other locals
    real(dp) :: ark(3), srk(3), rk_phase(3)
    integer :: I, J, K, K1, K2
    integer :: factor_capacity
    logical :: is_ski
    integer, allocatable :: mtab2(:, :)
    integer :: out_level
    integer :: saved_steer(20)
    logical :: override_verbosity, irrep_ok

    success = .false.
    out_level = 1
    override_verbosity = present(verbosity)
    if (override_verbosity) out_level = max(0, verbosity)
    ark(1:3) = rk(1:3)
    srk(1:3) = rk(1:3)
    rk_phase(1:3) = rk(1:3) * 2*pi
    IV = 1
    ibz = 1
    nopi1 = 1
    ksym = 1
    factor_capacity = max(100, 4*order)

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
            rgr, mtab, gel, steer, tsmall)
       if (out_level >= 1) write(*,'(A,I3,A)') " Little group order: ", kg, " elements"
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

    is_ski = ((steer(20) .ne. 0) .or. (ksym .ne. 0) .or. (ibz .ne. 0))
    if (.not. is_ski) then
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

    if ((IV <= 2) .or. is_ski) then
       allocate(jpdd(kgord, kgord, kgord))
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

       allocate(nalr(ncl))
       if (.not. irrep_ok) then
          deallocate(jpdd, laj, allow, nalr, kgel, kkgel, sil, til)
          deallocate(rgr, nopi, nopli1, nopli, listp, mtab2)
          success = .false.
          return
       end if

       nup = 0
       nip = 0
       do I = 1, ncl
          nip = nip + laj(I)
          if (allow(I) .ne. 0) nup = nup + laj(I)
       end do

       if (out_level >= 1) then
          write(*,'(A,I3,A)') " Total irreducible representations: ", nip
          write(*,'(A,I3,A)') " Allowed representations: ", nup
       end if

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

       call sym_sumsets(np, nvec, npl, til, kgord, kgel, rgr, listp, a, ai, b, r, u, nel, nat, ksym, ibz, steer)

       allocate(projmatrix_out(matrixorder, matrixorder))
       projmatrix_out(:,:) = 0

       call sym_projmat(laj, kgord, allow, jpdd, projmatrix_out, nvec, nat, lmax, np, nel, ncl, npl, &
            kgel, kkgel, listp, steer, ksym, ibz, pgnr, ldrmm, rk_phase, u, tsmall, ttsmall)

       if (out_level >= 1) write(*,*) "Projection matrix construction complete."
       success = .true.

       deallocate(laj, allow, jpdd, np, nvec, npl, nalr)
    end if

    deallocate(kgel, kkgel, sil, til)
    deallocate(rgr, nopi, nopli1, nopli, listp, mtab2)

  end subroutine sympw_compute_kpoint

end module sympw_core
