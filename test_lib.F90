! ============================================
! Comprehensive test program for sympw_lib API
! ============================================
! Tests cover:
!   Scenario 1: Oh (36), Gamma,        1 atom, lmax=2  (baseline)
!   Scenario 2: Oh (36), X-point,      1 atom, lmax=2  (non-Gamma little group)
!   Scenario 3: Td (35), Gamma,        1 atom, lmax=2  (subgroup remap, Oh parent)
!   Scenario 4: D6h(31), Gamma,        1 atom, lmax=2  (MD6h multiplication table)
!   Scenario 5: D2h (8), Gamma,        1 atom, lmax=1  (low symmetry, orthorhombic)
!   Scenario 6: D6h(31), K-point,      1 atom, lmax=1  (D6h non-Gamma)
!   Scenario 7: C4v(13), Gamma,        1 atom, lmax=1  (tetragonal, Oh subgroup)
!   Scenario 8: Oh (36), Gamma,        2 atoms, lmax=2  (Si diamond primitive cell, non-symmorphic)
!   Scenario 9: Oh (36), L-point,      8 atoms, lmax=2  (Si diamond conventional cell reduction)
!
! Each scenario verifies:
!   (1) matrix_order matches the expected basis size
!   (2) P^2 = P  (idempotency)
!   (3) P = P^†  (Hermiticity)
!   (4) Tr(P) integer
!   (5) Real SH transform: P_real is real, still a projector
!   (6) Block decomposition: sum(dim) = matrix_order
! ============================================

program test_lib
  use accuracy
  use sympw_lib
  use sympw_real_sh, only: complex_to_real_projector
  implicit none

  type(sympw_crystal_t) :: crystal
  type(sympw_result_t) :: result
  real(dp) :: kpoint(3), max_err, trace_val
  integer, parameter :: checks_per_scenario = 7
  integer, parameter :: cell_info_checks = 3
  integer, parameter :: scenario9_checks = checks_per_scenario + cell_info_checks
  integer, parameter :: expected_scenarios = 9
  integer :: error_code, passed, total, scenario_passed, scenarios_ok, expected_order
  real(dp) :: expected_transform(3,3)

  passed = 0
  total = 0
  scenarios_ok = 0

  write(*,*)
  write(*,*) "=========================================="
  write(*,*) "sympw_lib Comprehensive Test Suite"
  write(*,*) "=========================================="

  ! ==========================================
  ! Scenario 1: Oh (36), Gamma, lmax=2
  ! ==========================================
  write(*,*)
  write(*,*) "--- Scenario 1: Oh(36), Gamma, 1 atom, lmax=2 ---"
  scenario_passed = 0
  expected_order = 9

  call setup_cubic_1atom(crystal, 2, 36)
  call sympw_init(crystal, error_code)
  if (error_code /= 0) then
     write(*,*) "  FAIL: init error_code =", error_code
  else
     kpoint(:) = (/0.0_dp, 0.0_dp, 0.0_dp/)
     call sympw_analyze_kpoint(kpoint, result)
     call check_result(result, expected_order, scenario_passed, total, max_err, trace_val, crystal%lmax, crystal%nat)
     call sympw_finalize()
  end if
  call teardown_crystal(crystal)
  passed = passed + scenario_passed
  if (scenario_passed == checks_per_scenario) scenarios_ok = scenarios_ok + 1
  write(*,'(A,I1,A)') "  Scenario 1: ", scenario_passed, "/7 checks passed"

  ! ==========================================
  ! Scenario 2: Oh (36), X-point, lmax=2 (non-Gamma)
  ! ==========================================
  write(*,*)
  write(*,*) "--- Scenario 2: Oh(36), X-point, 1 atom, lmax=2 ---"
  scenario_passed = 0
  expected_order = 9

  call setup_cubic_1atom(crystal, 2, 36)
  call sympw_init(crystal, error_code)
  if (error_code /= 0) then
     write(*,*) "  FAIL: init error_code =", error_code
  else
     kpoint(:) = (/0.5_dp, 0.0_dp, 0.0_dp/)
     call sympw_analyze_kpoint(kpoint, result)
     call check_result(result, expected_order, scenario_passed, total, max_err, trace_val, crystal%lmax, crystal%nat)
     call sympw_finalize()
  end if
  call teardown_crystal(crystal)
  passed = passed + scenario_passed
  if (scenario_passed == checks_per_scenario) scenarios_ok = scenarios_ok + 1
  write(*,'(A,I1,A)') "  Scenario 2: ", scenario_passed, "/7 checks passed"

  ! ==========================================
  ! Scenario 3: Td (35), Gamma, lmax=2 (subgroup of Oh)
  ! ==========================================
  write(*,*)
  write(*,*) "--- Scenario 3: Td(35), Gamma, 1 atom, lmax=2 ---"
  scenario_passed = 0
  expected_order = 9

  call setup_cubic_1atom(crystal, 2, 35)
  call sympw_init(crystal, error_code)
  if (error_code /= 0) then
     write(*,*) "  FAIL: init error_code =", error_code
  else
     kpoint(:) = (/0.0_dp, 0.0_dp, 0.0_dp/)
     call sympw_analyze_kpoint(kpoint, result)
     call check_result(result, expected_order, scenario_passed, total, max_err, trace_val, crystal%lmax, crystal%nat)
     call sympw_finalize()
  end if
  call teardown_crystal(crystal)
  passed = passed + scenario_passed
  if (scenario_passed == checks_per_scenario) scenarios_ok = scenarios_ok + 1
  write(*,'(A,I1,A)') "  Scenario 3: ", scenario_passed, "/7 checks passed"

  ! ==========================================
  ! Scenario 4: D6h (31), Gamma, lmax=2 (hexagonal, MD6h table)
  ! ==========================================
  write(*,*)
  write(*,*) "--- Scenario 4: D6h(31), Gamma, 1 atom, lmax=2 ---"
  scenario_passed = 0
  expected_order = 9

  call setup_hexagonal_1atom(crystal, 2, 31)
  call sympw_init(crystal, error_code)
  if (error_code /= 0) then
     write(*,*) "  FAIL: init error_code =", error_code
  else
     kpoint(:) = (/0.0_dp, 0.0_dp, 0.0_dp/)
     call sympw_analyze_kpoint(kpoint, result)
     call check_result(result, expected_order, scenario_passed, total, max_err, trace_val, crystal%lmax, crystal%nat)
     call sympw_finalize()
  end if
  call teardown_crystal(crystal)
  passed = passed + scenario_passed
  if (scenario_passed == checks_per_scenario) scenarios_ok = scenarios_ok + 1
  write(*,'(A,I1,A)') "  Scenario 4: ", scenario_passed, "/7 checks passed"

  ! ==========================================
  ! Scenario 5: D2h (8), Gamma, lmax=1 (orthorhombic)
  ! ==========================================
  write(*,*)
  write(*,*) "--- Scenario 5: D2h(8), Gamma, 1 atom, lmax=1 ---"
  scenario_passed = 0
  expected_order = 4

  call setup_orthorhombic_1atom(crystal, 1, 8)
  call sympw_init(crystal, error_code)
  if (error_code /= 0) then
     write(*,*) "  FAIL: init error_code =", error_code
  else
     kpoint(:) = (/0.0_dp, 0.0_dp, 0.0_dp/)
     call sympw_analyze_kpoint(kpoint, result)
     call check_result(result, expected_order, scenario_passed, total, max_err, trace_val, crystal%lmax, crystal%nat)
     call sympw_finalize()
  end if
  call teardown_crystal(crystal)
  passed = passed + scenario_passed
  if (scenario_passed == checks_per_scenario) scenarios_ok = scenarios_ok + 1
  write(*,'(A,I1,A)') "  Scenario 5: ", scenario_passed, "/7 checks passed"

  ! ==========================================
  ! Scenario 6: D6h (31), K-point, lmax=1 (hexagonal non-Gamma)
  ! ==========================================
  write(*,*)
  write(*,*) "--- Scenario 6: D6h(31), K-point, 1 atom, lmax=1 ---"
  scenario_passed = 0
  expected_order = 4

  call setup_hexagonal_1atom(crystal, 1, 31)
  call sympw_init(crystal, error_code)
  if (error_code /= 0) then
     write(*,*) "  FAIL: init error_code =", error_code
  else
     ! K-point in hexagonal BZ: (1/3, 1/3, 0)
     kpoint(:) = (/0.3333333333333_dp, 0.3333333333333_dp, 0.0_dp/)
     call sympw_analyze_kpoint(kpoint, result)
     call check_result(result, expected_order, scenario_passed, total, max_err, trace_val, crystal%lmax, crystal%nat)
     call sympw_finalize()
  end if
  call teardown_crystal(crystal)
  passed = passed + scenario_passed
  if (scenario_passed == checks_per_scenario) scenarios_ok = scenarios_ok + 1
  write(*,'(A,I1,A)') "  Scenario 6: ", scenario_passed, "/7 checks passed"

  ! ==========================================
  ! Scenario 7: C4v (13), Gamma, lmax=1 (tetragonal, Oh subgroup)
  ! ==========================================
  write(*,*)
  write(*,*) "--- Scenario 7: C4v(13), Gamma, 1 atom, lmax=1 ---"
  scenario_passed = 0
  expected_order = 4

  call setup_tetragonal_1atom(crystal, 1, 13)
  call sympw_init(crystal, error_code)
  if (error_code /= 0) then
     write(*,*) "  FAIL: init error_code =", error_code
  else
     kpoint(:) = (/0.0_dp, 0.0_dp, 0.0_dp/)
     call sympw_analyze_kpoint(kpoint, result)
     call check_result(result, expected_order, scenario_passed, total, max_err, trace_val, crystal%lmax, crystal%nat)
     call sympw_finalize()
  end if
  call teardown_crystal(crystal)
  passed = passed + scenario_passed
  if (scenario_passed == checks_per_scenario) scenarios_ok = scenarios_ok + 1
  write(*,'(A,I1,A)') "  Scenario 7: ", scenario_passed, "/7 checks passed"

  ! ==========================================
  ! Scenario 8: Oh (36), Gamma, Si diamond primitive cell
  !             2 atoms, lmax=2, FCC lattice
  !             Tests non-primitive translations
  !             with proper cocycle check.
  ! ==========================================
  write(*,*)
  write(*,*) "--- Scenario 8: Oh(36), Gamma, Si diamond (2 atoms, lmax=2) ---"
  scenario_passed = 0
  expected_order = 18

  call setup_diamond_primitive(crystal, 2, 36)
  call sympw_init(crystal, error_code)
  if (error_code /= 0) then
     write(*,*) "  FAIL: init error_code =", error_code
  else
     kpoint(:) = (/0.0_dp, 0.0_dp, 0.0_dp/)
     call sympw_analyze_kpoint(kpoint, result)
     call check_result(result, expected_order, scenario_passed, total, max_err, trace_val, crystal%lmax, crystal%nat)
     call sympw_finalize()
  end if
  call teardown_crystal(crystal)
  passed = passed + scenario_passed
  if (scenario_passed == checks_per_scenario) scenarios_ok = scenarios_ok + 1
  write(*,'(A,I1,A)') "  Scenario 8: ", scenario_passed, "/7 checks passed"

  ! ==========================================
  ! Scenario 9: Oh (36), L-point, Si diamond conventional cell
  !             8 atoms in FCC conventional coordinates.
  !             sympw_init should reduce to the 2-atom primitive
  !             cell and sympw_analyze_kpoint should transform
  !             reciprocal fractional coordinates accordingly.
  ! ==========================================
  write(*,*)
  write(*,*) "--- Scenario 9: Oh(36), L-point, Si diamond conventional cell ---"
  scenario_passed = 0
  expected_order = 18

  call setup_diamond_conventional(crystal, 2, 36)
  call sympw_init(crystal, error_code)
  if (error_code /= 0) then
     write(*,*) "  FAIL: init error_code =", error_code
  else
     expected_transform(:, :) = 0.0_dp
     expected_transform(1, :) = (/0.0_dp, 0.5_dp, 0.5_dp/)
     expected_transform(2, :) = (/0.5_dp, 0.0_dp, 0.5_dp/)
     expected_transform(3, :) = (/0.5_dp, 0.5_dp, 0.0_dp/)
     call check_cell_info(.true., 2, expected_transform, scenario_passed, total)
     kpoint(:) = (/0.5_dp, 0.5_dp, 0.5_dp/)
     call sympw_analyze_kpoint(kpoint, result)
     call check_result(result, expected_order, scenario_passed, total, max_err, trace_val, crystal%lmax, (/2/))
     call sympw_finalize()
  end if
  call teardown_crystal(crystal)
  passed = passed + scenario_passed
  if (scenario_passed == scenario9_checks) scenarios_ok = scenarios_ok + 1
  write(*,'(A,I2,A)') "  Scenario 9: ", scenario_passed, "/10 checks passed"

  ! ==========================================
  ! Summary
  ! ==========================================
  write(*,*)
  write(*,*) "=========================================="
  write(*,'(A,I2,A,I2,A)') " Overall: ", passed, " / ", total, " checks passed"
  write(*,'(A,I1,A,I1,A)') " Scenarios fully passing: ", scenarios_ok, " / ", expected_scenarios
  write(*,*) "=========================================="

  if (passed == total .and. scenarios_ok == expected_scenarios) then
     write(*,*) "All checks passed!"
  else
     write(*,*) "Some checks failed."
     stop 1
  end if

contains

  ! ----- Setup helpers -----

  subroutine setup_cubic_1atom(c, lmax_val, pgnr_val)
    type(sympw_crystal_t), intent(out) :: c
    integer, intent(in) :: lmax_val, pgnr_val
    c%lattice(1,:) = (/5.0_dp, 0.0_dp, 0.0_dp/)
    c%lattice(2,:) = (/0.0_dp, 5.0_dp, 0.0_dp/)
    c%lattice(3,:) = (/0.0_dp, 0.0_dp, 5.0_dp/)
    c%nel = 1
    allocate(c%nat(1), c%lmax(1), c%pos_frac(3, 1, 1))
    c%nat(1) = 1
    c%lmax(1) = lmax_val
    c%pos_frac(:, 1, 1) = (/0.0_dp, 0.0_dp, 0.0_dp/)
    c%pgnr = pgnr_val
  end subroutine setup_cubic_1atom

  subroutine setup_hexagonal_1atom(c, lmax_val, pgnr_val)
    type(sympw_crystal_t), intent(out) :: c
    integer, intent(in) :: lmax_val, pgnr_val
    ! Hexagonal: a=5.0, c=10.0, gamma=120 deg
    c%lattice(1,:) = (/5.0_dp, 0.0_dp, 0.0_dp/)
    c%lattice(2,:) = (/-2.5_dp, 4.330127018922193_dp, 0.0_dp/)
    c%lattice(3,:) = (/0.0_dp, 0.0_dp, 10.0_dp/)
    c%nel = 1
    allocate(c%nat(1), c%lmax(1), c%pos_frac(3, 1, 1))
    c%nat(1) = 1
    c%lmax(1) = lmax_val
    c%pos_frac(:, 1, 1) = (/0.0_dp, 0.0_dp, 0.0_dp/)
    c%pgnr = pgnr_val
  end subroutine setup_hexagonal_1atom

  subroutine setup_orthorhombic_1atom(c, lmax_val, pgnr_val)
    type(sympw_crystal_t), intent(out) :: c
    integer, intent(in) :: lmax_val, pgnr_val
    ! Orthorhombic: a=4, b=5, c=6
    c%lattice(1,:) = (/4.0_dp, 0.0_dp, 0.0_dp/)
    c%lattice(2,:) = (/0.0_dp, 5.0_dp, 0.0_dp/)
    c%lattice(3,:) = (/0.0_dp, 0.0_dp, 6.0_dp/)
    c%nel = 1
    allocate(c%nat(1), c%lmax(1), c%pos_frac(3, 1, 1))
    c%nat(1) = 1
    c%lmax(1) = lmax_val
    c%pos_frac(:, 1, 1) = (/0.0_dp, 0.0_dp, 0.0_dp/)
    c%pgnr = pgnr_val
  end subroutine setup_orthorhombic_1atom

  subroutine setup_tetragonal_1atom(c, lmax_val, pgnr_val)
    type(sympw_crystal_t), intent(out) :: c
    integer, intent(in) :: lmax_val, pgnr_val
    ! Tetragonal: a=b=5, c=8
    c%lattice(1,:) = (/5.0_dp, 0.0_dp, 0.0_dp/)
    c%lattice(2,:) = (/0.0_dp, 5.0_dp, 0.0_dp/)
    c%lattice(3,:) = (/0.0_dp, 0.0_dp, 8.0_dp/)
    c%nel = 1
    allocate(c%nat(1), c%lmax(1), c%pos_frac(3, 1, 1))
    c%nat(1) = 1
    c%lmax(1) = lmax_val
    c%pos_frac(:, 1, 1) = (/0.0_dp, 0.0_dp, 0.0_dp/)
    c%pgnr = pgnr_val
  end subroutine setup_tetragonal_1atom

  subroutine setup_diamond_primitive(c, lmax_val, pgnr_val)
    type(sympw_crystal_t), intent(out) :: c
    integer, intent(in) :: lmax_val, pgnr_val
    ! Si diamond primitive cell: FCC lattice, 2 atoms
    ! Lattice from POSCAR_primitive: scaling 5.43
    c%lattice(1,:) = (/0.0_dp, 2.715_dp, 2.715_dp/)
    c%lattice(2,:) = (/2.715_dp, 0.0_dp, 2.715_dp/)
    c%lattice(3,:) = (/2.715_dp, 2.715_dp, 0.0_dp/)
    c%nel = 1
    allocate(c%nat(1), c%lmax(1), c%pos_frac(3, 1, 2))
    c%nat(1) = 2
    c%lmax(1) = lmax_val
    c%pos_frac(:, 1, 1) = (/0.0_dp, 0.0_dp, 0.0_dp/)
    c%pos_frac(:, 1, 2) = (/0.25_dp, 0.25_dp, 0.25_dp/)
    c%pgnr = pgnr_val
  end subroutine setup_diamond_primitive

  subroutine setup_diamond_conventional(c, lmax_val, pgnr_val)
    type(sympw_crystal_t), intent(out) :: c
    integer, intent(in) :: lmax_val, pgnr_val
    ! Si diamond conventional FCC cell: cubic lattice, 8 atoms
    c%lattice(1,:) = (/5.43_dp, 0.0_dp, 0.0_dp/)
    c%lattice(2,:) = (/0.0_dp, 5.43_dp, 0.0_dp/)
    c%lattice(3,:) = (/0.0_dp, 0.0_dp, 5.43_dp/)
    c%nel = 1
    allocate(c%nat(1), c%lmax(1), c%pos_frac(3, 1, 8))
    c%nat(1) = 8
    c%lmax(1) = lmax_val
    c%pos_frac(:, 1, 1) = (/0.0_dp, 0.0_dp, 0.0_dp/)
    c%pos_frac(:, 1, 2) = (/0.25_dp, 0.25_dp, 0.25_dp/)
    c%pos_frac(:, 1, 3) = (/0.5_dp, 0.5_dp, 0.0_dp/)
    c%pos_frac(:, 1, 4) = (/0.5_dp, 0.0_dp, 0.5_dp/)
    c%pos_frac(:, 1, 5) = (/0.0_dp, 0.5_dp, 0.5_dp/)
    c%pos_frac(:, 1, 6) = (/0.75_dp, 0.75_dp, 0.25_dp/)
    c%pos_frac(:, 1, 7) = (/0.75_dp, 0.25_dp, 0.75_dp/)
    c%pos_frac(:, 1, 8) = (/0.25_dp, 0.75_dp, 0.75_dp/)
    c%pgnr = pgnr_val
  end subroutine setup_diamond_conventional

  subroutine teardown_crystal(c)
    type(sympw_crystal_t), intent(inout) :: c
    deallocate(c%nat, c%lmax, c%pos_frac)
  end subroutine teardown_crystal

  ! ----- Verification -----

  subroutine check_cell_info(expected_reduced, expected_nat, expected_transform, sp, tot)
    logical, intent(in) :: expected_reduced
    integer, intent(in) :: expected_nat
    real(dp), intent(in) :: expected_transform(3,3)
    integer, intent(inout) :: sp, tot

    type(sympw_cell_info_t) :: info
    integer :: error_code, reported_nat
    logical :: nat_ok
    real(dp) :: transform_err

    call sympw_get_cell_info(info, error_code)

    tot = tot + 1
    write(*,'(A,A)') "  Cell reduction metadata:", &
         merge(" PASS", " FAIL", error_code == 0 .and. (info%reduced .eqv. expected_reduced))
    if (error_code == 0 .and. (info%reduced .eqv. expected_reduced)) sp = sp + 1

    reported_nat = -1
    nat_ok = .false.
    if (error_code == 0 .and. allocated(info%nat)) then
       if (size(info%nat) >= 1) then
          reported_nat = info%nat(1)
          nat_ok = (reported_nat == expected_nat)
       end if
    end if
    tot = tot + 1
    write(*,'(A,I3,A,I3,A,A)') "  Reduced atom count:", reported_nat, &
         " (expected", expected_nat, ")", merge(" PASS", " FAIL", nat_ok)
    if (nat_ok) sp = sp + 1

    if (error_code == 0) then
       transform_err = maxval(abs(info%k_transform(:, :) - expected_transform(:, :)))
    else
       transform_err = huge(1.0_dp)
    end if
    tot = tot + 1
    write(*,'(A,E12.4,A)') "  K transform metadata error =", transform_err, &
         merge(" PASS", " FAIL", transform_err < tol_equal)
    if (transform_err < tol_equal) sp = sp + 1
  end subroutine check_cell_info

  subroutine check_result(res, expected_order, sp, tot, max_err, tr, lmax_list, nat_list)
    type(sympw_result_t), intent(in) :: res
    integer, intent(in) :: expected_order
    integer, intent(inout) :: sp, tot
    real(dp), intent(out) :: max_err, tr
    integer, intent(in) :: lmax_list(:)
    integer, intent(in) :: nat_list(:)
    integer :: n, i, sum_dim
    real(dp) :: im_max
    complex(dp), allocatable :: P_real(:, :)

    if (.not. res%success) then
       write(*,*) "  FAIL: analyze_kpoint failed"
       max_err = -1.0_dp
       tr = -1.0_dp
       return
    end if

    n = res%matrix_order
    tot = tot + 1
    write(*,'(A,I3,A,I3,A,A)') "  Matrix order:", n, " (expected", expected_order, ")", &
         merge(" PASS", " FAIL", n == expected_order)
    if (n == expected_order) sp = sp + 1

    call verify_projector(res, max_err, tr)

    tot = tot + 1
    write(*,'(A,E12.4,A)') "  Idempotency |P^2-P| =", max_err, &
         merge(" PASS", " FAIL", max_err < 100.0_dp * tol_projection)
    if (max_err < 100.0_dp * tol_projection) sp = sp + 1

    call check_hermiticity(res, max_err)
    tot = tot + 1
    write(*,'(A,E12.4,A)') "  Hermiticity |P-P†| =", max_err, &
         merge(" PASS", " FAIL", max_err < tol_projection)
    if (max_err < tol_projection) sp = sp + 1

    tot = tot + 1
    write(*,'(A,F12.6)') "  Trace(P) =", tr
    if (abs(tr - nint(tr)) < 0.1_dp) then
       write(*,'(A,I3,A)') "  Trace is close to integer:", nint(tr), " PASS"
       sp = sp + 1
    else
       write(*,*) "  FAIL: trace not integer"
    end if

    ! --- Real SH transform check ---
    allocate(P_real(n, n))
    call complex_to_real_projector(res%projector, lmax_list, nat_list, P_real)

    ! Check that P_real is indeed real
    im_max = maxval(abs(aimag(P_real)))
    tot = tot + 1
    write(*,'(A,E12.4,A)') "  Real SH: max |Im(P)| =", im_max, &
         merge(" PASS", " FAIL", im_max < 100.0_dp * tol_projection)
    if (im_max < 100.0_dp * tol_projection) sp = sp + 1

    ! Check that P_real is still a projector.
    ! Tolerance relaxed vs tol_projection: the two extra matmuls in
    ! the unitary CSH->RSH transform accumulate floating-point error,
    ! especially for high-symmetry Gamma-point projectors that are
    ! themselves close to the tol_projection limit.
    call verify_real_projector(P_real, max_err, tr)
    tot = tot + 1
    write(*,'(A,E12.4,A)') "  Real SH: |P_real^2-P_real| =", max_err, &
         merge(" PASS", " FAIL", max_err < 100.0_dp * tol_projection)
    if (max_err < 100.0_dp * tol_projection) sp = sp + 1

    ! --- Block structure check ---
    if (res%n_blocks > 0) then
       sum_dim = 0
       do i = 1, res%n_blocks
          sum_dim = sum_dim + res%blocks(i)%dim
       end do
       tot = tot + 1
       write(*,'(A,I3,A,I3,A)') "  Blocks: ", res%n_blocks, " blocks, total dim =", sum_dim, &
            merge(" PASS", " FAIL", sum_dim == n)
       if (sum_dim == n) sp = sp + 1
    else
       tot = tot + 1
       write(*,*) "  Blocks: no blocks found FAIL"
    end if

    deallocate(P_real)
  end subroutine check_result

  subroutine verify_real_projector(P, max_err, tr)
    complex(dp), intent(in) :: P(:, :)
    real(dp), intent(out) :: max_err, tr
    integer :: n, i, j
    complex(dp), allocatable :: P2(:, :), diff(:, :)

    n = size(P, 1)
    allocate(P2(n, n), diff(n, n))
    P2 = matmul(P, P)
    diff = P2 - P

    max_err = 0.0_dp
    tr = 0.0_dp
    do j = 1, n
       tr = tr + real(P(j, j))
       do i = 1, n
          max_err = max(max_err, abs(diff(i, j)))
       end do
    end do
    deallocate(P2, diff)
  end subroutine verify_real_projector

  subroutine verify_projector(res, max_err, tr)
    type(sympw_result_t), intent(in) :: res
    real(dp), intent(out) :: max_err, tr
    integer :: n, i, j
    complex(dp), allocatable :: p2(:,:), diff_mat(:,:)

    n = res%matrix_order
    allocate(p2(n, n), diff_mat(n, n))
    p2 = matmul(res%projector, res%projector)
    diff_mat = p2 - res%projector

    max_err = 0.0_dp
    tr = 0.0_dp
    do j = 1, n
       tr = tr + real(res%projector(j, j))
       do i = 1, n
          max_err = max(max_err, abs(diff_mat(i, j)))
       end do
    end do
    deallocate(p2, diff_mat)
  end subroutine verify_projector

  subroutine check_hermiticity(res, max_err)
    type(sympw_result_t), intent(in) :: res
    real(dp), intent(out) :: max_err
    integer :: n, i, j
    complex(dp), allocatable :: diff_mat(:,:)

    n = res%matrix_order
    allocate(diff_mat(n, n))
    diff_mat = res%projector - transpose(conjg(res%projector))

    max_err = 0.0_dp
    do j = 1, n
       do i = 1, n
          max_err = max(max_err, abs(diff_mat(i, j)))
       end do
    end do
    deallocate(diff_mat)
  end subroutine check_hermiticity

end program test_lib
