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
!   Scenario 10: Oh(36), X-point,      2 atoms, lmax=2  (Si diamond nonsymmorphic irrep filter)
!   Scenario 11: Oh(36), X-point,      8 atoms, lmax=2  (interior-k projective irreps)
!   Scenario 12: invalid crystal metadata returns explicit error codes
!   Scenario 13: noncommensurate projective phase fails without aborting
!   Scenario 14: hcp P6_3/mmc A-point uses a finite screw-axis extension
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
  integer, parameter :: kpoint_info_checks = 8
  integer, parameter :: kpoint_basis_checks = 2
  integer, parameter :: invalid_input_checks = 4
  integer, parameter :: scenario9_checks = checks_per_scenario + cell_info_checks + kpoint_info_checks
  integer, parameter :: scenario10_checks = checks_per_scenario + kpoint_info_checks
  integer, parameter :: scenario11_checks = checks_per_scenario + cell_info_checks + &
       kpoint_info_checks + kpoint_basis_checks
  integer, parameter :: scenario12_checks = invalid_input_checks
  integer, parameter :: scenario13_checks = 1
  integer, parameter :: scenario14_checks = checks_per_scenario + kpoint_info_checks + 2
  integer, parameter :: expected_scenarios = 14
  integer :: error_code, passed, total, scenario_passed, scenarios_ok, expected_order
  real(dp) :: expected_transform(3,3), expected_kpoint(3)
  logical :: tr_ok

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
     call check_kpoint_info(result, .true., 12, 12, 6, 6, 8, 8, scenario_passed, total)
     call check_result(result, expected_order, scenario_passed, total, max_err, trace_val, crystal%lmax, (/2/))
     call sympw_finalize()
  end if
  call teardown_crystal(crystal)
  passed = passed + scenario_passed
  if (scenario_passed == scenario9_checks) scenarios_ok = scenarios_ok + 1
  write(*,'(A,I2,A)') "  Scenario 9: ", scenario_passed, "/18 checks passed"

  ! ==========================================
  ! Scenario 10: Oh (36), X-point, Si diamond primitive cell
  !              Checks the nonsymmorphic factor-group branch where
  !              lifted elements enlarge G_k/T_k and allow() filters
  !              out irreps incompatible with the Bloch phase.
  ! ==========================================
  write(*,*)
  write(*,*) "--- Scenario 10: Oh(36), X-point, Si diamond primitive cell ---"
  scenario_passed = 0
  expected_order = 18

  call setup_diamond_primitive(crystal, 2, 36)
  call sympw_init(crystal, error_code)
  if (error_code /= 0) then
     write(*,*) "  FAIL: init error_code =", error_code
  else
     kpoint(:) = (/0.5_dp, 0.0_dp, 0.5_dp/)
     call sympw_analyze_kpoint(kpoint, result)
     call check_kpoint_info(result, .true., 16, 32, 14, 4, 20, 8, scenario_passed, total)
     call check_result(result, expected_order, scenario_passed, total, max_err, trace_val, crystal%lmax, crystal%nat)
     call sympw_finalize()
  end if
  call teardown_crystal(crystal)
  passed = passed + scenario_passed
  if (scenario_passed == scenario10_checks) scenarios_ok = scenarios_ok + 1
  write(*,'(A,I2,A)') "  Scenario 10: ", scenario_passed, "/15 checks passed"

  ! ==========================================
  ! Scenario 11: Oh (36), X-point, Si diamond conventional cell
  !              Checks that API results preserve caller-basis k,
  !              report the primitive-basis k used internally, and
  !              construct the finite projective factor group at interior k.
  ! ==========================================
  write(*,*)
  write(*,*) "--- Scenario 11: Oh(36), X-point, Si diamond conventional cell ---"
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
     kpoint(:) = (/0.5_dp, 0.0_dp, 0.5_dp/)
     expected_kpoint(:) = (/0.25_dp, 0.5_dp, 0.25_dp/)
     call sympw_analyze_kpoint(kpoint, result)
     call check_kpoint_basis(result, kpoint, expected_kpoint, scenario_passed, total)
     call check_kpoint_info(result, .true., 4, 8, 8, 4, 8, 4, scenario_passed, total)
     call check_result(result, expected_order, scenario_passed, total, max_err, trace_val, crystal%lmax, (/2/))
     call sympw_finalize()
  end if
  call teardown_crystal(crystal)
  passed = passed + scenario_passed
  if (scenario_passed == scenario11_checks) scenarios_ok = scenarios_ok + 1
  write(*,'(A,I2,A,I2,A)') "  Scenario 11: ", scenario_passed, "/", scenario11_checks, " checks passed"

  ! ==========================================
  ! Scenario 12: invalid crystal metadata
  ! ==========================================
  write(*,*)
  write(*,*) "--- Scenario 12: invalid crystal metadata ---"
  scenario_passed = 0

  call setup_minimal_crystal_header(crystal)
  call sympw_init(crystal, error_code)
  call check_error_code("missing nat", error_code, 3, scenario_passed, total)
  call teardown_crystal(crystal)

  call setup_minimal_crystal_header(crystal)
  allocate(crystal%nat(1), crystal%lmax(1), crystal%pos_frac(3, 1, 1))
  crystal%nat(1) = 0
  crystal%lmax(1) = 0
  crystal%pos_frac(:, :, :) = 0.0_dp
  call sympw_init(crystal, error_code)
  call check_error_code("zero atom count", error_code, 7, scenario_passed, total)
  call teardown_crystal(crystal)

  call setup_minimal_crystal_header(crystal)
  allocate(crystal%nat(1), crystal%lmax(1), crystal%pos_frac(2, 1, 1))
  crystal%nat(1) = 1
  crystal%lmax(1) = 0
  crystal%pos_frac(:, :, :) = 0.0_dp
  call sympw_init(crystal, error_code)
  call check_error_code("bad position shape", error_code, 10, scenario_passed, total)
  call teardown_crystal(crystal)

  call setup_minimal_crystal_header(crystal)
  allocate(crystal%nat(1), crystal%lmax(1), crystal%pos_frac(3, 1, 1))
  crystal%lattice(2, :) = crystal%lattice(1, :)
  crystal%nat(1) = 1
  crystal%lmax(1) = 0
  crystal%pos_frac(:, :, :) = 0.0_dp
  call sympw_init(crystal, error_code)
  call check_error_code("singular lattice", error_code, 13, scenario_passed, total)
  call teardown_crystal(crystal)

  passed = passed + scenario_passed
  if (scenario_passed == scenario12_checks) scenarios_ok = scenarios_ok + 1
  write(*,'(A,I2,A)') "  Scenario 12: ", scenario_passed, "/4 checks passed"

  ! ==========================================
  ! Scenario 13: nonsymmorphic symmetry line with a phase that does not
  !              close within the supported finite central extension.
  ! ==========================================
  write(*,*)
  write(*,*) "--- Scenario 13: noncommensurate interior-k projective phase ---"
  scenario_passed = 0

  call setup_diamond_conventional(crystal, 0, 36)
  call sympw_init(crystal, error_code)
  if (error_code /= 0) then
     write(*,*) "  FAIL: init error_code =", error_code
  else
     kpoint(:) = (/0.271828_dp, 0.0_dp, 0.271828_dp/)
     call sympw_analyze_kpoint(kpoint, result)
     call check_analysis_failure(result, scenario_passed, total)
     call sympw_finalize()
  end if
  call teardown_crystal(crystal)
  passed = passed + scenario_passed
  if (scenario_passed == scenario13_checks) scenarios_ok = scenarios_ok + 1
  write(*,'(A,I2,A,I2,A)') "  Scenario 13: ", scenario_passed, "/", scenario13_checks, " checks passed"

  ! ==========================================
  ! Scenario 14: hcp P6_3/mmc at A.
  ! The 6_3 screw contributes a half-translation along c, so at
  ! k=(0,0,1/2) its Bloch phase is -i and the 24-element little
  ! co-group is represented by a 48-element finite extension.
  ! ==========================================
  write(*,*)
  write(*,*) "--- Scenario 14: hcp P6_3/mmc A-point screw extension ---"
  scenario_passed = 0
  expected_order = 2

  call setup_hcp_nonsymmorphic(crystal, 0, 31)
  call sympw_init(crystal, error_code)
  if (error_code /= 0) then
     write(*,*) "  FAIL: init error_code =", error_code
  else
     kpoint(:) = (/0.0_dp, 0.0_dp, 0.5_dp/)
     call sympw_analyze_kpoint(kpoint, result)
     call check_kpoint_info(result, .true., 24, 48, 15, 3, 24, 8, &
          scenario_passed, total)
     call check_result(result, expected_order, scenario_passed, total, max_err, &
          trace_val, crystal%lmax, crystal%nat)
     call check_projective_labels(result, scenario_passed, total)
     call sympw_check_spinless_time_reversal(kpoint, -kpoint, tol_projection, &
          tr_ok, max_err)
     total = total + 1
     write(*,'(A,E12.4,A)') "  A-point spinless TR audit residual =", max_err, &
          merge(" PASS", " FAIL", tr_ok .and. max_err < tol_projection)
     if (tr_ok .and. max_err < tol_projection) scenario_passed = scenario_passed + 1
     call sympw_finalize()
  end if
  call teardown_crystal(crystal)
  passed = passed + scenario_passed
  if (scenario_passed == scenario14_checks) scenarios_ok = scenarios_ok + 1
  write(*,'(A,I2,A,I2,A)') "  Scenario 14: ", scenario_passed, "/", &
       scenario14_checks, " checks passed"

  ! ==========================================
  ! Summary
  ! ==========================================
  write(*,*)
  write(*,*) "=========================================="
  write(*,'(A,I4,A,I4,A)') " Overall: ", passed, " / ", total, " checks passed"
  write(*,'(A,I2,A,I2,A)') " Scenarios fully passing: ", scenarios_ok, " / ", expected_scenarios
  write(*,*) "=========================================="

  if (passed == total .and. scenarios_ok == expected_scenarios) then
     write(*,*) "All checks passed!"
  else
     write(*,*) "Some checks failed."
     stop 1
  end if

contains

  ! ----- Setup helpers -----

  subroutine setup_minimal_crystal_header(c)
    type(sympw_crystal_t), intent(out) :: c
    c%lattice(:, :) = 0.0_dp
    c%lattice(1, 1) = 1.0_dp
    c%lattice(2, 2) = 1.0_dp
    c%lattice(3, 3) = 1.0_dp
    c%nel = 1
    c%pgnr = 36
  end subroutine setup_minimal_crystal_header

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

  subroutine setup_hcp_nonsymmorphic(c, lmax_val, pgnr_val)
    type(sympw_crystal_t), intent(out) :: c
    integer, intent(in) :: lmax_val, pgnr_val
    real(dp), parameter :: lattice_a = 3.2_dp

    c%lattice(1,:) = (/lattice_a, 0.0_dp, 0.0_dp/)
    c%lattice(2,:) = (/-0.5_dp*lattice_a, &
         0.5_dp*sqrt(3.0_dp)*lattice_a, 0.0_dp/)
    c%lattice(3,:) = (/0.0_dp, 0.0_dp, 5.2_dp/)
    c%nel = 1
    allocate(c%nat(1), c%lmax(1), c%pos_frac(3, 1, 2))
    c%nat(1) = 2
    c%lmax(1) = lmax_val
    c%pos_frac(:, 1, 1) = (/1.0_dp/3.0_dp, 2.0_dp/3.0_dp, 0.25_dp/)
    c%pos_frac(:, 1, 2) = (/2.0_dp/3.0_dp, 1.0_dp/3.0_dp, 0.75_dp/)
    c%pgnr = pgnr_val
  end subroutine setup_hcp_nonsymmorphic

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
    if (allocated(c%nat)) deallocate(c%nat)
    if (allocated(c%lmax)) deallocate(c%lmax)
    if (allocated(c%pos_frac)) deallocate(c%pos_frac)
  end subroutine teardown_crystal

  ! ----- Verification -----

  subroutine check_error_code(label, actual, expected, sp, tot)
    character(len=*), intent(in) :: label
    integer, intent(in) :: actual, expected
    integer, intent(inout) :: sp, tot
    logical :: ok

    ok = (actual == expected)
    tot = tot + 1
    write(*,'(A,A,A,I3,A,I3,A,A)') "  ", trim(label), " error_code:", &
         actual, " (expected", expected, ")", merge(" PASS", " FAIL", ok)
    if (ok) sp = sp + 1
  end subroutine check_error_code

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

  subroutine check_kpoint_basis(res, expected_input, expected_internal, sp, tot)
    type(sympw_result_t), intent(in) :: res
    real(dp), intent(in) :: expected_input(3), expected_internal(3)
    integer, intent(inout) :: sp, tot
    real(dp) :: k_err

    k_err = maxval(abs(res%kpoint_input(:) - expected_input(:)))
    tot = tot + 1
    write(*,'(A,E12.4,A)') "  Caller-basis k metadata error =", k_err, &
         merge(" PASS", " FAIL", k_err < tol_equal)
    if (k_err < tol_equal) sp = sp + 1

    k_err = maxval(abs(res%kpoint_internal(:) - expected_internal(:)))
    tot = tot + 1
    write(*,'(A,E12.4,A)') "  Internal-basis k metadata error =", k_err, &
         merge(" PASS", " FAIL", k_err < tol_equal)
    if (k_err < tol_equal) sp = sp + 1
  end subroutine check_kpoint_basis

  subroutine check_kpoint_info(res, expected_factor, expected_little_order, &
       expected_factor_order, expected_classes, expected_allowed_irreps, &
       expected_irrep_dim_sum, expected_allowed_dim_sum, sp, tot)
    type(sympw_result_t), intent(in) :: res
    logical, intent(in) :: expected_factor
    integer, intent(in) :: expected_little_order, expected_factor_order
    integer, intent(in) :: expected_classes, expected_allowed_irreps
    integer, intent(in) :: expected_irrep_dim_sum, expected_allowed_dim_sum
    integer, intent(inout) :: sp, tot
    logical :: ok

    tot = tot + 1
    ok = (res%factor_group_used .eqv. expected_factor)
    write(*,'(A,A)') "  Factor-group metadata:", merge(" PASS", " FAIL", ok)
    if (ok) sp = sp + 1

    tot = tot + 1
    ok = (res%little_group_order == expected_little_order)
    write(*,'(A,I3,A,I3,A,A)') "  Little-group order:", res%little_group_order, &
         " (expected", expected_little_order, ")", merge(" PASS", " FAIL", ok)
    if (ok) sp = sp + 1

    tot = tot + 1
    ok = (res%factor_group_order == expected_factor_order)
    write(*,'(A,I3,A,I3,A,A)') "  Factor-group order:", res%factor_group_order, &
         " (expected", expected_factor_order, ")", merge(" PASS", " FAIL", ok)
    if (ok) sp = sp + 1

    tot = tot + 1
    ok = (res%n_classes == expected_classes)
    write(*,'(A,I3,A,I3,A,A)') "  Conjugacy classes:", res%n_classes, &
         " (expected", expected_classes, ")", merge(" PASS", " FAIL", ok)
    if (ok) sp = sp + 1

    tot = tot + 1
    ok = (res%n_irreps == expected_classes)
    write(*,'(A,I3,A,I3,A,A)') "  Irrep count:", res%n_irreps, &
         " (expected", expected_classes, ")", merge(" PASS", " FAIL", ok)
    if (ok) sp = sp + 1

    tot = tot + 1
    ok = (res%n_allowed_irreps == expected_allowed_irreps)
    write(*,'(A,I3,A,I3,A,A)') "  Allowed irrep count:", res%n_allowed_irreps, &
         " (expected", expected_allowed_irreps, ")", merge(" PASS", " FAIL", ok)
    if (ok) sp = sp + 1

    tot = tot + 1
    ok = (res%irrep_dimension_sum == expected_irrep_dim_sum)
    write(*,'(A,I3,A,I3,A,A)') "  Irrep dimension sum:", res%irrep_dimension_sum, &
         " (expected", expected_irrep_dim_sum, ")", merge(" PASS", " FAIL", ok)
    if (ok) sp = sp + 1

    tot = tot + 1
    ok = (res%allowed_irrep_dimension_sum == expected_allowed_dim_sum)
    write(*,'(A,I3,A,I3,A,A)') "  Allowed irrep dimension sum:", &
         res%allowed_irrep_dimension_sum, " (expected", expected_allowed_dim_sum, ")", &
         merge(" PASS", " FAIL", ok)
    if (ok) sp = sp + 1
  end subroutine check_kpoint_info

  subroutine check_analysis_failure(res, sp, tot)
    type(sympw_result_t), intent(in) :: res
    integer, intent(inout) :: sp, tot

    tot = tot + 1
    write(*,'(A,A)') "  Unsupported projective-irrep case rejected:", &
         merge(" PASS", " FAIL", .not. res%success)
    if (.not. res%success) sp = sp + 1
  end subroutine check_analysis_failure

  subroutine check_projective_labels(res, sp, tot)
    type(sympw_result_t), intent(in) :: res
    integer, intent(inout) :: sp, tot
    integer :: irrep_position
    logical :: ok

    ok = res%success .and. res%factor_group_used .and. allocated(res%irreps) .and. &
         res%mulliken_status == SYMPW_MULLIKEN_STATUS_PROJECTIVE
    if (ok) then
       do irrep_position = 1, size(res%irreps)
          if (len_trim(res%irreps(irrep_position)%label) == 0 .or. &
               len_trim(res%irreps(irrep_position)%mulliken_label) /= 0) then
             ok = .false.
             exit
          end if
       end do
    end if
    tot = tot + 1
    write(*,'(A,A)') "  Projective irreps use fingerprint-only labels:", &
         merge(" PASS", " FAIL", ok)
    if (ok) sp = sp + 1
  end subroutine check_projective_labels

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
         merge(" PASS", " FAIL", max_err < tol_projection_work)
    if (max_err < tol_projection_work) sp = sp + 1

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
         merge(" PASS", " FAIL", im_max < tol_projection_work)
    if (im_max < tol_projection_work) sp = sp + 1

    ! Check that P_real is still a projector.
    ! Tolerance relaxed vs tol_projection: the two extra matmuls in
    ! the unitary CSH->RSH transform accumulate floating-point error,
    ! especially for high-symmetry Gamma-point projectors that are
    ! themselves close to the tol_projection limit.
    call verify_real_projector(P_real, max_err, tr)
    tot = tot + 1
    write(*,'(A,E12.4,A)') "  Real SH: |P_real^2-P_real| =", max_err, &
         merge(" PASS", " FAIL", max_err < tol_projection_work)
    if (max_err < tol_projection_work) sp = sp + 1

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
