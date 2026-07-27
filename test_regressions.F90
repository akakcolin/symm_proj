program test_regressions
  use accuracy
  use constants, only: pi, npgodat, nge2, primen
  use charac, only: lcm, modulus
  use sympw_lib
  use vasp_reader
  use eigvec, only: sym_eigvec
  use intsec, only: sym_intsec
  use projmat, only: validate_projector_matrix
  use sumsets, only: sym_sumsets
  use modsymprj, only: symprj
  use sympw_config, only: read_sympw_config
  use sympw_vasp_input, only: load_vasp_crystal_input
  use bztest, only: sym_bztest
  use time_reversal, only: find_trim_points, build_time_reversal_pairs
  use time_reversal_optimization, only: verify_spinless_projector_pair, &
       build_spinless_partner_projector
  use sympw_real_sh, only: complex_to_real_projector
  use sympw_group_mode, only: projective_factor_group_active
  use sympw_mulliken, only: assign_mulliken_label
  use sympw_phase, only: bloch_phase
  use sympw_pointgroup_data, only: pg_data_t, init_point_group_data, &
       deallocate_point_group_data, detect_structure_point_group
  implicit none

  type(sympw_crystal_t) :: crystal
  type(sympw_result_t) :: lifecycle_result
  type(sympw_cell_info_t) :: cell_info
  integer :: error_code
  integer :: passed, total
  character(len=256) :: poscar_path
  character(len=256) :: comment
  real(dp) :: scale_factors(3), lattice(3,3)
  character(len=2), allocatable :: elements(:)
  integer, allocatable :: nat_per_elem(:)
  real(dp), allocatable :: positions(:,:)
  logical :: is_cartesian
  integer :: nel, total_atoms

  passed = 0
  total = 0

  call test_lmax_limit(crystal, passed, total)
  call test_invalid_point_group(crystal, passed, total)
  call test_auto_point_group_initialization(crystal, passed, total)
  call test_inconsistent_point_group_initialization(crystal, passed, total)
  call test_invalid_point_group_name(passed, total)
  call test_oversized_metadata(crystal, cell_info, passed, total)
  call test_negative_poscar_scale(passed, total)
  call test_cartesian_poscar_scale(passed, total)
  call test_anisotropic_poscar_scale(passed, total)
  call test_invalid_poscar_scale_count(passed, total)
  call test_malformed_poscar_reports_error(passed, total)
  call test_malformed_kpoints_reports_error(passed, total)
  call test_cartesian_kpoints_conversion(passed, total)
  call test_shared_config_parser(passed, total)
  call test_complex_c3_eigenvector(passed, total)
  call test_complex_c3_irrep_labels(passed, total)
  call test_supported_mulliken_labels(passed, total)
  call test_mulliken_input_validation(passed, total)
  call test_complex_subspace_intersection(passed, total)
  call test_graphene_point_group(passed, total)
  call test_time_reversal_projector_relation(passed, total)
  call test_real_basis_time_reversal_relation(passed, total)
  call test_library_basis_and_tr_audit(passed, total)
  call test_f_orbital_support(passed, total)
  call test_projective_factor_group_mode(passed, total)
  call test_summation_set_modes(passed, total)
  call test_summation_set_failure_status(passed, total)
  call test_legacy_symprj_nonorthogonal_mapping(passed, total)
  call test_bloch_phase_convention(passed, total)
  call test_spinless_partner_projector_builder(passed, total)
  call test_time_reversal_pairing(passed, total)
  call test_character_arithmetic_helpers(passed, total)
  call test_block_extraction_input_validation(passed, total)
  call test_projector_validation_status(passed, total)
  call test_all_point_groups_at_gamma(passed, total)
  call test_library_lifecycle(crystal, lifecycle_result, passed, total)
  write(*,'(A,I0,A,I0)') "Regression checks: ", passed, " / ", total
  if (passed /= total) error stop "Regression tests failed"

contains

  subroutine test_lmax_limit(c, sp, tot)
    type(sympw_crystal_t), intent(inout) :: c
    integer, intent(inout) :: sp, tot

    call setup_single_atom(c, 4)
    call sympw_init(c, error_code)
    call check("lmax above supported range is rejected", error_code == 14, sp, tot)
    call sympw_finalize()
    call teardown(c)
  end subroutine test_lmax_limit


  subroutine test_invalid_point_group(c, sp, tot)
    type(sympw_crystal_t), intent(inout) :: c
    integer, intent(inout) :: sp, tot

    call setup_single_atom(c, 0)
    c%pgnr = 37
    call sympw_init(c, error_code)
    call check("invalid point group is rejected", error_code == 1, sp, tot)
    call sympw_finalize()
    call teardown(c)
  end subroutine test_invalid_point_group


  subroutine test_auto_point_group_initialization(c, sp, tot)
    type(sympw_crystal_t), intent(inout) :: c
    integer, intent(inout) :: sp, tot

    type(sympw_cell_info_t) :: detected_info
    integer :: info_error

    call setup_single_atom(c, 0)
    c%pgnr = 0
    call sympw_init(c, error_code)
    call check("automatic point-group initialization succeeds", error_code == 0, sp, tot)
    if (error_code == 0) then
       call sympw_get_cell_info(detected_info, info_error)
       call check("automatic point-group initialization detects Oh", &
            info_error == 0 .and. detected_info%point_group_number == 36, sp, tot)
       call check("canonical cell metadata exposes the orbital basis", &
            detected_info%basis_dimension == 1 .and. allocated(detected_info%lmax) .and. &
            all(detected_info%lmax == [0]), sp, tot)
    else
       call check("automatic point-group initialization detects Oh", .false., sp, tot)
       call check("canonical cell metadata exposes the orbital basis", .false., sp, tot)
    end if
    call sympw_finalize()
    call teardown(c)
  end subroutine test_auto_point_group_initialization

  subroutine test_inconsistent_point_group_initialization(c, sp, tot)
    type(sympw_crystal_t), intent(inout) :: c
    integer, intent(inout) :: sp, tot

    c%lattice = 0.0_dp
    c%lattice(1,1) = 2.0_dp
    c%lattice(2,2) = 2.0_dp
    c%lattice(3,3) = 2.0_dp
    c%nel = 2
    c%pgnr = 2
    allocate(c%nat(2), c%lmax(2), c%pos_frac(3,2,1))
    c%nat = [1, 1]
    c%lmax = [0, 0]
    c%pos_frac = 0.0_dp
    c%pos_frac(:,2,1) = [0.2_dp, 0.0_dp, 0.0_dp]

    call sympw_init(c, error_code)
    call check("inconsistent point group returns an initialization error", &
         error_code == 16, sp, tot)
    call sympw_finalize()
    call teardown(c)
  end subroutine test_inconsistent_point_group_initialization

  subroutine test_invalid_point_group_name(sp, tot)
    integer, intent(inout) :: sp, tot
    call check("invalid numeric point-group name falls back to C1", &
         point_group_name_to_number("0") == 1, sp, tot)
  end subroutine test_invalid_point_group_name

  subroutine test_oversized_metadata(c, info, sp, tot)
    type(sympw_crystal_t), intent(inout) :: c
    type(sympw_cell_info_t), intent(out) :: info
    integer, intent(inout) :: sp, tot

    c%lattice(:, :) = 0.0_dp
    c%lattice(1,1) = 5.0_dp
    c%lattice(2,2) = 5.0_dp
    c%lattice(3,3) = 5.0_dp
    c%nel = 1
    c%pgnr = 36
    allocate(c%nat(2), c%lmax(2), c%pos_frac(3,1,1))
    c%nat = [1, 99]
    c%lmax = [0, 99]
    c%pos_frac(:,1,1) = 0.0_dp

    call sympw_init(c, error_code)
    call check("oversized metadata is sliced to nel", error_code == 0, sp, tot)
    if (error_code == 0) then
       call sympw_get_cell_info(info, error_code)
       call check("oversized metadata keeps physical atom count", info%nat(1) == 1, sp, tot)
       call sympw_finalize()
    end if
    call teardown(c)
  end subroutine test_oversized_metadata

  subroutine test_negative_poscar_scale(sp, tot)
    integer, intent(inout) :: sp, tot

    poscar_path = "/tmp/symm_proj_regression_negative.POSCAR"
    call write_text_file(poscar_path, &
         "negative scale"//new_line('a')// &
         "-8.0"//new_line('a')// &
         "1 0 0"//new_line('a')// &
         "0 1 0"//new_line('a')// &
         "0 0 1"//new_line('a')// &
         "Si"//new_line('a')// &
         "1"//new_line('a')// &
         "Direct"//new_line('a')// &
         "0 0 0"//new_line('a'))

    call read_poscar(poscar_path, comment, scale_factors, lattice, elements, nat_per_elem, &
         positions, is_cartesian, nel, total_atoms)
    call check("negative POSCAR scale produces positive volume", &
         abs(abs(determinant3_local(lattice)) - 8.0_dp) < 1.0e-10_dp, sp, tot)
    call cleanup_poscar_inputs()
  end subroutine test_negative_poscar_scale

  subroutine test_cartesian_poscar_scale(sp, tot)
    integer, intent(inout) :: sp, tot

    poscar_path = "/tmp/symm_proj_regression_cartesian.POSCAR"
    call write_text_file(poscar_path, &
         "cartesian scale"//new_line('a')// &
         "2.0"//new_line('a')// &
         "1 0 0"//new_line('a')// &
         "0 1 0"//new_line('a')// &
         "0 0 1"//new_line('a')// &
         "Si"//new_line('a')// &
         "1"//new_line('a')// &
         "Cartesian"//new_line('a')// &
         "1 0 0"//new_line('a'))

    call read_poscar(poscar_path, comment, scale_factors, lattice, elements, nat_per_elem, &
         positions, is_cartesian, nel, total_atoms)
    call check("Cartesian POSCAR coordinates honor scale", &
         maxval(abs(positions(1,:) - [2.0_dp, 0.0_dp, 0.0_dp])) < 1.0e-10_dp, sp, tot)
    call cleanup_poscar_inputs()
  end subroutine test_cartesian_poscar_scale


  subroutine test_anisotropic_poscar_scale(sp, tot)
    integer, intent(inout) :: sp, tot

    poscar_path = "/tmp/symm_proj_regression_anisotropic.POSCAR"
    call write_text_file(poscar_path, &
         "anisotropic scale"//new_line('a')// &
         "2.0 3.0 4.0"//new_line('a')// &
         "1 0 0"//new_line('a')// &
         "0 1 0"//new_line('a')// &
         "0 0 1"//new_line('a')// &
         "Si"//new_line('a')// &
         "1"//new_line('a')// &
         "Cartesian"//new_line('a')// &
         "1 1 1"//new_line('a'))

    call read_poscar(poscar_path, comment, scale_factors, lattice, elements, nat_per_elem, &
         positions, is_cartesian, nel, total_atoms)
    call check("three-component POSCAR scale acts on Cartesian lattice components", &
         maxval(abs(lattice - reshape([2.0_dp, 0.0_dp, 0.0_dp, &
                                      0.0_dp, 3.0_dp, 0.0_dp, &
                                      0.0_dp, 0.0_dp, 4.0_dp], [3,3]))) < 1.0e-10_dp, sp, tot)
    call check("three-component POSCAR scale acts on Cartesian atom components", &
         maxval(abs(positions(1,:) - [2.0_dp, 3.0_dp, 4.0_dp])) < 1.0e-10_dp, sp, tot)
    call cleanup_poscar_inputs()
  end subroutine test_anisotropic_poscar_scale


  subroutine test_invalid_poscar_scale_count(sp, tot)
    integer, intent(inout) :: sp, tot
    integer :: parse_error

    poscar_path = "/tmp/symm_proj_regression_two_scales.POSCAR"
    call write_text_file(poscar_path, &
         "invalid scale count"//new_line('a')// &
         "2.0 3.0"//new_line('a')// &
         "1 0 0"//new_line('a')// &
         "0 1 0"//new_line('a')// &
         "0 0 1"//new_line('a')// &
         "Si"//new_line('a')// &
         "1"//new_line('a')// &
         "Direct"//new_line('a')// &
         "0 0 0"//new_line('a'))

    call read_poscar(poscar_path, comment, scale_factors, lattice, elements, nat_per_elem, &
         positions, is_cartesian, nel, total_atoms, parse_error)
    call check("POSCAR rejects a two-value scale line", parse_error /= 0, sp, tot)
    call cleanup_poscar_inputs()
  end subroutine test_invalid_poscar_scale_count

  subroutine test_malformed_poscar_reports_error(sp, tot)
    integer, intent(inout) :: sp, tot
    integer :: parse_error

    poscar_path = "/tmp/symm_proj_regression_malformed.POSCAR"
    call write_text_file(poscar_path, "incomplete POSCAR"//new_line('a'))

    call read_poscar(poscar_path, comment, scale_factors, lattice, elements, nat_per_elem, &
         positions, is_cartesian, nel, total_atoms, parse_error)
    call check("malformed POSCAR reports a parse error", parse_error /= 0, sp, tot)
    call check("malformed POSCAR leaves no element allocation", .not. allocated(elements), sp, tot)
    call check("malformed POSCAR leaves no position allocation", .not. allocated(positions), sp, tot)
    call cleanup_poscar_inputs()
  end subroutine test_malformed_poscar_reports_error

  subroutine test_malformed_kpoints_reports_error(sp, tot)
    integer, intent(inout) :: sp, tot
    character(len=256) :: kpoints_path
    real(dp), allocatable :: malformed_kpoints(:,:)
    character(len=20), allocatable :: malformed_names(:)
    integer :: malformed_nkpts, parse_error
    character(len=20) :: malformed_mode

    kpoints_path = "/tmp/symm_proj_regression_malformed.KPOINTS"
    call write_text_file(kpoints_path, "invalid KPOINTS"//new_line('a')// &
         "not-a-number"//new_line('a'))

    call read_kpoints(kpoints_path, malformed_kpoints, malformed_names, malformed_nkpts, &
         malformed_mode, error_code=parse_error)
    call check("malformed KPOINTS reports a parse error", parse_error /= 0, sp, tot)
    call check("malformed KPOINTS leaves no point allocation", &
         .not. allocated(malformed_kpoints), sp, tot)
    call check("malformed KPOINTS leaves no name allocation", &
         .not. allocated(malformed_names), sp, tot)
    if (allocated(malformed_kpoints)) deallocate(malformed_kpoints)
    if (allocated(malformed_names)) deallocate(malformed_names)
  end subroutine test_malformed_kpoints_reports_error


  subroutine test_cartesian_kpoints_conversion(sp, tot)
    integer, intent(inout) :: sp, tot

    type(sympw_crystal_t) :: loaded_crystal
    real(dp), allocatable :: loaded_kpoints(:,:)
    character(len=20), allocatable :: loaded_names(:)
    integer, allocatable :: input_lmax(:)
    character(len=256) :: loaded_comment, kpoints_path
    character(len=20) :: implicit_mode
    integer :: input_error, implicit_count
    logical :: implicit_cartesian

    poscar_path = "/tmp/symm_proj_regression_cartesian_k.POSCAR"
    kpoints_path = "/tmp/symm_proj_regression_cartesian.KPOINTS"
    call write_text_file(poscar_path, &
         "cartesian k-point scale"//new_line('a')// &
         "2.0 3.0 4.0"//new_line('a')// &
         "1 0 0"//new_line('a')// &
         "0 2 0"//new_line('a')// &
         "0 0 3"//new_line('a')// &
         "Si"//new_line('a')// &
         "1"//new_line('a')// &
         "Direct"//new_line('a')// &
         "0 0 0"//new_line('a'))
    call write_text_file(kpoints_path, &
         "standard Cartesian explicit point"//new_line('a')// &
         "1"//new_line('a')// &
         "Cartesian"//new_line('a')// &
         "0.25 0.25 0.0 ! test"//new_line('a'))
    allocate(input_lmax(1))
    input_lmax(1) = 0

    call load_vasp_crystal_input(poscar_path, kpoints_path, input_lmax, "", &
         loaded_crystal, loaded_kpoints, loaded_names, loaded_comment, input_error)
    call check("standard Cartesian KPOINTS input is accepted", input_error == 0, sp, tot)
    if (input_error == 0) then
       call check("Cartesian KPOINTS honors the effective POSCAR scales", &
            maxval(abs(loaded_kpoints(1,:) - [0.25_dp, 0.50_dp, 0.0_dp])) < 1.0e-10_dp, &
            sp, tot)
    else
       call check("Cartesian KPOINTS honors the effective POSCAR scales", .false., sp, tot)
    end if

    if (allocated(loaded_kpoints)) deallocate(loaded_kpoints)
    if (allocated(loaded_names)) deallocate(loaded_names)
    call write_text_file(kpoints_path, &
         "implicit reciprocal explicit points"//new_line('a')// &
         "2"//new_line('a')// &
         "0.0 0.0 0.0 ! Gamma"//new_line('a')// &
         "0.5 0.0 0.0 ! X"//new_line('a'))
    call read_kpoints(kpoints_path, loaded_kpoints, loaded_names, implicit_count, &
         implicit_mode, implicit_cartesian, input_error)
    call check("KPOINTS may omit the coordinate mode", input_error == 0, sp, tot)
    if (input_error == 0) then
       call check("implicit Reciprocal KPOINTS preserves the first point", &
            implicit_count == 2 .and. .not. implicit_cartesian .and. &
            maxval(abs(loaded_kpoints(1,:) - [0.0_dp, 0.0_dp, 0.0_dp])) < 1.0e-10_dp .and. &
            maxval(abs(loaded_kpoints(2,:) - [0.5_dp, 0.0_dp, 0.0_dp])) < 1.0e-10_dp, sp, tot)
    else
       call check("implicit Reciprocal KPOINTS preserves the first point", .false., sp, tot)
    end if

    call teardown(loaded_crystal)
    if (allocated(loaded_kpoints)) deallocate(loaded_kpoints)
    if (allocated(loaded_names)) deallocate(loaded_names)
    if (allocated(input_lmax)) deallocate(input_lmax)
  end subroutine test_cartesian_kpoints_conversion

  subroutine test_shared_config_parser(sp, tot)
    integer, intent(inout) :: sp, tot
    character(len=256) :: config_path, config_poscar, config_kpoints
    character(len=32) :: config_point_group
    integer, allocatable :: config_lmax(:)
    integer :: config_error

    config_path = "/tmp/symm_proj_regression.conf"
    call write_text_file(config_path, &
         "POSCAR_FILE inputs/POSCAR # structure"//new_line('a')// &
         "KPOINTS_FILE inputs/KPOINTS"//new_line('a')// &
         "LMAX 0 2 # s and d channels"//new_line('a')// &
         "POINT_GROUP D6h"//new_line('a'))

    call read_sympw_config(config_path, config_poscar, config_kpoints, &
         config_lmax, config_point_group, config_error)
    call check("shared config parser accepts valid input", config_error == 0, sp, tot)
    call check("shared config parser resolves POSCAR relative path", &
         trim(config_poscar) == "/tmp/inputs/POSCAR", sp, tot)
    call check("shared config parser resolves KPOINTS relative path", &
         trim(config_kpoints) == "/tmp/inputs/KPOINTS", sp, tot)
    call check("shared config parser preserves zero LMAX", &
         allocated(config_lmax) .and. size(config_lmax) == 2 .and. &
         all(config_lmax == [0, 2]), sp, tot)
    call check("shared config parser reads point group", &
         trim(config_point_group) == "D6h", sp, tot)
    if (allocated(config_lmax)) deallocate(config_lmax)
  end subroutine test_shared_config_parser

  subroutine test_complex_c3_eigenvector(sp, tot)
    integer, intent(inout) :: sp, tot
    integer :: multab(3,3), inel(3), cind(3), steer_local(20)
    integer :: nvec
    complex(dp) :: characters(3,3), fi(3,3), omega
    real(dp) :: vector_norm

    multab(1,:) = [1, 2, 3]
    multab(2,:) = [2, 3, 1]
    multab(3,:) = [3, 1, 2]
    inel = [1, 3, 2]
    cind = [1, 2, 3]
    steer_local = 0
    omega = exp(cmplx(0.0_dp, 2.0_dp*pi/3.0_dp, kind=dp))
    characters(:, :) = cmplx(0.0_dp, 0.0_dp, kind=dp)
    characters(1,:) = [cmplx(1.0_dp,0.0_dp,dp), cmplx(1.0_dp,0.0_dp,dp), cmplx(1.0_dp,0.0_dp,dp)]
    characters(2,:) = [cmplx(1.0_dp,0.0_dp,dp), omega, conjg(omega)]
    characters(3,:) = [cmplx(1.0_dp,0.0_dp,dp), conjg(omega), omega]
    fi(:, :) = cmplx(0.0_dp, 0.0_dp, kind=dp)

    call sym_eigvec(fi, nvec, 2, omega, 2, inel, cind, characters, multab, 3, steer_local)
    call check("C3 complex irrep produces an eigenvector", nvec >= 1, sp, tot)
    if (nvec >= 1) then
       vector_norm = sqrt(sum(abs(fi(:,1))**2))
       call check("C3 eigenvector preserves complex phase", maxval(abs(aimag(fi(:,1)))) > 0.1_dp, sp, tot)
       call check("C3 eigenvector is normalized", abs(vector_norm - 1.0_dp) < 1.0e-10_dp, sp, tot)
    else
       call check("C3 eigenvector preserves complex phase", .false., sp, tot)
       call check("C3 eigenvector is normalized", .false., sp, tot)
    end if
  end subroutine test_complex_c3_eigenvector

  subroutine test_complex_c3_irrep_labels(sp, tot)
    integer, intent(inout) :: sp, tot
    type(sympw_crystal_t) :: c3_crystal
    type(sympw_result_t) :: c3_result
    integer :: irrep_position, other_irrep, complex_irrep_count
    logical :: conjugate_pair_found, labels_distinct

    call setup_single_atom(c3_crystal, 1)
    c3_crystal%pgnr = 16
    c3_crystal%lattice(1,:) = [5.0_dp, 0.0_dp, 0.0_dp]
    c3_crystal%lattice(2,:) = [-2.5_dp, 2.5_dp*sqrt(3.0_dp), 0.0_dp]
    c3_crystal%lattice(3,:) = [0.0_dp, 0.0_dp, 8.0_dp]
    call sympw_init(c3_crystal, error_code)
    if (error_code == 0) then
       call sympw_analyze_kpoint([0.0_dp, 0.0_dp, 0.0_dp], c3_result)
    end if

    complex_irrep_count = 0
    conjugate_pair_found = .false.
    labels_distinct = .false.
    if (error_code == 0 .and. c3_result%success .and. allocated(c3_result%irreps)) then
       do irrep_position = 1, size(c3_result%irreps)
          if (maxval(abs(aimag(c3_result%irreps(irrep_position)%characters))) > &
               tol_irrep_phase) complex_irrep_count = complex_irrep_count + 1
          do other_irrep = 1, irrep_position - 1
             if (maxval(abs(c3_result%irreps(irrep_position)%characters - &
                  conjg(c3_result%irreps(other_irrep)%characters))) < tol_irrep_phase) then
                conjugate_pair_found = .true.
                labels_distinct = c3_result%irreps(irrep_position)%label /= &
                     c3_result%irreps(other_irrep)%label
             end if
          end do
       end do
    end if
    call check("C3 exposes both complex-conjugate one-dimensional irreps", &
         complex_irrep_count == 2 .and. conjugate_pair_found, sp, tot)
    call check("complex-conjugate C3 irreps have distinct stable labels", &
         labels_distinct, sp, tot)
    call sympw_finalize()
    call teardown(c3_crystal)
  end subroutine test_complex_c3_irrep_labels

  subroutine test_supported_mulliken_labels(sp, tot)
    integer, intent(inout) :: sp, tot
    type(sympw_crystal_t) :: group_crystal
    type(sympw_result_t) :: group_result
    integer, parameter :: supported_groups(26) = [1, 2, 3, 4, 5, 6, 7, 8, &
         12, 13, 14, 15, 18, 19, 20, 21, 22, 23, 27, 28, 29, 30, 31, 34, 35, 36]
    integer, parameter :: complex_pair_groups(10) = [9, 10, 11, 16, 17, 24, 25, 26, 32, 33]
    integer :: group_position, group_number, group_error, group_lmax
    logical :: all_groups_ok, group_ok, d2h_occurrences_ok, d4h_occurrences_ok
    logical :: d4_occurrences_ok, c4v_occurrences_ok, d2d_occurrences_ok
    logical :: d3_occurrences_ok, c3v_occurrences_ok, d3d_occurrences_ok
    logical :: d6_occurrences_ok, c6v_occurrences_ok, d3h_occurrences_ok
    logical :: d6h_occurrences_ok, o_occurrences_ok, td_occurrences_ok, oh_occurrences_ok
    logical :: complex_pair_blank, cyclic_real_projectors_ok
    logical :: tetrahedral_real_projectors_ok, empty_real_projector_ok

    all_groups_ok = .true.
    d2h_occurrences_ok = .false.
    d4h_occurrences_ok = .false.
    d4_occurrences_ok = .false.
    c4v_occurrences_ok = .false.
    d2d_occurrences_ok = .false.
    d3_occurrences_ok = .false.
    c3v_occurrences_ok = .false.
    d3d_occurrences_ok = .false.
    d6_occurrences_ok = .false.
    c6v_occurrences_ok = .false.
    d3h_occurrences_ok = .false.
    d6h_occurrences_ok = .false.
    o_occurrences_ok = .false.
    td_occurrences_ok = .false.
    oh_occurrences_ok = .false.
    do group_position = 1, size(supported_groups)
       group_number = supported_groups(group_position)
       group_lmax = merge(2, 1, group_number == 31 .or. group_number >= 34)
       call setup_single_atom(group_crystal, group_lmax)
       group_crystal%pgnr = group_number
       if (group_number >= 16 .and. group_number <= 31) then
          call set_hexagonal_test_lattice(group_crystal)
       end if
       call sympw_init(group_crystal, group_error)
       group_ok = group_error == 0
       if (group_ok) then
          call sympw_analyze_kpoint([0.0_dp, 0.0_dp, 0.0_dp], group_result)
          group_ok = group_result%success .and. &
               group_result%mulliken_status == SYMPW_MULLIKEN_STATUS_AVAILABLE .and. &
               mulliken_set_matches(group_result, group_number)
          if (group_number == 8 .and. group_ok) then
             d2h_occurrences_ok = mulliken_multiplicity(group_result, "Ag") == 1 .and. &
                  mulliken_multiplicity(group_result, "B1u") == 1 .and. &
                  mulliken_multiplicity(group_result, "B2u") == 1 .and. &
                  mulliken_multiplicity(group_result, "B3u") == 1 .and. &
                  irrep_projector_matches_real_subspace(group_result, "Ag", [1]) .and. &
                  irrep_projector_matches_real_subspace(group_result, "B1u", [3]) .and. &
                  irrep_projector_matches_real_subspace(group_result, "B2u", [2]) .and. &
                  irrep_projector_matches_real_subspace(group_result, "B3u", [4])
          else if (group_number == 15 .and. group_ok) then
             d4h_occurrences_ok = mulliken_multiplicity(group_result, "A1g") == 1 .and. &
                  mulliken_multiplicity(group_result, "A2u") == 1 .and. &
                  mulliken_multiplicity(group_result, "Eu") == 1 .and. &
                  irrep_projector_matches_real_subspace(group_result, "A1g", [1]) .and. &
                  irrep_projector_matches_real_subspace(group_result, "A2u", [3]) .and. &
                  irrep_projector_matches_real_subspace(group_result, "Eu", [2, 4])
          end if
          if (group_number == 12 .and. group_ok) then
             d4_occurrences_ok = mulliken_multiplicity(group_result, "A1") == 1 .and. &
                  mulliken_multiplicity(group_result, "A2") == 1 .and. &
                  mulliken_multiplicity(group_result, "E") == 1 .and. &
                  irrep_projector_matches_real_subspace(group_result, "A1", [1]) .and. &
                  irrep_projector_matches_real_subspace(group_result, "A2", [3]) .and. &
                  irrep_projector_matches_real_subspace(group_result, "E", [2, 4])
          else if (group_number == 13 .and. group_ok) then
             c4v_occurrences_ok = mulliken_multiplicity(group_result, "A1") == 2 .and. &
                  mulliken_multiplicity(group_result, "E") == 1 .and. &
                  irrep_projector_matches_real_subspace(group_result, "A1", [1, 3]) .and. &
                  irrep_projector_matches_real_subspace(group_result, "E", [2, 4])
          else if (group_number == 14 .and. group_ok) then
             d2d_occurrences_ok = mulliken_multiplicity(group_result, "A1") == 1 .and. &
                  mulliken_multiplicity(group_result, "B2") == 1 .and. &
                  mulliken_multiplicity(group_result, "E") == 1 .and. &
                  irrep_projector_matches_real_subspace(group_result, "A1", [1]) .and. &
                  irrep_projector_matches_real_subspace(group_result, "B2", [3]) .and. &
                  irrep_projector_matches_real_subspace(group_result, "E", [2, 4])
          end if
          if (group_number == 18 .and. group_ok) then
             d3_occurrences_ok = mulliken_multiplicity(group_result, "A1") == 1 .and. &
                  mulliken_multiplicity(group_result, "A2") == 1 .and. &
                  mulliken_multiplicity(group_result, "E") == 1 .and. &
                  irrep_projector_matches_real_subspace(group_result, "A1", [1]) .and. &
                  irrep_projector_matches_real_subspace(group_result, "A2", [3]) .and. &
                  irrep_projector_matches_real_subspace(group_result, "E", [2, 4])
          else if (group_number == 20 .and. group_ok) then
             c3v_occurrences_ok = mulliken_multiplicity(group_result, "A1") == 2 .and. &
                  mulliken_multiplicity(group_result, "E") == 1 .and. &
                  irrep_projector_matches_real_subspace(group_result, "A1", [1, 3]) .and. &
                  irrep_projector_matches_real_subspace(group_result, "E", [2, 4])
          else if (group_number == 22 .and. group_ok) then
             d3d_occurrences_ok = mulliken_multiplicity(group_result, "A1g") == 1 .and. &
                  mulliken_multiplicity(group_result, "A2u") == 1 .and. &
                  mulliken_multiplicity(group_result, "Eu") == 1 .and. &
                  irrep_projector_matches_real_subspace(group_result, "A1g", [1]) .and. &
                  irrep_projector_matches_real_subspace(group_result, "A2u", [3]) .and. &
                  irrep_projector_matches_real_subspace(group_result, "Eu", [2, 4])
          else if (group_number == 27 .and. group_ok) then
             d6_occurrences_ok = mulliken_multiplicity(group_result, "A1") == 1 .and. &
                  mulliken_multiplicity(group_result, "A2") == 1 .and. &
                  mulliken_multiplicity(group_result, "E1") == 1 .and. &
                  irrep_projector_matches_real_subspace(group_result, "A1", [1]) .and. &
                  irrep_projector_matches_real_subspace(group_result, "A2", [3]) .and. &
                  irrep_projector_matches_real_subspace(group_result, "E1", [2, 4])
          else if (group_number == 28 .and. group_ok) then
             c6v_occurrences_ok = mulliken_multiplicity(group_result, "A1") == 2 .and. &
                  mulliken_multiplicity(group_result, "E1") == 1 .and. &
                  irrep_projector_matches_real_subspace(group_result, "A1", [1, 3]) .and. &
                  irrep_projector_matches_real_subspace(group_result, "E1", [2, 4])
          else if (group_number == 29 .and. group_ok) then
             d3h_occurrences_ok = mulliken_multiplicity(group_result, "A1'") == 1 .and. &
                  mulliken_multiplicity(group_result, "A2''") == 1 .and. &
                  mulliken_multiplicity(group_result, "E'") == 1 .and. &
                  irrep_projector_matches_real_subspace(group_result, "A1'", [1]) .and. &
                  irrep_projector_matches_real_subspace(group_result, "A2''", [3]) .and. &
                  irrep_projector_matches_real_subspace(group_result, "E'", [2, 4])
          else if (group_number == 31 .and. group_ok) then
             d6h_occurrences_ok = mulliken_multiplicity(group_result, "A1g") == 2 .and. &
                  mulliken_multiplicity(group_result, "A2u") == 1 .and. &
                  mulliken_multiplicity(group_result, "E1u") == 1 .and. &
                  mulliken_multiplicity(group_result, "E1g") == 1 .and. &
                  mulliken_multiplicity(group_result, "E2g") == 1 .and. &
                  irrep_projector_matches_real_subspace(group_result, "A1g", [1, 7], 2) .and. &
                  irrep_projector_matches_real_subspace(group_result, "A2u", [3], 2) .and. &
                  irrep_projector_matches_real_subspace(group_result, "E1u", [2, 4], 2) .and. &
                  irrep_projector_matches_real_subspace(group_result, "E1g", [6, 8], 2) .and. &
                  irrep_projector_matches_real_subspace(group_result, "E2g", [5, 9], 2)
          else if (group_number == 34 .and. group_ok) then
             o_occurrences_ok = mulliken_multiplicity(group_result, "A1") == 1 .and. &
                  mulliken_multiplicity(group_result, "T1") == 1 .and. &
                  mulliken_multiplicity(group_result, "E") == 1 .and. &
                  mulliken_multiplicity(group_result, "T2") == 1 .and. &
                  irrep_projector_matches_real_subspace(group_result, "A1", [1], 2) .and. &
                  irrep_projector_matches_real_subspace(group_result, "T1", [2, 3, 4], 2) .and. &
                  irrep_projector_matches_real_subspace(group_result, "E", [7, 9], 2) .and. &
                  irrep_projector_matches_real_subspace(group_result, "T2", [5, 6, 8], 2)
          else if (group_number == 35 .and. group_ok) then
             td_occurrences_ok = mulliken_multiplicity(group_result, "A1") == 1 .and. &
                  mulliken_multiplicity(group_result, "T2") == 2 .and. &
                  mulliken_multiplicity(group_result, "E") == 1 .and. &
                  irrep_projector_matches_real_subspace(group_result, "A1", [1], 2) .and. &
                  irrep_projector_matches_real_subspace(group_result, "T2", &
                       [2, 3, 4, 5, 6, 8], 2) .and. &
                  irrep_projector_matches_real_subspace(group_result, "E", [7, 9], 2)
          else if (group_number == 36 .and. group_ok) then
             oh_occurrences_ok = mulliken_multiplicity(group_result, "A1g") == 1 .and. &
                  mulliken_multiplicity(group_result, "T1u") == 1 .and. &
                  mulliken_multiplicity(group_result, "Eg") == 1 .and. &
                  mulliken_multiplicity(group_result, "T2g") == 1 .and. &
                  irrep_projector_matches_real_subspace(group_result, "A1g", [1], 2) .and. &
                  irrep_projector_matches_real_subspace(group_result, "T1u", [2, 3, 4], 2) .and. &
                  irrep_projector_matches_real_subspace(group_result, "Eg", [7, 9], 2) .and. &
                  irrep_projector_matches_real_subspace(group_result, "T2g", [5, 6, 8], 2)
          end if
       end if
       if (.not. group_ok) write(*,*) "Mulliken regression failed for point group", group_number
       all_groups_ok = all_groups_ok .and. group_ok
       call sympw_finalize()
       call teardown(group_crystal)
    end do

    call check("supported ordinary Gamma groups expose complete Mulliken label sets", &
         all_groups_ok, sp, tot)
    call check("D2h Mulliken labels identify the s and p orbital occurrences", &
         d2h_occurrences_ok, sp, tot)
    call check("D4h Mulliken labels identify the s, pz, and transverse-p subspaces", &
         d4h_occurrences_ok, sp, tot)
    call check("D4, C4v, and D2d Mulliken labels identify their orbital subspaces", &
         d4_occurrences_ok .and. c4v_occurrences_ok .and. d2d_occurrences_ok, sp, tot)
    call check("D3, C3v, D3d, and D3h labels identify their orbital subspaces", &
         d3_occurrences_ok .and. c3v_occurrences_ok .and. d3d_occurrences_ok .and. &
         d3h_occurrences_ok, sp, tot)
    call check("D6, C6v, and D6h labels distinguish E1 and E2 orbital subspaces", &
         d6_occurrences_ok .and. c6v_occurrences_ok .and. d6h_occurrences_ok, sp, tot)
    call check("O, Td, and Oh labels identify vector and d-orbital subspaces", &
         o_occurrences_ok .and. td_occurrences_ok .and. oh_occurrences_ok, sp, tot)

    call setup_single_atom(group_crystal, 0)
    group_crystal%pgnr = 8
    call sympw_init(group_crystal, group_error)
    group_ok = group_error == 0
    if (group_ok) then
       call sympw_analyze_kpoint([0.137_dp, 0.211_dp, 0.319_dp], group_result)
       group_ok = group_result%success
    end if
    if (group_ok) then
       group_ok = group_result%mulliken_status == &
            SYMPW_MULLIKEN_STATUS_NOT_FULL_POINT_GROUP .and. &
            all_mulliken_labels_blank(group_result)
    end if
    call check("non-Gamma little groups retain fingerprint-only labels", &
         group_ok, sp, tot)
    call sympw_finalize()
    call teardown(group_crystal)

    complex_pair_blank = .true.
    cyclic_real_projectors_ok = .true.
    tetrahedral_real_projectors_ok = .false.
    empty_real_projector_ok = .false.
    do group_position = 1, size(complex_pair_groups)
       group_number = complex_pair_groups(group_position)
       group_lmax = merge(2, 1, group_number >= 32)
       call setup_single_atom(group_crystal, group_lmax)
       group_crystal%pgnr = group_number
       if (group_number >= 16 .and. group_number <= 31) then
          call set_hexagonal_test_lattice(group_crystal)
       end if
       call sympw_init(group_crystal, group_error)
       group_ok = group_error == 0
       if (group_ok) then
          call sympw_analyze_kpoint([0.0_dp, 0.0_dp, 0.0_dp], group_result)
          group_ok = group_result%success
       end if
       if (group_ok) then
          group_ok = group_result%mulliken_status == &
               SYMPW_MULLIKEN_STATUS_COMPLEX_PAIR .and. &
               all_mulliken_labels_blank(group_result) .and. &
               group_result%real_irrep_view_available .and. &
               real_mulliken_set_matches(group_result, group_number)
       end if
       if (group_ok) then
          select case(group_number)
          case(9)
             cyclic_real_projectors_ok = cyclic_real_projectors_ok .and. &
                  real_irrep_projector_matches_real_subspace(group_result, "A", [1, 3]) .and. &
                  real_irrep_projector_matches_real_subspace(group_result, "E", [2, 4])
             empty_real_projector_ok = real_irrep_projector_is_unavailable(group_result, "B")
          case(16)
             cyclic_real_projectors_ok = cyclic_real_projectors_ok .and. &
                  real_irrep_projector_matches_real_subspace(group_result, "A", [1, 3]) .and. &
                  real_irrep_projector_matches_real_subspace(group_result, "E", [2, 4])
          case(24)
             cyclic_real_projectors_ok = cyclic_real_projectors_ok .and. &
                  real_irrep_projector_matches_real_subspace(group_result, "A", [1, 3]) .and. &
                  real_irrep_projector_matches_real_subspace(group_result, "E1", [2, 4])
          case(32)
             tetrahedral_real_projectors_ok = &
                  real_irrep_projector_matches_real_subspace(group_result, "A", [1], 2) .and. &
                  real_irrep_projector_matches_real_subspace(group_result, "E", [7, 9], 2) .and. &
                  real_irrep_projector_matches_real_subspace(group_result, "T", &
                  [2, 3, 4, 5, 6, 8], 2)
          end select
       end if
       complex_pair_blank = complex_pair_blank .and. group_ok
       call sympw_finalize()
       call teardown(group_crystal)
    end do
    call check("groups whose conventional E labels merge complex pairs retain fingerprints", &
         complex_pair_blank, sp, tot)
    call check("paired real views expose cyclic-group orbital projectors", &
         cyclic_real_projectors_ok, sp, tot)
    call check("paired real views expose tetrahedral orbital projectors", &
         tetrahedral_real_projectors_ok, sp, tot)
    call check("zero-multiplicity real irreps reject projector requests safely", &
         empty_real_projector_ok, sp, tot)
  end subroutine test_supported_mulliken_labels


  subroutine test_mulliken_input_validation(sp, tot)
    integer, intent(inout) :: sp, tot
    real(dp) :: rotation_table(3,3,72)
    complex(dp) :: characters(1)
    character(len=16) :: mulliken_label
    logical :: label_ok

    rotation_table = 0.0_dp
    characters(1) = cmplx(1.0_dp, 0.0_dp, dp)
    call assign_mulliken_label(2, [0], rotation_table, characters, &
         mulliken_label, label_ok)
    call check("Mulliken resolver rejects invalid operation identifiers safely", &
         .not. label_ok .and. len_trim(mulliken_label) == 0, sp, tot)
  end subroutine test_mulliken_input_validation


  logical function mulliken_set_matches(result, point_group_number) result(matches)
    type(sympw_result_t), intent(in) :: result
    integer, intent(in) :: point_group_number

    select case(point_group_number)
    case(1)
       matches = has_mulliken_labels(result, [character(len=4) :: "A"])
    case(2)
       matches = has_mulliken_labels(result, [character(len=4) :: "Ag", "Au"])
    case(3)
       matches = has_mulliken_labels(result, [character(len=4) :: "A", "B"])
    case(4)
       matches = has_mulliken_labels(result, [character(len=4) :: "A'", "A''"])
    case(5)
       matches = has_mulliken_labels(result, [character(len=4) :: "Ag", "Bg", "Au", "Bu"])
    case(6)
       matches = has_mulliken_labels(result, [character(len=4) :: "A", "B1", "B2", "B3"])
    case(7)
       matches = has_mulliken_labels(result, [character(len=4) :: "A1", "A2", "B1", "B2"])
    case(8)
       matches = has_mulliken_labels(result, [character(len=4) :: &
            "Ag", "B1g", "B2g", "B3g", "Au", "B1u", "B2u", "B3u"])
    case(12:14)
       matches = has_mulliken_labels(result, [character(len=4) :: &
            "A1", "A2", "B1", "B2", "E"])
    case(15)
       matches = has_mulliken_labels(result, [character(len=4) :: &
            "A1g", "A2g", "B1g", "B2g", "Eg", &
            "A1u", "A2u", "B1u", "B2u", "Eu"])
    case(18:21)
       matches = has_mulliken_labels(result, [character(len=4) :: "A1", "A2", "E"])
    case(22:23)
       matches = has_mulliken_labels(result, [character(len=4) :: &
            "A1g", "A2g", "Eg", "A1u", "A2u", "Eu"])
    case(27:28)
       matches = has_mulliken_labels(result, [character(len=4) :: &
            "A1", "A2", "B1", "B2", "E1", "E2"])
    case(29:30)
       matches = has_mulliken_labels(result, [character(len=4) :: &
            "A1'", "A2'", "E'", "A1''", "A2''", "E''"])
    case(31)
       matches = has_mulliken_labels(result, [character(len=4) :: &
            "A1g", "A2g", "B1g", "B2g", "E1g", "E2g", &
            "A1u", "A2u", "B1u", "B2u", "E1u", "E2u"])
    case(34:35)
       matches = has_mulliken_labels(result, [character(len=4) :: &
            "A1", "A2", "E", "T1", "T2"])
    case(36)
       matches = has_mulliken_labels(result, [character(len=4) :: &
            "A1g", "A2g", "Eg", "T1g", "T2g", &
            "A1u", "A2u", "Eu", "T1u", "T2u"])
    case default
       matches = .false.
    end select
  end function mulliken_set_matches


  logical function has_mulliken_labels(result, expected_labels) result(matches)
    type(sympw_result_t), intent(in) :: result
    character(len=*), intent(in) :: expected_labels(:)
    integer :: expected_position, irrep_position
    logical :: found

    matches = .false.
    if (.not. allocated(result%irreps)) return
    if (size(result%irreps) /= size(expected_labels)) return
    do expected_position = 1, size(expected_labels)
       found = .false.
       do irrep_position = 1, size(result%irreps)
          if (trim(result%irreps(irrep_position)%mulliken_label) == &
               trim(expected_labels(expected_position))) then
             found = .true.
             exit
          end if
       end do
       if (.not. found) return
    end do
    matches = .true.
  end function has_mulliken_labels


  logical function real_mulliken_set_matches(result, point_group_number) result(matches)
    type(sympw_result_t), intent(in) :: result
    integer, intent(in) :: point_group_number

    select case(point_group_number)
    case(9:10)
       matches = has_real_mulliken_labels(result, [character(len=4) :: "A", "B", "E"])
    case(11)
       matches = has_real_mulliken_labels(result, [character(len=4) :: &
            "Ag", "Bg", "Eg", "Au", "Bu", "Eu"])
    case(16)
       matches = has_real_mulliken_labels(result, [character(len=4) :: "A", "E"])
    case(17)
       matches = has_real_mulliken_labels(result, [character(len=4) :: &
            "Ag", "Eg", "Au", "Eu"])
    case(24)
       matches = has_real_mulliken_labels(result, [character(len=4) :: &
            "A", "B", "E1", "E2"])
    case(25)
       matches = has_real_mulliken_labels(result, [character(len=4) :: &
            "A'", "E'", "A''", "E''"])
    case(26)
       matches = has_real_mulliken_labels(result, [character(len=4) :: &
            "Ag", "Bg", "E1g", "E2g", "Au", "Bu", "E1u", "E2u"])
    case(32)
       matches = has_real_mulliken_labels(result, [character(len=4) :: "A", "E", "T"])
    case(33)
       matches = has_real_mulliken_labels(result, [character(len=4) :: &
            "Ag", "Eg", "Tg", "Au", "Eu", "Tu"])
    case default
       matches = .false.
    end select
  end function real_mulliken_set_matches


  logical function has_real_mulliken_labels(result, expected_labels) result(matches)
    type(sympw_result_t), intent(in) :: result
    character(len=*), intent(in) :: expected_labels(:)
    integer :: expected_position, view_position
    logical :: found

    matches = .false.
    if (.not. result%real_irrep_view_available .or. &
         .not. allocated(result%real_irreps)) return
    if (size(result%real_irreps) /= size(expected_labels)) return
    do expected_position = 1, size(expected_labels)
       found = .false.
       do view_position = 1, size(result%real_irreps)
          if (trim(result%real_irreps(view_position)%label) == &
               trim(expected_labels(expected_position))) then
             found = .true.
             exit
          end if
       end do
       if (.not. found) return
    end do
    matches = .true.
  end function has_real_mulliken_labels


  integer function mulliken_multiplicity(result, label) result(multiplicity)
    type(sympw_result_t), intent(in) :: result
    character(len=*), intent(in) :: label
    integer :: irrep_position

    multiplicity = -1
    if (.not. allocated(result%irreps)) return
    do irrep_position = 1, size(result%irreps)
       if (trim(result%irreps(irrep_position)%mulliken_label) == trim(label)) then
          multiplicity = result%irreps(irrep_position)%multiplicity
          return
       end if
    end do
  end function mulliken_multiplicity


  logical function irrep_projector_matches_real_subspace(result, label, basis_indices, &
       lmax_value) result(matches)
    type(sympw_result_t), intent(in) :: result
    character(len=*), intent(in) :: label
    integer, intent(in) :: basis_indices(:)
    integer, intent(in), optional :: lmax_value
    complex(dp), allocatable :: projector(:,:), projector_real(:,:), expected(:,:)
    integer :: irrep_position, basis_position, lmax_local
    logical :: found, projector_ok

    matches = .false.
    lmax_local = 1
    if (present(lmax_value)) lmax_local = lmax_value
    if (lmax_local < 0) return
    if (.not. allocated(result%irreps)) return
    if (size(basis_indices) < 1 .or. any(basis_indices < 1) .or. &
         any(basis_indices > result%matrix_order)) return
    found = .false.
    do irrep_position = 1, size(result%irreps)
       if (trim(result%irreps(irrep_position)%mulliken_label) == trim(label)) then
          found = .true.
          exit
       end if
    end do
    if (.not. found) return

    call sympw_get_irrep_projector(result, irrep_position, projector, projector_ok)
    if (.not. projector_ok) return
    allocate(projector_real(result%matrix_order, result%matrix_order))
    allocate(expected(result%matrix_order, result%matrix_order))
    call complex_to_real_projector(projector, [lmax_local], [1], projector_real)
    expected = cmplx(0.0_dp, 0.0_dp, dp)
    do basis_position = 1, size(basis_indices)
       expected(basis_indices(basis_position), basis_indices(basis_position)) = &
            cmplx(1.0_dp, 0.0_dp, dp)
    end do
    matches = maxval(abs(projector_real - expected)) < tol_projection
  end function irrep_projector_matches_real_subspace


  logical function real_irrep_projector_matches_real_subspace(result, label, basis_indices, &
       lmax_value) result(matches)
    type(sympw_result_t), intent(in) :: result
    character(len=*), intent(in) :: label
    integer, intent(in) :: basis_indices(:)
    integer, intent(in), optional :: lmax_value
    complex(dp), allocatable :: projector(:,:), projector_real(:,:), expected(:,:)
    integer :: view_position, basis_position, lmax_local
    logical :: found, projector_ok

    matches = .false.
    lmax_local = 1
    if (present(lmax_value)) lmax_local = lmax_value
    if (lmax_local < 0) return
    if (.not. result%real_irrep_view_available .or. &
         .not. allocated(result%real_irreps)) return
    if (size(basis_indices) < 1 .or. any(basis_indices < 1) .or. &
         any(basis_indices > result%matrix_order)) return
    found = .false.
    do view_position = 1, size(result%real_irreps)
       if (trim(result%real_irreps(view_position)%label) == trim(label)) then
          found = .true.
          exit
       end if
    end do
    if (.not. found) return

    call sympw_get_real_irrep_projector(result, view_position, projector, projector_ok)
    if (.not. projector_ok) return
    allocate(projector_real(result%matrix_order, result%matrix_order))
    allocate(expected(result%matrix_order, result%matrix_order))
    call complex_to_real_projector(projector, [lmax_local], [1], projector_real)
    expected = cmplx(0.0_dp, 0.0_dp, dp)
    do basis_position = 1, size(basis_indices)
       expected(basis_indices(basis_position), basis_indices(basis_position)) = &
            cmplx(1.0_dp, 0.0_dp, dp)
    end do
    matches = maxval(abs(projector_real - expected)) < tol_projection
  end function real_irrep_projector_matches_real_subspace


  logical function real_irrep_projector_is_unavailable(result, label) result(unavailable)
    type(sympw_result_t), intent(in) :: result
    character(len=*), intent(in) :: label
    complex(dp), allocatable :: projector(:,:)
    integer :: view_position
    logical :: projector_ok

    unavailable = .false.
    if (.not. allocated(result%real_irreps)) return
    do view_position = 1, size(result%real_irreps)
       if (trim(result%real_irreps(view_position)%label) /= trim(label)) cycle
       call sympw_get_real_irrep_projector(result, view_position, projector, projector_ok)
       unavailable = .not. projector_ok .and. .not. allocated(projector)
       return
    end do
  end function real_irrep_projector_is_unavailable


  logical function all_mulliken_labels_blank(result) result(all_blank)
    type(sympw_result_t), intent(in) :: result
    integer :: irrep_position

    all_blank = .false.
    if (.not. allocated(result%irreps)) return
    all_blank = .true.
    do irrep_position = 1, size(result%irreps)
       if (len_trim(result%irreps(irrep_position)%mulliken_label) /= 0) then
          all_blank = .false.
          return
       end if
    end do
  end function all_mulliken_labels_blank

  subroutine test_complex_subspace_intersection(sp, tot)
    integer, intent(inout) :: sp, tot
    integer :: intersection_dim
    complex(dp) :: fi(3,2), dfi(2,3), shared(3)
    real(dp) :: inv_sqrt_two

    inv_sqrt_two = 1.0_dp/sqrt(2.0_dp)
    shared = [cmplx(inv_sqrt_two, 0.0_dp, dp), &
         cmplx(0.0_dp, inv_sqrt_two, dp), cmplx(0.0_dp, 0.0_dp, dp)]
    dfi(1,:) = shared
    dfi(2,:) = [cmplx(0.0_dp, 0.0_dp, dp), cmplx(0.0_dp, 0.0_dp, dp), &
         cmplx(1.0_dp, 0.0_dp, dp)]
    fi(:,1) = shared
    fi(:,2) = [cmplx(0.0_dp, inv_sqrt_two, dp), cmplx(inv_sqrt_two, 0.0_dp, dp), &
         cmplx(0.0_dp, 0.0_dp, dp)]

    intersection_dim = -1
    call sym_intsec(fi, intersection_dim, 2, 2, 3, dfi)
    call check("complex subspace intersection has correct dimension", &
         intersection_dim == 1, sp, tot)
    if (intersection_dim == 1) then
       call check("complex subspace intersection preserves shared vector", &
            abs(abs(dot_product(shared, fi(:,1))) - 1.0_dp) < 1.0e-8_dp, sp, tot)
    else
       call check("complex subspace intersection preserves shared vector", .false., sp, tot)
    end if
  end subroutine test_complex_subspace_intersection

  subroutine test_graphene_point_group(sp, tot)
    integer, intent(inout) :: sp, tot
    integer :: detected_group
    integer :: graphene_nat(1)
    real(dp) :: graphene_lattice(3,3), graphene_inverse(3,3)
    real(dp) :: graphene_positions(3,1,2)
    type(pg_data_t) :: pg
    type(sympw_crystal_t) :: graphene_crystal
    type(sympw_result_t) :: graphene_result

    graphene_lattice(1,:) = [2.46_dp, 0.0_dp, 0.0_dp]
    graphene_lattice(2,:) = [-1.23_dp, 2.13_dp, 0.0_dp]
    graphene_lattice(3,:) = [0.0_dp, 0.0_dp, 20.0_dp]
    call inverse3_local(graphene_lattice, graphene_inverse)
    graphene_inverse = transpose(graphene_inverse)
    graphene_nat = [2]
    graphene_positions(:,1,1) = matmul(transpose(graphene_lattice), &
         [0.0_dp, 0.0_dp, 0.5_dp])
    graphene_positions(:,1,2) = matmul(transpose(graphene_lattice), &
         [0.333_dp, 0.667_dp, 0.5_dp])

    call init_point_group_data(pg, 0)
    detected_group = detect_structure_point_group(graphene_lattice, graphene_inverse, &
         graphene_positions, 1, graphene_nat, pg)
    call check("graphene structure is detected as D6h", detected_group == 31, sp, tot)
    call deallocate_point_group_data(pg)

    call inverse3_local(graphene_lattice, graphene_inverse)
    call sym_bztest(detected_group, [1.0_dp/3.0_dp, 1.0_dp/3.0_dp, 0.0_dp], &
         graphene_inverse)
    call check("rounded graphene K point lies on the first-BZ boundary", &
         detected_group == 0, sp, tot)

    graphene_crystal%lattice = graphene_lattice
    graphene_crystal%nel = 1
    graphene_crystal%pgnr = 0
    allocate(graphene_crystal%nat(1), graphene_crystal%lmax(1), &
         graphene_crystal%pos_frac(3,1,2))
    graphene_crystal%nat = [2]
    graphene_crystal%lmax = [0]
    graphene_crystal%pos_frac(:,1,1) = [0.0_dp, 0.0_dp, 0.5_dp]
    graphene_crystal%pos_frac(:,1,2) = [0.333_dp, 0.667_dp, 0.5_dp]
    call sympw_init(graphene_crystal, error_code)
    call check("rounded graphene structure auto-initializes", error_code == 0, sp, tot)
    if (error_code == 0) then
       call sympw_analyze_kpoint([0.333_dp, 0.333_dp, 0.0_dp], graphene_result)
       call check("rounded graphene K point has D3h little group", &
            graphene_result%little_group_order == 12, sp, tot)
       call check("rounded graphene K point reports canonical coordinates", &
            maxval(abs(graphene_result%kpoint_internal - &
            [1.0_dp/3.0_dp, 1.0_dp/3.0_dp, 0.0_dp])) < tol_zero, sp, tot)
       call check("symmorphic graphene K point is projected", graphene_result%success, sp, tot)
       call sympw_finalize()
    else
       call check("rounded graphene K point has D3h little group", .false., sp, tot)
       call check("rounded graphene K point reports canonical coordinates", .false., sp, tot)
       call check("symmorphic graphene K point is projected", .false., sp, tot)
    end if
    call teardown(graphene_crystal)
  end subroutine test_graphene_point_group


  subroutine test_time_reversal_projector_relation(sp, tot)
    integer, intent(inout) :: sp, tot

    complex(dp) :: projector_k(2,2), projector_minus_k(2,2)
    logical :: is_symmetric
    real(dp) :: max_diff

    projector_k = reshape([cmplx(0.5_dp, 0.0_dp, dp), &
         cmplx(0.0_dp, 0.5_dp, dp), cmplx(0.0_dp, -0.5_dp, dp), &
         cmplx(0.5_dp, 0.0_dp, dp)], [2,2])
    projector_minus_k = conjg(projector_k)

    call verify_spinless_projector_pair(projector_k, projector_minus_k, 1.0e-12_dp, &
         is_symmetric, max_diff)
    call check("spinless TR projector relation accepts conjugate pair", &
         is_symmetric .and. max_diff < 1.0e-12_dp, sp, tot)

    projector_minus_k(1,2) = projector_minus_k(1,2) + cmplx(1.0e-3_dp, 0.0_dp, dp)
    call verify_spinless_projector_pair(projector_k, projector_minus_k, 1.0e-5_dp, &
         is_symmetric, max_diff)
    call check("spinless TR projector relation rejects mismatched pair", &
         (.not. is_symmetric) .and. max_diff > 1.0e-4_dp, sp, tot)
  end subroutine test_time_reversal_projector_relation

  subroutine test_real_basis_time_reversal_relation(sp, tot)
    integer, intent(inout) :: sp, tot
    integer :: i, j
    integer :: lmax_list(1), nat_list(1)
    real(dp) :: vector_norm, max_diff, max_residual
    logical :: is_symmetric, copy_ok, copy_matches, projector_valid
    complex(dp) :: state_k(4), time_reversal_matrix(4,4)
    complex(dp) :: projector_csh_k(4,4), projector_csh_minus_k(4,4)
    complex(dp) :: projector_rsh_k(4,4), projector_rsh_minus_k(4,4)
    complex(dp), allocatable :: copied_projector(:,:)

    lmax_list = [1]
    nat_list = [1]
    state_k = [cmplx(0.3_dp, -0.1_dp, dp), cmplx(1.0_dp, 0.5_dp, dp), &
         cmplx(-0.25_dp, 0.75_dp, dp), cmplx(0.4_dp, -0.2_dp, dp)]
    vector_norm = sqrt(sum(abs(state_k)**2))
    state_k = state_k / vector_norm

    do i = 1, 4
       do j = 1, 4
          projector_csh_k(i,j) = state_k(i) * conjg(state_k(j))
       end do
    end do
    call validate_projector_matrix(projector_csh_k, tol_projection, &
         projector_valid, max_residual)
    call check("CSH rank-one projector validates", &
         projector_valid .and. max_residual <= tol_projection, sp, tot)

    time_reversal_matrix = cmplx(0.0_dp, 0.0_dp, dp)
    time_reversal_matrix(1,1) = cmplx(1.0_dp, 0.0_dp, dp)
    time_reversal_matrix(2,4) = cmplx(-1.0_dp, 0.0_dp, dp)
    time_reversal_matrix(3,3) = cmplx(1.0_dp, 0.0_dp, dp)
    time_reversal_matrix(4,2) = cmplx(-1.0_dp, 0.0_dp, dp)
    projector_csh_minus_k = matmul(time_reversal_matrix, &
         matmul(conjg(projector_csh_k), transpose(time_reversal_matrix)))

    call verify_spinless_projector_pair(projector_csh_k, projector_csh_minus_k, &
         1.0e-12_dp, is_symmetric, max_diff)
    call check("CSH time reversal is not plain complex conjugation", &
         .not. is_symmetric, sp, tot)

    call complex_to_real_projector(projector_csh_k, lmax_list, nat_list, projector_rsh_k)
    call complex_to_real_projector(projector_csh_minus_k, lmax_list, nat_list, projector_rsh_minus_k)
    call validate_projector_matrix(projector_rsh_k, tol_projection, &
         projector_valid, max_residual)
    call check("RSH transformed projector validates", &
         projector_valid .and. max_residual <= tol_projection, sp, tot)
    call verify_spinless_projector_pair(projector_rsh_k, projector_rsh_minus_k, &
         1.0e-12_dp, is_symmetric, max_diff)
    call check("RSH time reversal is plain complex conjugation", &
         is_symmetric .and. max_diff < 1.0e-12_dp, sp, tot)

    call build_spinless_partner_projector(projector_rsh_k, tol_projection, &
         copied_projector, copy_ok, max_residual)
    copy_matches = .false.
    if (copy_ok .and. allocated(copied_projector)) then
       copy_matches = maxval(abs(copied_projector - projector_rsh_minus_k)) < 1.0e-12_dp
    end if
    call check("spinless RSH copy matches explicit CSH time reversal", &
         copy_matches, sp, tot)
    if (allocated(copied_projector)) deallocate(copied_projector)
  end subroutine test_real_basis_time_reversal_relation

  subroutine test_library_basis_and_tr_audit(sp, tot)
    integer, intent(inout) :: sp, tot
    type(sympw_crystal_t) :: audit_crystal
    type(sympw_result_t) :: audit_result, repeated_result
    real(dp) :: kpoint(3), minus_kpoint(3), max_diff, basis_residual
    logical :: audit_ok, irrep_metadata_ok, irrep_projectors_ok, labels_stable

    call setup_single_atom(audit_crystal, 1)
    call sympw_init(audit_crystal, error_code)
    call check("library initializes for basis/TR audit", error_code == 0, sp, tot)
    if (error_code == 0) then
       kpoint = [0.25_dp, 0.0_dp, 0.0_dp]
       minus_kpoint = -kpoint
       call sympw_analyze_kpoint(kpoint, audit_result)
       call check("library exposes symmetry-adapted basis", &
            audit_result%success .and. allocated(audit_result%symmetry_basis), sp, tot)
       call check("library exposes CSH and RSH projectors", &
            allocated(audit_result%projector) .and. allocated(audit_result%projector_real), sp, tot)
       if (audit_result%success .and. allocated(audit_result%symmetry_basis) .and. &
            allocated(audit_result%projector)) then
          basis_residual = maxval(abs(audit_result%projector - &
               matmul(audit_result%symmetry_basis, &
               transpose(conjg(audit_result%symmetry_basis)))))
          call check("library projector equals T*T^H", &
               basis_residual < tol_projection_work, sp, tot)
       else
          call check("library projector equals T*T^H", .false., sp, tot)
       end if
       call audit_irrep_subspaces(audit_result, irrep_metadata_ok, irrep_projectors_ok)
       call check("library exposes allowed-irrep column metadata", &
            irrep_metadata_ok, sp, tot)
       call check("irrep-resolved projectors sum to aggregate projector", &
            irrep_projectors_ok, sp, tot)
       call sympw_analyze_kpoint(kpoint, repeated_result)
       call compare_irrep_labels(audit_result, repeated_result, labels_stable)
       call check("irrep character labels are stable across repeated analysis", &
            labels_stable, sp, tot)
       call sympw_check_spinless_time_reversal(kpoint, minus_kpoint, &
            tol_projection_work, audit_ok, max_diff)
       call check("independent library k/-k audit passes", &
            audit_ok .and. max_diff < tol_projection_work, sp, tot)
       call sympw_finalize()
    else
       call check("library exposes symmetry-adapted basis", .false., sp, tot)
       call check("library exposes CSH and RSH projectors", .false., sp, tot)
       call check("library projector equals T*T^H", .false., sp, tot)
       call check("library exposes allowed-irrep column metadata", .false., sp, tot)
       call check("irrep-resolved projectors sum to aggregate projector", .false., sp, tot)
       call check("irrep character labels are stable across repeated analysis", .false., sp, tot)
       call check("independent library k/-k audit passes", .false., sp, tot)
    end if
    call teardown(audit_crystal)
  end subroutine test_library_basis_and_tr_audit

  subroutine audit_irrep_subspaces(result, metadata_ok, projectors_ok)
    type(sympw_result_t), intent(in) :: result
    logical, intent(out) :: metadata_ok, projectors_ok

    complex(dp), allocatable :: irrep_projector(:,:), projector_sum(:,:)
    integer :: irrep_position, other_irrep, column_count, expected_column, rank_sum
    logical :: projector_ok, projector_valid
    logical :: has_longitudinal_scalar_pair, has_transverse_doublet
    real(dp) :: max_residual
    complex(dp) :: character_inner_product

    metadata_ok = .false.
    projectors_ok = .false.
    if (.not. result%success .or. .not. allocated(result%symmetry_basis) .or. &
         .not. allocated(result%projector) .or. .not. allocated(result%irreps)) return
    if (size(result%irreps) /= result%n_allowed_irreps) return

    allocate(projector_sum(result%matrix_order, result%matrix_order))
    projector_sum = cmplx(0.0_dp, 0.0_dp, dp)
    expected_column = 1
    rank_sum = 0
    has_longitudinal_scalar_pair = .false.
    has_transverse_doublet = .false.
    do irrep_position = 1, size(result%irreps)
       if (result%irreps(irrep_position)%group_index < 1 .or. &
            result%irreps(irrep_position)%dimension < 1 .or. &
            result%irreps(irrep_position)%multiplicity < 0) return
       if (len_trim(result%irreps(irrep_position)%label) == 0 .or. &
            .not. allocated(result%irreps(irrep_position)%characters)) return
       if (size(result%irreps(irrep_position)%characters) /= &
            result%factor_group_order) return
       if (abs(result%irreps(irrep_position)%characters(1) - &
            result%irreps(irrep_position)%dimension) > tol_irrep_phase) return
       character_inner_product = sum(conjg(result%irreps(irrep_position)%characters) * &
            result%irreps(irrep_position)%characters) / real(result%factor_group_order, dp)
       if (abs(character_inner_product - cmplx(1.0_dp, 0.0_dp, dp)) > &
            tol_irrep_phase) return
       do other_irrep = 1, irrep_position - 1
          if (trim(result%irreps(irrep_position)%label) == &
               trim(result%irreps(other_irrep)%label)) return
          character_inner_product = sum(conjg(result%irreps(other_irrep)%characters) * &
               result%irreps(irrep_position)%characters) / &
               real(result%factor_group_order, dp)
          if (abs(character_inner_product) > tol_irrep_phase) return
       end do
       if (result%irreps(irrep_position)%multiplicity == 0) then
          if (result%irreps(irrep_position)%column_start /= 0 .or. &
               result%irreps(irrep_position)%column_end /= 0) return
          call sympw_get_irrep_projector(result, irrep_position, &
               irrep_projector, projector_ok)
          if (projector_ok .or. allocated(irrep_projector)) return
          cycle
       end if

       column_count = result%irreps(irrep_position)%column_end - &
            result%irreps(irrep_position)%column_start + 1
       if (result%irreps(irrep_position)%column_start /= expected_column .or. &
            column_count /= result%irreps(irrep_position)%dimension * &
            result%irreps(irrep_position)%multiplicity) return
       expected_column = result%irreps(irrep_position)%column_end + 1
       rank_sum = rank_sum + column_count
       if (result%irreps(irrep_position)%dimension == 1 .and. &
            result%irreps(irrep_position)%multiplicity == 2) then
          has_longitudinal_scalar_pair = .true.
       end if
       if (result%irreps(irrep_position)%dimension == 2 .and. &
            result%irreps(irrep_position)%multiplicity == 1) then
          has_transverse_doublet = .true.
       end if

       call sympw_get_irrep_projector(result, irrep_position, irrep_projector, projector_ok)
       if (.not. projector_ok .or. .not. allocated(irrep_projector)) return
       call validate_projector_matrix(irrep_projector, tol_projection_work, &
            projector_valid, max_residual)
       if (.not. projector_valid) return
       projector_sum = projector_sum + irrep_projector
    end do

    metadata_ok = expected_column == result%matrix_order + 1 .and. &
         rank_sum == result%matrix_order .and. has_longitudinal_scalar_pair .and. &
         has_transverse_doublet
    projectors_ok = metadata_ok .and. &
         maxval(abs(projector_sum - result%projector)) < tol_projection_work
  end subroutine audit_irrep_subspaces

  subroutine compare_irrep_labels(first_result, second_result, labels_match)
    type(sympw_result_t), intent(in) :: first_result, second_result
    logical, intent(out) :: labels_match

    integer :: irrep_position

    labels_match = .false.
    if (.not. first_result%success .or. .not. second_result%success .or. &
         .not. allocated(first_result%irreps) .or. &
         .not. allocated(second_result%irreps)) return
    if (size(first_result%irreps) /= size(second_result%irreps)) return
    do irrep_position = 1, size(first_result%irreps)
       if (first_result%irreps(irrep_position)%label /= &
            second_result%irreps(irrep_position)%label) return
       if (maxval(abs(first_result%irreps(irrep_position)%characters - &
            second_result%irreps(irrep_position)%characters)) > tol_irrep_phase) return
    end do
    labels_match = .true.
  end subroutine compare_irrep_labels

  subroutine test_f_orbital_support(sp, tot)
    integer, intent(inout) :: sp, tot
    type(sympw_crystal_t) :: f_crystal
    type(sympw_result_t) :: f_result

    call setup_single_atom(f_crystal, 3)
    call sympw_init(f_crystal, error_code)
    call check("lmax=3 crystal initializes", error_code == 0, sp, tot)
    if (error_code == 0) then
       call sympw_analyze_kpoint([0.0_dp, 0.0_dp, 0.0_dp], f_result)
       call check("lmax=3 produces the full s+p+d+f basis", &
            f_result%success .and. f_result%matrix_order == 16, sp, tot)
       call check("lmax=3 exposes real-basis projector", &
            allocated(f_result%projector_real), sp, tot)
       call sympw_finalize()
    else
       call check("lmax=3 produces the full s+p+d+f basis", .false., sp, tot)
       call check("lmax=3 exposes real-basis projector", .false., sp, tot)
    end if
    call teardown(f_crystal)
  end subroutine test_f_orbital_support

  subroutine test_projective_factor_group_mode(sp, tot)
    integer, intent(inout) :: sp, tot
    integer :: space_group_fallback, ksym, ibz
    logical :: truth_table_ok

    call check("projective factor group activates only when all legacy flags are zero", &
         projective_factor_group_active(0, 0, 0), sp, tot)

    truth_table_ok = .true.
    do space_group_fallback = 0, 1
       do ksym = 0, 1
          do ibz = 0, 1
             if (space_group_fallback + ksym + ibz == 0) cycle
             truth_table_ok = truth_table_ok .and. &
                  .not. projective_factor_group_active(space_group_fallback, ksym, ibz)
          end do
       end do
    end do
    call check("any ordinary-group flag disables the projective factor group", &
         truth_table_ok, sp, tot)
  end subroutine test_projective_factor_group_mode

  subroutine test_summation_set_modes(sp, tot)
    integer, intent(inout) :: sp, tot
    integer :: np(1,2,2), npl(1,2,2,2)
    integer :: nat(1), kgel(2), listp(2), steer(20)
    real(dp) :: nvec(1,2,2,2,3)
    real(dp) :: a(3,3), ai(3,3), b(3,3), r(3,1,2), u(2,3)
    real(dp) :: til(2,3), rgr(3,3,2)
    logical :: ordinary_ok, projective_ok

    a = reshape([2.0_dp, 1.0_dp, 0.0_dp, &
                 0.0_dp, 2.0_dp, 0.0_dp, &
                 0.0_dp, 0.0_dp, 3.0_dp], [3,3])
    ai = reshape([0.5_dp, 0.0_dp, 0.0_dp, &
                 -0.25_dp, 0.5_dp, 0.0_dp, &
                  0.0_dp, 0.0_dp, 1.0_dp/3.0_dp], [3,3])
    b = 0.0_dp
    r = 0.0_dp
    r(:,1,2) = [1.0_dp, 0.0_dp, 0.0_dp]
    rgr = 0.0_dp
    rgr(1,1,:) = 1.0_dp
    rgr(2,2,:) = 1.0_dp
    rgr(3,3,:) = 1.0_dp
    nat = [2]
    kgel = [1, 1]
    listp = [1, 1]
    u = 0.0_dp
    u(1,:) = [0.5_dp, 0.0_dp, 0.0_dp]
    til = 0.0_dp
    til(2,:) = [0.5_dp, 0.0_dp, 0.0_dp]

    np = 0
    npl = 0
    nvec = 0.0_dp
    steer = 0
    steer(20) = 1
    call sym_sumsets(np, nvec, npl, til, 1, kgel, rgr, listp, &
         a, ai, b, r, u, 1, nat, 1, 1, steer)
    ordinary_ok = np(1,1,2) == 1 .and. np(1,2,1) == 1 .and. &
         maxval(abs(nvec(1,2,1,1,:) - [1.0_dp, 0.0_dp, 0.0_dp])) < tol_zero
    call check("ordinary summation sets honor row-vector lattice storage", &
         ordinary_ok, sp, tot)

    np = 0
    npl = 0
    nvec = 0.0_dp
    steer = 0
    call sym_sumsets(np, nvec, npl, til, 2, kgel, rgr, listp, &
         a, ai, b, r, u, 1, nat, 0, 0, steer)
    projective_ok = all(np(1,1:2,1:2) == 1) .and. &
         npl(1,1,1,1) == 1 .and. npl(1,1,2,1) == 2 .and. &
         npl(1,2,2,1) == 1 .and. npl(1,2,1,1) == 2 .and. &
         maxval(abs(nvec(1,2,1,1,:) - [1.0_dp, 0.0_dp, 0.0_dp])) < tol_zero
    call check("projective summation sets use lifted translations", &
         projective_ok, sp, tot)
  end subroutine test_summation_set_modes

  subroutine test_summation_set_failure_status(sp, tot)
    integer, intent(inout) :: sp, tot
    integer :: np(1,2,2), npl(1,2,2,1)
    integer :: nat(1), kgel(1), listp(1), steer(20)
    real(dp) :: nvec(1,2,2,1,3)
    real(dp) :: a(3,3), ai(3,3), b(3,3), r(3,1,2), u(1,3)
    real(dp) :: til(1,3), rgr(3,3,1)
    logical :: mapping_ok

    a = 0.0_dp
    a(1,1) = 2.0_dp
    a(2,2) = 2.0_dp
    a(3,3) = 2.0_dp
    ai = 0.0_dp
    ai(1,1) = 0.5_dp
    ai(2,2) = 0.5_dp
    ai(3,3) = 0.5_dp
    b = 0.0_dp
    r = 0.0_dp
    rgr = 0.0_dp
    rgr(1,1,1) = 1.0_dp
    rgr(2,2,1) = 1.0_dp
    rgr(3,3,1) = 1.0_dp
    kgel = [1]
    listp = [1]
    til = 0.0_dp
    steer = 0
    steer(20) = 1

    np = 0
    npl = 0
    nvec = 0.0_dp
    nat = [1]
    u(1,:) = [0.5_dp, 0.0_dp, 0.0_dp]
    call sym_sumsets(np, nvec, npl, til, 1, kgel, rgr, listp, &
         a, ai, b, r, u, 1, nat, 1, 1, steer, success=mapping_ok)
    call check("missing atom mapping returns failure status", &
         .not. mapping_ok, sp, tot)

    np = 0
    npl = 0
    nvec = 0.0_dp
    nat = [2]
    u = 0.0_dp
    call sym_sumsets(np, nvec, npl, til, 1, kgel, rgr, listp, &
         a, ai, b, r, u, 1, nat, 1, 1, steer, success=mapping_ok)
    call check("ambiguous atom mapping returns failure status", &
         .not. mapping_ok, sp, tot)
  end subroutine test_summation_set_failure_status

  subroutine test_legacy_symprj_nonorthogonal_mapping(sp, tot)
    integer, intent(inout) :: sp, tot
    type(pg_data_t) :: legacy_pg
    integer :: lmax(1), nat(1), num_block(1), steer(20)
    real(dp) :: a(3,3), r(3,1,2), u(2,3), kpoints(1,3)
    complex(dp) :: projmatrix(2,2,1), res(3,3), identity(2,2)
    logical :: legacy_ok

    call init_point_group_data(legacy_pg, 0)
    a = reshape([2.0_dp, 1.0_dp, 0.0_dp, &
                 0.0_dp, 2.0_dp, 0.0_dp, &
                 0.0_dp, 0.0_dp, 3.0_dp], [3,3])
    r = 0.0_dp
    r(:,1,2) = [1.0_dp, 0.0_dp, 0.0_dp]
    u = 0.0_dp
    u(2,:) = [0.5_dp, 0.0_dp, 0.0_dp]
    kpoints = 0.0_dp
    lmax = [0]
    nat = [2]
    steer = 0
    steer(11) = 1
    res = cmplx(0.0_dp, 0.0_dp, dp)

    call symprj(projmatrix, 2, num_block, 2, a, r, u, 1, lmax, nat, &
         kpoints, legacy_pg%Oh, legacy_pg%D6h, legacy_pg%MOh, &
         legacy_pg%MD6h, legacy_pg%npgo, nge2, legacy_pg%ldrmm, &
         legacy_pg%rgr3, res, primen, steer, 0)

    identity = cmplx(0.0_dp, 0.0_dp, dp)
    identity(1,1) = cmplx(1.0_dp, 0.0_dp, dp)
    identity(2,2) = cmplx(1.0_dp, 0.0_dp, dp)
    legacy_ok = num_block(1) == 2 .and. &
         maxval(abs(matmul(projmatrix(:,:,1), &
         transpose(conjg(projmatrix(:,:,1)))) - identity)) < tol_projection
    call check("legacy symprj uses shared nonorthogonal atom mapping", &
         legacy_ok, sp, tot)
    call deallocate_point_group_data(legacy_pg)
  end subroutine test_legacy_symprj_nonorthogonal_mapping

  subroutine test_bloch_phase_convention(sp, tot)
    integer, intent(inout) :: sp, tot
    real(dp) :: k_phase(3), lattice_shift(3), nonsymmorphic_shift(3)
    real(dp) :: inv_sqrt_two
    complex(dp) :: phase

    k_phase = [0.5_dp*pi, 0.0_dp, 0.0_dp]
    lattice_shift = [1.0_dp, 0.0_dp, 0.0_dp]
    nonsymmorphic_shift = [0.5_dp, 0.0_dp, 0.0_dp]
    inv_sqrt_two = 1.0_dp / sqrt(2.0_dp)

    phase = bloch_phase(k_phase, lattice_shift)
    call check("Bloch lattice-shift phase uses exp(-ik*n)", &
         abs(phase - cmplx(0.0_dp, -1.0_dp, dp)) < tol_phase, sp, tot)

    phase = bloch_phase(k_phase, lattice_shift, nonsymmorphic_shift)
    call check("Bloch nonsymmorphic phase uses exp(i*k*(tau-n))", &
         abs(phase - cmplx(inv_sqrt_two, -inv_sqrt_two, dp)) < tol_phase, sp, tot)
  end subroutine test_bloch_phase_convention

  subroutine test_spinless_partner_projector_builder(sp, tot)
    integer, intent(inout) :: sp, tot
    complex(dp) :: projector_k(2,2), nonsquare(2,3)
    complex(dp), allocatable :: projector_minus_k(:,:)
    logical :: copy_ok
    real(dp) :: max_residual

    projector_k = reshape([cmplx(0.5_dp, 0.0_dp, dp), &
         cmplx(0.0_dp, 0.5_dp, dp), cmplx(0.0_dp, -0.5_dp, dp), &
         cmplx(0.5_dp, 0.0_dp, dp)], [2,2])
    call build_spinless_partner_projector(projector_k, tol_projection, &
         projector_minus_k, copy_ok, max_residual)
    call check("spinless partner builder validates conjugate projector", &
         copy_ok .and. allocated(projector_minus_k), sp, tot)
    if (allocated(projector_minus_k)) then
       call check("spinless partner builder returns complex conjugate", &
            maxval(abs(projector_minus_k - conjg(projector_k))) < 1.0e-12_dp, sp, tot)
       deallocate(projector_minus_k)
    else
       call check("spinless partner builder returns complex conjugate", .false., sp, tot)
    end if

    nonsquare = cmplx(0.0_dp, 0.0_dp, dp)
    call build_spinless_partner_projector(nonsquare, tol_projection, &
         projector_minus_k, copy_ok, max_residual)
    call check("spinless partner builder rejects nonsquare input", &
         .not. copy_ok .and. .not. allocated(projector_minus_k), sp, tot)
  end subroutine test_spinless_partner_projector_builder


  subroutine test_time_reversal_pairing(sp, tot)
    integer, intent(inout) :: sp, tot

    real(dp) :: kpoints(4,3)
    integer, allocatable :: trim_indices(:)
    integer :: tr_pairs(4), n_trim

    kpoints(1,:) = [0.0_dp, 0.0_dp, 0.0_dp]
    kpoints(2,:) = [0.25_dp, -0.25_dp, 0.0_dp]
    kpoints(3,:) = [-0.25_dp, 0.25_dp, 0.0_dp]
    kpoints(4,:) = [0.125_dp, 0.0_dp, 0.0_dp]

    call find_trim_points(kpoints, 4, trim_indices, n_trim, 1.0e-10_dp)
    call check("TRIM detection finds only Gamma", n_trim == 1 .and. trim_indices(1) == 1, sp, tot)
    call build_time_reversal_pairs(kpoints, 4, tr_pairs, 1.0e-10_dp)
    call check("TR pairing marks TRIM as self-paired", tr_pairs(1) == 1, sp, tot)
    call check("TR pairing links both non-TRIM partners", tr_pairs(2) == 3 .and. tr_pairs(3) == 2, sp, tot)
    call check("TR pairing marks absent partner as unpaired", tr_pairs(4) == -1, sp, tot)
    deallocate(trim_indices)
  end subroutine test_time_reversal_pairing


  subroutine test_character_arithmetic_helpers(sp, tot)
    integer, intent(inout) :: sp, tot

    call check("modulus handles zero", modulus(0, 7) == 0, sp, tot)
    call check("modulus normalizes negative values", modulus(-1, 7) == 6, sp, tot)
    call check("modulus normalizes values above divisor", modulus(15, 7) == 1, sp, tot)
    call check("lcm handles zero", lcm(0, 6) == 0 .and. lcm(6, 0) == 0, sp, tot)
    call check("lcm is nonnegative for signed inputs", lcm(-6, 8) == 24, sp, tot)
  end subroutine test_character_arithmetic_helpers

  subroutine test_block_extraction_input_validation(sp, tot)
    integer, intent(inout) :: sp, tot
    complex(dp) :: projector(2,2)
    type(sympw_block_t), allocatable :: blocks(:)
    integer :: n_blocks

    projector = cmplx(0.0_dp, 0.0_dp, dp)
    call sympw_extract_blocks(projector, 3, tol_projection, n_blocks, blocks)
    call check("block extraction rejects inconsistent matrix dimensions", &
         n_blocks == 0 .and. allocated(blocks) .and. size(blocks) == 0, sp, tot)
  end subroutine test_block_extraction_input_validation

  subroutine test_projector_validation_status(sp, tot)
    integer, intent(inout) :: sp, tot
    complex(dp) :: projector(2,2)
    logical :: valid

    projector = cmplx(0.0_dp, 0.0_dp, dp)
    projector(1,1) = cmplx(1.0_dp, 0.0_dp, dp)
    call validate_projector_matrix(projector, tol_projection, valid)
    call check("projector validator accepts a Hermitian idempotent matrix", valid, sp, tot)

    projector(1,2) = cmplx(0.25_dp, 0.0_dp, dp)
    call validate_projector_matrix(projector, tol_projection, valid)
    call check("projector validator returns failure without stopping", .not. valid, sp, tot)
  end subroutine test_projector_validation_status

  subroutine test_all_point_groups_at_gamma(sp, tot)
    integer, intent(inout) :: sp, tot
    type(sympw_crystal_t) :: group_crystal
    type(sympw_result_t) :: group_result
    integer :: group_number, group_error
    logical :: all_groups_ok, group_ok
    real(dp), parameter :: lattice_length = 5.0_dp
    real(dp) :: gamma(3)

    gamma = 0.0_dp
    all_groups_ok = .true.
    do group_number = 1, 36
       call setup_single_atom(group_crystal, 0)
       group_crystal%pgnr = group_number
       if (group_number >= 16 .and. group_number <= 31) then
          group_crystal%lattice(:, :) = 0.0_dp
          group_crystal%lattice(1, :) = [lattice_length, 0.0_dp, 0.0_dp]
          group_crystal%lattice(2, :) = [-0.5_dp*lattice_length, &
               0.5_dp*sqrt(3.0_dp)*lattice_length, 0.0_dp]
          group_crystal%lattice(3, 3) = lattice_length
       end if

       call sympw_init(group_crystal, group_error)
       group_ok = (group_error == 0)
       if (group_ok) then
          call sympw_analyze_kpoint(gamma, group_result)
          group_ok = group_result%success .and. &
               group_result%little_group_order == npgodat(group_number)
       end if
       if (.not. group_ok) then
          write(*,*) "Point-group Gamma regression failed for group", group_number
       end if
       all_groups_ok = all_groups_ok .and. group_ok
       call sympw_finalize()
       call teardown(group_crystal)
    end do

    call check("all 36 point groups project successfully at Gamma", all_groups_ok, sp, tot)
  end subroutine test_all_point_groups_at_gamma


  subroutine test_library_lifecycle(c, result, sp, tot)
    type(sympw_crystal_t), intent(inout) :: c
    type(sympw_result_t), intent(out) :: result
    integer, intent(inout) :: sp, tot

    real(dp) :: kpoint(3)

    call sympw_finalize()
    kpoint = [0.0_dp, 0.0_dp, 0.0_dp]
    call sympw_analyze_kpoint(kpoint, result)
    call check("uninitialized analysis fails safely", .not. result%success, sp, tot)
    call check("uninitialized analysis has no projector", .not. allocated(result%projector), sp, tot)
    call check("uninitialized analysis reports Mulliken status as not analyzed", &
         result%mulliken_status == SYMPW_MULLIKEN_STATUS_NOT_ANALYZED, sp, tot)

    call setup_single_atom(c, 0)
    call sympw_init(c, error_code)
    call check("initial library initialization succeeds", error_code == 0, sp, tot)
    call sympw_init(c, error_code)
    call check("repeated library initialization succeeds", error_code == 0, sp, tot)
    if (error_code == 0) then
       call sympw_analyze_kpoint(kpoint, result)
       call check("analysis works after repeated initialization", result%success, sp, tot)
    end if
    call sympw_finalize()
    call sympw_analyze_kpoint(kpoint, result)
    call check("finalized analysis fails safely", .not. result%success, sp, tot)
    call teardown(c)
  end subroutine test_library_lifecycle

  subroutine setup_single_atom(c, lmax_value)
    type(sympw_crystal_t), intent(out) :: c
    integer, intent(in) :: lmax_value

    c%lattice(:, :) = 0.0_dp
    c%lattice(1,1) = 5.0_dp
    c%lattice(2,2) = 5.0_dp
    c%lattice(3,3) = 5.0_dp
    c%nel = 1
    c%pgnr = 36
    allocate(c%nat(1), c%lmax(1), c%pos_frac(3,1,1))
    c%nat(1) = 1
    c%lmax(1) = lmax_value
    c%pos_frac(:,1,1) = 0.0_dp
  end subroutine setup_single_atom

  subroutine set_hexagonal_test_lattice(c)
    type(sympw_crystal_t), intent(inout) :: c

    c%lattice(:, :) = 0.0_dp
    c%lattice(1,:) = [5.0_dp, 0.0_dp, 0.0_dp]
    c%lattice(2,:) = [-2.5_dp, 2.5_dp*sqrt(3.0_dp), 0.0_dp]
    c%lattice(3,:) = [0.0_dp, 0.0_dp, 8.0_dp]
  end subroutine set_hexagonal_test_lattice

  subroutine teardown(c)
    type(sympw_crystal_t), intent(inout) :: c
    if (allocated(c%nat)) deallocate(c%nat)
    if (allocated(c%lmax)) deallocate(c%lmax)
    if (allocated(c%pos_frac)) deallocate(c%pos_frac)
  end subroutine teardown

  subroutine cleanup_poscar_inputs()
    if (allocated(elements)) deallocate(elements)
    if (allocated(nat_per_elem)) deallocate(nat_per_elem)
    if (allocated(positions)) deallocate(positions)
  end subroutine cleanup_poscar_inputs

  subroutine write_text_file(path, content)
    character(len=*), intent(in) :: path, content
    integer :: unit_number

    unit_number = 91
    open(unit_number, file=path, status="replace", action="write")
    write(unit_number, '(A)', advance="no") content
    close(unit_number)
  end subroutine write_text_file

  subroutine check(label, condition, sp, tot)
    character(len=*), intent(in) :: label
    logical, intent(in) :: condition
    integer, intent(inout) :: sp, tot

    tot = tot + 1
    if (condition) then
       sp = sp + 1
       write(*,'(A,A)') "PASS: ", trim(label)
    else
       write(*,'(A,A)') "FAIL: ", trim(label)
    end if
  end subroutine check

  real(dp) function determinant3_local(mat) result(det)
    real(dp), intent(in) :: mat(3,3)

    det = mat(1,1) * (mat(2,2) * mat(3,3) - mat(2,3) * mat(3,2)) - &
         mat(1,2) * (mat(2,1) * mat(3,3) - mat(2,3) * mat(3,1)) + &
         mat(1,3) * (mat(2,1) * mat(3,2) - mat(2,2) * mat(3,1))
  end function determinant3_local

  subroutine inverse3_local(mat, inverse)
    real(dp), intent(in) :: mat(3,3)
    real(dp), intent(out) :: inverse(3,3)
    real(dp) :: det

    det = determinant3_local(mat)
    inverse(1,1) = (mat(2,2)*mat(3,3) - mat(2,3)*mat(3,2))/det
    inverse(1,2) = (mat(1,3)*mat(3,2) - mat(1,2)*mat(3,3))/det
    inverse(1,3) = (mat(1,2)*mat(2,3) - mat(1,3)*mat(2,2))/det
    inverse(2,1) = (mat(2,3)*mat(3,1) - mat(2,1)*mat(3,3))/det
    inverse(2,2) = (mat(1,1)*mat(3,3) - mat(1,3)*mat(3,1))/det
    inverse(2,3) = (mat(1,3)*mat(2,1) - mat(1,1)*mat(2,3))/det
    inverse(3,1) = (mat(2,1)*mat(3,2) - mat(2,2)*mat(3,1))/det
    inverse(3,2) = (mat(1,2)*mat(3,1) - mat(1,1)*mat(3,2))/det
    inverse(3,3) = (mat(1,1)*mat(2,2) - mat(1,2)*mat(2,1))/det
  end subroutine inverse3_local

end program test_regressions
