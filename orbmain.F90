program main
  use accuracy
  use constants
  use groupkp
  use irrep
  use sumsets
  use projmat
  use genera
  use vasp_reader
  use time_reversal
  use time_reversal_optimization
  use sympw_pointgroup_data
  use sympw_core
  use sympw_real_sh, only: complex_to_real_projector
  implicit none

  integer :: I, J, K, K1, K2, IV
  integer :: ikp
  integer :: nel, pgnr
  integer :: order, first
  integer, dimension(100) :: npri
  integer, allocatable :: gel(:)
  real(dp), allocatable :: u(:,:)

  integer :: number_of_wave_vectors
  real(dp), dimension(3) :: rk
  integer, allocatable :: kkgel(:)

  real(dp), dimension(3,3) :: a, b, bi, ai

  real(dp) :: T
  integer :: atco

  integer, allocatable :: mtab(:,:)
  integer, allocatable :: mtab2(:,:)
  integer :: G

  integer, dimension(20) :: steer
  integer :: debug

  integer, allocatable :: lmax(:)
  integer, allocatable :: nat(:)
  real(dp), allocatable :: r(:,:,:)

  real(dp) :: tsmall, ttsmall
  real(dp), dimension(3) :: tsk

  integer :: ksym, ntz, ibz
  integer, allocatable :: nopli1(:)
  integer, allocatable :: nopli(:,:), nopi(:)
  integer :: nopi1
  integer, allocatable :: allow(:)
  real(dp), allocatable :: jdpk(:,:)
  integer :: ncl
  integer, allocatable :: laj(:)
  integer, allocatable :: np(:,:,:)
  complex(dp), allocatable :: jpdd(:,:,:)
  real(dp), allocatable :: jdprod(:,:)
  real(dp), allocatable :: all_kpoints(:,:)
  integer, allocatable :: npl(:,:,:,:)
  complex(dp), allocatable :: projmatrix(:,:,:)
  integer :: matrixorder

  character(len=256) :: infile, arg
  integer :: stat
  logical :: use_vasp_format
  logical :: use_config_file, auto_point_group, kpoints_cartesian
  logical :: cell_reduced
  character(len=256) :: poscar_file, kpoints_file, comment_line
  real(dp) :: scale_factor
  real(dp) :: k_transform(3,3)
  character(len=2), allocatable :: elements(:)
  integer, allocatable :: nat_vasp(:)
  real(dp), allocatable :: positions_vasp(:,:)
  logical :: is_cartesian
  integer :: nel_vasp, total_atoms_vasp, atom_idx
  real(dp), allocatable :: kpoints_vasp(:,:)
  character(len=20), allocatable :: kpoint_names(:)
  integer :: nkpts_vasp
  character(len=20) :: kpt_mode
  character(len=10) :: pg_name

  ! Time-reversal symmetry variables
  integer, allocatable :: trim_indices(:), tr_pairs(:)
  integer :: n_trim, ik_tr
  logical :: is_trim
  logical, allocatable :: should_compute(:)
  integer, allocatable :: source_kpoint(:)
  integer :: n_computed, n_skipped

  ! Point group data container
  type(pg_data_t) :: pg

  integer, parameter :: fh = 15
  integer :: matrix_order_per_kpt
  logical :: kpt_success
  complex(dp), allocatable :: kpt_projmatrix(:,:)

  debug = 0
  nopi1 = 1
  ksym = 1
  ntz = 0

  call get_command_argument(number=1, value=infile, status=stat)

  pg_name = ""
  kpoints_cartesian = .false.
  use_config_file = (index(infile, '.conf') > 0 .or. index(infile, '.cfg') > 0)
  use_vasp_format = (use_config_file .or. index(infile, 'POSCAR') > 0 .or. &
       index(infile, 'CONTCAR') > 0)

  if (use_vasp_format) then
     ! ============================================
     ! VASP format input
     ! ============================================
     write(*,*) "=========================================="
     write(*,*) "Using VASP format input"
     write(*,*) "=========================================="

     if (use_config_file) then
        call read_config_file(infile, poscar_file, kpoints_file, lmax, pg_name)
        if (len_trim(poscar_file) == 0 .or. len_trim(kpoints_file) == 0) then
           write(*,*) "Error: POSCAR and KPOINTS must be specified in config file"
           error stop
        end if
     else
        poscar_file = infile
        if (command_argument_count() >= 2) then
           call get_command_argument(2, kpoints_file)
        else
           kpoints_file = "KPOINTS"
        end if
     end if

     call read_poscar(poscar_file, comment_line, scale_factor, a, elements, &
                     nat_vasp, positions_vasp, is_cartesian, nel_vasp, total_atoms_vasp)

     nel = nel_vasp

     if (.not. allocated(lmax)) then
        allocate(lmax(nel))
        if ((.not. use_config_file) .and. command_argument_count() >= 2 + nel) then
           do I = 1, nel
              call get_command_argument(2 + I, arg)
              read(arg, *) lmax(I)
           end do
        else
           lmax(:) = 2
           write(*,*) "Using default lmax = 2 for all elements"
        end if
     else if (size(lmax) == 1 .and. nel > 1) then
        K = lmax(1)
        deallocate(lmax)
        allocate(lmax(nel))
        lmax(:) = K
     else if (size(lmax) /= nel) then
        write(*,*) "Error: LMAX count must be 1 or match number of elements"
        error stop
     end if

     call read_kpoints(kpoints_file, kpoints_vasp, kpoint_names, nkpts_vasp, kpt_mode, kpoints_cartesian)
     number_of_wave_vectors = nkpts_vasp
     allocate(all_kpoints(number_of_wave_vectors, 3))
     all_kpoints = kpoints_vasp

     if (is_cartesian) then
        write(*,*) "Converting Cartesian to fractional coordinates..."
        bi = a
        call sym_matinv(bi, 3)
        do I = 1, total_atoms_vasp
           positions_vasp(I,:) = matmul(positions_vasp(I,:), bi)
        end do
     end if

     call reduce_centered_cell(a, positions_vasp, nat_vasp, nel_vasp, &
          total_atoms_vasp, k_transform, cell_reduced, verbosity=1)
     if (cell_reduced .and. (.not. kpoints_cartesian)) then
        write(*,*) "Converting reciprocal fractional KPOINTS to primitive basis..."
        do I = 1, number_of_wave_vectors
           all_kpoints(I, :) = matmul(k_transform, all_kpoints(I, :))
        end do
     end if

     allocate(nat(nel))
     nat = nat_vasp

     auto_point_group = (len_trim(pg_name) == 0)
     if (auto_point_group) then
        pg_name = detect_point_group(a)
        write(*,*) "Lattice-only point group guess: ", trim(pg_name)
     else
        write(*,*) "Using specified point group: ", trim(pg_name)
     end if
     pgnr = point_group_name_to_number(pg_name)
     write(*,*) "Point group number: ", pgnr

     allocate(r(3, nel, maxval(nat)))
     atom_idx = 1
     do I = 1, nel
        do J = 1, nat(I)
           r(:, I, J) = positions_vasp(atom_idx, :)
           atom_idx = atom_idx + 1
        end do
     end do

     ! Convert fractional to Cartesian (required by symmetry engine)
     do I = 1, nel
        do J = 1, nat(I)
           tsk(1:3) = r(:, I, J)
           r(:, I, J) = matmul(transpose(a), tsk)
        end do
     end do

     deallocate(nat_vasp, positions_vasp, elements, kpoints_vasp, kpoint_names)

     atco = 0
     tsmall = tol_equal
     ttsmall = tol_projection
     steer(:) = 0
     steer(2) = 1
     steer(11) = 1

     write(*,*) "Lattice vectors (Angstrom):"
     do I = 1, 3
        write(*,'(3F12.6)') a(I,:)
     end do

     T = 2*pi
     b(:,:) = a(:,:)
     call sym_matinv(b, 3)
     bi = transpose(a)
     ai = transpose(b)

     if (kpoints_cartesian) then
        write(*,*) "Converting Cartesian KPOINTS to reciprocal fractional coordinates..."
        do I = 1, number_of_wave_vectors
           all_kpoints(I, :) = matmul(all_kpoints(I, :), bi)
        end do
     end if

     write(*,*) "Reciprocal lattice vectors (1/Angstrom):"
     do I = 1, 3
        write(*,'(3F12.6)') b(I, :)
     end do
     write(*,*) 'Chemical elements:', nel
     write(*,*) "Maximum L quantum number:", lmax(1)

  else
     ! ============================================
     ! Original format input
     ! ============================================
     open(fh, file=infile, status='OLD', action='read')

     read(fh, *) steer(:)

     do I = 1, 3
        read(fh, *) a(I,1), a(I,2), a(I,3)
     end do

     write(*,*) "Crystal Structure Input"
     write(*,*) "Unit cell vectors (Cartesian):"
     do I = 1, 3
        write(*,'(A,I1,A,3F12.6)') "  a", I, " = ", a(I,:)
     end do

     T = 2*pi
     b(:,:) = a(:,:)
     call sym_matinv(b, 3)
     bi = transpose(a)
     ai = transpose(b)

     write(*,*) "Reciprocal unit cell vectors:"
     do I = 1, 3
        write(*,'(A,I1,A,3F12.6)') "  b", I, " = ", b(I, :)
     end do

     read(fh, *) pgnr
     read(fh, *) nel
     allocate(lmax(nel))
     allocate(nat(nel))

     do I = 1, nel
        read(fh, *) lmax(I)
     end do

     tsmall = tol_equal
     ttsmall = tol_projection

     do I = 1, nel
        read(fh,*) nat(I)
     end do

     allocate(r(3, nel, maxval(nat)))
     do I = 1, nel
        K = nat(I)
        do J = 1, K
           read(fh, *) atco
           read(fh, *) r(:, I, J)
           if (atco /= 1) then
              tsk(1:3) = r(:, I, J)
              r(:, I, J) = matmul(transpose(a(:,:)), tsk)
           end if
        end do
     end do
  end if

  ! ============================================
  ! Initialize point group data
  ! ============================================
  write(*,*)
  write(*,*) "=========================================="
  write(*,*) "Initializing Point Group Data"
  write(*,*) "=========================================="

  call init_point_group_data(pg, debug)

  if (use_vasp_format .and. auto_point_group) then
     pgnr = detect_structure_point_group(a, ai, r, nel, nat, pg)
     pg_name = point_group_number_to_name(pgnr)
     write(*,*) "Structure-verified point group: ", trim(pg_name), "  Number: ", pgnr
  else if (use_vasp_format) then
     write(*,*) "Structure point group supplied by input: ", trim(pg_name), "  Number: ", pgnr
  end if

  if (steer(12) /= 0) then
     write(*,*) "Prime Numbers (first 100):"
     do I = 1, 100, 10
        write(*,'(10I7)') primen(I:min(I+9, 100))
     end do
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
  if (use_vasp_format) then
     call detect_nonprimitive_translations(u, r, a, ai, pg%rgr3, gel, order, pgnr, nel, nat)
     if (any(abs(u) > tol_zero)) then
        write(*,*) "Detected nonprimitive translations; factor-group path will be used on boundary k-points"
     else
        write(*,*) "No nonprimitive translations detected; using ordinary little co-group path"
     end if
  else
     u(:,:) = 0
     if (steer(20) == 0) then
        do I = 1, order
           read(fh, *) atco
           read(fh, *) u(I, :)
           if (atco /= 1) then
              tsk(1:3) = u(I, 1:3)
              u(I, 1:3) = matmul(ai, tsk)
           end if
        end do
     end if
  end if

  write(*,*)
  write(*,*) "Point Group Identification"
  write(*,'(A,I3)') " Point group number: ", pgnr
  if ((pgnr >= 16) .and. (pgnr <= 31)) then
     allocate(mtab(24, 24))
     mtab(:,:) = 0
     mtab(:,:) = pg%MD6h(:,:)
     write(*,*) "This point group is a subgroup of D6h"
  else if (pgnr == 2) then
     ! Ci: uses D6h parent where element 13 is pure inversion
     allocate(mtab(24, 24))
     mtab(:,:) = 0
     mtab(:,:) = pg%MD6h(:,:)
     write(*,*) "This point group is a subgroup of D6h"
  else
     allocate(mtab(48, 48))
     mtab(:,:) = 0
     mtab(:,:) = pg%MOh(:,:)
     write(*,*) "This point group is a subgroup of Oh"
  end if

  if ((pgnr /= 31) .and. (pgnr /= 36)) then
     write(*,*) "(Extracting subgroup elements from parent group)"
     allocate(kkgel(maxval(gel)))
     kkgel(:) = 0
     do I = 1, order
        kkgel(gel(I)) = I
     end do
     do I = 1, order
        do J = 1, order
           mtab(I, J) = mtab(gel(I), gel(J))
        end do
     end do
     do I = 1, order
        do J = 1, order
           mtab(I, J) = kkgel(mtab(I, J))
        end do
     end do
     deallocate(kkgel)
  end if

  write(*,'(A,I3,A)') " Total symmetry operations: ", order
  if (steer(12) /= 0) then
     write(*,*) "Group elements:"
     do I = 1, order, 12
        write(*,'(12I6)') gel(I:min(I+11, order))
     end do
  end if
  write(*,*) "Orbital basis: L_max =", lmax(:)

  allocate(mtab2(order, order))
  mtab2(:,:) = 0

  if (use_vasp_format) then
     do I = 1, number_of_wave_vectors
        write(*,*) "k-point", all_kpoints(I,:)
     end do
  end if

  ! Compute total matrix dimension
  matrixorder = 0
  do I = 1, nel
     K = 0
     do J = 0, lmax(I)
        K = K + 2*J + 1
     end do
     matrixorder = matrixorder + nat(I)*K
  end do
  write(*,'(A,I5)') " Total basis function dimension: ", matrixorder

  allocate(projmatrix(number_of_wave_vectors, matrixorder, matrixorder))
  projmatrix(:,:,:) = 0

  ! ============================================
  ! Time-reversal analysis
  ! ============================================
  write(*,*) "Processing ", number_of_wave_vectors, " k-point(s)..."

  call find_trim_points(all_kpoints, number_of_wave_vectors, &
                        trim_indices, n_trim, tsmall)

  allocate(tr_pairs(number_of_wave_vectors))
  call build_time_reversal_pairs(all_kpoints, number_of_wave_vectors, &
                                  tr_pairs, tsmall)

  write(*,*) "Time-Reversal Symmetry Analysis"
  write(*,'(A,I3,A,I3)') " Found ", n_trim, " TRIM points out of ", number_of_wave_vectors
  if (n_trim > 0) then
     write(*,*) "TRIM point indices:"
     do ik_tr = 1, n_trim, 12
        write(*,'(12I6)') trim_indices(ik_tr:min(ik_tr+11, n_trim))
     end do
  end if

  write(*,*) "Time-reversal k-point pairing:"
  do ik_tr = 1, number_of_wave_vectors
     if (tr_pairs(ik_tr) == ik_tr) then
        write(*,'(A,I3,A,3F10.5,A)') "  k", ik_tr, " (", &
             all_kpoints(ik_tr,:), ") is a TRIM point"
     else if (tr_pairs(ik_tr) > ik_tr) then
        write(*,'(A,I3,A,I3,A)') "  k", ik_tr, " <--> k", tr_pairs(ik_tr), &
             " (time-reversal partners)"
     end if
  end do

  allocate(should_compute(number_of_wave_vectors))
  allocate(source_kpoint(number_of_wave_vectors))

  call mark_kpoints_to_compute(tr_pairs, number_of_wave_vectors, &
                                should_compute, source_kpoint, &
                                n_computed, n_skipped, n_trim)

  call print_tr_optimization_summary(number_of_wave_vectors, n_computed, &
                                      n_skipped, n_trim, should_compute, &
                                      source_kpoint, tr_pairs)

  ! ============================================
  ! K-point loop - using sympw_core
  ! ============================================
  do ikp = 1, number_of_wave_vectors
     rk(1:3) = all_kpoints(ikp,:)

     is_trim = is_time_reversal_invariant_point(all_kpoints(ikp,:), tsmall)
     if (is_trim) then
        write(*,*) "*** This is a time-reversal invariant point (TRIM) ***"
     end if

     call sympw_compute_kpoint(rk, a, ai, b, bi, nel, nat, lmax, order, r, u, &
          pgnr, pg%rgr3, pg%ldrmm, mtab, gel, steer, npri, tsmall, ttsmall, &
          ikp, matrix_order_per_kpt, kpt_projmatrix, kpt_success, verbosity=1)

     if (kpt_success) then
        if (matrix_order_per_kpt == matrixorder) then
           ! Compute full projector in complex spherical-harmonic basis: P = T * T^H
           kpt_projmatrix(:, :) = matmul(kpt_projmatrix(:, :), transpose(conjg(kpt_projmatrix(:, :))))
           ! Transform to real spherical-harmonic basis (DFTB+ / Slater-Koster convention)
           call complex_to_real_projector(kpt_projmatrix, lmax, nat, projmatrix(ikp, :, :))
        end if
        deallocate(kpt_projmatrix)

        ! Display projection matrix only when explicitly requested.
        if (steer(1) /= 0 .and. matrixorder <= 60) then
           write(*,*) "Projection Matrix Result"
           write(*,'(A,I3,A,3F8.4)') " K-point ", ikp, ": ", all_kpoints(ikp,:)
           write(*,'(A,I5,A,I5)') " Matrix dimension: ", matrixorder, " x ", matrixorder

           write(*,'(A6)', advance='no') "Row"
           do K1 = 1, min(matrixorder, 6)
              write(*,'(A16,I4)', advance='no') "Col ", K1
           end do
           write(*,*)
           write(*,*) repeat("-", 6 + min(matrixorder, 6)*20)

           do K2 = 1, matrixorder, 6
              if (K2 > 1) then
                 write(*,*)
                 write(*,'(A6)', advance='no') "Row"
                 do K1 = K2, min(K2+5, matrixorder)
                    write(*,'(A16,I4)', advance='no') "Col ", K1
                 end do
                 write(*,*)
                 write(*,*) repeat("-", 6 + min(6, matrixorder-K2+1)*20)
              end if
              do I = 1, matrixorder
                 write(*,'(I6)', advance='no') I
                 do K1 = K2, min(K2+5, matrixorder)
                    write(*,'(A1,F8.4,A1,F8.4,A1)', advance='no') &
                         "(", real(projmatrix(ikp, I, K1)), ",", aimag(projmatrix(ikp, I, K1)), ")"
                 end do
                 write(*,*)
              end do
           end do
           write(*,*)
        end if
     end if
  end do

  ! ============================================
  ! Cleanup
  ! ============================================
  deallocate(r, lmax, nat)
  deallocate(gel)
  deallocate(mtab2, mtab)
  deallocate(all_kpoints)
  deallocate(u)
  deallocate(projmatrix)
  deallocate(trim_indices, tr_pairs)
  deallocate(should_compute, source_kpoint)
  call deallocate_point_group_data(pg)

  if (.not. use_vasp_format) then
     close(fh)
  end if

contains

  subroutine read_config_file(filename, poscar, kpoints_file_out, lmax_out, pg)
    character(len=*), intent(in) :: filename
    character(len=*), intent(out) :: poscar, kpoints_file_out, pg
    integer, allocatable, intent(out) :: lmax_out(:)

    integer :: fh_cfg, ios_cfg, nlmax, i_cfg, comment_pos
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

       comment_pos = index(line, '#')
       if (comment_pos > 0) line = line(:comment_pos-1)
       line = adjustl(line)
       if (len_trim(line) == 0) cycle

       read(line, *) keyword
       select case(trim(keyword))
       case('POSCAR', 'POSCAR_FILE')
          read(line, *) keyword, poscar
       case('KPOINTS', 'KPOINTS_FILE')
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

    call resolve_config_path(filename, poscar)
    call resolve_config_path(filename, kpoints_file_out)
    deallocate(temp_lmax)
  end subroutine read_config_file

  subroutine resolve_config_path(config_filename, path_value)
    character(len=*), intent(in) :: config_filename
    character(len=*), intent(inout) :: path_value

    integer :: i_path, slash_pos

    if (len_trim(path_value) == 0) return
    if (path_value(1:1) == '/') return

    slash_pos = 0
    do i_path = 1, len_trim(config_filename)
       if (config_filename(i_path:i_path) == '/') slash_pos = i_path
    end do

    if (slash_pos > 0) then
       path_value = config_filename(1:slash_pos) // trim(path_value)
    end if
  end subroutine resolve_config_path

end program main
