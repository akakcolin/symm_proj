module sumsets
  use accuracy
  use constants
  use sympw_group_mode, only: projective_factor_group_active
  implicit none
  private
  public :: sym_sumsets
  public :: detect_nonprimitive_translations
  
contains
  ! Formation of the summation sets G(MU,NU). nup is the number of projection
  ! matrices for this wave vector. We form np(K,I,J), which is the number
  ! of elements of the group of the k-vector (groupk), for which the difference
  ! rgr*(coordinate of atom I) - (coordinate of atom J) of chemical element K,
  ! is a lattice vector. npl(1:nel,1:nat(I1),1:nat(I1),1:np(1:nel,1:nat(I1),
  ! 1:nat(I1))) gives the indices of these pointgroup operators.
  ! nvec(1:nel,1:nat(I1),1:nat(I1),1:np(1:nel,1:nat(I1),1:nat(I1)),1:3)
  ! gives the corresponding lattice vectors.
  
  subroutine sym_sumsets( np, nvec, npl, til, kgord, kgel, rgr, listp, a, ai, b, r, u, nel, nat, ksym, ibz, steer, &
       success)
    integer, intent(inout) :: np(:,:,:)
    real(dp), intent(inout) :: nvec(:,:,:,:,:)
    integer, intent(inout) :: npl(:,:,:,:)
    real(dp), intent(in) :: rgr(:,:,:)
    real(dp), intent(in) :: til(:,:)
    real(dp), intent(in) :: a(:,:), b(:,:), ai(:,:), r(:,:,:), u(:,:)
    integer, intent(in) :: ksym
    integer, intent(in) :: ibz
    integer, intent(in) :: steer(:)
    integer, intent(in) :: nat(:)
    integer, intent(in) :: nel
    integer, intent(in) :: kgord
    integer, intent(in) :: kgel(:)
    integer, intent(in) :: listp(:)
    logical, intent(out), optional :: success
  

    real(dp), dimension(3) :: trac

    integer :: I, I1, I2, I3, I4, I5, I6, I7
    integer :: J, K
    integer :: match_atom
    integer :: match_count
    real(dp), dimension(3) :: mapped_shift

    if (present(success)) success = .true.

    !allocate(nvec(nel, nat(I1), nat(I1),tmp_dim, 3))

    I = 0
    do while (I < nel)
       I = I + 1
       ! I1 is here the index of the chemical element
       J = nat(I)
       ! JJ is the number of atoms of chemical element I1
       I1 = 0
       do while (I1 < J)
          I1 = I1 + 1
          !I1 is the atom row index
          np(I, I1, 1:J) = 0
          I2 = 0
          do while (I2 < kgord)
             I2 = I2 + 1
             ! I2 is the index of elements of groupk
             I3 = I2

             ! inverse point group operator
             if (projective_factor_group_active(steer(20), ksym, ibz)) then
                I4 = listp(I3)
                I4 = kgel(I4)
                trac(1:3) = matmul(transpose(a(1:3, 1:3)), til(I2,1:3))
                ! nonprimitive translations in cartesian coordinates
             else

                I5 = kgel(I2)
                I4 = kgel(I3)
                trac(1:3) = matmul(transpose(a(1:3, 1:3)), u(I5,1:3))
             end if

             do I6= 1, 3
                do I7 = 1, 3
                   ! space group transformation of the atom position vector
                   trac(I6) = trac(I6) + rgr(I6, I7, I4)*r(I7, I, I1)
                   !write(*,*) "rgr, r", rgr(I6, I7, I4), r(I, I1, I7)
                end do
             end do

             call find_unique_atom_mapping(trac, r, I, J, ai, tol_structure_symmetry, &
                  match_atom, mapped_shift, match_count)

             if (match_count /= 1) then
                if (match_count == 0) then
                   write(*,*) "No atom mapping found", I, I1, I2, I4
                else
                   write(*,*) "Ambiguous atom mapping", I, I1, I2, I4, match_count
                end if
                if (present(success)) then
                   success = .false.
                   return
                end if
                error stop "Invalid atom mapping"
             end if

             np(I, I1, match_atom) = np(I, I1, match_atom) + 1
             K = np(I, I1, match_atom)
             npl(I, I1, match_atom, K) = I2
             nvec(I, I1, match_atom, K, 1:3) = mapped_shift(1:3)
             if (steer(18) .ne. 0) then
                write(*,*) "np=(", I, I1, match_atom, "=", K, " npl=", I2, "nvec=", mapped_shift(1:3)
             end if
          end do
       end do
    end do

  end subroutine sym_sumsets

  subroutine find_unique_atom_mapping(trac, atom_positions, element_index, nat_this, &
       ai, tol, match_atom, mapped_shift, match_count)
    real(dp), intent(in) :: trac(3)
    integer, intent(in) :: element_index, nat_this
    real(dp), intent(in) :: atom_positions(:,:,:)
    real(dp), intent(in) :: ai(3,3)
    real(dp), intent(in) :: tol
    integer, intent(out) :: match_atom
    real(dp), intent(out) :: mapped_shift(3)
    integer, intent(out) :: match_count

    integer :: atom_index, component
    real(dp) :: dif(3)
    real(dp) :: candidate_shift(3)
    logical :: is_match

    match_atom = 0
    match_count = 0
    mapped_shift(:) = 0
    if (size(atom_positions, 1) < 3 .or. element_index < 1 .or. &
         element_index > size(atom_positions, 2) .or. nat_this < 1 .or. &
         nat_this > size(atom_positions, 3)) return

    do atom_index = 1, nat_this
       dif(:) = trac(:) - atom_positions(:, element_index, atom_index)
       candidate_shift(:) = 0
       is_match = .true.

       candidate_shift(:) = matmul(ai, dif)
      do component = 1, 3
          if (abs(candidate_shift(component) - nint(candidate_shift(component))) > tol) then
             is_match = .false.
             exit
          end if
       end do

       if (is_match) then
          match_count = match_count + 1
          if (match_count == 1) then
             match_atom = atom_index
             mapped_shift(:) = nint(candidate_shift(:))
          end if
       end if
    end do
  end subroutine find_unique_atom_mapping

  ! ============================================
  ! Auto-detect non-primitive translations for
  ! each point group operation, such that {R|τ}
  ! maps the crystal structure onto itself.
  !
  ! Algorithm (loop over reference atoms):
  !   For each atom as reference:
  !     1. For each g: get τ from frac(a^{-1}·(r_j - R·r_ref))
  !        choosing j such that τ is minimal
  !     2. Verify τ works for all atoms via verify_tau
  !     3. Verify group cocycle condition:
  !        τ(g1·g2) ≡ τ(g1) + R(g1)·τ(g2)  (mod 1)
  !     4. If all pass, accept and return
  !   If no reference atom yields consistent τ:
  !     error — structure likely a supercell lacking
  !     the assumed point group symmetry
  !
  ! The identity operation always has τ = 0.
  ! ============================================
  subroutine detect_nonprimitive_translations(u, r, a, ai, rgr3, gel, order, pgnr, nel, nat, success)
    real(dp), intent(out) :: u(:,:)
    real(dp), intent(inout) :: r(:,:,:)
    real(dp), intent(in)  :: a(3,3)
    real(dp), intent(in)  :: ai(3,3)
    real(dp), intent(in)  :: rgr3(3,3,72)
    integer,  intent(in)  :: gel(:)
    integer,  intent(in)  :: order
    integer,  intent(in)  :: pgnr
    integer,  intent(in)  :: nel
    integer,  intent(in)  :: nat(:)
    logical, intent(out), optional :: success

    integer  :: ig, ielem, iatom, jatom, ref_atom
    real(dp) :: r_ref(3), r_rot(3), f_rot(3), f_j(3), f_diff(3), tau(3)
    real(dp) :: origin_shift(3), origin_shift_cart(3)
    integer  :: rt_idx
    real(dp) :: rot(3,3)
    real(dp), allocatable :: rot_all(:,:,:), u_try(:,:)
    logical  :: found, cocycle_ok, origin_found
    integer  :: max_nat, num_ref_atoms

    if (present(success)) success = .true.
    u(:,:) = 0.0_dp

    ! Identity (position 1) always has zero translation
    if (order < 2) return

    max_nat = maxval(nat(1:nel))

    ! Pre-compute rotation matrices for all group elements
    allocate(rot_all(3, 3, order))
    do ig = 1, order
       rt_idx = rotation_table_index(gel(ig), pgnr)
       rot_all(:,:,ig) = rgr3(:,:,rt_idx)
    end do

    allocate(u_try(order, 3))

    ! Number of reference atoms to try (all atoms of first element)
    num_ref_atoms = nat(1)

    do ref_atom = 1, num_ref_atoms
       r_ref(:) = r(:, 1, ref_atom)
       u_try(:,:) = 0.0_dp

       do ig = 1, order
          rot(:,:) = rot_all(:,:,ig)

          ! Apply rotation in Cartesian: R · r_ref
          r_rot = matmul(rot, r_ref)

          ! Convert to fractional: a^{-1} · r_rot
          f_rot = matmul(ai, r_rot)

          found = .false.

          ! Try each atom as the target of the reference,
          ! preferring smaller τ first (jatom=ref_atom first for τ≈0)
          do jatom = 1, nat(1)
             f_j = matmul(ai, r(:, 1, jatom))
             f_diff(:) = f_j(:) - f_rot(:)

             ! Non-primitive part: fractional part in [0, 1)
             tau(:) = f_diff(:) - floor(f_diff(:))

             if (verify_tau(tau, rot, a, ai, r, nel, nat)) then
                u_try(ig, :) = tau(:)
                found = .true.
                exit
             end if
          end do

          if (.not. found) then
             ! Exit the ref_atom loop early — this reference fails
             exit
          end if
       end do

       if (.not. found) cycle  ! Try next reference atom

       ! Each {R|tau} has been verified against every atom. The set is
       ! physically usable only if the operations also satisfy the space-group
       ! cocycle relation under multiplication.
       cocycle_ok = check_cocycle(u_try, rot_all, a, ai, order, tol_structure_symmetry)
       if (cocycle_ok) then
          call find_symmorphic_origin(u_try, rot_all, a, ai, order, &
               tol_structure_symmetry, origin_shift, origin_found)
          if (origin_found) then
             origin_shift_cart = matmul(transpose(a), origin_shift)
             do ielem = 1, nel
                do iatom = 1, nat(ielem)
                   r(:, ielem, iatom) = r(:, ielem, iatom) - origin_shift_cart
                end do
             end do
             u(:,:) = 0.0_dp
          else
             u(:,:) = u_try(:,:)
          end if
          deallocate(rot_all, u_try)
          return
       end if
    end do

    ! No reference atom gave consistent τ values
    deallocate(rot_all, u_try)

    if (present(success)) then
       success = .false.
       return
    end if

    write(*,*) ""
    write(*,*) "=========================================="
    write(*,*) "ERROR: No consistent non-primitive translations found"
    write(*,*) "=========================================="
    write(*,*) "The atomic structure does not have the assumed point group"
    write(*,*) "symmetry (PG ", pgnr, ") with this choice of unit cell."
    write(*,*) ""
    write(*,*) "Possible causes:"
    write(*,*) "  1. The input is an arbitrary supercell or non-standard centered"
    write(*,*) "     cell that cannot be reduced by the built-in A/B/C/I/F detector."
    write(*,*) "     Use a primitive cell, or a standard centered conventional cell"
    write(*,*) "     whose centering translations are visible in the atomic basis."
    write(*,*) "  2. The atomic positions are inconsistent with the detected"
    write(*,*) "     point group symmetry."
    write(*,*) "  3. The lattice vectors (POSCAR/CONTCAR) do not match the"
    write(*,*) "     crystallographic convention expected for this structure."
    write(*,*) ""
    write(*,*) "  Number of atoms of first element:", nat(1)
    write(*,*) "  Detected point group:", pgnr
    write(*,*) "  Group order:", order
    write(*,*) ""
    write(*,*) "  To fix: prefer the primitive cell. Standard centered conventional"
    write(*,*) "  A/B/C/I/F cells are reduced automatically when their centering"
    write(*,*) "  translations are detectable from the basis."
    write(*,*) "=========================================="
    error stop "Inconsistent non-primitive translations"

  contains

    ! Verify that τ works for all atoms: for each atom i of each
    ! element, R·r_i + a·τ matches some atom j modulo lattice.
    logical function verify_tau(tau_vec, rot_mat, a_lat, ai_lat, r_pos, nel_in, nat_in) result(ok)
      real(dp), intent(in) :: tau_vec(3)
      real(dp), intent(in) :: rot_mat(3,3)
      real(dp), intent(in) :: a_lat(3,3)
      real(dp), intent(in) :: ai_lat(3,3)
      real(dp), intent(in) :: r_pos(:,:,:)
      integer,  intent(in) :: nel_in
      integer,  intent(in) :: nat_in(:)

      integer  :: el, ia, ja, match_cnt
      real(dp) :: r_img(3), f_img(3), f_target(3), diff_frac(3)

      ok = .true.
      do el = 1, nel_in
         do ia = 1, nat_in(el)
            r_img(:) = matmul(rot_mat, r_pos(:, el, ia)) + matmul(transpose(a_lat), tau_vec)
            f_img = matmul(ai_lat, r_img)

            match_cnt = 0
            do ja = 1, nat_in(el)
               f_target = matmul(ai_lat, r_pos(:, el, ja))
               diff_frac(:) = f_img(:) - f_target(:)
               if (all(abs(diff_frac - nint(diff_frac)) < tol_structure_symmetry)) then
                  match_cnt = match_cnt + 1
               end if
            end do

            if (match_cnt == 0) then
               ok = .false.
               return
            end if
         end do
      end do
    end function verify_tau

    ! Verify group cocycle condition for all pairs (i,j):
    !   τ(i·j) ≡ τ(i) + R(i)·τ(j)  (mod 1)
    !
    ! First builds a local multiplication table by matching
    ! rotation matrices, then checks the cocycle equation.
    logical function check_cocycle(u_loc, rot_loc, a_lat, ai_lat, ord, tol) result(ok)
      real(dp), intent(in) :: u_loc(:,:)
      real(dp), intent(in) :: rot_loc(:,:,:)
      real(dp), intent(in) :: a_lat(3,3)
      real(dp), intent(in) :: ai_lat(3,3)
      integer,  intent(in) :: ord
      real(dp), intent(in) :: tol

      integer  :: i, j, k
      integer, allocatable :: mtab_loc(:,:)
      real(dp) :: rot_prod(3,3), rot_diff(3,3)
      real(dp) :: expected(3), actual(3), diff(3)
      real(dp) :: rj_tau(3)
      logical  :: found_k

      allocate(mtab_loc(ord, ord))
      mtab_loc(:,:) = 0

      ! Build multiplication table from rotation matrices
      do i = 1, ord
         do j = 1, ord
            rot_prod = matmul(rot_loc(:,:,i), rot_loc(:,:,j))
            found_k = .false.
            do k = 1, ord
               rot_diff = rot_prod - rot_loc(:,:,k)
               if (maxval(abs(rot_diff)) < tol_rotation_match) then
                  mtab_loc(i, j) = k
                  found_k = .true.
                  exit
               end if
            end do
            if (.not. found_k) then
               ok = .false.
               deallocate(mtab_loc)
               return
            end if
         end do
      end do

      ! Check cocycle: τ(mtab(i,j)) ≡ τ(i) + R_frac(i)·τ(j)  (mod 1)
      ! where R_frac = ai · R_cart · a^T  is the rotation expressed
      ! in the fractional basis (so it acts directly on fractional τ).
      ! Equivalently: convert τ(j) to Cartesian, rotate, convert back.
      ok = .true.
      do i = 1, ord
         do j = 1, ord
            k = mtab_loc(i, j)
            ! τ(j) in Cartesian: a^T · u(j)
            ! rotated: R_cart(i) · a^T · u(j)
            ! back to fractional: ai · R_cart(i) · a^T · u(j)
            rj_tau = matmul(ai_lat, matmul(rot_loc(:,:,i), matmul(transpose(a_lat), u_loc(j,:))))
            expected(:) = u_loc(i,:) + rj_tau(:)
            actual(:) = u_loc(k,:)
            diff = expected - actual
            if (any(abs(diff - nint(diff)) > tol)) then
               ok = .false.
               deallocate(mtab_loc)
               return
            end if
         end do
      end do

      deallocate(mtab_loc)
    end function check_cocycle

    subroutine find_symmorphic_origin(u_loc, rot_loc, a_lat, ai_lat, ord, tol, origin, found_origin)
      real(dp), intent(in) :: u_loc(:,:)
      real(dp), intent(in) :: rot_loc(:,:,:)
      real(dp), intent(in) :: a_lat(3,3), ai_lat(3,3)
      integer, intent(in) :: ord
      real(dp), intent(in) :: tol
      real(dp), intent(out) :: origin(3)
      logical, intent(out) :: found_origin

      ! Denominator 24 covers crystallographic origin shifts generated by 2-, 3-, 4-, and 6-fold operations.
      integer, parameter :: origin_grid = 24
      integer :: group_index, ix, iy, iz
      real(dp) :: rotation_fractional(3,3), shifted_translation(3)

      found_origin = .false.
      origin(:) = 0.0_dp
      do ix = 0, origin_grid - 1
         do iy = 0, origin_grid - 1
            do iz = 0, origin_grid - 1
               origin = real([ix, iy, iz], kind=dp)/real(origin_grid, kind=dp)
               do group_index = 1, ord
                  rotation_fractional = matmul(ai_lat, &
                       matmul(rot_loc(:,:,group_index), transpose(a_lat)))
                  shifted_translation = u_loc(group_index,:) + &
                       matmul(rotation_fractional, origin) - origin
                  if (any(abs(shifted_translation - nint(shifted_translation)) > tol)) exit
               end do
               if (group_index > ord) then
                  found_origin = .true.
                  return
               end if
            end do
         end do
      end do
    end subroutine find_symmorphic_origin

  end subroutine detect_nonprimitive_translations

end module sumsets
