module groupkp 
  use accuracy
  use constants
  use bztest 
  use sympw_phase, only: bloch_phase
  implicit none
  private
  public :: sym_groupkp

contains

  subroutine sym_groupkp(kg, kgord, k2gord, kgel, kkgel, mtab2, ibz, listp, &
       & nopi, nopi1, nopli, nopli1, sil, til, ksym, rk, ark, a, ai, b,bi, u, order,pgnr, &
       & rgr, mtab, gel, steer, tsmall, factor_group_ok)
    integer, intent(inout) :: kg, kgord
    integer, intent(out) :: k2gord
    integer, intent(inout) :: kgel(:)
    integer, intent(inout) :: kkgel(:)
    integer, intent(inout) :: mtab2(:,:)
    integer, intent(out) :: ibz
    integer, intent(out) :: listp(:)

    integer, intent(out) :: nopi(:)
    integer, intent(out) :: nopi1
    integer, intent(out) :: nopli(:,:) 
    integer, intent(out) :: nopli1(:)
    complex(dp), intent(out) :: sil(:)
    real(dp), intent(out) :: til(:,:)
    integer, intent(out)  :: ksym

    real(dp), intent(in) :: rk(:)
    real(dp), intent(in) :: ark(:)
    real(dp), intent(in) :: a(:,:)
    real(dp), intent(in) :: ai(:,:)
    real(dp), intent(in) :: b(:,:)
    real(dp), intent(in) :: bi(:,:)
    real(dp), intent(in) :: u(:,:) ! (3, order)

    integer, intent(in) :: order
    integer, intent(in) :: pgnr
    real(dp), intent(in) :: rgr(:,:,:)
    integer, intent(in) :: mtab(:,:)
    integer, intent(in) :: gel(:)
    integer, intent(in) :: steer(:) ! steer(20)

    real(dp), intent(in) :: tsmall
    logical, intent(out) :: factor_group_ok

    real(dp), dimension(3) :: srk
    integer :: II, K, I, J, K1, K2, I1
    real(dp), dimension(3) :: brk, robrk, rosk
    real(dp) :: TT, TTT, membership_tol
    integer :: ntz
    integer, allocatable :: inverk(:)
    integer, allocatable :: tmp_kgel(:)

    !rk(1:3) = ark(1:3)*2*pi
    srk(1:3) = ark(1:3)
    if (steer(18) == 1) then
       write(*,*)
       write(*,*) "=========================================="
       write(*,*) "K-point Group Analysis"
       write(*,*) "=========================================="
       write(*,'(A,I6)') " Group order: ", order
       write(*,*)
    end if
    allocate(inverk(order))
    allocate(tmp_kgel(order))  ! Use actual group order instead of hardcoded 230
    inverk(:) = 0
    factor_group_ok = .true.
    membership_tol = max(tsmall, tol_kpoint_membership)
    ! section 2.3
    kgord = 1
    tmp_kgel(1) = 1

    !todo check 
    ! brk is the wave vector in cartesian coordinates
    !brk(1:3) = matmul(b(1:3, 1:3), rk(1:3))

    do II = 1, 3
       brk(II) = dot_product(b(II, 1:3), ark(1:3))
    end do

    ! brk is the wave vector expressed in cartesian coordinates

    do K = 2, order
       !todo check
       do II = 1, 3
          robrk(II) = dot_product(rgr(1:3, II, K), brk(1:3))
       end do
       do II = 1, 3
          rosk(II) = dot_product(bi(1:3, II), robrk(1:3))
       end do

       ! robrk(1:3) = matmul(brk(1:3), rgr(1:3, 1:3, K))
       ! rosk(1:3) = matmul(robrk(1:3) , bi(1:3, 1:3))

       ! rosk is the rotated wave vector in reciprocal lattice coordinates

       rosk(1:3) = rosk(1:3) - ark(1:3)

       ! this difference should be a reciprocal lattice vector (integer in reduced coordinates)
       ! if the symmetry operation belongs to the group of the k-vector

       K1 = 1
       do while(K1 <= 3)

          TT = abs(rosk(K1) - nint(rosk(K1)))
          if (TT > membership_tol) then
             ! if this condition is fulfilled, rosk is not a reciprocal lattice vector
             exit
          end if
          K1 = K1 + 1
       end do

       if (K1 > 3) then
          kgord = kgord + 1
          tmp_kgel(kgord) = K
       end if
    end do

    do I = 1, kgord
       kgel(I) = tmp_kgel(I)
    end do

    do I  = 1, kgord
       kkgel(I) = gel(kgel(I))
    end do

    kg = kgord

    ! The kth element of the group of the wave vector is the kkgel(K)th element of
    ! the complete set of point group operators

    do I = 1, kgord
       do J = 1, order
          mtab2(I, J) = mtab(kgel(I), J)
       end do
    end do

    do I = 1, kgord
       do J = 1, kgord
          mtab2(I, J) = mtab2(I, kgel(J))
       end do
    end do

    do I = 1, kgord
       inverk(kgel(I)) = I
    end do

    do I = 1, kgord
       do J = 1, kgord
          if (inverk(mtab2(I, J)) == 0) then
             error stop "Little-group multiplication table is not closed"
          end if
          mtab2(I, J) = inverk(mtab2(I, J))
       end do
    end do
    if (steer(18) == 1) then
       write(*,*) "K-point symmetry analysis completed"
    end if

    call validate_little_group(mtab2, kgord, tol_group_closure)

    k2gord = kgord
    ibz = 1
    ksym = 1
    nopi1 = 1
    nopli1(:) = 1
    listp(:) = 0
    do I = 1, kgord
       listp(I) = I
    end do
    til(:,:) = 0.0_dp
    sil(:) = cmplx(1.0_dp, 0.0_dp, dp)

    ! section 3
    ! tests for the nonsymmorphic space group
    if (.not. (steer(20) .ne. 0)) then
       ! section 3.1
       ! test if k-vector lies within the first brillouin zone

       ibz = 0
       call sym_bztest(ntz, srk, b)

       if (ntz > 0) then
          ! k - vector outside Bz
          write(*,*)
          write(*,*) "=========================================="
          write(*,*) "WARNING: K-point Outside Brillouin Zone"
          write(*,*) "=========================================="
          write(*,'(A,3F12.6)') " K-point: ", ark(1:3)
          write(*,*)
          return
       end if

       if (ntz < 0) then
          ibz = 1
          if (steer(18) .eq. 1) then
             write(*,*)
             write(*,*) "Note: Nonsymmorphic space group, k-point within BZ"
          end if
       end if

       ! section 3.2
       ! Test if the group of the k-vector is symmorphic
       ksym = 0
       I = 1
       do while (I <= kgord)
          I1 = kgel(I)
          K = 1
          do while (K <= 3)
             if (u(I1, K) .ne. 0) then
                exit
             end if
             K = K + 1
          end do
          if (K <= 3) then
             exit
          end if
          I = I + 1
       end do
       if (I > kgord) then
          ksym = 1
          if (steer(18) .eq. 1) then
             write(*,*)
             write(*,*) "Note: Nonsymmorphic space group with symmorphic Gk"
          end if
       end if

       if (I <= kgord) then
          call factorgroup(listp, k2gord, kgord, nopi, nopi1, nopli, nopli1, mtab2, til, &
               & sil, kgel, a, ai, b, u, rk, rgr, factor_group_ok)
          if (.not. factor_group_ok) then
             deallocate(inverk)
             deallocate(tmp_kgel)
             return
          end if
          ibz = 0
       end if
    end if

    deallocate(inverk)
    deallocate(tmp_kgel)

  end subroutine sym_groupkp

  subroutine validate_little_group(mt, group_order, tol)
    integer, intent(in) :: mt(:,:)
    integer, intent(in) :: group_order
    real(dp), intent(in) :: tol

    integer :: i, j, k, left, right, inverse_index

    if (group_order < 1) then
       error stop "Invalid little-group order"
    end if

    do i = 1, group_order
       if (mt(i, 1) < 1 .or. mt(i, 1) > group_order) then
          error stop "Little-group multiplication table has invalid first-column entries"
       end if
       if (mt(1, i) < 1 .or. mt(1, i) > group_order) then
          error stop "Little-group multiplication table has invalid first-row entries"
       end if
    end do

    do i = 1, group_order
       do j = 1, group_order
          k = mt(i, j)
          if (k < 1 .or. k > group_order) then
             error stop "Little-group multiplication table is not closed"
          end if
       end do
    end do

    do i = 1, group_order
       if (mt(1, i) /= i .or. mt(i, 1) /= i) then
          error stop "Little-group multiplication table has inconsistent identity element"
       end if
    end do

    do i = 1, group_order
       inverse_index = 0
       do j = 1, group_order
          if (mt(i, j) == 1 .and. mt(j, i) == 1) then
             inverse_index = j
             exit
          end if
       end do
       if (inverse_index == 0) then
          error stop "Little-group multiplication table has no inverse element"
       end if
    end do

    do i = 1, group_order
       do j = 1, group_order
          do k = 1, group_order
             left = mt(mt(i, j), k)
             right = mt(i, mt(j, k))
             if (left /= right) then
                error stop "Little-group multiplication table violates associativity"
             end if
          end do
       end do
    end do
  end subroutine validate_little_group
  
  ! formation of the factor group Gk/Tk
  
  ! For a nonsymmorphic little group, construct a finite central extension from
  ! the Bloch phases. This applies at boundary and interior k-points whenever the
  ! phases close at finite order. k2gord will be the order
  ! of this group. listp(I) contains the index of the pointgroup operator of the
  ! Ith element of this group. Several elements may have the same pointgroup
  ! operator, so that they differ only in the translation part til(I,K), K = 1:3
  ! and the corresponding exponential part sil(I). nopi(I) is the number of
  ! elements with the same Ith pointgroup operator. The Kth operator, which has
  ! I as its pointgroup operator is group element number nopli(I,K) in the factor
  ! group. nr(I) is the number of elements in the Ith row of the group
  ! multiplication table mtab3(I,K), which has been calculated. At the end all
  ! nr(I) = k2gord.
  !
  subroutine factorgroup(listp, k2gord, kgord, nopi, nopi1, nopli, nopli1,  &
          & mtab2, til, sil, kgel, a, ai, b, u, rk, rgr, success)

    integer, intent(out) :: listp(:)
    integer, intent(out) :: k2gord
    integer, intent(out) :: nopi(:)
    integer, intent(out) :: nopi1
    integer, intent(out) :: nopli(:,:) 
    integer, intent(out) :: nopli1(:)
    integer, intent(inout) :: mtab2(:,:)
    real(dp), intent(out) :: til(:,:)
    complex(dp), intent(out):: sil(:)

    integer, intent(inout) :: kgord

    integer, intent(in) :: kgel(:)
    real(dp), intent(in) :: a(:,:)
    real(dp), intent(in) :: ai(:,:)
    real(dp), intent(in) :: b(:,:)
    real(dp), intent(in) :: u(:,:)

    real(dp), intent(in) :: rk(:)
    real(dp), intent(in) :: rgr(:,:,:)
    logical, intent(out) :: success

    integer, allocatable :: mtab3(:,:)
    integer :: I, point_index, existing_index
    integer :: left_index, right_index, product_point
    integer :: factor_capacity, initial_kgord, max_phase_order, previous_order
    real(dp), dimension(3) :: phase_translation, product_translation
    complex(dp) :: product_phase

    initial_kgord = kgord
    k2gord = initial_kgord
    success = .true.
    factor_capacity = min(size(mtab2, 1), size(mtab2, 2), size(listp), size(sil), &
         & size(til, 1), size(nopli, 2))

    if (initial_kgord > factor_capacity) then
       success = .false.
       return
    end if

    allocate(mtab3(factor_capacity, factor_capacity))
    mtab3(:,:) = 0
    listp(:) = 0
    til(:,:) = 0.0_dp
    sil(:) = cmplx(0.0_dp, 0.0_dp, dp)
    nopi(:) = 0
    nopli(:,:) = 0
    nopli1(:) = 1

    do I = 1, initial_kgord
       listp(I) = I
       point_index = kgel(I)
       til(I, 1:3) = u(point_index, 1:3)
       phase_translation(:) = til(I, 1:3)
       sil(I) = bloch_phase(rk(1:3), phase_translation)
       nopi(I) = 1
       nopli(I, 1) = I
    end do

    til(1, 1:3) = 0.0_dp
    sil(1) = cmplx(1.0_dp, 0.0_dp, dp)

    max_phase_order = max(1, factor_capacity/initial_kgord)
    do I = 1, initial_kgord
       if (.not. phase_has_finite_order(sil(I), max_phase_order)) then
          success = .false.
          kgord = initial_kgord
          deallocate(mtab3)
          return
       end if
    end do

    do
       previous_order = k2gord
       do left_index = 1, k2gord
          do right_index = 1, k2gord
             if (mtab3(left_index, right_index) /= 0) cycle

             call multiply_factor_elements(left_index, right_index, product_point, &
                  & product_translation, product_phase)

             existing_index = find_factor_element(product_point, product_phase)
             if (existing_index == 0) then
                if (k2gord >= factor_capacity) then
                   success = .false.
                   kgord = initial_kgord
                   deallocate(mtab3)
                   return
                end if
                if (nopi(product_point) >= size(nopli, 2)) then
                   success = .false.
                   kgord = initial_kgord
                   deallocate(mtab3)
                   return
                end if

                k2gord = k2gord + 1
                existing_index = k2gord
                listp(existing_index) = product_point
                til(existing_index, 1:3) = product_translation(1:3)
                sil(existing_index) = product_phase
                nopi(product_point) = nopi(product_point) + 1
                nopli(product_point, nopi(product_point)) = existing_index
             end if

             mtab3(left_index, right_index) = existing_index
          end do
       end do

       if (.not. any(mtab3(1:k2gord, 1:k2gord) == 0)) exit
       if (k2gord == previous_order) then
          success = .false.
          kgord = initial_kgord
          deallocate(mtab3)
          return
       end if
    end do

    call validate_little_group(mtab3, k2gord, tol_group_closure)

    kgord = k2gord
    nopi1 = nopi(1)
    nopli1(1:nopi1) = nopli(1, 1:nopi1)
    mtab2(1:kgord, 1:kgord) = mtab3(1:kgord, 1:kgord)

    deallocate(mtab3)

  contains

    logical function phase_has_finite_order(phase, max_order) result(has_finite_order)
      complex(dp), intent(in) :: phase
      integer, intent(in) :: max_order
      integer :: phase_order

      has_finite_order = .false.
      do phase_order = 1, max_order
         if (abs(phase**phase_order - cmplx(1.0_dp, 0.0_dp, dp)) <= tol_irrep_phase) then
            has_finite_order = .true.
            return
         end if
      end do
    end function phase_has_finite_order

    subroutine multiply_factor_elements(left, right, product_point_out, translation_out, phase_out)
      integer, intent(in) :: left, right
      integer, intent(out) :: product_point_out
      real(dp), intent(out) :: translation_out(3)
      complex(dp), intent(out) :: phase_out

      integer :: left_point, right_point, rotation_index
      real(dp) :: translation_cart(3), rotated_cart(3), rotated_fractional(3)

      left_point = listp(left)
      right_point = listp(right)
      product_point_out = mtab2(left_point, right_point)
      rotation_index = kgel(left_point)

      translation_cart = matmul(transpose(a(1:3, 1:3)), til(right, 1:3))
      rotated_cart = matmul(rgr(1:3, 1:3, rotation_index), translation_cart)
      rotated_fractional = matmul(ai(1:3, 1:3), rotated_cart)
      translation_out(1:3) = til(left, 1:3) + rotated_fractional(1:3)
      phase_out = bloch_phase(rk(1:3), translation_out)
    end subroutine multiply_factor_elements

    integer function find_factor_element(point, phase) result(found_index)
      integer, intent(in) :: point
      complex(dp), intent(in) :: phase

      integer :: candidate_number, candidate_index

      found_index = 0
      do candidate_number = 1, nopi(point)
         candidate_index = nopli(point, candidate_number)
         if (abs(sil(candidate_index) - phase) <= tol_irrep_phase) then
            found_index = candidate_index
            return
         end if
      end do
    end function find_factor_element

  end subroutine factorgroup

end module groupkp
