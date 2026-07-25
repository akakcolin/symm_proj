! ============================================
! Complex → Real Spherical Harmonics Transform
! ============================================
! DFTB+ uses real spherical harmonics (Slater-Koster
! convention), while sympw internally works with
! complex spherical harmonics (Wigner D-matrices
! are defined in the complex basis).
!
! This module applies the block-diagonal unitary
! transformation
!          P_real = U * P_complex * U^H
!
! Convention (Condon-Shortley phase included):
!   For L >= 0, m in [-L, L]:
!
!     m =  0 : R_0  = C_0
!     m >  0 : R_{+m} = (1/√2)[(-1)^m * C_{+m} + C_{-m}]
!     m <  0 : R_{-m} = (-i/√2)[(-1)^m * C_{+m} - C_{-m}]
!
! Resulting ordering (L=1): [py, pz, px]
! ============================================

module sympw_real_sh
  use accuracy
  implicit none
  private

  public :: complex_to_real_projector
  public :: get_block_offsets

contains

  ! Build the (2L+1)×(2L+1) unitary transformation matrix
  ! from complex to real spherical harmonics for a single L.
  subroutine build_ul_matrix(UL, L)
    complex(dp), intent(out) :: UL(:, :)
    integer, intent(in) :: L

    integer :: N, m, idx_p, idx_n, sign_m
    real(dp) :: inv_sqrt2

    N = 2*L + 1
    inv_sqrt2 = 1.0_dp / sqrt(2.0_dp)
    UL(:, :) = cmplx(0, 0, dp)

    ! m = 0: identity
    UL(L+1, L+1) = cmplx(1, 0, dp)

    do m = 1, L
       idx_p = m + L + 1       ! positive m index in array
       idx_n = -m + L + 1      ! negative m index in array
       sign_m = (-1)**m

       ! Real positive-m row: R_{+m} = (sign_m * C_{+m} + C_{-m}) / √2
       UL(idx_p, idx_p) = inv_sqrt2 * sign_m
       UL(idx_p, idx_n) = inv_sqrt2

       ! Real negative-m row: R_{-m} = -i * (sign_m * C_{+m} - C_{-m}) / √2
       UL(idx_n, idx_p) = cmplx(0, -inv_sqrt2 * sign_m, dp)
       UL(idx_n, idx_n) = cmplx(0,  inv_sqrt2, dp)
    end do
  end subroutine build_ul_matrix


  ! Build the full N×N block-diagonal transformation matrix U_total
  ! and return the start offset and size for each (atom, L) block.
  !
  ! Block indexing: block_id runs sequentially over
  !   element i → atom j (in element i) → L
  ! and blocks_blk(:, 1) = start_row, blocks_blk(:, 2) = block_size.
  subroutine build_full_transform(U_total, blocks_blk, n_blocks, &
       lmax_list, nat_list, N)
    complex(dp), allocatable, intent(out) :: U_total(:, :)
    integer, allocatable, intent(out) :: blocks_blk(:, :)
    integer, intent(out) :: n_blocks
    integer, intent(in) :: lmax_list(:)
    integer, intent(in) :: nat_list(:)
    integer, intent(in) :: N

    integer :: nel, ichem, iatom, L, Ln, block_start, block_id
    complex(dp), allocatable :: UL(:, :)

    nel = size(lmax_list)

    ! Count blocks
    n_blocks = 0
    do ichem = 1, nel
       do iatom = 1, nat_list(ichem)
          do L = 0, lmax_list(ichem)
             n_blocks = n_blocks + 1
          end do
       end do
    end do

    allocate(U_total(N, N))
    allocate(blocks_blk(n_blocks, 2))
    U_total(:, :) = cmplx(0, 0, dp)

    block_id = 0
    block_start = 1
    do ichem = 1, nel
       do iatom = 1, nat_list(ichem)
          do L = 0, lmax_list(ichem)
             Ln = 2*L + 1
             block_id = block_id + 1
             blocks_blk(block_id, 1) = block_start
             blocks_blk(block_id, 2) = Ln

             if (L == 0) then
                U_total(block_start, block_start) = cmplx(1, 0, dp)
             else
                allocate(UL(Ln, Ln))
                call build_ul_matrix(UL, L)
                U_total(block_start:block_start+Ln-1, &
                        block_start:block_start+Ln-1) = UL(1:Ln, 1:Ln)
                deallocate(UL)
             end if

             block_start = block_start + Ln
          end do
       end do
    end do
  end subroutine build_full_transform


  ! Get the block offsets for the basis ordering.
  ! block_id runs: (ichem, iatom, L) in the same order as matrixorder.
  ! Returns blocks_blk(:, 1) = start_row, blocks_blk(:, 2) = size(2L+1).
  subroutine get_block_offsets(blocks_blk, n_blocks, lmax_list, nat_list)
    integer, allocatable, intent(out) :: blocks_blk(:, :)
    integer, intent(out) :: n_blocks
    integer, intent(in) :: lmax_list(:)
    integer, intent(in) :: nat_list(:)

    integer :: nel, ichem, iatom, L, Ln, block_start, block_id

    nel = size(lmax_list)

    n_blocks = 0
    do ichem = 1, nel
       do iatom = 1, nat_list(ichem)
          do L = 0, lmax_list(ichem)
             n_blocks = n_blocks + 1
          end do
       end do
    end do

    allocate(blocks_blk(n_blocks, 2))

    block_id = 0
    block_start = 1
    do ichem = 1, nel
       do iatom = 1, nat_list(ichem)
          do L = 0, lmax_list(ichem)
             Ln = 2*L + 1
             block_id = block_id + 1
             blocks_blk(block_id, 1) = block_start
             blocks_blk(block_id, 2) = Ln
             block_start = block_start + Ln
          end do
       end do
    end do
  end subroutine get_block_offsets


  ! Transform a complex projection matrix Pc → real projection matrix Pr.
  !
  ! P_real = U * P_complex * U^H
  !
  ! Input:
  !   Pc          - complex projector (N×N) in CSH basis
  !   lmax_list   - max angular momentum per chemical element
  !   nat_list    - number of atoms per chemical element
  !
  ! Output:
  !   Pr          - real projector (N×N) in RSH basis
  subroutine complex_to_real_projector(Pc, lmax_list, nat_list, Pr)
    complex(dp), intent(in)  :: Pc(:, :)
    integer, intent(in)      :: lmax_list(:)
    integer, intent(in)      :: nat_list(:)
    complex(dp), intent(out) :: Pr(:, :)

    integer :: N, n_blocks, i, j, ri, rj, ci, cj
    integer :: expected_N, ichem, L
    complex(dp), allocatable :: U_total(:, :)
    integer, allocatable :: blk_info(:, :)
    complex(dp), allocatable :: temp(:, :)

    N = size(Pc, 1)
    if (size(Pc, 2) /= N .or. size(Pr, 1) /= N .or. size(Pr, 2) /= N) then
       write(*,*) "complex_to_real_projector: projector arrays must be square and size-matched"
       error stop
    end if

    expected_N = 0
    do ichem = 1, size(lmax_list)
       do L = 0, lmax_list(ichem)
          expected_N = expected_N + nat_list(ichem) * (2*L + 1)
       end do
    end do
    if (expected_N /= N) then
       write(*,*) "complex_to_real_projector: basis metadata size", expected_N, &
            "does not match projector size", N
       error stop
    end if

    Pr(:, :) = cmplx(0, 0, dp)

    call build_full_transform(U_total, blk_info, n_blocks, lmax_list, nat_list, N)

    ! P_real = U * P_complex * U^H
    ! Do it in two matmuls: temp = Pc * U^H, then Pr = U * temp
    allocate(temp(N, N))

    ! temp = Pc * U^H
    temp(1:N, 1:N) = matmul(Pc(1:N, 1:N), transpose(conjg(U_total(1:N, 1:N))))

    ! Pr = U * temp
    Pr(1:N, 1:N) = matmul(U_total(1:N, 1:N), temp(1:N, 1:N))

    ! Keep projector output Hermitian after the two dense transforms.
    Pr(1:N, 1:N) = 0.5_dp * (Pr(1:N, 1:N) + transpose(conjg(Pr(1:N, 1:N))))

    ! In a real spherical-harmonic basis, tiny imaginary parts are numerical
    ! roundoff for real projectors; keep genuinely complex matrices unchanged.
    if (maxval(abs(aimag(Pr(1:N, 1:N)))) < 100.0_dp * tol_projection) then
       Pr(1:N, 1:N) = cmplx(real(Pr(1:N, 1:N), dp), 0.0_dp, dp)
    end if

    deallocate(U_total, blk_info, temp)
  end subroutine complex_to_real_projector

end module sympw_real_sh
