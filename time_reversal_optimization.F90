module time_reversal_optimization
  use accuracy
  use constants
  use projmat, only: validate_projector_matrix
  implicit none
  private
  public :: mark_kpoints_to_compute, print_tr_optimization_summary
  public :: verify_spinless_projector_pair
  public :: build_spinless_partner_projector

contains

  !============================================================================
  ! 标记哪些k点需要计算，哪些可以从时间反演伙伴复制
  !============================================================================
  subroutine mark_kpoints_to_compute(tr_pairs, nk, should_compute, source_kpoint, &
                                      n_computed, n_skipped, n_trim)
    integer, intent(in) :: tr_pairs(:)     ! 时间反演配对数组
    integer, intent(in) :: nk              ! k点总数
    logical, intent(out) :: should_compute(:)  ! 是否需要计算
    integer, intent(out) :: source_kpoint(:)   ! 如果不计算，从哪个k点复制
    integer, intent(out) :: n_computed     ! 需要计算的k点数
    integer, intent(out) :: n_skipped      ! 可以跳过的k点数
    integer, intent(out) :: n_trim         ! TRIM点数量

    integer :: ikp

    n_computed = 0
    n_skipped = 0
    n_trim = 0

    do ikp = 1, nk
       if (tr_pairs(ikp) == ikp) then
          ! TRIM点：k = -k，必须计算
          should_compute(ikp) = .true.
          source_kpoint(ikp) = ikp
          n_computed = n_computed + 1
          n_trim = n_trim + 1

       else if (tr_pairs(ikp) > ikp) then
          ! 时间反演配对中的第一个：计算
          should_compute(ikp) = .true.
          source_kpoint(ikp) = ikp
          n_computed = n_computed + 1

       else if (tr_pairs(ikp) > 0 .and. tr_pairs(ikp) < ikp) then
          ! 时间反演配对中的第二个：可以从伙伴复制
          should_compute(ikp) = .false.
          source_kpoint(ikp) = tr_pairs(ikp)
          n_skipped = n_skipped + 1

       else
          ! 没有时间反演伙伴（tr_pairs = -1 或 0）：必须计算
          should_compute(ikp) = .true.
          source_kpoint(ikp) = ikp
          n_computed = n_computed + 1

       end if
    end do

  end subroutine mark_kpoints_to_compute


  !============================================================================
  ! 打印时间反演优化的统计信息
  !============================================================================
  subroutine print_tr_optimization_summary(nk, n_computed, n_skipped, n_trim, &
                                            should_compute, source_kpoint, tr_pairs, &
                                            optimization_enabled)
    integer, intent(in) :: nk, n_computed, n_skipped, n_trim
    logical, intent(in) :: should_compute(:)
    integer, intent(in) :: source_kpoint(:)
    integer, intent(in) :: tr_pairs(:)
    logical, intent(in), optional :: optimization_enabled

    real(dp) :: potential_speedup
    integer :: ikp, n_pairs
    logical :: enabled

    enabled = .false.
    if (present(optimization_enabled)) enabled = optimization_enabled

    write(*,*)
    write(*,*) "=========================================="
    write(*,*) "Time-Reversal Optimization Analysis"
    write(*,*) "=========================================="
    write(*,*)

    ! 基本统计
    write(*,'(A,I6)') " Total k-points:                ", nk
    write(*,'(A,I6)') " TRIM points (k = -k):          ", n_trim

    n_pairs = 0
    do ikp = 1, min(nk, size(tr_pairs))
       if (tr_pairs(ikp) > ikp) n_pairs = n_pairs + 1
    end do
    if (n_pairs > 0) then
       write(*,'(A,I6,A,I6,A)') " Time-reversal pairs:           ", n_pairs, &
            " pairs (", n_pairs*2, " points)"
    end if
    write(*,*)

    ! 优化潜力
    write(*,'(A,I6)') " Would compute:                 ", n_computed
    write(*,'(A,I6)') " Could skip:                    ", n_skipped

    if (n_computed > 0) then
       potential_speedup = real(nk, dp) / real(n_computed, dp)
       write(*,*)
       write(*,'(A,F6.2,A)') " Potential speedup:             ", potential_speedup, "x"
       write(*,'(A,F6.1,A)') " Potential time saving:         ", &
            (1.0_dp - 1.0_dp/potential_speedup) * 100.0_dp, "%"
    end if

    write(*,*)
    write(*,*) "Detailed k-point classification:"
    write(*,*)

    ! 详细列表
    do ikp = 1, nk
       if (should_compute(ikp)) then
          if (tr_pairs(ikp) == ikp) then
             write(*,'(A,I4,A)') "  k", ikp, ": COMPUTE (TRIM point)"
          else if (tr_pairs(ikp) > ikp) then
             write(*,'(A,I4,A,I4,A)') "  k", ikp, ": COMPUTE (TR partner: k", &
                  tr_pairs(ikp), ")"
          else
             write(*,'(A,I4,A)') "  k", ikp, ": COMPUTE (no TR partner)"
          end if
       else
          write(*,'(A,I4,A,I4,A)') "  k", ikp, ": SKIP (copy from k", &
               source_kpoint(ikp), ")"
       end if
    end do

    write(*,*)
    write(*,*) "------------------------------------------"
    if (enabled) then
       write(*,*) "Spinless time-reversal projector reuse is ENABLED."
       write(*,*) "Copied projectors are validated before use."
    else
       write(*,*) "Spinless time-reversal projector reuse is DISABLED."
    end if
    write(*,*) "------------------------------------------"
    write(*,*)

  end subroutine print_tr_optimization_summary


  ! Verify the spinless time-reversal relation for full projectors.
  ! In a real, spin-independent orbital basis, Theta is complex
  ! conjugation, so P(-k) must equal conjg(P(k)).
  subroutine verify_spinless_projector_pair(projector_k, projector_minus_k, tol, &
                                             is_symmetric, max_diff)
    complex(dp), intent(in) :: projector_k(:,:), projector_minus_k(:,:)
    real(dp), intent(in) :: tol
    logical, intent(out) :: is_symmetric
    real(dp), intent(out) :: max_diff

    integer :: i, j

    is_symmetric = .false.
    max_diff = huge(1.0_dp)
    if (size(projector_k, 1) /= size(projector_k, 2) .or. &
         size(projector_minus_k, 1) /= size(projector_minus_k, 2) .or. &
         any(shape(projector_k) /= shape(projector_minus_k))) return

    max_diff = 0.0_dp
    do i = 1, size(projector_k, 1)
       do j = 1, size(projector_k, 2)
          max_diff = max(max_diff, abs(projector_minus_k(i,j) - conjg(projector_k(i,j))))
       end do
    end do
    is_symmetric = (max_diff <= tol)
  end subroutine verify_spinless_projector_pair


  subroutine build_spinless_partner_projector(projector_k, tol, projector_minus_k, &
                                               success, max_residual)
    complex(dp), intent(in) :: projector_k(:,:)
    real(dp), intent(in) :: tol
    complex(dp), allocatable, intent(out) :: projector_minus_k(:,:)
    logical, intent(out) :: success
    real(dp), intent(out) :: max_residual

    integer :: alloc_stat

    success = .false.
    max_residual = huge(1.0_dp)
    if (tol < 0.0_dp .or. size(projector_k, 1) /= size(projector_k, 2) .or. &
         size(projector_k, 1) < 1) return

    allocate(projector_minus_k(size(projector_k, 1), size(projector_k, 2)), stat=alloc_stat)
    if (alloc_stat /= 0) return
    projector_minus_k = conjg(projector_k)
    call validate_projector_matrix(projector_minus_k, tol, success, max_residual)
    if (.not. success) deallocate(projector_minus_k)
  end subroutine build_spinless_partner_projector


  !============================================================================
  ! 验证时间反演对称性（调试用）
  !============================================================================
  subroutine verify_tr_symmetry_of_projection_matrix(projmatrix, ikp1, ikp2, &
                                                       dim1, dim2, tol, is_symmetric)
    complex(dp), intent(in) :: projmatrix(:,:,:)
    integer, intent(in) :: ikp1, ikp2  ! 时间反演伙伴的索引
    integer, intent(in) :: dim1, dim2  ! 矩阵维度
    real(dp), intent(in) :: tol        ! 容差
    logical, intent(out) :: is_symmetric

    real(dp) :: max_diff

    if (dim1 /= size(projmatrix, 1) .or. dim2 /= size(projmatrix, 2) .or. &
         ikp1 < 1 .or. ikp1 > size(projmatrix, 3) .or. &
         ikp2 < 1 .or. ikp2 > size(projmatrix, 3)) then
       is_symmetric = .false.
       max_diff = huge(1.0_dp)
    else
       call verify_spinless_projector_pair(projmatrix(:,:,ikp1), projmatrix(:,:,ikp2), tol, &
            is_symmetric, max_diff)
    end if

    if (.not. is_symmetric) then
       write(*,'(A,I4,A,I4,A,E12.4)') &
            " WARNING: TR symmetry violated between k", ikp1, " and k", ikp2, &
            ", max diff = ", max_diff
    end if

  end subroutine verify_tr_symmetry_of_projection_matrix

end module time_reversal_optimization
