module time_reversal_optimization
  use accuracy
  use constants
  implicit none
  private
  public :: mark_kpoints_to_compute, print_tr_optimization_summary

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
                                            should_compute, source_kpoint, tr_pairs)
    integer, intent(in) :: nk, n_computed, n_skipped, n_trim
    logical, intent(in) :: should_compute(:)
    integer, intent(in) :: source_kpoint(:)
    integer, intent(in) :: tr_pairs(:)

    real(dp) :: potential_speedup
    integer :: ikp, n_pairs

    write(*,*)
    write(*,*) "=========================================="
    write(*,*) "Time-Reversal Optimization Analysis"
    write(*,*) "=========================================="
    write(*,*)

    ! 基本统计
    write(*,'(A,I6)') " Total k-points:                ", nk
    write(*,'(A,I6)') " TRIM points (k = -k):          ", n_trim

    n_pairs = (nk - n_trim) / 2
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
    write(*,*) "NOTE: Currently computing ALL k-points"
    write(*,*) "      for verification purposes."
    write(*,*) "      The above shows potential savings."
    write(*,*) "------------------------------------------"
    write(*,*)

  end subroutine print_tr_optimization_summary


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

    integer :: i, j
    real(dp) :: max_diff

    is_symmetric = .true.
    max_diff = 0.0_dp

    ! 检查 P(k2) 是否等于 P*(k1)
    do i = 1, dim1
       do j = 1, dim2
          max_diff = max(max_diff, abs(projmatrix(i,j,ikp2) - conjg(projmatrix(i,j,ikp1))))
          if (abs(projmatrix(i,j,ikp2) - conjg(projmatrix(i,j,ikp1))) > tol) then
             is_symmetric = .false.
          end if
       end do
    end do

    if (.not. is_symmetric) then
       write(*,'(A,I4,A,I4,A,E12.4)') &
            " WARNING: TR symmetry violated between k", ikp1, " and k", ikp2, &
            ", max diff = ", max_diff
    end if

  end subroutine verify_tr_symmetry_of_projection_matrix

end module time_reversal_optimization
