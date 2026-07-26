module time_reversal
  use accuracy
  use constants
  implicit none
  private
  public :: is_time_reversal_invariant_point, find_trim_points
  public :: are_time_reversal_partners, build_time_reversal_pairs

contains

  ! 判断k点是否为时间反演不变点（TRIM）
  ! k = -k + G，其中G是倒格矢量
  logical function is_time_reversal_invariant_point(k, tol)
    real(dp), intent(in) :: k(:)
    real(dp), intent(in) :: tol

    real(dp) :: k_plus_minus_k(3)
    integer :: i

    if (size(k) < 3) then
       is_time_reversal_invariant_point = .false.
       return
    end if

    ! 计算 k + (-k) = 2k
    k_plus_minus_k = 2.0_dp * k(1:3)

    ! 检查 2k 是否接近倒格矢量（即每个分量接近整数）
    is_time_reversal_invariant_point = .true.
    do i = 1, 3
       ! 检查是否接近整数
       if (abs(k_plus_minus_k(i) - nint(k_plus_minus_k(i))) > tol) then
          is_time_reversal_invariant_point = .false.
          return
       end if
    end do

  end function is_time_reversal_invariant_point


  ! 在给定的k点列表中找出所有TRIM点
  subroutine find_trim_points(kpoints, nk, trim_indices, n_trim, tol)
    integer, intent(in) :: nk
    real(dp), intent(in) :: kpoints(nk, 3)
    integer, allocatable, intent(out) :: trim_indices(:)
    integer, intent(out) :: n_trim
    real(dp), intent(in) :: tol

    integer :: ik
    integer, allocatable :: temp_indices(:)

    allocate(temp_indices(nk))
    n_trim = 0

    do ik = 1, nk
       if (is_time_reversal_invariant_point(kpoints(ik, :), tol)) then
          n_trim = n_trim + 1
          temp_indices(n_trim) = ik
       end if
    end do

    ! 分配正确大小的数组
    allocate(trim_indices(n_trim))
    trim_indices(1:n_trim) = temp_indices(1:n_trim)

    deallocate(temp_indices)

  end subroutine find_trim_points


  ! 检查两个k点是否通过时间反演相关
  ! 即 k2 = -k1 + G
  logical function are_time_reversal_partners(k1, k2, tol)
    real(dp), intent(in) :: k1(:), k2(:)
    real(dp), intent(in) :: tol

    real(dp) :: k_sum(3)
    integer :: i

    if (size(k1) < 3 .or. size(k2) < 3) then
       are_time_reversal_partners = .false.
       return
    end if

    ! 计算 k1 + k2
    k_sum = k1(1:3) + k2(1:3)

    ! 检查 k1 + k2 是否为倒格矢量（每个分量接近整数）
    are_time_reversal_partners = .true.
    do i = 1, 3
       if (abs(k_sum(i) - nint(k_sum(i))) > tol) then
          are_time_reversal_partners = .false.
          return
       end if
    end do

  end function are_time_reversal_partners


  ! 为k点列表建立时间反演配对
  subroutine build_time_reversal_pairs(kpoints, nk, pairs, tol)
    integer, intent(in) :: nk
    real(dp), intent(in) :: kpoints(nk, 3)
    integer, intent(out) :: pairs(nk)  ! pairs(i) = j 表示 k_i 和 k_j 是时间反演伙伴
    real(dp), intent(in) :: tol

    integer :: ik, jk

    ! 初始化：每个k点与自己配对（表示未找到伙伴）
    pairs = 0

    do ik = 1, nk
       if (pairs(ik) /= 0) cycle  ! 已经配对过

       ! 检查是否为TRIM点
       if (is_time_reversal_invariant_point(kpoints(ik, :), tol)) then
          pairs(ik) = ik  ! TRIM点与自己配对
          cycle
       end if

       ! 寻找时间反演伙伴
       do jk = ik+1, nk
          if (pairs(jk) /= 0) cycle  ! 已经配对过

          if (are_time_reversal_partners(kpoints(ik, :), kpoints(jk, :), tol)) then
             pairs(ik) = jk
             pairs(jk) = ik
             exit
          end if
       end do

       ! 如果没找到伙伴，标记为未配对（可能k点列表不完整）
       if (pairs(ik) == 0) then
          pairs(ik) = -1
       end if
    end do

  end subroutine build_time_reversal_pairs

end module time_reversal
