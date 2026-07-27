program verify_implementations
  use accuracy, only: dp
  implicit none

  write(*,*) "==============================================="
  write(*,*) "物理实现核实程序"
  write(*,*) "==============================================="
  write(*,*)

  call verify_vasp_kpoint_convention()
  call verify_bloch_phase_signs()
  call verify_reciprocal_lattice_relation()
  call verify_centered_cell_reduction()

  write(*,*)
  write(*,*) "==============================================="
  write(*,*) "核实完成"
  write(*,*) "==============================================="

contains

  subroutine verify_vasp_kpoint_convention()
    ! 验证VASP Cartesian k点转换
    ! VASP约定：Cartesian k点使用 2π/s_i 为单位
    ! 其中 s_i 是各向异性scale因子

    real(dp) :: lattice(3,3), scale_factors(3)
    real(dp) :: k_cartesian(3), k_reciprocal_expected(3), k_reciprocal_computed(3)
    real(dp) :: reciprocal_conversion(3,3)
    real(dp) :: diff
    integer :: i

    write(*,*) "=== 测试1: VASP Cartesian k点转换 ==="

    ! 测试用例：正交晶格，各向异性scale
    lattice = 0.0_dp
    lattice(1,:) = [1.0_dp, 0.0_dp, 0.0_dp]
    lattice(2,:) = [0.0_dp, 2.0_dp, 0.0_dp]
    lattice(3,:) = [0.0_dp, 0.0_dp, 3.0_dp]

    scale_factors = [2.0_dp, 3.0_dp, 4.0_dp]

    ! VASP Cartesian k点 (0.25, 0.25, 0.0)
    ! 单位是 2π/s_i，即 (2π/2, 2π/3, 0)
    k_cartesian = [0.25_dp, 0.25_dp, 0.0_dp]

    ! 期望的倒空间分数坐标
    ! b1 = 2π (a2 × a3) / V = 2π [1, 0, 0] / 6 = π/3 [1,0,0]
    ! b2 = 2π (a3 × a1) / V = 2π [0, 2, 0] / 6 = 2π/3 [0,1,0]
    ! b3 = 2π (a1 × a2) / V = 2π [0, 0, 3] / 6 = π [0,0,1]
    !
    ! k_cart = 0.25 * 2π/2 [1,0,0] + 0.25 * 2π/3 [0,1,0]
    !        = π/4 [1,0,0] + π/6 [0,1,0]
    !
    ! k_frac · b = k_cart
    ! k_frac1 * π/3 = π/4  => k_frac1 = 3/4 = 0.75? 不对
    !
    ! 让我重新计算：
    ! 实际晶格（缩放后）：
    ! a1 = [2, 0, 0], a2 = [0, 6, 0], a3 = [0, 0, 12]
    ! V = 2*6*12 = 144
    ! b1 = 2π [1, 0, 0] * 6*12 / 144 = 2π [1,0,0] / 2 = π [1,0,0]
    ! b2 = 2π [0, 1, 0] * 2*12 / 144 = 2π [0,1,0] / 6 = π/3 [0,1,0]
    ! b3 = 2π [0, 0, 1] * 2*6 / 144 = 2π [0,0,1] / 12 = π/6 [0,0,1]
    !
    ! VASP k_cart = 0.25 * 2π/2 [1,0,0] + 0.25 * 2π/3 [0,1,0]
    !             = π/4 [1,0,0] + π/6 [0,1,0]
    !
    ! k = k_frac1 b1 + k_frac2 b2
    ! π/4 [1,0,0] = k_frac1 * π [1,0,0]  => k_frac1 = 1/4
    ! π/6 [0,1,0] = k_frac2 * π/3 [0,1,0] => k_frac2 = 1/2

    k_reciprocal_expected = [0.25_dp, 0.50_dp, 0.0_dp]

    ! 代码中的转换
    reciprocal_conversion = transpose(lattice)
    do i = 1, 3
       reciprocal_conversion(i, :) = reciprocal_conversion(i, :) / scale_factors(i)
    end do
    k_reciprocal_computed = matmul(k_cartesian, reciprocal_conversion)

    diff = maxval(abs(k_reciprocal_computed - k_reciprocal_expected))

    write(*,'(A,3F10.4)') "  输入k_cartesian:       ", k_cartesian
    write(*,'(A,3F10.4)') "  期望k_reciprocal:      ", k_reciprocal_expected
    write(*,'(A,3F10.4)') "  计算k_reciprocal:      ", k_reciprocal_computed
    write(*,'(A,ES12.3)') "  最大差值:              ", diff

    if (diff < 1.0e-10_dp) then
       write(*,*) "  ✓ VASP k点转换正确"
    else
       write(*,*) "  ✗ VASP k点转换有误"
    end if
    write(*,*)

  end subroutine verify_vasp_kpoint_convention

  subroutine verify_bloch_phase_signs()
    ! 验证Bloch相位符号约定
    real(dp), parameter :: pi = 3.14159265358979323846_dp
    real(dp) :: k_phase(3), lattice_shift(3), nonsymm_shift(3)
    complex(dp) :: phase_expected, phase_computed
    real(dp) :: diff

    write(*,*) "=== 测试2: Bloch相位符号约定 ==="

    ! 测试晶格平移相位：exp(-i k·n)
    k_phase = [0.5_dp * pi, 0.0_dp, 0.0_dp]  ! k = π/2 [1,0,0]
    lattice_shift = [1.0_dp, 0.0_dp, 0.0_dp]

    phase_expected = cmplx(0.0_dp, -1.0_dp, dp)  ! exp(-i π/2)
    phase_computed = exp(cmplx(0.0_dp, -dot_product(k_phase, lattice_shift), dp))

    diff = abs(phase_computed - phase_expected)

    write(*,'(A,3F8.3)') "  k相位:                 ", k_phase
    write(*,'(A,3F8.3)') "  晶格平移n:             ", lattice_shift
    write(*,'(A,2F8.3)') "  期望相位exp(-ik·n):    ", phase_expected
    write(*,'(A,2F8.3)') "  计算相位:              ", phase_computed
    write(*,'(A,ES12.3)') "  差值:                  ", diff

    if (diff < 1.0e-10_dp) then
       write(*,*) "  ✓ 晶格平移相位符号正确"
    else
       write(*,*) "  ✗ 晶格平移相位符号有误"
    end if

    ! 测试非共形群相位：exp(i k·(τ-n))
    nonsymm_shift = [0.5_dp, 0.0_dp, 0.0_dp]
    phase_expected = cmplx(1.0_dp/sqrt(2.0_dp), -1.0_dp/sqrt(2.0_dp), dp)
    ! exp(i k·(τ-n)) = exp(i π/2 * (0.5 - 1)) = exp(-i π/4)
    phase_computed = exp(cmplx(0.0_dp, &
         -dot_product(k_phase, lattice_shift) + dot_product(k_phase, nonsymm_shift), dp))

    diff = abs(phase_computed - phase_expected)

    write(*,'(A,3F8.3)') "  非共形平移τ:           ", nonsymm_shift
    write(*,'(A,2F8.3)') "  期望相位exp(ik(τ-n)):  ", phase_expected
    write(*,'(A,2F8.3)') "  计算相位:              ", phase_computed
    write(*,'(A,ES12.3)') "  差值:                  ", diff

    if (diff < 1.0e-10_dp) then
       write(*,*) "  ✓ 非共形群相位符号正确"
    else
       write(*,*) "  ✗ 非共形群相位符号有误"
    end if
    write(*,*)

  end subroutine verify_bloch_phase_signs

  subroutine verify_reciprocal_lattice_relation()
    ! 验证倒格矢定义 b_i · a_j = 2π δ_ij
    real(dp), parameter :: pi = 3.14159265358979323846_dp
    real(dp) :: lattice(3,3), reciprocal(3,3)
    real(dp) :: volume, test_matrix(3,3)
    real(dp) :: max_error
    integer :: i, j

    write(*,*) "=== 测试3: 倒格矢关系 b_i·a_j = 2π δ_ij ==="

    ! 简单正交晶格
    lattice(1,:) = [2.0_dp, 0.0_dp, 0.0_dp]
    lattice(2,:) = [0.0_dp, 3.0_dp, 0.0_dp]
    lattice(3,:) = [0.0_dp, 0.0_dp, 4.0_dp]

    ! 计算体积
    volume = lattice(1,1) * (lattice(2,2) * lattice(3,3) - lattice(2,3) * lattice(3,2)) - &
             lattice(1,2) * (lattice(2,1) * lattice(3,3) - lattice(2,3) * lattice(3,1)) + &
             lattice(1,3) * (lattice(2,1) * lattice(3,2) - lattice(2,2) * lattice(3,1))

    ! 计算倒格矢（按行存储）
    reciprocal(1,:) = 2.0_dp * pi * &
         cross_product(lattice(2,:), lattice(3,:)) / volume
    reciprocal(2,:) = 2.0_dp * pi * &
         cross_product(lattice(3,:), lattice(1,:)) / volume
    reciprocal(3,:) = 2.0_dp * pi * &
         cross_product(lattice(1,:), lattice(2,:)) / volume

    ! 验证 b_i · a_j = 2π δ_ij
    do i = 1, 3
       do j = 1, 3
          test_matrix(i,j) = dot_product(reciprocal(i,:), lattice(j,:))
       end do
    end do

    max_error = 0.0_dp
    do i = 1, 3
       do j = 1, 3
          if (i == j) then
             max_error = max(max_error, abs(test_matrix(i,j) - 2.0_dp * pi))
          else
             max_error = max(max_error, abs(test_matrix(i,j)))
          end if
       end do
    end do

    write(*,'(A)') "  测试矩阵 b_i·a_j:"
    do i = 1, 3
       write(*,'(A,3F10.4)') "    ", test_matrix(i,:)
    end do
    write(*,'(A,F10.4)') "  期望对角值: 2π =       ", 2.0_dp * pi
    write(*,'(A,ES12.3)') "  最大误差:              ", max_error

    if (max_error < 1.0e-10_dp) then
       write(*,*) "  ✓ 倒格矢定义正确"
    else
       write(*,*) "  ✗ 倒格矢定义有误"
    end if
    write(*,*)

  end subroutine verify_reciprocal_lattice_relation

  subroutine verify_centered_cell_reduction()
    ! 验证中心晶胞约化的k点变换
    real(dp) :: lattice_conv(3,3), lattice_prim(3,3)
    real(dp) :: k_conv(3), k_prim_expected(3), k_prim_computed(3)
    real(dp) :: k_transform(3,3)
    real(dp) :: diff

    write(*,*) "=== 测试4: 中心晶胞约化后的k点变换 ==="

    ! 体心立方(BCC)约化
    ! 惯用胞：立方，体心原子在(0.5, 0.5, 0.5)
    lattice_conv(1,:) = [1.0_dp, 0.0_dp, 0.0_dp]
    lattice_conv(2,:) = [0.0_dp, 1.0_dp, 0.0_dp]
    lattice_conv(3,:) = [0.0_dp, 0.0_dp, 1.0_dp]

    ! 原胞（BCC标准选择）
    lattice_prim(1,:) = [-0.5_dp,  0.5_dp,  0.5_dp]
    lattice_prim(2,:) = [ 0.5_dp, -0.5_dp,  0.5_dp]
    lattice_prim(3,:) = [ 0.5_dp,  0.5_dp, -0.5_dp]

    ! k点变换矩阵 = 原胞晶格变换矩阵
    k_transform = lattice_prim

    ! 惯用胞中的H点 (0.5, 0.5, 0.0)
    k_conv = [0.5_dp, 0.5_dp, 0.0_dp]

    ! 在原胞倒空间中的坐标
    ! k_prim = k_transform^T · k_conv
    k_prim_computed = matmul(transpose(k_transform), k_conv)
    k_prim_expected = [0.0_dp, 0.5_dp, 0.5_dp]  ! 原胞中的N点

    diff = maxval(abs(k_prim_computed - k_prim_expected))

    write(*,'(A,3F8.3)') "  惯用胞k点(H):          ", k_conv
    write(*,'(A,3F8.3)') "  期望原胞k点(N):        ", k_prim_expected
    write(*,'(A,3F8.3)') "  计算原胞k点:           ", k_prim_computed
    write(*,'(A,ES12.3)') "  最大差值:              ", diff

    if (diff < 1.0e-10_dp) then
       write(*,*) "  ✓ BCC约化的k点变换正确"
    else
       write(*,*) "  ✗ BCC约化的k点变换有误"
    end if
    write(*,*)

  end subroutine verify_centered_cell_reduction

  function cross_product(a, b) result(c)
    real(dp), intent(in) :: a(3), b(3)
    real(dp) :: c(3)

    c(1) = a(2) * b(3) - a(3) * b(2)
    c(2) = a(3) * b(1) - a(1) * b(3)
    c(3) = a(1) * b(2) - a(2) * b(1)
  end function cross_product

end program verify_implementations
