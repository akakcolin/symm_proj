program analyze_vasp_conversion
  use accuracy, only: dp
  implicit none
  real(dp), parameter :: pi = 3.14159265358979323846_dp

  write(*,*) "=================================================="
  write(*,*) "VASP k点转换深入分析"
  write(*,*) "=================================================="
  write(*,*)

  call analyze_vasp_kpoint_formula()
  call test_actual_code_logic()

contains

  subroutine analyze_vasp_kpoint_formula()
    real(dp) :: lattice(3,3), scale_factors(3)
    real(dp) :: lattice_scaled(3,3)
    real(dp) :: reciprocal_lattice(3,3), volume
    real(dp) :: k_vasp_cartesian(3), k_internal(3)
    real(dp) :: reciprocal_conversion(3,3)
    integer :: i

    write(*,*) "=== VASP k点单位约定分析 ==="
    write(*,*)

    ! 设置：正交晶格，各向异性scale
    lattice(1,:) = [1.0_dp, 0.0_dp, 0.0_dp]
    lattice(2,:) = [0.0_dp, 2.0_dp, 0.0_dp]
    lattice(3,:) = [0.0_dp, 0.0_dp, 3.0_dp]
    scale_factors = [2.0_dp, 3.0_dp, 4.0_dp]

    write(*,*) "1. POSCAR输入晶格（scale前）："
    do i = 1, 3
       write(*,'(A,I1,A,3F8.3)') "   a", i, " = ", lattice(i,:)
    end do
    write(*,'(A,3F8.3)') "   scale_factors = ", scale_factors
    write(*,*)

    ! 缩放后的晶格
    do i = 1, 3
       lattice_scaled(:, i) = lattice(:, i) * scale_factors(i)
    end do

    write(*,*) "2. 实际晶格（scale后）："
    do i = 1, 3
       write(*,'(A,I1,A,3F8.3)') "   a", i, " = ", lattice_scaled(i,:)
    end do
    write(*,*)

    ! 计算倒格矢
    volume = lattice_scaled(1,1) * (lattice_scaled(2,2) * lattice_scaled(3,3))
    reciprocal_lattice(1,:) = 2.0_dp * pi * &
         cross_product(lattice_scaled(2,:), lattice_scaled(3,:)) / volume
    reciprocal_lattice(2,:) = 2.0_dp * pi * &
         cross_product(lattice_scaled(3,:), lattice_scaled(1,:)) / volume
    reciprocal_lattice(3,:) = 2.0_dp * pi * &
         cross_product(lattice_scaled(1,:), lattice_scaled(2,:)) / volume

    write(*,*) "3. 倒格矢（按行）："
    do i = 1, 3
       write(*,'(A,I1,A,3F10.5)') "   b", i, " = ", reciprocal_lattice(i,:)
    end do
    write(*,'(A,3F10.5)') "   单位：2π/a = ", 2.0_dp*pi / [2.0_dp, 6.0_dp, 12.0_dp]
    write(*,*)

    ! VASP Cartesian k点
    k_vasp_cartesian = [0.25_dp, 0.25_dp, 0.0_dp]

    write(*,*) "4. VASP Cartesian k点："
    write(*,'(A,3F8.3)') "   k_vasp_cart = ", k_vasp_cartesian
    write(*,*) "   VASP约定：单位是 2π/s_i"
    write(*,'(A,3F10.5)') "   即单位向量：", 2.0_dp*pi/scale_factors
    write(*,*)

    ! VASP的实际k值（Cartesian坐标，单位2π/Å）
    write(*,*) "5. 实际Cartesian k值（2π/Å）："
    write(*,'(A,3F10.5)') "   k_cart_real = k_vasp * 2π/s = ", &
         k_vasp_cartesian * 2.0_dp * pi / scale_factors
    write(*,*)

    ! 转换到倒空间分数坐标
    ! k_frac · b = k_cart_real
    ! k_frac_i * b_i = k_cart_real
    ! 对正交晶格：k_frac_i = k_cart_real_i / |b_i|
    write(*,*) "6. 倒空间分数坐标（手算）："
    write(*,'(A,3F10.5)') "   k_frac = k_cart / |b| = ", &
         (k_vasp_cartesian * 2.0_dp * pi / scale_factors) / &
         (2.0_dp * pi / [2.0_dp, 6.0_dp, 12.0_dp])
    write(*,*)

    ! 代码中的转换
    write(*,*) "7. 代码实现的转换："
    write(*,*) "   reciprocal_conversion = transpose(lattice) / scale_factors"
    reciprocal_conversion = transpose(lattice)
    do i = 1, 3
       reciprocal_conversion(i, :) = reciprocal_conversion(i, :) / scale_factors(i)
    end do
    write(*,*) "   reciprocal_conversion矩阵："
    do i = 1, 3
       write(*,'(A,3F10.5)') "     ", reciprocal_conversion(i,:)
    end do
    write(*,*)

    k_internal = matmul(k_vasp_cartesian, reciprocal_conversion)
    write(*,'(A,3F10.5)') "   k_internal = k_vasp * reciprocal_conversion = ", k_internal
    write(*,*)

    write(*,*) "8. 问题诊断："
    write(*,*) "   代码得到：", k_internal
    write(*,*) "   应该得到：[0.25, 0.50, 0.0]"
    write(*,*) "   差异：代码没有考虑2π因子！"
    write(*,*)

  end subroutine analyze_vasp_kpoint_formula

  subroutine test_actual_code_logic()
    real(dp) :: lattice(3,3), scale_factors(3)
    real(dp) :: reciprocal_conversion(3,3)
    real(dp) :: k_vasp(3), k_result(3)
    integer :: i

    write(*,*) "=== 测试代码实际逻辑 ==="
    write(*,*)

    ! 简单立方，统一scale
    lattice = 0.0_dp
    lattice(1,1) = 1.0_dp
    lattice(2,2) = 1.0_dp
    lattice(3,3) = 1.0_dp
    scale_factors = [5.0_dp, 5.0_dp, 5.0_dp]

    write(*,*) "简单立方晶格，a = 5 Å"
    write(*,*) "倒格矢：b = 2π/5 [1,0,0], [0,1,0], [0,0,1]"
    write(*,*)

    k_vasp = [0.5_dp, 0.0_dp, 0.0_dp]
    write(*,'(A,3F8.3)') "VASP k点（单位2π/5）：", k_vasp
    write(*,*) "实际值：0.5 * 2π/5 = π/5 [1,0,0]"
    write(*,*) "分数坐标应该是：0.5（因为 π/5 = 0.5 * 2π/5）"
    write(*,*)

    ! 代码转换
    reciprocal_conversion = transpose(lattice)
    do i = 1, 3
       reciprocal_conversion(i, :) = reciprocal_conversion(i, :) / scale_factors(i)
    end do
    k_result = matmul(k_vasp, reciprocal_conversion)

    write(*,'(A,3F8.3)') "代码给出：", k_result
    write(*,*) "结果：0.1（错误！应该是0.5）"
    write(*,*)

    write(*,*) "=== 结论 ==="
    write(*,*) "代码转换公式有问题："
    write(*,*) "  当前：k_frac = k_vasp * transpose(lattice) / scale"
    write(*,*) "  这给出：k_frac_i = k_vasp_i * lattice_ii / scale_i"
    write(*,*) "  对正交晶格：= k_vasp_i * 1 / scale_i"
    write(*,*) ""
    write(*,*) "但VASP的k点单位是 2π/scale_i，所以："
    write(*,*) "  实际k值 = k_vasp_i * 2π/scale_i"
    write(*,*) "  而倒格矢 = 2π/scale_i"
    write(*,*) "  分数坐标 = k_vasp_i（不需要除以scale！）"
    write(*,*)

  end subroutine test_actual_code_logic

  function cross_product(a, b) result(c)
    real(dp), intent(in) :: a(3), b(3)
    real(dp) :: c(3)
    c(1) = a(2) * b(3) - a(3) * b(2)
    c(2) = a(3) * b(1) - a(1) * b(3)
    c(3) = a(1) * b(2) - a(2) * b(1)
  end function cross_product

end program analyze_vasp_conversion
