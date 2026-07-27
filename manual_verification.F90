program manual_verification
  use accuracy, only: dp
  implicit none

  real(dp) :: lattice(3,3), scale_factors(3)
  real(dp) :: reciprocal_conversion(3,3)
  real(dp) :: k_vasp(3), k_result(3)
  integer :: i

  write(*,*) "========================================"
  write(*,*) "手动验证实际测试案例"
  write(*,*) "========================================"
  write(*,*)

  ! 测试案例的POSCAR晶格
  lattice(1,:) = [1.0_dp, 0.0_dp, 0.0_dp]
  lattice(2,:) = [0.0_dp, 2.0_dp, 0.0_dp]
  lattice(3,:) = [0.0_dp, 0.0_dp, 3.0_dp]

  scale_factors = [2.0_dp, 3.0_dp, 4.0_dp]

  write(*,*) "POSCAR晶格（缩放前）："
  do i = 1, 3
     write(*,'(A,I1,A,3F8.3)') "  a", i, " = ", lattice(i,:)
  end do
  write(*,'(A,3F8.3)') "  scale = ", scale_factors
  write(*,*)

  ! 代码的转换公式
  write(*,*) "代码转换公式："
  write(*,*) "  reciprocal_conversion = transpose(lattice) / scale_factors"
  write(*,*)

  reciprocal_conversion = transpose(lattice)
  do i = 1, 3
     reciprocal_conversion(i, :) = reciprocal_conversion(i, :) / scale_factors(i)
  end do

  write(*,*) "reciprocal_conversion矩阵："
  do i = 1, 3
     write(*,'(A,3F10.5)') "  ", reciprocal_conversion(i,:)
  end do
  write(*,*)

  ! VASP k点
  k_vasp = [0.25_dp, 0.25_dp, 0.0_dp]
  write(*,'(A,3F8.3)') "VASP k点输入: ", k_vasp
  write(*,*)

  ! 转换
  k_result = matmul(k_vasp, reciprocal_conversion)
  write(*,'(A,3F10.5)') "代码计算结果: ", k_result
  write(*,'(A,3F10.5)') "测试期望结果: ", [0.25_dp, 0.50_dp, 0.0_dp]
  write(*,*)

  write(*,*) "详细分析："
  write(*,*) "  k_x = 0.25 * (1/2) = 0.125"
  write(*,*) "  k_y = 0.25 * (2/3) = 0.167"
  write(*,*) "  k_z = 0.00 * (3/4) = 0.000"
  write(*,*)

  write(*,*) "但测试期望 [0.25, 0.50, 0.0]"
  write(*,*) "这需要："
  write(*,*) "  k_x = 0.25 * 1.0 = 0.25"
  write(*,*) "  k_y = 0.25 * 2.0 = 0.50"
  write(*,*)

  write(*,*) "结论："
  write(*,*) "如果测试真的通过了，那么代码中的lattice"
  write(*,*) "在sympw_vasp_input.F90中可能已经被缩放过了！"
  write(*,*)

  write(*,*) "让我检查sympw_vasp_input.F90:113行："
  write(*,*) '  do i = 1, 3'
  write(*,*) '     lattice(:, i) = lattice(:, i) * scale_factors(i)'
  write(*,*) '  end do'
  write(*,*)
  write(*,*) "这意味着在第122行进行k点转换时："
  write(*,*) "lattice已经是缩放后的值了！"
  write(*,*)

  ! 用缩放后的�ice重新计算
  write(*,*) "========================================"
  write(*,*) "用缩放后的晶格重新计算"
  write(*,*) "========================================"
  write(*,*)

  do i = 1, 3
     lattice(:, i) = lattice(:, i) * scale_factors(i)
  end do

  write(*,*) "缩放后的晶格："
  do i = 1, 3
     write(*,'(A,I1,A,3F8.3)') "  a", i, " = ", lattice(i,:)
  end do
  write(*,*)

  reciprocal_conversion = transpose(lattice)
  do i = 1, 3
     reciprocal_conversion(i, :) = reciprocal_conversion(i, :) / scale_factors(i)
  end do

  write(*,*) "新的reciprocal_conversion矩阵："
  do i = 1, 3
     write(*,'(A,3F10.5)') "  ", reciprocal_conversion(i,:)
  end do
  write(*,*)

  k_result = matmul(k_vasp, reciprocal_conversion)
  write(*,'(A,3F10.5)') "新的计算结果: ", k_result
  write(*,'(A,3F10.5)') "测试期望结果: ", [0.25_dp, 0.50_dp, 0.0_dp]
  write(*,'(A,ES12.3)') "差值:         ", maxval(abs(k_result - [0.25_dp, 0.50_dp, 0.0_dp]))
  write(*,*)

  if (maxval(abs(k_result - [0.25_dp, 0.50_dp, 0.0_dp])) < 1.0e-10_dp) then
     write(*,*) "✓ 完美匹配！"
     write(*,*)
     write(*,*) "解释："
     write(*,*) "代码在k点转换时使用的lattice已经缩放过，"
     write(*,*) "所以公式实际上是："
     write(*,*) "  reciprocal_conversion = transpose(scaled_lattice) / scale"
     write(*,*) "  = transpose(original_lattice * scale) / scale"
     write(*,*) "  = transpose(original_lattice)"
     write(*,*)
     write(*,*) "这就是为什么能得到正确结果！"
  else
     write(*,*) "✗ 仍然不匹配"
  end if

end program manual_verification
