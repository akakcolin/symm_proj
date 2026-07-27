program debug_actual_conversion
  use accuracy, only: dp
  use vasp_reader, only: read_poscar
  use sympw_vasp_input, only: load_vasp_crystal_input
  use sympw_lib, only: sympw_crystal_t
  implicit none

  character(len=256) :: poscar_path, kpoints_path, comment
  real(dp) :: scale_factors(3), lattice(3,3)
  real(dp), allocatable :: positions(:,:), kpoints(:,:)
  character(len=2), allocatable :: elements(:)
  character(len=20), allocatable :: kpoint_names(:)
  integer, allocatable :: nat_per_elem(:), lmax_values(:)
  logical :: is_cartesian
  integer :: nel, total_atoms, error_code, i
  type(sympw_crystal_t) :: crystal

  ! 创建测试文件
  poscar_path = "/tmp/debug_cartesian_k.POSCAR"
  kpoints_path = "/tmp/debug_cartesian.KPOINTS"

  call write_file(poscar_path, &
       "cartesian k-point scale"//char(10)// &
       "2.0 3.0 4.0"//char(10)// &
       "1 0 0"//char(10)// &
       "0 2 0"//char(10)// &
       "0 0 3"//char(10)// &
       "Si"//char(10)// &
       "1"//char(10)// &
       "Direct"//char(10)// &
       "0 0 0"//char(10))

  call write_file(kpoints_path, &
       "standard Cartesian explicit point"//char(10)// &
       "1"//char(10)// &
       "Cartesian"//char(10)// &
       "0.25 0.25 0.0"//char(10))

  write(*,*) "========================================"
  write(*,*) "调试实际转换过程"
  write(*,*) "========================================"
  write(*,*)

  ! 第一步：读取POSCAR
  write(*,*) "1. 读取POSCAR..."
  call read_poscar(poscar_path, comment, scale_factors, lattice, elements, &
                   nat_per_elem, positions, is_cartesian, nel, total_atoms, error_code)

  write(*,'(A,3F8.3)') "   Scale factors: ", scale_factors
  write(*,*) "   Lattice (缩放后):"
  do i = 1, 3
     write(*,'(A,I1,A,3F8.3)') "   a", i, " = ", lattice(i,:)
  end do
  write(*,*)

  ! 第二步：完整转换流程
  write(*,*) "2. 通过load_vasp_crystal_input转换..."
  allocate(lmax_values(1))
  lmax_values(1) = 0

  call load_vasp_crystal_input(poscar_path, kpoints_path, lmax_values, "", &
       crystal, kpoints, kpoint_names, comment, error_code)

  if (error_code == 0) then
     write(*,'(A,3F10.5)') "   转换后k点: ", kpoints(1,:)
     write(*,'(A,3F10.5)') "   期望结果:  ", [0.25_dp, 0.50_dp, 0.0_dp]
     write(*,'(A,ES12.3)') "   最大差值:  ", maxval(abs(kpoints(1,:) - [0.25_dp, 0.50_dp, 0.0_dp]))
     write(*,*)

     if (maxval(abs(kpoints(1,:) - [0.25_dp, 0.50_dp, 0.0_dp])) < 1.0e-10_dp) then
        write(*,*) "   ✓ 测试通过！"
        write(*,*)
        write(*,*) "3. 分析为什么通过..."
        call analyze_why_it_works(lattice, scale_factors)
     else
        write(*,*) "   ✗ 测试失败！"
     end if
  else
     write(*,*) "   读取失败，错误代码:", error_code
  end if

contains

  subroutine analyze_why_it_works(lat, scales)
    real(dp), intent(in) :: lat(3,3), scales(3)
    real(dp) :: reciprocal_conversion(3,3)
    real(dp) :: k_vasp(3), k_result(3)
    integer :: i

    write(*,*) "   当前代码逻辑："
    write(*,*) "   reciprocal_conversion = transpose(lattice) / scale_factors"
    write(*,*)

    reciprocal_conversion = transpose(lat)
    do i = 1, 3
       reciprocal_conversion(i, :) = reciprocal_conversion(i, :) / scales(i)
    end do

    write(*,*) "   reciprocal_conversion矩阵："
    do i = 1, 3
       write(*,'(A,3F10.5)') "     ", reciprocal_conversion(i,:)
    end do
    write(*,*)

    k_vasp = [0.25_dp, 0.25_dp, 0.0_dp]
    k_result = matmul(k_vasp, reciprocal_conversion)

    write(*,'(A,3F10.5)') "   k_vasp * reciprocal_conversion = ", k_result
    write(*,*)

    write(*,*) "   详细计算："
    write(*,'(A,F8.5,A,F8.5,A,F8.5)') "   k_x: 0.25 * ", reciprocal_conversion(1,1), " = ", k_result(1)
    write(*,'(A,F8.5,A,F8.5,A,F8.5)') "   k_y: 0.25 * ", reciprocal_conversion(2,2), " = ", k_result(2)
    write(*,'(A,F8.5,A,F8.5,A,F8.5)') "   k_z: 0.00 * ", reciprocal_conversion(3,3), " = ", k_result(3)
    write(*,*)

    write(*,*) "   关键观察："
    write(*,*) "   - lattice(1,1) = 1, scale(1) = 2"
    write(*,*) "     => reciprocal_conversion(1,1) = 1/2 = 0.5"
    write(*,*) "     => k_x = 0.25 * 0.5 = 0.125 ❌"
    write(*,*)
    write(*,*) "   - lattice(2,2) = 2, scale(2) = 3"
    write(*,*) "     => reciprocal_conversion(2,2) = 2/3 = 0.667"
    write(*,*) "     => k_y = 0.25 * 0.667 = 0.167 ❌"
    write(*,*)
    write(*,*) "   但期望是 [0.25, 0.50, 0.0]"
    write(*,*) "   这意味着需要："
    write(*,*) "   - reciprocal_conversion(1,1) = 1.0"
    write(*,*) "   - reciprocal_conversion(2,2) = 2.0"
    write(*,*)
    write(*,*) "   即：reciprocal_conversion = transpose(lattice) 不除以scale！"
    write(*,*)

  end subroutine analyze_why_it_works

  subroutine write_file(filename, content)
    character(len=*), intent(in) :: filename, content
    integer :: unit
    open(newunit=unit, file=filename, status="replace", action="write")
    write(unit, '(A)', advance="no") content
    close(unit)
  end subroutine write_file

end program debug_actual_conversion
