module sympw_mulliken_real_pairs
  use accuracy, only: dp, rotation_table_index, tol_irrep_phase, tol_rotation_match
  implicit none
  private

  public :: assign_real_view_mulliken_label

contains

  subroutine assign_real_view_mulliken_label(point_group_number, operator_ids, &
       rotation_table, dimension, characters, label, success)
    integer, intent(in) :: point_group_number, dimension
    integer, intent(in) :: operator_ids(:)
    real(dp), intent(in) :: rotation_table(:,:,:)
    complex(dp), intent(in) :: characters(:)
    character(len=*), intent(out) :: label
    logical, intent(out) :: success

    integer :: principal_index, parity_index, parity_sign

    label = ""
    success = .false.
    if (.not. requires_real_pair_view(point_group_number)) return
    if (dimension < 1 .or. size(operator_ids) /= size(characters) .or. &
         size(characters) < 1) return
    if (abs(characters(1) - cmplx(real(dimension, dp), 0.0_dp, dp)) > &
         tol_irrep_phase .or. maxval(abs(aimag(characters))) > tol_irrep_phase) return
    if (.not. valid_rotation_input(point_group_number, operator_ids, rotation_table)) return

    select case(point_group_number)
    case(9)
       principal_index = find_c4_z(operator_ids, point_group_number, rotation_table)
       call label_c4_view(characters, dimension, principal_index, label, success)
    case(10)
       principal_index = find_s4_z(operator_ids, point_group_number, rotation_table)
       call label_c4_view(characters, dimension, principal_index, label, success)
    case(11)
       principal_index = find_c4_z(operator_ids, point_group_number, rotation_table)
       parity_index = find_inversion(operator_ids, point_group_number, rotation_table)
       call label_c4_view(characters, dimension, principal_index, label, success)
       if (success) call append_parity(characters, dimension, parity_index, label, success)
    case(16)
       call label_c3_view(dimension, label, success)
    case(17)
       parity_index = find_inversion(operator_ids, point_group_number, rotation_table)
       call label_c3_view(dimension, label, success)
       if (success) call append_parity(characters, dimension, parity_index, label, success)
    case(24)
       principal_index = find_c6_z(operator_ids, point_group_number, rotation_table)
       call label_c6_view(characters, dimension, principal_index, label, success)
    case(25)
       parity_index = find_horizontal_mirror(operator_ids, point_group_number, rotation_table)
       call label_c3_view(dimension, label, success)
       if (success) then
          parity_sign = scalar_character_sign(characters, parity_index, dimension)
          if (parity_sign > 0) then
             label = trim(label)//"'"
          else if (parity_sign < 0) then
             label = trim(label)//"''"
          else
             label = ""
             success = .false.
          end if
       end if
    case(26)
       principal_index = find_c6_z(operator_ids, point_group_number, rotation_table)
       parity_index = find_inversion(operator_ids, point_group_number, rotation_table)
       call label_c6_view(characters, dimension, principal_index, label, success)
       if (success) call append_parity(characters, dimension, parity_index, label, success)
    case(32)
       call label_t_view(dimension, label, success)
    case(33)
       parity_index = find_inversion(operator_ids, point_group_number, rotation_table)
       call label_t_view(dimension, label, success)
       if (success) call append_parity(characters, dimension, parity_index, label, success)
    end select
  end subroutine assign_real_view_mulliken_label


  logical function requires_real_pair_view(point_group_number) result(required)
    integer, intent(in) :: point_group_number

    select case(point_group_number)
    case(9:11, 16:17, 24:26, 32:33)
       required = .true.
    case default
       required = .false.
    end select
  end function requires_real_pair_view


  logical function valid_rotation_input(point_group_number, operator_ids, rotation_table) &
       result(valid)
    integer, intent(in) :: point_group_number, operator_ids(:)
    real(dp), intent(in) :: rotation_table(:,:,:)

    valid = .false.
    if (size(rotation_table, 1) < 3 .or. size(rotation_table, 2) < 3) return
    if (point_group_number >= 16 .and. point_group_number <= 26) then
       if (size(rotation_table, 3) < 72 .or. any(operator_ids < 1) .or. &
            any(operator_ids > 24)) return
    else
       if (size(rotation_table, 3) < 48 .or. any(operator_ids < 1) .or. &
            any(operator_ids > 48)) return
    end if
    valid = .true.
  end function valid_rotation_input


  subroutine label_c3_view(dimension, label, success)
    integer, intent(in) :: dimension
    character(len=*), intent(out) :: label
    logical, intent(out) :: success

    success = .true.
    select case(dimension)
    case(1); label = "A"
    case(2); label = "E"
    case default
       label = ""
       success = .false.
    end select
  end subroutine label_c3_view


  subroutine label_c4_view(characters, dimension, principal_index, label, success)
    complex(dp), intent(in) :: characters(:)
    integer, intent(in) :: dimension, principal_index
    character(len=*), intent(out) :: label
    logical, intent(out) :: success

    integer :: principal_sign

    label = ""
    success = .false.
    if (principal_index < 1 .or. principal_index > size(characters)) return
    if (dimension == 2) then
       if (abs(characters(principal_index)) > tol_irrep_phase) return
       label = "E"
       success = .true.
       return
    end if
    if (dimension /= 1) return
    principal_sign = character_sign(characters, principal_index)
    if (principal_sign == 0) return
    label = merge("A", "B", principal_sign > 0)
    success = .true.
  end subroutine label_c4_view


  subroutine label_c6_view(characters, dimension, principal_index, label, success)
    complex(dp), intent(in) :: characters(:)
    integer, intent(in) :: dimension, principal_index
    character(len=*), intent(out) :: label
    logical, intent(out) :: success

    integer :: principal_sign

    label = ""
    success = .false.
    if (principal_index < 1 .or. principal_index > size(characters)) return
    principal_sign = character_sign(characters, principal_index)
    if (principal_sign == 0) return
    if (dimension == 1) then
       label = merge("A", "B", principal_sign > 0)
    else if (dimension == 2) then
       label = merge("E1", "E2", principal_sign > 0)
    else
       return
    end if
    success = .true.
  end subroutine label_c6_view


  subroutine label_t_view(dimension, label, success)
    integer, intent(in) :: dimension
    character(len=*), intent(out) :: label
    logical, intent(out) :: success

    success = .true.
    select case(dimension)
    case(1); label = "A"
    case(2); label = "E"
    case(3); label = "T"
    case default
       label = ""
       success = .false.
    end select
  end subroutine label_t_view


  subroutine append_parity(characters, dimension, parity_index, label, success)
    complex(dp), intent(in) :: characters(:)
    integer, intent(in) :: dimension, parity_index
    character(len=*), intent(inout) :: label
    logical, intent(inout) :: success

    integer :: parity_sign

    if (.not. success) return
    parity_sign = scalar_character_sign(characters, parity_index, dimension)
    if (parity_sign == 0) then
       label = ""
       success = .false.
       return
    end if
    label = trim(label)//merge("g", "u", parity_sign > 0)
  end subroutine append_parity


  integer function character_sign(characters, element_index) result(sign_value)
    complex(dp), intent(in) :: characters(:)
    integer, intent(in) :: element_index

    sign_value = 0
    if (element_index < 1 .or. element_index > size(characters)) return
    if (abs(aimag(characters(element_index))) > tol_irrep_phase) return
    if (abs(abs(real(characters(element_index), dp)) - 1.0_dp) > tol_irrep_phase) return
    sign_value = merge(1, -1, real(characters(element_index), dp) > 0.0_dp)
  end function character_sign


  integer function scalar_character_sign(characters, element_index, dimension) result(sign_value)
    complex(dp), intent(in) :: characters(:)
    integer, intent(in) :: element_index, dimension

    sign_value = 0
    if (element_index < 1 .or. element_index > size(characters) .or. dimension < 1) return
    if (abs(aimag(characters(element_index))) > tol_irrep_phase) return
    if (abs(abs(real(characters(element_index), dp)) - real(dimension, dp)) > &
         tol_irrep_phase) return
    sign_value = merge(1, -1, real(characters(element_index), dp) > 0.0_dp)
  end function scalar_character_sign


  integer function find_inversion(operator_ids, point_group_number, rotation_table) result(index_value)
    integer, intent(in) :: operator_ids(:), point_group_number
    real(dp), intent(in) :: rotation_table(:,:,:)
    integer :: element_index
    real(dp) :: identity(3,3), rotation(3,3)

    identity = 0.0_dp
    identity(1,1) = 1.0_dp
    identity(2,2) = 1.0_dp
    identity(3,3) = 1.0_dp
    index_value = 0
    do element_index = 1, size(operator_ids)
       call get_rotation(operator_ids(element_index), point_group_number, rotation_table, rotation)
       if (maxval(abs(rotation + identity)) < tol_rotation_match) then
          index_value = element_index
          return
       end if
    end do
  end function find_inversion


  integer function find_c4_z(operator_ids, point_group_number, rotation_table) result(index_value)
    integer, intent(in) :: operator_ids(:), point_group_number
    real(dp), intent(in) :: rotation_table(:,:,:)

    index_value = find_axial_operation(operator_ids, point_group_number, rotation_table, &
         1.0_dp, 1.0_dp, .true.)
  end function find_c4_z


  integer function find_s4_z(operator_ids, point_group_number, rotation_table) result(index_value)
    integer, intent(in) :: operator_ids(:), point_group_number
    real(dp), intent(in) :: rotation_table(:,:,:)

    index_value = find_axial_operation(operator_ids, point_group_number, rotation_table, &
         -1.0_dp, -1.0_dp, .false.)
  end function find_s4_z


  integer function find_c6_z(operator_ids, point_group_number, rotation_table) result(index_value)
    integer, intent(in) :: operator_ids(:), point_group_number
    real(dp), intent(in) :: rotation_table(:,:,:)

    index_value = find_axial_operation(operator_ids, point_group_number, rotation_table, &
         1.0_dp, 2.0_dp, .true.)
  end function find_c6_z


  integer function find_horizontal_mirror(operator_ids, point_group_number, rotation_table) &
       result(index_value)
    integer, intent(in) :: operator_ids(:), point_group_number
    real(dp), intent(in) :: rotation_table(:,:,:)
    integer :: element_index
    real(dp) :: rotation(3,3), z_axis(3)

    index_value = 0
    z_axis = [0.0_dp, 0.0_dp, 1.0_dp]
    do element_index = 1, size(operator_ids)
       call get_rotation(operator_ids(element_index), point_group_number, rotation_table, rotation)
       if (abs(determinant3(rotation) + 1.0_dp) > tol_rotation_match .or. &
            abs(trace3(rotation) - 1.0_dp) > tol_rotation_match) cycle
       if (sqrt(sum((matmul(rotation, z_axis) + z_axis)**2)) < tol_rotation_match) then
          index_value = element_index
          return
       end if
    end do
  end function find_horizontal_mirror


  integer function find_axial_operation(operator_ids, point_group_number, rotation_table, &
       determinant_target, trace_target, preserves_z) result(index_value)
    integer, intent(in) :: operator_ids(:), point_group_number
    real(dp), intent(in) :: rotation_table(:,:,:), determinant_target, trace_target
    logical, intent(in) :: preserves_z
    integer :: element_index
    real(dp) :: rotation(3,3), z_axis(3), axis_error

    index_value = 0
    z_axis = [0.0_dp, 0.0_dp, 1.0_dp]
    do element_index = 1, size(operator_ids)
       call get_rotation(operator_ids(element_index), point_group_number, rotation_table, rotation)
       if (abs(determinant3(rotation) - determinant_target) > tol_rotation_match .or. &
            abs(trace3(rotation) - trace_target) > tol_rotation_match) cycle
       if (preserves_z) then
          axis_error = sqrt(sum((matmul(rotation, z_axis) - z_axis)**2))
       else
          axis_error = sqrt(sum((matmul(rotation, z_axis) + z_axis)**2))
       end if
       if (axis_error < tol_rotation_match) then
          index_value = element_index
          return
       end if
    end do
  end function find_axial_operation


  subroutine get_rotation(operator_id, point_group_number, rotation_table, rotation)
    integer, intent(in) :: operator_id, point_group_number
    real(dp), intent(in) :: rotation_table(:,:,:)
    real(dp), intent(out) :: rotation(3,3)

    rotation = rotation_table(1:3, 1:3, &
         rotation_table_index(operator_id, point_group_number))
  end subroutine get_rotation


  real(dp) function trace3(matrix) result(trace_value)
    real(dp), intent(in) :: matrix(3,3)

    trace_value = matrix(1,1) + matrix(2,2) + matrix(3,3)
  end function trace3


  real(dp) function determinant3(matrix) result(determinant_value)
    real(dp), intent(in) :: matrix(3,3)

    determinant_value = matrix(1,1)*(matrix(2,2)*matrix(3,3) - matrix(2,3)*matrix(3,2)) - &
         matrix(1,2)*(matrix(2,1)*matrix(3,3) - matrix(2,3)*matrix(3,1)) + &
         matrix(1,3)*(matrix(2,1)*matrix(3,2) - matrix(2,2)*matrix(3,1))
  end function determinant3

end module sympw_mulliken_real_pairs
