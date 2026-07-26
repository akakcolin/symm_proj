module sympw_mulliken_cubic
  use accuracy, only: dp, rotation_table_index, tol_irrep_phase, tol_rotation_match
  implicit none
  private

  public :: assign_cubic_mulliken_label

contains

  subroutine assign_cubic_mulliken_label(point_group_number, operator_ids, &
       rotation_table, characters, label, success)
    integer, intent(in) :: point_group_number
    integer, intent(in) :: operator_ids(:)
    real(dp), intent(in) :: rotation_table(:,:,:)
    complex(dp), intent(in) :: characters(:)
    character(len=*), intent(out) :: label
    logical, intent(out) :: success

    integer :: c3_index, axial_c2_index, principal_index, secondary_index
    integer :: inversion_index, irrep_dimension, parity_sign

    label = ""
    success = .false.
    if (point_group_number < 34 .or. point_group_number > 36) return
    if (size(operator_ids) /= size(characters) .or. size(characters) < 1) return
    if (size(rotation_table, 1) < 3 .or. size(rotation_table, 2) < 3 .or. &
         size(rotation_table, 3) < 48) return
    if (any(operator_ids < 1) .or. any(operator_ids > 48)) return
    if (abs(aimag(characters(1))) > tol_irrep_phase) return
    irrep_dimension = nint(real(characters(1), dp))
    if (irrep_dimension < 1 .or. &
         abs(characters(1) - cmplx(real(irrep_dimension, dp), 0.0_dp, dp)) > &
         tol_irrep_phase) return

    c3_index = find_any_c3(operator_ids, point_group_number, rotation_table)
    axial_c2_index = find_c2_axis(operator_ids, point_group_number, rotation_table, 1)
    select case(point_group_number)
    case(34)
       principal_index = find_c4_z(operator_ids, point_group_number, rotation_table)
       secondary_index = find_c2_direction(operator_ids, point_group_number, &
            rotation_table, [1.0_dp, 1.0_dp, 0.0_dp])
    case(35)
       principal_index = find_s4_z(operator_ids, point_group_number, rotation_table)
       secondary_index = find_any_mirror(operator_ids, point_group_number, rotation_table)
    case(36)
       principal_index = find_c4_z(operator_ids, point_group_number, rotation_table)
       secondary_index = find_c2_direction(operator_ids, point_group_number, &
            rotation_table, [1.0_dp, 1.0_dp, 0.0_dp])
    end select

    call label_cubic_family(characters, irrep_dimension, c3_index, axial_c2_index, &
         principal_index, secondary_index, label, success)
    if (.not. success) return
    if (point_group_number == 36) then
       inversion_index = find_inversion(operator_ids, point_group_number, rotation_table)
       parity_sign = scalar_character_sign(characters, inversion_index, irrep_dimension)
       if (parity_sign == 0) then
          label = ""
          success = .false.
          return
       end if
       label = trim(label)//merge("g", "u", parity_sign > 0)
    end if
  end subroutine assign_cubic_mulliken_label


  subroutine label_cubic_family(characters, dimension, c3_index, axial_c2_index, &
       principal_index, secondary_index, label, success)
    complex(dp), intent(in) :: characters(:)
    integer, intent(in) :: dimension, c3_index, axial_c2_index
    integer, intent(in) :: principal_index, secondary_index
    character(len=*), intent(out) :: label
    logical, intent(out) :: success

    integer :: c3_sign, axial_c2_sign, principal_sign, secondary_sign

    label = ""
    success = .false.
    if (c3_index < 1 .or. axial_c2_index < 1 .or. principal_index < 1 .or. &
         secondary_index < 1) return
    select case(dimension)
    case(1)
       c3_sign = character_sign(characters, c3_index)
       axial_c2_sign = character_sign(characters, axial_c2_index)
       principal_sign = character_sign(characters, principal_index)
       secondary_sign = character_sign(characters, secondary_index)
       if (c3_sign /= 1 .or. axial_c2_sign /= 1 .or. principal_sign == 0 .or. &
            secondary_sign /= principal_sign) return
       label = merge("A1", "A2", principal_sign > 0)
    case(2)
       if (abs(characters(c3_index) + cmplx(1.0_dp, 0.0_dp, dp)) > &
            tol_irrep_phase .or. &
            abs(characters(axial_c2_index) - cmplx(2.0_dp, 0.0_dp, dp)) > &
            tol_irrep_phase .or. abs(characters(principal_index)) > tol_irrep_phase .or. &
            abs(characters(secondary_index)) > tol_irrep_phase) return
       label = "E"
    case(3)
       principal_sign = character_sign(characters, principal_index)
       secondary_sign = character_sign(characters, secondary_index)
       if (abs(characters(c3_index)) > tol_irrep_phase .or. &
            abs(characters(axial_c2_index) + cmplx(1.0_dp, 0.0_dp, dp)) > &
            tol_irrep_phase .or. principal_sign == 0 .or. &
            secondary_sign /= -principal_sign) return
       label = merge("T1", "T2", principal_sign > 0)
    case default
       return
    end select
    success = .true.
  end subroutine label_cubic_family


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


  integer function find_any_c3(operator_ids, point_group_number, rotation_table) result(index_value)
    integer, intent(in) :: operator_ids(:), point_group_number
    real(dp), intent(in) :: rotation_table(:,:,:)

    integer :: element_index
    real(dp) :: rotation(3,3)

    index_value = 0
    do element_index = 1, size(operator_ids)
       call get_rotation(operator_ids(element_index), point_group_number, rotation_table, rotation)
       if (is_c3(rotation)) then
          index_value = element_index
          return
       end if
    end do
  end function find_any_c3


  integer function find_c2_axis(operator_ids, point_group_number, rotation_table, axis) result(index_value)
    integer, intent(in) :: operator_ids(:), point_group_number, axis
    real(dp), intent(in) :: rotation_table(:,:,:)
    real(dp) :: direction(3)

    index_value = 0
    direction = 0.0_dp
    if (axis < 1 .or. axis > 3) return
    direction(axis) = 1.0_dp
    index_value = find_c2_direction(operator_ids, point_group_number, rotation_table, direction)
  end function find_c2_axis


  integer function find_c2_direction(operator_ids, point_group_number, rotation_table, &
       direction_in) result(index_value)
    integer, intent(in) :: operator_ids(:), point_group_number
    real(dp), intent(in) :: rotation_table(:,:,:), direction_in(3)

    integer :: element_index
    real(dp) :: rotation(3,3), direction(3), direction_norm

    index_value = 0
    direction_norm = sqrt(sum(direction_in**2))
    if (direction_norm < tol_rotation_match) return
    direction = direction_in/direction_norm
    do element_index = 1, size(operator_ids)
       call get_rotation(operator_ids(element_index), point_group_number, rotation_table, rotation)
       if (.not. is_c2(rotation)) cycle
       if (sqrt(sum((matmul(rotation, direction) - direction)**2)) < tol_rotation_match) then
          index_value = element_index
          return
       end if
    end do
  end function find_c2_direction


  integer function find_c4_z(operator_ids, point_group_number, rotation_table) result(index_value)
    integer, intent(in) :: operator_ids(:), point_group_number
    real(dp), intent(in) :: rotation_table(:,:,:)

    integer :: element_index
    real(dp) :: rotation(3,3), z_axis(3)

    index_value = 0
    z_axis = [0.0_dp, 0.0_dp, 1.0_dp]
    do element_index = 1, size(operator_ids)
       call get_rotation(operator_ids(element_index), point_group_number, rotation_table, rotation)
       if (.not. is_c4(rotation)) cycle
       if (sqrt(sum((matmul(rotation, z_axis) - z_axis)**2)) < tol_rotation_match) then
          index_value = element_index
          return
       end if
    end do
  end function find_c4_z


  integer function find_s4_z(operator_ids, point_group_number, rotation_table) result(index_value)
    integer, intent(in) :: operator_ids(:), point_group_number
    real(dp), intent(in) :: rotation_table(:,:,:)

    integer :: element_index
    real(dp) :: rotation(3,3), z_axis(3)

    index_value = 0
    z_axis = [0.0_dp, 0.0_dp, 1.0_dp]
    do element_index = 1, size(operator_ids)
       call get_rotation(operator_ids(element_index), point_group_number, rotation_table, rotation)
       if (.not. is_s4(rotation)) cycle
       if (sqrt(sum((matmul(rotation, z_axis) + z_axis)**2)) < tol_rotation_match) then
          index_value = element_index
          return
       end if
    end do
  end function find_s4_z


  integer function find_any_mirror(operator_ids, point_group_number, rotation_table) result(index_value)
    integer, intent(in) :: operator_ids(:), point_group_number
    real(dp), intent(in) :: rotation_table(:,:,:)

    integer :: element_index
    real(dp) :: rotation(3,3)

    index_value = 0
    do element_index = 1, size(operator_ids)
       call get_rotation(operator_ids(element_index), point_group_number, rotation_table, rotation)
       if (is_mirror(rotation)) then
          index_value = element_index
          return
       end if
    end do
  end function find_any_mirror


  subroutine get_rotation(operator_id, point_group_number, rotation_table, rotation)
    integer, intent(in) :: operator_id, point_group_number
    real(dp), intent(in) :: rotation_table(:,:,:)
    real(dp), intent(out) :: rotation(3,3)

    rotation = rotation_table(1:3, 1:3, &
         rotation_table_index(operator_id, point_group_number))
  end subroutine get_rotation


  logical function is_c2(rotation) result(matches)
    real(dp), intent(in) :: rotation(3,3)

    matches = abs(determinant3(rotation) - 1.0_dp) < tol_rotation_match .and. &
         abs(trace3(rotation) + 1.0_dp) < tol_rotation_match
  end function is_c2


  logical function is_c3(rotation) result(matches)
    real(dp), intent(in) :: rotation(3,3)

    matches = abs(determinant3(rotation) - 1.0_dp) < tol_rotation_match .and. &
         abs(trace3(rotation)) < tol_rotation_match
  end function is_c3


  logical function is_c4(rotation) result(matches)
    real(dp), intent(in) :: rotation(3,3)

    matches = abs(determinant3(rotation) - 1.0_dp) < tol_rotation_match .and. &
         abs(trace3(rotation) - 1.0_dp) < tol_rotation_match
  end function is_c4


  logical function is_s4(rotation) result(matches)
    real(dp), intent(in) :: rotation(3,3)

    matches = abs(determinant3(rotation) + 1.0_dp) < tol_rotation_match .and. &
         abs(trace3(rotation) + 1.0_dp) < tol_rotation_match
  end function is_s4


  logical function is_mirror(rotation) result(matches)
    real(dp), intent(in) :: rotation(3,3)

    matches = abs(determinant3(rotation) + 1.0_dp) < tol_rotation_match .and. &
         abs(trace3(rotation) - 1.0_dp) < tol_rotation_match
  end function is_mirror


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

end module sympw_mulliken_cubic
