module sympw_mulliken
  use accuracy, only: dp, rotation_table_index, tol_irrep_phase, tol_rotation_match
  use sympw_mulliken_cubic, only: assign_cubic_mulliken_label
  implicit none
  private

  public :: assign_mulliken_label, mulliken_point_group_supported
  public :: mulliken_point_group_requires_pairing

contains

  subroutine assign_mulliken_label(point_group_number, operator_ids, rotation_table, &
       characters, label, success)
    integer, intent(in) :: point_group_number
    integer, intent(in) :: operator_ids(:)
    real(dp), intent(in) :: rotation_table(:,:,:)
    complex(dp), intent(in) :: characters(:)
    character(len=*), intent(out) :: label
    logical, intent(out) :: success

    integer :: inversion_index, c2_index, c2x_index, c2y_index, c2z_index
    integer :: mirror_index, mirror_x_index, mirror_y_index
    integer :: principal_index, secondary_index, diagonal_index, horizontal_index
    integer :: inversion_sign, c2_sign, c2x_sign, c2y_sign, c2z_sign
    integer :: mirror_sign, mirror_x_sign, mirror_y_sign
    integer :: irrep_dimension, parity_sign

    label = ""
    success = .false.
    if (size(operator_ids) /= size(characters) .or. size(characters) < 1) return
    if (size(rotation_table, 1) < 3 .or. size(rotation_table, 2) < 3) return
    if (.not. mulliken_point_group_supported(point_group_number)) return
    if (point_group_number == 2 .or. &
         (point_group_number >= 16 .and. point_group_number <= 31)) then
       if (size(rotation_table, 3) < 72 .or. any(operator_ids < 1) .or. &
            any(operator_ids > 24)) return
    else
       if (size(rotation_table, 3) < 48 .or. any(operator_ids < 1) .or. &
            any(operator_ids > 48)) return
    end if
    if (abs(aimag(characters(1))) > tol_irrep_phase) return
    irrep_dimension = nint(real(characters(1), dp))
    if (irrep_dimension < 1 .or. &
         abs(characters(1) - cmplx(real(irrep_dimension, dp), 0.0_dp, dp)) > &
         tol_irrep_phase) return
    if (point_group_number <= 8 .and. irrep_dimension /= 1) return

    select case(point_group_number)
    case(1)
       if (size(characters) /= 1) return
       label = "A"

    case(2)
       inversion_index = find_inversion(operator_ids, point_group_number, rotation_table)
       inversion_sign = character_sign(characters, inversion_index)
       if (inversion_sign == 0) return
       label = merge("Ag", "Au", inversion_sign > 0)

    case(3)
       c2_index = find_unique_c2(operator_ids, point_group_number, rotation_table)
       c2_sign = character_sign(characters, c2_index)
       if (c2_sign == 0) return
       label = merge("A", "B", c2_sign > 0)

    case(4)
       mirror_index = find_unique_mirror(operator_ids, point_group_number, rotation_table)
       mirror_sign = character_sign(characters, mirror_index)
       if (mirror_sign == 0) return
       if (mirror_sign > 0) then
          label = "A'"
       else
          label = "A''"
       end if

    case(5)
       inversion_index = find_inversion(operator_ids, point_group_number, rotation_table)
       c2_index = find_unique_c2(operator_ids, point_group_number, rotation_table)
       inversion_sign = character_sign(characters, inversion_index)
       c2_sign = character_sign(characters, c2_index)
       if (inversion_sign == 0 .or. c2_sign == 0) return
       if (c2_sign > 0) then
          label = merge("Ag", "Au", inversion_sign > 0)
       else
          label = merge("Bg", "Bu", inversion_sign > 0)
       end if

    case(6)
       c2x_index = find_c2_axis(operator_ids, point_group_number, rotation_table, 1)
       c2y_index = find_c2_axis(operator_ids, point_group_number, rotation_table, 2)
       c2z_index = find_c2_axis(operator_ids, point_group_number, rotation_table, 3)
       c2x_sign = character_sign(characters, c2x_index)
       c2y_sign = character_sign(characters, c2y_index)
       c2z_sign = character_sign(characters, c2z_index)
       if (.not. valid_d2_signs(c2x_sign, c2y_sign, c2z_sign)) return
       call label_d2(c2x_sign, c2y_sign, c2z_sign, label, success)
       if (.not. success) return

    case(7)
       c2z_index = find_c2_axis(operator_ids, point_group_number, rotation_table, 3)
       mirror_x_index = find_mirror_normal_axis(operator_ids, point_group_number, &
            rotation_table, 1)
       mirror_y_index = find_mirror_normal_axis(operator_ids, point_group_number, &
            rotation_table, 2)
       c2z_sign = character_sign(characters, c2z_index)
       mirror_x_sign = character_sign(characters, mirror_x_index)
       mirror_y_sign = character_sign(characters, mirror_y_index)
       if (c2z_sign == 0 .or. mirror_x_sign == 0 .or. mirror_y_sign == 0) return
       if (c2z_sign /= mirror_x_sign*mirror_y_sign) return
       if (c2z_sign > 0) then
          label = merge("A1", "A2", mirror_y_sign > 0)
       else
          label = merge("B1", "B2", mirror_y_sign > 0)
       end if

    case(8)
       inversion_index = find_inversion(operator_ids, point_group_number, rotation_table)
       c2x_index = find_c2_axis(operator_ids, point_group_number, rotation_table, 1)
       c2y_index = find_c2_axis(operator_ids, point_group_number, rotation_table, 2)
       c2z_index = find_c2_axis(operator_ids, point_group_number, rotation_table, 3)
       inversion_sign = character_sign(characters, inversion_index)
       c2x_sign = character_sign(characters, c2x_index)
       c2y_sign = character_sign(characters, c2y_index)
       c2z_sign = character_sign(characters, c2z_index)
       if (inversion_sign == 0 .or. &
            .not. valid_d2_signs(c2x_sign, c2y_sign, c2z_sign)) return
       call label_d2(c2x_sign, c2y_sign, c2z_sign, label, success)
       if (.not. success) return
       label = trim(label)//merge("g", "u", inversion_sign > 0)

    case(12)
       principal_index = find_c4_z(operator_ids, point_group_number, rotation_table)
       c2z_index = find_c2_axis(operator_ids, point_group_number, rotation_table, 3)
       secondary_index = find_c2_axis(operator_ids, point_group_number, rotation_table, 1)
       diagonal_index = find_c2_direction(operator_ids, point_group_number, &
            rotation_table, [1.0_dp, 1.0_dp, 0.0_dp])
       call label_d4_family(characters, irrep_dimension, principal_index, c2z_index, &
            secondary_index, diagonal_index, label, success)
       if (.not. success) return

    case(13)
       principal_index = find_c4_z(operator_ids, point_group_number, rotation_table)
       c2z_index = find_c2_axis(operator_ids, point_group_number, rotation_table, 3)
       secondary_index = find_mirror_normal_axis(operator_ids, point_group_number, &
            rotation_table, 2)
       diagonal_index = find_mirror_normal_direction(operator_ids, point_group_number, &
            rotation_table, [1.0_dp, -1.0_dp, 0.0_dp])
       call label_d4_family(characters, irrep_dimension, principal_index, c2z_index, &
            secondary_index, diagonal_index, label, success)
       if (.not. success) return

    case(14)
       principal_index = find_s4_z(operator_ids, point_group_number, rotation_table)
       c2z_index = find_c2_axis(operator_ids, point_group_number, rotation_table, 3)
       secondary_index = find_c2_axis(operator_ids, point_group_number, rotation_table, 1)
       diagonal_index = find_mirror_normal_direction(operator_ids, point_group_number, &
            rotation_table, [1.0_dp, -1.0_dp, 0.0_dp])
       call label_d4_family(characters, irrep_dimension, principal_index, c2z_index, &
            secondary_index, diagonal_index, label, success)
       if (.not. success) return

    case(15)
       inversion_index = find_inversion(operator_ids, point_group_number, rotation_table)
       principal_index = find_c4_z(operator_ids, point_group_number, rotation_table)
       c2z_index = find_c2_axis(operator_ids, point_group_number, rotation_table, 3)
       secondary_index = find_c2_axis(operator_ids, point_group_number, rotation_table, 1)
       diagonal_index = find_c2_direction(operator_ids, point_group_number, &
            rotation_table, [1.0_dp, 1.0_dp, 0.0_dp])
       parity_sign = scalar_character_sign(characters, inversion_index, irrep_dimension)
       if (parity_sign == 0) return
       call label_d4_family(characters, irrep_dimension, principal_index, c2z_index, &
            secondary_index, diagonal_index, label, success)
       if (.not. success) return
       label = trim(label)//merge("g", "u", parity_sign > 0)

    case(18:19)
       principal_index = find_c3_z(operator_ids, point_group_number, rotation_table)
       secondary_index = find_basal_c2(operator_ids, point_group_number, rotation_table)
       call label_d3_family(characters, irrep_dimension, principal_index, &
            secondary_index, label, success)
       if (.not. success) return

    case(20:21)
       principal_index = find_c3_z(operator_ids, point_group_number, rotation_table)
       secondary_index = find_vertical_mirror(operator_ids, point_group_number, rotation_table)
       call label_d3_family(characters, irrep_dimension, principal_index, &
            secondary_index, label, success)
       if (.not. success) return

    case(22:23)
       inversion_index = find_inversion(operator_ids, point_group_number, rotation_table)
       principal_index = find_c3_z(operator_ids, point_group_number, rotation_table)
       secondary_index = find_basal_c2(operator_ids, point_group_number, rotation_table)
       parity_sign = scalar_character_sign(characters, inversion_index, irrep_dimension)
       if (parity_sign == 0) return
       call label_d3_family(characters, irrep_dimension, principal_index, &
            secondary_index, label, success)
       if (.not. success) return
       label = trim(label)//merge("g", "u", parity_sign > 0)

    case(27)
       principal_index = find_c6_z(operator_ids, point_group_number, rotation_table)
       c2z_index = find_c2_axis(operator_ids, point_group_number, rotation_table, 3)
       secondary_index = find_c2_axis(operator_ids, point_group_number, rotation_table, 1)
       diagonal_index = find_c2_axis(operator_ids, point_group_number, rotation_table, 2)
       call label_d6_family(characters, irrep_dimension, principal_index, c2z_index, &
            secondary_index, diagonal_index, label, success)
       if (.not. success) return

    case(28)
       principal_index = find_c6_z(operator_ids, point_group_number, rotation_table)
       c2z_index = find_c2_axis(operator_ids, point_group_number, rotation_table, 3)
       secondary_index = find_mirror_normal_axis(operator_ids, point_group_number, &
            rotation_table, 2)
       diagonal_index = find_mirror_normal_axis(operator_ids, point_group_number, &
            rotation_table, 1)
       call label_d6_family(characters, irrep_dimension, principal_index, c2z_index, &
            secondary_index, diagonal_index, label, success)
       if (.not. success) return

    case(29:30)
       horizontal_index = find_mirror_normal_axis(operator_ids, point_group_number, &
            rotation_table, 3)
       principal_index = find_c3_z(operator_ids, point_group_number, rotation_table)
       secondary_index = find_basal_c2(operator_ids, point_group_number, rotation_table)
       parity_sign = scalar_character_sign(characters, horizontal_index, irrep_dimension)
       if (parity_sign == 0) return
       call label_d3_family(characters, irrep_dimension, principal_index, &
            secondary_index, label, success)
       if (.not. success) return
       if (parity_sign > 0) then
          label = trim(label)//"'"
       else
          label = trim(label)//"''"
       end if

    case(31)
       inversion_index = find_inversion(operator_ids, point_group_number, rotation_table)
       principal_index = find_c6_z(operator_ids, point_group_number, rotation_table)
       c2z_index = find_c2_axis(operator_ids, point_group_number, rotation_table, 3)
       secondary_index = find_c2_axis(operator_ids, point_group_number, rotation_table, 1)
       diagonal_index = find_c2_axis(operator_ids, point_group_number, rotation_table, 2)
       parity_sign = scalar_character_sign(characters, inversion_index, irrep_dimension)
       if (parity_sign == 0) return
       call label_d6_family(characters, irrep_dimension, principal_index, c2z_index, &
            secondary_index, diagonal_index, label, success)
       if (.not. success) return
       label = trim(label)//merge("g", "u", parity_sign > 0)

    case(34:36)
       call assign_cubic_mulliken_label(point_group_number, operator_ids, &
            rotation_table, characters, label, success)
       if (.not. success) return

    case default
       return
    end select

    success = len_trim(label) > 0
  end subroutine assign_mulliken_label


  logical function mulliken_point_group_supported(point_group_number) result(supported)
    integer, intent(in) :: point_group_number

    ! Cyclic trigonal/hexagonal groups and C4/S4/C4h conventionally merge
    ! complex-conjugate one-dimensional irreps into real E pairs, so their
    ! individual complex irreps keep fingerprints.
    select case(point_group_number)
    case(1:8, 12:15, 18:23, 27:31, 34:36)
       supported = .true.
    case default
       supported = .false.
    end select
  end function mulliken_point_group_supported


  logical function mulliken_point_group_requires_pairing(point_group_number) result(requires_pairing)
    integer, intent(in) :: point_group_number

    select case(point_group_number)
    case(9:11, 16:17, 24:26, 32:33)
       requires_pairing = .true.
    case default
       requires_pairing = .false.
    end select
  end function mulliken_point_group_requires_pairing


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


  integer function find_unique_c2(operator_ids, point_group_number, rotation_table) result(index_value)
    integer, intent(in) :: operator_ids(:), point_group_number
    real(dp), intent(in) :: rotation_table(:,:,:)

    integer :: element_index
    real(dp) :: rotation(3,3)

    index_value = 0
    do element_index = 1, size(operator_ids)
       call get_rotation(operator_ids(element_index), point_group_number, rotation_table, rotation)
       if (is_c2(rotation)) then
          if (index_value /= 0) then
             index_value = 0
             return
          end if
          index_value = element_index
       end if
    end do
  end function find_unique_c2


  integer function find_unique_mirror(operator_ids, point_group_number, rotation_table) result(index_value)
    integer, intent(in) :: operator_ids(:), point_group_number
    real(dp), intent(in) :: rotation_table(:,:,:)

    integer :: element_index
    real(dp) :: rotation(3,3)

    index_value = 0
    do element_index = 1, size(operator_ids)
       call get_rotation(operator_ids(element_index), point_group_number, rotation_table, rotation)
       if (is_mirror(rotation)) then
          if (index_value /= 0) then
             index_value = 0
             return
          end if
          index_value = element_index
       end if
    end do
  end function find_unique_mirror


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


  integer function find_mirror_normal_axis(operator_ids, point_group_number, rotation_table, &
       axis) result(index_value)
    integer, intent(in) :: operator_ids(:), point_group_number, axis
    real(dp), intent(in) :: rotation_table(:,:,:)

    real(dp) :: direction(3)

    index_value = 0
    direction = 0.0_dp
    if (axis < 1 .or. axis > 3) return
    direction(axis) = 1.0_dp
    index_value = find_mirror_normal_direction(operator_ids, point_group_number, &
         rotation_table, direction)
  end function find_mirror_normal_axis


  integer function find_mirror_normal_direction(operator_ids, point_group_number, &
       rotation_table, direction_in) result(index_value)
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
       if (.not. is_mirror(rotation)) cycle
       if (sqrt(sum((matmul(rotation, direction) + direction)**2)) < tol_rotation_match) then
          index_value = element_index
          return
       end if
    end do
  end function find_mirror_normal_direction


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


  integer function find_c3_z(operator_ids, point_group_number, rotation_table) result(index_value)
    integer, intent(in) :: operator_ids(:), point_group_number
    real(dp), intent(in) :: rotation_table(:,:,:)

    integer :: element_index
    real(dp) :: rotation(3,3), z_axis(3)

    index_value = 0
    z_axis = [0.0_dp, 0.0_dp, 1.0_dp]
    do element_index = 1, size(operator_ids)
       call get_rotation(operator_ids(element_index), point_group_number, rotation_table, rotation)
       if (.not. is_c3(rotation)) cycle
       if (sqrt(sum((matmul(rotation, z_axis) - z_axis)**2)) < tol_rotation_match) then
          index_value = element_index
          return
       end if
    end do
  end function find_c3_z


  integer function find_c6_z(operator_ids, point_group_number, rotation_table) result(index_value)
    integer, intent(in) :: operator_ids(:), point_group_number
    real(dp), intent(in) :: rotation_table(:,:,:)

    integer :: element_index
    real(dp) :: rotation(3,3), z_axis(3)

    index_value = 0
    z_axis = [0.0_dp, 0.0_dp, 1.0_dp]
    do element_index = 1, size(operator_ids)
       call get_rotation(operator_ids(element_index), point_group_number, rotation_table, rotation)
       if (.not. is_c6(rotation)) cycle
       if (sqrt(sum((matmul(rotation, z_axis) - z_axis)**2)) < tol_rotation_match) then
          index_value = element_index
          return
       end if
    end do
  end function find_c6_z


  integer function find_basal_c2(operator_ids, point_group_number, rotation_table) result(index_value)
    integer, intent(in) :: operator_ids(:), point_group_number
    real(dp), intent(in) :: rotation_table(:,:,:)

    integer :: element_index
    real(dp) :: rotation(3,3), z_axis(3)

    index_value = 0
    z_axis = [0.0_dp, 0.0_dp, 1.0_dp]
    do element_index = 1, size(operator_ids)
       call get_rotation(operator_ids(element_index), point_group_number, rotation_table, rotation)
       if (.not. is_c2(rotation)) cycle
       if (sqrt(sum((matmul(rotation, z_axis) + z_axis)**2)) < tol_rotation_match) then
          index_value = element_index
          return
       end if
    end do
  end function find_basal_c2


  integer function find_vertical_mirror(operator_ids, point_group_number, rotation_table) &
       result(index_value)
    integer, intent(in) :: operator_ids(:), point_group_number
    real(dp), intent(in) :: rotation_table(:,:,:)

    integer :: element_index
    real(dp) :: rotation(3,3), z_axis(3)

    index_value = 0
    z_axis = [0.0_dp, 0.0_dp, 1.0_dp]
    do element_index = 1, size(operator_ids)
       call get_rotation(operator_ids(element_index), point_group_number, rotation_table, rotation)
       if (.not. is_mirror(rotation)) cycle
       if (sqrt(sum((matmul(rotation, z_axis) - z_axis)**2)) < tol_rotation_match) then
          index_value = element_index
          return
       end if
    end do
  end function find_vertical_mirror


  subroutine get_rotation(operator_id, point_group_number, rotation_table, rotation)
    integer, intent(in) :: operator_id, point_group_number
    real(dp), intent(in) :: rotation_table(:,:,:)
    real(dp), intent(out) :: rotation(3,3)

    integer :: table_index

    rotation = 0.0_dp
    table_index = rotation_table_index(operator_id, point_group_number)
    if (table_index < 1 .or. table_index > size(rotation_table, 3)) return
    rotation = rotation_table(1:3, 1:3, table_index)
  end subroutine get_rotation


  logical function is_c2(rotation) result(matches)
    real(dp), intent(in) :: rotation(3,3)

    matches = abs(determinant3(rotation) - 1.0_dp) < tol_rotation_match .and. &
         abs(trace3(rotation) + 1.0_dp) < tol_rotation_match
  end function is_c2


  logical function is_c4(rotation) result(matches)
    real(dp), intent(in) :: rotation(3,3)

    matches = abs(determinant3(rotation) - 1.0_dp) < tol_rotation_match .and. &
         abs(trace3(rotation) - 1.0_dp) < tol_rotation_match
  end function is_c4


  logical function is_c3(rotation) result(matches)
    real(dp), intent(in) :: rotation(3,3)

    matches = abs(determinant3(rotation) - 1.0_dp) < tol_rotation_match .and. &
         abs(trace3(rotation)) < tol_rotation_match
  end function is_c3


  logical function is_c6(rotation) result(matches)
    real(dp), intent(in) :: rotation(3,3)

    matches = abs(determinant3(rotation) - 1.0_dp) < tol_rotation_match .and. &
         abs(trace3(rotation) - 2.0_dp) < tol_rotation_match
  end function is_c6


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


  logical function valid_d2_signs(c2x_sign, c2y_sign, c2z_sign) result(valid)
    integer, intent(in) :: c2x_sign, c2y_sign, c2z_sign

    valid = c2x_sign /= 0 .and. c2y_sign /= 0 .and. c2z_sign /= 0 .and. &
         c2x_sign*c2y_sign == c2z_sign
  end function valid_d2_signs


  subroutine label_d2(c2x_sign, c2y_sign, c2z_sign, label, success)
    integer, intent(in) :: c2x_sign, c2y_sign, c2z_sign
    character(len=*), intent(out) :: label
    logical, intent(out) :: success

    label = ""
    success = .true.
    if (c2x_sign > 0 .and. c2y_sign > 0 .and. c2z_sign > 0) then
       label = "A"
    else if (c2x_sign < 0 .and. c2y_sign < 0 .and. c2z_sign > 0) then
       label = "B1"
    else if (c2x_sign < 0 .and. c2y_sign > 0 .and. c2z_sign < 0) then
       label = "B2"
    else if (c2x_sign > 0 .and. c2y_sign < 0 .and. c2z_sign < 0) then
       label = "B3"
    else
       success = .false.
    end if
  end subroutine label_d2


  subroutine label_d4_family(characters, dimension, principal_index, c2z_index, &
       secondary_index, diagonal_index, label, success)
    complex(dp), intent(in) :: characters(:)
    integer, intent(in) :: dimension, principal_index, c2z_index
    integer, intent(in) :: secondary_index, diagonal_index
    character(len=*), intent(out) :: label
    logical, intent(out) :: success

    integer :: principal_sign, c2z_sign, secondary_sign, diagonal_sign

    label = ""
    success = .false.
    if (principal_index < 1 .or. c2z_index < 1 .or. secondary_index < 1 .or. &
         diagonal_index < 1) return
    if (dimension == 2) then
       if (abs(characters(principal_index)) > tol_irrep_phase .or. &
            abs(characters(c2z_index) + cmplx(2.0_dp, 0.0_dp, dp)) > tol_irrep_phase .or. &
            abs(characters(secondary_index)) > tol_irrep_phase .or. &
            abs(characters(diagonal_index)) > tol_irrep_phase) return
       label = "E"
       success = .true.
       return
    end if
    if (dimension /= 1) return

    principal_sign = character_sign(characters, principal_index)
    c2z_sign = character_sign(characters, c2z_index)
    secondary_sign = character_sign(characters, secondary_index)
    diagonal_sign = character_sign(characters, diagonal_index)
    if (principal_sign == 0 .or. c2z_sign /= 1 .or. secondary_sign == 0 .or. &
         diagonal_sign /= principal_sign*secondary_sign) return
    if (principal_sign > 0) then
       label = merge("A1", "A2", secondary_sign > 0)
    else
       label = merge("B1", "B2", secondary_sign > 0)
    end if
    success = .true.
  end subroutine label_d4_family


  subroutine label_d3_family(characters, dimension, principal_index, secondary_index, &
       label, success)
    complex(dp), intent(in) :: characters(:)
    integer, intent(in) :: dimension, principal_index, secondary_index
    character(len=*), intent(out) :: label
    logical, intent(out) :: success

    integer :: principal_sign, secondary_sign

    label = ""
    success = .false.
    if (principal_index < 1 .or. secondary_index < 1) return
    if (dimension == 2) then
       if (abs(characters(principal_index) + cmplx(1.0_dp, 0.0_dp, dp)) > &
            tol_irrep_phase .or. abs(characters(secondary_index)) > tol_irrep_phase) return
       label = "E"
       success = .true.
       return
    end if
    if (dimension /= 1) return

    principal_sign = character_sign(characters, principal_index)
    secondary_sign = character_sign(characters, secondary_index)
    if (principal_sign /= 1 .or. secondary_sign == 0) return
    label = merge("A1", "A2", secondary_sign > 0)
    success = .true.
  end subroutine label_d3_family


  subroutine label_d6_family(characters, dimension, principal_index, c2z_index, &
       secondary_index, diagonal_index, label, success)
    complex(dp), intent(in) :: characters(:)
    integer, intent(in) :: dimension, principal_index, c2z_index
    integer, intent(in) :: secondary_index, diagonal_index
    character(len=*), intent(out) :: label
    logical, intent(out) :: success

    integer :: principal_sign, c2z_sign, secondary_sign, diagonal_sign

    label = ""
    success = .false.
    if (principal_index < 1 .or. c2z_index < 1 .or. secondary_index < 1 .or. &
         diagonal_index < 1) return
    principal_sign = character_sign(characters, principal_index)
    if (dimension == 2) then
       if (principal_sign == 0 .or. abs(characters(secondary_index)) > tol_irrep_phase .or. &
            abs(characters(diagonal_index)) > tol_irrep_phase) return
       if (principal_sign > 0 .and. &
            abs(characters(c2z_index) + cmplx(2.0_dp, 0.0_dp, dp)) < tol_irrep_phase) then
          label = "E1"
       else if (principal_sign < 0 .and. &
            abs(characters(c2z_index) - cmplx(2.0_dp, 0.0_dp, dp)) < tol_irrep_phase) then
          label = "E2"
       else
          return
       end if
       success = .true.
       return
    end if
    if (dimension /= 1) return

    c2z_sign = character_sign(characters, c2z_index)
    secondary_sign = character_sign(characters, secondary_index)
    diagonal_sign = character_sign(characters, diagonal_index)
    if (principal_sign == 0 .or. c2z_sign /= principal_sign .or. secondary_sign == 0 .or. &
         diagonal_sign /= principal_sign*secondary_sign) return
    if (principal_sign > 0) then
       label = merge("A1", "A2", secondary_sign > 0)
    else
       label = merge("B1", "B2", secondary_sign > 0)
    end if
    success = .true.
  end subroutine label_d6_family


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

end module sympw_mulliken
