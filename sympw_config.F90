module sympw_config
  implicit none
  private
  public :: read_sympw_config

contains

  subroutine read_sympw_config(filename, poscar, kpoints, lmax, point_group, error_code)
    character(len=*), intent(in) :: filename
    character(len=*), intent(out) :: poscar, kpoints, point_group
    integer, allocatable, intent(out) :: lmax(:)
    integer, intent(out) :: error_code

    integer :: file_unit, ios, comment_pos, lmax_count
    integer :: lmax_values(100)
    character(len=256) :: line, keyword
    logical :: file_open, parse_ok

    poscar = ''
    kpoints = ''
    point_group = ''
    error_code = 0
    lmax_count = 0
    lmax_values = 0
    file_open = .false.

    open(newunit=file_unit, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) then
       call report_config_error(1, "Cannot open config file: "//trim(filename))
       return
    end if
    file_open = .true.

    do
       read(file_unit, '(A)', iostat=ios) line
       if (ios < 0) exit
       if (ios > 0) then
          call report_config_error(2, "Unable to read config file")
          return
       end if

       comment_pos = index(line, '#')
       if (comment_pos == 1) then
          line = ''
       else if (comment_pos > 1) then
          line = line(:comment_pos-1)
       end if
       line = adjustl(line)
       if (len_trim(line) == 0) cycle

       read(line, *, iostat=ios) keyword
       if (ios /= 0) then
          call report_config_error(2, "Invalid config line: "//trim(line))
          return
       end if

       select case(trim(keyword))
       case('POSCAR', 'POSCAR_FILE')
          poscar = extract_value(line, keyword)
       case('KPOINTS', 'KPOINTS_FILE')
          kpoints = extract_value(line, keyword)
       case('LMAX')
          call parse_lmax_values(line, lmax_values, lmax_count, parse_ok)
          if (.not. parse_ok) then
             call report_config_error(2, "Invalid LMAX line in config file")
             return
          end if
       case('POINTGROUP', 'POINT_GROUP')
          point_group = extract_value(line, keyword)
       end select
    end do

    close(file_unit)
    file_open = .false.

    if (len_trim(poscar) == 0) then
       call report_config_error(3, "POSCAR must be specified in config file")
       return
    end if
    if (len_trim(kpoints) == 0) then
       call report_config_error(4, "KPOINTS must be specified in config file")
       return
    end if

    if (lmax_count > 0) then
       allocate(lmax(lmax_count), stat=ios)
       if (ios /= 0) then
          call report_config_error(5, "LMAX allocation failed")
          return
       end if
       lmax(:) = lmax_values(1:lmax_count)
    end if

    call resolve_config_path(filename, poscar)
    call resolve_config_path(filename, kpoints)

  contains

    subroutine report_config_error(code, message)
      integer, intent(in) :: code
      character(len=*), intent(in) :: message
      integer :: close_ios

      if (file_open) then
         close(file_unit, iostat=close_ios)
         file_open = .false.
      end if
      if (allocated(lmax)) deallocate(lmax)
      error_code = code
      write(*,*) "Error: ", trim(message)
    end subroutine report_config_error

  end subroutine read_sympw_config

  function extract_value(line, keyword) result(value)
    character(len=*), intent(in) :: line, keyword
    character(len=256) :: value

    value = ''
    if (len_trim(line) <= len_trim(keyword)) return
    value = trim(adjustl(line(len_trim(keyword)+1:)))
  end function extract_value

  subroutine parse_lmax_values(line, values, count, ok)
    character(len=*), intent(in) :: line
    integer, intent(out) :: values(:), count
    logical, intent(out) :: ok

    character(len=256) :: keyword, remainder
    integer :: ios, index_char, nchar
    logical :: in_token

    values = 0
    count = 0
    ok = .false.
    read(line, *, iostat=ios) keyword
    if (ios /= 0 .or. trim(keyword) /= 'LMAX') return

    remainder = adjustl(line(len_trim(keyword)+1:))
    in_token = .false.
    nchar = len_trim(remainder)
    do index_char = 1, nchar
       if (remainder(index_char:index_char) == ' ' .or. &
            remainder(index_char:index_char) == char(9)) then
          in_token = .false.
       else if (.not. in_token) then
          count = count + 1
          in_token = .true.
       end if
    end do
    if (count < 1 .or. count > size(values)) then
       count = 0
       return
    end if

    read(remainder, *, iostat=ios) values(1:count)
    ok = (ios == 0)
  end subroutine parse_lmax_values

  subroutine resolve_config_path(config_filename, path_value)
    character(len=*), intent(in) :: config_filename
    character(len=*), intent(inout) :: path_value

    integer :: index_char, slash_pos

    if (len_trim(path_value) == 0) return
    if (path_value(1:1) == '/') return

    slash_pos = 0
    do index_char = 1, len_trim(config_filename)
       if (config_filename(index_char:index_char) == '/') slash_pos = index_char
    end do
    if (slash_pos > 0) then
       path_value = config_filename(1:slash_pos)//trim(path_value)
    end if
  end subroutine resolve_config_path

end module sympw_config
