module sympw_group_mode
  implicit none
  private

  public :: projective_factor_group_active

contains

  pure logical function projective_factor_group_active(space_group_fallback, &
                                                        ksym, ibz) result(active)
    integer, intent(in) :: space_group_fallback
    integer, intent(in) :: ksym
    integer, intent(in) :: ibz

    active = space_group_fallback == 0 .and. ksym == 0 .and. ibz == 0
  end function projective_factor_group_active

end module sympw_group_mode
