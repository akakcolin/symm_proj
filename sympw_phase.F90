module sympw_phase
  use accuracy, only: dp
  implicit none
  private

  public :: bloch_phase

contains

  ! Return exp[i k_phase . (nonsymmorphic_shift - lattice_shift)].
  ! k_phase is the fractional reciprocal vector multiplied by 2*pi.
  pure complex(dp) function bloch_phase(k_phase, lattice_shift, &
                                        nonsymmorphic_shift) result(phase)
    real(dp), intent(in) :: k_phase(3)
    real(dp), intent(in) :: lattice_shift(3)
    real(dp), intent(in), optional :: nonsymmorphic_shift(3)

    real(dp) :: phase_angle

    phase_angle = -dot_product(k_phase, lattice_shift)
    if (present(nonsymmorphic_shift)) then
       phase_angle = phase_angle + dot_product(k_phase, nonsymmorphic_shift)
    end if
    phase = exp(cmplx(0.0_dp, phase_angle, kind=dp))
  end function bloch_phase

end module sympw_phase
