module bztest 
  use accuracy
  use constants
  implicit none
  private
  public ::sym_bztest

contains
 
  ! determines if vector rk lies within or at the border of the first Brillouin zone
  ! rk-components of reciprocal lattice vectors
  ! (b1, b2,b3) = (x,y,z)*b
  ! cart is the cartesian components of rk
  ! ntz = -1 if within the brillouin zone
  ! ntz = 0 if on the border of the first Brillouin zone
  ! ntz = 1 if outside of the brillouin zone
  
  subroutine sym_bztest(ntz, rk, b)
    integer, intent(out):: ntz

    real(dp), intent(in) :: rk(:)
    real(dp), intent(in) :: b(:,:)

    integer :: n1, n2, n3
    real(dp) :: cark(3), gvec(3), nred(3)
    real(dp) :: zero_norm, shifted_norm, metric_tol
    logical :: on_boundary

    cark(1:3) = matmul(b(1:3, 1:3), rk(1:3))
    zero_norm = sum(cark(1:3)**2)
    metric_tol = 1.0e-10_dp * max(1.0_dp, zero_norm)
    on_boundary = .false.

    do n1 = -3, 3
       do n2 = -3, 3
          do n3 = -3, 3
             if (n1 == 0 .and. n2 == 0 .and. n3 == 0) cycle

             nred(:) = [real(n1, dp), real(n2, dp), real(n3, dp)]
             gvec(1:3) = matmul(b(1:3, 1:3), nred(1:3))
             shifted_norm = sum((cark(1:3) - gvec(1:3))**2)

             if (zero_norm - shifted_norm > metric_tol) then
                ntz = 1
                return
             end if
             if (abs(zero_norm - shifted_norm) <= metric_tol) then
                on_boundary = .true.
             end if
          end do
       end do
    end do

    if (on_boundary) then
       ntz = 0
    else
       ntz = -1
    end if
  end subroutine sym_bztest
end module bztest
