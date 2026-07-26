module eigvec 
  use accuracy
  use constants
  use permu
  implicit none
  private
  public :: sym_eigvec 
contains

  ! calculates the eigenvectors of group element, corresponding th eigenvalue eval and projects these
  ! eigenvectors on the Jth irreducible subspace. The eigenvectors are stored in fi(1:G, 1:nvec)

  !The eigenvectors of group element IN, with eigenvalue lab are calculated. Function
  !permu is called to create eigenvectors, using the loop structure of element IN. The
  !eigenvectors are projected on the Jth irreducible eigenspace, using the projection
  !operator Sj. The resulting eigenvectors are orthonormalized to each other. If no
  !eigenvector corresponding to eigenvalue lab is found, nvec is set equal to 0.
  
  subroutine sym_eigvec(fi, nvec, elem, eval, J, inel, cind, ch, multab, G, steer)

    complex(dp), intent(out) :: fi(:,:)
    integer, intent(out) :: nvec

    integer, intent(in) :: elem
    complex(dp), intent(in) :: eval !! non-degenerate eigenvalue 
    integer, intent(in) :: J
    integer, intent(in) :: inel(:)
    integer, intent(in) :: cind(:)
    complex(dp), intent(in) :: ch(:,:)
    integer, intent(in) :: multab(:,:)
    integer, intent(in) :: G
    integer, intent(in) :: steer(:)


    integer :: K2, K4, K5, K6
    integer :: nvr, IND
    integer :: nml, numl
    integer ::  LPE
    integer ::  N
    real(dp) :: rnorm
    complex(dp) :: overlap, P

    integer, allocatable :: loopl(:)
    integer, allocatable :: lpstr(:)
    complex(dp), allocatable :: vec(:)
    complex(dp), allocatable :: vec2(:)

    allocate(loopl(G))
    allocate(lpstr(G))
    allocate(vec(G))
    allocate(vec2(G))

    call sym_permu(loopl, lpstr, numl, multab, G, inel, elem, steer)
    fi(:, :) = cmplx(0.0_dp, 0.0_dp, kind=dp)
    vec(:) = cmplx(0.0_dp, 0.0_dp, kind=dp)

    IND = 1
    nvr = 0
    ! numl  is the number of loops
    ! lpstr loopstructure
    ! loopl length of Kth loop
    do nml= 1, numl
       if(nml >1) then
          IND = IND + loopl(nml-1)
       end if

       if (abs(eval**loopl(nml) - cmplx(1.0_dp, 0.0_dp, kind=dp)) > tol_zero) cycle

       nvr = nvr + 1
       vec(:) = cmplx(0.0_dp, 0.0_dp, kind=dp)
       K2 = lpstr(IND)
       vec(K2) = cmplx(1.0_dp, 0.0_dp, kind=dp)
       P = cmplx(1.0_dp, 0.0_dp, kind=dp)
       P = P*eval
       LPE = IND + loopl(nml)
       do N = IND+1, LPE-1
          K2 = lpstr(N)
          vec(K2) = P
          P = P*eval
       end do
       ! Projection SJ*vec
       do K4 = 1, G
          do K5 = 1, G
             K6 = inel(K5)
             K6 = multab(K4, K6)
             K6 = cind(K6)
             fi(K4, nvr) = fi(K4, nvr) + conjg(ch(J,K6))*vec(K5)
          end do
       end do
    end do

    nvec = 0
    do nml = 1, nvr
       vec2(1:G) = fi(1:G, nml)
       do K2 = 1, nvec
          overlap = dot_product(fi(1:G, K2), vec2(1:G))
          vec2(1:G) = vec2(1:G) - overlap*fi(1:G, K2)
       end do

       rnorm = sqrt(sum(abs(vec2(1:G))**2))
       if (rnorm > tol_zero) then
          nvec = nvec + 1
          fi(1:G, nvec) = vec2(1:G)/rnorm
       end if
    end do
    if (nvec < size(fi, 2)) fi(:, nvec + 1:) = cmplx(0.0_dp, 0.0_dp, kind=dp)

    deallocate(loopl)
    deallocate(lpstr)
    deallocate(vec)
    deallocate(vec2)
  end subroutine sym_eigvec

end module eigvec
