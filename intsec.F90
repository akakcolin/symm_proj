module intsec
  use accuracy
  use eigensolver, only: heev
  implicit none
  private
  public :: sym_intsec

contains

  subroutine sym_intsec(fi, intersection_dim, nvr1, nvr2, G, dfi)
    complex(dp), intent(inout) :: fi(:,:)
    integer, intent(out) :: intersection_dim
    integer, intent(in) :: nvr1
    integer, intent(in) :: nvr2
    integer, intent(in) :: G
    complex(dp), intent(in) :: dfi(:,:)

    integer :: eigen_index, info, previous
    real(dp) :: vector_norm
    complex(dp) :: overlap_value
    real(dp), allocatable :: eigenvalues(:)
    complex(dp), allocatable :: candidate(:)
    complex(dp), allocatable :: gram(:,:)
    complex(dp), allocatable :: overlap(:,:)

    intersection_dim = 0
    if (nvr1 <= 0 .or. nvr2 <= 0) return
    if (nvr1 > size(dfi, 1) .or. G > size(dfi, 2)) return
    if (G > size(fi, 1) .or. nvr2 > size(fi, 2)) return

    allocate(overlap(nvr1, nvr2))
    allocate(gram(nvr1, nvr1))
    allocate(eigenvalues(nvr1))
    allocate(candidate(G))

    ! Unit singular values of U^H V span the intersection of orthonormal bases U and V.
    overlap = matmul(conjg(dfi(1:nvr1, 1:G)), fi(1:G, 1:nvr2))
    gram = matmul(overlap, transpose(conjg(overlap)))
    gram = 0.5_dp*(gram + transpose(conjg(gram)))

    call heev(gram, eigenvalues, info)
    if (info /= 0) then
       deallocate(overlap, gram, eigenvalues, candidate)
       return
    end if

    do eigen_index = nvr1, 1, -1
       if (abs(eigenvalues(eigen_index) - 1.0_dp) > tol_orthog) cycle

       candidate = matmul(transpose(dfi(1:nvr1, 1:G)), gram(:, eigen_index))
       do previous = 1, intersection_dim
          overlap_value = dot_product(fi(1:G, previous), candidate)
          candidate = candidate - overlap_value*fi(1:G, previous)
       end do

       vector_norm = sqrt(sum(abs(candidate)**2))
       if (vector_norm <= tol_zero) cycle

       intersection_dim = intersection_dim + 1
       fi(1:G, intersection_dim) = candidate/vector_norm
    end do

    if (intersection_dim < size(fi, 2)) then
       fi(:, intersection_dim + 1:) = cmplx(0.0_dp, 0.0_dp, kind=dp)
    end if

    deallocate(overlap, gram, eigenvalues, candidate)
  end subroutine sym_intsec

end module intsec
