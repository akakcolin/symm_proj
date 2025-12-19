module eigensolver
  use accuracy
  use constants
  implicit none
  private
  public :: geev

!  !!* Simple eigensolver for a general matrix
!  !!* @param a contains the matrix for the solver, returns eigenvalues if
!  !!* requested  
!  !!* @param w eigenvalues
!  !!* @param vr 
!  !!* @param vl 
!  !!* @param jobz compute eigenvalues 'N' or eigenvalues and eigenvectors 'V'
  !  !!* @caveat the matrix a is overwritten
  interface geev
    module procedure real_sgeev
    module procedure dble_dgeev
    module procedure cmplx_cgeev
    module procedure dblecmplx_zgeev
  end interface geev

contains
  
  subroutine real_sgeev(a,w,vr,vl)
    implicit none
    ! Real symmetric matrices
    real(rsp), intent(inout) :: a(:,:)
    real(rsp), intent(out) :: vr(:,:), vl(:,:)
    !    ! Eigenvalues
    complex(rsp), intent(out) :: w(:)
    real(rsp), allocatable :: wl(:), wr(:)
    real(rsp), allocatable :: work(:)
    integer n, lda, info
    integer shapea(2), shapew(1)
    integer :: int_idealwork
    real(rsp) :: idealwork(1)
    shapea=shape(a)
    shapew=shape(w)

    n=shapea(1)

    lda=shapea(1)
    allocate(wr(n))
    allocate(wl(n))
    call SGEEV('V', 'V', n, a, lda, wr, wl, vr, n, vl, n, idealwork, -1, info)
    if (info/=0) then
       write(*,*) "Failue in SGEEV to determine optimum workspace"
    endif
    int_idealwork=Int(idealwork(1))
    allocate(work(int_idealwork))
    call SGEEV('V', 'V', n, a, lda, wr, wl, vr, n, vl, n, work, int_idealwork, info)
    if (info/=0) then
       if (info<0) then
          write(*,*) 'Failure in diagonalisation routine sgeev'
       else
          write(*,*) 'Failure in diagonalisation routine sgeev. diagonal element did not converge to zero.'
       endif
    endif
    deallocate(work)
    w=cmplx(wr,wl)
    deallocate(wr)
    deallocate(wl)
  end subroutine real_sgeev

  subroutine dble_dgeev(a,w,vr,vl)
    implicit none
    ! Real symmetric matrices
    real(rdp), intent(inout) :: a(:,:)
    real(rdp), intent(out) :: vr(:,:), vl(:,:)
    ! Eigenvalues
    complex(rdp), intent(Out) :: w(:)
    real(rdp), allocatable :: wl(:), wr(:)
    real(rdp), allocatable :: work(:)
    integer n, lda, info
    integer shapea(2), shapew(1)
    integer :: int_idealwork
    real(rdp) :: idealwork(1)
    shapea=shape(a)
    shapew=shape(w)
    n=shapea(1)
    lda=shapea(1)
    allocate(wr(n))
    allocate(wl(n))
    call DGEEV('V', 'V', n, a, lda, wr, wl, vr, n, vl, n, idealwork, -1, info)
    if (info/=0) then
       write(*,*) "Failue in DGEEV to determine optimum workspace"
    endif
    int_idealwork=Int(idealwork(1))
    allocate(work(int_idealwork))

    call DGEEV('V', 'V', n, a, lda, wr, wl, vr, n, vl, n, work, int_idealwork, info)
    
    if (info/=0) then
       if (info<0) then
          write(*,*)'Failure in diagonalisation routine dgeev'
       else
          write(*,*)'Failure in diagonalisation routine sgeev. diagonal element did not converge to zero.'
       endif
    endif
    deallocate(work)
    w=cmplx(wr,wl)
    deallocate(wr)
    deallocate(wl)
  end subroutine dble_dgeev

  subroutine cmplx_cgeev(a,w,vr,vl)
    implicit none
    ! Real symmetric matrices
    complex(rsp), intent(inout) :: a(:,:)
    complex(rsp), intent(out) :: vr(:,:), vl(:,:)
    ! Eigenvalues
    complex(rsp), intent(out) :: w(:)
    complex(rsp), allocatable :: work(:)
    real(rsp), allocatable :: rwork(:)
    integer n, lda, info
    integer shapea(2), shapew(1)
    integer :: int_idealwork
    complex(rsp) :: idealwork(1)
    shapea=shape(a)
    shapew=shape(w)
    n=shapea(1)
    lda=shapea(1)
    
    allocate(rwork(2*n))
    
    call CGEEV('V', 'V', n, a, lda, w, vr, n, vl, n, idealwork, -1, rwork, info)
    if (info/=0) then
       write(*,*)"Failue in CGEEV to determine optimum workspace"
    endif
    int_idealwork=Int(idealwork(1))
    allocate(work(int_idealwork))
    
    call CGEEV('V', 'V', n, a, lda, w, vr, n, vl, n, work, int_idealwork, rwork, info)
    
    if (info/=0) then
       if (info<0) then
          write(*,*)'Failure in diagonalisation routine cgeev, illegal argument at position '
       else
          write(*,*) 'Failure in diagonalisation routine cgeev, diagonal element did not converge to zero.'
       endif
    endif
    deallocate(rwork)
    deallocate(work)
  end subroutine cmplx_cgeev

  subroutine dblecmplx_zgeev(a,w,vr,vl)
    implicit None
    ! Real symmetric matrices
    complex(rdp), intent(InOut) :: a(:,:)
    complex(rdp), intent(Out) :: vr(:,:), vl(:,:)
    ! Eigenvalues
    complex(rdp), intent(Out) :: w(:)
    complex(rdp), allocatable :: work(:)
    real(rdp), allocatable :: rwork(:)
    integer n, lda, info
    integer shapea(2), shapew(1)
    integer :: int_idealwork
    complex(rdp) :: idealwork(1)
    shapea=shape(a)
    shapew=shape(w)
    n=shapea(1)
    lda=shapea(1)
    allocate(rwork(2*n))
    call ZGEEV('V', 'V', n, a, lda, w, vr, n, vl, n, idealwork, -1, rwork, info)
    if (info/=0) then
       write(*,*) "Failue in ZGEEV to determine optimum workspace"
    endif
    int_idealwork=Int(idealwork(1))
    allocate(work(int_idealwork))
    call ZGEEV('V', 'V', n, a, lda, w, vr, n, vl, n, work, int_idealwork, rwork, info)
    
    if (info/=0) then
       if (info<0) then
          write(*,*) 'Failure in diagonalisation routine zgeev, illegal argument at position '         
       else
          write(*,*) 'Failure in diagonalisation routine zgeev diagonal element did not converge to zero.'
       endif
    endif

    deallocate(rwork)
    deallocate(work)
  end subroutine dblecmplx_zgeev

end module eigensolver
