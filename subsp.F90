module subsp 
  use accuracy
  use constants
  use eigvec
  use intsec
  use eigensolver, only: geev 
  implicit none
  private
  public ::sym_subsp

contains
 
  ! find an irreducible vector, if possible, within
  ! a certain subspace of the irreducible space
  !
  !In case the degeneracy is not split using group elements of the maximal abelian
  !subgroup stored in kelem(1:nvct), but for dfi(1:nvr1,1:G) also constitute an invariant set
  !of columns. Then it might be possible to remove the degeneracy, using this subspace of
  !the irreducible space. The group elements except those stored in kelem(1:nvct) for
  !which dfi(1:nvr1,1:G) constitute an invariant space are stored in ninv(1:N). The group
  !elements stored in ninv(1:N) are diagonalized in the representation spanned by
  !dfi(1:nvr1,1:G). If the corresponding eigenvalues are not fully degenerate the
  !degeneracy is at least partly removed. If it is possible to fully remove the degeneracy
  !within function subsp, the control is returned to function degen and then to function
  !repres; otherwise a call is made to function mgt.
  subroutine sym_subsp(dfi, nvr, kelem, nvct, J, LJ1, G, inel, multab, steer, cind, ch, norder)
    complex(dp), intent(inout) :: dfi(:,:)
    integer, intent(out) :: nvr

    integer, intent(in) :: kelem(:)
    integer, intent(in) :: nvct
    integer, intent(in) :: J
    integer, intent(in) :: LJ1
    integer, intent(in) :: G
    integer, intent(in) :: inel(:)
    integer, intent(in) :: multab(:,:)
    integer, intent(in) :: steer(:)
    integer, intent(in) :: cind(:)
    complex(dp), intent(in) :: ch(:,:)
    integer, intent(in) :: norder(:)

    integer :: I, II, I1, I2, III
    integer :: J1, J2
    integer :: N, L, K, K1, K2, K5
    integer :: kloop, nvr1, nvr2 
    real(dp) :: delta_wr
    real(dp) :: rin 
    complex(dp) :: LAB
    complex(dp) :: pi2i
    integer, allocatable :: ninv(:)
    real(dp), allocatable :: vec(:)
    real(dp), allocatable :: vec2(:)
    real(dp), allocatable :: subm(:,:)
    real(dp), allocatable :: vr(:,:), vl(:,:)
    real(dp), allocatable :: eigenv(:)
    complex(dp), allocatable :: wr(:)
    complex(dp), allocatable :: fi(:,:)

    allocate(ninv(G))
    allocate(vec(G))
    allocate(fi(G, G))

    allocate(vec2(nvr))
    allocate(subm(nvr, nvr))
    allocate(wr(nvr))
    allocate(vr(nvr, nvr))
    allocate(vl(nvr, nvr))
    allocate(eigenv(nvr))

    pi2i = cmplx(0, 2*pi)
    N=0
    I=1
    nvr1 = nvr
    
    ninv(:) = 0.0
    
    ! Operate with the regular representatives on the eigenspace dfi(1:nvr, 1:G)
    ! to decide for which operators, except those in kelem(1:nvct)
    ! dfi(1:nvr, 1:G) is an invariant subspace
    write(*,*) "subsp 80 lines"
    
    do L =1, G
       if(kelem(I) .ne. L) then
          do J1=1, nvr1
             K = inel(L)
             do II = 1, G
                vec(II) = dfi(J1, multab(K, II))
             end do
             
             !vec(1:G) = dfi(J1, multab(K, 1:G))
             vec2(1:nvr1) = matmul(conjg(dfi(1:nvr1, 1:G)), vec)
             do I1 = 1, G
                rin = vec(I1)
                do J2 = 1, nvr1
                   rin = rin - vec2(J2)*dfi(J2,I1)
                end do
                vec(I1) = rin
             end do
             I1 = 1
             do while(I1 < G)
                if(abs(vec(I1)) >= 0.001) then 
                   exit
                end if
                I1 = I1 + 1
             end do
             if (I1 <= G) then
                exit
             end if
          end do
          write(*,*) "subsp 110 lines"
          if(I1 > G) then
             N = N + 1
             ninv(N) = L
          end if
       else
          if( I < nvct) then
             I = I + 1
          end if
       end if
    end do

    if (N .ne. 0) then
       do L =1, N
          K = ninv(L)
          K1 = inel(K)
          do I1 = 1, nvr1
             do II = 1, G
                vec(II) = dfi(I1, multab(K1, II))
             end do

             do III=1, nvr1
                subm(III, I1) = dot_product(conjg(dfi(III, 1:G)), vec(1:G))
             end do
             !subm(1:nvr1, I1) = matmul(conjg(dfi(1:nvr1, 1:G)), transpose(vec(1:G)) 
          end do

          write(*,*) "geev"
          write(*,*) "subm", subm
          
          call geev(subm, wr, vr, vl)
          write(*,*) "wr"
          write(*,*) wr
          write(*,*) "vr"
          write(*,*) vr
          write(*,*) "vl"
          write(*,*) vl
          write(*,*) "end geev"
          I1 = 1
          do while(I1 <= (nvr1-1))
             I2 = I1 + 1
             delta_wr = abs(wr(I1) - wr(I2))
             if (delta_wr > 0.001) then
                exit
             end if
             I1 = I1 + 1
          end do
          if(I1 < nvr1) then
             K2 = cind(K)
             K2 = norder(K2)
             do I1 = 1, K2
                eigenv(I1) = exp(pi2i*(I1-1)/ K2)
             end do
             J1 = 1
             do while( J1 <= K2)
                if(abs(wr(1) - eigenv(J1)) < 0.001) then
                   LAB = eigenv(J1)
                   exit
                end if
                J1 = J1 + 1
             end do
             if (J1 <= K2) then
                kloop = 0
                call sym_eigvec(fi, nvr2, K, LAB, J, kloop, inel, cind, ch, multab, G, steer)
                call sym_intsec(fi, K5, nvr1, nvr2, G, dfi)
                ![fi, nvr2] = eigvec
                ![f1, k5] = intsec(nvr1, nvr2, G, fi, dfi)
                if(K5 .ne. 0) then
                   nvr1 = K5

                   dfi(1:nvr1, 1:G) = transpose(fi(1:G,1:nvr1))
                   if (nvr1 == LJ1) then
                      exit
                   end if
                end if
             end if
          end if
       end do
       nvr = nvr1
    end if

    deallocate(vec)
    deallocate(vec2)
    deallocate(ninv)
    deallocate(subm)
    deallocate(wr)
    deallocate(vr)
    deallocate(vl)
    deallocate(eigenv)
    deallocate(fi)
  end subroutine sym_subsp
end module subsp




