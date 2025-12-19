module groupkp 
  use accuracy
  use constants
  use bztest 
  implicit none
  private
  public :: sym_groupkp

contains

  subroutine sym_groupkp(kg, kgord, k2gord, kgel, kkgel, mtab2, ibz, listp, &
       & nopi, nopi1, nopli, nopli1, sil, til, ksym, rk, ark, a, ai, b,bi, u, order,pgnr, &
       & rgr, mtab, gel, steer, tsmall)
    integer, intent(inout) :: kg, kgord
    integer, intent(out) :: k2gord
    integer, intent(inout) :: kgel(:)
    integer, intent(inout) :: kkgel(:)
    integer, intent(inout) :: mtab2(:,:)
    integer, intent(out) :: ibz
    integer, intent(out) :: listp(:)

    integer, intent(out) :: nopi(:)
    integer, intent(out) :: nopi1
    integer, intent(out) :: nopli(:,:) 
    integer, intent(out) :: nopli1(:)
    complex(dp), intent(out) :: sil(:)
    real(dp), intent(out) :: til(:,:)
    integer, intent(out)  :: ksym

    real(dp), intent(in) :: rk(:)
    real(dp), intent(in) :: ark(:)
    real(dp), intent(in) :: a(:,:)
    real(dp), intent(in) :: ai(:,:)
    real(dp), intent(in) :: b(:,:)
    real(dp), intent(in) :: bi(:,:)
    real(dp), intent(in) :: u(:,:) ! (3, order)

    integer, intent(in) :: order
    integer, intent(in) :: pgnr
    real(dp), intent(in) :: rgr(:,:,:)
    integer, intent(in) :: mtab(:,:)
    integer, intent(in) :: gel(:)
    integer, intent(in) :: steer(:) ! steer(20)

    real(dp), intent(in) :: tsmall

    real(dp), dimension(3) :: srk
    integer :: II, K, I, J, K1, K2, I1
    real(dp), dimension(3) :: brk, robrk, rosk
    real(dp) :: TT, TTT
    integer :: ntz 
    integer, allocatable :: inverk(:)

    integer, dimension(230):: tmp_kgel ! tmp value , if bug, make it bigger

    !rk(1:3) = ark(1:3)*2*pi
    srk(1:3) = ark(1:3)
    write(*,*) "order", order
    allocate(inverk(order))
    inverk(:) = 1
    ! section 2.3
    kgord = 1
    tmp_kgel(1) = 1

    !todo check 
    ! brk is the wave vector in cartesian coordinates
    !brk(1:3) = matmul(b(1:3, 1:3), rk(1:3))

    do II = 1, 3
       brk(II) = dot_product(b(II, 1:3), rk(1:3))
    end do

    ! brk is the wave vector expressed in cartesian coordinates

    do K = 2, order
       !todo check
       do II = 1, 3
          robrk(II) = dot_product(rgr(1:3, II, K), brk(1:3))
       end do
       do II = 1, 3
          rosk(II) = dot_product(bi(1:3, II), robrk(1:3))
       end do

       ! robrk(1:3) = matmul(brk(1:3), rgr(1:3, 1:3, K))
       ! rosk(1:3) = matmul(robrk(1:3) , bi(1:3, 1:3))

       ! rosk is the rotated wave vector in reciprocal lattice coordinates

       rosk(1:3) = rosk(1:3) - rk(1:3)

       ! this difference should be a big k-vector, if the elements belongs to the group of the K-vector

       K1 = 1
       do while(K1 <= 3)

          TT = abs(rosk(K1))
          TTT = abs(TT - 2*pi)
          if ((TT > tsmall) .and. (TTT > tsmall)) then
             ! if this condition is fulfilled, rosk is not a big k-vector
             exit
          end if
          K1 = K1 + 1
       end do

       if (K1 > 3) then
          kgord = kgord + 1
          tmp_kgel(kgord) = K
       end if
    end do

    do I = 1, kgord
       kgel(I) = tmp_kgel(I)
    end do

    do I  = 1, kgord
       kkgel(I) = gel(kgel(I))
    end do

    kg = kgord

    ! The kth element of the group of the wave vector is the kkgel(K)th element of
    ! the complete set of point group operators

    do I = 1, kgord
       do J = 1, order
          mtab2(I, J) = mtab(kgel(I), J)
       end do
    end do

    do I = 1, kgord
       do J = 1, kgord
          mtab2(I, J) = mtab2(I, kgel(J))
       end do
    end do

    do I = 1, kgord
       inverk(kgel(I)) = I
    end do

    do I = 1, kgord
       do J = 1, kgord
          mtab2(I, J) = inverk(mtab2(I, J))
       end do
    end do
    write(*,*) "section 3"

    ! section 3
    ! tests for the nonsymmorphic space group
    if (.not. (steer(20) .ne. 0)) then
       ! section 3.1
       ! test if k-vector lies within the first brillouin zone

       ibz = 0
       call sym_bztest(ntz, srk, b)

       if (ntz > 0) then
          ! k - vector outside Bz
          write(*,*) rk(1:3), "outside the Bz"
          return
       end if

       if (ntz < 0) then
          ibz = 1
          if (steer(18) .eq. 1) then
             write(*,*) "Nonsymmorphic, but within Bz"
          end if
       end if

       ! section 3.2
       ! Test if the group of the k-vector is symmorphic
       ksym = 0
       I = 1
       do while (I <= kgord)
          I1 = kgel(I)
          K = 1
          do while (K <= 3)
             if (u(I1, K) .ne. 0) then
                exit
             end if
             K = K + 1
          end do
          if (K <= 3) then
             exit
          end if
          I = I + 1
       end do
       if (K > kgord) then
          ksym = 1
          if (steer(18) .eq. 1) then
             write(*,*) "Nonsymmorphic, but symmorph Gk"
          end if
       end if

       if ( (ibz .eq. 0) .and. (I <= kgord)) then
           write(*,*) "call factorgroup"
          call factorgroup(listp, k2gord, kgord, nopi, nopi1, nopli, nopli1, mtab2, til, &
               & sil, kgel, a, ai, b, u, rk, rgr )
       end if
    end if

    deallocate(inverk)

  end subroutine sym_groupkp
  
  ! formation of the factor group Gk/Tk
  
  ! Here we treat the case of a nonsymmorphic group of the k-vector, with the
  ! k-vector on one of the sides of the Brillouin zone boundaries. First, initiate
  ! the tables used to construct the factor group Gk/Tk. k2gord will be the order
  ! of this group. listp(I) contains the index of the pointgroup operator of the
  ! Ith element of this group. Several elements may have the same pointgroup
  ! operator, so that they differ only in the translation part til(I,K), K = 1:3
  ! and the corresponding exponential part sil(I). nopi(I) is the number of
  ! elements with the same Ith pointgroup operator. The Kth operator, which has
  ! I as its pointgroup operator is group element number nopli(I,K) in the factor
  ! group. nr(I) is the number of elements in the Ith row of the group
  ! multiplication table mtab3(I,K), which has been calculated. At the end all
  ! nr(I) = k2gord.
  !
  subroutine factorgroup(listp, k2gord, kgord, nopi, nopi1, nopli, nopli1,  &
          & mtab2, til, sil, kgel, a, ai, b, u, rk, rgr )

    integer, intent(out) :: listp(:)
    integer, intent(out) :: k2gord
    integer, intent(out) :: nopi(:)
    integer, intent(out) :: nopi1
    integer, intent(out) :: nopli(:,:) 
    integer, intent(out) :: nopli1(:)
    integer, intent(inout) :: mtab2(:,:)
    real(dp), intent(out) :: til(:,:)
    complex(dp), intent(out):: sil(:)

    integer, intent(inout) :: kgord

    integer, intent(in) :: kgel(:)
    real(dp), intent(in) :: a(:,:)
    real(dp), intent(in) :: ai(:,:)
    real(dp), intent(in) :: b(:,:)
    real(dp), intent(in) :: u(:,:)

    real(dp), intent(in) :: rk(:)
    real(dp), intent(in) :: rgr(:,:,:)


    integer, allocatable :: mtab3(:,:)
    integer :: I, II, K, K1, K2, NT
    complex(dp) :: sres, R4
    real(dp) :: ar
    integer :: N1, N2, N3
    integer :: index1, indk1

    real(dp), dimension(3) :: tsk, rprod
    integer, allocatable :: nr(:)

    k2gord = kgord

    allocate(mtab3(48, 48))
    allocate(nr(48))

    do I = 1, k2gord
       listp(I) = I
       K1 = kgel(I)
       til(I, 1:3) = u(K1, 1:3)
       nopi(I) = 1
       nopli(I, 1) = I
       mtab3(1, I) = I
       mtab3(I, 1) = I
       nr(I) = 1
       ar = dot_product(til(I, 1:3), rk(1:3))
       !write(*,*) "ar", ar
       sres = cmplx(0, -ar)
       sil(I) = exp(sres)
       !write(*,*) "sil", sil(I)
    end do
    nr(1) = k2gord
    I = 2

    do while (I <= k2gord)
       K = nr(I) + 1
       N1 = listp(I)
       N3 = kgel(N1)

       do while ( K <= k2gord)

          N2 = listp(K)
          index1 = mtab2(N1, N2)

          !todo check 
          do II = 1, 3
             tsk(II) = dot_product(a(II, 1:3), til(K, 1:3))
          end do

          do II = 1, 3
             rprod(II) =dot_product(rgr(1:3, II, N3), tsk(1:3))
          end do

          tsk(1:3) = rprod(1:3)
          do K1 = 1, 3
             rprod(K1) = dot_product(ai(1:3, K1), tsk(1:3))
             rprod(K1) = rprod(K1) + til(I, K1)
          end do

          ar = dot_product(rk(1:3), rprod(1:3))

          R4 = cmplx(0, -ar)
          sres = exp(R4)
          NT = nopi(index1)
          K1 = 1
          do while (K1 <= NT)
             indk1 = nopli(index1, K1)
             if (abs(sil(indk1) - sres) <= 0.05) then
                mtab3(I, K) = indk1
                nr(I) = nr(I) + 1
                exit
             end if
             K1 = K1 + 1
          end do

          if (K1 > NT) then
             k2gord = k2gord + 1
             mtab3(I, K) = k2gord
             nr(I) = nr(I) + 1
             mtab3(1, k2gord)  = k2gord
             nr(1) = k2gord
             mtab3(k2gord, 1) = k2gord
             nr(k2gord) = 1
             listp(k2gord) = index1
             til(k2gord, 1:3) = rprod(1:3)
             sil(k2gord) = sres
             nopi(index1) = nopi(index1) + 1
             K2 = nopi(index1)
             nopli(index1, K2) = k2gord
             if (I .ne. 2) then
                I = 2
                K = nr(I)
                N1 = listp(I)
                N3 = kgel(N1)
             end if
          end if
          K = K + 1
       end do
       I = I + 1
    end do

    kgord = k2gord
    nopi1 = nopi(1)
    nopli1(1:nopi1) = nopli(1, 1:nopi1)

    mtab2(1:kgord, 1:kgord) = mtab3(1:kgord, 1:kgord)

    deallocate(mtab3)
    deallocate(nr)

  end subroutine factorgroup

end module groupkp

