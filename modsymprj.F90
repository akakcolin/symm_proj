module modsymprj
  use accuracy
  use constants 
  use groupkp
  use irrep
  use projmat
  use genera
  implicit none
  private
  public :: symprj

contains

  subroutine symprj(projmatrix, matrixorder, num_block, pgnr, a, r, u, nel, lmax, nat, kpoints,  &
       & Oh, D6h, MOh, MD6h, npgo, nge, ldrmm, rgr3, res, primen, steer, debug)
    complex(dp), intent(out):: projmatrix(:,:,:)
    integer, intent(in) :: matrixorder
    integer, intent(out):: num_block(:)
    integer, intent(in) :: pgnr
    integer, intent(in) :: primen(:)
    integer, intent(inout) :: steer(:)
    integer, intent(in) :: debug
    integer, intent(in) :: nel
    integer, intent(in) :: lmax(:)
    integer, intent(in) :: nat(:)
    integer, intent(in) :: MOh(:,:)
    integer, intent(in):: MD6h(:,:)
    integer, intent(in) :: npgo(:,:)
    integer, intent(in) :: nge(:)

    real(dp),intent(in) :: a(:,:)
    real(dp),intent(in) :: r(:,:,:)
    real(dp), intent(in) :: u(:,:)
    real(dp), intent(in) :: kpoints(:,:)

    real(dp), intent(in) :: Oh(:,:)
    real(dp), intent(in) :: D6h(:,:)
    real(dp), intent(in):: rgr3(:,:,:)
    complex(dp), intent(in) :: ldrmm(:,:)
    !complex(dp), intent(in) :: rcgr3(:,:,:)
    complex(dp), intent(in) :: res(:,:)

    integer :: I1, I10, I11, I12, I3, I4, I5, I6, I7, I8, I9
    integer :: ichem, ito, ifd
    integer :: nopi1
    integer :: I, J, K, K1, K2, IV
    integer :: K3, K4, K5, LD1, M2, mu1, mu2, KI, kg
    integer :: II, J1, JD, JJ, R5
    integer :: index1, ikp
    integer :: ilmax, indk1, isign, itotal
    integer :: order, first, uco
    integer :: kgord, k2gord
    integer :: tmp_dim ! temp value for maxvalue of np
    integer :: J2, LJ1, KJ
    integer :: nmberg
    integer :: index
    integer :: L, L1, L2
    integer :: N, N2, NT, nb, ntr, nup, NN1, NN2, NC
    integer :: nblock, ndi, nip, nfacto
    integer :: nal, N3, N31, N1, ncl
    integer :: K48
    integer :: number_of_wave_vectors
    integer :: ksym, ntz, ibz
    integer :: M1, ncoset

    real(dp) :: R1
    real(dp) :: ptrace, sumtot

    integer, dimension(100) :: npri
    logical :: is_ski

    real(dp) :: rh, rntr
    real(dp) :: tsmall, ttsmall
    real(dp) :: T, TT, TTT
    real(dp) :: ar
    real(dp) :: ep 
    real(dp) :: lsqsum

    real(dp), dimension(3) :: rk, ark, srk, brk, rosk, robrk
    real(dp), dimension(3,3) :: b, bi, ai
    real(dp), dimension(3) :: tsk
    real(dp), dimension(3) :: rprod
    real(dp), dimension(3) :: difi, dif
    real(dp), dimension(3) :: trac

    complex(dp) :: sres
    complex(dp) :: R4

    integer, allocatable :: listp(:)
    integer, allocatable :: gel(:)
    integer, allocatable :: kgel(:) 
    integer, allocatable :: kkgel(:)
    integer, allocatable :: map(:,:)
    integer, allocatable :: mtab(:,:)
    integer, allocatable :: mtab2(:,:)
    integer, allocatable :: ngen(:)
    integer, allocatable :: nalr(:)
    integer, allocatable :: nopli1(:)
    integer, allocatable :: nopli(:,:)
    integer, allocatable :: allow(:)
    integer, allocatable :: inver(:)
    integer, allocatable :: nopi(:)
    integer, allocatable :: laj(:)
    integer, allocatable :: np(:,:,:)
    integer, allocatable :: npl(:,:,:,:)
    integer, allocatable  :: cind_invp(:)

    real(dp), allocatable :: rgr(:,:,:)
    real(dp), allocatable :: inverk(:)
    real(dp), allocatable :: factor(:)
    real(dp), allocatable :: til(:,:) 
    real(dp), allocatable :: jdpk(:,:)
    real(dp), allocatable :: tmatri(:,:)
    real(dp), allocatable :: jdprod(:,:)
    real(dp), allocatable :: nvec(:,:,:,:,:)

    complex(dp), allocatable :: sil(:)
    complex(dp), allocatable :: jpdd(:,:,:)
    complex(dp), allocatable :: temp_matrix(:,:)

    !integer, allocatable :: column_index(:,:), row_index(:,:), column_index_tmp(:,:)
    integer :: col, I2
    integer :: shift_columns, binx, oinx
    real(dp) ::  D

    nopi1 = 1;
    ksym = 1;
    ntz = 0;


    if(debug .eq. 1) then
       write(*,*) "Rotation angles for group Oh"

       do I = 1, 48
          write(*,*) I, Oh(1:3, I)
       end do

       write(*,*) "Rotation angles for group D6h"

       do I = 1, 24
          write(*,*) I, D6h(1:3, I)
       end do


       write(*,*) "The group Oh multiplication table:"

       do I = 1, 24
          write(*,*) MOh(:, I)
       end do

       write(*,*) "The group D6h multiplication table:"

       do I = 1, 12
          write(*,*) MD6h(:, I)
       end do


       write(*,*) "npgo"
       write(*,*) npgo(1,:)
       write(*,*) npgo(2,:)   

       write(*,*) "The group elements of the 429 crystallographic point groups:"
       do I = 1, 429
          K = npgo(1,I)
          L = npgo(2,I)
          L2 = L + K -1
          write(*,*)  nge(L:L2)
       end do

       write(*,*) "Rotation/inverseion matrices"

       do I = 1, 72
          write(*,*) I
          do K1=1,3
             write(*,*) rgr3(K1,:,I)
          end do
       end do

       write(*,*)"One hundred prime numbers are calculated:"
       write(*,*) primen(:)

       write(*,*) "Input, crystal with unit cell vectors"
       do I = 1, 3
          write(*,*) a(I,:)
       end do

       write(*,*) "Reciprocal unit cell vectors are"
    end if

    T = 2*pi

    b(:,:) = a(:,:)
    call sym_matinv(b, 3)
    bi = transpose(a)
    ai = transpose(b)

    if(debug .eq. 1) then
       do I = 1, 3
          write(*,*) b(I, :)
       end do

       write(*,*) "pgnr, nel", pgnr, nel

       ! section 1.7
       write(*,*) 'The unit cell contains', nel, ' chemical elements\n'

    end if
    tsmall = 0.00001
    ttsmall = 0.000001

    allocate(np(nel, maxval(nat), maxval(nat)))
    allocate(nvec(nel, maxval(nat(:)), maxval(nat(:)), 100, 3))
    allocate(npl(nel, maxval(nat(:)), maxval(nat(:)), 100))
    allocate(til(72, 3))

    np(:,:,:)=0
    nvec(:,:,:,:,:)=0
    npl(:,:,:,:) = 0
    ! section 1.10

    ! section 1.11
    order = npgo(1, pgnr)
    first = npgo(2, pgnr)

    allocate(gel(order))

    gel(1:order) = nge(first:(first+order-1))

    npri(:) = primen(:)

    allocate(inver(maxval(gel(:))))
    inver(:)=0

    ! section 1.14
    if (( pgnr >=16) .and. (pgnr <=31)) then
       allocate(mtab(24, 24))
       allocate(mtab2(24, 24))
       mtab(:,:) = 0
       mtab(:,:) = MD6h(:,:)
       K48 = 48
       !write(*,*) 'The pointgroup (no. ', pgnr, ') of the crystal is a subgroup of D6h, with element:' 
    else
       allocate(mtab(48, 48))
       allocate(mtab2(48, 48))
       mtab(:,:) = 0
       mtab(:,:) = MOh(:,:)
       K48= 0
       !write(*,*) 'The pointgroup (no. ', pgnr, ') of the crystal is a subgroup of Oh, with element numbers:'     
    end if

    ! section 1.17
    if( (pgnr .ne. 31) .and. (pgnr .ne. 36)) then
       ! inver gives the numbering of the elements in the specific point
       ! group. Example: in the group 5=C2h, element 28 is the 4th
       ! element of the group, so inver(28)=4.
       do I = 1, order
          inver(gel(I))  = I
       end do

       do I = 1, order
          do J = 1, order
             mtab(I, J) = mtab(gel(I), gel(J))
          end do
       end do

       do I = 1, order
          do J = 1, order
             !write(*,*) mtab(I,J)
             mtab(I,J) = inver(mtab(I,J))
          end do
       end do
    end if


    ! section 1.18
    write(*,*) gel(1:order)
    write(*,*) "The maximum value for the orbital quantum number L is set to:", lmax(:)

    ! section 1.19
    ! load the rotation matrices for the orthogonal coordinate system

    allocate(rgr(3, 3, order))
    !allocate(mtab2(order, order))
    mtab2(:,:)=0

    do I = 1, order
       rgr(1:3, 1:3, I) = rgr3(1:3, 1:3, gel(I) + K48)
    end do
    ! section 1.20


    number_of_wave_vectors = size(kpoints(:,:), dim=1)
    !allocate(factor(nfacto))
    do I = 1, number_of_wave_vectors
       write(*,*) "all-points", kpoints(I,:)
    end do

    projmatrix(:,:,:) = 0

    allocate(temp_matrix(matrixorder, matrixorder))
    !allocate(row_index(maxval(lmax), nel))
    !allocate(column_index(maxval(lmax), nel))
    !allocate(column_index_tmp(maxval(lmax), nel))
    !column_index(1,1) = 1
    !row_index(1,1) = 1

    col = 1
    !do I = 1, nel
    !   J = nat(I)
    !   do K=2, lmax(I)
    !      l = j*(2*(K-2) + 1)
    !      col = col + l
    !      column_index(K, I) = col
    !      row_index(K, I) = col
    !   end do
    !   if( I < nel) then
    !      col = col + J*(2*lmax(I) + 1)
    !      column_index(1, I+1) = col
    !      row_index(1, I+1) = col
    !   end if
    !end do

    num_block(:) = 1
    do ikp = 1, number_of_wave_vectors
       !do I = 1, nel
       !   do II = 1, lmax(I)
       !      column_index_tmp(II, I) = column_index(II,I)
       !write(*,*)"column_index II I", II, I, column_index_tmp(II, I)
       !   end do
       !end do
       rk(1:3) = kpoints(ikp,:)
       write(*,*) "rk", rk(1:3)
       ark(1:3) = rk(1:3)
       srk(1:3) = rk(1:3)
       IV = 1
       ibz = 1   ! test

       allocate(nopli1(100))
       nopli1(:) = 1;
       allocate(nopli(order*2, 48))
       allocate(nopi(order*2))
       !allocate(listp(100))

       if( (rk(1) < tsmall) .and. (rk(2) < tsmall) .and. (rk(3) < tsmall)) then
          allocate(kgel(order))
          allocate(kkgel(order))
          allocate(sil(order))

          allocate(listp(order))
          ! rk(1:3) is gamma point
          ! section for the case of zero wave vector. then the point group of thw wave vector
          ! is equal to the point group of the space group
          kgord = order
          mtab2(1:kgord, 1:kgord) = mtab(1:kgord, 1:kgord)

          !do I = 1, kgord
          !   write(*,*)mtab(i,:)
          !end do

          do I = 1, kgord
             kgel(I) = I
             kkgel(I) = gel(I)
          end do
          ibz = 1
          kg = kgord
       else
          allocate(kgel(order))
          allocate(kkgel(order))
          allocate(sil(100))  ! temp value 
          allocate(listp(100))

          nopi(:) = 1

          !write(*,*) "rk", rk(1:3)
          sil(1)= cmplx(1, 0)
          kgel(1) = 1
          kgord= 1
          rk(1:3) = ark(1:3)*2*pi
          srk(1:3)= ark(1:3)

          call sym_groupkp(kg, kgord, k2gord, kgel, kkgel, mtab2, ibz, listp, &
               & nopi, nopi1, nopli, nopli1, sil, til, ksym, rk, ark, a, ai, b,bi, u, order,pgnr, &
               & rgr, mtab, gel, steer, tsmall)
       end if


       ! section 6
       ! calculation of the diagonal element of the irreducible representations
       ! section 6.1

       write(*,*) "Projection matrices for the wave vector" , srk(1:3)
       write(*,*) "The pointgroup of the wave vector consists of ", kg, " operators, indexed as numbers:"
       write(*,*) kkgel(1:kg)

       is_ski = ((steer(20) .ne. 0) .or. (ksym .ne. 0) .or. (ibz .ne. 0))

       !write(*,*) "is_ski, steer(20), ksym, ibz", is_ski, steer(20), ksym, ibz

       if (.not. is_ski) then
          write(*,*) " "
          write(*,*) "The factor group Gk/Tk consists of"
          do I = 1, kgord
             write(*,*) I, "pointgroup operator: ", kkgel(listp(I))
             write(*,*) "nonprimitive translation: (", til(I, 1:3), "), exp = ", sil(I)
          end do
       end if

       ! Now we have determined the point group of the wave vector, or (for
       ! the nonsymmorphic case with wave vector the BZ-boundary) the factor
       ! group Gk/Tk.

       !Next we determine all (allowable) irreducible representations of this finite group.

       allocate(cind_invp(kgord))
       if ((IV <= 2) .or. is_ski) then
          allocate(jpdd(kgord, kgord, kgord))
          allocate(laj(kgord))
          allocate(allow(kgord))
          allow(:) = 0

          jpdd(:,:,:)=0
          do I = 1, kgord
             cind_invp(I) = I
          end do
          ! section 6.2
          call sym_irrep(jpdd, allow, ncl, laj, cind_invp, kgord, mtab2, npri, steer, &
               & ibz, ksym, nopi1, nopli1, sil)

          allocate(nalr(ncl)) 
          if (steer(11) == 0) then
             exit
          end if
          ! section 6.3
          nup = 0
          nip = 0
          do I = 1, ncl
             nip = nip + laj(I)
             if (allow(I) .ne. 0) then
                nup = nup + laj(I)
             end if
          end do
          !write(*,*) "laj", laj(:)
          !write(*,*) "nup", nup
          !write(*,*) "allow", allow
          if ( nip .ne. nup) then
             nal = 0
             do I = 1, ncl
                if (allow(I) .ne. 0) then
                   nal = nal + 1
                   nalr(nal) = I
                end if
             end do
             write(*,*) "Allowed irreps for J = ", nalr(1:nal)
          end if

          nblock = 0
          do ilmax = 1, nel
             nblock = nblock + lmax(ilmax) + 1
          end do

          write(*,*) " "
          write(*,*) "There are ", nup , "projection matrices"
          write(*,*) "for the wave vector"
          write(*,*)" with index J for the irreps and index JD for the diagonal elements of the irrep."

          !write(*,*) "Each projection matrix is blockdiagonalized with respect to the indices ichem = 1, ", nel
          !write(*,*) "and L = ", lmax(1:nel), ", so each projection matrix consists of", nblock
          !write(*,*) "subblocks of sub-projection matrices along the main diagonal. "
          !write(*,*) "These are independently orthonormalised into suh-T-matrices, from which the "
          !write(*,*) "complete T-matrix can be constructed"


          !call sym_sumsets( np, nvec, npl, til, kgord, kgel, rgr, listp, &
          !     & a, ai, b, r, u, nel, nat, ksym, ibz, steer)

          ! Formation of the summation sets G(MU,NU). nup is the number of projection
          ! matrices for this wave vector. We form np(K,I,J), which is the number
          ! of elements of the group of the k-vector (groupk), for which the difference
          ! rgr*(coordinate of atom I) - (coordinate of atom J) of chemical element K,
          ! is a lattice vector. npl(1:nel,1:nat(I1),1:nat(I1),1:np(1:nel,1:nat(I1),
          ! 1:nat(I1))) gives the indices of these pointgroup operators.
          ! nvec(1:nel,1:nat(I1),1:nat(I1),1:np(1:nel,1:nat(I1),1:nat(I1)),1:3)
          ! gives the corresponding lattice vectors.

          !real(dp), dimension(3) :: difi, dif, trac

          !  integer :: I, I1, I2, I3, I4, I5, I6, I7, I8, II 
          !  integer :: J, isign, K, ifd

          !allocate(nvec(nel, nat(I1), nat(I1),tmp_dim, 3))

          I = 0
          do while (I < nel)
             I = I + 1
             ! I1 is here the index of the chemical element
             J = nat(I)
             ! JJ is the number of atoms of chemical element I1
             I1 = 0
             do while (I1 < J)
                I1 = I1 + 1
                !I1 is the atom row index
                np(I, I1, 1:J) = 0
                I2 = 0

!!!!!!!!!!!!!!!!!!!!!!!!!!!! need to check  whether is right !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                do while (I2 < kgord)
                   I2 = I2 + 1
                   ! I2 is the index of elements of groupk
                   I3 = I2

                   ! inverse point group operator
                   if (.not. ((steer(20) .ne. 0) .or. (ksym .ne. 0) .or. (ibz .ne. 0))) then
                      I4 = listp(I3)
                      I4 = kgel(I4)
                      !need to check 
                      trac(1:3) = matmul( a(1:3, 1:3), til(I2,1:3))
                      ! nonprimitive translations in cartesian coordinates
                   else

                      I5 = kgel(I2)
                      I4 = kgel(I3)
                      trac(1:3) = matmul(a(1:3, 1:3), u(I5,1:3))
                   end if

                   do I6= 1, 3
                      do I7 = 1, 3
                         ! space group transformation of the atom position vector
                         trac(I6) = trac(I6) + rgr(I6, I7, I4)*r(I7, I, I1)
                      end do
                   end do

                   I8 = 1
                   do while( I8 <= J)
                      ! i9 is the atom column index
                      do II = 1, 3
                         dif(II) = trac(II) - r(II, I, I8)
                      end do
                      !todo check
                      do II = 1, 3
                         difi(II) = ai(1,II)*dif(1) + ai(2, II)*dif(2) + ai(3,II)*dif(3)
                      end do
                      ! transform to lattice coordinates. Test if difi is a lattice vector

                      do II = 1, 3
                         isign = 1
                         if (difi(II) < 0) then
                            isign = -1
                         end if
                         D = isign*difi(II)

                         if (D>0) then ! fix(D)
                            ifd = floor(D)
                         else
                            ifd = ceiling(D)
                         end if

                         if ((D - ifd) >= 0.98) then
                            ifd = ifd + 1
                         end if
                         if (abs(D-ifd) > 0.02) then
                            exit
                         end if
                         difi(II) = isign*ifd
                      end do

                      if (abs(D-ifd) <= 0.02) then
                         np(I, I1, I8) = np(I, I1, I8) + 1
                         K = np(I, I1, I8)
                         npl(I, I1, I8, K) = I2
                         nvec(I, I1, I8, K, 1:3) = difi(1:3)
                         if (steer(18) .ne. 0) then
                            write(*,*) "np=(", I, I1, I8, "=", K, " npl=", I2, "nvec=", difi(1:3)
                         end if
                         exit
                      end if
                      I8 = I8 + 1
                   end do
                   if (I8 > J) then
                      write(*,*) "Wrong space group", I, I1, I2, I4
                      stop
                   end if
                end do
                if (I8 > J) then
                   exit
                end if
             end do
             if (I8 > J) then
                exit
             end if
          end do

          !end subroutine sym_sumsets

          call sym_projmat(laj, kgord, allow, jpdd, projmatrix(:,:, ikp), nvec, nat, lmax, np, nel, ncl, npl, &
               & kgel, kkgel, listp, steer, ksym, ibz, K48, ldrmm, rk, u, tsmall, ttsmall)

          num_block(ikp)=nup

          if (debug .eq. 1) then 
100          FORMAT(12(F7.3, F7.3))
             do I = 1, matrixorder
                write(*,100) projmatrix(:,I, ikp)
             end do
             write(*,*) "result"
          end if

          if (debug .eq. 1) then 
             ! reorder projmatrix using cind_invp
             shift_columns = matrixorder / size(cind_invp)
             temp_matrix(:,:) = projmatrix(:,:, ikp)
             cind_invp(1)=1
             cind_invp(2)=3
             cind_invp(3)=4
             cind_invp(4)=2
             do I = 1, nup
                oinx= cind_invp(I)*shift_columns
                binx = I*shift_columns
                write(*,*) "binx-shift_columns+1  is", binx-shift_columns+1
                write(*,*) "oinx-shift_columns+1  is", oinx-shift_columns+1
                projmatrix(:,binx-shift_columns+1:binx, ikp) = temp_matrix(:,oinx-shift_columns+1:oinx)
             end do
             !projmatrix(:,:, ikp) = temp_matrix(:,:)


             !if (debug .eq. 1) then 
             write(*,*) "shift_columns  and cind_invp"
             write(*,*) shift_columns
             write(*,*) cind_invp
             !end if
          end if

          deallocate(laj)
          deallocate(allow)
          deallocate(jpdd)
       end if
       deallocate(kgel)
       deallocate(kkgel)
       deallocate(nopli1)
       deallocate(sil)
       deallocate(nalr)
       deallocate(cind_invp)
       deallocate(nopi)
       deallocate(nopli)
       deallocate(listp)
    end do
    deallocate(np)
    deallocate(npl)
    deallocate(inver)
    deallocate(nvec)
    deallocate(gel)
    deallocate(mtab2)
    deallocate(mtab)
    deallocate(rgr)
    deallocate(til)
    !deallocate(row_index)
    !deallocate(column_index)
    !deallocate(column_index_tmp)
    deallocate(temp_matrix)
  end subroutine symprj

end module modsymprj
