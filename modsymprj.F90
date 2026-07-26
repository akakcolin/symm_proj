module modsymprj
  use accuracy
  use constants 
  use sympw_group_mode, only: projective_factor_group_active
  use groupkp
  use irrep
  use projmat
  use sumsets, only: sym_sumsets
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

    integer :: nopi1
    integer :: I, J, K, K1, K2, IV
    integer :: kg
    integer :: ikp
    integer :: order, first
    integer :: kgord, k2gord
    integer :: L, L2
    integer :: nup
    integer :: nip
    integer :: nal, ncl
    integer :: number_of_wave_vectors
    integer :: ksym, ntz, ibz

    integer, dimension(100) :: npri
    logical :: factor_group_ok, projective_mode

    real(dp) :: tsmall, ttsmall

    real(dp), dimension(3) :: rk, ark, srk
    real(dp), dimension(3,3) :: b, bi, ai

    integer, allocatable :: listp(:)
    integer, allocatable :: gel(:)
    integer, allocatable :: kgel(:) 
    integer, allocatable :: kkgel(:)
    integer, allocatable :: mtab(:,:)
    integer, allocatable :: mtab2(:,:)
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
    real(dp), allocatable :: til(:,:) 
    real(dp), allocatable :: nvec(:,:,:,:,:)

    complex(dp), allocatable :: sil(:)
    complex(dp), allocatable :: jpdd(:,:,:)
    complex(dp), allocatable :: temp_matrix(:,:)

    integer :: shift_columns, binx, oinx

    nopi1 = 1;
    ksym = 1;
    ntz = 0;


    if(debug .eq. 1) then
       write(*,*)
       write(*,*) "=========================================="
       write(*,*) "Rotation Angles for Group Oh"
       write(*,*) "=========================================="
       write(*,*) "Element    Phi            Theta          Psi"
       do I = 1, 48
          write(*,'(I5,3F15.10)') I, Oh(1:3, I)
       end do

       write(*,*)
       write(*,*) "=========================================="
       write(*,*) "Rotation Angles for Group D6h"
       write(*,*) "=========================================="
       write(*,*) "Element    Phi            Theta          Psi"
       do I = 1, 24
          write(*,'(I5,3F15.10)') I, D6h(1:3, I)
       end do


       write(*,*)
       write(*,*) "=========================================="
       write(*,*) "Group Oh Multiplication Table"
       write(*,*) "=========================================="
       write(*,*) "Row x Column = Result"
       write(*,*)
       do I = 1, 24
          write(*,'(48I3)') MOh(:, I)
       end do

       write(*,*)
       write(*,*) "=========================================="
       write(*,*) "Group D6h Multiplication Table"
       write(*,*) "=========================================="
       write(*,*) "Row x Column = Result"
       write(*,*)
       do I = 1, 12
          write(*,'(24I3)') MD6h(:, I)
       end do


       write(*,*)
       write(*,*) "=========================================="
       write(*,*) "Point Group Statistics"
       write(*,*) "=========================================="
       write(*,*) "Number of point groups by order:"
       write(*,'(A,36I4)') "  Order:  ", npgo(1,1:36)
       write(*,'(A,36I4)') "  Count:  ", npgo(2,1:36)

       write(*,*)
       write(*,*) "=========================================="
       write(*,*) "Group Elements of 429 Point Groups"
       write(*,*) "=========================================="
       do I = 1, 429
          K = npgo(1,I)
          L = npgo(2,I)
          L2 = L + K -1
          if (K <= 12) then
             write(*,'(A,I4,A,I3,A,12I4)') "Group", I, " (order", K, "):", nge(L:L2)
          else if (K <= 24) then
             write(*,'(A,I4,A,I3,A,24I4)') "Group", I, " (order", K, "):", nge(L:L2)
          else
             write(*,'(A,I4,A,I3,A,48I4)') "Group", I, " (order", K, "):", nge(L:L2)
          end if
       end do

       write(*,*)
       write(*,*) "=========================================="
       write(*,*) "Rotation/Inversion Matrices"
       write(*,*) "=========================================="
       do I = 1, 72
          write(*,'(A,I3)') "Matrix ", I
          do K1=1,3
             write(*,'(3F12.6)') real(rgr3(K1,:,I))
          end do
          write(*,*)
       end do

       write(*,*)
       write(*,*) "=========================================="
       write(*,*) "Prime Numbers (first 100)"
       write(*,*) "=========================================="
       do I = 1, 100, 10
          write(*,'(10I7)') primen(I:min(I+9, 100))
       end do

       write(*,*)
       write(*,*) "=========================================="
       write(*,*) "Input Crystal Structure"
       write(*,*) "=========================================="
       write(*,*) "Unit cell vectors:"
       do I = 1, 3
          write(*,'(3F12.6)') a(I,:)
       end do

       write(*,*)
       write(*,*) "Reciprocal unit cell vectors:"
    end if

    b(:,:) = a(:,:)
    call sym_matinv(b, 3)
    bi = transpose(a)
    ai = transpose(b)

    if(debug .eq. 1) then
       do I = 1, 3
          write(*,'(3F12.6)') b(I, :)
       end do

       write(*,*) "pgnr, nel", pgnr, nel

       ! section 1.7
       write(*,*) 'The unit cell contains', nel, ' chemical elements\n'

    end if
    tsmall = tol_rotation_match
    ttsmall = tol_projection_work

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

    gel(1:order) = nge2(first:(first+order-1))

    npri(:) = primen(:)

    allocate(inver(maxval(gel(:))))
    inver(:)=0

    ! section 1.14
    if (( pgnr >=16) .and. (pgnr <=31)) then
       allocate(mtab(24, 24))
       allocate(mtab2(24, 24))
       mtab(:,:) = 0
       mtab(:,:) = MD6h(:,:)
       !write(*,*) 'The pointgroup (no. ', pgnr, ') of the crystal is a subgroup of D6h, with element:' 
    else
       allocate(mtab(48, 48))
       allocate(mtab2(48, 48))
       mtab(:,:) = 0
       mtab(:,:) = MOh(:,:)
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
       rgr(1:3, 1:3, I) = rgr3(1:3, 1:3, rotation_table_index(gel(I), pgnr))
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
          sil(1) = cmplx(1.0_dp, 0.0_dp, kind=dp)
          kgel(1) = 1
          kgord= 1
          rk(1:3) = ark(1:3)*2*pi
          srk(1:3)= ark(1:3)

          call sym_groupkp(kg, kgord, k2gord, kgel, kkgel, mtab2, ibz, listp, &
               & nopi, nopi1, nopli, nopli1, sil, til, ksym, rk, ark, a, ai, b,bi, u, order,pgnr, &
               & rgr, mtab, gel, steer, tsmall, factor_group_ok)
          if (.not. factor_group_ok) then
             write(*,*) "Skipping k-point: projective phases do not form a supported finite factor group"
             deallocate(kgel, kkgel, nopli1, sil, nopi, nopli, listp)
             cycle
          end if
       end if


       ! section 6
       ! calculation of the diagonal element of the irreducible representations
       ! section 6.1

       write(*,*)
       write(*,*) "=========================================="
       write(*,*) "Projection Matrices"
       write(*,*) "=========================================="
       write(*,'(A,3F10.6)') " Wave vector: ", srk(1:3)
       write(*,*)
       write(*,'(A,I4,A)') " Point group of wave vector: ", kg, " operators"
       write(*,*) "Operator indices:"
       do I = 1, kg, 12
          write(*,'(12I6)') kkgel(I:min(I+11, kg))
       end do
       write(*,*)

       projective_mode = projective_factor_group_active(steer(20), ksym, ibz)

       !write(*,*) "projective_mode, steer(20), ksym, ibz", projective_mode, steer(20), ksym, ibz

       if (projective_mode) then
          write(*,*) " "
          write(*,*) "The factor group Gk/Tk consists of"
          do I = 1, kgord
             write(*,*) I, "pointgroup operator: ", kkgel(listp(I))
             write(*,*) "nonprimitive translation: (", til(I, 1:3), "), exp = ", sil(I)
          end do
       end if

       ! Now we have determined the point group of the wave vector, or the finite
       ! factor group used to represent its nonsymmorphic projective irreps.

       !Next we determine all (allowable) irreducible representations of this finite group.

       allocate(cind_invp(kgord))
       if ((IV <= 2) .or. .not. projective_mode) then
          allocate(jpdd(kgord, maxdim, kgord))
          allocate(laj(kgord))
          allocate(allow(kgord))
          allow(:) = 0

          jpdd(:,:,:)=0
          do I = 1, kgord
             cind_invp(I) = I
          end do
          ! section 6.2
          call sym_irrep(jpdd, allow, ncl, laj, kgord, mtab2, npri, steer, &
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

          write(*,*) " "
          write(*,*) "There are ", nup , "projection matrices"
          write(*,*) "for the wave vector"
          write(*,*)" with index J for the irreps and index JD for the diagonal elements of the irrep."

          call sym_sumsets(np, nvec, npl, til, kgord, kgel, rgr, listp, &
               a, ai, b, r, u, nel, nat, ksym, ibz, steer)

          call sym_projmat(laj, kgord, allow, jpdd, projmatrix(:,:, ikp), nvec, nat, lmax, np, nel, ncl, npl, &
               & kgel, kkgel, listp, steer, ksym, ibz, pgnr, ldrmm, rk, u, tsmall, ttsmall)

          num_block(ikp)=nup

          if (debug .eq. 1) then
             write(*,*)
             write(*,*) "=========================================="
             write(*,*) "Projection Matrix"
             write(*,*) "=========================================="
             write(*,'(A,I3,A,3F8.4)') " K-point ", ikp, ": ", kpoints(ikp,:)
             write(*,'(A,I5,A,I5)') " Matrix dimension: ", matrixorder, " x ", matrixorder
             write(*,*)

             ! Display column headers (every 6 columns)
             if (matrixorder <= 60) then
                write(*,'(A6)', advance='no') "Row"
                do K1 = 1, min(matrixorder, 6)
                   write(*,'(A20)', advance='no') "Col " // trim(adjustl(char(48+K1)))
                end do
                write(*,*)
                write(*,*) repeat("-", 6 + min(matrixorder, 6) * 20)

                ! Display matrix in blocks of 6 columns
                do K2 = 1, matrixorder, 6
                   if (K2 > 1) then
                      write(*,*)
                      write(*,'(A6)', advance='no') "Row"
                      do K1 = K2, min(K2+5, matrixorder)
                         write(*,'(A19,I1)', advance='no') "Col ", K1
                      end do
                      write(*,*)
                      write(*,*) repeat("-", 6 + min(6, matrixorder-K2+1) * 20)
                   end if

                   do I = 1, matrixorder
                      write(*,'(I6)', advance='no') I
                      do K1 = K2, min(K2+5, matrixorder)
                         write(*,'(A1,F8.4,A1,F8.4,A1)', advance='no') &
                              "(", real(projmatrix(K1, I, ikp)), ",", aimag(projmatrix(K1, I, ikp)), ")"
                      end do
                      write(*,*)
                   end do
                end do
             else
                ! For very large matrices, just show dimensions
                write(*,*) "Matrix too large to display (dimension > 60)"
                write(*,*) "Use output file for full matrix data"
             end if
             write(*,*)
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
