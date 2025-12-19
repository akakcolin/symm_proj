program main
  use accuracy
  use constants 
  use groupkp
  use irrep
  use sumsets
  use projmat
  use genera
  implicit none

  integer :: I, J, K, K1, K2, IV
  integer :: K3, K4, K5, LD1, M2, mu1, mu2, KI, kg
  integer :: II, J1, JD, JJ, R5
  integer :: index1, ikp, nfacto
  integer :: ilmax, indk1, isign, itotal
  integer :: nb, nblock, NC, ndi, nip, NN1, NN2
  integer :: NT, ntr, nup 
  real(dp) :: rh, rntr
  complex(dp) :: sres
  real(dp), allocatable :: inverk(:)
  integer, allocatable :: listp(:)
  integer, allocatable :: nr(:)
  real(dp), dimension(3,3) :: a, b, bi, ai

  real(dp) :: T
  
  integer :: order, first, uco
  integer, dimension(100) :: npri
  integer, allocatable :: gel(:)
  real(dp), allocatable :: u(:,:)

  real(dp), allocatable :: rgr(:,:,:)
  
  integer :: K48
  integer :: number_of_wave_vectors
  integer :: last 
  integer :: wvco
  real(dp), dimension(3) :: rk, ark, srk, brk, rosk, robrk 
  integer :: kgord, k2gord 
  integer, allocatable :: kgel(:) 
  real(dp) :: TT, TTT
  integer, allocatable :: kkgel(:)
   
  complex(dp), allocatable :: sil(:)

  integer, allocatable :: map(:,:)
  integer :: nmberg
  integer, allocatable :: mtab(:,:)
  integer, allocatable :: mtab2(:,:)
  integer, allocatable :: mtab3(:,:)
  integer, allocatable :: ngen(:)
  integer :: G
  integer :: index
  integer :: L, L1, L2, N, N2

  real(dp), dimension(4, 48) :: Oh
  real(dp), dimension(4, 24) :: D6h
  integer, dimension(48, 48) :: MOh
  integer, dimension(24, 24) :: MD6h
  integer, dimension(2, 36) :: npgo
  complex(dp), allocatable:: D(:,:)
  complex(dp), dimension(3, 3, 72) :: rcgr3
  real(dp), dimension(3,3, 72) :: rgr3
  complex(dp), dimension(3,3) :: res
  real(dp) :: fi, theta, psi, irt2
  
  complex(dp), dimension(3,3) :: Q1, Q

  !! symprj
  character(len=12) :: datafile
  integer, parameter :: fh = 15
  
  integer ::   ksym, ntz, ibz
  integer, dimension(20) :: steer 
  !real(dp), dimension(2,72,84) :: ldrmmorg
  
  integer, allocatable :: lmax(:)
  integer, allocatable :: nat(:)
  integer :: nel, pgnr
  real(dp) :: tsmall, ttsmall
  real(dp), dimension(3) :: tsk
  real(dp), allocatable :: r(:,:,:)
  real(dp), allocatable :: factor(:)
  
  integer :: atco ! atco =1 means cartesian coordinate, =0 lattice coordinate

  real(dp), allocatable :: til(:,:)
  real(dp), dimension(3) :: rprod
  integer :: nal, N3, N31
  integer, allocatable :: nalr(:)
  real(dp), dimension(3) :: difi, dif
  complex(dp) :: R4
  complex(dp), dimension(72,84) :: ldrmm
  real(dp) :: R1
  real(dp) :: ptrace, sumtot

  integer, allocatable :: nopli1(:)
  integer, allocatable :: nopli(:,:)
  integer, allocatable :: allow(:)
  real(dp), allocatable :: jdpk(:,:)
  real(dp), allocatable :: tmatri(:,:)
  real(dp) :: ar
  real(dp) :: ep 
  integer :: I1, I10, I11, I12, I3, I4, I5, I6, I7, I8, I9, ichem, ito, ifd
  real(dp) :: lsqsum
  integer :: M1, N1, ncoset
  real(dp), dimension(3) :: trac
  real(dp), allocatable :: nvec(:,:,:,:,:)
  integer, allocatable :: inver(:)
  integer :: nopi1 
  integer :: ncl
  integer, allocatable :: nopi(:)
  logical :: is_ski
  integer, allocatable :: laj(:)
  integer, allocatable :: np(:,:,:)
  complex(dp), allocatable :: jpdd(:,:,:) 
  real(dp), allocatable :: jdprod(:,:)
  real(dp), allocatable :: all_kpoints(:,:)
  integer, allocatable :: npl(:,:,:,:)
  integer :: tmp_dim ! temp value for maxvalue of np
  integer :: debug, J2, LJ1, KJ
  complex(dp), allocatable :: projmatrix(:,:,:)
  integer :: matrixorder

  character(len=32) :: infile, arg
  integer :: stat

   integer, allocatable  :: cind_invp(:)
 
  call get_command_argument(number=1, value=infile, status=stat)
  
  open(fh, file=infile, status='OLD', action='read')
  
  debug=0
  nopi1 = 1;
  ksym = 1;
  ntz = 0;

  do I = 1, 24
     index= (I-1)*3
     Oh(4, I) = 0
     Oh(1,I) = Ohdat(index + 1)*pi
     Oh(2,I) = Ohdat(index + 2)*pi
     Oh(3,I) = Ohdat(index + 3)*pi
     Oh(1,I+24) = Oh(1,I)
     Oh(2,I+24) = Oh(2,I)
     Oh(3,I+24) = Oh(3,I)
     Oh(4,I+24) = 1
  end do


  do I = 1, 12
     index= (I-1)*3
     D6h(4, I) = 0
     D6h(1,I) = D6hdat(index + 1)*pi/3
     D6h(2,I) = D6hdat(index + 2)*pi/3
     D6h(3,I) = D6hdat(index + 3)*pi/3
     D6h(1,I+12) = D6h(1,I)
     D6h(2,I+12) = D6h(2,I)
     D6h(3,I+12) = D6h(3,I)
     D6h(4,I+12) = 1
  end do


  if(debug .eq. 1) then
     write(*,*) "Rotation angles for group Oh"

     do I = 1, 48
        write(*,*) I, Oh(1:3, I)
     end do

     write(*,*) "Rotation angles for group D6h"

     do I = 1, 24
        write(*,*) I, D6h(1:3, I)
     end do
  end if


  do I = 1,24
     index = (I-1)*24
     do J = 1, 24
        MOh(I,J) = MOhdat(index + J)
        MOh(I, J+24) = MOh(I,J) + 24
        MOh(I+24, J) = MOh(I, J+24)
        MOh(I+24, J+24) = MOh(I, J)
     end do
  end do
  do I = 1,12
     index = (I-1)*12
     do J = 1, 12
        MD6h(I,J) = MD6hdat(index + J)
        MD6h(I, J+12) = MD6h(I,J) + 12
        MD6h(I+12, J) = MD6h(I, J+12)
        MD6h(I+12, J+12) = MD6h(I, J)
     end do

  end do

  if (debug .eq. 1) then
     write(*,*) "The group Oh multiplication table:"

     do I = 1, 24
        write(*,*) MOh(:, I)
     end do

     write(*,*) "The group D6h multiplication table:"

     do I = 1, 12
        write(*,*) MD6h(:, I)
     end do
  end if

  do I=1, 36
     npgo(1,I) = npgodat(I)
     npgo(2,I) = npgodat(I+36)
  end do

  if (debug .eq. 1) then
     write(*,*) "npgo"
     write(*,*) npgo(1,:)
     write(*,*) npgo(2,:)   

     write(*,*) "The group elements of the 36 crystallographic point groups:"
     do I = 1, 36
        K = npgo(1,I)
        L = npgo(2,I)
        L2 = L + K -1
        write(*,*)  nge(L:L2)
     end do
  end if

  do I = 1, 24
     fi = Oh(1, I)
     theta= Oh(2, I)
     psi = Oh(3, I)
     !write(*,*) "ANGLES", I, fi, theta, psi
     do K = 1, 4
        L = K - 1
        N = 2*L + 1
        K1 = 1
        allocate(D(N, N))        
        call dmatr(D, L, fi, theta, psi)
        N2 = (-1)**L
        if (L .ne. 1) then
           if (L .eq. 2) then
              K1 = 11
           end if
           if ( L .eq. 3) then
              K1 = 36
           end if
        else
           rcgr3(1:3, 1:3, I) = D(1:3, 1:3)
           rcgr3(1:3, 1:3, I+24) = -D(1:3, 1:3)
           K1 = 2
        end if

        do L1 = 1, N
           do L2 = 1, N
              K2 = K1 + (L1 - 1)* N + L2 - 1
              ldrmm(I, K2) = D(L1, L2)
              ldrmm(I+24, K2) = D(L1, L2)*N2
           end do
        end do
        deallocate(D)
     end do
  end do

  do I = 1, 12
     fi = D6h(1, I)
     theta = D6h(2, I)
     psi = D6h(3, I)
     !write(*,*) "ANGLES", I, fi, theta, psi
     do K = 1, 4
        L = K - 1
        N = 2*L + 1
        K1= 1
        N2= (-1)**L
        allocate(D(N, N))
        call dmatr(D, L, fi, theta, psi)
        if (L .ne. 1) then
           if (L .eq. 2) then
              K1 = 11
           end if
           if ( L .eq. 3) then
              K1 = 36
           end if
        else
           rcgr3(1:3, 1:3, I+48) = D(1:3, 1:3)
           rcgr3(1:3, 1:3, I+60) = -D(1:3, 1:3)
           K1 = 2
        end if
        do L1 = 1, N
           do L2 = 1, N
              K2 = K1 + (L1 - 1)* N + L2 - 1
              ldrmm(I+48, K2) = D(L1, L2)
              ldrmm(I+60, K2) = D(L1, L2)*N2
           end do
        end do
        deallocate(D)
     end do
  end do

  irt2 = 1/sqrt(2.0)

  Q1(1,1) = irt2
  Q1(1,2) = 0
  Q1(1,3) = -1*irt2
  Q1(2,1) = cmplx(0, -irt2)
  Q1(2,2) = 0
  Q1(2,3) = cmplx(0,-irt2)
  Q1(3,1) = 0
  Q1(3,2) = 1
  Q1(3,3) = 0

  Q = transpose(conjg(Q1))

  do I = 1, 72
     res(1:3, 1:3) = matmul(Q1(1:3, 1:3), rcgr3(1:3, 1:3, I))
     rgr3(1:3, 1:3, I) = matmul(res(1:3, 1:3), Q(1:3,1:3))
  end do
  if (debug .eq. 1) then
     write(*,*) "Rotation/inverseion matrices"

     do I = 1, 72
        write(*,*) I
        do K1=1,3
           write(*,*) rgr3(K1,:,I)
        end do
     end do

     write(*,*)"One hundred prime numbers are calculated:"
     write(*,*) primen(:)
  end if

!!! 
!!!  need to read struct data like vasp format 

 

  read(fh, *) steer(:)
  !write(*,*) steer

  do I = 1, 3
     read(fh, *) a(I,1), a(I,2), a(I,3)
  end do

  write(*,*) "Input, crystal with unit cell vectors"
  do I = 1, 3
     write(*,*) a(I,:)
  end do

  write(*,*) "Reciprocal unit cell vectors are"

  T = 2*pi

  b(:,:) = a(:,:)
  call sym_matinv(b, 3)
  bi = transpose(a)
  ai = transpose(b)

  do I = 1, 3
     write(*,*) b(I, :)
  end do

  ! section 1.5
  read(fh, *) pgnr
  read(fh, *) nel
  allocate(lmax(nel))
  allocate(nat(nel))

  !lmax is the maximum value of orbital quantum number
  ! different lmax for different atoms.

  do I = 1, nel
     read(fh, *) lmax(I)
  end do

  ! section 1.7
  write(*,*) 'The unit cell contains', nel, ' chemical elements\n'
  !pgnr is the index number of the point group, 1=C1, 2=S2, 3 = C2,
  !4 =Cs, 5=C2h, 6=D2, 7=C2v, 8=D2h, 9=C4, 10=S4, 11=C4h, 12=D4, 13=C4v,
  ! 14=D2d, 15=D4h, 16=C3, 17=S6, 18=D3 (for space groups 149, 151, 153),
  ! 19=D3 (for space groups 150, 152, 154, 155), 20 = C3v (for space
  ! groups 156, 158, 160, 161), 21 = C3v (for space groups 157, 159),
  ! 22 = D3d (for space groups 164, 165, 166, 167), 23 = D3d
  ! (for space groups 162, 163) 24 = C6, 25 = C3h, 26= C6h,
  ! 27 = D6, 28 = C6v, 29 = D3h (for space groups 187, 188), 30 = D3h
  ! (for space groups 189, 190), 31 = D6h, 32 = T, 33 = Th, 34 = O,
  ! 35 = Td, 36 = Oh

  tsmall = 0.00001
  ttsmall = 0.000001
  !section 1.8
  do I = 1, nel
     read(fh,*) nat(I)
  end do
  !write(*,*)"nat", nat(:)

  allocate(r(3,nel,maxval(nat)))
  ! nat(I) is the number of atoms of chemical elements I, per nuit cell
  ! section 1.9
  do I = 1, nel
     K = nat(I)
     do J = 1, K
        read(fh, *) atco
        read(fh, *) r(:,I, J)
        write(*,*) r(:,I,J)
        ! atco = 1 means cartesian coordinates, atco = 0 means lattice coordinates
        if (atco .ne. 1) then
           tsk(1:3) = r(:, I, J)
           r(:,I, J) = matmul(a(:,:), tsk)
        end if
        !write(*,*) "r(:, I, J), tsk", r(:,I, J), tsk(1:3)
     end do
  end do

  allocate(np(nel, maxval(nat), maxval(nat)))
  allocate(nvec(nel, maxval(nat(:)), maxval(nat(:)), 100, 3))
  allocate(npl(nel, maxval(nat(:)), maxval(nat(:)), 100))
  allocate(til(72,3))

  np(:,:,:)=0
  nvec(:,:,:,:,:)=0
  npl(:,:,:,:) = 0
  ! section 1.10

  ! section 1.11
  order = npgo(1, pgnr)
  first = npgo(2, pgnr)

  allocate(gel(order))
  
  gel(1:order) = nge(first:(first+order-1))
  !write(*,*)"gel", gel(1:order)
  npri(:) = primen(:)

  allocate(listp(order))

  allocate(u(order, 3))
  allocate(inver(maxval(gel(:))))
  inver(:)=0
  
  u(:,:) = 0

  if (steer(20) .eq. 0) then
     ! section 1.13
     ! the nonprimitive translations
     
     do I = 1, order
        read(fh, *) uco
        read(fh, *) u(I, :)

        ! uco = 1 means cartesian coordinates, uco = 0 means lattice coordinates
        ! nonprimitive translation are calculated in lattice coordinates
        
        if (uco .ne. 1) then
           tsk(1:3) = u(I, 1:3)
           u(I, 1:3) = matmul(ai, tsk)
        end if
     end do
     !u = transpose(u)
  end if

  
  
  ! section 1.14
  if (( pgnr >=16) .and. (pgnr <=31)) then
     allocate(mtab(24, 24))
     mtab(:,:) = 0
     mtab(:,:) = MD6h(:,:)
     K48 = 48
     write(*,*) 'The pointgroup (no. ', pgnr, ') of the crystal is a subgroup of D6h, with element:' 
  else
     allocate(mtab(48, 48))    
     mtab(:,:) = 0
     mtab(:,:) = MOh(:,:)
     K48= 0
     write(*,*) 'The pointgroup (no. ', pgnr, ') of the crystal is a subgroup of Oh, with element numbers:'     
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
  allocate(mtab2(order, order))
  mtab2(:,:)=0

  do I = 1, order
     rgr(1:3, 1:3, I) = rgr3(1:3, 1:3, gel(I) + K48)
  end do
  ! section 1.20

  ! kpoints 
  last = 0
  read(fh, *) number_of_wave_vectors 

  ! +1 just to temp mathod , to save 0,0,0
  allocate(all_kpoints(number_of_wave_vectors+1,3)) ! all save to reciprocal lattice coordinate  

  all_kpoints(:,:) = 0
  write(*,*) "number_of_wave_vectors", number_of_wave_vectors

  I = 1;
  do while ( last .ne. 1)
     ! last=1 for the last k-vector in the input, otherwise last=0.
     ! wvco=1 means cartesian coordinates; wvco=0 means reciprocal
     ! lattice coordinates. rk(1:3) is a k-vector, expressed in the
     ! reciprocal unit vectors. nfacto is not zero, if want
     ! to make calculations for other k-vectors with the same
     ! direction, but different length. Then nfacto is the number
     ! of such vectors to be treated after the first k-vector in that
     ! direction.
     !factor(:) = 0
     read(fh,*) last
     read(fh,*) wvco
     read(fh,*) rk(:)
     if (wvco .eq. 1) then
        tsk(:) = rk(:)
        rk(1:3) = matmul(bi(:,:), tsk(:))
        ! wave vector in reciprocal lattice coordinates
     end if
     all_kpoints(I,1:3) = rk(1:3)
     read(fh, *) nfacto
     I = I + 1
  end do
  
  !allocate(factor(nfacto))
  do I = 1, number_of_wave_vectors
     write(*,*) "all-points",all_kpoints(I,:)
  end do

  matrixorder =0
  do ichem = 1, nel
     N = 0
     do L = 0, lmax(ichem)
        N = N + 2*L + 1
     end do
     matrixorder = matrixorder + nat(ichem)*N
  end do
  write(*,*) "matrixorder =", matrixorder
  allocate(projmatrix(number_of_wave_vectors, matrixorder, matrixorder))

  do I = 1, number_of_wave_vectors
     projmatrix(I, :,:) =0
     !do II = 1, matrixorder
     !   projmatrix(I, II, II) = 1
     !end do     
  end do
  
  
  do ikp = 1, number_of_wave_vectors
     rk(1:3) = all_kpoints(ikp,:)
     ark(1:3) = rk(1:3)
     srk(1:3) = rk(1:3)
     IV = 1
     ibz = 1   ! test

     allocate(nopli1(100))
     nopli1(:) = 1;

     allocate(kgel(order))
     allocate(kkgel(order))
     allocate(sil(order))
     if( (rk(1) < tsmall) .and. (rk(2) < tsmall) .and. (rk(3) < tsmall)) then
        ! rk(1:3) is gamma point
        ! section for the case of zero wave vector. then the point group of thw wave vector
        ! is equal to the point group of the space group
        kgord = order
        mtab2(1:kgord, 1:kgord) = mtab(1:kgord, 1:kgord)
        do I = 1, kgord
           write(*,*)mtab(i,:)
        end do

        do I = 1, kgord
           kgel(I) = I
           kkgel(I) = gel(I)
        end do
        ibz = 1
        kg = kgord
     else
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
       allocate(jpdd(kgord,kgord,kgord))
       allocate(laj(kgord))
       allocate(allow(kgord))

       jpdd(:,:,:)=0
       ! section 6.2
       call sym_irrep(jpdd, allow, ncl, laj, cind_invp, kgord, mtab2, npri, steer, ibz, ksym, nopi1, nopli1, sil)

       !allocate(laj(ncl))
       !allocate(allow(ncl))
       !do I=1, ncl
       !         laj(I) = tmp_laj(I)
       !   allow(I) = tmp_allow(I)
       !   write(*,*) "tmp_laj, tmp_allow", tmp_laj(I), tmp_allow(I)
       !end do
       ! calculationdeallocate(tmp_laj)
       !deallocate(tmp_allow)
      
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
       write(*,*) "There are ", nup
       write(*,*) "projection matrices for the wave vector"
       write(*,*)" with index J for the irreps and index JD for the diagonal elements of the irrep."

       write(*,*) "Each projection matrix is blockdiagonalized with respect to the indices ichem = 1, ", nel
       write(*,*) "and L = ", lmax(1:nel), ", so each projection matrix consists of", nblock
       write(*,*) "subblocks of sub-projection matrices along the main diagonal. "
       write(*,*) "These are independently orthonormalised into suh-T-matrices, from which the "
       write(*,*) "complete T-matrix can be constructed"


       call sym_sumsets( np, nvec, npl, til, kgord, kgel, rgr, listp, a, ai, b, r, u, nel, nat, ksym, ibz, steer)

       call sym_projmat(laj, kgord, allow, jpdd, projmatrix(ikp,:,:), nvec, nat, lmax, np, nel, ncl, npl, &
            & kgel, kkgel, listp, steer, ksym, ibz, K48, ldrmm, rk, u,tsmall, ttsmall)

        100 FORMAT(12(F7.3, F7.3))
       do I = 1, matrixorder
          write(*,100) projmatrix(ikp, I, :)
       end do


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
 end do
  deallocate(r)
  deallocate(lmax)
  deallocate(nat)
  deallocate(np)
  deallocate(npl)
  deallocate(inver)
  deallocate(nvec)
  
  deallocate(gel)
  deallocate(listp)
  deallocate(mtab2)
  deallocate(mtab)
  deallocate(all_kpoints)
  deallocate(rgr)
  deallocate(til)
  deallocate(projmatrix)
  
        ! section 7
        
end program main
