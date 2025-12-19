module repres  
  use accuracy
  use constants
  use permu
  use intsec
  use subsp
  use eigvec 
  use degen
  implicit none
  private
  public :: sym_repres 

contains

  !Function repres calculates the irreducible representations of the group elements in case
  !there is at least one non-degenerate eigenvalue for at least one group element. The
  !calculation starts with determining the loop structure of the group elements using
  !function permu. In case all eigenvalues for all group elements are degenerate, function
  !degen is called.
  
  !An eigenvector to group element IN with the non-degenerate eigenvalue lab is
  !calculated in the regular representation. This eigenvector is projected on the Jth
  !irreducible subspace using the projection operator Sj. The resulting vector is stored in
  !fi(1:G,1). fi(1:G,1) is operated on, using group elements other than powers of IN in order
  !to create an orthonormal set of LJ1 vectors fi(1:G,1:LJ1). The regular representation of
  !each generator of the group is transformed to the irreducible representation using the
  !orthonormal set fi(1:G,1:LJ1). The irreducible representation of each group element is
  !calculated by multiplying the irreducible representations of the appropriate generators.
  !For one-dimensional irreps, the representations are obvious from the corresponding characters.
  

  subroutine sym_repres(jpdd, allow, laj, multab, G, steer, inel, lj, ncl, nfirst, classl, &
       & sirt, onert, cind, ch, nmberg, ngen, map, norder, ibz, ksym, nopi1, nopli1, sil)

    complex(dp), intent(out) :: jpdd(:,:,:)
    integer, intent(out) :: allow(:)
    integer, intent(out) :: laj(:)

    integer, intent(in) :: multab(:,:) 
    integer, intent(in) :: G
    integer, intent(in) :: steer(:)
    integer, intent(in) :: inel(:)
    integer, intent(in) :: lj(:)
    integer, intent(in) :: ncl
    integer, intent(in) :: nfirst(:)
    integer, intent(in) :: classl(:)
    integer, intent(in) :: sirt(:,:)
    integer, intent(in) :: onert(:)
    integer, intent(in) :: cind(:)
    complex(dp), intent(in) :: ch(:,:)
    integer, intent(in) :: nmberg
    integer, intent(in) :: ngen(:)
    integer, intent(in) :: map(:,:)
    integer, intent(in) :: norder(:)
    integer, intent(in) :: ibz
    integer, intent(in) :: ksym
    integer, intent(in) :: nopi1
    integer, intent(in) :: nopli1(:)
    complex(dp), intent(in) :: sil(:) 
    
    real(dp) :: rnorm

    complex(dp), allocatable :: vec(:), vec2(:), fi(:,:)
    integer, allocatable :: ntry2(:)
    complex(dp), allocatable :: dfi(:,:)

    complex(dp), allocatable :: grupel(:,:,:)
    real(dp), allocatable :: genfi(:,:)
    real(dp), allocatable :: grupelord2(:,:), subm(:,:,:)
    real(dp), allocatable :: grupelord4(:,:,:,:)

    integer, allocatable :: loopl(:)
    integer, allocatable :: lpstr(:)
    
    
    integer :: mg
    integer :: LG, LPE, N7, NO1
    integer :: I,II, I1, I2, I3, I4, I5, IV, IN
    integer :: J, J1, J2
    integer :: N, N6, N8
    integer :: K1, K11, K2, K3, K4, K5, K6, K7, KJ
    integer :: IND
    integer :: RIN, RINAB
    integer :: ML1, ML2, LJ1, ncl1, nip, NML, numl, nopil
    
    complex(dp) :: pi2i
    complex(dp) :: lab
    complex(dp) :: P 
    
    integer, allocatable :: n2(:)

    allocate(n2(ncl))

    allocate(loopl(G))
    allocate(lpstr(G))

    allocate(vec(G))
    allocate(vec2(G))

    allocate(dfi(G,G))
    allocate(fi(G, G))
    allocate(grupel(G,G,G))

    allocate(grupelord2(2,G))
    allocate(grupelord4(2,G,G,G))
    allocate(ntry2(G))

    grupel(:,:,:)= 0
    
    ncl1 = ncl - 1
    write(*,*) "lj", lj
    do I = 1, ncl
       laj(I) = lj(I)
       n2(I) = I
    end do

    do I = 1, ncl1
       I1 = I + 1
       do J = I1, ncl
          if(laj(I) .gt. laj(J)) then
             nip = laj(J)
             laj(J) = laj(I)
             laj(I) = nip
             nip = n2(J)
             n2(J) = n2(I)
             n2(I) = nip
          end if
       end do
    end do

    if(steer(7) .ne. 0) then
       write(*,*) "New representation numbering 1:", ncl
       write(*,*) "Olod representation numbering", n2(1:ncl)
    end if

    pi2i = cmplx(0, 2*pi)

    KJ = 0
    do while (KJ < ncl)

       KJ = KJ + 1
       J = n2(KJ)
       LJ1 = laj(KJ)

       if(LJ1 .ne. 1) then
          I = sirt(J, 1)
          IN = nfirst(I)
          IN = classl(IN)
          lab = pi2i*(sirt(J, 3) -1)/sirt(J, 2)
          lab = exp(lab)
          NO1 = norder(I) - 1

          if (onert(J) .eq. 1) then
             call sym_permu(loopl, lpstr, numl, multab, G, inel, IN, steer)
             NML = 0
             IND = 1
             do while( NML < numl)
                NML = NML + 1
                if(NML > 1) then
                   IND = IND + loopl(NML - 1)
                end if
                vec(:) = 0
                K2 = lpstr(IND)
                vec(K2) = 1
                P = 1
                LPE = IND + loopl(NML)
                do N = IND+1, LPE -1
                   K3 = lpstr(N)
                   P = P*lab
                   vec(K3) = P
                end do

                ! Projection SJ*vec
                vec2(:) = 0
                do K4=1, G
                   do K5 = 1, G
                      K6 = inel(K5)
                      k6 = multab(K4, K6)
                      K6 = cind(K6)
                      vec2(K4) = vec2(K4) + conjg(ch(J, K6))*vec(K5)
                   end do
                end do
                I2 = 1
                do while( I2 <= G)
                   if(abs(vec2(I2)) >= 0.001) then
                      exit
                   end if
                   I2 = I2 + 1
                end do
                if(I2 <= G) then
                   rnorm = 0
                   do I2 = 1, G
                      rnorm = rnorm + (abs(vec2(I2)))**2
                   end do
                   rnorm = 1/ sqrt(rnorm)
                   fi(1:G, 1) = rnorm * vec2(1:G)
                   ntry2(:) = 0
                   ntry2(IN) = 1
                   K2 = IN
                   do I2 = 1, NO1
                      K2 = multab(K2, IN)
                      ntry2(K2) = 1
                   end do
                   ! multiply fi with all group elements (except for powers of already tested group elements)
                   ! to form new independent vectors
                   IV = 1
                   do K3 = 1, G
                      if(ntry2(K3) .ne. 1) then
                         ntry2(K3) = 1
                         ! multiply with the regular representation of group element K3
                         I3 = inel(K3)
                         vec(1:G) = fi(multab(I3, 1:G),1)

                         do K11 = 1, IV
                            RIN = 0
                            RIN = dot_product(vec(1:G), conjg(fi(1:G, K11)))

                            !RIN = vec(1:G)*conj(fi(1:G, K11))
                            RINAB = abs(RIN)

                            if (RINAB >= 0.001) then
                               if (abs(RINAB -1) >= 0.001) then
                                  vec(1:G) = vec(1:G) - RIN*fi(1:G, K11)
                                  rnorm = dot_product(vec(1:G), vec(1:G))
                                  !rnorm = sum(abs(vec(1:G))**2)
                                  if (rnorm >=0.001) then
                                     rnorm = 1/sqrt(rnorm)
                                  else
                                     exit
                                  end if
                                  vec(1:G) = vec(1:G)*rnorm
                               else
                                  exit
                               end if
                            end if
                         end do
                         if(abs(RINAB - 1) >= 0.001)  then
                            IV = IV + 1
                            fi(1:G, IV) = vec(1:G)
                         end if
                         if(IV >= LJ1) then
                            exit
                         end if
                      end if
                   end do
                end if
                if( IV >= LJ1) then
                   exit
                end if
             end do
          else
             ! An irreducible representation with no non-degenerate eigenvalue is calculated
             write(*,*) "The ", KJ, "th irreducible representation has no non-degenerate eigenvalue"

             call sym_degen(fi, LJ1, J, KJ, IN, lab, multab, G, ngen, map, nmberg, cind, norder, steer, inel, ch, sirt)
          end if

          ! construction of the irreducible representation

          if (LJ1 .ne. 0) then

             allocate(genfi(G, LJ1))
             genfi(:,:)=0

             dfi(1:LJ1, 1:G) = transpose(conjg(fi(1:G, 1:LJ1)))

             do K4 = 2, nmberg
                grupel(1:LJ1, 1:LJ1, ngen(K4)) = 0
             end do

             grupel(:,:,1) = 0
             do II = 1, LJ1
                grupel(II, II, 1) = 1
             end do
             do II = 1, LJ1
                write(*,*) grupel(II, 1:LJ1, 1)
             end do

             do K1 = 2, nmberg
                K5 = ngen(K1)
                K2 = inel(K5)
                do II = 1, G
                   mg = multab(K2, II)
                   genfi(II, 1:LJ1) =fi(mg, 1:LJ1)
                end do

                grupel(1:LJ1, 1:LJ1, K5) = matmul(dfi(1:LJ1, 1:G), genfi(1:G, 1:LJ1))

                do II = 1, LJ1
                   write(*,*) grupel(II, 1:LJ1, K5)
                end do

             end do

             do LG = 1, G
                ML2 = map(LG, 2)
                if(ML2 .ne. 0) then
                   ML1 = map(LG, 1)

                   grupel(1:LJ1, 1:LJ1, LG) = matmul(grupel(1:LJ1, 1:LJ1, ML1), grupel(1:LJ1, 1:LJ1, ML2))
                   ! construction of the irrep of the other group elements by multiplying
                   ! the representatives of the generators
                end if
             end do
             if (steer(9) .ne. 0) then !steer(9)
                do I3 = 1, G
                   do I4 = 1, LJ1
                      do I5 = 1, LJ1
                         grupelord4(1, I5, I4, I3) = real(grupel(I5, I4, I3))
                         grupelord4(2, I5, I4, I3) = aimag(grupel(I5, I4, I3))
                      end do
                   end do
                end do

                do I3= 1,G
                   write(*,*) "Representation number ", KJ, " element number ", I3
                   do I4 = 1, LJ1
                      write(*,*) "(", grupelord4(1,I4,1:LJ1, I3), " ", grupelord4(2,I4,1:LJ1, I3), ")"
                   end do
                end do
             end if
             deallocate(genfi)  
          end if
       else

          if(steer(8) .ne. 0) then ! steer(8) 
             do II = 1, G
                grupelord2(1, II) = real(ch(J, cind(II)))
                grupelord2(2, II) = aimag(ch(J, cind(II)))
             end do
             do I3 = 1, G
                write(*,*) "Representation number ", KJ, " element number ", I3
                write(*,*) "(", grupelord2(1,I3), " ", grupelord2(2,I3), ")"
             end do

             do II = 1, G
                grupel(1, 1, II) = cmplx(grupelord2(1, II), grupelord2(2, II))
             end do
          end if
       end if

       if( .not. ((ibz .ne. 0) .or. (steer(20) .ne. 0) .or. (ksym .ne. 0) )) then 
          N6 = 1
          do while (N6 <= nopi1)
             N7 = nopli1(N6)
             N8 = 1
             do while (N8 <= LJ1)
                if(abs(sil(N7) - grupel(N8, N8, N7)) > 0.001) THEN
                   exit
                end if
                N8 = N8 + 1
             end do
             if (N8 <= LJ1) then
                exit
             end if
             N6 = N6 + 1
          end do

          if (N6 > nopi1) then
             allow(KJ) = 1
             do J2 = 1, G
                do J1 = 1, LJ1
                   jpdd(KJ, J1, J2) = grupel(J1, J1, J2)
                end do
             end do
          else
             allow(KJ) = 0
          end if
       else
          allow(KJ) = 1
          do J1 = 1, G
             do J2 = 1, LJ1
                jpdd(KJ, J2, J1) = grupel(J2, J2, J1)
             end do
          end do
       end if
       !write(*,*) "allow in repres", allow

    end do

    deallocate(n2)
    deallocate(loopl)
    deallocate(lpstr)
    deallocate(vec)
    deallocate(vec2)
    deallocate(grupelord2)
    deallocate(grupelord4)
    deallocate(fi)
    deallocate(dfi)
    deallocate(ntry2)
    deallocate(grupel)
  end subroutine sym_repres
end module repres
