module charac  
  use accuracy
  use constants
  implicit none
  private
  public :: lcm, modulus, sym_charac

contains
  
  ! least common multiple 
  integer function lcm(p, q)
    implicit none
    integer, intent(in) :: p
    integer, intent(in) :: q

    integer :: x, y, k
    if( p .eq. q) then
       lcm = p
       return
    else if (q .eq. 0) then
       lcm = 0
       return
    end if

    x = abs(p)
    y = abs(q)
111 continue
    if (x .gt. y) then
       x = mod(x, y)
       if (x .ne. 0) go to 111
    else if ( x .lt. y) then
       y = mod(y, x)
       if (y .ne. 0) go to 111
    end if
    
    k = max(x, y)
    
    lcm = (p*q)/k
  end function lcm
  
  ! calculates the interger I modulus P and returns this value in the integer J
  integer function modulus(I, P)
    implicit none
    integer, intent(in) :: I
    integer, intent(in) :: P
    integer :: K
    if(I < 0) then
       K = I
       do while( K < 0)
          K = K + P
       end do
    end if
    if(I>=0) then
       K = I
       do while( K>= P)
          K = K - P
       end do
    end if
    modulus = K
  end function modulus

  ! The irreducible characters are calculated according to Dixon's method

  !The class index of each group element is calculated and stored as cind(N) = I if group element N belongs to class I.
  !The order norder(1:ncl), and the powers npow(I,K), of the elements in each class I are found.
  !That is, the index of the class to which group element QK belongs, if Q belongs to class I.

  !An exponent ex is found as the least common multiple of the orders norder(1:ncl) using the method of Euclides.

  !A conjugate class whose elements have the lowest possible degeneracy is registered for each irreducible representation,
  !in case there are no non-degenerate eigenvalues.
  subroutine sym_charac(ch, cind, onert, sirt, lj, norder, cind_invp, multab, G, ncl, h, nfirst, classl, inel, steer, prime)
    complex(dp), intent(out) :: ch(:,:)
    integer, intent(out) :: cind(:)
    integer, intent(out) :: onert(:)
    integer, intent(out) :: sirt(:,:)
    integer, intent(out) :: lj(:)
    integer, intent(out) :: norder(:)
    integer, intent(out) :: cind_invp(:) ! inverse module P

    integer, intent(in) :: multab(:,:) 
    integer, intent(in) :: G
    integer, intent(in) :: ncl
    integer, intent(in) :: h(:)
    integer, intent(in) :: nfirst(:)
    integer, intent(in) :: classl(:)
    integer, intent(in) :: inel(:)
    integer, intent(in) :: steer(:)
    integer, intent(in) :: prime(:)

    ! section 1
    ! the order, norder(I) and the powers, npow(I,K) of the elements in each class are found
    ! That is, the index of the class to which groupelement Q^K belongs, if Q belongs to class I.
    ! nconj(I) is the index of the calss of inverse elements of class I.
    ! The array nar is used several times for different variables

    real(dp) :: pi2
    integer :: K, L, M, I1, I, J, ex, ijp, J1, J2
    integer :: L1, L2, F, S, MJ, N, ncl2, nfe, nfi, nfk, nflc, nh, NI, nli
    integer :: e, IH, invs, deg2, NP1, P, root, row, sirtk3
    integer :: T, X, Y, Zprim, LC, JJ, K1, nllc, NJ
    integer :: norm2
    real(dp) :: maxh 
    real(dp) :: degen


    integer, allocatable :: nconj(:)
    integer, allocatable :: npow(:,:)

    integer, allocatable :: nup(:), inv(:), ind(:)
    integer, allocatable :: w(:,:)
    integer, allocatable :: nar(:)
    integer, allocatable :: mat(:,:)
    integer, allocatable :: u(:,:), v(:,:)
    integer, allocatable :: hinv(:), nordin(:)
    real(dp), allocatable :: z(:,:) 
    integer, allocatable :: sqr(:)
    integer, allocatable :: minimum(:)

    complex(dp) :: croot


    allocate(u(ncl, ncl))
    allocate(v(ncl, ncl*2))
    allocate(z(ncl, ncl))

    allocate(nconj(ncl))
    allocate(npow(ncl, ncl))

    allocate(hinv(ncl))
    allocate(nordin(ncl))

    allocate(nar(ncl))
    allocate(w(ncl, ncl))
    allocate(mat(ncl, ncl))
    allocate(minimum(ncl))
    minimum(:)=0

    npow(:,:)= 0
    nconj(:)=0

    pi2 = pi*2

    nconj(1) = 1

    if(G > 1) then

       ! cind(N) = I if group element N belongs to class I

       do I = 1, ncl
          K = nfirst(I)
          L = h(I)
          M = K + L - 1
          do I1 = K, M
             N = classl(I1)
             cind(N) = I
          end do
       end do
       write(*,*) "The class index for each group element is M, cind", M, cind
       do I = 1, ncl
          onert(I) = 0
          K = nfirst(I)
          K = classl(K)
          J = K
          norder(I) = 1
          npow(I,1) = I
          N = 1
          if (J .ne. 1) then
             do while( J .ne. 1)
                N = N + 1
                J = multab(J, K)
                if(J .ne. 1) then
                   npow(I, N) = cind(J)
                end if
             end do
             npow(I, N) = 1
             norder(I) = N
             M = inel(K)
             nconj(I) = cind(M)
          end if
       end do

       write(*,*) "The order of each class is "
       write(*,*) norder
       write(*,*) "The power of each class is "
       write(*,*) npow
       write(*,*) "The conjugated class of each class is "
       write(*,*) nconj

       ! Find the exponent ex, the least common multiple of norder(I) , I = 1:ncl

       ex = 1
       do K=2, ncl
          N = norder(K)
          M = ex
          ! determine the greatest common divisor gcd of N and M. Then the least common multiple is M*N/gcd
          ex=lcm(N, M)
       end do

       write(*,*) "The exponent of the group is ", ex

       ! section 3
       ! Determine prime P, such that ex divides P - 1

       maxh=floor(2*sqrt(real(G))) + 1

       do I = 1, 100
          NP1 = prime(I) - 1
          if((prime(I) >= maxh) .and. ((int(NP1/ex)*ex - NP1) .eq. 0)) then
             P = prime(I)
             exit
          end if
       end do

       write(*,*) "The prime P, such that ex divides P-1 is ", P
       ! section 4
       ! Determine a primitive root Z, such that Z^ex == 1 (modules P) but
       ! Z^1 ~= 1 (mod P) for I < ex
       ! Determine indices ind(I), such that ind(I) == K if Z^K == 1(mod P)
       ! and powers nup(K), such that nup(K) == I if Z^K == I (mod P)
       ! inverses inv(I), such that inv(I)*I == 1( mod P)

       M = P - 1

       do root = 2, M
          I = 1
          J = root
          do while( J .ne. 1)
             I = I + 1
             J = modulus(J*root, P)
          end do
          if ( I .eq. ex) then
             exit
          end if
       end do
       if ( root > M) then
          write(*,*) "No primitive root"
          stop
       end if
       Zprim = root
       J =Zprim
       I = 1

       allocate(ind(M))
       allocate(nup(ex))
       allocate(inv(M))
       allocate(sqr(520))
       sqr(:) = 0
       ind(:)=0
       nup(:)= 0

       do while( I <= ex)
          ind(J) = I
          nup(I) = J
          K= modulus(I*I, P)
          if(sqr(K) .eq. 0) then
             sqr(K) = I
          end if
          I = I + 1
          J=modulus(J*Zprim, P)
       end do

       inv(1) = 1
       do I = 2, M
          do L = 2, M
             ijp=modulus(I*L, P)
             if( ijp .eq. 1) then
                inv(I) = L
                exit
             end if
          end do
       end do

       do I = 1, M
        cind_invp(I) = inv(I)
       end do

       if(steer(6) > 0) then
          write(*,*) "The primitive root of the group is", Zprim
          write(*,*) "The indices of the group are"
          write(*,*) ind
          write(*,*) "The powers of the group are"
          write(*,*) nup
          write(*,*) "The inverse modulus P are"
          write(*,*) inv
          !write(*,*) "The square roots modulus P are"
          !write(*,*) sqr
       end if

       ! section 5
       ! The eigenspaces of the first classconstant matrix are formed and the
       ! eigenspaces of dimension higher than 1 are reduced by operation with
       ! other classconstant matrices.
       ! The eigenvectors are stored as rows, one should multiply with the
       ! transposed classconstant matrices.
       ! The eigenvectors lie in w(I,J).
       ! If the first row of an eigenspace is row S, then the last row of this
       ! eigenspace is row nar(S).
       ! We start with the space of the unit class; all operations modulus P.

       w(:,:) = 0
       do I = 1, ncl
          w(I,I) = 1
       end do
       nar(:) = 0
       nar(1) = ncl
       !
       ! form class constant matrix c(LC, I, L) and store it in mat(I,L)
       !  C_i C_j = \sum c_{ijk} C_k
       !
       LC = 2
       do while( LC <= ncl)
          !! class constant matrix
          !!  LC  is index 
          mat(:,:) = 0
          nflc = nfirst(LC)        !  first index of class(LC)
          nllc = nflc + h(LC) - 1  !  last index of class(LC)
          do I = 1, ncl
             nfi = nfirst(I)       ! first index of class(I)
             nli = nfi + h(I) - 1  ! last index of class(I)
             do MJ = nflc, nllc
                NJ = classl(MJ)    !  get LC's index
                do IH = nfi, nli
                   NI = classl(IH) ! get I's index 
                   L = multab(NJ, NI) ! get group element (LC, I)
                   do K = 1, ncl
                      nfk = nfirst(K)    ! first index of class(K)
                      nfe = classl(nfk)  ! last index of class(K) 
                      if( L .eq. nfe) then  ! when L ==  K‘s 
                         mat(I, K) = mat(I, K) + 1
                         exit
                      end if
                   end do
                end do
             end do
          end do
     
          S = 1
          ncl2 = 2 * ncl
          do while ( nar(S) .ne. 0)
             do while( nar(S) .ne. S) !!  number class  != 1
                F = S             ! F = S = 1 T = nar(S) = 
                T = nar(S)
                do I = S, T    ! 0, ncl 
                   do J = 1, ncl
                      ! the result of the operation of the class constant matrix on the eigenspace is stored in u(I,J)
                      u(I, J) = 0
                      do K = 1, ncl
                         ! note transpose of mat
                         u(I, J) = u(I, J) + w(I, K)*mat(J, K)
                      end do
                      u(I, J) = modulus(u(I, J), P)
                   end do
                end do
                nh = 1
                do while ( nh <= P)
                   e = nh - 1
                   do I = S, T
                      do J = 1, ncl
                         K = J + ncl
                         v(I, J) = w(I, J)
                         v(I, K) = modulus( (u(I, J) + e*w(I, J)), P)
                      end do
                   end do
! If -e is an eigenvalue of mat(I,J), then the right part of v(I,K) now
! contains the sum of eigenspaces minus the vectors belonging to this
! eigenvalue. Then there are linear combinations of rows, which vanish.
! The corresponding linear combinations of w(I,J) (stored in the left
! part of v(I,K)), will be common eigenvectors of the present
! classconstant matrix and all preceding ones.
!
                   J = F
                   do I = S, T
                      K = ncl + 1
                      do while (K <= ncl2)

                         if(v(I, K) .ne. 0) then
                            Y = v(I, K)
                            invs = inv(Y)
                            do L = 1, ncl2
                               v(I, L) = modulus(v(I, L)* invs, P)
                            end do
                            !write(*,*) v(I,1:ncl2)

                            ! this makes v(I,K) == 1
                            do row = (I+1), T
                               Y = modulus(P - v(row, K), P)
                               do L = 1, ncl2
                                  v(row, L) = modulus(v(row, L) + v(I, L)*Y, P)
                               end do
                            end do
                            exit
                         end if
                         ! this makes v(row, K) == 0
                         K = K + 1
                      end do
                      if( K .gt. ncl2) then
                         z(F, 1:ncl) = v(I, 1:ncl)
                         ! z(i,k) is a temporary storage of produced common eigenvators
                         F = F + 1
                      end if
                   end do
                   nar(J) = F - 1
                   !write(*,*)"F T", F, T
                   if (F .gt. T) then
                      exit
                   end if
                   nh = nh + 1
                end do
                if ( F .gt. T) then
                   exit
                end if
                if (nh .gt. P) then
                   write(*,*) "Not enough eigencolumns"
                    write(*,*) "nh P", nh, P
                   stop
                end if
             end do
             w(S:T, 1:ncl) = z(S:T, 1:ncl)
             if(nar(S) .ne. ncl) then
                S = nar(S) + 1
             else
                exit
             end if
          end do
          if (nar(S) .eq. 0) then
             write(*,*) "Error, nar(s) == 0"
             stop
          end if
          Y = LC
          do I = 1, ncl
             if(nar(I) .ne. I) then
                LC = LC + 1
                exit
             end if
          end do
          if (Y .eq. LC) then
             exit
          end if
       end do
       ! Section 6
       ! 
       ! Calculation of the characters. The dimensions are stored in lj(K).
       ! The coefficients of the powers of the primitive roots are stored
       ! in v(I,J).
       !
       do I = 1, ncl
          if(w(I, 1) .ne. 0) then
             L = inv(w(I, 1))
             do K = 1, ncl
                w(I, K) = modulus(w(I, K)*L, P)
             end do
          else
             write(*,*) "Error, w(I,1) == 0"
             stop
          end if
       end do
       do I = 1, ncl
          J = h(I)
          hinv(I) = inv(J)
          K = norder(I)
          nar(I) = ex/K
          nordin(I) = inv(K)
       end do
       do K = 1, ncl
          norm2 = 0.0
          do I = 1, ncl
             J = nconj(I)
             X = modulus(w(K,I)*w(K,J), P)
             X = modulus(X*hinv(I), P)
             norm2 = modulus(norm2 + X, P)
          end do

          deg2 = modulus(G*inv(norm2), P)
          ! deg2 is the square of the dimenstion of the irreducible representation
          lj(K) = sqr(deg2)
          do N = 1, ncl
             u(K, N) = modulus(lj(K)*w(K, N)*hinv(N), P)
          end do
          ! u(K, I) contains the irreducible character modulus P
          do I1 = 1, ncl
             M = norder(I1)
             do J = 1, M
                J1 = J - 1
                X = J1*nar(I1)
                Y = 0
                do L2 = 1, M
                   L = L2 - 1
                   S = modulus(-L*X, ex)
                   S = modulus(S + ex, ex)
                   if (L .eq. 0) then
                      L = M
                   end if
                   T = npow(I1, L)
                   if( S .eq. 0) then
                      Y = modulus(Y + u(K, T), P)
                   else
                      Y = modulus(Y + u(K, T)*nup(S), P)
                   end if
                end do
                v(I1, J) = modulus(nordin(I1)*Y, P)
             end do
             if(steer(2) > 0) then
                ! a nondegenerate eigenvalue of an eigenvaule with the lowest possible degeneracy , is registered
                if (onert(K) .ne. 1) then
                   sirtk3 = 1
                   if(v(I1, 1) .ne. 1) then
                      degen = v(I1, 1)
                      if (v(I1, 1) .eq. 0) then
                         degen = 10  !! not understand 
                      end if
                      J2 = 2
                      do while( J2 <= M)
                         if(v(I1, J2) < degen) then
                            if(v(I1, J2) .ne. 0) then
                               degen = v(I1, J2)
                               sirtk3 = J2
                               if(v(I1, J2) .eq. 1) then
                                  exit
                               end if
                            end if
                         end if
                         J2 = J2 + 1
                      end do
                   end if
                   if ((v(I1, 1) .eq. 1) .or. (J2 <= M)) then
                      onert(K) = 1
                      sirt(K, 1) = I1
                      sirt(K, 2) = M
                      sirt(K, 3) = sirtk3
                   end if
                   if((v(I1, 1) .ne. 1) .and. (J2 > M)) then
                      if ((I1 .eq. 1) .or. (degen < minimum(K))) then
                         sirt(K, 1) = I1
                         sirt(K, 2) = M
                         sirt(K, 3) = sirtk3
                         minimum(K) = degen    ! need to check
                      end if
                   end if
                end if
             end if
             if(steer(6) .ne. 0) then
                write(*,*) "Character ( ", K, I1, ") in ", M," th roots of unity is", v(I1, 1:M)
             end if
             if(I1 .ne. 1) then
                ch(K, I1) = v(I1, 1)
                ! The zeroth power of Z is 1, so the first term in the addition is 1*v(I1,1)
                do L1 = 2, M
                   if( v(I1, L1) .ne. 0) then
                      croot =cmplx(0, pi2*(L1-1)/M)
                      croot = exp(croot)
                      ch(K, I1) = ch(K, I1) + croot*v(I1, L1)
                   end if
                end do
             else
                ch(K, 1) = lj(K)
             end if
             if (abs(aimag(ch(K, I1))) < 0.0001) then
                ch(K, I1) = real(ch(K, I1))
             end if
             if (abs(ch(K, I1)) < 0.0001) then
                ch(K, I1) = 0
             end if
          end do
       end do

       ! section 7
       ! check the results
       L = sum(lj(1:ncl)**2)

       if(L .ne. G) then
          write(*,*) "Error L != G"
          write(*,*) "L = ", L, " G= ", G, "lj = ", lj
          stop
       end if
       if(steer(7)>0) then
           write(*,*) "lj = ", lj
          do J = 1, ncl
             write(*,*) "ch( ", J, ")"
             write(*,*) ch(J,:)
          end do
       end if
       deallocate(ind)
       deallocate(nup)
    deallocate(inv)
    deallocate(sqr)
    else
       lj(1) = 1
       ch(1,1) = 1
       cind = 1
       onert = 1
       sirt = 1
       norder = 1
    end if

    deallocate(w)
    deallocate(mat)
    deallocate(hinv)
    deallocate(nconj)
    deallocate(npow)
    deallocate(u)
    deallocate(v)
    deallocate(z)
    deallocate(nar)
    deallocate(minimum)
  end subroutine sym_charac

end module charac


