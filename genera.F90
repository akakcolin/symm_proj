module genera 
  use accuracy
  use constants
  implicit none
  private
  public :: sym_genera, sym_matinv, dmatr, fac, sym_inverse

contains

    ! pgnr is point group index 
    ! sg_num is the space group number
  subroutine getpgnr(pgnr, sg_num)
      integer, intent(out) :: pgnr
      integer, intent(in) :: sg_num
      if (sg_num .eq. 1) then
          pgnr = 1
      else if (sg_num .eq. 2) then 
          pgnr = 2
      else if (sg_num >2 .and. sg_num <6) then
          pgnr = 3
      else if (sg_num >=6 .and. sg_num <10) then
          pgnr = 4
      else if (sg_num >=10 .and. sg_num <16) then
          pgnr = 5
      else if (sg_num >=16 .and. sg_num <25) then
          pgnr = 6
      else if (sg_num >=25 .and. sg_num < 47) then
          pgnr = 7
      else if (sg_num >=47 .and. sg_num <75) then
          pgnr = 8
      else if (sg_num >=75 .and. sg_num <81) then
          pgnr = 9
      else if (sg_num .eq. 81 .or. sg_num .eq. 82) then
          pgnr = 10
      else if (sg_num >=83 .and. sg_num <89) then
          pgnr = 11
      else if (sg_num >=89 .and. sg_num <99) then
          pgnr = 12
      else if (sg_num >=99 .and. sg_num < 111) then
          pgnr = 13
      else if (sg_num >=111 .and. sg_num < 123) then
          pgnr = 14
      else if (sg_num >=123 .and. sg_num < 143) then
          pgnr = 15
      else if (sg_num >=143 .and. sg_num < 147) then
          pgnr = 16
      else if (sg_num >=147 .and. sg_num < 149) then
          pgnr = 17
      else if (sg_num .eq. 149 .or. sg_num .eq. 151 .or. sg_num .eq. 153) then
          pgnr = 18
      else if (sg_num .eq. 150 .or. sg_num .eq. 152 .or. sg_num .eq. 154 .or. sg_num .eq. 155) then 
          pgnr = 19
      else if (sg_num .eq. 156 .or. sg_num .eq. 158 .or. sg_num .eq. 160 .or. sg_num .eq. 161) then
          pgnr = 20
      else if (sg_num .eq. 157 .or. sg_num .eq. 159) then
          pgnr = 21
      else if (sg_num >= 164 .and. sg_num < 168 ) then
          pgnr = 22
      else if (sg_num .eq. 162 .or. sg_num .eq. 163) then
          pgnr = 23
      else if (sg_num >=168 .and. sg_num < 174) then
          pgnr = 24
      else if (sg_num .eq. 174) then
          pgnr = 25
      else if (sg_num .eq. 175 .or. sg_num .eq. 176) then
          pgnr = 26
      else if (sg_num >= 177 .and. sg_num < 183) then
          pgnr = 27
      else if (sg_num>=183 .and. sg_num < 187) then
          pgnr = 28
      else if (sg_num .eq. 187 .or. sg_num .eq. 188) then
          pgnr = 29
      else if (sg_num .eq. 189 .or. sg_num .eq. 190) then
          pgnr = 30
      else if (sg_num >= 191 .and. sg_num < 195) then
          pgnr = 31
      else if (sg_num >=195 .and. sg_num < 200) then
          pgnr = 32
      else if (sg_num >=200 .and. sg_num <207) then
          pgnr = 33
      else if (sg_num >=207 .and. sg_num < 215) then
          pgnr = 34
      else if (sg_num >= 215 .and. sg_num < 221) then
          pgnr = 35
      else if (sg_num >=221 .and. sg_num <=230) then
         pgnr = 36
      end if
    end subroutine getpgnr

     
  ! The inverse of group element I is calcualted and stored in inel(I)
  subroutine sym_inverse(inel, G, multab)
    integer, intent(out) :: inel(:)

    integer, intent(in) :: G
    integer, intent(in) :: multab(:,:)

    integer :: I, J, II
    inel(:) = 0
    do I = 1, G
       if (inel(I) == 0) then
          do J = 1, G
             if (multab(I,J) == 1) then
                inel(I) = J
                inel(J) = I
                exit
             end if
          end do
       end if
    end do
    write(*,*) "inel", inel(:)
    do I = 1, G
       if(inel(I) ==  0) then
          write(*,*) "No inverse element for group element", I
          write(*,*) "multab(" , G, ") is "
          do II = 1, G
            write(*,*) multab(:, II)
          end do 
       end if
    end do
  end subroutine sym_inverse

  ! calucate the factorial N of K
  subroutine fac(N, K)
    integer, intent(out) :: N
    integer, intent(in) :: K

    integer :: I 
    if(K < 0) then
       return
    end if
    if (K==0) then
       N = 1
    else
       N = 1
       do I = 2,K
          N = N*I
       end do
    end if
  end subroutine fac
  
  ! calculates the D-matrices
  subroutine dmatr(D, L, fi, theta, psi)
    complex(dp), intent(out) :: D(:,:)
    integer, intent(in) :: L
    real(dp), intent(in) :: fi
    real(dp), intent(in) :: theta
    real(dp), intent(in) :: psi

    integer :: I , I1, L1, L2, L3
    integer :: K1, K2, K3, K4, K5, K6
    integer :: M1, M2, N, n1, N2
    complex(dp) :: afi, apsi 
    real(dp) :: cost, sint, T, Q1, Q2
    integer :: fac3, fac4, fac5, fac6, faci
    real(dp), allocatable :: Dord(:,:,:)
    
    !write(*,*) "DM_ANGLES ", fi, theta, psi
    T = theta/2
    cost = cos(T)
    sint = sin(T)
    N = 2*L + 1
    allocate(Dord(2, N, N))
    do K1 = 1, N
       M1 = K1 - L - 1
       do K2 = 1, N
          M2 = K2 - L - 1
          L1 = 1
          if ((M1 - M2) > 0) then
             L1 = M1 - M2 + 1
          end if
          L2  = L + M1 + 1
          if ((M1 + M2) > 0) then
             L2 = L - M2 + 1
          end if
          D(K1, K2) = cmplx(0,0)
          do I1 = L1, L2
             I = I1 - 1
             T = 1.0
             K3 = L - M1
             K4 = L + M1
             K5 = L - M2
             K6 = L + M2
             call fac(fac3, K3)
             call fac(fac4, K4)
             call fac(fac5, K5)
             call fac(fac6, K6)
             T = T*fac3* fac4 * fac5 * fac6
             K3 = L - M2 - I
             K4 = L + M1 -I
             K5 = I + M2 - M1
             call fac(fac3, K3)
             call fac(fac4, K4)
             call fac(fac5, K5)
             call fac(faci, I)
             T = sqrt(T)/(fac3 * fac4 * fac5 * faci)
             N2 = (-1)**I
             T = T*N2
             K3 = 2*L - 2*I - M2 + M1
             Q1 = cost**K3
             K4 = 2*I + M2 - M1
             Q2 = sint**K4
             T = T*Q1*Q2
             D(K1, K2) = D(K1, K2) + T
          end do
          afi = cmplx(0, -M1*fi)
          apsi =cmplx(0, -M2*psi)
          D(K1,K2) = D(K1, K2)*exp(afi+apsi)
       end do
    end do

    do K1=1, N
       do K2=1, N
          Dord(1, K1, K2) = real(D(K1, K2))
          Dord(2, K1, K2) = aimag(D(K1, K2))
       end do
    end do

    deallocate(Dord)
  end subroutine dmatr

  subroutine sym_matinv(a, nrow)
    real(dp), intent(inout) :: a(:,:)
    integer, intent(in) :: nrow

    integer :: info
    integer, allocatable :: ipiv(:)
    real(dp), allocatable :: work(:)

    allocate(ipiv(nrow),work(nrow))
    !     LU decomoposition of a general matrix
    call dgetrf(nrow,nrow,a,nrow,ipiv,info)
    if (info == 0) then
       !     generate inverse of a matrix given its LU decomposition
       call dgetri(nrow,a,nrow,ipiv,work,nrow,info)
    endif
    deallocate(ipiv,work)
    if(info .ne. 0)then
       write(*,*)"Error in matrix inversion!"
       write(*,*)"Error code",info
       error stop
    endif
  end subroutine sym_matinv

  ! A set of group elements that can produce all other elements by group element
  ! multiplication, are generated. Element number map(L,1) multiplied by element
  ! number map(L,2), gives element number L. If map(L,2) == 0, L is one of the
  ! generators. ngen(I) = group element index of the Ith generator. nmberg = the
  ! total number of generators. The list of group elements is traversed,
  ! starting with N = 1. When N has been increased by 1, it is checked that N
  ! has not been produced by multiplication of generators, that already have
  ! been registred. If not, map(N,1) is still equal to zero and we register N as
  ! a new generator. The number N2 of group elements that not yet have been
  ! checked, is diminished by 1. Then we form the products of N with all
  ! preceding elements. If these products give new elements, these are
  ! registered and N2 is diminished by 1. All preceding group elements may be
  ! used in these products, since if they are not generators themselves, it has
  ! been registered how they can be produced by generators. When N2 = 0, all
  ! group elements have been treated and those L with map(L,2) = 0, are the 
  ! generators.

  subroutine sym_genera(map, nmberg, ngen, G, multab)
    integer, intent(out) :: map(:,:)
    integer, intent(out) :: nmberg
    integer, intent(out) :: ngen(:)

    integer, intent(in) :: G
    integer, intent(in) :: multab(:,:)

    integer :: L, N2, M, N1, N

    do L = 1, G
       map(L, 1) = 0
       map(L, 2) = 0
    end do
    map(1,1) = 1
    if(G > 1) then
       N2 = G - 1
       do N = 2, G
          if(map(N, 1) == 0) then
             map(N, 1) = N
             N2 = N2 - 1
             if(N2 == 0) then
                exit
             end if
          end if
          do M = 2, N
             N1 = multab(M, N)
             if((N1>N) .and. (map(N1, 1) == 0)) then
                map(N1,1) = M
                map(N1,2) = N
                N2 = N2 - 1
                if( N2 == 0) then
                   exit
                end if
             end if
          end do
          if( N2 == 0) then
             exit
          end if
          do M = 2, N
             N1 = multab(N, M)
             if((N1>N) .and. (map(N1, 1) == 0)) then
                map(N1,1) = N
                map(N1,2) = M
                N2 = N2 - 1
                if( N2 == 0) then
                   exit
                end if
             end if
          end do
          if( N2 == 0) then
             exit
          end if
       end do
       nmberg = 1
       ngen(1) = 1
       do L = 2, G
          if(map(L,2) == 0) then
             nmberg = nmberg + 1
             ngen(nmberg) = L
          end if
       end do
    else
       nmberg  = 1
       ngen(1) = 1
    end if
  end subroutine sym_genera

end module genera
