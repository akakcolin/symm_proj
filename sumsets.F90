module sumsets
  use accuracy
  use constants
  implicit none
  private
  public :: sym_sumsets
  
contains
  ! Formation of the summation sets G(MU,NU). nup is the number of projection
  ! matrices for this wave vector. We form np(K,I,J), which is the number
  ! of elements of the group of the k-vector (groupk), for which the difference
  ! rgr*(coordinate of atom I) - (coordinate of atom J) of chemical element K,
  ! is a lattice vector. npl(1:nel,1:nat(I1),1:nat(I1),1:np(1:nel,1:nat(I1),
  ! 1:nat(I1))) gives the indices of these pointgroup operators.
  ! nvec(1:nel,1:nat(I1),1:nat(I1),1:np(1:nel,1:nat(I1),1:nat(I1)),1:3)
  ! gives the corresponding lattice vectors.
  
  subroutine sym_sumsets( np, nvec, npl, til, kgord, kgel, rgr, listp, a, ai, b, r, u, nel, nat, ksym, ibz, steer)
    integer, intent(inout) :: np(:,:,:)
    real(dp), intent(inout) :: nvec(:,:,:,:,:)
    integer, intent(inout) :: npl(:,:,:,:)
    real(dp), intent(in) :: rgr(:,:,:)
    real(dp), intent(in) :: til(:,:)
    real(dp), intent(in) :: a(:,:), b(:,:), ai(:,:), r(:,:,:), u(:,:)
    integer, intent(in) :: ksym
    integer, intent(in) :: ibz
    integer, intent(in) :: steer(:)
    integer, intent(in) :: nat(:)
    integer, intent(in) :: nel
    integer, intent(in) :: kgord
    integer, intent(in) :: kgel(:)
    integer, intent(in) :: listp(:)
  

    real(dp), dimension(3) :: difi, dif, trac

    integer :: I, I1, I2, I3, I4, I5, I6, I7, I8, II 
    integer :: J, isign, K, ifd
    real(dp) ::  D

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
                   !write(*,*) "rgr, r", rgr(I6, I7, I4), r(I, I1, I7)
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
                   !dot_product(ai(1:3, II), dif(1:3))
                end do
                !difi(1:3) = matmul(dif(1:3), ai(1:3, 1:3))
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
                exit
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

  end subroutine sym_sumsets
end module sumsets
