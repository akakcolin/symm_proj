module degen 
  use accuracy
  use constants
  use intsec
  use subsp
  use eigvec
  implicit none
  private
  public :: sym_degen, fix

contains

  integer function fix(a)
    real, intent(in) :: a

    if(a>0) then
       fix = floor(a)
    else
       fix= ceiling(a)
    end if
  end function fix

  !In case there is no non-degenerate eigenvalue for any group element in the irreducible
  !representation, function repres calls on function degen.

  !The eigenvectors of group element IN, corresponding to eigenvalue lab in the regular
  !representation are calculated using function eigvec. A set of mutual independent,
  !commuting group elements to element IN is calculated and stored in ntry1(1:I3). Group
  !elements from the set ntry1(1:I3) are successively taken and the eigenvectors
  !corresponding to possible eigenvalues of the group element. Function intsec is called
  !where the intersection of the subspaces spanned by the eigenvectors of IN and a group
  !element belonging to the set ntry1(1:I3). If this intersection spanned by an orthonormal
  !basis has the dimension LJ1, an othonormal basis which transforms irreducibly is
  !formed by operating on the vectors with group elements not belonging to the loop of IN.
  !A set of orthonormal columns is no formed, which will transform the regular
  !representation to the irreducible one when returned to function repres.

  subroutine sym_degen(fi, LJ1, J, KJ, IN, lab, multab, G, ngen, map, &
          & nmberg, cind, norder, steer, inel, ch, sirt)
    complex(dp), intent(inout) :: fi(:,:)
    integer, intent(inout) :: LJ1
    
    integer, intent(in) :: J
    integer, intent(in) :: KJ
    integer, intent(in) :: IN
    integer, intent(in) :: multab(:,:)
    integer, intent(in) :: G
    integer, intent(in) :: ngen(:)
    integer, intent(in) :: map(:,:)
    integer, intent(in) :: nmberg
    integer, intent(in) :: cind(:)
    integer, intent(in) :: norder(:)
    integer, intent(in) :: steer(:)
    integer, intent(in) :: inel(:)
    integer, intent(in) :: sirt(:, :)
    complex(dp), intent(in) :: lab
    complex(dp), intent(in) :: ch(:,:)
    
    integer :: mg
    integer :: II, I2, I3, I4, IV
    integer :: KI, K1, K2, K3, K4, K5, K6, K11
    integer :: J1, J2, J3, J4, J7, LG, nvru
    integer :: L1, L2, ML1, ML2, nvr, K111
    integer :: kloop, nvr1, nvr2
    integer :: D, dprim, isec

    real(dp) :: rinab
    real(dp) :: rnorm, rnorm2
    complex(dp) :: pi2i
    complex(dp) :: RIN
    integer, allocatable :: ntry1(:), ntry2(:), ntry3(:), ntry4(:)
    real(dp), allocatable :: vec(:)
    real(dp), allocatable:: eigenv(:)
    complex(dp), allocatable :: dfi(:,:)

    allocate(ntry1(G))
    allocate(ntry2(G))
    allocate(ntry3(G))
    allocate(ntry4(G))

    allocate(dfi(G,G))
    allocate(vec(G))
    allocate(eigenv(G))
        
    
! In case of degeneracy in the eigenvalues, function degen calculates
! a common set of mutual independent commuting group elements and specifies
! a unique common eigencolumn, if possible. 

    pi2i = cmplx(0, 2*pi)
    
    kloop = 0
  
    call sym_eigvec(fi, nvr1, IN, lab, J, kloop, inel, cind, ch, multab, G, steer)
    
    if (nvr1 > 0) then
       D = fix(real(nvr1/LJ1))
       dfi(1:nvr1, 1:G) = transpose(fi(1:G, 1:nvr1))
       write (*,*) "dfi(1:nvr1, 1:G)", dfi(1:nvr1, 1:G)
       ! calucate a set of mutual independent, commuting group elements to element IN
       ntry2(:) = 0
       I2 = 1
       ntry2(I2) = 1
       I2 = multab(I2, IN)
       do while ( I2 .ne. 1)
          ntry2(I2) = 1
          I2 = multab(I2, IN)
       end do

       ntry1(1) = IN
       I3 = 1
       do I2= 2, G
          if (ntry2(I2) .ne. 1) then
             KI = 1
             do while (KI <= I3)
                if (multab(ntry1(KI), I2) .ne. multab(I2, ntry1(KI))) then
                   exit
                end if
                KI = KI + 1
             end do
             if (KI > I3) then
                ntry3(:) = 0
                K2 = I2
                ntry3(K2) = 1
                K2 = multab(K2, I2)
                do while ( K2 .ne. 1)
                   ntry3(K2) = 1
                   K2 = multab(K2, I2)
                end do
                I3 = I3 + 1
                ntry1(I3) = I2
                K3 = cind(I2)
                K3 = norder(K3) - 1
                do K2 = 2, G
                   if(ntry2(K2) .ne. 0) then
                      K4 = 1
                      do K5= 1, K3
                         K4 = multab(K4, I2)
                         k6 = multab(K2, K4)
                         ntry3(K6) = 1
                      end do
                   end if
                end do
                do K2 = 2, G
                   if (ntry3(K2) == 1) then
                      ntry2(K2) = 1
                   end if
                end do
             end if
          end if
       end do

       k2 = 1
       do I2 = 1, G
          if (ntry2(I2) == 1) then
             ntry4(K2) = I2
             K2 = K2 + 1
          end if
       end do
       nvru = K2 - 1
       if(I3 .ne. 1) then
          write(*,*) "Commuting group elements"
          write(*,*) ntry1(1:I3)
       end if

       ! A set of commuting group elements is stored in ntry1(1:I3).
       ! We are looking for common eigenvaectors to the set ntry1(1:I3)
       I2 = 2
       do while (I2 <= I3)
          K2 = ntry1(I2)
          K3 = cind(K2)
          K3 = norder(K3)
         
          do K4 = 1, K3
             eigenv(K4) = exp(pi2i*(K4 - 1)/ K3)
          end do
          do K4 = 1, K3
             kloop = 0
             RIN = eigenv(K4)
             call sym_eigvec(fi, nvr2, K2, RIN, J, kloop, inel, cind, ch, multab, G, steer)
             kloop = 1
             if (nvr2 .ne. 0) then
                call sym_intsec(fi, isec, nvr1, nvr2, G, dfi)
                if (isec .ne. 0) then
                   if (isec == LJ1) then
                      exit
                   end if
                   nvr1 = isec
                   dfi(1:nvr1, 1:G) = transpose(fi(1:G, 1:nvr1))
                end if
             end if

          end do
          if( isec == LJ1) then
             exit
          end if
          I2 = I2 + 1
       end do
       if(I2 > I3) then
          write(*,*) "Error, not enough basis vectors (only ", LJ1, " for irrep ", J
       end if
    else
       write(*,*) "No commuting element for element ", IN
    end if

    if (isec .ne. LJ1) then
       call sym_subsp(dfi, nvr1, ntry4, nvru, J, LJ1, G, inel, multab, steer, cind, ch, norder)

       !!! we cancel calling mgt , still have warning
       if (nvr1 .ne. LJ1) then
          write(*,*) "Here, we need to call mgt function !!!!"
          LJ1= 0
       end if
       fi(1:G, 1:nvr1) = transpose(dfi(1:nvr1, 1:G))
    end if

    ! dfi has been reduced to A LJ1 times degenerate space
    ntry1(:) = 0
    K3 = sirt(J, 2)
    K4 = IN
    do K5 = 1, K3
       K4 = multab(K4, IN)
       ntry1(K4) = 1
    end do
    I2 = 1
    do while ( I2 <= LJ1)
       do II = 1, G
          dfi(1, II) = fi(II, I2)
       end do
       IV = 1
       I3 = 1
       do while( I3 <= G)
          if (ntry1(I3) .ne. 1) then
             ! operate on dfi(1, I) with group element I3
             K3 = inel(I3)

             do II = 1, G
                mg = multab(K3, II)
                vec(II) = dfi(1, mg)
             end do
             I4 = 1
             do while ( I4 <= IV)
                rin = dot_product(vec(1:G), dfi(I4, 1:G))
                rinab = abs(rin)
                if(abs(rinab - 1) >= 0.000001) then
                   if (rinab >= 0.000001) then
                      do K4 = 1, G
                         vec(K4) = vec(K4) - rin*dfi(I4, K4)
                      end do
                      rnorm = dot_product(vec(1:G), vec(1:G))
                      !rnorm = sum ( vec(1:G)*vec(1:G))
                      if (rnorm < 0.000001) then
                         exit
                      end if
                      rnorm2 = 1/sqrt(rnorm)
                      vec(1:G) = rnorm2*vec(1:G)
                   end if
                else
                   exit
                end if
                I4 = I4 + 1
             end do
             if( I4 > IV) then
                IV = IV + 1
                do II = 1, G
                   dfi(IV, II) = vec(II)
                end do
                if (IV >= LJ1) then
                   fi(1:G, 1:LJ1) = transpose(dfi(1:LJ1, 1:G))
                   stop
                end if
             end if
          end if
          I3 = I3 + 1
       end do
       I2 = I2 + 1
    end do
    deallocate(dfi)
    deallocate(vec)
    deallocate(eigenv)
    deallocate(ntry1)
    deallocate(ntry2)
    deallocate(ntry3)
    deallocate(ntry4)
  end subroutine sym_degen

end module degen
