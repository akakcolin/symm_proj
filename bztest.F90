module bztest 
  use accuracy
  use constants
  implicit none
  private
  public ::sym_bztest

contains
 
  ! determines if vector rk lies within or at the border of the first Brillouin zone
  ! rk-components of reciprocal lattice vectors
  ! (b1, b2,b3) = (x,y,z)*b
  ! cart is the cartesian components of rk
  ! ntz = -1 if within the brillouin zone
  ! ntz = 0 if on the border of the first Brillouin zone
  ! ntz = 1 if outside of the brillouin zone
  
  subroutine sym_bztest(ntz, rk, b)
    integer, intent(out):: ntz

    real(dp), intent(in) :: rk(:)
    real(dp), intent(in) :: b(:,:)

    integer :: I, I1, K1, K2
    integer :: nbz
    integer, dimension(3):: signe
    real(dp) :: bnorm, ckinp
    real(dp), dimension(3) :: cark, c
    
    signe(:) = 1

    do I = 1,3
       IF(rk(I) < 0) then
          signe(I) = -1
       end IF
    end do
    ! need to check 
    cark(1) = b(1,1)*rk(1) + b(1,2)*rk(2) + b(1,3) * rk(3)
    cark(2) = b(2,1)*rk(1) + b(2,2)*rk(2) + b(2,3) * rk(3)
    cark(3) = b(3,1)*rk(1) + b(3,2)*rk(2) + b(3,3) * rk(3)
    
    !cark(1:3) = b(1:3, 1:3)*transpose(rk(1:3))
    nbz = 0
    do I1 = 1, 7
       if(I1 <= 3) then
          !c(1:3) = b(1:3, I1)*signe(I1)
          c(1) = b(1, I1)*signe(I1)
          c(2) = b(2, I1)*signe(I1)
          c(3) = b(3, I1)*signe(I1)
       end if
       if(I1 > 3) then
          if (I1 < 7) then
             if ( I1 == 4) then
                K1 = 1
                K2 = 2
             end if
             if (I1 == 5) then
                K2 = 3
             end if
             if( I1== 6) then
                K1 = 2
             end if
             !
             c(1) = b(1,K1)*signe(K1) + b(1,K2)*signe(K2)
             c(2) = b(2,K1)*signe(K1) + b(2,K2)*signe(K2)
             c(3) = b(3,K1)*signe(K1) + b(3,K2)*signe(K2)
             !c(1:3) = b(1:3, K1)*signe(K1) + b(1:3, K2)*signe(K2)
          else
             c(1) = b(1,1)*rk(1) + b(1,2)*rk(2) + b(1,3) * rk(3)
             c(2) = b(2,1)*rk(1) + b(2,2)*rk(2) + b(2,3) * rk(3)
             c(3) = b(3,1)*rk(1) + b(3,2)*rk(2) + b(3,3) * rk(3)
             !c(1:3) = b(1:3, 1:3)* transpose(signe(1:3))
          end if
       end if
       ! test if the projection of rk on vector c is less then half the length of c
       bnorm = 0.5*(c(1)**2 + c(2)**2 + c(3)**2)
       ! the adapted signs ensure tests in the right octant
       ckinp = dot_product(cark(:), c(:))
       ckinp = ckinp - bnorm
       if (ckinp < -0.001) then
          nbz = nbz  + 1
       end if
       if (ckinp > 0.001) then
          ! outside the brillouin zone
          ntz = 1
          return
       end if
    end do
    if (nbz == 7)  then
       ! wehen all seven tests are fulfilled, then the rk lies inside the BZ 
       ntz = -1
       return
    else
       ! rk lines on the border of the Brillouin zone 
       ntz = 0
       return
    end if
  end subroutine sym_bztest
end module bztest
