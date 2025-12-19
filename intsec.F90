module intsec
  use accuracy
  use constants
  implicit none
  private
  public :: sym_intsec

contains
  
  !https://math.stackexchange.com/questions/2265341/calculating-dimension-of-the-intersection-of-two-subspaces
  ! calculate the intersection of two vectors spaces stored in fi and dfi with dimension nvr2 and nvr1, respectively.
  ! the resulting space is stored in fi
  !Let 𝑈 and 𝑉 be two sub spaces(in matrix form: columns as basis vectors). Let 𝑧 be a vector that lies in intersection of these two sub spaces. Then ∃ two coeff vectors 𝑥,𝑦 such that
!𝑧=𝑈𝑥𝑈𝑥𝑈𝑇𝑈𝑥𝑥Thus𝑥𝑥=𝑉𝑦=𝑉𝑦=𝑈𝑇𝑉𝑦=(𝑈𝑇𝑈)−1𝑈𝑇𝑉𝑦 and similarly 𝑦=(𝑉𝑇𝑉)−1𝑉𝑇𝑈𝑥=(𝑈𝑇𝑈)−1𝑈𝑇𝑉(𝑉𝑇𝑉)−1𝑉𝑇𝑈𝑥=𝑀̂ 𝑥,
!where,
!𝑀̂ =(𝑈𝑇𝑈)−1𝑈𝑇𝑉(𝑉𝑇𝑉)−1𝑉𝑇𝑈
!We can see that 𝑥 is the Eigen vector of 𝑀̂  corresponding to Eigen value 1. Thus required basis is the set of independent vectors such that
!{𝑈𝑥:𝑀̂ 𝑥=𝑥}
!
!In another way, let 𝑀1^=(𝑈(𝑈𝑇𝑈)−1𝑈𝑇)(𝑉(𝑉𝑇𝑉)−1𝑉𝑇)=𝑃𝑈𝑃𝑉. The required basis is the set of independent vectors such that
!{𝑠:𝑀1^𝑠=𝑠}
!
!Geometrically 𝑃𝑈=𝑈(𝑈𝑇𝑈)−1𝑈𝑇 and 𝑃𝑉=𝑉(𝑉𝑇𝑉)−1𝑉𝑇 are projection matrices onto the sub spaces 𝑈 and 𝑉 respectively. So we can see that the basis elements are those independent vectors, which remain unchanged after two projections, corresponding to the given two sub spaces.

  ! fi(1:G, nvr2)
  ! dfi(nvr1, 1:G)
  
  subroutine sym_intsec(fi, K5, nvr1, nvr2, G, dfi)
    complex(dp), intent(inout) :: fi(:,:)
    integer, intent(inout) :: K5

    integer, intent(in) :: nvr1
    integer, intent(in) :: nvr2
    integer, intent(in) :: G
    complex(dp), intent(in) :: dfi(:,:)

    integer :: K6, I4, I5, I6, I7 
    integer :: nvr12, II, I, J
    real(dp) :: rnorm, rnorm2

    integer, allocatable :: ntry2(:)
    integer, allocatable :: ntry3(:)
    complex(dp), allocatable :: vec(:)
    complex(dp), allocatable :: vec2(:)
    complex(dp), allocatable :: fin(:,:)
    complex(dp) , allocatable :: dfin(:,:)
    complex(dp), allocatable :: tvec(:)
    complex(dp), allocatable :: tvec2(:)

    allocate(ntry2(G))
    allocate(ntry3(G))
    
    allocate(vec(G))
    allocate(vec2(G))

    allocate(tvec(G))
    allocate(tvec2(G))
    
    allocate(dfin(nvr2, G))
    allocate(fin(G, nvr2)) 

    
    ntry2(:) = 0.0
    ntry3(:) = 0.0
!! the normals to the spaces spanned by fi and dfi in the sum of both spaces, are calculated and stored in dfin and fin
    ! dim( nv12 ) = dim(nvr1) + dim(nvr2)

    nvr12 = nvr1 + nvr2
    
    !write(*,*) "fi", fi(:,:)
    !dfin(1:nvr2, 1:G)=transpose(fi(1:G, 1:nvr2))
    
    K6 = 1
    
    do K5= nvr2, nvr12
       do while (K6 <= nvr1)
          do I4= 1, K5
             if(I4 <= nvr2) then
                vec(I4) = dot_product(dfi(K6, 1:G), conjg(fi(1:G, I4)))
             else
                vec(I4) = dot_product(dfi(K6, 1:G), dfin(I4-nvr2, 1:G))
             end if
          end do
          I7 = K5 - nvr2
          do I4 =1,G
             vec2(I4) = dfi(K6, I4)

             do I5 = 1, nvr2
                vec2(I4) = vec2(I4) - vec(I5)*fi(I4,I5)
             end do

             if (I7 .ne. 0) then
                do I6 = 1, I7
                   vec2(I4) = vec2(I4) - vec(I6+nvr2)*dfin(I6,I4)
                end do
             end if
          end do
          I4 = 1
          do while(I4 <= G)
             if (abs(vec2(I4)) * abs(vec2(I4)) < 0.001) then
                I4 = I4 + 1
             else
                exit
             end if
          end do
          if(I4 <=G) then
             rnorm = dot_product(vec2(1:G), vec2(1:G))
             rnorm = 1/sqrt(rnorm)
             do II = 1, G
                dfin(I7+1, II) = vec2(II) * rnorm
             end do
             ntry2(K6) = 1
             K6 = K6 + 1
             exit
          end if
          K6 = K6 + 1
       end do
       if(K6 > nvr1) then
          exit
       end if
    end do
    K6 = 1
    
    do K5 = nvr1, nvr12
       do while (K6 <= nvr2)
          do I4 = 1,K5
             if (I4 <= nvr1) then
                vec(I4) = dot_product(conjg(dfi(I4, 1:G)), fi(1:G, K6))
             else
                vec(I4) = dot_product(fin(1:G, I4-nvr1),  fi(1:G, K6))
             end if
          end do
          I7 = K5 - nvr1
          do I4=1,G
             vec2(I4) = fi(I4, K6)
             do I5=1,nvr1
                vec2(I4) = vec2(I4) - vec(I5)*dfi(I5, I4)
             end do
             if( I7 .ne. 0) then
                do I6 = 1,I7
                   vec2(I4) = vec2(I4) - vec(I6+nvr1)*fin(I4,I6)
                end do
             end if
          end do
          I4 = 1
          do while( I4 <= g)
             if (abs(vec2(I4))*abs(vec2(I4)) < 0.001) then
                I4 = I4 + 1
             else
                exit
             end if
          end do
          if (I4 <= G) then
             rnorm = dot_product(vec2(1:G), vec2(1:G))
             rnorm = 1/sqrt(rnorm)
             fin(1:G, I7+1) = vec2(1:G)* rnorm
             ntry3(K6) = 1
             K6 = K6 + 1
             exit
          end if
          K6 = K6 + 1
       end do
   
       if(K6 > nvr2) then
          exit
       end if
    end do
    K5 = 0
    K6 = 0
    do I4=1, G
       K5 = K5 + ntry2(I4)
       K6 = K6 + ntry3(I4)
    end do
    
    ! K5 norms to fi and the K6 normals to dfi are stored in dfin and fin
    ! the wanted intersection of spaces fi and dfi is to be simultaneously orthogonal
    ! to the space spanned by fin and dfin
    I7 = K6
    do I4=1, K5
       do II = 1, G
          tvec(II) = dfin(I4, II)
       end do
       do II = 1, I7

          tvec2(1:G) = conjg(fin(1:G, II))
          vec(II) = dot_product(tvec, tvec2)
       end do
       !vec(1:I7) = matmul(dfin(I4, 1:G) , conj(fin(1:G, 1:I7)))
       do I5 = 1, G
          vec2(I5) = dfin(I4, I5)
          do I6 =1, I7
             vec2(I5) = vec2(I5) - vec(I6)*fin(I5,I6)
          end do
       end do
       I5 = 1
       do while( I5 <= G)
          if(abs(vec2(I5)) < 0.001) then
             I5 = I5 + 1
          else
             exit
          end if
       end do
       if ( I5 <= G) then
          I7 = I7 + 1
          rnorm = 0
          rnorm = dot_product(vec2(1:G), vec2(1:G))
          !rnorm = sum(abs(transpose(vec2(1:G))) * abs(transpose(vec2(1:G))))
          rnorm = 1/sqrt(rnorm)
          fin(1:G, I7) = vec2(1:G) * rnorm
       end if
      
       
    end do
    
    ! an orthonormal basis for the sum of the spaces spanned by fin and dfin
    ! is now stored in fin with I7 basis vectors. By orthogonalizing the basis
    ! vectors of dfi to those of fin, we find the intersection of fi and dfi
    K5 = 0
    K6 = I7
    do I4=1, nvr1
       do I5=1, I7
          if (I5 <= K6) then  
             vec(I5) = dot_product(dfi(I4, 1:G), conjg(fin(1:G, I5)))
          else
             do II = 1, G
                vec(I5) = vec(I5) + dfi(I4, II) * dfin(I5-K6, II)
             end do
          end if
       end do

       do I5=1, G
          vec2(I5) = dfi(I4, I5)
          do I6 = 1, I7
             if(I6 <= K6) then
                vec2(I5) = vec2(I5) - vec(I6)*fin(I5,I6);
             else
                vec2(I5) = vec2(I5) - vec(I6)*dfin(I6-K6,I5)
             end if
          end do
       end do
       I5 = 1
       do while( I5 <= G)
          if(abs(vec2(I5)) < 0.001) then
             I5 = I5 +1
          else
             exit
          end if
       end do
       if(I5 <= G) then
          I7 = I7 + 1
          K5 = K5 + 1
          rnorm = dot_product(vec2(1:G), vec2(1:G))
          !rnorm = sum(abs(transpose(vec2(1:G)))*abs(transpose(vec2(1:G))))
          rnorm = 1/sqrt(rnorm)
          do II = 1, G
             dfin(I7-K6, II) = vec2(II)*rnorm
          end do
       end if
    end do
    
    if (K5 .ne. 0) then
       ! the intersection of fi and dfi is stored in fi
       do I = 1, G
          do J = 1, K5
             fi(I, J) = dfin(J, I)
          end do
       end do
       !fi(1:G,1:K5) = transpose(dfin(1:K5, 1:G))
    end if
    
    deallocate(ntry2)
    deallocate(ntry3)
    deallocate(vec)
    deallocate(vec2)
    deallocate(fin)
    deallocate(tvec)
    deallocate(tvec2)
    deallocate(dfin)
  end subroutine sym_intsec
end module intsec
