module irrep 
  use accuracy
  use constants
  use genera
  use charac
  use classes
  use degen
  use repres
   
  implicit none
  private
  public :: sym_irrep

contains
  
  subroutine sym_irrep(jpdd, allow, ncl, laj, cind_invp, &
       & G , mtab2, prime,steer, ibz, ksym, nopi1, nopli1, sil)
    complex(dp), intent(out) :: jpdd(:,:,:)
    integer, intent(out) :: allow(:)
    integer, intent(out) :: ncl
    integer, intent(out) :: laj(:)
    integer, intent(out) :: cind_invp(:)
    
    integer, intent(inout) :: steer(:)    
    integer, intent(in) :: G
    integer, intent(in) :: mtab2(:,:)
    integer, intent(in) :: prime(:)

    integer, intent(in) :: ibz
    integer, intent(in) :: ksym
    integer, intent(in) :: nopi1
    integer, intent(in) :: nopli1(:)
    complex(dp), intent(in) :: sil(:)

    integer, allocatable :: cind(:)
    integer, allocatable :: classl(:)
    integer, allocatable :: inel(:)
    integer, allocatable :: h(:), nfirst(:)
    integer, allocatable :: tmp_nfirst(:), tmp_h(:)
    integer :: I, J, M, L 
    integer, allocatable :: A(:)
    complex(dp), allocatable :: ch(:,:)
    integer, allocatable :: lj(:)
    integer, allocatable :: map(:,:)
    integer, allocatable :: ngen(:), norder(:), sirt(:,:), onert(:)
    integer :: nmberg
    
    ! input group order G
    ! read an identification of the program
    ! read the multiplication table of the group
    M = G*(G+1) /2
    ! a short check is made, that the input multipliction table is correct
    allocate(A(G))
    allocate(inel(G))
    allocate(classl(G))
    allocate(map(G,2))
    allocate(ngen(G))
    allocate(tmp_nfirst(M))
    allocate(tmp_h(M))

    A = 0
    do I= 1, G
       do J = 1, G
          A(I) = A(I)  + mtab2(I, J)
       end do
    end do

    do I = 1, G
       if (A(I) .ne. M) then
          steer(11) = 0
       end if
    end do

    if(steer(11) .ne. 0) then
       ! if steer(11) is true, the multiplication table will be printed, row by row
       write(*,*) "The group multiplication table:"
       do I= 1, G
          write(*,*) mtab2(I,1:G)
       end do
    end if
    ! The inverse group elements are calculted and stored in inel(J)
    call sym_inverse(inel, G, mtab2)
    ! a set of group element generators are calculated
    call sym_genera(map, nmberg, ngen, G, mtab2)

    ! the group calsses are calculated
    call sym_classes(tmp_nfirst, tmp_h, classl, ncl, G, mtab2, inel)
    
    allocate(nfirst(ncl))
    allocate(h(ncl))
    do I = 1, ncl
       nfirst(I) = tmp_nfirst(I)
       h(I) = tmp_h(I)
    end do 
    deallocate(tmp_nfirst)
    deallocate(tmp_h)
    
    allocate(ch(ncl, ncl))
    allocate(lj(ncl))
    allocate(norder(ncl))
    allocate(onert(ncl))
    allocate(sirt(ncl,3))
    allocate(cind(G))
    sirt(:,:)=0
    ch(:,:)=0

    ! the irreducible group characters are caluculated
    call sym_charac(ch, cind, onert, sirt, lj, norder, cind_invp, mtab2, G, ncl, &
         & h, nfirst, classl, inel, steer, prime)

    ! the irreducible representation of the group elements are calculated
    call sym_repres(jpdd, allow, laj, mtab2, G, steer, inel, lj, ncl, &
         & nfirst, classl, sirt, onert, cind, ch, nmberg, ngen, &
         & map, norder, ibz, ksym, nopi1, nopli1, sil)
    
    deallocate(A)
    deallocate(inel)
    deallocate(h)
    deallocate(nfirst)
    deallocate(classl)
    deallocate(ch)
    deallocate(lj)
    deallocate(ngen)
    deallocate(map)
    deallocate(norder)
    deallocate(onert)
    deallocate(sirt)
    deallocate(cind)
    write(*,*) "Irrep has finished executing"
    
  end subroutine sym_irrep
  
  ! find an irreducible vector, if possible, within
  ! a certain subspace of the irreducible space
  !
end module irrep

