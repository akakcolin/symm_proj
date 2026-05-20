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
    integer :: match_atom
    integer :: match_count
    real(dp) ::  D
    real(dp), dimension(3) :: mapped_shift

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

             call find_unique_atom_mapping(trac, r(:, I, 1:J), J, ai, tol_lattice_integer, &
                  & match_atom, mapped_shift, match_count)

             if (match_count /= 1) then
                write(*,*) "Wrong space group or ambiguous atom mapping", I, I1, I2, I4
                error stop "Invalid atom mapping"
             end if

             np(I, I1, match_atom) = np(I, I1, match_atom) + 1
             K = np(I, I1, match_atom)
             npl(I, I1, match_atom, K) = I2
             nvec(I, I1, match_atom, K, 1:3) = mapped_shift(1:3)
             if (steer(18) .ne. 0) then
                write(*,*) "np=(", I, I1, match_atom, "=", K, " npl=", I2, "nvec=", mapped_shift(1:3)
             end if
          end do
       end do
    end do

  end subroutine sym_sumsets

  subroutine find_unique_atom_mapping(trac, atom_positions, nat_this, ai, tol, match_atom, mapped_shift, match_count)
    real(dp), intent(in) :: trac(3)
    real(dp), intent(in) :: atom_positions(3, nat_this)
    integer, intent(in) :: nat_this
    real(dp), intent(in) :: ai(3,3)
    real(dp), intent(in) :: tol
    integer, intent(out) :: match_atom
    real(dp), intent(out) :: mapped_shift(3)
    integer, intent(out) :: match_count

    integer :: atom_index, component
    real(dp) :: dif(3)
    real(dp) :: candidate_shift(3)
    logical :: is_match

    match_atom = 0
    match_count = 0
    mapped_shift(:) = 0

    do atom_index = 1, nat_this
       dif(:) = trac(:) - atom_positions(:, atom_index)
       candidate_shift(:) = 0
       is_match = .true.

       do component = 1, 3
          candidate_shift(component) = ai(1, component)*dif(1) + &
               & ai(2, component)*dif(2) + ai(3, component)*dif(3)
          if (abs(candidate_shift(component) - nint(candidate_shift(component))) > tol) then
             is_match = .false.
             exit
          end if
       end do

       if (is_match) then
          match_count = match_count + 1
          if (match_count == 1) then
             match_atom = atom_index
             mapped_shift(:) = nint(candidate_shift(:))
          else
             write(*,*) "Ambiguous atom mapping detected"
             write(*,*) " target position: ", trac(:)
             write(*,*) " atom index candidates: ", match_atom, atom_index
             error stop "Ambiguous atom mapping"
          end if
       end if
    end do

    if (match_count == 0) then
       write(*,*) "No atom mapping found"
       write(*,*) " target position: ", trac(:)
       error stop "Missing atom mapping"
    end if
  end subroutine find_unique_atom_mapping
end module sumsets
