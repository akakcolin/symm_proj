! ============================================
! Point Group Data Initialization Module
! ============================================
! Extracted from orbmain.F90 to provide reusable
! point group initialization for all entry points.
!
! Handles:
!   - Rotation angle arrays for Oh (48 ops) and D6h (24 ops)
!   - Multiplication tables MOh, MD6h
!   - Point group statistics (npgo)
!   - Wigner D-matrices (ldrmm) for l=0..3
!   - Rotation matrices (rgr3) in orthogonal coordinates
! ============================================

module sympw_pointgroup_data
  use accuracy
  use constants
  use genera, only: dmatr
  implicit none
  private

  public :: pg_data_t, init_point_group_data, deallocate_point_group_data
  public :: detect_structure_point_group, point_group_number_to_name

  ! Container for all precomputed point group data
  type :: pg_data_t
     real(dp), allocatable :: Oh(:,:)       ! (4, 48) Euler angles for Oh
     real(dp), allocatable :: D6h(:,:)      ! (4, 24) Euler angles for D6h
     integer, allocatable  :: MOh(:,:)      ! (48, 48) Oh multiplication table
     integer, allocatable  :: MD6h(:,:)     ! (24, 24) D6h multiplication table
     integer, allocatable  :: npgo(:,:)     ! (2, 36) point group statistics
     real(dp), allocatable :: rgr3(:,:,:)   ! (3, 3, 72) rotation matrices
     complex(dp), allocatable :: ldrmm(:,:) ! (72, 84) D-matrix elements
  end type pg_data_t

contains

  subroutine init_point_group_data(pg, debug)
    type(pg_data_t), intent(out) :: pg
    integer, intent(in) :: debug

    complex(dp), allocatable :: D(:,:)
    complex(dp), dimension(3, 3, 72) :: rcgr3
    complex(dp), dimension(3,3) :: Q1, Q, res
    real(dp) :: fi, theta, psi, irt2
    integer :: I, J, K, L, N, N2, K1, K2, L1, L2, idx

    allocate(pg%Oh(4, 48))
    allocate(pg%D6h(4, 24))
    allocate(pg%MOh(48, 48))
    allocate(pg%MD6h(24, 24))
    allocate(pg%npgo(2, 36))
    allocate(pg%rgr3(3, 3, 72))
    allocate(pg%ldrmm(72, 84))

    pg%ldrmm(:,:) = cmplx(0, 0, dp)

    ! --- Oh rotation angles ---
    do I = 1, 24
       idx = (I-1)*3
       pg%Oh(4, I) = 0
       pg%Oh(1, I) = Ohdat(idx + 1)*pi
       pg%Oh(2, I) = Ohdat(idx + 2)*pi
       pg%Oh(3, I) = Ohdat(idx + 3)*pi
       pg%Oh(1, I+24) = pg%Oh(1, I)
       pg%Oh(2, I+24) = pg%Oh(2, I)
       pg%Oh(3, I+24) = pg%Oh(3, I)
       pg%Oh(4, I+24) = 1
    end do

    ! --- D6h rotation angles ---
    do I = 1, 12
       idx = (I-1)*3
       pg%D6h(4, I) = 0
       pg%D6h(1, I) = D6hdat(idx + 1)*pi/3
       pg%D6h(2, I) = D6hdat(idx + 2)*pi/3
       pg%D6h(3, I) = D6hdat(idx + 3)*pi/3
       pg%D6h(1, I+12) = pg%D6h(1, I)
       pg%D6h(2, I+12) = pg%D6h(2, I)
       pg%D6h(3, I+12) = pg%D6h(3, I)
       pg%D6h(4, I+12) = 1
    end do

    if (debug == 1) then
       write(*,*) "Rotation Angles for Group Oh"
       do I = 1, 48
          write(*,'(I5,3F15.10)') I, pg%Oh(1:3, I)
       end do
       write(*,*) "Rotation Angles for Group D6h"
       do I = 1, 24
          write(*,'(I5,3F15.10)') I, pg%D6h(1:3, I)
       end do
    end if

    ! --- Oh multiplication table ---
    do I = 1, 24
       idx = (I-1)*24
       do J = 1, 24
          pg%MOh(I, J) = MOhdat(idx + J)
          pg%MOh(I, J+24) = pg%MOh(I, J) + 24
          pg%MOh(I+24, J) = pg%MOh(I, J+24)
          pg%MOh(I+24, J+24) = pg%MOh(I, J)
       end do
    end do

    ! --- D6h multiplication table ---
    do I = 1, 12
       idx = (I-1)*12
       do J = 1, 12
          pg%MD6h(I, J) = MD6hdat(idx + J)
          pg%MD6h(I, J+12) = pg%MD6h(I, J) + 12
          pg%MD6h(I+12, J) = pg%MD6h(I, J+12)
          pg%MD6h(I+12, J+12) = pg%MD6h(I, J)
       end do
    end do

    if (debug == 1) then
       write(*,*) "Group Oh Multiplication Table"
       do I = 1, 24
          write(*,'(48I3)') pg%MOh(:, I)
       end do
       write(*,*) "Group D6h Multiplication Table"
       do I = 1, 12
          write(*,'(24I3)') pg%MD6h(:, I)
       end do
    end if

    ! --- Point group statistics ---
    do I = 1, 36
       pg%npgo(1, I) = npgodat(I)
       pg%npgo(2, I) = npgodat(I+36)
    end do

    if (debug == 1) then
       write(*,*) "Point Group Statistics"
       write(*,'(A,36I4)') "  Order:  ", pg%npgo(1,:)
       write(*,'(A,36I4)') "  Count:  ", pg%npgo(2,:)
       do I = 1, 36
          K = pg%npgo(1, I)
          L = pg%npgo(2, I)
          L2 = L + K - 1
          write(*,'(A,I3,A,I3,A,36I4)') "Group", I, " (order", K, "):", nge(L:L2)
       end do
    end if

    ! --- Wigner D-matrices for Oh (indices 1..48) ---
    do I = 1, 24
       fi = pg%Oh(1, I)
       theta = pg%Oh(2, I)
       psi = pg%Oh(3, I)
       do K = 1, 4
          L = K - 1
          N = 2*L + 1
          K1 = 1
          allocate(D(N, N))
          call dmatr(D, L, fi, theta, psi)
          N2 = (-1)**L
          if (L /= 1) then
             if (L == 2) K1 = 11
             if (L == 3) K1 = 36
          else
             rcgr3(1:3, 1:3, I) = D(1:3, 1:3)
             rcgr3(1:3, 1:3, I+24) = -D(1:3, 1:3)
             K1 = 2
          end if
          do L1 = 1, N
             do L2 = 1, N
                K2 = K1 + (L1 - 1)*N + L2 - 1
                pg%ldrmm(I, K2) = D(L1, L2)
                pg%ldrmm(I+24, K2) = D(L1, L2)*N2
             end do
          end do
          deallocate(D)
       end do
    end do

    ! --- Wigner D-matrices for D6h (indices 49..72) ---
    do I = 1, 12
       fi = pg%D6h(1, I)
       theta = pg%D6h(2, I)
       psi = pg%D6h(3, I)
       do K = 1, 4
          L = K - 1
          N = 2*L + 1
          K1 = 1
          N2 = (-1)**L
          allocate(D(N, N))
          call dmatr(D, L, fi, theta, psi)
          if (L /= 1) then
             if (L == 2) K1 = 11
             if (L == 3) K1 = 36
          else
             rcgr3(1:3, 1:3, I+48) = D(1:3, 1:3)
             rcgr3(1:3, 1:3, I+60) = -D(1:3, 1:3)
             K1 = 2
          end if
          do L1 = 1, N
             do L2 = 1, N
                K2 = K1 + (L1 - 1)*N + L2 - 1
                pg%ldrmm(I+48, K2) = D(L1, L2)
                pg%ldrmm(I+60, K2) = D(L1, L2)*N2
             end do
          end do
          deallocate(D)
       end do
    end do

    ! --- Transform complex rotation matrices to real orthogonal basis ---
    irt2 = 1/sqrt(2.0_dp)

    Q1(1,1) = irt2
    Q1(1,2) = 0
    Q1(1,3) = -1*irt2
    Q1(2,1) = cmplx(0, -irt2, dp)
    Q1(2,2) = 0
    Q1(2,3) = cmplx(0, -irt2, dp)
    Q1(3,1) = 0
    Q1(3,2) = 1
    Q1(3,3) = 0

    Q = transpose(conjg(Q1))

    do I = 1, 72
       res(1:3, 1:3) = matmul(Q1(1:3, 1:3), rcgr3(1:3, 1:3, I))
       pg%rgr3(1:3, 1:3, I) = matmul(res(1:3, 1:3), Q(1:3, 1:3))
    end do

    if (debug == 1) then
       write(*,*) "Rotation/Inversion Matrices"
       do I = 1, 72
          write(*,'(A,I3)') "Matrix ", I
          do K1 = 1, 3
             write(*,'(3F12.6)') real(pg%rgr3(K1,:,I))
          end do
       end do
    end if

  end subroutine init_point_group_data


  integer function detect_structure_point_group(lattice, ai, positions_cart, nel, nat, pg) result(pgnr)
    real(dp), intent(in) :: lattice(3,3)
    real(dp), intent(in) :: ai(3,3)
    real(dp), intent(in) :: positions_cart(:,:,:)
    integer, intent(in) :: nel
    integer, intent(in) :: nat(:)
    type(pg_data_t), intent(in) :: pg

    integer, parameter :: candidates(36) = [36, 35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 25, &
         24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, &
         8, 7, 6, 5, 4, 3, 2, 1]
    integer :: idx

    pgnr = 1
    do idx = 1, size(candidates)
       if (structure_has_point_group(lattice, ai, positions_cart, nel, nat, pg, candidates(idx))) then
          pgnr = candidates(idx)
          return
       end if
    end do
  end function detect_structure_point_group


  character(len=10) function point_group_number_to_name(pgnr) result(name)
    integer, intent(in) :: pgnr

    select case(pgnr)
    case(1);  name = "C1"
    case(2);  name = "Ci"
    case(3);  name = "C2"
    case(4);  name = "Cs"
    case(5);  name = "C2h"
    case(6);  name = "D2"
    case(7);  name = "C2v"
    case(8);  name = "D2h"
    case(9);  name = "C4"
    case(10); name = "S4"
    case(11); name = "C4h"
    case(12); name = "D4"
    case(13); name = "C4v"
    case(14); name = "D2d"
    case(15); name = "D4h"
    case(16); name = "C3"
    case(17); name = "C3i"
    case(18); name = "D3"
    case(20); name = "C3v"
    case(22); name = "D3d"
    case(24); name = "C6"
    case(25); name = "C3h"
    case(26); name = "C6h"
    case(27); name = "D6"
    case(28); name = "C6v"
    case(29); name = "D3h"
    case(31); name = "D6h"
    case(32); name = "T"
    case(33); name = "Th"
    case(34); name = "O"
    case(35); name = "Td"
    case(36); name = "Oh"
    case default
       write(name, '(A,I0)') "PG", pgnr
    end select
  end function point_group_number_to_name


  logical function structure_has_point_group(lattice, ai, positions_cart, nel, nat, pg, pgnr) result(ok)
    real(dp), intent(in) :: lattice(3,3)
    real(dp), intent(in) :: ai(3,3)
    real(dp), intent(in) :: positions_cart(:,:,:)
    integer, intent(in) :: nel
    integer, intent(in) :: nat(:)
    type(pg_data_t), intent(in) :: pg
    integer, intent(in) :: pgnr

    integer :: order, first, ig, op_id, rt_idx
    real(dp) :: rot(3,3)

    ok = .false.
    if (pgnr < 1 .or. pgnr > 36) return

    order = pg%npgo(1, pgnr)
    first = pg%npgo(2, pgnr)

    do ig = 1, order
       op_id = nge2(first + ig - 1)
       rt_idx = rotation_table_index(op_id, pgnr)
       rot(:,:) = pg%rgr3(:,:,rt_idx)

       if (.not. rotation_preserves_lattice(rot, lattice, ai)) return
       if (.not. rotation_maps_basis_with_tau(rot, lattice, ai, positions_cart, nel, nat)) return
    end do

    ok = .true.
  end function structure_has_point_group


  logical function rotation_preserves_lattice(rot, lattice, ai) result(ok)
    real(dp), intent(in) :: rot(3,3)
    real(dp), intent(in) :: lattice(3,3)
    real(dp), intent(in) :: ai(3,3)

    integer :: ivec
    real(dp) :: avec(3), mapped(3), coeff(3)

    ok = .true.
    do ivec = 1, 3
       avec(:) = lattice(ivec, :)
       mapped(:) = matmul(rot, avec)
       coeff(:) = matmul(ai, mapped)
       if (any(abs(coeff - nint(coeff)) > 1.0e-5_dp)) then
          ok = .false.
          return
       end if
    end do
  end function rotation_preserves_lattice


  logical function rotation_maps_basis_with_tau(rot, lattice, ai, positions_cart, nel, nat) result(ok)
    real(dp), intent(in) :: rot(3,3)
    real(dp), intent(in) :: lattice(3,3)
    real(dp), intent(in) :: ai(3,3)
    real(dp), intent(in) :: positions_cart(:,:,:)
    integer, intent(in) :: nel
    integer, intent(in) :: nat(:)

    integer :: ref_atom, target_atom
    real(dp) :: r_ref(3), target(3), tau(3)

    ok = .false.
    if (nel < 1 .or. nat(1) < 1) return

    do ref_atom = 1, nat(1)
       r_ref(:) = positions_cart(:, 1, ref_atom)
       do target_atom = 1, nat(1)
          target(:) = positions_cart(:, 1, target_atom)
          tau(:) = matmul(ai, target - matmul(rot, r_ref))
          tau(:) = tau - floor(tau)
          if (verify_tau_for_basis(tau, rot, lattice, ai, positions_cart, nel, nat)) then
             ok = .true.
             return
          end if
       end do
    end do
  end function rotation_maps_basis_with_tau


  logical function verify_tau_for_basis(tau, rot, lattice, ai, positions_cart, nel, nat) result(ok)
    real(dp), intent(in) :: tau(3)
    real(dp), intent(in) :: rot(3,3)
    real(dp), intent(in) :: lattice(3,3)
    real(dp), intent(in) :: ai(3,3)
    real(dp), intent(in) :: positions_cart(:,:,:)
    integer, intent(in) :: nel
    integer, intent(in) :: nat(:)

    integer :: ichem, iatom, target_atom
    real(dp) :: mapped_cart(3), mapped_frac(3), target_frac(3), diff_frac(3)
    logical :: found

    ok = .true.
    do ichem = 1, nel
       do iatom = 1, nat(ichem)
          mapped_cart(:) = matmul(rot, positions_cart(:, ichem, iatom)) + matmul(transpose(lattice), tau)
          mapped_frac(:) = matmul(ai, mapped_cart)

          found = .false.
          do target_atom = 1, nat(ichem)
             target_frac(:) = matmul(ai, positions_cart(:, ichem, target_atom))
             diff_frac(:) = mapped_frac(:) - target_frac(:)
             if (all(abs(diff_frac - nint(diff_frac)) < 1.0e-5_dp)) then
                found = .true.
                exit
             end if
          end do

          if (.not. found) then
             ok = .false.
             return
          end if
       end do
    end do
  end function verify_tau_for_basis


  subroutine deallocate_point_group_data(pg)
    type(pg_data_t), intent(inout) :: pg
    if (allocated(pg%Oh))     deallocate(pg%Oh)
    if (allocated(pg%D6h))    deallocate(pg%D6h)
    if (allocated(pg%MOh))    deallocate(pg%MOh)
    if (allocated(pg%MD6h))   deallocate(pg%MD6h)
    if (allocated(pg%npgo))   deallocate(pg%npgo)
    if (allocated(pg%rgr3))   deallocate(pg%rgr3)
    if (allocated(pg%ldrmm))  deallocate(pg%ldrmm)
  end subroutine deallocate_point_group_data

end module sympw_pointgroup_data
