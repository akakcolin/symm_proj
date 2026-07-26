!!* Contains a list of constants for the control of precision of the
!!* calculation, both for the fortran numerical model and defaults for the
!!* various algorithms in the code.
!!* @desc Sets the precision of real and complex variables at compile time
!!*   throughout the code.
!!* @note Not all routines use the string length specifications to set
!!*   their character string lengths.
module accuracy
  implicit none

  integer, parameter :: dp = kind(1.0d0) !* precision of the real data type
  integer, parameter :: cp = dp          !* precision of the complex data type
  integer, parameter :: sc = 10          !* length of a short string
  integer, parameter :: mc = 50          !* length of a medium length string
  integer, parameter :: lc = 200         !* length of a long string

  integer, parameter :: rdp = kind(0.0d0) !* Real double precision - don't edit
  integer, parameter :: rsp = kind(0.0)   !* Real single precision - don't edit
  
  !! Program technical constants

  !!* Length of the tail after the SK table at which point elements = 0
  real(dp), parameter :: distFudge = 1.0_dp

  !!* Length of the tail after the SK table for the old extrapolation alg.
  real(dp), parameter :: distFudgeOld = 0.3_dp

  !!* Desired tolerance for number of total electrons when finding Ef
  !!* @desc Fermi level is searched to give the number of electrons 
  !!*   as accurate as elecTol. If bisection ends and difference between
  !!*   nr. of electrons calculated/theoretical is bigger than elecTolMax,
  !!*   the program stops.
  real(dp), parameter :: elecTol = 1.0e-15_dp

  !!* Maximal allowed tolerance for number of total electrons when finding Ef
  !!* or when reading in charges from external file
  real(dp), parameter :: elecTolMax = 1.0e-7_dp

  !!* Minimal temperature, temperatures below that are replaced by this value
  real(dp), parameter :: minTemp = 1.0e-8_dp

  !!* Tolerance for atomic distances. Atoms closer than that are regarded
  !!* to sit on the same positions. (Dummy atoms)
  real(dp), parameter :: tolSameDist = 1.0e-5_dp

  !!* Tolerance for atomic square distances
  real(dp), parameter :: tolSameDist2 = tolSameDist**2

  !!* Minimal distance between neihbors. (Neighbor distances smaller than that
  !!* are meaningless because the parametrisation usually do not cover this
  !!* region.)
  real(dp), parameter :: minNeighDist = 1.0e-2_dp

  !!* Minimal square distance between neighbors
  real(dp), parameter :: minNeighDist2 = minNeighDist**2

  !!* Numerical differentiation distance for 2 point formula
  real(dp), parameter :: deltaXDiff = 0.01_dp
  !real(dp), parameter :: deltaXDiff = 1e-5_dp

  !!* Cut-off value to calculate the short-range part of $\gamma_{ab}$
  real(dp), parameter :: minShortGamma = 1.0e-10_dp

  !!* Cut-off value to calculate the short-range part of Ewald sum
  real(dp), parameter :: minShortEwald = 1.0e-10_dp
  
  !!* Tolerance for error in cut-off of short-range part of $\gamma_{ab}$
  real(dp), parameter :: tolShortGamma = 1.0e-10_dp

  !!* Tolerance for error in cut-off of short-range part of $\gamma_{ab}$
  real(dp), parameter :: tolShortEwald = 1.0e-10_dp

  !!* Minimum value for alpha in Ewald sum
  real(dp), parameter :: tolMinAlpha = 1.0e-4_dp
  
  !!* Tolerance for minimum possible value of an atomic Hubbard U
  real(dp), parameter :: MinHubTol = 1.0e-6_dp 

  !!* Tolerance for minimum possible difference in values of Hubbard U
  real(dp), parameter :: MinHubDiff = 0.3125_dp*1.0e-5_dp

  !!* Nr. of max. bisection steps
  integer, parameter :: nSearchIter = 30 

  !!* Exponential function treated as infinity with arguments higher than this
  !!* (=-int(log(epsilon(1.0_8)))).
  real(dp), parameter :: mExpArg = 36.043653389117154_dp

  !!* Tolerance for the error in the dispersion
  real(dp), parameter :: tolDispersion = 1.0e-9_dp

  !!* Tolerance for the dispersion damping function being considered 1
  real(dp), parameter :: tolDispDamp = 1.0e-10_dp

  !!* Symmetry analysis tolerances
  real(dp), parameter :: tol_zero = 1.0e-10_dp      !* Tolerance for zero check
  real(dp), parameter :: tol_equal = 1.0e-8_dp      !* Tolerance for equality check
  real(dp), parameter :: tol_orthog = 1.0e-6_dp     !* Tolerance for orthogonality
  real(dp), parameter :: tol_phase = 1.0e-6_dp      !* Tolerance for phase factor comparison
  real(dp), parameter :: tol_projection = 1.0e-8_dp !* Tolerance for projection matrix validation
  real(dp), parameter :: tol_projection_work = 100.0_dp * tol_projection
  real(dp), parameter :: tol_lattice_integer = 1.0e-6_dp
  real(dp), parameter :: tol_group_closure = 1.0e-8_dp
  real(dp), parameter :: tol_irrep_phase = 1.0e-6_dp
  real(dp), parameter :: tol_projector_trace = 1.0e-6_dp
  real(dp), parameter :: tol_structure_symmetry = 2.0e-3_dp
  real(dp), parameter :: tol_kpoint_membership = 2.0e-3_dp
  real(dp), parameter :: tol_kpoint_snap = 5.0e-4_dp
  real(dp), parameter :: tol_character_cleanup = 1.0e-4_dp
  real(dp), parameter :: tol_rotation_match = 1.0e-5_dp
  real(dp), parameter :: tol_eigenvalue_cluster = 1.0e-3_dp
  real(dp), parameter :: tol_lattice_metric = 1.0e-3_dp
  real(dp), parameter :: tol_lattice_angle_deg = 0.2_dp
  integer, parameter :: max_projective_phase_order = 12

  integer, parameter :: pg_parent_oh = 0
  integer, parameter :: pg_parent_d6h = 48

contains

  subroutine snap_fractional_kpoint(kpoint_in, kpoint_out)
    real(dp), intent(in) :: kpoint_in(3)
    real(dp), intent(out) :: kpoint_out(3)

    integer, parameter :: max_denominator = 12
    integer :: component, denominator
    real(dp) :: candidate, candidate_error, best_error

    kpoint_out = kpoint_in
    do component = 1, 3
       best_error = tol_kpoint_snap
       do denominator = 1, max_denominator
          candidate = real(nint(kpoint_in(component)*denominator), kind=dp)/ &
               real(denominator, kind=dp)
          candidate_error = abs(kpoint_in(component) - candidate)
          if (candidate_error < best_error) then
             kpoint_out(component) = candidate
             best_error = candidate_error
          end if
       end do
    end do
  end subroutine snap_fractional_kpoint

  integer function point_group_table_offset(point_group_number) result(offset)
    integer, intent(in) :: point_group_number

    if ((point_group_number >= 16) .and. (point_group_number <= 31)) then
       offset = pg_parent_d6h
    else if (point_group_number == 2) then
       ! Ci: uses D6h parent where element 13 is pure inversion
       offset = pg_parent_d6h
    else
       offset = pg_parent_oh
    end if
  end function point_group_table_offset

  integer function rotation_table_index(point_operator_id, point_group_number) result(table_index)
    integer, intent(in) :: point_operator_id
    integer, intent(in) :: point_group_number

    table_index = point_operator_id + point_group_table_offset(point_group_number)
    if ((table_index < 1) .or. (table_index > 72)) then
       write(*,*) "ERROR: Invalid rotation-table index"
       write(*,*) " point_operator_id =", point_operator_id
       write(*,*) " point_group_number =", point_group_number
       write(*,*) " table_index =", table_index
       error stop "Invalid rotation-table index"
    end if
  end function rotation_table_index

end module accuracy
