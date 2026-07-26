module sympw_cli_runner
  use accuracy, only: dp, tol_equal, tol_projection
  use sympw_lib
  use time_reversal, only: find_trim_points, build_time_reversal_pairs
  use time_reversal_optimization, only: mark_kpoints_to_compute, &
       print_tr_optimization_summary, build_spinless_partner_projector, &
       verify_spinless_projector_pair
  implicit none
  private

  public :: run_sympw_kpoint_batch

contains

  subroutine run_sympw_kpoint_batch(kpoints, kpoint_names, matrix_order, &
       print_projectors, success)
    real(dp), intent(in) :: kpoints(:,:)
    character(len=*), intent(in) :: kpoint_names(:)
    integer, intent(in) :: matrix_order
    logical, intent(in) :: print_projectors
    logical, intent(out) :: success

    type(sympw_result_t) :: result
    integer, allocatable :: trim_indices(:), tr_pairs(:), source_kpoint(:)
    logical, allocatable :: should_compute(:)
    complex(dp), allocatable :: projectors(:,:,:), copied_projector(:,:)
    real(dp) :: current_kpoint(3), tr_tolerance, tr_residual, max_tr_difference
    integer :: nkpts, n_trim, n_computed, n_skipped, kpoint_index, partner_index
    integer :: n_tr_copied, n_tr_fallback, n_tr_verified, n_tr_failed, alloc_stat
    logical :: tr_copy_ok, tr_projector_ok
    logical, parameter :: spinless_tr_optimization_enabled = .true.

    success = .false.
    nkpts = size(kpoints, 1)
    if (nkpts < 1 .or. size(kpoints, 2) /= 3 .or. &
         size(kpoint_names) /= nkpts .or. matrix_order < 1) return

    allocate(projectors(matrix_order, matrix_order, nkpts), stat=alloc_stat)
    if (alloc_stat /= 0) return
    projectors(:, :, :) = cmplx(0.0_dp, 0.0_dp, dp)

    call find_trim_points(kpoints, nkpts, trim_indices, n_trim, tol_equal)
    allocate(tr_pairs(nkpts), should_compute(nkpts), source_kpoint(nkpts), stat=alloc_stat)
    if (alloc_stat /= 0) return
    call build_time_reversal_pairs(kpoints, nkpts, tr_pairs, tol_equal)
    call mark_kpoints_to_compute(tr_pairs, nkpts, should_compute, source_kpoint, &
         n_computed, n_skipped, n_trim)
    call print_tr_optimization_summary(nkpts, n_computed, n_skipped, n_trim, &
         should_compute, source_kpoint, tr_pairs, spinless_tr_optimization_enabled)

    tr_tolerance = max(100.0_dp*tol_projection, 1.0e-8_dp)
    n_tr_copied = 0
    n_tr_fallback = 0
    do kpoint_index = 1, nkpts
       if (spinless_tr_optimization_enabled .and. .not. should_compute(kpoint_index)) then
          partner_index = source_kpoint(kpoint_index)
          tr_residual = huge(1.0_dp)
          if (partner_index >= 1 .and. partner_index < kpoint_index) then
             call build_spinless_partner_projector(projectors(:, :, partner_index), &
                  tr_tolerance, copied_projector, tr_copy_ok, tr_residual)
             if (tr_copy_ok) then
                projectors(:, :, kpoint_index) = copied_projector(:, :)
                deallocate(copied_projector)
                n_tr_copied = n_tr_copied + 1
                write(*,'(A,I0,A,I0)') " K-point ", kpoint_index, &
                     " copied from spinless TR partner ", partner_index
                if (print_projectors .and. matrix_order <= 60) then
                   call print_projector_matrix(kpoint_index, kpoint_names(kpoint_index), &
                        projectors(:, :, kpoint_index))
                end if
                cycle
             end if
          end if
          n_tr_fallback = n_tr_fallback + 1
          write(*,'(A,I0,A,ES12.4)') " TR copy failed for k-point ", kpoint_index, &
               "; computing explicitly, residual = ", tr_residual
       end if

       current_kpoint(:) = kpoints(kpoint_index, :)
       call sympw_analyze_kpoint(current_kpoint, result)
       if (.not. result%success .or. result%matrix_order /= matrix_order .or. &
            .not. allocated(result%projector_real)) then
          write(*,*) "K-point analysis failed at index", kpoint_index
          return
       end if
       projectors(:, :, kpoint_index) = result%projector_real(:, :)
       call print_kpoint_irreps(kpoint_index, kpoint_names(kpoint_index), result)
       if (print_projectors .and. matrix_order <= 60) then
          call print_projector_matrix(kpoint_index, kpoint_names(kpoint_index), &
               projectors(:, :, kpoint_index))
       end if
    end do

    n_tr_verified = 0
    n_tr_failed = 0
    do kpoint_index = 1, nkpts
       partner_index = tr_pairs(kpoint_index)
       if (partner_index <= kpoint_index) cycle
       call verify_spinless_projector_pair(projectors(:, :, kpoint_index), &
            projectors(:, :, partner_index), tr_tolerance, tr_projector_ok, &
            max_tr_difference)
       if (tr_projector_ok) then
          n_tr_verified = n_tr_verified + 1
       else
          n_tr_failed = n_tr_failed + 1
          write(*,'(A,I0,A,I0,A,ES12.4)') " Spinless TR check failed for k", &
               kpoint_index, " and k", partner_index, "; residual = ", max_tr_difference
       end if
    end do

    write(*,*)
    write(*,*) "=========================================="
    write(*,*) "Symmetry Projection Complete"
    write(*,*) "=========================================="
    write(*,'(A,I0,A,I0)') " Explicitly analyzed k-points: ", &
         nkpts - n_tr_copied, "; TR copies: ", n_tr_copied
    write(*,'(A,I0)') " TR copy fallbacks: ", n_tr_fallback
    write(*,'(A,I0,A,I0)') " Spinless TR checks passed: ", n_tr_verified, &
         "; failed: ", n_tr_failed
    success = n_tr_failed == 0
  end subroutine run_sympw_kpoint_batch


  subroutine print_kpoint_irreps(index_value, name, analysis)
    integer, intent(in) :: index_value
    character(len=*), intent(in) :: name
    type(sympw_result_t), intent(in) :: analysis
    integer :: irrep_position

    write(*,*)
    write(*,'(A,I0,2A,3F10.6)') " K-point ", index_value, " ", trim(name), &
         analysis%kpoint_internal
    write(*,'(A,I0,A,I0)') " Little-group order: ", analysis%little_group_order, &
         "; represented-group order: ", analysis%factor_group_order
    select case(analysis%mulliken_status)
    case(SYMPW_MULLIKEN_STATUS_AVAILABLE)
       write(*,*) " Irreducible representations:"
       do irrep_position = 1, size(analysis%irreps)
          write(*,'(2X,A,2(A,I0))') trim(analysis%irreps(irrep_position)%mulliken_label), &
               "  dimension=", analysis%irreps(irrep_position)%dimension, &
               "  multiplicity=", analysis%irreps(irrep_position)%multiplicity
       end do
    case(SYMPW_MULLIKEN_STATUS_COMPLEX_PAIR)
       if (analysis%real_irrep_view_available .and. allocated(analysis%real_irreps)) then
          write(*,*) " Conventional real representations:"
          do irrep_position = 1, size(analysis%real_irreps)
             write(*,'(2X,A,2(A,I0))') trim(analysis%real_irreps(irrep_position)%label), &
                  "  dimension=", analysis%real_irreps(irrep_position)%dimension, &
                  "  multiplicity=", analysis%real_irreps(irrep_position)%multiplicity
          end do
       end if
    case default
       write(*,*) " Irreducible representations use stable character fingerprints:"
       do irrep_position = 1, size(analysis%irreps)
          write(*,'(2X,A,2(A,I0))') trim(analysis%irreps(irrep_position)%label), &
               "  dimension=", analysis%irreps(irrep_position)%dimension, &
               "  multiplicity=", analysis%irreps(irrep_position)%multiplicity
       end do
    end select
  end subroutine print_kpoint_irreps


  subroutine print_projector_matrix(index_value, name, projector)
    integer, intent(in) :: index_value
    character(len=*), intent(in) :: name
    complex(dp), intent(in) :: projector(:,:)
    integer :: row_index, column_index, column_start, matrix_order

    matrix_order = size(projector, 1)
    if (size(projector, 2) /= matrix_order) return
    write(*,*) "Projection Matrix Result"
    write(*,'(A,I0,2A)') " K-point ", index_value, ": ", trim(name)
    write(*,'(A,I0,A,I0)') " Matrix dimension: ", matrix_order, " x ", matrix_order
    do column_start = 1, matrix_order, 6
       write(*,'(A6)', advance='no') "Row"
       do column_index = column_start, min(column_start + 5, matrix_order)
          write(*,'(A16,I4)', advance='no') "Col ", column_index
       end do
       write(*,*)
       do row_index = 1, matrix_order
          write(*,'(I6)', advance='no') row_index
          do column_index = column_start, min(column_start + 5, matrix_order)
             write(*,'(A1,F8.4,A1,F8.4,A1)', advance='no') &
                  "(", real(projector(row_index, column_index), dp), ",", &
                  aimag(projector(row_index, column_index)), ")"
          end do
          write(*,*)
       end do
       write(*,*)
    end do
  end subroutine print_projector_matrix

end module sympw_cli_runner
