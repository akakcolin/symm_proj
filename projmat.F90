module projmat
  use accuracy
  use constants
  use sympw_group_mode, only: projective_factor_group_active
  use sympw_phase, only: bloch_phase
  implicit none
  private
  public :: sym_projmat, validate_projector_matrix
  
contains
  ! section 8
  ! the formation of the projection matrices.
  !Here starts the loops necessary to form the projection matrix
  ! section 8.1

  subroutine sym_projmat(laj, kgord, allow, jpdd, projmatrix, nvec, nat, lmax, np, nel, ncl, npl, &
       & kgel, kkgel, listp, steer, ksym, ibz, pgnr, ldrmm, rk, u,tsmall, ttsmall, success, &
       & irrep_column_start, irrep_column_end)
    integer, intent(in) :: kgord
    integer, intent(in) :: allow(:)
    integer, intent(in) :: ncl
    integer, intent(in) :: nel
    integer, intent(in) :: laj(:)
    complex(dp), intent(inout) :: jpdd(:,:,:)
    complex(dp), intent(out) :: projmatrix(:,:)
    
    integer, intent(in) :: steer(:)
    integer, intent(in) :: nat(:)
    integer, intent(in) :: lmax(:)
    integer, intent(in) :: np(:,:,:)
    integer, intent(in) :: npl(:,:,:,:)
    integer, intent(in) :: kgel(:)
    integer, intent(in) :: kkgel(:)
    integer, intent(in) :: listp(:)
    real(dp), intent(in) :: nvec(:,:,:,:,:)
    complex(dp), intent(in) :: ldrmm(:,:)
    integer, intent(in) :: ksym
    integer, intent(in) :: ibz
    integer, intent(in) :: pgnr
    real(dp), intent(in) :: rk(:)
    real(dp), intent(in) :: tsmall, ttsmall
    real(dp), intent(in) :: u(:,:)
    logical, intent(out), optional :: success
    integer, intent(out), optional :: irrep_column_start(:), irrep_column_end(:)
   ! integer, intent(in) :: row_index(:,:)
   ! integer, intent(inout) :: column_index_tmp(:,:)
   ! integer, intent(in) :: nblock
   
    
    integer :: J, J1, J2, JD, K, KJ, K4, K5, KI, LD1, K1, K2
    integer :: N31, NC, ntr, LJ1
    integer :: lsqsum
    
    integer :: ndi, III
    
    integer :: M1, N1, NN1, M2, N2, NN2, N3
    integer :: nb, ichem, L, N, mu1, mu2, atom_idx
    integer :: ncoset, K3
    integer :: I, I1,  ito, itotal
    real(dp) :: R1
    real(dp) :: lattice_shift(3), nonsymmorphic_shift(3)
    real(dp) :: rh, rntr, sumtot

    complex(dp) :: R4, ptrace
    complex(dp) :: ep

    integer, allocatable :: nspec(:)
    real(dp), allocatable :: nrn(:,:)

    complex(dp), allocatable :: jdpk(:,:)
    complex(dp), allocatable :: jdprod(:,:)
    complex(dp), allocatable :: tmatri(:,:)
    complex(dp), dimension(72) :: ldmm
    integer, allocatable :: step_size(:)
    integer :: temp_row, temp_col, j_column_start
    integer :: alloc_stat
    logical :: validation_ok

    if (present(success)) success = .true.
    if (present(irrep_column_start)) then
       if (size(irrep_column_start) < ncl) then
          if (present(success)) success = .false.
          return
       end if
       irrep_column_start(:) = 0
    end if
    if (present(irrep_column_end)) then
       if (size(irrep_column_end) < ncl) then
          if (present(success)) success = .false.
          return
       end if
       irrep_column_end(:) = 0
    end if

    allocate(step_size(nel), stat=alloc_stat)
    if (alloc_stat /= 0) then
       write(*,*) "Projection workspace allocation failed: step_size"
       if (present(success)) success = .false.
       return
    end if
    do ichem = 1, nel
       step_size(ichem) = 0
       do L = 0, lmax(ichem)
          step_size(ichem) = step_size(ichem) + 2*L + 1
       end do
    end do
    temp_col = 1

    J=0
    N31 = -1

    do while( J < ncl)
       J = J + 1
       ! J is the index of the irreducible representation
       if (allow(J) .ne. 0) then
          j_column_start = temp_col

          ! Conjugate all diagonal elements of jpdd first
          ! (conjugation moved before JD loop for clarity)
          do JD = 1, laj(J)
             do I = 1, kgord
                jpdd(J, JD, I) = conjg(jpdd(J, JD, I))
             end do
          end do

          do JD = 1, laj(J)

             !write(*,*) ""
             !write(*,*) "T-matrix for J = ", J, ", JD = ", JD

             nb = 1

             ! ichem is the index of the chemical element
             do ichem = 1, nel

                lsqsum = 0

                do L = 0, lmax(ichem)
                   ! L is the orbital quantum number
                   if (L > 0) then
                      lsqsum = lsqsum + (2*L - 1)**2

                   end if
                   N = 2*L + 1
                   ndi = N * nat(ichem)

                   allocate(jdpk(ndi, ndi), jdprod(ndi, ndi), tmatri(ndi, ndi), stat=alloc_stat)
                   if (alloc_stat /= 0) then
                      write(*,*) "Projection workspace allocation failed: orbital block", ndi
                      if (allocated(jdpk)) deallocate(jdpk)
                      if (allocated(jdprod)) deallocate(jdprod)
                      if (allocated(tmatri)) deallocate(tmatri)
                      deallocate(step_size)
                      if (present(success)) success = .false.
                      return
                   end if
                   do mu1 = 1, nat(ichem)
                      ! mu1 is the atom index, row index in the projection matrix
                      do mu2 = 1, nat(ichem)
                         ! mu2 is the atom index, column index in the projection matrix
                         ncoset = np(ichem, mu1, mu2)
                         ! ncoset is the order of the little coset
                         if (ncoset .ne. 0) then
                            allocate(nrn(ncoset, 3), stat=alloc_stat)
                            if (alloc_stat /= 0) then
                               write(*,*) "Projection workspace allocation failed: atom coset", ncoset
                               deallocate(jdpk, jdprod, tmatri, step_size)
                               if (present(success)) success = .false.
                               return
                            end if
                            nrn(:,:) = 0
                            do I = 1, ncoset
                               do J1 = 1, 3
                                  nrn(I, J1) = nvec(ichem, mu1, mu2, I, J1)
                               end do
                            end do
                         end if
                         ! section 8.2
                         do M1 = -L, L
                            N1 = M1 + L + 1
                            NN1 = N1 + (mu2 -1)*N
                            do M2 = -L, L
                               N2 = M2 + L + 1
                               NN2 = N2 + (mu1 - 1)*N
                               jdpk(NN1, NN2) = cmplx(0.0_dp, 0.0_dp, kind=dp)
                               if (ncoset .ne. 0) then
                                  N3 = lsqsum + (N1 - 1)*N + N2
                                  if (N3 .ne. N31) then
                                     N31 = N3
                                     ldmm(1:72) = ldrmm(1:72, N3)
                                  end if

                                  do I = 1, ncoset
                                     K3 = npl(ichem, mu1, mu2, I)
                                     if (projective_factor_group_active(steer(20), ksym, ibz)) then
                                        K = kkgel(listp(K3))
                                        nonsymmorphic_shift(:) = 0.0_dp
                                     else
                                        K = kkgel(K3)
                                        KI = kgel(K3)
                                        nonsymmorphic_shift(:) = u(KI, 1:3)
                                     end if
                                     K = rotation_table_index(K, pgnr)
                                     lattice_shift(:) = nrn(I, 1:3)
                                     ep = bloch_phase(rk(1:3), lattice_shift, &
                                          nonsymmorphic_shift)
                                     ! section 8.3
                                     R4 = jpdd(J, JD, K3) * ep * ldmm(K)
                                     jdpk(NN1, NN2) = jdpk(NN1, NN2) + R4
                                  end do
                               end if
                            end do
                         end do
                         if (ncoset .ne. 0) then
                            deallocate(nrn)
                         end if
                      end do
                   end do

                   ndi = N*nat(ichem)
                   rh = real(laj(J))/ real(kgord)
                   jdpk(1:ndi, 1:ndi) = jdpk(1:ndi, 1:ndi) * rh

                   ! Force Hermiticity: the projection operator is Hermitian by
                   ! group-theoretic construction (P = P^dagger). Validate the
                   ! raw group sum before applying a roundoff cleanup.
                   call validate_raw_projection(jdpk(1:ndi, 1:ndi), ndi, &
                        tol_projection_work, J, JD, ichem, L, validation_ok)
                   if (.not. validation_ok) then
                      deallocate(jdpk, jdprod, tmatri, step_size)
                      if (present(success)) then
                         success = .false.
                         return
                      end if
                      error stop "Raw group projection validation failed"
                   end if
                   jdpk(1:ndi, 1:ndi) = 0.5_dp * (jdpk(1:ndi, 1:ndi) + &
                        transpose(conjg(jdpk(1:ndi, 1:ndi))))

                   ! Check trace: if the irrep is not contained in this orbital
                   ! space, jdpk is numerically zero — skip extraction.
                   ptrace = cmplx(0.0_dp, 0.0_dp, kind=dp)
                   do III = 1, ndi
                      ptrace = ptrace + jdpk(III,III)
                   end do
                   rntr = real(ptrace)
                   ntr = nint(rntr)
                   if (ntr == 0) then
                      ! Irrep not contained in this L-subspace; skip.
                      deallocate(jdpk)
                      deallocate(jdprod)
                      deallocate(tmatri)
                      cycle
                   end if
                   ! section 8.5
                   ! we orthormalise the submatrix (fixed L, ichem) and store the resulting
                   ! sub-T-matrix
                   if (ntr .ne. 0) then
                      ! ntr is the trace, the number of linearly independent columns in the submatrix.
                      ! First we select columns with the diagonal term equal to 1, since these are
                      ! automatically orthonormal to all other columns. Skip all columns with diagonal
                      ! term equal to 0, since these are zero columns.
                      !
                      allocate(nspec(ndi), stat=alloc_stat)
                      if (alloc_stat /= 0) then
                         write(*,*) "Projection workspace allocation failed: special columns", ndi
                         deallocate(jdpk, jdprod, tmatri, step_size)
                         if (present(success)) success = .false.
                         return
                      end if
                      nspec(:) = 0
                      ! nspec(I) registrates these special columns.
                      itotal = 0
                      do I = 1, ndi
                         if ((abs(jdpk(I, I) -1))**2 <= ttsmall) then
                            itotal = itotal + 1
                            do K2 = 1, ndi
                               tmatri(K2, itotal) = 0
                            end do
                            tmatri(I, itotal) = 1
                            nspec(I) = 1
                         else
                            if  ((abs(jdpk(I,I)))**2 <= ttsmall) then
                               nspec(I) = -1
                            end if
                         end if
                      end do

                      I1 = itotal + 1
                      I = 1
                      if (itotal .ne. ntr) then
                         do while (I <= ndi)
                            if (nspec(I) .eq. 0) then
                               ito = itotal
                               itotal = itotal + 1
                               sumtot = sum(abs(jdpk(1:ndi, I))**2)
                               if (sumtot >= ttsmall) then
                                  sumtot = 1/sqrt(sumtot)
                                  tmatri(1:ndi, itotal) = jdpk(1:ndi, I)*sumtot
                                  ! Modified Gram-Schmidt against ALL previous columns
                                  ! (including unit-diagonal ones, which were at NC=1..ito before
                                  !  Gram-Schmidt columns were added)
                                  if (ito >= 1) then
                                     NC = 1
                                     do while (NC <= ito)
                                        R4 = dot_product(tmatri(1:ndi, NC), tmatri(1:ndi, itotal))
                                        if (abs(R4) > tol_zero) then
                                           tmatri(1:ndi, itotal) = tmatri(1:ndi, itotal) - R4*tmatri(1:ndi, NC)
                                           R1 = sum(abs(tmatri(1:ndi, itotal))**2)
                                           if (R1 < ttsmall) then
                                              itotal = itotal - 1
                                              exit
                                           end if
                                        end if
                                        NC = NC + 1
                                     end do
                                     ! Renormalize after MGS (only needed when MGS ran)
                                     if (NC > ito) then
                                        R1 = sum(abs(tmatri(1:ndi, itotal))**2)
                                        if (R1 >= ttsmall) then
                                           R1 = 1/sqrt(R1)
                                           tmatri(1:ndi, itotal) = tmatri(1:ndi, itotal) * R1
                                        else
                                           itotal = itotal - 1
                                        end if
                                     end if
                                  end if
                                  ! When ito == 0: column is already normalized by sumtot,
                                  ! no MGS or renormalization needed.
                                  if (itotal .eq. ntr) then
                                     exit
                                  end if
                               end if
                            end if
                            I = I + 1
                         end do

                        if (itotal .ne. ntr) then
                            write(*,*) "Error, not enough orthonormal columns, itotal, ntr", itotal, ntr
                            deallocate(nspec)
                            deallocate(jdpk)
                            deallocate(jdprod)
                            deallocate(tmatri)
                            if (present(success)) success = .false.
                            return
                         end if
                      end if
                      if (allocated(nspec)) deallocate(nspec)
                   else
                      itotal = 0
                   end if
                   ! section 8.6
                   ! here starts the output section. The projection matrices (T- matrices) are printed.
                   ! Thereafter all of them have been calculated for all values of J, JD, ichem, L

                   if (itotal .ne. 0) then
                      call validate_projection_block(tmatri(1:ndi, 1:itotal), ndi, &
                           tol_projection, J, JD, ichem, L, validation_ok)
                      if (.not. validation_ok) then
                         deallocate(jdpk, jdprod, tmatri, step_size)
                         if (present(success)) then
                            success = .false.
                            return
                         end if
                         error stop "Projection block validation failed"
                      end if

                      do I = 1, itotal
                         do atom_idx = 1, nat(ichem)
                            temp_row = projection_block_row_index(ichem, atom_idx, L, nat, step_size)
                            projmatrix(temp_row:temp_row+N-1, temp_col) = &
                                 & tmatri((atom_idx-1)*N+1:(atom_idx-1)*N+N, I)
                         end do
                         temp_col = temp_col + 1
                      end do

                      !write(*,*)"Subblock ", nb, " sub-T-matrix for ichem = ", ichem
                      !write(*,*) ", L = ", L, " , ", nat(ichem), " atoms per numit cell"
                      !write(*,*) "columns", itotal, ndi, " rows, the rows are indexed as, "
                      !write(*,*) "(atom 1 M = -L,.,L) (atom 2 M = -L,.,L)"

                      !do I = 1, itotal
                         !write(*,*) "Column ", I
                         !write(*,*) tmatriorg(1:2, 1:ndi, I)
                         !write(*,*) ""
                         !write(*,*)"I L+1, ichem, J, col_index, row_index", I, L+1, &
                         !     & ichem, J, col_index(L+1, ichem, j), row_index(L+1, ichem, j)
                         !write(*,*) hugematrix(1:ndi ,I, L+1, ichem, j)
                      !end do
                      !write(*,*) ""
                      nb = nb + 1
                   else
                      !write(*,*) "Subblock ", nb, " sub-T-matrix for ichem = ", ichem
                      !write(*,*) ", L = ", L, " , ", nat(ichem), " atoms per cell is empty"
                      nb = nb + 1
                   end if
                   deallocate(jdpk)
                   deallocate(jdprod)
                   deallocate(tmatri)
                end do
             end do

          end do
          if (temp_col > j_column_start) then
             if (present(irrep_column_start)) irrep_column_start(J) = j_column_start
             if (present(irrep_column_end)) irrep_column_end(J) = temp_col - 1
          end if
       end if
    end do

    deallocate(step_size)

  end subroutine sym_projmat

  integer function projection_block_row_index(ichem, atom_idx, L, nat, step_size) result(row_index)
    integer, intent(in) :: ichem
    integer, intent(in) :: atom_idx
    integer, intent(in) :: L
    integer, intent(in) :: nat(:)
    integer, intent(in) :: step_size(:)

    integer :: I1
    integer :: base_row

    base_row = 0
    do I1 = 1, ichem - 1
       base_row = base_row + nat(I1)*step_size(I1)
    end do

    row_index = base_row + (atom_idx - 1)*step_size(ichem) + L*L + 1
  end function projection_block_row_index

  subroutine validate_projector_matrix(projector, tol, valid, max_residual)
    complex(dp), intent(in) :: projector(:,:)
    real(dp), intent(in) :: tol
    logical, intent(out) :: valid
    real(dp), intent(out), optional :: max_residual

    complex(dp), allocatable :: squared(:,:), delta(:,:)
    complex(dp) :: trace_value
    real(dp) :: idempotency_residual, hermiticity_residual, trace_residual
    integer :: n, row_idx, alloc_stat

    valid = .false.
    if (present(max_residual)) max_residual = huge(1.0_dp)
    if (tol < 0.0_dp .or. size(projector, 1) /= size(projector, 2)) return

    n = size(projector, 1)
    if (n < 1) return
    allocate(squared(n, n), delta(n, n), stat=alloc_stat)
    if (alloc_stat /= 0) return

    squared = matmul(projector, projector)
    delta = squared - projector
    idempotency_residual = maxval(abs(delta))

    delta = projector - transpose(conjg(projector))
    hermiticity_residual = maxval(abs(delta))

    trace_value = cmplx(0.0_dp, 0.0_dp, kind=dp)
    do row_idx = 1, n
       trace_value = trace_value + projector(row_idx, row_idx)
    end do
    trace_residual = max(abs(aimag(trace_value)), &
         abs(real(trace_value) - nint(real(trace_value))))

    if (present(max_residual)) then
       max_residual = max(idempotency_residual, hermiticity_residual, trace_residual)
    end if
    valid = idempotency_residual <= tol .and. hermiticity_residual <= tol .and. &
         abs(aimag(trace_value)) <= tol .and. &
         abs(real(trace_value) - nint(real(trace_value))) <= tol_projector_trace

    deallocate(squared, delta)
  end subroutine validate_projector_matrix

  subroutine validate_raw_projection(raw_projector, n, tol, J, JD, ichem, L, valid)
    complex(dp), intent(in) :: raw_projector(:,:)
    integer, intent(in) :: n
    real(dp), intent(in) :: tol
    integer, intent(in) :: J
    integer, intent(in) :: JD
    integer, intent(in) :: ichem
    integer, intent(in) :: L
    logical, intent(out) :: valid

    real(dp) :: max_residual

    valid = .false.
    if (size(raw_projector, 1) /= n .or. size(raw_projector, 2) /= n) then
       write(*,*) "Raw projection block has inconsistent dimensions for J =", J, &
            ", JD =", JD, ", ichem =", ichem, ", L =", L
       return
    end if

    call validate_projector_matrix(raw_projector, tol, valid, max_residual)
    if (.not. valid) then
       write(*,*) "Raw projection validation failed for J =", J, &
            ", JD =", JD, ", ichem =", ichem, ", L =", L
       write(*,*) "Maximum residual =", max_residual
    end if
  end subroutine validate_raw_projection


  subroutine validate_projection_block(pblock, n, tol, J, JD, ichem, L, valid)
    complex(dp), intent(in) :: pblock(:,:)
    integer, intent(in) :: n
    real(dp), intent(in) :: tol
    integer, intent(in) :: J
    integer, intent(in) :: JD
    integer, intent(in) :: ichem
    integer, intent(in) :: L
    logical, intent(out) :: valid

    complex(dp), allocatable :: projector(:,:)
    complex(dp), allocatable :: gram(:,:)
    real(dp) :: max_residual
    integer :: m, alloc_stat

    valid = .false.
    m = size(pblock, 2)
    if (size(pblock, 1) /= n) then
       write(*,*) "Projection block has inconsistent row count for J =", J, &
            & ", JD =", JD, ", ichem =", ichem, ", L =", L
       write(*,*) "Expected", n, " rows, got", size(pblock, 1)
       return
    end if

    ! Check column orthonormality: pblock^H * pblock ≈ I_m
    allocate(gram(m, m), stat=alloc_stat)
    if (alloc_stat /= 0) then
       write(*,*) "Projection block Gram allocation failed"
       return
    end if
    gram = matmul(transpose(conjg(pblock)), pblock)
    gram = gram - identity_matrix(m)
    max_residual = maxval(abs(gram))
    deallocate(gram)
    if (max_residual > tol) then
       write(*,*) "Projection block column orthonormality failed for J =", J, &
            & ", JD =", JD, ", ichem =", ichem, ", L =", L
       write(*,*) "Maximum residual =", max_residual
       return
    end if

    ! Build the full projector: P = pblock * pblock^H
    allocate(projector(n, n), stat=alloc_stat)
    if (alloc_stat /= 0) then
       write(*,*) "Projection block validation allocation failed"
       return
    end if
    projector = matmul(pblock, transpose(conjg(pblock)))
    call validate_projector_matrix(projector, tol, valid, max_residual)
    if (.not. valid) then
       write(*,*) "Projection block validation failed for J =", J, &
            & ", JD =", JD, ", ichem =", ichem, ", L =", L
       write(*,*) "Maximum residual =", max_residual
    end if

    deallocate(projector)
  end subroutine validate_projection_block

  function identity_matrix(n) result(identity)
    integer, intent(in) :: n
    complex(dp) :: identity(n,n)
    integer :: index

    identity = cmplx(0.0_dp, 0.0_dp, dp)
    do index = 1, n
       identity(index, index) = cmplx(1.0_dp, 0.0_dp, dp)
    end do
  end function identity_matrix
end module projmat
