module projmat
  use accuracy
  use constants
  implicit none
  private
  public :: sym_projmat
  
contains
  ! section 8
  ! the formation of the projection matrices.
  !Here starts the loops necessary to form the projection matrix
  ! section 8.1

  subroutine sym_projmat(laj, kgord, allow, jpdd, projmatrix, nvec, nat, lmax, np, nel, ncl, npl, &
       & kgel, kkgel, listp, steer, ksym, ibz, pgnr, ldrmm, rk, u,tsmall, ttsmall)
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
   ! integer, intent(in) :: row_index(:,:)
   ! integer, intent(inout) :: column_index_tmp(:,:)
   ! integer, intent(in) :: nblock
   
    
    integer :: J, J1, J2, JD, K, KJ, K4, K5, KI, LD1, K1
    integer :: N31, NC, ntr, LJ1
    integer :: lsqsum
    
    integer :: ndi, III
    
    integer :: M1, N1, NN1, M2, N2, NN2, N3
    integer :: nb, ichem, L, N, mu1, mu2, atom_idx
    integer :: ncoset, K3
    integer :: I, I1,  ito, itotal
    real(dp) :: R1
    real(dp) :: tmp_R
    real(dp) :: rh, rntr, sumtot, ptrace 

    complex(dp) ::R4, R5
    complex(dp) :: ep

    integer, allocatable :: nspec(:) 
    real(dp), allocatable :: nrn(:,:) 
    real(dp), allocatable :: jpddorg(:,:,:)
    real(dp), allocatable :: jdpkorg(:,:,:)
    real(dp), allocatable :: tmatriorg(:,:,:)
 
    complex(dp), allocatable :: jdpk(:,:)
    complex(dp), allocatable :: jdprod(:,:)
    complex(dp), allocatable :: tmatri(:,:)
    complex(dp), dimension(72) :: ldmm
    integer, allocatable :: step_size(:)
    integer :: temp_row, temp_col
    integer :: lorb, offset_l, nrow_block
    integer :: alloc_stat

    allocate(step_size(nel))
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

          do JD = 1, laj(J)

             !write(*,*) ""
             !write(*,*) "T-matrix for J = ", J, ", JD = ", JD

             nb = 1
             ! jd is the index of the diagonal elements of the irreducible representation J
             do I = 1, kgord
                jpdd(J, JD, I) =conjg(jpdd(J, JD, I))
             end do

             if (steer(18) .ne. 0) then
                do I = 1, kgord
                   jpddorg(1, JD, I) = real(jpdd(J, JD, I))
                   jpddorg(2, JD, I) = aimag(jpdd(J, JD, I))
                end do
                !write(*,*) " "
             end if

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

                   allocate(jdpk(ndi, ndi))
                   allocate(jdprod(ndi, ndi))
                   allocate(tmatri(ndi, ndi))
                   allocate(tmatriorg(2, ndi, ndi))
                   allocate(jdpkorg(2, ndi, ndi))
                   do mu1 = 1, nat(ichem)
                      ! mu1 is the atom index, row index in the projection matrix
                      do mu2 = 1, nat(ichem)
                         ! mu2 is the atom index, column index in the projection matrix
                         ncoset = np(ichem, mu1, mu2)
                         ! ncoset is the order of the little coset
                         if (ncoset .ne. 0) then
                            allocate(nrn(ncoset, 3))
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
                               jdpk(NN1, NN2) = cmplx(0,0)
                               if (ncoset .ne. 0) then
                                  N3 = lsqsum + (N1 - 1)*N + N2
                                  if (N3 .ne. N31) then
                                     N31 = N3
                                     ldmm(1:72) = ldrmm(1:72, N3)
                                     if (steer(18) .ne. 0) then
                                        do I = 1, 72
                                           write(*,*) ldmm(I)
                                        end do
                                     end if
                                  end if

                                  do I = 1, ncoset
                                     K3 = npl(ichem, mu1, mu2, I)
                                     if (.not. ((steer(20) .ne. 0) .or. (ksym .ne. 0) .or. (ibz .ne. 0))) then
                                        K = kkgel(listp(K3))
                                        R5 = cmplx(0, 0)
                                     else
                                        K = kkgel(K3)
                                        KI = kgel(K3)
                                        tmp_R = u(KI, 1)*rk(1) + u(KI, 2)*rk(2) + u(KI,3)*rk(3)
                                        R5 = cmplx(0, tmp_R)
                                     end if
                                     K = rotation_table_index(K, pgnr)
                                     tmp_R = nrn(I,1)*rk(1) + nrn(I,2)*rk(2) + nrn(I,3)*rk(3)
                                     R4 = cmplx(0, -tmp_R) + R5
                                     ep = exp(R4)
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
                   ! section 8.4
                   ! test projection matrix: must satisfy P^2 = P (idempotent) and P = P^dagger (Hermitian)

                   ! Test 1: Check idempotency P^2 = P
                   jdprod(1:ndi, 1:ndi) = matmul(jdpk(:,:) , jdpk(:, :))
                   tmatri(1:ndi, 1:ndi) = jdprod(1:ndi, 1:ndi) - jdpk(1:ndi, 1:ndi)

                   K4 = 1
                   do while (K4 <= ndi)
                      K5 = 1
                      do while (K5 <= ndi)
                         if (abs(tmatri(K4, K5)) > tsmall) then
                            exit
                         end if
                         K5 = K5 + 1
                      end do

                      if (K5 <= ndi) then
                         exit
                      end if
                      K4 = K4 + 1
                   end do
                   ! error, no projection matrix for J (failed idempotency test)
                   if (K4 <= ndi) then
                      write(*,*) "Error: P^2 != P, not a projection matrix for J = ", J
                      write(*,*) ", JD= ", JD, ", laj= ", laj(J), ", ichem = ", ichem, "L = ", L, " ndi= ", ndi

                      jdpkorg(1, 1:ndi, 1:ndi) = real(jdpk(1:ndi,1:ndi))
                      jdpkorg(2, 1:ndi, 1:ndi) = aimag(jdpk(1:ndi,1:ndi))
                      tmatriorg(1, 1:ndi, 1:ndi) = real(tmatri(1:ndi, 1:ndi))
                      tmatriorg(2, 1:ndi, 1:ndi) = aimag(tmatri(1:ndi, 1:ndi))
                   end if

                   ! Test 2: Check Hermiticity P = P^dagger
                   tmatri(1:ndi, 1:ndi) = jdpk(1:ndi, 1:ndi)  - transpose((conjg(jdpk(1:ndi, 1:ndi))))
                   K4 = 1
                   do while (K4 <= ndi)
                      K5 = 1
                      do while (K5 <= ndi)
                         if (abs(tmatri(K4, K5)) > tsmall) then
                            exit
                         end if
                         K5 = K5 + 1
                      end do
                      if (K5 <= ndi) then
                         exit
                      end if
                      K4 = K4 + 1
                   end do
                   if (K4 <= ndi) then
                      write(*,*) "Error: P != P^dagger, projection matrix not Hermitian"
                      write(*,*) "Error, no projection matrix for J =", J, "JD =", JD, "laj = ", laj(J)
                      write(*,*) ""
                      tmatriorg(1, 1:ndi, 1:ndi) = real(tmatri(1:ndi, 1:ndi))
                      tmatriorg(2, 1:ndi, 1:ndi) = aimag(tmatri(1:ndi, 1:ndi))
                      write(*,*) ""
                   end if
                   ! section 8.5
                   ! we orthormalise the submatrix (fixed L, ichem) and store the resulting
                   ! sub-T-matrix
                   !ptrace = sum(diag(jdpk(1: ndi, 1:ndi)))
                   ptrace = cmplx(0,0)
                   do III = 1, ndi
                      ptrace = ptrace + jdpk(III,III)
                   end do

                   rntr = real(ptrace)

                   if(rntr > 0) then
                      ntr = floor(rntr)
                   else
                      ntr = ceiling(rntr)
                   end if
                   if ((rntr - ntr) > 0.5) then
                      ntr = ntr + 1
                   end if
                   if (ntr .ne. 0) then
                      ! ntr is the trace, the number of linearly independent columns in the submatrix.
                      ! First we select columns with the diagonal term equal to 1, since these are
                      ! automatically orthonormal to all other columns. Skip all columns with diagonal
                      ! term equal to 0, since these are zero columns.
                      !
                      allocate(nspec(ndi))
                      nspec(:) = 0
                      ! nspec(I) registrates these special columns.
                      itotal = 0
                      do I = 1, ndi
                         if ((abs(jdpk(I, I) -1))**2 <= ttsmall) then
                            itotal = itotal + 1
                            do N = 1, ndi
                               tmatri(N, itotal) = 0
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
                                  if (itotal .ne. I1) then
                                     NC = I1
                                     ! Modified Gram-Schmidt orthogonalization (more numerically stable)
                                     do while (NC <= ito)
                                        ! orthonormalisation against previously found orthonormal columns
                                        R4 = dot_product(conjg(tmatri(1:ndi, NC)), tmatri(1:ndi, itotal))
                                        if (abs(R4) > 1.0e-10_dp) then
                                           ! Project out component along NC-th vector
                                           tmatri(1:ndi, itotal) = tmatri(1:ndi, itotal) - R4*tmatri(1:ndi, NC)
                                           ! Check if vector became too small (numerically dependent)
                                           R1 = sum(abs(tmatri(1:ndi, itotal))**2)
                                           if (R1 < ttsmall) then
                                              ! Vector is linearly dependent, discard it
                                              itotal = itotal - 1
                                              exit
                                           end if
                                        end if
                                        NC = NC + 1
                                     end do
                                     ! Renormalize after all projections (Modified Gram-Schmidt)
                                     if (NC > ito) then
                                        R1 = sum(abs(tmatri(1:ndi, itotal))**2)
                                        if (R1 >= ttsmall) then
                                           R1 = 1/sqrt(R1)
                                           tmatri(1:ndi, itotal) = tmatri(1:ndi, itotal) * R1
                                        else
                                           itotal = itotal - 1
                                        end if
                                        if (itotal .eq. ntr) then
                                           exit
                                        end if
                                     end if
                                  else
                                     if (itotal .eq. ntr) then
                                        exit
                                     end if
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
                            deallocate(tmatriorg)
                            deallocate(jdpkorg)
                            exit
                         end if
                      end if
                         deallocate(nspec)
                   else
                      itotal = 0
                   end if
                   ! section 8.6
                   ! here starts the output section. The projection matrices (T- matrices) are printed.
                   ! Thereafter all of them have been calculated for all values of J, JD, ichem, L

                   if (itotal .ne. 0) then
                      call validate_projection_block(tmatri(1:ndi, 1:itotal), ndi, tol_projection, J, JD, ichem, L)

                      do I = 1, itotal
                         do atom_idx = 1, nat(ichem)
                            temp_row = projection_block_row_index(ichem, atom_idx, L, nat, step_size)
                            projmatrix(temp_row:temp_row+nrow_block-1, temp_col) = &
                                 & tmatri((atom_idx-1)*nrow_block+1:(atom_idx-1)*nrow_block+nrow_block, I)
                         end do
                         do I1 = 1, ndi
                            tmatriorg(1, I1, I) = real(tmatri(I1, I))
                            tmatriorg(2, I1, I) = aimag(tmatri(I1, I))
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
                   deallocate(jdpkorg)
                   deallocate(tmatri)
                   deallocate(tmatriorg)
                end do
             end do

          end do
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

    row_index = base_row + (atom_idx - 1)*step_size(ichem) + 2*L + 1
  end function projection_block_row_index

  subroutine validate_projection_block(pblock, n, tol, J, JD, ichem, L)
    complex(dp), intent(in) :: pblock(:,:)
    integer, intent(in) :: n
    real(dp), intent(in) :: tol
    integer, intent(in) :: J
    integer, intent(in) :: JD
    integer, intent(in) :: ichem
    integer, intent(in) :: L

    complex(dp), allocatable :: p2(:,:), delta(:,:)
    complex(dp), allocatable :: hermitian_delta(:,:), gram(:,:)
    complex(dp) :: trace_value
    integer :: row_idx, col_idx

    if (size(pblock, 1) /= n .or. size(pblock, 2) /= n) then
       write(*,*) "Projection block has inconsistent dimensions for J =", J, &
            & ", JD =", JD, ", ichem =", ichem, ", L =", L
       error stop "Projection block has inconsistent dimensions"
    end if

    allocate(p2(n, n))
    allocate(delta(n, n))
    allocate(hermitian_delta(n, n))
    allocate(gram(n, n))

    p2 = matmul(pblock, pblock)
    delta = p2 - pblock
    hermitian_delta = pblock - transpose(conjg(pblock))
    gram = matmul(transpose(conjg(pblock)), pblock)

    trace_value = cmplx(0.0_dp, 0.0_dp, kind=dp)
    do row_idx = 1, n
       trace_value = trace_value + pblock(row_idx, row_idx)
    end do

    do row_idx = 1, n
       do col_idx = 1, n
          if (abs(delta(row_idx, col_idx)) > tol) then
             write(*,*) "Projection block idempotency failed for J =", J, &
                  & ", JD =", JD, ", ichem =", ichem, ", L =", L
             write(*,*) "Projection block idempotency failed at (", row_idx, ",", col_idx, ")"
             write(*,*) "Residual =", delta(row_idx, col_idx)
             error stop "Projection block is not idempotent"
          end if
       end do
    end do

    do row_idx = 1, n
       do col_idx = 1, n
          if (abs(hermitian_delta(row_idx, col_idx)) > tol) then
             write(*,*) "Projection block Hermiticity failed for J =", J, &
                  & ", JD =", JD, ", ichem =", ichem, ", L =", L
             write(*,*) "Projection block Hermiticity failed at (", row_idx, ",", col_idx, ")"
             write(*,*) "Residual =", hermitian_delta(row_idx, col_idx)
             error stop "Projection block is not Hermitian"
          end if
       end do
    end do

    do row_idx = 1, n
       do col_idx = 1, n
          if (row_idx /= col_idx) then
             if (abs(gram(row_idx, col_idx)) > tol_projection) then
                write(*,*) "Projection block column orthogonality failed for J =", J, &
                     & ", JD =", JD, ", ichem =", ichem, ", L =", L
                write(*,*) "Projection block column orthogonality failed at (", row_idx, ",", col_idx, ")"
                write(*,*) "Residual =", gram(row_idx, col_idx)
                error stop "Projection block columns are not orthogonal"
             end if
          else
             if (abs(gram(row_idx, col_idx) - 1.0_dp) > tol_projection) then
                write(*,*) "Projection block column normalization failed for J =", J, &
                     & ", JD =", JD, ", ichem =", ichem, ", L =", L
                write(*,*) "Projection block column normalization failed at (", row_idx, ",", col_idx, ")"
                write(*,*) "Residual =", gram(row_idx, col_idx)
                error stop "Projection block columns are not normalized"
             end if
          end if
       end do
    end do

    if (abs(aimag(trace_value)) > tol) then
       write(*,*) "Projection block trace has non-negligible imaginary part for J =", J, &
            & ", JD =", JD, ", ichem =", ichem, ", L =", L
       error stop "Projection block trace has non-negligible imaginary part"
    end if

    if (abs(real(trace_value) - nint(real(trace_value))) > tol_projector_trace) then
       write(*,*) "Projection block trace =", trace_value, "for J =", J, &
            & ", JD =", JD, ", ichem =", ichem, ", L =", L
       error stop "Projection block trace is not close to an integer"
    end if

    deallocate(p2)
    deallocate(delta)
    deallocate(hermitian_delta)
    deallocate(gram)
  end subroutine validate_projection_block
end module projmat
