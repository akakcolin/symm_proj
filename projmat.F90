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
       & kgel, kkgel, listp, steer, ksym, ibz, K48, ldrmm, rk, u,tsmall, ttsmall)
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
    integer, intent(in) :: K48
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
    integer :: nb, ichem, L, N, mu1, mu2
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
    complex(dp), allocatable :: hugematrix(:,:,:,:,:,:)
    integer, allocatable :: col_index(:,:,:), row_index(:,:,:)
    integer, allocatable :: step_size(:)
    integer :: temp_row, temp_col 
    
    
    !integer :: ncols, nrows
    ! 1 3 5 
    allocate(hugematrix(maxval(nat)*7, maxval(nat)*7, 4, nel, ncl, maxval(laj)))

    allocate(col_index(4,nel, ncl))
    allocate(row_index(4,nel, ncl))
    col_index=0
    row_index=0

    hugematrix(:,:,:,:,:,:) = 0

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
                                     K = K + K48
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
                   ! test projection matrix

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
                   ! error, no projection matrix for J
                   if (K4 <= ndi) then
                      write(*,*) "Error, no projection matrix for J = ", J
                      write(*,*) ", JD= ", JD, ", laj= ", laj(J), ", ichem = ", ichem, "L = ", L, " ndi= ", ndi

                      jdpkorg(1, 1:ndi, 1:ndi) = real(jdpk(1:ndi,1:ndi))
                      jdpkorg(2, 1:ndi, 1:ndi) = aimag(jdpk(1:ndi,1:ndi))
                      tmatriorg(1, 1:ndi, 1:ndi) = real(tmatri(1:ndi, 1:ndi))
                      tmatriorg(2, 1:ndi, 1:ndi) = aimag(tmatri(1:ndi, 1:ndi))
                   end if
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
                      write(*,*) "jdpk - hermite conjugate(jdpk)"
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
                                     do while (NC <= ito)
                                        ! orthonormalisation against previously found orthonormal columns, which have non one element equal to 1
                                        R4 = dot_product(tmatri(1:ndi, NC), tmatri(1:ndi, itotal))
                                        if (abs(abs(R4)*abs(R4) -1 ) > 0.01) then
                                           tmatri(1:ndi, itotal) = tmatri(1:ndi, itotal) - R4*tmatri(1:ndi, NC)
                                           R1 = 0
                                           LD1 = 1
                                           do while (LD1 <= ndi)
                                              if (abs(tmatri(LD1, itotal)) >= 0.01) then
                                                 ! if all elements of the column become very samll after an orthonormalisation, we do
                                                 ! not use the column, Renormalisation would give large errors
                                                 exit
                                              end if
                                              LD1 = LD1 + 1
                                           end do
                                           if (LD1 <= ndi) then
                                              R1= sum(abs(tmatri(1:ndi, itotal))**2)
                                              R1 = 1/sqrt(R1)
                                              ! renormalisation
                                              tmatri(1:ndi, itotal) = tmatri(1:ndi, itotal) *R1
                                           else
                                              itotal = itotal - 1
                                              exit
                                           end if
                                        else
                                           itotal = itotal -1
                                           exit
                                        end if
                                        NC = NC + 1
                                     end do
                                     if (NC > ito) then
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
                      do I = 1, itotal
                         do I1 = 1, ndi
                            tmatriorg(1, I1, I) = real(tmatri(I1, I))
                            tmatriorg(2, I1, I) = aimag(tmatri(I1, I))
                            hugematrix(I1, I, L+1, ichem, j, JD) = tmatri(I1, I)
                         end do
                      end do

                      col_index(L+1, ichem, j) = itotal
                      row_index(L+1, ichem, j) = ndi

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

    !! use to output  projmatrix for dftb s p 
    !do JD=1, maxval(laj)
    !do J=1, ncl
    !   do ichem = 1, nel
    !      do L = 1, lmax(ichem)+1
    !         do I = 1, col_index(L, ichem, j)
    !            write(*,*) hugematrix(1:row_index(L,ichem, j), I, L, ichem, j,JD)
    !         end do
    !      end do
    !   end do
    !end do
    !end do

    allocate(step_size(nel))

    do ichem = 1, nel
       step_size(ichem) = 0
       do L = 0, lmax(ichem)
          step_size(ichem) = step_size(ichem) + 2*L + 1
       end do
    end do
    temp_col = 1
    do JD = 1, maxval(laj)
    do J = 1, ncl
       ndi = 0
       do ichem = 1, nel
          if (ichem >1) then
             ndi = ndi + nat(ichem-1)*step_size(ichem-1)
          end if
          do L = 1, lmax(ichem) + 1
             do I = 1, col_index(L, ichem, j)
                if (L == 1) then 
                   do n = 1, nat(ichem)
                      !write(*,*) "n,  I, ichem  temp_col", ndi + (n-1)*step_size(ichem)+1, &
                      !     & hugematrix(n, I, 1, ichem, j), temp_col
                      temp_row = ndi + (n-1)*step_size(ichem)+1
                      projmatrix(temp_row,temp_col) = &
                           & hugematrix(n,I,L,ichem,J, JD)
                   end do
                else if (L == 2) then
                   do n = 1, nat(ichem)
                      !dowrite(*,*) "n,  I, ichem  temp_col", ndi + (n-1)*step_size(ichem)+2,&
                      !     & ndi + (n-1)*step_size(ichem)+4, &
                      !     & hugematrix((n-1)*3+1:(n-1)*3+3, I, 2, ichem, j), temp_col
                      temp_row=ndi+(n-1)*step_size(ichem)
                      projmatrix(temp_row+2:temp_row+4, temp_col ) = &
                           & hugematrix((n-1)*3+1:(n-1)*3+3,I,L,ichem, j, JD)
                   end do
                else if (L == 3) then
                   do n = 1, nat(ichem)
                      temp_row = ndi+(n-1)*step_size(ichem)
                      projmatrix(temp_row+5: temp_row+9, temp_col) = &
                           & hugematrix((n-1)*5+1:(n-1)*5+5,I,L,ichem, j, JD)
                   end do
                end if
                temp_col = temp_col + 1
             end do
          end do
       end do
    end do
    end do

    ! reorder projmat
    deallocate(hugematrix)
    deallocate(col_index)
    deallocate(row_index)
    deallocate(step_size)

  end subroutine sym_projmat
end module projmat
