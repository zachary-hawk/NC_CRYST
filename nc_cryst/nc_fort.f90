module nc_fort
  implicit none
contains
  subroutine sym_positions(tol,super_n,pos,latt,inv_latt,prim_count,prim_list,&
       frac_pos,pos_keep,n_atoms,bonds,n_bonds,exclude)
    implicit none
    !integer,parameter :: dp = SELECTED_REAL_KIND(15)
    integer,intent(in) :: super_n
    real,intent(in),dimension(:,:) :: pos
    real,intent(in),dimension(:,:) :: latt
    real,intent(in),dimension(:,:) :: inv_latt
    real,intent(in) :: tol
    logical,intent(in) :: exclude
    real,dimension(super_n,3),intent(out) :: frac_pos

    integer :: i,j,k,l,m
    integer,intent(out) :: prim_count
    integer,dimension(1:super_n),intent(out) :: prim_list
    integer,dimension(super_n),intent(out) :: pos_keep
    integer,intent(out) :: n_atoms
    integer,intent(out) :: n_bonds
    real,intent(out),dimension(1:10000,3) :: bonds
    real :: sym_tol = 1E-2
    real :: bond
    logical :: a_bond,b_bond

    prim_count=0
    do i=1,super_n
       frac_pos(i,:)=matmul(inv_latt,pos(i,:))-1
    end do

    prim_list=-1
    pos_keep(:)=-1

    do i = 1,super_n
       if (frac_pos(i,1).le.1+sym_tol .and.frac_pos(i,2).le.1+sym_tol &
            & .and. frac_pos(i,3).le.1+sym_tol .and. &
            & frac_pos(i,1).ge.0-sym_tol .and. frac_pos(i,2).ge.0-sym_tol&
            & .and. frac_pos(i,3).ge.0-sym_tol )then
          prim_count=prim_count+1
          prim_list(prim_count)=i-1
       end if
    end do

    pos_keep(1:super_n)=prim_list(1:super_n)

    do i=1,super_n
       frac_pos(i,:)=matmul(latt,frac_pos(i,:))
    end do

    n_bonds=0
    do i=1,super_n
       do j=i,super_n
          if (i.eq.j) cycle
          a_bond=.false.
          b_bond=.false.
          do l=1,prim_count
             if (prim_list(l).eq.i-1.and..not.a_bond)then
                a_bond=.true.
             end if
             if (prim_list(l).eq.j-1.and..not.b_bond)then
                b_bond=.true.
             end if

          end do
          if (.not.a_bond.and..not.b_bond)cycle

          bond=sqrt(sum((pos(i,:)-pos(j,:))**2))

          if (bond.lt.tol)then
             if (.not.exclude)then 
                if (.not.b_bond)then
                   prim_count=prim_count+1
                   pos_keep(prim_count)=j-1
                elseif(.not.a_bond)then
                   prim_count=prim_count+1
                   pos_keep(prim_count)=i-1
                end if
             end if
             n_bonds=n_bonds+1
             bonds(n_bonds,1)=i-1
             bonds(n_bonds,2)=j-1
             bonds(n_bonds,3)=bond
          end if
       end do
    end do
    prim_list=pos_keep







  end subroutine sym_positions




  subroutine multmatmul(latt,vec,N,out)
    implicit none
    real,dimension(:,:),intent(in) :: latt
    real,dimension(:,:),intent(in) :: vec
    integer,intent(in) :: N

    real,dimension(N,3),intent(out):: out

    integer :: i
    do i=1,N
       out(i,:)=matmul(latt,vec(i,:))
    end do

  end  subroutine multmatmul




  subroutine matinv3(A,B)
    !! Performs a direct calculation of the inverse of a 3×3 matrix.
    real, intent(in) :: A(3,3)   !! Matrix
    real, intent(out):: B(3,3)   !! Inverse matrix
    real             :: detinv

    ! Calculate the inverse determinant of the matrix
    detinv = 1/(A(1,1)*A(2,2)*A(3,3) - A(1,1)*A(2,3)*A(3,2)&
         - A(1,2)*A(2,1)*A(3,3) + A(1,2)*A(2,3)*A(3,1)&
         + A(1,3)*A(2,1)*A(3,2) - A(1,3)*A(2,2)*A(3,1))

    ! Calculate the inverse of the matrix
    B(1,1) = +detinv * (A(2,2)*A(3,3) - A(2,3)*A(3,2))
    B(2,1) = -detinv * (A(2,1)*A(3,3) - A(2,3)*A(3,1))
    B(3,1) = +detinv * (A(2,1)*A(3,2) - A(2,2)*A(3,1))
    B(1,2) = -detinv * (A(1,2)*A(3,3) - A(1,3)*A(3,2))
    B(2,2) = +detinv * (A(1,1)*A(3,3) - A(1,3)*A(3,1))
    B(3,2) = -detinv * (A(1,1)*A(3,2) - A(1,2)*A(3,1))
    B(1,3) = +detinv * (A(1,2)*A(2,3) - A(1,3)*A(2,2))
    B(2,3) = -detinv * (A(1,1)*A(2,3) - A(1,3)*A(2,1))
    B(3,3) = +detinv * (A(1,1)*A(2,2) - A(1,2)*A(2,1))
  end subroutine matinv3






  subroutine trans_map(red_list,R,list,L,trans,N,map,counter)
    implicit none
    real,    intent(in),dimension(:,:)    :: red_list
    integer, intent(in)                   :: R
    real,    intent(in),dimension(:,:)    :: list
    integer, intent(in)                   :: L
    real,    intent(in),dimension(:,:,:)  :: trans
    integer, intent(in)                   :: N
    real,   intent(out),dimension(R,N,3,3)  :: map
    integer,intent(out),   dimension(1:R) :: counter

    integer :: i
    integer :: j,k,m

    real,dimension(1:3) :: pos


    counter(:)=1

    ! first get inverse
    !do i = 1,N
    !   call  matinv3(trans(i,:,:),inv_trans(i,:,:))
    !end do


    do j=1,R


       ! asymmetric position list(j,:) 
       do k=1,N
          ! Loop through inv_trans
          pos(:) = matmul(trans(k,:,:),red_list(j,:))!-(/0.5,0.5,0.5/))
          map(j,1,:,:)=trans(1,:,:)
          do m=1,L
             ! Loop for red list
             if ( abs(list(m,1)-pos(1)).lt. 0.01 .and.  abs(list(m,2)-pos(2)).lt. 0.01 &
                  & .and. abs(list(m,3)-pos(3)).lt. 0.01 ) then
                counter(j)=counter(j)+1
                map(j,counter(j),:,:)=trans(k,:,:)
             end if
          end do
       end do
    end do


    !print*,map


  end subroutine trans_map


  subroutine is_close(array, pos, len,tol,break)
    implicit none
    real,intent(in),dimension(:,:) :: array
    real,intent(in),dimension(:) :: pos
    real,intent(in) :: tol
    integer,intent(in) :: len
    logical, intent(out) :: break

    integer :: i

    do i =1,len
       if (abs(pos(1)-array(i,1)).lt. tol .and.abs(pos(2)-array(i,2)).lt. tol .and.abs(pos(3)-array(i,3)).lt. tol)then
          break=.true.
          continue
       else
          break=.false.
       end if
    end  do





  end subroutine is_close
end module nc_fort
