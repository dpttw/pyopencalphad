! 02/18/2016
! This function is written based on C interface provided by OC
! oc/tq/C/cexample1/liboctqc.F90
! 
! This function works with OC3B
!
integer function pytq(call_func,int_var,double_var,char_var,int_out,double_out,char_out)

  use liboctq

  implicit none

  type(gtp_equilibrium_data), pointer :: ceq
!  integer, parameter :: maxc=20,maxp=100  ! according to liboctq.F90

! ============================ inputs
! call_func is which service
! int_var is an integer argument
!    ---- according to liboctq.F90, max number of integer inputs is 4
! double_var is real argument
! char_var is a double argument (can it be an array?)

  character(Len=20):: call_func
  integer, intent(in):: int_var(:)
  real(8), intent(in):: double_var(:)
  character(Len=*):: char_var

! ============================ outputs
  integer, intent(out):: int_out(100)
  real(8), intent(out):: double_out(100)
  character(Len=*), intent(out):: char_out(100)



  integer i,j,k,ierr,nc
  
! ============================ ini
  integer:: idum  ! dummy variable

! ============================ rfil
  character(Len=256):: filename

! ============================ gcom
  integer:: no_el
  character(Len=24):: cnames(maxc)

! ============================ gnp
!  integer:: np


!  double precision localv
!  real(8):: npf(maxp)
  save ceq

  ierr=0

!print*, size(int_var),size(double_var)  !,size(char_var)
!print*, char_var
!print*, "len:  ",len_trim(call_func)

  if(call_func(1:2) == 'tq') then
    call_func = trim(call_func(3:len_trim(call_func)))
  else
    call_func = trim(call_func)
  end if

! ========================= functions
  select case(call_func)

    case default
       ierr=7777

    case('ini') ! initiate
      idum=20
      call tqini(idum,ceq)
      if(gx%bmperr.ne.0) goto 900

    case('rfil') ! read tdbfile
      filename=' '
      filename=trim(char_var)
      call tqrfil(filename,ceq)

      if(gx%bmperr.ne.0) goto 900

    case('gcom') ! number of components and their names
!      no_el=0
!      call tqgcom(no_el,cnames,ceq)
!      if(gx%bmperr.ne.0) goto 900

      int_out(1)=nel
      do i=1,no_el
        char_out(i)=cnames(i)(1:len_trim(cnames(i)))
      end do

    case('gnp') ! number of phase tuples, stored in ntup
!      call tqgnp(np,ceq)
!      if(gx%bmperr.ne.0) goto 900
!      int_out(2)=np

      int_out(1)=ntup

!-------------------------------------------
    case('gpn') ! list name of phase
      cnames(1)=' '
      nc=ntup+1
      call tqgpn(nc,cnames(1),ceq)
      if(gx%bmperr.ne.0) goto 900

      j=len_trim(cnames(1))
!     write(*,*)'Name of phase ',nc,': ',cnames(1),j
      do k=1,j
        tv(k)=ichar(cnames(1)(k:k))
      enddo
!     write(*,*)'First letter: ',char(tv(1))

      tv(j+1)=0
!-------------------------------------------
!  case('setc') ! set condition
!     filename=' '
!     tochar2: do i=1,len(filename)
!        if(tv(i).eq.0) exit tochar2
!        filename(i:i)=char(tv(i))
!     enddo tochar2
!     i=iv
!     j=0
!     localv=rv
!!     write(*,*)'CASE 6; ',filename(1:len_trim(filename)),i,localv
!     call tqsetc(filename,i,j,localv,cnum(1),ceq)
!     if(gx%bmperr.ne.0) goto 900
!-------------------------------------------
!  case('ce') ! calculate equilibrium
!! i=0 means call grid minimizer
!     i=0
!     j=0
!     filename=' '
!     localv=0
!     call tqce(filename,i,j,localv,ceq)
!     if(gx%bmperr.ne.0) goto 900
!-------------------------------------------
!  case('getv') ! get value
!     filename=' '
!     tochar3: do i=1,len(filename)
!        if(tv(i).eq.0) exit tochar3
!        filename(i:i)=char(tv(i))
!     enddo tochar3
!     i=iv
!     j=0
!     k=size(npf)
!     call tqgetv(filename,i,j,k,npf,ceq)
!     if(gx%bmperr.ne.0) goto 900
!     rv=npf(1)
!--------------------------------------------
!  case('reset') ! reset error vcode
!     gx%bmperr=0
  end select

  goto 1000
! If there is an error I terminate here as I do not know how to do it in C
900 continue
  write(*,*)'Error code ',gx%bmperr,' for call ',call_func
  ierr=gx%bmperr
  stop
!
1000 continue
  pytq=ierr
  return
end function pytq
!
