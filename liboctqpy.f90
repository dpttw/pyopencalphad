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
  integer, intent(out):: int_out(800)
  real(8), intent(out):: double_out(800)
  character*24, intent(out):: char_out(800)
! f2py has specific way to recognize string
!     character(LEN=24):: xx(100)  => fortran: 24*100 - py: len(xx)=100*1
!     character*24:: xx(100)  => fortran: 24*100 - py: len(xx)=100*24
!     character:: xx(100)*24 => fortran: 1*100 - py: len(xx)=100*24
!     character:: XX(100) => fortran: 1*100 - py: len(xx)=100*1
!     character*24:: XX => no need to test it, I need 100 
! ============================

  integer:: i,j,k,ierr,nc
  integer:: string_index(2)
  integer:: local_int(3)
  real(8):: local_double(2)
  character(Len=256):: local_char

  character(Len=256):: filename

! ============================ ini
  integer:: idum  ! dummy variable

! ============================ gcom
  integer:: no_el
  character(Len=24):: phasenames(maxp)
  character(Len=2):: elenames(maxc)

! ============================ gphc
  integer:: no_sublatice
  integer:: no_ele_sublatice(maxp)
  integer:: ele_index(maxc*maxp)
  real(8):: comp_sublatice(maxc*maxp)
  real(8):: no_sites(maxp)
  real(8):: extra_info(2)

! ============================ gdif
  real(8):: molar_frac(maxc),chem_pot(maxc*maxp)
  real(8):: thermo_factor(maxc*maxp),mob(maxc)
  real(8):: diffusivity

! ============================ gnp, gpn
  integer:: np  ! #phase
  integer:: cond(800) 

!  double precision localv
  real(8):: npf(800)
  save ceq

  ierr=0

  if(call_func(1:2) == 'tq') then
    call_func = trim(call_func(3:len_trim(call_func)))
  else
    call_func = trim(call_func)
  end if

! ========================= functions
  select case(call_func)

    case default
       ierr=7777
!-------------------------------------------
    case('ini') ! initiate
      idum=20
      call tqini(idum,ceq)
      if(gx%bmperr.ne.0) goto 900
!-------------------------------------------
    case('rfil') ! read tdbfile
      local_char=' '
      local_char=trim(char_var)
      call tqrfil(local_char,ceq)

      if(gx%bmperr.ne.0) goto 900
!-------------------------------------------
    case('rpfil') ! read tdbfile & select element
      local_int(1) = int_var(1)
      local_char=' '
      local_char=trim(char_var)

      string_index(1)=SCAN(local_char,' ')
      string_index(2)=len(local_char) 

      filename=local_char(1:string_index(1))
      string_index(1)=string_index(1)+1
      string_index(2)=string_index(1)+SCAN(local_char(string_index(1):),' ')-1

      i=1
      do while(string_index(1)<string_index(2) )
        elenames(i)=trim(local_char(string_index(1):string_index(2)))

        i=i+1
        string_index(1)=string_index(2)+1 
        string_index(2)=string_index(1)+SCAN(local_char(string_index(1):),' ')-1
      end do

      call tqrpfil(filename,local_int(1),elenames,ceq)

      if(gx%bmperr.ne.0) goto 900
!-------------------------------------------
    case('phsts') ! set phase status
      local_int(1)=int_var(1)
      local_int(2)=int_var(2)
      local_double(1)=double_var(1)
      call tqphsts(local_int(1),local_int(2),local_double(1),ceq)

      if(gx%bmperr.ne.0) goto 900

!-------------------------------------------
    case('gcom') ! number of components and their names
      no_el=0
      call tqgcom(no_el,elenames,ceq)
      if(gx%bmperr.ne.0) goto 900

      int_out(1)=no_el      !also equal to nel
      do i=1,no_el
        char_out(i)=elenames(i)(1:len_trim(elenames(i)))
      end do
!-------------------------------------------
    case('gnp') ! number of phase tuples, stored in ntup
!      call tqgnp(np,ceq)
!      if(gx%bmperr.ne.0) goto 900
!      int_out(2)=np

      int_out(1)=ntup

!-------------------------------------------
    case('gpn') ! list name of phase
      phasenames(1)=' '
      int_out(1)=ntup
      do i=1,ntup
        call tqgpn(i,phasenames(i),ceq)
        if(gx%bmperr.ne.0) goto 900

        char_out(i)=phasenames(i)(1:len_trim(phasenames(i)))
      end do

!-------------------------------------------
    case('gpi') ! get phase index
      local_char=' '
      local_char=trim(char_var)

      !do i=1,ntup
        call tqgpi(local_int(1),local_char,ceq)
        if(gx%bmperr.ne.0) goto 900

        !char_out(i)=phasenames(i)(1:len_trim(phasenames(i)))
      !end do

      int_out(1)=local_int(1)

!-------------------------------------------
    case('setc') ! set condition
      local_char=char_var(1:len_trim(char_var))
      local_double(1)=double_var(1)
      local_int(1)=int_var(1)
      local_int(2)=int_var(2)

      call tqsetc(local_char,local_int(1),local_int(2),local_double(1),cond(1),ceq)
      if(gx%bmperr.ne.0) goto 900

!-------------------------------------------
    case('ce') ! calculate equilibrium
      local_int(1)=int_var(1)    ! i=0 means call grid minimizer
      local_int(2)=int_var(2)
      local_char=' '
      local_double(1)=0.
      call tqce(local_char,local_int(1),local_int(2),local_double(1),ceq)
      if(gx%bmperr.ne.0) goto 900

!-------------------------------------------
    case('getv') ! get value
      local_char=' '
      local_char=trim(char_var)

      local_int(1)=int_var(1)
      local_int(2)=int_var(2)
      local_int(3)=size(npf)
      call tqgetv(local_char,local_int(1),local_int(2),local_int(3),npf,ceq)
      if(gx%bmperr.ne.0) goto 900

      double_out=npf

!-------------------------------------------
    case('gphc')
      local_int(1)=int_var(1)   !(input) phase tuple index
      !(output) no_sublatice: number of sublattices (1 if no sublattices)
      !(output) no_ele_sublatice: number element in sublatice
      !(output) ele_index: index of the elements
      !(output) comp_sublatice: chemical composition in sublattice
      !(output) no_sites: number sites of sublatice
      !(output) extra_info: 2 extra information

      call tqgphc1(local_int(1),no_sublatice,no_ele_sublatice,ele_index,comp_sublatice,no_sites,extra_info,ceq)
      if(gx%bmperr.ne.0) goto 900

      int_out(1)=no_sublatice

      nc=0
      do i=1,no_sublatice
        nc=nc+no_ele_sublatice(i)
      end do
!      int_out(2)=nc

      int_out(2:1+no_sublatice)=no_ele_sublatice(1:no_sublatice)
      int_out(2+no_sublatice:1+no_sublatice+nc)=ele_index(1:nc)

      double_out(1:nc)=comp_sublatice(1:nc)
      double_out(nc+1:nc+no_sublatice)=no_sites(1:no_sublatice)
      double_out(nc+no_sublatice+1)=extra_info(1)
      double_out(nc+no_sublatice+2)=extra_info(2)

!print*, local_int(1),no_sublatice
!print*, "size of composition array",nc
!print*, "element in sublattice",no_ele_sublatice(1:no_sublatice)
!print*, "element location",ele_index(1:nc)
!print*, "composition in each sublattice",comp_sublatice(1:nc)
!print*, "sites ",no_sites(1:no_sublatice)
!print*, "extra",extra_info
!print*, int_out(1:1+no_sublatice+nc)
!print*, double_out(1:nc+no_sublatice+2)

!-------------------------------------------
    case('gdif')
      local_int(1)=size(double_var)

!print*, local_int(1)
!print*, "GDIF",double_var(1),double_var(2)
!print*, double_var(3:local_int(1))

      call tqgdif(int_var(1),int_var(2),int_var(3),double_var(1),double_var(2),double_var(3:local_int(1)),diffusivity,ceq)

      double_out(1)=diffusivity



!--------------------------------------------
    case('reset') ! reset error vcode
      gx%bmperr=0
    end select

  goto 1000
! If there is an error I terminate here as I do not know how to do it in C

900 continue
  write(*,*)'Error code ',gx%bmperr,' for call ',call_func
  ierr=gx%bmperr
  stop

1000 continue
  pytq=ierr
  return
end function pytq

