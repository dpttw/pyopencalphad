! Fortran based library for f2py compiler
module pytq
  use liboctq
  use liboceq
  implicit none

  type(gtp_equilibrium_data), pointer:: ceq

contains

  subroutine pytqini()

!    use liboctq
    implicit none

    integer:: n
!    type(gtp_equilibrium_data), pointer:: ceq

    call tqini(n,ceq)
    print*, loc(ceq)

    return

  end subroutine
! ===============================================
!  subroutine pytqrfil(filename)

!    use liboctq
!    implicit none

!    character(Len=60), intent(in):: filename
!    type(gtp_equilibrium_data), pointer:: ceq

!    call tqrfil(filename,ceq)
!    print*, loc(ceq)

!  end subroutine
! ===============================================
!subroutine pytqrpfil(filename,nsel,selel,ceq)

!  use liboctq
!  implicit none

!  character(Len=60), intent(in):: filename
!  type(gtp_equilibrium_data), pointer:: ceq

!  integer nsel
!  character(Len=2) selel(*)

!  call tqrpfil(filename,nsel,selel,ceq)

!end subroutine
! ===============================================
!subroutine pytqgcom(n,compnames,ceq)

!  use liboctq
!  implicit none

!  integer, intent(out):: n
!  character(Len=24), intent(out) :: compnames(*)
!  type(gtp_equilibrium_data), pointer :: ceq 

!  call tqgcom(n,compnames,ceq)

!end subroutine
! ===============================================
!subroutine pytqgnp(n,ceq)

!  use liboctq
!  implicit none

!  integer, intent(out):: n 
!  type(gtp_equilibrium_data), pointer :: ceq

!  call tqgnp(n,ceq)

!end subroutine
! ===============================================
!subroutine pytqgpn()

!  use liboctq
!  implicit none

!  call tqgpn(n,phnames(n),ceq)

!end subroutine


!subroutine pytqgpi()

!  use liboctq
!  implicit none

!  call tqgpi(ph_index(n),phnames(n),ceq)

!end subroutine


!subroutine pytqsetc()

!  use liboctq
!  implicit none

!  call tqsetc('T',0,0,temperature,cnum(1),ceq)
!  call tqsetc('P',0,0,1d5,cnum(2),ceq)
!  call tqsetc('N',0,0,one,cnum(3),ceq)

!end subroutine


!subroutine pytqce()

!  use liboctq
!  implicit none

!  target=' '
!  call tqce(target,0,0,value,ceq)
!end subroutine

!subroutine pytqgetv()

!  use liboctq
!  implicit none

!call tqgetv('G',n1,n2,n3,G_freeeng,ceq)

!end subroutine

end module
