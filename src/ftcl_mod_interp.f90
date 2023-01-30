! ftcl_mod_interp.f90 --
!     Module to hold information about the Tcl interpreter
!
!     TODO:
!     Should we keep a copy of the clientData, as in the current implementation
!     or not? My concern: are allocatable components automatically copied or not?
!
module ftcl_mod_interp
    use iso_c_binding

    implicit none

    type(c_ptr) :: ftcl_interp
    integer     :: rc              ! Return code from Tcl functions

    integer, public, save   :: cmdcount = 1  ! Workaround ...

    abstract interface
        subroutine stored_proc( procdata )
            class(*) :: procdata
        end subroutine stored_proc
    end interface

    type :: proc_info
        procedure(stored_proc), pointer, nopass :: proc
        class(*), allocatable                   :: procdata
    end type proc_info

    type(proc_info), dimension(100), save :: cmdproc

    interface
        type(c_ptr) function Tcl_CreateCommand( interp, cmdname, proc, clientData, deleteProc ) bind(C, name = "Tcl_CreateCommand" )
            import :: c_ptr, c_funptr
            type(c_ptr), value             :: interp
            character(len=1), dimension(*) :: cmdname
            type(c_funptr), value          :: proc
            type(c_ptr), value             :: clientData
            type(c_funptr), value          :: deleteProc
        end function Tcl_CreateCommand

        function strlen( string ) bind(C, name = 'strlen' )
            import :: c_ptr, c_int
            type(c_ptr), value             :: string
            integer(kind=c_int)            :: strlen
        end function strlen
    end interface

contains

! ftcl_init_command --
!     Create the actual command that bridges between Fortran and Tcl
!
subroutine ftcl_init_command
    type(c_ptr) :: dummy_cmd

    dummy_cmd = Tcl_CreateCommand( ftcl_interp, "ftcl_command", c_funloc(ftcl_command), c_null_ptr, c_null_funptr )
end subroutine ftcl_init_command

! ftcl_command --
!     Run the associated Fortran routine
!
! Arguments:
!     clientData        Private data for the Tcl command (dummy here)
!     interp            Tcl interpreter
!     argc              Number of (string) arguments
!     argv              Array of pointers to the string arguments
!
integer function ftcl_command( clientData, interp, argc, argv )
    type(c_ptr), value :: clientData
    type(c_ptr), value :: interp
    integer, value     :: argc
    type(c_ptr), dimension(*) :: argv

    character(len=1), dimension(:), pointer :: argument
    character(len=:), allocatable           :: argstring
    integer                                 :: i, ierr
    integer                                 :: idx

    !
    ! Get the second argument. the first is the name of the Tcl cmmand
    !
    call c_f_pointer( argv(2), argument, [strlen(argv(2))] )

    allocate( character(len=size(argument)) :: argstring )
    do i = 1, len(argstring)
        argstring(i:i) = argument(i)
    enddo

    write( 20, * ) 'Argument: ', argstring

    read( argstring, *, iostat = ierr ) idx
    write( 20, * ) 'Index: ', idx, ierr, ' -> ', argstring

    !
    ! Check for possible errors before we continue!
    !
    if ( ierr /= 0 ) then
        return
    endif

    if ( idx > cmdcount .or. idx < 1 ) then
        return
    endif

    !
    ! Call the Fortran routine
    !
    write(20,*) 'Go', associated(cmdproc(idx)%proc), allocated(cmdproc(idx)%procdata)
    call cmdproc(idx)%proc( cmdproc(idx)%procdata )

    write(20,*) 'Done'

    ftcl_command = 0 ! No error information provided right now
end function ftcl_command

! ftcl_create_command --
!     Create a new Ftcl command via a Fortran routine
!
! Arguments:
!     proc              Procedure (Fortran subroutine) that implements the command
!     procdata          Private data belonging to the new command
!
! Notes:
!     - The actual Tcl command functions as a bridge
!     - Using the string interface for now, as it is simpler to connect to
!     - The interfacing with Tcl via command arguments is minimal
!
subroutine ftcl_create_command( proc, procdata )
    interface
        subroutine proc( procdata )
            class(*) :: procdata
        end subroutine proc
    end interface

    class(*) :: procdata

    integer :: i

    cmdcount = cmdcount + 1
    cmdproc(cmdcount)%proc => proc
    allocate( cmdproc(cmdcount)%procdata, source = procdata )

    do i = 1,10
        write(20,*) i, associated(cmdproc(i)%proc), allocated(cmdproc(i)%procdata )
    enddo

end subroutine ftcl_create_command

end module ftcl_mod_interp
