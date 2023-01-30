! ftcl_variables.f90 --
!    Routines to set and get variables
!
module ftcl_variables
    use iso_c_binding
    use ftcl_mod_interp

    interface
        function tcl_newstringobj( string, length ) bind(C, name = 'Tcl_NewStringObj' )
            import :: c_ptr, c_int
            character(len=1), dimension(*) :: string
            integer(kind=c_int), value     :: length
            type(c_ptr)                    :: tcl_newstringobj
        end function tcl_newstringobj

        function tcl_getvar( interp, varname, flags ) bind(C, name = 'Tcl_GetVar' )
            import :: c_ptr, c_int
            type(c_ptr), value                         :: interp
            character(len=1), dimension(*), intent(in) :: varname
            integer(kind=c_int), value                 :: flags
            type(c_ptr)                                :: tcl_getvar
        end function tcl_getvar
    end interface

    interface ftcl_getvar
        module procedure ftcl_getvar_string
        module procedure ftcl_getvar_integer
    end interface ftcl_getvar

contains

! ftcl_getvar_string --
!     Get the value of a variable as a string
!
! Arguments:
!     varname          Name of the variable
!     value            Value as an (allocatable) string
!
! Note:
!     Not the most efficient implementation, perhaps but it works.
!     Also: no check for errors yet.
!
!     Most important aspect: we need to copy the string because we do
!     not know how long it wil persist.
!
subroutine ftcl_getvar_string( varname, value )
    character(*), intent(in)                :: varname
    character(:), allocatable, intent(out)  :: value

    integer                                 :: i
    type(c_ptr)                             :: cvalue
    character(len=1), pointer, dimension(:) :: cchar

    cvalue = tcl_getvar( ftcl_interp, trim(varname) // c_null_char , 0_c_int )

    call c_f_pointer( cvalue, cchar, [strlen(cvalue)] )

    if ( allocated(value) ) then
        deallocate( value )
    endif

    allocate( character(len=size(cchar)) :: value )
    do i = 1,size(cchar)
        value(i:i) = cchar(i)
    enddo
end subroutine ftcl_getvar_string

! ftcl_getvar_integer --
!     Get the value of a variable as an integer
!
! Arguments:
!     varname          Name of the variable
!     value            Value as an integer
!
subroutine ftcl_getvar_integer( varname, value )
    character(*), intent(in)                :: varname
    integer, intent(out)                    :: value

    character(:), allocatable               :: string
    integer                                 :: ierr

    call ftcl_getvar_string( varname, string )

    read( string, *, iostat = ierr ) value

    if ( ierr /= 0 ) then
        value = 0
    endif
end subroutine ftcl_getvar_integer

end module ftcl_variables
