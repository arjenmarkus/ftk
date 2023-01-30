! ftk_module.f90 --
!     Preliminary attempt to create a module for creatuing GUIs in Tcl/Tk wtih Fortran
!     The idea:
!     Provide access to Tk but without too much details
!
module ftk_module
    use iso_c_binding
    use ftcl_mod_interp
    use ftcl_variables

    implicit none

    private

    public :: ftcl_evaluate ! Temporary?
    public :: ftk_create_widget, ftk_grid_add_row, ftcl_getvar, ftk_button_add_command

    character(len=1), parameter, public :: default_toplevel = '.'
    integer, parameter, public          :: ftk_canvas       = 1
    integer, parameter, public          :: ftk_entry        = 2
    integer, parameter, public          :: ftk_label        = 3
    integer, parameter, public          :: ftk_checkbutton  = 4
    integer, parameter, public          :: ftk_radiobutton  = 5
    integer, parameter, public          :: ftk_button       = 6
    integer, parameter, public          :: ftk_frame        = 7

    type, public :: ftk_option
        character(len=20)             :: name
        character(len=:), allocatable :: value
    end type

    character(len=:), allocatable, public :: ftk_add_left
    character(len=:), allocatable, public :: ftk_add_top

    interface
        integer(c_int) function Tcl_EvalEx( interp, string, length, flags ) bind(C, name = "Tcl_EvalEx" )
            import c_int, c_ptr
            type(c_ptr), value             :: interp
            character(len=1), dimension(*) :: string
            integer(c_int), value          :: length, flags
        end function Tcl_EvalEx
    end interface

contains

! Ftk_Init --
!     Initialise the package
!
! Arguments:
!     interp         The Tcl/Tk interpreter
!
! Returns:
!     0 asit should all go fine
!
integer function Ftk_Init( interp ) bind(C, name = "Ftk_Init")
!dec$ attributes dllexport :: Ftk_init
    type(c_ptr), value :: interp

    character(len=:), allocatable  :: cnv, cnv2, button, entry, label, version
    type(ftk_option), dimension(2) :: options

    integer :: dummy

    ftcl_interp = interp

    ftk_add_left = '-'
    ftk_add_top  = '^'

    call ftcl_init_command
    call start_gui

    Ftk_init = 0
end function Ftk_init

! ftcl_evaluate
!     Evaluate a Tcl command as found in a string
!
! Arguemtns:
!     string         String containing the command
!
! Note:
!     The return value is not passed on directly
!
subroutine ftcl_evaluate( string )
     character(len=*), intent(in)  :: string

     rc = Tcl_EvalEx( ftcl_interp, string, len(string), 0_c_int )

end subroutine ftcl_evaluate

! ftk_make_options --
!     Construct a string of options
!
! Arguments:
!     options           The options array
!
! Returns:
!     Full name of the widget
!
function ftk_make_options( options )
    type(ftk_option), dimension(:), intent(in) :: options

    character(len=:), allocatable :: ftk_make_options
    integer                       :: i

    ftk_make_options = ''

    do i = 1, size(options)
        ftk_make_options = ftk_make_options // " " // options(i)%name // " """ // options(i)%value // """"
    end do
end function ftk_make_options

! ftk_create_widget --
!     Create a widget inside the given parent
!
! Arguments:
!     parent            The parent widget
!     type              The type of widget (a predefined parameter)
!     widget            The name of the widget to be created
!     options           zero or more key-value pairs
!
! Returns:
!     Full name of the widget
!
function ftk_create_widget( parent, type, widget, options )
    character(len=*), intent(in)   :: parent
    integer, intent(in)            :: type
    character(len=*), intent(in)   :: widget
    type(ftk_option), dimension(:), optional, intent(in) :: options

    character(len=len(parent)+len(widget)+1) :: ftk_create_widget
    character(len=:), allocatable            :: widget_options

    if ( parent /= default_toplevel ) then
        ftk_create_widget = trim(parent) // '.' // trim(widget)
    else
        ftk_create_widget = '.' // trim(widget)
    endif

    if ( present(options) ) then
        widget_options = ftk_make_options( options )
    else
        widget_options = ''
    endif

    select case ( type )
        case ( ftk_canvas )
            call ftcl_evaluate( "canvas " // ftk_create_widget // " " // widget_options )

        case ( ftk_entry )
            call ftcl_evaluate( "ttk::entry " // ftk_create_widget // " " // widget_options )

        case ( ftk_label )
            call ftcl_evaluate( "ttk::label " // ftk_create_widget // " " // widget_options )

        case ( ftk_checkbutton )
            call ftcl_evaluate( "ttk::checkbutton " // ftk_create_widget // " " // widget_options )

        case ( ftk_radiobutton )
            call ftcl_evaluate( "ttk::radiobutton " // ftk_create_widget // " " // widget_options )

        case ( ftk_button )
            call ftcl_evaluate( "ttk::button " // ftk_create_widget // " " // widget_options )

        case ( ftk_frame )
            call ftcl_evaluate( "ttk::frame " // ftk_create_widget // " " // widget_options )

        case default
            ! This would bean error
    end select
end function ftk_create_widget

! ftk_grid_add_row --
!     Manage the widgets via the grid geometry manager
!
! Arguments:
!     widgets           Array of widgets
!
! Returns:
!     Nothing
!
subroutine ftk_grid_add_row( widgets )
    character(len=*), dimension(:) :: widgets

    character(len=:), allocatable  :: grid_cmd
    integer                        :: i

    grid_cmd = 'grid '

    do i = 1, size(widgets)
        grid_cmd = grid_cmd // " " // trim(widgets(i))
    end do

    call ftcl_evaluate( grid_cmd )
end subroutine ftk_grid_add_row

! ftk_button_add_command --
!     Add a command to the button
!
! Arguments:
!     button            Name of the button
!     proc              Fortran routine to call
!     data              Private data to pass to it when called
!
! Returns:
!     Nothing
!
subroutine ftk_button_add_command( button, proc, data )
    character(len=*)               :: button
    procedure(stored_proc)         :: proc
    class(*)                       :: data

    character(len=100)             :: cmd

    call ftcl_create_command( proc, data )

    write( cmd, * ) button, " configure -command {ftcl_command ", cmdcount, "}"
    write( 20, * ) cmd

    call ftcl_evaluate( cmd )
end subroutine ftk_button_add_command

end module ftk_module
