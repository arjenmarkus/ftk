! demo_gui.f90 --
!     Very simple GUI to show that the basics are working:
!     It presents a column of three variables, start, stop and colour,
!     as well as two buttons, one "Draw" and one "Exit" and a canvas.
!     When you press "Draw", it draws a circle sector in the canvas
!     using the given colour. Any previous sector is removed first.
!
!     The GUI is starting from the start_gui routine - the name is
!     fixed and it should appear outside any modules.
!
subroutine start_gui
    use ftk_module

    implicit none

    !
    ! Widgets we create: they must be saved!
    !
    character(len=:), allocatable, save  :: canvas, title_label, param_label, &
                                            label_1, entry_1, &
                                            label_2, entry_2, &
                                            label_3, entry_3, &
                                            button_frame, draw_button, exit_button
    type(ftk_option), dimension(3) :: option

    !
    ! Set up the window:
    !
    !    --- Text to explain the GUI ----
    !    Parameters:          +-----------------------------+
    !    Start:      [    ]   |                             |
    !    Stop:       [    ]   |                             |
    !    Colour:     [    ]   |                             |
    !                         |                             |
    !                         |                             |
    !                         |                             |
    !                         |                             |
    !                         +-----------------------------+
    !                    [Draw]      [Exit]
    !

    call ftcl_evaluate( "console show" )

    canvas      = ftk_create_widget( default_toplevel, ftk_canvas, "canvas" )

    option(1)   = ftk_option( "-text", "Draw a circle sector in a given colour" )
    option(2)   = ftk_option( "-text", "Parameters:" )
    title_label = ftk_create_widget( default_toplevel, ftk_label, "title", option(1:1) )
    param_label = ftk_create_widget( default_toplevel, ftk_label, "param", option(2:2) )

    option(1)   = ftk_option( "-text", "Start angle:" )
    option(2)   = ftk_option( "-text", "Stop angle:" )
    option(3)   = ftk_option( "-text", "Colour:" )
    label_1     = ftk_create_widget( default_toplevel, ftk_label, "label_1", option(1:1) )
    label_2     = ftk_create_widget( default_toplevel, ftk_label, "label_2", option(2:2) )
    label_3     = ftk_create_widget( default_toplevel, ftk_label, "label_3", option(3:3) )

    option(1)   = ftk_option( "-textvariable", "start" )
    option(2)   = ftk_option( "-textvariable", "stop" )
    option(3)   = ftk_option( "-textvariable", "colour" )
    entry_1     = ftk_create_widget( default_toplevel, ftk_entry, "entry_1", option(1:1) )
    entry_2     = ftk_create_widget( default_toplevel, ftk_entry, "entry_2", option(2:2) )
    entry_3     = ftk_create_widget( default_toplevel, ftk_entry, "entry_3", option(3:3) )

    button_frame = ftk_create_widget( default_toplevel, ftk_frame, "frame" )

    option(1)   = ftk_option( "-text", "Draw" )
    option(2)   = ftk_option( "-text", "Exit" )
    draw_button = ftk_create_widget( button_frame, ftk_button, "draw", option(1:1) )
    exit_button = ftk_create_widget( button_frame, ftk_button, "exit", option(2:2) )

    call ftk_grid_add_row( [draw_button, exit_button] )

    call ftk_grid_add_row( [title_label, ftk_add_left, ftk_add_left] )
    call ftk_grid_add_row( [param_label, ftk_add_left, canvas]       )
    call ftk_grid_add_row( [label_1, entry_1, ftk_add_top]         )
    call ftk_grid_add_row( [label_2, entry_2, ftk_add_top]         )
    call ftk_grid_add_row( [label_3, entry_3, ftk_add_top]         )
    call ftk_grid_add_row( [button_frame, ftk_add_left, ftk_add_left] )

    !
    ! Initialise
    !
    call ftcl_evaluate( "set start 60" )
    call ftcl_evaluate( "set stop 180" )
    call ftcl_evaluate( "set colour red" )

    !
    ! Add the commands
    ! TODO:
    ! Find out why we need to have a dummy first entry. It is bizarre.
    !
    call ftk_button_add_command( ".a"       , do_not_use, ""  )
    call ftk_button_add_command( draw_button, draw_sector, ""  )
    call ftk_button_add_command( exit_button, stop_program, ""  )

    ! TODO: the rest

    !
    ! SImply return, the event loop takes over
    !

contains
subroutine draw_sector( dummy )
    class(*) :: dummy

    integer                       :: start, stop
    character(len=:), allocatable :: colour
    character(len=200)            :: string

    write(20,*) 'Draw!'

    call ftcl_getvar( "start",  start )
    write(20,*) 'Draw!', start
    call ftcl_getvar( "stop",   stop  )
    write(20,*) 'Draw!', stop
    call ftcl_getvar( "colour", colour )
    write(20,*) 'Draw!', colour

    if ( colour == "" ) then
        colour = "white"
    endif

    call ftcl_evaluate( canvas // " delete all" )

    write( string, * ) canvas, " create arc ", 10, 10, 210, 210, " -start ", start, " -extent ", stop-start, " -fill ", colour
    write(20,*) 'Draw!', string

    call ftcl_evaluate( string )
    write(20,*) 'Draw! - done'

end subroutine draw_sector

subroutine stop_program( dummy )
    class(*) :: dummy

    write(20,*)  "We are done"
    stop

end subroutine stop_program

subroutine print_hello( dummy )
    class(*) :: dummy

    write(20,*)  "Hello"

end subroutine print_hello

subroutine do_not_use( dummy )
    class(*) :: dummy

end subroutine do_not_use

end subroutine start_gui
