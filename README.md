# Building graphical user-interfaces with Fortran and Tcl/Tk

The project aims to provide a stragihtforward means to build graphical user-interfaces
in Fortran based on Tcl/Tk. In its current state it is proof of concept, not a full-fledged
library. While only some basic features have been implemented, it does show the possibilities
of leveraging Tcl/Tk for this purpose.


## Some considerations

The philosophy of the interface is that much can be done via _evaluating_ Tcl commands, rather
than calling the Tcl API directly. Of course, this means that some roundabout processing is required:
building the string representing the command first in Fortran and then having it evaluated by Tcl/Tk.
Still, for the purpose of setting up the GUI this is fast enough and it saves detailed interfacing
between C and Fortran.

Another consideration of this interface is that much if not all can be done via Fortran directly,
thanks to the standardised C-Fortran binding. That does mean that some things are a trifle
difficult: the stubs mechanism relies on macros, just as the lifetime management for TclObj
data structures and Fortran can not deal with C macros. That means that, at least for the
moment, the interfacing relies on the string interface.

The library does not provide any error handling yet. It is after all a proof of concept.

## Demo

The project contains a single demo program. It needs to be built as a library that can be
loaded by Tcl/Tk's wish program. The sample build script demonstrates this.

A convenient aspect of simply loading a library is that the Fortran part does not need to
initialise the interpreter.

The demo program does not pay much attention to the layout. That is something that needs to be improved.
Also, a lot of convenience routines are still missing.
