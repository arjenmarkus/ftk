rem The stubs library is not useable!

ifort -c ftcl_mod_interp.f90
ifort -c ftcl_variables.f90
ifort -c ftk_module.f90
ifort demo_gui.f90 ftk_module.obj ftcl_variables.obj ftcl_mod_interp.obj -exe:ftk.dll c:\ActiveTcl\lib\tcl86t.lib -dll
