toplevel .goalsdialog -class Dialog
wm title .goalsdialog {Steam Sky - Select goal}
wm transient .
if {$tcl_platform(os) == "Linux"} {
   wm attributes .goalsdialog -type dialog
}
wm geometry .goalsdialog 200x600+[expr [winfo vrootwidth .goalsdialog] / 2]+[expr [winfo vrootheight .goalsdialog] / 4]
pack [ttk::treeview .goalsdialog.view] -expand true -fill both
pack [ttk::button .goalsdialog.selectbutton -text {Select goal}] -fill x
pack [ttk::button .goalsdialog.closebutton -text {Close (Escape)} -command {destroy .goalsdialog}] -fill x
bind .goalsdialog <Escape> {.goalsdialog.closebutton invoke}
