toplevel .debugdialog -class Dialog
wm title .debugdialog {Steam Sky - Debug menu}
wm transient .debugdialog .
if {$tcl_platform(os) == "Linux"} {
   wm attributes .debugdialog -type dialog
}
grid [ttk::frame .debugdialog.buttons]
grid [ttk::frame .debugdialog.main] -column 1 -row 0
proc ShowFrame {framename} {
   global mainframe
   grid remove [grid slaves .debugdialog.main]
   grid .debugdialog.main
}
grid [ttk::button .debugdialog.buttons.ship -text Ship -command {ShowFrame .debugdialog.main.ship}]
grid [ttk::button .debugdialog.buttons.crew -text Crew -command {ShowFrame .debugdialog.main.crew}]
grid [ttk::button .debugdialog.buttons.cargo -text Cargo -command {ShowFrame .debugdialog.main.cargo}]
grid [ttk::button .debugdialog.buttons.bases -text Bases -command {ShowFrame .debugdialog.main.bases}]
grid [ttk::button .debugdialog.buttons.world -text World -command {ShowFrame .debugdialog.main.world}]
grid [ttk::button .debugdialog.buttons.refresh -text Refresh]
grid [ttk::button .debugdialog.buttons.save -text {Save game}]
# Ship options
set shipframe [ttk::frame .debugdialog.main.shipframe]
grid [ttk::button $shipframe.move -text {Move ship}]
grid [ttk::label $shipframe.lblx -text {X:}] -column 1 -row 0
grid [ttk::spinbox $shipframe.x -from 1 -to 1024] -column 2 -row 0
grid [ttk::label $shipframe.lbly -text {Y:}] -column 3 -row 0
grid [ttk::spinbox $shipframe.y -from 1 -to 1024] -column 4 -row 0
grid [ttk::label $shipframe.modulelbl -text {Module:}]
grid [ttk::combobox $shipframe.module -state readonly] -column 1 -row 1 -columnspan 3
grid [ttk::label $shipframe.protolbl -text {Prototype:}]
grid [ttk::entry $shipframe.proto]  -column 1 -row 2 -columnspan 3
grid [ttk::label $shipframe.weightlbl -text {Weight:}]
grid [ttk::spinbox $shipframe.weight -from 0 -to 100000] -column 1 -row 3 -columnspan 3
grid [ttk::label $shipframe.durlbl -text {Durability:}]
grid [ttk::spinbox $shipframe.dur -from 0 -to 1000] -column 1 -row 4 -columnspan 3
grid [ttk::label $shipframe.maxdurlbl -text {Max durability:}]
grid [ttk::spinbox $shipframe.maxdur -from 0 -to 1000] -column 1 -row 5 -columnspan 3
grid [ttk::label $shipframe.upgradelbl -text {Upgrade progress:}]
grid [ttk::spinbox $shipframe.upgrade -from 0 -to 1000000] -column 1 -row 6 -columnspan 3
grid [ttk::button $shipframe.change -text Change] -columnspan 4
# Crew options
# Cargo options
# Bases options
# World options
grid $shipframe
wm geometry .debugdialog +[expr ([winfo vrootwidth .debugdialog] / 2) - 200]+[expr [winfo vrootheight .debugdialog] / 3]
