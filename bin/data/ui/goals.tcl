toplevel .goalsdialog -class Dialog
wm title .goalsdialog {Steam Sky - Select Goal}
wm transient .goalsdialog .
if {$tcl_platform(os) == "Linux"} {
   wm attributes .goalsdialog -type dialog
}
set view [ttk::treeview .goalsdialog.view -show tree -yscrollcommand [list .goalsdialog.yscroll set] -xscrollcommand [list .goalsdialog.xscroll set]]
set selectbutton [ttk::button .goalsdialog.selectbutton -text {Select goal}]
grid $view -sticky nwes
$view column #0 -minwidth 400 -stretch 1
$view insert {} end -id 0 -text Random
$view insert {} end -id REPUTATION -text {Gain max reputation in bases}
$view insert {} end -id DESTROY -text {Destroy enemy ships}
$view insert {} end -id DISCOVER -text {Discover map}
$view insert {} end -id VISIT -text {Visit bases}
$view insert {} end -id CRAFT -text {Craft items}
$view insert {} end -id MISSION -text {Finish missions}
$view insert {} end -id KILL -text {Kill enemies in melee combat}
$view selection set 0
bind $view <<TreeviewSelect>> {
   set selected [lindex [$view selection] 0]
   if {[$view parent $selected] == {} && [$view item $selected -text] != {Random}} {
      $selectbutton state disabled
   } else {
      $selectbutton state !disabled
   }
}
bind $view <Double-1> {$selectbutton invoke}
bind $view <Return> {$selectbutton invoke}
grid [ttk::scrollbar .goalsdialog.yscroll -orient vertical -command [list $view yview]] -column 1 -row 0 -sticky ns
grid [ttk::scrollbar .goalsdialog.xscroll -orient horizontal -command [list $view xview]] -row 1 -column 0 -sticky we -columnspan 2
grid columnconfigure .goalsdialog $view -weight 1
grid rowconfigure .goalsdialog $view -weight 1
grid $selectbutton -row 2 -columnspan 2 -sticky we
grid [ttk::button .goalsdialog.closebutton -text {Close (Escape)} -command {CloseDialog .goalsdialog}] -row 3 -columnspan 2 -sticky we
bind .goalsdialog <Escape> {.goalsdialog.closebutton invoke}
wm geometry .goalsdialog =400x400+[expr ([winfo vrootwidth .goalsdialog] / 2) - 200]+[expr [winfo vrootheight .goalsdialog] / 3]
