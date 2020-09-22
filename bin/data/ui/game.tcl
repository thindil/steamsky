# Game menu
menu .gamemenu -title {Steam Sky - menu}
# Orders menu
menu .orders -tearoff false
# Destination menu
menu .destination -tearoff false
# Game header
ttk::frame .header
grid [ttk::menubutton .header.menubutton -text {Menu} -menu .gamemenu] -sticky w
ttk::button .header.closebutton -text {Close [Escape]} -command {ShowSkyMap}
grid [ttk::label .header.time -text {1600-03-01}] -row 0 -column 2
tooltip::tooltip .header.time {The game time}
grid columnconfigure .header .header.time -weight 1
grid [ttk::label .header.nofuel -style Headerred.TLabel -font InterfaceIcons] -row 0 -column 3 -sticky e
grid [ttk::label .header.nofood -style Headerred.TLabel -font InterfaceIcons] -row 0 -column 4 -sticky e
grid [ttk::label .header.nodrink -style Headerred.TLabel -font InterfaceIcons] -row 0 -column 5 -sticky e
grid [ttk::label .header.overloaded -style Headerred.TLabel] -row 0 -column 6 -sticky e
grid [ttk::button .header.pilot -style Header.Toolbutton -command ShowCrewInfo] -row 0 -column 7 -sticky e
grid [ttk::button .header.engineer -style Header.Toolbutton -command ShowCrewInfo] -row 0 -column 8 -sticky e
grid [ttk::button .header.gunner -style Header.Toolbutton -command ShowCrewInfo] -row 0 -column 9 -sticky e
grid [ttk::button .header.talk -style Header.Toolbutton -command ShowCrewInfo] -row 0 -column 10 -sticky e
grid [ttk::button .header.repairs -style Header.Toolbutton -command ShowShipInfo] -row 0 -column 11 -sticky e
grid [ttk::button .header.upgrade -style Header.Toolbutton -command ShowShipInfo] -row 0 -column 12 -sticky e
grid [ttk::button .header.clean -style Header.Toolbutton -command ShowShipInfo] -row 0 -column 13 -sticky e
grid [ttk::button .header.crafting -style Header.Toolbutton -command ShowCrafting] -row 0 -column 14 -sticky e
pack .header -fill x
ttk::panedwindow .paned
# Game map
.paned add [ttk::frame .paned.mapframe]
set mapview [text .paned.mapframe.map -bg black -fg white -font MapFont -cursor crosshair -bd 0]
grid $mapview -sticky nwes
$mapview tag configure unvisited -background #1f2223
$mapview tag configure yellow -foreground yellow
$mapview tag configure green -foreground #4e9a06
$mapview tag configure red -foreground red
$mapview tag configure cyan -foreground cyan
$mapview tag configure lime -foreground lime
$mapview tag configure red2 -foreground #a40000
$mapview tag configure red3 -foreground #732727
$mapview tag configure green2 -foreground #73d216
$mapview tag configure gray -foreground #1f2223
$mapview tag configure black -foreground black
# Move map dialog
proc ValidateSpinbox {value currentvalue max} {
   set newvalue [regsub -all {[^0-9]} $value {}]
   if {$newvalue == "" || [expr $currentvalue + $newvalue] > $max} {
      return false
   }
   return true
}
# Move map buttons
set mframe [ttk::frame .paned.mapframe.buttons]
grid [ttk::button $mframe.show -style Toolbutton -command ShowMapButtons] -columnspan 5 -sticky we
grid [ttk::button $mframe.left -style Toolbutton -command {MoveMapButtons left}] -rowspan 3 -row 1 -column 0 -sticky ns
grid [ttk::button $mframe.nw -text {NW} -style Toolbutton -command {MoveMap nw}] -row 1 -column 1
grid [ttk::button $mframe.n -text {N} -style Toolbutton -command {MoveMap n}] -column 2 -row 1
grid [ttk::button $mframe.ne -text {NE} -style Toolbutton -command {MoveMap ne}] -column 3 -row 1
grid [ttk::button $mframe.right -style Toolbutton -command {MoveMapButtons right}] -rowspan 3 -row 1 -column 4 -sticky ns
grid [ttk::button $mframe.w -text {W} -style Toolbutton -command {MoveMap w}] -row 2 -column 1
grid [ttk::button $mframe.wait -text {...} -style Toolbutton -command {
   toplevel .movemapdialog -class Dialog
   wm title .movemapdialog {Steam Sky - move map}
   wm transient .movemapdialog .
   if {$::tcl_platform(os) == "Linux"} {
      wm attributes .movemapdialog -type dialog
   }
   pack [ttk::frame .movemapdialog.frame] -expand true -fill both
   grid [ttk::label .movemapdialog.frame.xlabel -text X:]
   grid [ttk::spinbox .movemapdialog.frame.x -from 1.0 -to 1024.0 -increment 1.0 -validate key -validatecommand {ValidateSpinbox %S %s 1024} -width 5] -row 0 -column 1
   .movemapdialog.frame.x set 1
   grid [ttk::label .movemapdialog.frame.ylabel -text Y:] -row 1
   grid [ttk::spinbox .movemapdialog.frame.y -from 1.0 -to 1024.0 -increment 1.0 -validate key -validatecommand {ValidateSpinbox %S %s 1024} -width 5] -row 1 -column 1
   .movemapdialog.frame.y set 1
   grid [ttk::button .movemapdialog.frame.moveto -text {Move map to selected location} -command {MoveMap movemapto}] -row 2 -columnspan 2 -sticky we
   set width [winfo reqwidth .movemapdialog.frame.moveto]
   grid [ttk::button .movemapdialog.frame.centeronship -text {Center map on ship} -command {MoveMap centeronship}] -row 3 -columnspan 2 -sticky we
   grid [ttk::button .movemapdialog.frame.centeronhome -text {Center map on home base} -command {MoveMap centeronhome}] -row 4 -columnspan 2 -sticky we
   grid [ttk::button .movemapdialog.frame.close -text {Close} -command {CloseDialog .movemapdialog}] -row 5 -columnspan 2 -sticky we
   set height [expr [winfo reqheight .movemapdialog.frame.close] * 6]
   wm geometry .movemapdialog [winfo reqwidth .movemapdialog.frame.moveto]x[expr [winfo reqheight .movemapdialog.frame.close] * 6]+[expr ([winfo vrootwidth .movemapdialog] - $width) / 2]+[expr ([winfo vrootheight .movemapdialog] - $height) / 2]
}] -column 2 -row 2
grid [ttk::button $mframe.e -text {E} -style Toolbutton -command {MoveMap e}] -column 3 -row 2
grid [ttk::button $mframe.sw -text {SW} -style Toolbutton -command {MoveMap sw}] -row 3 -column 1
grid [ttk::button $mframe.s -text {S} -style Toolbutton -command {MoveMap s}] -column 2 -row 3
grid [ttk::button $mframe.se -text {SE} -style Toolbutton -command {MoveMap se}] -column 3 -row 3
grid [ttk::button $mframe.hide -style Toolbutton -command HideMapButtons] -columnspan 5 -row 4 -sticky we
grid $mframe -row 0 -column 0 -sticky se
grid [ttk::frame .paned.mapframe.info] -column 0 -row 0 -sticky ne
bind .paned.mapframe.info <Enter> MoveMapInfo
grid [ttk::label .paned.mapframe.info.info] -sticky nwes
ttk::label .paned.mapframe.info.eventinfo
grid rowconfigure .paned.mapframe 0 -weight 1
grid columnconfigure .paned.mapframe 0 -weight 1
# Last messages
.paned add [ttk::frame .paned.controls]
grid [ttk::frame .paned.controls.messages] -sticky w
pack [ttk::scrollbar .paned.controls.messages.scroll -orient vertical -command [list .paned.controls.messages.view yview]] -side right -fill y
set messagesview [text .paned.controls.messages.view -wrap word -yscrollcommand [list .paned.controls.messages.scroll set]]
$messagesview tag configure yellow -foreground yellow
$messagesview tag configure green -foreground #4e9a06
$messagesview tag configure red -foreground red
$messagesview tag configure cyan -foreground cyan
$messagesview tag configure blue -foreground #3465a4
$messagesview tag configure gray -foreground {dim gray}
pack $messagesview -side top -fill both
bind .paned.controls <Configure> {
   $messagesview configure -height [expr [winfo height .paned.controls] / [font metrics InterfaceFont -linespace]]
}
# Movement buttons
set bframe [ttk::frame .paned.controls.buttons]
grid $bframe -row 0 -column 1 -sticky nw
grid [ttk::combobox $bframe.speed -state readonly -values [list {Full stop} {Quarted speed} {Half speed} {Full speed}] -width 10] -columnspan 2 -sticky we
grid [ttk::button $bframe.moveto -text {Move to} -command {MoveShip moveto}] -row 0 -column 2
grid [ttk::button $bframe.nw -text {NW} -command {MoveShip nw}] -row 1
grid [ttk::button $bframe.n -text {N} -command {MoveShip n}] -column 1 -row 1
grid [ttk::button $bframe.ne -text {NE} -command {MoveShip ne}] -column 2 -row 1
grid [ttk::button $bframe.w -text {W} -command {MoveShip w}] -row 2
grid [ttk::button $bframe.wait -text {Wait} -command {MoveShip waitormove}] -column 1 -row 2
grid [ttk::button $bframe.e -text {E} -command {MoveShip e}] -column 2 -row 2
grid [ttk::button $bframe.sw -text {SW} -command {MoveShip sw}] -row 3
grid [ttk::button $bframe.s -text {S} -command {MoveShip s}] -column 1 -row 3
grid [ttk::button $bframe.se -text {SE} -command {MoveShip se}] -column 2 -row 3
grid columnconfigure .paned.controls .paned.controls.messages -weight 1
pack .paned -expand true -fill both
update
