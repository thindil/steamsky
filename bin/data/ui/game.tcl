# Game menu
menu .gamemenu -title {Steam Sky - menu}
proc InvokeMenu {index} {
   if {[winfo class [focus]] != {TEntry}} {
      .gamemenu invoke $index
   }
}
# Orders menu
menu .orders -tearoff false
# Destination menu
menu .destination -tearoff false
pack [ttk::frame .gameframe] -fill both -expand true
# Game header
ttk::frame .gameframe.header
grid [ttk::menubutton .gameframe.header.menubutton -text {Menu} -menu .gamemenu] -sticky w
ttk::button .gameframe.header.closebutton -text {Close [Escape]} -command {ShowSkyMap}
grid [ttk::label .gameframe.header.time -text {1600-03-01}] -row 0 -column 2
tooltip::tooltip .gameframe.header.time {The game time}
grid columnconfigure .gameframe.header .gameframe.header.time -weight 1
grid [ttk::label .gameframe.header.nofuel -style Headerred.TLabel -font InterfaceIcons] -row 0 -column 3 -padx 3
grid [ttk::label .gameframe.header.nofood -style Headerred.TLabel -font InterfaceIcons] -row 0 -column 4 -padx 3
grid [ttk::label .gameframe.header.nodrink -style Headerred.TLabel -font InterfaceIcons] -row 0 -column 5 -padx 3
grid [ttk::label .gameframe.header.overloaded -style Headerred.TLabel] -row 0 -column 6 -padx 3
grid [ttk::label .gameframe.header.pilot -style Headerred.TLabel -font InterfaceIcons] -row 0 -column 7 -padx 3
grid [ttk::label .gameframe.header.engineer -style Headerred.TLabel -font InterfaceIcons] -row 0 -column 8 -padx 3
grid [ttk::label .gameframe.header.gunner -style Headerred.TLabel -font InterfaceIcons] -row 0 -column 9 -padx 3
grid [ttk::label .gameframe.header.talk -style Headerred.TLabel -font InterfaceIcons] -row 0 -column 10 -padx 3
grid [ttk::label .gameframe.header.repairs -style Headerred.TLabel -font InterfaceIcons] -row 0 -column 11 -padx 3
grid [ttk::label .gameframe.header.upgrade -style Headerred.TLabel -font InterfaceIcons] -row 0 -column 12 -padx 3
grid [ttk::label .gameframe.header.clean -style Headerred.TLabel -font InterfaceIcons] -row 0 -column 13 -padx 3
grid [ttk::label .gameframe.header.crafting -style Headerred.TLabel -font InterfaceIcons] -row 0 -column 14 -padx 3
grid .gameframe.header -sticky we -padx 5
ttk::panedwindow .gameframe.paned
# Game map
.gameframe.paned add [ttk::frame .gameframe.paned.mapframe]
set mapview [text .gameframe.paned.mapframe.map -bg black -wrap none -fg white -font MapFont -cursor crosshair -bd 0]
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
set mframe [ttk::frame .gameframe.paned.mapframe.buttons]
grid [ttk::button $mframe.show -style Toolbutton -command ShowMapButtons] -columnspan 5 -sticky we
grid [ttk::button $mframe.left -style Toolbutton -command {MoveMapButtons left}] -rowspan 3 -row 1 -column 0 -sticky ns
grid [ttk::button $mframe.nw -text {NW} -style Toolbutton -command {MoveMap nw}] -row 1 -column 1
grid [ttk::button $mframe.n -text {N} -style Toolbutton -command {MoveMap n}] -column 2 -row 1
grid [ttk::button $mframe.ne -text {NE} -style Toolbutton -command {MoveMap ne}] -column 3 -row 1
grid [ttk::button $mframe.right -style Toolbutton -command {MoveMapButtons right}] -rowspan 3 -row 1 -column 4 -sticky ns
grid [ttk::button $mframe.w -text {W} -style Toolbutton -command {MoveMap w}] -row 2 -column 1
grid [ttk::button $mframe.wait -text {...} -style Toolbutton -command {
   if {[winfo ismapped .gameframe.paned.mapframe] == "0"} {
      return
   }
   if {[tk busy status .gameframe.paned] == 0} {
      tk busy .gameframe.header
      tk busy .gameframe.paned
   }
   ttk::frame .gameframe.movemapdialog
   grid [ttk::label .gameframe.movemapdialog.xlabel -text X:]
   grid [ttk::spinbox .gameframe.movemapdialog.x -from 1.0 -to 1024.0 -increment 1.0 -validate key -validatecommand {ValidateSpinbox %S %s 1024} -width 5] -row 0 -column 1
   .gameframe.movemapdialog.x set 1
   grid [ttk::label .gameframe.movemapdialog.ylabel -text Y:] -row 1
   grid [ttk::spinbox .gameframe.movemapdialog.y -from 1.0 -to 1024.0 -increment 1.0 -validate key -validatecommand {ValidateSpinbox %S %s 1024} -width 5] -row 1 -column 1
   .gameframe.movemapdialog.y set 1
   grid [ttk::button .gameframe.movemapdialog.moveto -text {Move map to selected location} -command {MoveMap movemapto}] -row 2 -columnspan 2 -sticky we
   set width [expr [winfo reqwidth .gameframe.movemapdialog.moveto] + 5]
   grid [ttk::button .gameframe.movemapdialog.centeronship -text {Center map on ship} -command {MoveMap centeronship} -underline 0] -row 3 -columnspan 2 -sticky we
   grid [ttk::button .gameframe.movemapdialog.centeronhome -text {Center map on home base} -command {MoveMap centeronhome} -underline 1] -row 4 -columnspan 2 -sticky we
   grid [ttk::button .gameframe.movemapdialog.close -text {Close} -command {CloseDialog .gameframe.movemapdialog}] -row 5 -columnspan 2 -sticky we
   set height [expr [winfo reqheight .gameframe.movemapdialog.close] * 6]
   grid .gameframe.movemapdialog -row 1 -column 0
   focus .gameframe.movemapdialog.close
   bind .gameframe.movemapdialog <Escape> {InvokeButton .gameframe.movemapdialog.close}
   bind .gameframe.movemapdialog <Return> {InvokeButton .gameframe.movemapdialog.moveto}
   bind .gameframe.movemapdialog <Alt-c> {InvokeButton .gameframe.movemapdialog.centeronship}
   bind .gameframe.movemapdialog <Alt-e> {InvokeButton .gameframe.movemapdialog.centeronhome}
}] -column 2 -row 2
grid [ttk::button $mframe.e -text {E} -style Toolbutton -command {MoveMap e}] -column 3 -row 2
grid [ttk::button $mframe.sw -text {SW} -style Toolbutton -command {MoveMap sw}] -row 3 -column 1
grid [ttk::button $mframe.s -text {S} -style Toolbutton -command {MoveMap s}] -column 2 -row 3
grid [ttk::button $mframe.se -text {SE} -style Toolbutton -command {MoveMap se}] -column 3 -row 3
grid [ttk::button $mframe.hide -style Toolbutton -command HideMapButtons] -columnspan 5 -row 4 -sticky we
grid $mframe -row 0 -column 0 -sticky se
# Map info frame
grid [ttk::frame .gameframe.paned.mapframe.info -relief solid -padding 5 -style MapInfo.TFrame -borderwidth 1] -column 0 -row 0 -sticky ne
bind .gameframe.paned.mapframe.info <Enter> MoveMapInfo
grid [ttk::label .gameframe.paned.mapframe.info.info -style MapInfo.TLabel] -sticky nwes
ttk::label .gameframe.paned.mapframe.info.eventinfo -wraplength 225
grid rowconfigure .gameframe.paned.mapframe 0 -weight 1
grid columnconfigure .gameframe.paned.mapframe 0 -weight 1
# Last messages
.gameframe.paned add [ttk::frame .gameframe.paned.controls]
grid [ttk::frame .gameframe.paned.controls.messages] -sticky w
pack [ttk::scrollbar .gameframe.paned.controls.messages.scroll -orient vertical -command [list .gameframe.paned.controls.messages.view yview]] -side right -fill y
set messagesview [text .gameframe.paned.controls.messages.view -wrap word -yscrollcommand [list .gameframe.paned.controls.messages.scroll set]]
$messagesview tag configure yellow -foreground yellow
$messagesview tag configure green -foreground #4e9a06
$messagesview tag configure red -foreground red
$messagesview tag configure cyan -foreground cyan
$messagesview tag configure blue -foreground #3465a4
$messagesview tag configure gray -foreground {dim gray}
pack $messagesview -side top -fill both
bind .gameframe.paned.controls <Configure> {
   $messagesview configure -height [expr [winfo height .gameframe.paned.controls] / [font metrics InterfaceFont -linespace]]
}
# Movement buttons
set bframe [ttk::frame .gameframe.paned.controls.buttons]
grid $bframe -row 0 -column 1 -sticky nw
grid [ttk::combobox $bframe.speed -state readonly -values [list {Full stop} {Quarted speed} {Half speed} {Full speed}] -width 10] -columnspan 2 -sticky we
grid [ttk::button $bframe.moveto -text {Move to} -command {MoveShip moveto} -width 0] -row 0 -column 2
grid [ttk::button $bframe.nw -text {NW} -command {MoveShip nw} -width -6] -row 1
grid [ttk::button $bframe.n -text {N} -command {MoveShip n} -width -6] -column 1 -row 1
grid [ttk::button $bframe.ne -text {NE} -command {MoveShip ne} -width -6] -column 2 -row 1
grid [ttk::button $bframe.w -text {W} -command {MoveShip w} -width -6] -row 2
grid [ttk::button $bframe.wait -text {Wait} -command {MoveShip waitormove} -width -6] -column 1 -row 2
grid [ttk::button $bframe.e -text {E} -command {MoveShip e} -width -6] -column 2 -row 2
grid [ttk::button $bframe.sw -text {SW} -command {MoveShip sw} -width -6] -row 3
grid [ttk::button $bframe.s -text {S} -command {MoveShip s} -width -6] -column 1 -row 3
grid [ttk::button $bframe.se -text {SE} -command {MoveShip se} -width -6] -column 2 -row 3
grid columnconfigure .gameframe.paned.controls .gameframe.paned.controls.messages -weight 1
grid .gameframe.paned -sticky nwes
grid columnconfigure .gameframe .gameframe.paned -weight 1
grid rowconfigure .gameframe .gameframe.paned -weight 1
update
