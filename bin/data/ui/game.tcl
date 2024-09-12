# Copyright (c) 2020-2024 Bartek thindil Jasicki
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

pack [ttk::frame .gameframe -style Main.TFrame] -fill both -expand true
# Game header
ttk::frame .gameframe.header
grid [ttk::button .gameframe.header.menubutton -style Small.TButton \
   -command ShowGameMenu] -sticky w
tooltip::tooltip .gameframe.header.menubutton \
   "The main game menu. Show info about the ships,\nits crew and allow to quit the game"
ttk::button .gameframe.header.closebutton -style Small.TButton \
   -command {ShowSkyMap}
tooltip::tooltip .gameframe.header.closebutton {Back to the game map [Escape key]}
grid [ttk::label .gameframe.header.time -text {1600-03-01}] -row 0 -column 2
tooltip::tooltip .gameframe.header.time {The game time}
grid columnconfigure .gameframe.header .gameframe.header.time -weight 1
grid [ttk::label .gameframe.header.fuel] -row 0 -column 3 -padx 3
grid [ttk::label .gameframe.header.food] -row 0 -column 4 -padx 3
grid [ttk::label .gameframe.header.drinks] -row 0 -column 5 -padx 3
grid [ttk::label .gameframe.header.overloaded] -row 0 -column 6 -padx 3
grid [ttk::label .gameframe.header.pilot] -row 0 -column 7 -padx 3
grid [ttk::label .gameframe.header.engineer] -row 0 -column 8 -padx 3
grid [ttk::label .gameframe.header.gunner] -row 0 -column 9 -padx 3
grid [ttk::label .gameframe.header.talk] -row 0 -column 10 -padx 3
grid [ttk::label .gameframe.header.repairs] -row 0 -column 11 -padx 3
grid [ttk::label .gameframe.header.upgrade] -row 0 -column 12 -padx 3
grid [ttk::label .gameframe.header.clean] -row 0 -column 13 -padx 3
grid [ttk::label .gameframe.header.crafting] -row 0 -column 14 -padx 3
grid .gameframe.header -sticky we -padx 5 -pady {5 0}
ttk::panedwindow .gameframe.paned
# Game map
.gameframe.paned add [ttk::frame .gameframe.paned.mapframe]
set mapview [text .gameframe.paned.mapframe.map \
   -bg [set ttk::theme::[ttk::style theme use]::colors(-black)] -wrap none \
   -fg white -font MapFont -cursor crosshair -bd 0]
grid $mapview -sticky nwes
$mapview tag configure unvisited -background [ttk::style lookup Map -unvisited]
$mapview tag configure yellow -foreground [ttk::style lookup Map -yellow]
$mapview tag configure green -foreground [ttk::style lookup Map -green]
$mapview tag configure red -foreground [ttk::style lookup Map -red]
$mapview tag configure cyan -foreground [ttk::style lookup Map -cyan]
$mapview tag configure lime -foreground [ttk::style lookup Map -lime]
$mapview tag configure red2 -foreground [ttk::style lookup Map -red2]
$mapview tag configure red3 -foreground [ttk::style lookup Map -red3]
$mapview tag configure green2 -foreground [ttk::style lookup Map -green2]
$mapview tag configure gray -foreground [ttk::style lookup Map -gray]
$mapview tag configure black -foreground [ttk::style lookup Map -black]
proc ValidateSpinbox {widget value button} {
   if {$value == ""} {
      if {$button != ""} {
         $button configure -state disabled
      }
      return true
   }
   if {$button != ""} {
      $button configure -state normal
   }
   set newvalue [regsub -all {[^0-9]} $value {}]
   set minvalue [$widget cget -from]
   if {$newvalue == ""} {
      $widget set $minvalue
      return false
   }
   if {$newvalue < $minvalue} {
      $widget set $minvalue
      return true
   }
   set maxvalue [$widget cget -to]
   if {$newvalue > $maxvalue} {
      $widget set $maxvalue
      return true
   }
   $widget set $newvalue
   $widget icursor end
   return true
}
# Move map buttons
set mframe [ttk::frame .gameframe.paned.mapframe.buttons]
grid [ttk::button $mframe.show -style Toolbutton -command ShowMapButtons] \
   -columnspan 5 -sticky we
tooltip::tooltip $mframe.show {Show the map manipulation buttons}
grid [ttk::button $mframe.left -style Map.Toolbutton \
   -command {MoveMapButtons left}] -rowspan 3 -row 1 -column 0 -sticky ns
tooltip::tooltip $mframe.left {Move map buttons to the left corner}
grid [ttk::button $mframe.nw -style Map.Toolbutton -command {MoveMap nw}] \
   -row 1 -column 1
tooltip::tooltip $mframe.nw {Move map up and left}
grid [ttk::button $mframe.n -style Map.Toolbutton -command {MoveMap n}] \
   -column 2 -row 1
tooltip::tooltip $mframe.n {Move map up}
grid [ttk::button $mframe.ne -style Map.Toolbutton -command {MoveMap ne}] \
   -column 3 -row 1
tooltip::tooltip $mframe.ne {Move map up and right}
grid [ttk::button $mframe.right -style Map.Toolbutton \
   -command {MoveMapButtons right}] -rowspan 3 -row 1 -column 4 -sticky ns
tooltip::tooltip $mframe.right {Move map buttons to the right corner}
grid [ttk::button $mframe.w -style Map.Toolbutton -command {MoveMap w}] \
   -row 2 -column 1
tooltip::tooltip $mframe.w {Move map left}
grid [ttk::button $mframe.wait -style Map.Toolbutton -command {
   if {[winfo ismapped .gameframe.paned.mapframe] == "0"} {
      return
   }
   if {[winfo exists .gameframe.movemapdialog]} {
      CloseDialog .gameframe.movemapdialog
      return
   }
   tk busy .gameframe.header
   tk busy .gameframe.paned
   ttk::frame .gameframe.movemapdialog -style Dialog.TFrame
   grid [ttk::label .gameframe.movemapdialog.header -text {Move map} \
      -style Header.TLabel] -sticky we -columnspan 2
   grid [ttk::label .gameframe.movemapdialog.xlabel -text X: -takefocus 0] \
      -pady {5 0}
   grid [ttk::spinbox .gameframe.movemapdialog.x -from 1 -to 1024 \
      -validate key \
      -validatecommand {ValidateSpinbox %W %P .gameframe.movemapdialog.moveto} \
      -width 5] -row 1 -column 1 -pady {5 0}
   .gameframe.movemapdialog.x set 1
   grid [ttk::label .gameframe.movemapdialog.ylabel -text Y: -takefocus 0] \
      -row 2
   grid [ttk::spinbox .gameframe.movemapdialog.y -from 1 -to 1024 \
      -validate key \
      -validatecommand {ValidateSpinbox %W %P .gameframe.movemapdialog.moveto} \
      -width 5] -row 2 -column 1
   .gameframe.movemapdialog.y set 1
   grid [ttk::button .gameframe.movemapdialog.moveto \
      -text {Move map to selected location} -command {MoveMap movemapto} \
      -underline 0] -row 3 -columnspan 2 -sticky we -padx 5
   grid [ttk::button .gameframe.movemapdialog.centeronship \
      -text {Center map on ship} -command {MoveMap centeronship} -underline 0] \
      -row 4 -columnspan 2 -sticky we -padx 5
   grid [ttk::button .gameframe.movemapdialog.centeronhome \
      -text {Center map on home base} -command {MoveMap centeronhome} \
      -underline 1] -row 5 -columnspan 2 -sticky we -padx 5
   grid [ttk::button .gameframe.movemapdialog.close -text {Close} \
      -command {CloseDialog .gameframe.movemapdialog}] -row 6 -columnspan 2 \
      -sticky we -padx 5 -pady {0 5}
   place .gameframe.movemapdialog -in .gameframe -relx 0.3 -rely 0.25
   focus .gameframe.movemapdialog.close
   foreach widget [winfo children .gameframe.movemapdialog] {
      bind $widget <Alt-m> {.gameframe.movemapdialog.moveto invoke;break}
      bind $widget <Alt-c> {.gameframe.movemapdialog.centeronship invoke;break}
      bind $widget <Alt-e> {.gameframe.movemapdialog.centeronhome invoke;break}
      bind $widget <Escape> {.gameframe.movemapdialog.close invoke;break}
   }
   bind .gameframe.movemapdialog.close <Tab> \
      {focus .gameframe.movemapdialog.x;break}
}] -column 2 -row 2
tooltip::tooltip $mframe.wait {Show more the map's options}
grid [ttk::button $mframe.e -style Map.Toolbutton -command {MoveMap e}] \
   -column 3 -row 2
tooltip::tooltip $mframe.e {Move map right}
grid [ttk::button $mframe.sw -style Map.Toolbutton -command {MoveMap sw}] \
   -row 3 -column 1
tooltip::tooltip $mframe.sw {Move map down and left}
grid [ttk::button $mframe.s -style Map.Toolbutton -command {MoveMap s}] \
   -column 2 -row 3
tooltip::tooltip $mframe.s {Move map down}
grid [ttk::button $mframe.se -style Map.Toolbutton -command {MoveMap se}] \
   -column 3 -row 3
tooltip::tooltip $mframe.se {Move map down and right}
grid [ttk::button $mframe.hide -style Map.Toolbutton -command HideMapButtons] \
   -columnspan 5 -row 4 -sticky we
tooltip::tooltip $mframe.hide {Hide the map manipulation buttons}
grid $mframe -row 0 -column 0 -sticky se
# Map info frame
set mapinfo [text .gameframe.paned.mapframe.info -wrap word -height 10 \
   -width 20 -background [ttk::style lookup MapInfo -background] \
   -relief ridge -borderwidth 3 -padx 5]
$mapinfo tag configure yellow -foreground [ttk::style lookup Map -yellow]
$mapinfo tag configure green -foreground [ttk::style lookup Map -green]
$mapinfo tag configure red -foreground [ttk::style lookup Map -red]
$mapinfo tag configure cyan -foreground [ttk::style lookup Map -cyan]
$mapinfo tag configure lime -foreground [ttk::style lookup Map -lime]
$mapinfo tag configure red2 -foreground [ttk::style lookup Map -red2]
$mapinfo tag configure red3 -foreground [ttk::style lookup Map -red3]
$mapinfo tag configure green2 -foreground [ttk::style lookup Map -green2]
$mapinfo tag configure pink -foreground [ttk::style lookup Map -pink]
$mapinfo tag configure yellow2 -foreground [ttk::style lookup Map -goldenyellow]
$mapinfo tag configure underline -font UnderlineFont
grid $mapinfo -column 0 -row 0 -sticky ne
bind .gameframe.paned.mapframe.info <Enter> MoveMapInfo
grid rowconfigure .gameframe.paned.mapframe 0 -weight 1
grid columnconfigure .gameframe.paned.mapframe 0 -weight 1
# Last messages
.gameframe.paned add [ttk::frame .gameframe.paned.controls]
grid [ttk::frame .gameframe.paned.controls.messages -style LastMessages.TFrame] \
   -sticky we
pack [ttk::scrollbar .gameframe.paned.controls.messages.scroll -orient vertical \
   -command [list .gameframe.paned.controls.messages.view yview]] -side right \
   -fill y -padx {0 5} -pady 5
set messagesview [text .gameframe.paned.controls.messages.view -wrap word \
   -yscrollcommand [list .gameframe.paned.controls.messages.scroll set]]
$messagesview tag configure yellow -foreground \
   [ttk::style lookup Messages -yellow]
$messagesview tag configure green -foreground \
   [ttk::style lookup Messages -green]
$messagesview tag configure red -foreground \
   [ttk::style lookup Messages -red]
$messagesview tag configure cyan -foreground \
   [ttk::style lookup Messages -cyan]
$messagesview tag configure blue -foreground \
   [ttk::style lookup Messages -blue]
$messagesview tag configure gray -foreground \
   [ttk::style lookup Messages -gray]
pack $messagesview -side top -fill both -padx 5 -pady 5
tooltip::tooltip $messagesview \
   "The last game messages. You can see more of them\nIn Menu->Last messages screen"
::autoscroll::autoscroll .gameframe.paned.controls.messages.scroll
bind .gameframe.paned.controls <Configure> {
   $messagesview configure -height [expr \
      [winfo height .gameframe.paned.controls] / [font metrics InterfaceFont \
      -linespace]]
}
# Movement buttons
set bframe [ttk::frame .gameframe.paned.controls.buttons]
grid $bframe -row 0 -column 1 -sticky nw
grid [ttk::frame $bframe.box] -columnspan 3 -sticky we
grid [ttk::button $bframe.box.orders -command {ShowOrders} -text {Ship Orders}]
tooltip::tooltip $bframe.box.orders "Show available orders for your ship."
grid [ttk::combobox $bframe.box.speed -state readonly -values [list {Full stop} \
   {Quarted speed} {Half speed} {Full speed}] -width 10] -sticky we
tooltip::tooltip $bframe.box.speed \
   "Set speed for your ship. The faster you move,\nthe more fuel used. But faster movement has\nbigger chance to evade enemies."
grid [ttk::button $bframe.box.moveto -command {MoveShip moveto} \
   -style Move.TButton] -row 0 -column 1
tooltip::tooltip $bframe.box.moveto "Auto move your ship to its destination"
grid [ttk::button $bframe.nw -command {MoveShip nw} -style Move.TButton] \
   -row 1 -sticky we
tooltip::tooltip $bframe.nw "Move ship up and left"
grid [ttk::button $bframe.n -command {MoveShip n} -style Move.TButton] \
   -column 1 -row 1 -sticky we
tooltip::tooltip $bframe.n "Move ship up"
grid [ttk::button $bframe.ne -command {MoveShip ne} -style Move.TButton] \
   -column 2 -row 1 -sticky we
tooltip::tooltip $bframe.ne "Move ship up and right"
grid [ttk::button $bframe.w -command {MoveShip w} -style Move.TButton] -row 2 \
   -sticky we
tooltip::tooltip $bframe.w "Move ship left"
grid [ttk::button $bframe.wait -command {MoveShip waitormove} \
   -style Move.TButton] -column 1 -row 2 -sticky we
grid [ttk::button $bframe.e -command {MoveShip e} -style Move.TButton] \
   -column 2 -row 2 -sticky we
tooltip::tooltip $bframe.e "Move ship right"
grid [ttk::button $bframe.sw -command {MoveShip sw} -style Move.TButton] \
   -row 3 -sticky we
tooltip::tooltip $bframe.sw "Move ship down and left"
grid [ttk::button $bframe.s -command {MoveShip s} -style Move.TButton] \
   -column 1 -row 3 -sticky we
tooltip::tooltip $bframe.s "Move ship down"
grid [ttk::button $bframe.se -command {MoveShip se} -style Move.TButton] \
   -column 2 -row 3 -sticky we
tooltip::tooltip $bframe.se "Move ship down and right"
grid columnconfigure .gameframe.paned.controls \
   .gameframe.paned.controls.messages -weight 1
grid .gameframe.paned -sticky nwes -padx 5 -pady {0 5}
grid columnconfigure .gameframe .gameframe.paned -weight 1
grid rowconfigure .gameframe .gameframe.paned -weight 1
update
