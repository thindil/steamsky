# Copyright (c) 2020-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

ttk::frame .gameframe.paned.optionsframe
set optionscanvas [canvas .gameframe.paned.optionsframe.canvas -yscrollcommand [list .gameframe.paned.optionsframe.scrolly set] -xscrollcommand [list .gameframe.paned.optionsframe.scrollx set]]
pack [ttk::scrollbar .gameframe.paned.optionsframe.scrolly -orient vertical -command [list $optionscanvas yview]] -side right -fill y
pack $optionscanvas -side top -fill both
pack [ttk::scrollbar .gameframe.paned.optionsframe.scrollx -orient horizontal -command [list $optionscanvas xview]] -fill x
set optionsframe [ttk::frame $optionscanvas.options]
grid [ttk::notebook $optionsframe.notebook] -sticky nwes
# General options
set goptions [ttk::frame $optionsframe.notebook.general]
grid [ttk::label $goptions.lbl1 -text {Auto rest when crew is tired:}] -sticky w
tooltip::tooltip $goptions.lbl1 {Wait for crew is rested when pilot or engineer are too tired to work.}
grid [ttk::checkbutton $goptions.autorest] -row 0 -column 1 -sticky w
tooltip::tooltip $goptions.autorest {Wait for crew is rested when pilot or engineer are too tired to work.}
grid [ttk::label $goptions.lbl2 -text {Default speed after undocking:}] -sticky w
tooltip::tooltip $goptions.lbl2 {Default speed of ship after undock from base.}
grid [ttk::combobox $goptions.speed -state readonly -values [list {Full stop} {Quarted speed} {Half speed} {Full speed}] -width 10] -row 1 -column 1 -sticky w
tooltip::tooltip $goptions.speed {Default speed of ship after undock from base.}
grid [ttk::label $goptions.lbl3 -text {Auto center map after set destination:}] -sticky w
tooltip::tooltip $goptions.lbl3 {After set destination for ship, center map on ship.}
grid [ttk::checkbutton $goptions.autocenter] -row 2 -column 1 -sticky w
tooltip::tooltip $goptions.autocenter {After set destination for ship, center map on ship.}
grid [ttk::label $goptions.lbl4 -text {Auto set base after finished mission:}] -sticky w
tooltip::tooltip $goptions.lbl4 "After finished mission, set skybase from which\nmission was taken as a destination for ship."
grid [ttk::checkbutton $goptions.autoreturn] -row 3 -column 1 -sticky w
tooltip::tooltip $goptions.autoreturn "After finished mission, set skybase from\nwhich mission was taken as a destination for ship."
grid [ttk::label $goptions.lbl5 -text {Auto finish missions:}] -sticky w
tooltip::tooltip $goptions.lbl5 {Auto finish missions when ship is near corresponding skybase.}
grid [ttk::checkbutton $goptions.autofinish] -row 4 -column 1 -sticky w
tooltip::tooltip $goptions.autofinish {Auto finish missions when ship is near corresponding skybase.}
grid [ttk::label $goptions.lbl6 -text {Auto ask for bases:}] -sticky w
tooltip::tooltip $goptions.lbl6 {Auto ask for bases when ship end docking to bases.}
grid [ttk::checkbutton $goptions.autoaskforbases] -row 5 -column 1 -sticky w
tooltip::tooltip $goptions.autoaskforbases {Auto ask for bases when ship end docking to bases.}
grid [ttk::label $goptions.lbl7 -text {Auto ask for events:}] -sticky w
tooltip::tooltip $goptions.lbl7 {Auto ask for events when ship end docking to bases.}
grid [ttk::checkbutton $goptions.autoaskforevents] -row 6 -column 1 -sticky w
tooltip::tooltip $goptions.autoaskforevents {Auto ask for events when ship end docking to bases.}
grid [ttk::label $goptions.lbl8 -text {Low level of fuel:}] -sticky w
tooltip::tooltip $goptions.lbl8 "Amount of fuel below which you will see warning about\nlow level of. Enter value between 1 and 10 000."
grid [ttk::spinbox $goptions.fuel -from 1 -to 10000 -validate key -validatecommand {ValidateSpinbox %W %P} -width 5] -row 7 -column 1 -sticky w
tooltip::tooltip $goptions.fuel "Amount of fuel below which you will see warning about\nlow level of. Enter value between 1 and 10 000."
grid [ttk::label $goptions.lbl9 -text {Low level of drinks:}] -sticky w
tooltip::tooltip $goptions.lbl9 "Amount of drinks below which you will see warning\nabout low level of. Enter value between 1 and 10 000."
grid [ttk::spinbox $goptions.drinks -from 1 -to 10000 -validate key -validatecommand {ValidateSpinbox %W %P} -width 5] -row 8 -column 1 -sticky w
tooltip::tooltip $goptions.drinks "Amount of drinks below which you will see warning\nabout low level of. Enter value between 1 and 10 000."
grid [ttk::label $goptions.lbl10 -text {Low level of food:}] -sticky w
tooltip::tooltip $goptions.lbl10 "Amount of food below which you will see warning\nabout low level of. Enter value between 1 and 10 000."
grid [ttk::spinbox $goptions.food -from 1 -to 10000 -validate key -validatecommand {ValidateSpinbox %W %P} -width 5] -row 9 -column 1 -sticky w
tooltip::tooltip $goptions.food "Amount of food below which you will see warning\nabout low level of. Enter value between 1 and 10 000."
grid [ttk::label $goptions.lbl11 -text {Stop auto movement:}] -sticky w
tooltip::tooltip $goptions.lbl11 "Set when auto move ship should stop: never,\non meet any ship, on meet friendly ship or\non meet enemy ship."
grid [ttk::combobox $goptions.automovestop -state readonly -values [list {Never} {Any ship} {Friendly ship} {Enemy ship}] -width 10] -row 10 -column 1 -sticky w
tooltip::tooltip $goptions.automovestop "Set when auto move ship should stop: never,\non meet any ship, on meet friendly ship or\non meet enemy ship."
grid [ttk::label $goptions.lbl12 -text {Messages limit:}] -sticky w
tooltip::tooltip $goptions.lbl12 "Amount of messages stored in game. If new message arrive\nwhen limit is reached, oldest message will be deleted. Enter\nvalue between 10 and 5000."
grid [ttk::spinbox $goptions.messageslimit -from 10 -to 5000 -validate key -validatecommand {ValidateSpinbox %W %P} -width 5] -row 11 -column 1 -sticky w
tooltip::tooltip $goptions.messageslimit "Amount of messages stored in game. If new message arrive\nwhen limit is reached, oldest message will be deleted. Enter\nvalue between 10 and 5000."
grid [ttk::label $goptions.lbl13 -text {Saved messages:}] -sticky w
tooltip::tooltip $goptions.lbl13 "Maximum amount of last messages saved to file.\nEnter value between 5 and 200."
grid [ttk::spinbox $goptions.savedmessages -from 5 -to 200 -validate key -validatecommand {ValidateSpinbox %W %P} -width 5] -row 12 -column 1 -sticky w
tooltip::tooltip $goptions.savedmessages "Maximum amount of last messages saved to file.\nEnter value between 5 and 200."
grid [ttk::label $goptions.lbl14 -text {Messages order:}] -sticky w
tooltip::tooltip $goptions.lbl14 "In what order show messages in game. If Older first\nwill be select, then older messages will appear at top\nof the lists. Otherwise newer messages will be at top."
grid [ttk::combobox $goptions.messagesorder -state readonly -values [list {Older messages first} {Newer messages first}] -width 16] -row 13 -column 1 -sticky w
tooltip::tooltip $goptions.messagesorder "In what order show messages in game. If Older first\nwill be select, then older messages will appear at top\nof the lists. Otherwise newer messages will be at top."
grid [ttk::label $goptions.lbl15 -text {Autosave game:}] -sticky w
tooltip::tooltip $goptions.lbl15 {How often game should be automatically saved to disk.}
grid [ttk::combobox $goptions.autosave -state readonly -values [list {Never} {After dock to base} {After undock from base} {Every game day} {Every game month} {Every game year}] -width 18] -row 14 -column 1 -sticky w
tooltip::tooltip $goptions.autosave {How often game should be automatically saved to disk.}
$optionsframe.notebook add $goptions -text {General}
# Movement keys options
set specialkey {}
# Set proper shortcut, validate it and check if it is not set somewhere
proc SetShortcut {field key} {
   global specialkey moveoptions menuoptions mapoptions ioptions
   set fields [list $moveoptions.upleft $moveoptions.up $moveoptions.upright $moveoptions.left $moveoptions.wait $moveoptions.right $moveoptions.downleft $moveoptions.down $moveoptions.downright $moveoptions.moveto $moveoptions.fullstop $moveoptions.quarterspeed $moveoptions.halfspeed $moveoptions.fullspeed $menuoptions.shipinfo $menuoptions.orders $menuoptions.crafts $menuoptions.messages $menuoptions.knowledge $menuoptions.waitorders $menuoptions.gamestats $menuoptions.help $menuoptions.gameoptions $menuoptions.quit $menuoptions.resign $menuoptions.menu $mapoptions.center $mapoptions.centerhomebase $mapoptions.mapleft $mapoptions.mapright $mapoptions.mapup $mapoptions.mapdown $mapoptions.mapupleft $mapoptions.mapupright $mapoptions.mapdownleft $mapoptions.mapdownright $mapoptions.cursorupleft $mapoptions.cursorup $mapoptions.cursorupright $mapoptions.cursorleft $mapoptions.cursorright $mapoptions.cursordownleft $mapoptions.cursordown $mapoptions.cursordownright $mapoptions.clickmouse $mapoptions.zoomin $mapoptions.zoomout $mapoptions.mapoptions $ioptions.fullscreenkey]
   if {$key == "Control_L" || $key == "Control_R" || $key == "Alt_L" || $key == "Alt_R" || $key == "Shift_L" || $key == "Shift_R"} {
      set specialkey [string range $key 0 [expr [string length $key] - 3]]
      return
   }
   if {$specialkey == "Shift" && [string length $key] == 1} {
      set key "[string tolower $key]"
   }
   if {$specialkey != {}} {
      set value "$specialkey-$key"
   } else {
      set value "$key"
   }
   foreach keyentry $fields {
      if {$keyentry != $field && [$keyentry get] == $value} {
         return
      }
   }
   $field delete 0 end
   if {$specialkey != {}} {
      $field insert end $specialkey
      $field insert end -
      $field insert end $key
   } else {
      $field insert end $key
   }
   set specialkey {}
}
set moveoptions [ttk::frame $optionsframe.notebook.movement]
grid [ttk::label $moveoptions.lbl1 -text {Move ship up/left:}] -sticky w
grid [ttk::entry $moveoptions.upleft -width 15] -row 0 -column 1 -sticky w
bind $moveoptions.upleft <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl2 -text {Move ship up:}] -sticky w
grid [ttk::entry $moveoptions.up -width 15] -row 1 -column 1 -sticky w
bind $moveoptions.up <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl3 -text {Move ship up/right:}] -sticky w
grid [ttk::entry $moveoptions.upright -width 15] -row 2 -column 1 -sticky w
bind $moveoptions.upright <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl4 -text {Move ship left:}] -sticky w
grid [ttk::entry $moveoptions.left -width 15] -row 3 -column 1 -sticky w
bind $moveoptions.left <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl5 -text {Wait 1 minute or move 1 field:}] -sticky w
grid [ttk::entry $moveoptions.wait -width 15] -row 4 -column 1 -sticky w
bind $moveoptions.wait <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl6 -text {Move ship right:}] -sticky w
grid [ttk::entry $moveoptions.right -width 15] -row 5 -column 1 -sticky w
bind $moveoptions.right <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl7 -text {Move ship down/left:}] -sticky w
grid [ttk::entry $moveoptions.downleft -width 15] -row 6 -column 1 -sticky w
bind $moveoptions.downleft <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl8 -text {Move ship down:}] -sticky w
grid [ttk::entry $moveoptions.down -width 15] -row 7 -column 1 -sticky w
bind $moveoptions.down <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl9 -text {Move ship down/right:}] -sticky w
grid [ttk::entry $moveoptions.downright -width 15] -row 8 -column 1 -sticky w
bind $moveoptions.downright <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl10 -text {Move ship to destination:}] -sticky w
grid [ttk::entry $moveoptions.moveto -width 15] -row 9 -column 1 -sticky w
bind $moveoptions.moveto <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl11 -text {Set full stop for ship:}] -sticky w
grid [ttk::entry $moveoptions.fullstop -width 15] -row 10 -column 1 -sticky w
bind $moveoptions.fullstop <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl12 -text {Set quarter speed for ship:}] -sticky w
grid [ttk::entry $moveoptions.quarterspeed -width 15] -row 11 -column 1 -sticky w
bind $moveoptions.quarterspeed <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl13 -text {Set half speed for ship:}] -sticky w
grid [ttk::entry $moveoptions.halfspeed -width 15] -row 12 -column 1 -sticky w
bind $moveoptions.halfspeed <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl14 -text {Set full speed for ship:}] -sticky w
grid [ttk::entry $moveoptions.fullspeed -width 15] -row 13 -column 1 -sticky w
bind $moveoptions.fullspeed <KeyRelease> {SetShortcut %W %K}
$optionsframe.notebook add $moveoptions -text {Movement keys}
# Menu keys options
set menuoptions [ttk::frame $optionsframe.notebook.menu]
grid [ttk::label $menuoptions.lbl1 -text {Ship information:}] -sticky w
grid [ttk::entry $menuoptions.shipinfo -width 15] -row 0 -column 1 -sticky w
bind $menuoptions.shipinfo <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl4 -text {Ship orders:}] -sticky w
grid [ttk::entry $menuoptions.orders -width 15] -row 1 -column 1 -sticky w
bind $menuoptions.orders <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl5 -text {Crafting orders:}] -sticky w
grid [ttk::entry $menuoptions.crafts -width 15] -row 2 -column 1 -sticky w
bind $menuoptions.crafts <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl6 -text {Last messages:}] -sticky w
grid [ttk::entry $menuoptions.messages -width 15] -row 3 -column 1 -sticky w
bind $menuoptions.messages <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl7 -text {Knowledge lists:}] -sticky w
grid [ttk::entry $menuoptions.knowledge -width 15] -row 4 -column 1 -sticky w
bind $menuoptions.knowledge <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl11 -text {Wait orders:}] -sticky w
grid [ttk::entry $menuoptions.waitorders -width 15] -row 5 -column 1 -sticky w
bind $menuoptions.waitorders <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl12 -text {Game statistics:}] -sticky w
grid [ttk::entry $menuoptions.gamestats -width 15] -row 6 -column 1 -sticky w
bind $menuoptions.gamestats <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl13 -text {Help:}] -sticky w
grid [ttk::entry $menuoptions.help -width 15] -row 7 -column 1 -sticky w
bind $menuoptions.help <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl14 -text {Game options:}] -sticky w
grid [ttk::entry $menuoptions.gameoptions -width 15] -row 8 -column 1 -sticky w
bind $menuoptions.gameoptions <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl15 -text {Quit from game:}] -sticky w
grid [ttk::entry $menuoptions.quit -width 15] -row 9 -column 1 -sticky w
bind $menuoptions.quit <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl16 -text {Resign from game:}] -sticky w
grid [ttk::entry $menuoptions.resign -width 15] -row 10 -column 1 -sticky w
bind $menuoptions.resign <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl17 -text {Show menu:}] -sticky w
grid [ttk::entry $menuoptions.menu -width 15] -row 11 -column 1 -sticky w
bind $menuoptions.menu <KeyRelease> {SetShortcut %W %K}
$optionsframe.notebook add $menuoptions -text {Menu keys}
# Map keys options
set mapoptions [ttk::frame $optionsframe.notebook.map]
grid [ttk::label $mapoptions.lbl1 -text {Center map on player ship:}] -sticky w
grid [ttk::entry $mapoptions.center -width 15] -row 0 -column 1 -sticky w
bind $mapoptions.center <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl2 -text {Center map on home base:}] -sticky w
grid [ttk::entry $mapoptions.centerhomebase -width 15] -row 1 -column 1 -sticky w
bind $mapoptions.centerhomebase <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl3 -text {Move map to left:}] -sticky w
grid [ttk::entry $mapoptions.mapleft -width 15] -row 2 -column 1 -sticky w
bind $mapoptions.mapleft <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl4 -text {Move map to right:}] -sticky w
grid [ttk::entry $mapoptions.mapright -width 15] -row 3 -column 1 -sticky w
bind $mapoptions.mapright <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl5 -text {Move map up:}] -sticky w
grid [ttk::entry $mapoptions.mapup -width 15] -row 4 -column 1 -sticky w
bind $mapoptions.mapup <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl6 -text {Move map down:}] -sticky w
grid [ttk::entry $mapoptions.mapdown -width 15] -row 5 -column 1 -sticky w
bind $mapoptions.mapdown <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl7 -text {Move map up/left:}] -sticky w
grid [ttk::entry $mapoptions.mapupleft -width 15] -row 6 -column 1 -sticky w
bind $mapoptions.mapupleft <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl8 -text {Move map up/right:}] -sticky w
grid [ttk::entry $mapoptions.mapupright -width 15] -row 7 -column 1 -sticky w
bind $mapoptions.mapupright <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl9 -text {Move map down/left:}] -sticky w
grid [ttk::entry $mapoptions.mapdownleft -width 15] -row 8 -column 1 -sticky w
bind $mapoptions.mapdownleft <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl10 -text {Move map down/right:}] -sticky w
grid [ttk::entry $mapoptions.mapdownright -width 15] -row 9 -column 1 -sticky w
bind $mapoptions.mapdownright <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl11 -text {Move cursor up/left:}] -sticky w
grid [ttk::entry $mapoptions.cursorupleft -width 15] -row 10 -column 1 -sticky w
bind $mapoptions.cursorupleft <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl12 -text {Move cursor up:}] -sticky w
grid [ttk::entry $mapoptions.cursorup -width 15] -row 11 -column 1 -sticky w
bind $mapoptions.cursorup <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl13 -text {Move cursor up/right:}] -sticky w
grid [ttk::entry $mapoptions.cursorupright -width 15] -row 12 -column 1 -sticky w
bind $mapoptions.cursorupright <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl14 -text {Move cursor left:}] -sticky w
grid [ttk::entry $mapoptions.cursorleft -width 15] -row 13 -column 1 -sticky w
bind $mapoptions.cursorleft <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl15 -text {Move cursor right:}] -sticky w
grid [ttk::entry $mapoptions.cursorright -width 15] -row 14 -column 1 -sticky w
bind $mapoptions.cursorright <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl16 -text {Move cursor down/left:}] -sticky w
grid [ttk::entry $mapoptions.cursordownleft -width 15] -row 15 -column 1 -sticky w
bind $mapoptions.cursordownleft <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl17 -text {Move cursor down:}] -sticky w
grid [ttk::entry $mapoptions.cursordown -width 15] -row 16 -column 1 -sticky w
bind $mapoptions.cursordown <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl18 -text {Move cursor donw/right:}] -sticky w
grid [ttk::entry $mapoptions.cursordownright -width 15] -row 17 -column 1 -sticky w
bind $mapoptions.cursordownright <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl19 -text {Press mouse left button:}] -sticky w
grid [ttk::entry $mapoptions.clickmouse -width 15] -row 18 -column 1 -sticky w
bind $mapoptions.clickmouse <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl20 -text {Zoom in map:}] -sticky w
grid [ttk::entry $mapoptions.zoomin -width 15] -row 19 -column 1 -sticky w
bind $mapoptions.zoomin <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl21 -text {Zoom out map:}] -sticky w
grid [ttk::entry $mapoptions.zoomout -width 15] -row 20 -column 1 -sticky w
bind $mapoptions.zoomout <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl22 -text {Show move map options:}] -sticky w
grid [ttk::entry $mapoptions.mapoptions -width 15] -row 21 -column 1 -sticky w
bind $mapoptions.mapoptions <KeyRelease> {SetShortcut %W %K}
$optionsframe.notebook add $mapoptions -text {Map keys}
# Interface options
set ioptions [ttk::frame $optionsframe.notebook.interface]
grid [ttk::label $ioptions.lbl3 -text {Interface theme:}] -sticky w
tooltip::tooltip $ioptions.lbl3 {Select UI theme.}
grid [ttk::combobox $ioptions.theme -state readonly -width 15] -row 0 -column 1 -sticky w
tooltip::tooltip $ioptions.theme {Select UI theme.}
grid [ttk::label $ioptions.lbl4 -text {Show tooltips:}] -sticky w
tooltip::tooltip $ioptions.lbl4 {Show help tooltips for various game elements.}
grid [ttk::checkbutton $ioptions.showtooltips] -row 1 -column 1 -sticky w
tooltip::tooltip $ioptions.showtooltips {Show help tooltips for various game elements.}
grid [ttk::label $ioptions.lbl5 -text {Show last messages:}] -sticky w
tooltip::tooltip $ioptions.lbl5 {Show last messages window in every place in the game.}
grid [ttk::checkbutton $ioptions.showmessages] -row 2 -column 1 -sticky w
tooltip::tooltip $ioptions.showmessages {Show last messages window in every place in the game.}
grid [ttk::label $ioptions.lbl6 -text {Full screen mode:}] -sticky w
tooltip::tooltip $ioptions.lbl6 {Run the game in full screen mode.}
grid [ttk::checkbutton $ioptions.fullscreen] -row 3 -column 1 -sticky w
tooltip::tooltip $ioptions.fullscreen {Run the game in full screen mode.}
grid [ttk::label $ioptions.lbl7 -text {Full screen shortcut:}] -sticky w
grid [ttk::entry $ioptions.fullscreenkey -width 15] -row 4 -column 1 -sticky w
bind $ioptions.fullscreenkey <KeyRelease> {SetShortcut %W %K}
grid [ttk::label $ioptions.lbl8 -text {Close messages after:}] -sticky w
tooltip::tooltip $ioptions.lbl8 {Auto close game messages after that amount of seconds.}
grid [ttk::spinbox $ioptions.closemessages -from 1 -to 60 -validate key -validatecommand {ValidateSpinbox %W %P} -width 5] -row 5 -column 1 -sticky w
tooltip::tooltip $ioptions.closemessages {Auto close game messages after that amount of seconds.}
grid [ttk::label $ioptions.lbl9 -text {Show numeric values:}] -sticky w
tooltip::tooltip $ioptions.lbl9 "Show numeric values of many statistics, like crew\nabilities, weapons strength, etc."
grid [ttk::checkbutton $ioptions.shownumbers] -row 6 -column 1 -sticky w
tooltip::tooltip $ioptions.shownumbers "Show numeric values of many statistics, like crew\nabilities, weapons strength, etc."
grid [ttk::label $ioptions.lbl10 -text {Size of map font:}] -sticky w
tooltip::tooltip $ioptions.lbl10 {Size (in pixels) of font used to draw game map.}
grid [ttk::spinbox $ioptions.mapfont -from 3 -to 50 -validate key -validatecommand {ValidateSpinbox %W %P} -width 5] -row 7 -column 1 -sticky w
bind $ioptions.mapfont <FocusOut> {SetFonts %W}
tooltip::tooltip $ioptions.mapfont {Size (in pixels) of font used to draw game map.}
grid [ttk::label $ioptions.lbl11 -text {Size of help font:}] -sticky w
tooltip::tooltip $ioptions.lbl11 {Size (in pixels) of font used mainly in help.}
grid [ttk::spinbox $ioptions.helpfont -from 3 -to 50 -validate key -validatecommand {ValidateSpinbox %W %P} -width 5] -row 8 -column 1 -sticky w
bind $ioptions.helpfont <FocusOut> {SetFonts %W}
tooltip::tooltip $ioptions.helpfont {Size (in pixels) of font used mainly in help.}
grid [ttk::label $ioptions.lbl12 -text {Size of interface font:}] -sticky w
tooltip::tooltip $ioptions.lbl12 {Size (in pixels) of font used in interface (for example, here).}
grid [ttk::spinbox $ioptions.interfacefont -from 3 -to 50 -validate key -validatecommand {ValidateSpinbox %W %P} -width 5] -row 9 -column 1 -sticky w
bind $ioptions.interfacefont <FocusOut> {SetFonts %W}
tooltip::tooltip $ioptions.interfacefont {Size (in pixels) of font used in interface (for example, here).}
grid [ttk::button $ioptions.setdefault -text {Set default size for fonts} -command SetDefaultFonts] -columnspan 2
$optionsframe.notebook add $ioptions -text {Interface}
# Info options
set infooptions [ttk::frame $optionsframe.notebook.info]
grid [ttk::label $infooptions.lbl1 -text {Data directory path:}]
tooltip::tooltip $infooptions.lbl1 {Place where all standard game data are.}
grid [ttk::label $infooptions.data] -row 0 -column 1
tooltip::tooltip $infooptions.data {Place where all standard game data are.}
grid [ttk::label $infooptions.lbl2 -text {Save directory path:}]
tooltip::tooltip $infooptions.lbl2 {Place where all game saves and configuration files are.}
grid [ttk::label $infooptions.save] -row 1 -column 1
tooltip::tooltip $infooptions.save {Place where all game saves and configuration files are.}
grid [ttk::label $infooptions.lbl3 -text {Documentation directory path:}]
tooltip::tooltip $infooptions.lbl3 {Place where are game documentation files.}
grid [ttk::label $infooptions.docs] -row 2 -column 1
tooltip::tooltip $infooptions.docs {Place where are game documentation files.}
grid [ttk::label $infooptions.lbl4 -text {Modifications directory path:}]
tooltip::tooltip $infooptions.lbl4 {Place where you should put all modifications files.}
grid [ttk::label $infooptions.mods] -row 3 -column 1
tooltip::tooltip $infooptions.mods {Place where you should put all modifications files.}
$optionsframe.notebook add $infooptions -text {Info}
