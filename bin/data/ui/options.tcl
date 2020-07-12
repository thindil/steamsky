# test code
package require tooltip
ttk::frame .paned
set optionsframe [ttk::frame .paned.options]
# normal code
#ttk::frame .paned.optionsframe
#set optionscanvas [canvas .paned.optionsframe.canvas -yscrollcommand [list .paned.optionsframe.scrolly set] -xscrollcommand [list .paned.optionsframe.scrollx set]]
#grid $optionscanvas -sticky nwes
#grid [ttk::scrollbar .paned.optionsframe.scrollx -orient horizontal -command [list $optionscanvas xview]] -row 1 -column 0 -columnspan 2 -sticky we
#grid [ttk::scrollbar .paned.optionsframe.scrolly -orient vertical -command [list $optionscanvas yview]] -row 0 -column 1 -sticky ns
#set optionsframe [ttk::frame $optionscanvas.options]
grid [ttk::notebook $optionsframe.notebook] -sticky nwes
# General options
set goptions [ttk::frame $optionsframe.notebook.general]
grid [ttk::label $goptions.lbl1 -text {Auto rest when crew is tired:}]
tooltip::tooltip $goptions.lbl1 {Wait for crew is rested when pilot or engineer are too tired to work.}
grid [ttk::checkbutton $goptions.autorest] -row 0 -column 1
tooltip::tooltip $goptions.autorest {Wait for crew is rested when pilot or engineer are too tired to work.}
grid [ttk::label $goptions.lbl2 -text {Default speed after undocking:}]
tooltip::tooltip $goptions.lbl2 {Default speed of ship after undock from base.}
grid [ttk::combobox $goptions.speed -state readonly -values [list {Full stop} {Quarted speed} {Half speed} {Full speed}] -width 10] -row 1 -column 1
tooltip::tooltip $goptions.speed {Default speed of ship after undock from base.}
grid [ttk::label $goptions.lbl3 -text {Auto center map after set destination:}]
tooltip::tooltip $goptions.lbl3 {After set destination for ship, center map on ship.}
grid [ttk::checkbutton $goptions.autocenter] -row 2 -column 1
tooltip::tooltip $goptions.autocenter {After set destination for ship, center map on ship.}
grid [ttk::label $goptions.lbl4 -text {Auto set base after finished mission:}]
tooltip::tooltip $goptions.lbl4 {After finished mission, set skybase from which mission was taken as a destination for ship.}
grid [ttk::checkbutton $goptions.autoreturn] -row 3 -column 1
tooltip::tooltip $goptions.autoreturn {After finished mission, set skybase from which mission was taken as a destination for ship.}
grid [ttk::label $goptions.lbl5 -text {Auto finish missions:}]
tooltip::tooltip $goptions.lbl5 {Auto finish missions when ship is near corresponding skybase.}
grid [ttk::checkbutton $goptions.autofinish] -row 4 -column 1
tooltip::tooltip $goptions.autofinish {Auto finish missions when ship is near corresponding skybase.}
grid [ttk::label $goptions.lbl6 -text {Auto ask for bases:}]
tooltip::tooltip $goptions.lbl6 {Auto ask for bases when ship end docking to bases.}
grid [ttk::checkbutton $goptions.autoaskforbases] -row 5 -column 1
tooltip::tooltip $goptions.autoaskforbases {Auto ask for bases when ship end docking to bases.}
grid [ttk::label $goptions.lbl7 -text {Auto ask for events:}]
tooltip::tooltip $goptions.lbl7 {Auto ask for events when ship end docking to bases.}
grid [ttk::checkbutton $goptions.autoaskforevents] -row 6 -column 1
tooltip::tooltip $goptions.autoaskforevents {Auto ask for events when ship end docking to bases.}
grid [ttk::label $goptions.lbl8 -text {Low level of fuel:}]
tooltip::tooltip $goptions.lbl8 {Amount of fuel below which you will see warning about low level of.}
grid [ttk::spinbox $goptions.fuel -from 1 -to 10000 -validate key -validatecommand {ValidateSpinbox %S %s 10000}] -row 7 -column 1
tooltip::tooltip $goptions.fuel {Amount of fuel below which you will see warning about low level of.}
grid [ttk::label $goptions.lbl9 -text {Low level of drinks:}]
tooltip::tooltip $goptions.lbl9 {Amount of drinks below which you will see warning about low level of.}
grid [ttk::spinbox $goptions.drinks -from 1 -to 10000 -validate key -validatecommand {ValidateSpinbox %S %s 10000}] -row 8 -column 1
tooltip::tooltip $goptions.drinks {Amount of drinks below which you will see warning about low level of.}
grid [ttk::label $goptions.lbl10 -text {Low level of food:}]
tooltip::tooltip $goptions.lbl10 {Amount of food below which you will see warning about low level of.}
grid [ttk::spinbox $goptions.food -from 1 -to 10000 -validate key -validatecommand {ValidateSpinbox %S %s 10000}] -row 9 -column 1
tooltip::tooltip $goptions.food {Amount of food below which you will see warning about low level of.}
grid [ttk::label $goptions.lbl11 -text {Stop auto movement:}]
tooltip::tooltip $goptions.lbl11 {Set when auto move ship should stop: never, on meet any ship, on meet friendly ship or on meet enemy ship.}
grid [ttk::combobox $goptions.automovestop -state readonly -values [list {Never} {Any ship} {Friendly ship} {Enemy ship}] -width 10] -row 10 -column 1
tooltip::tooltip $goptions.automovestop {Set when auto move ship should stop: never, on meet any ship, on meet friendly ship or on meet enemy ship.}
grid [ttk::label $goptions.lbl12 -text {Messages limit:}]
tooltip::tooltip $goptions.lbl12 {Amount of messages stored in game. If new message arrive when limit is reached, oldest message will be deleted. Enter value between 10 and 5000.}
grid [ttk::spinbox $goptions.messageslimit -from 10 -to 5000 -validate key -validatecommand {ValidateSpinbox %S %s 5000}] -row 11 -column 1
tooltip::tooltip $goptions.messageslimit {Amount of messages stored in game. If new message arrive when limit is reached, oldest message will be deleted. Enter value between 10 and 5000.}
grid [ttk::label $goptions.lbl13 -text {Saved messages:}]
tooltip::tooltip $goptions.lbl13 {Maximum amount of last messages saved to file. Enter value between 5 and 200.}
grid [ttk::spinbox $goptions.savedmessages -from 5 -to 20 -validate key -validatecommand {ValidateSpinbox %S %s 20}] -row 12 -column 1
tooltip::tooltip $goptions.savedmessages {Maximum amount of last messages saved to file. Enter value between 5 and 200.}
grid [ttk::label $goptions.lbl14 -text {Messages order:}]
tooltip::tooltip $goptions.lbl14 {In what order show messages in game. If Older first will be select, then older messages will appear at top of the lists. Otherwise newer messages will be at top.}
grid [ttk::combobox $goptions.messagesorder -state readonly -values [list {Older messages first} {Newer messages first}] -width 16] -row 13 -column 1
tooltip::tooltip $goptions.messagesorder {In what order show messages in game. If Older first will be select, then older messages will appear at top of the lists. Otherwise newer messages will be at top.}
grid [ttk::label $goptions.lbl15 -text {Autosave game:}]
tooltip::tooltip $goptions.lbl15 {How often game should be automatically saved to disk.}
grid [ttk::combobox $goptions.autosave -state readonly -values [list {Never} {After dock to base} {After undock from base} {Every game day} {Every game month} {Every game year}] -width 18] -row 14 -column 1
tooltip::tooltip $goptions.autosave {How often game should be automatically saved to disk.}
$optionsframe.notebook add $goptions -text {General}
# Movement keys options
set specialkey {}
# Set proper shortcut, validate it and check if it is not set somewhere
proc SetShortcut {field key} {
   global specialkey moveoptions menuotions mapoptions
   set fields [list $moveoptions.upleft $moveoptions.up $moveoptions.upright $moveoptions.left $moveoptions.wait $moveoptions.right $moveoptions.downleft $moveoptions.down $moveoptions.downright $moveoptions.moveto $moveoptions.fullstop $moveoptions.quarterspeed $moveoptions.halfspeed $moveoptions.fullspeed $menuoptions.shipinfo $menuoptions.cargo $menuoptions.crew $menuoptions.orders $menuoptions.crafts $menuoptions.messages $menuoptions.bases $menuoptions.events $menuoptions.missions  $menuoptions.stories $menuoptions.waitorders $menuoptions.gamestats $menuoptions.help $menuoptions.gameoptions $menuoptions.quit $menuoptions.resign $menuoptions.menu $mapoptions.center $mapoptions.centerhomebase $mapoptions.mapleft $mapoptions.mapright $mapoptions.mapup $mapoptions.mapdown $mapoptions.mapupleft $mapoptions.mapupright $mapoptions.mapdownleft $mapoptions.mapdownright $mapoptions.cursorupleft $mapoptions.cursorup $mapoptions.cursorupright $mapoptions.cursorleft $mapoptions.cursorright $mapoptions.cursordownleft $mapoptions.cursordown $mapoptions.cursordownright $mapoptions.clickmouse $mapoptions.zoomin $mapoptions.zoomout $mapoptions.mapoptions $ioptions.fullscreenkey]
   $field delete 0 end
   if {$key == "Control_L" || $key == "Control_R" || $key == "Alt_L" || $key == "Alt_R" || $key == "Shift_L" || $key == "Shift_R"} {
      set specialkey "$key"
      return
   }
   if {$specialkey != {}} {
      set value "$specialkey+$key"
   } else {
      set value "$key"
   }
   foreach keyentry $fields {
      if {$keyentry != $field && [$keyentry get] == $value} {
         return
      }
   }
   if {$specialkey != {}} {
      $field insert 0 $specialkey
      $field insert end +
      if {$specialkey != "Shift_L" && $specialkey != "Shift_R"} {
         $field insert end $key
      }
      set specialkey {}
   }
}
set moveoptions [ttk::frame $optionsframe.notebook.movement]
grid [ttk::label $moveoptions.lbl1 -text {Move ship up/left:}]
grid [ttk::entry $moveoptions.upleft] -row 0 -column 1
bind $moveoptions.upleft <KeyPress> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl2 -text {Move ship up:}]
grid [ttk::entry $moveoptions.up] -row 1 -column 1
bind $moveoptions.up <KeyPress> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl3 -text {Move ship up/right:}]
grid [ttk::entry $moveoptions.upright] -row 2 -column 1
bind $moveoptions.upright <KeyPress> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl4 -text {Move ship left:}]
grid [ttk::entry $moveoptions.left] -row 3 -column 1
bind $moveoptions.left <KeyPress> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl5 -text {Wait 1 minute or move 1 field:}]
grid [ttk::entry $moveoptions.wait] -row 4 -column 1
bind $moveoptions.wait <KeyPress> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl6 -text {Move ship right:}]
grid [ttk::entry $moveoptions.right] -row 5 -column 1
bind $moveoptions.right <KeyPress> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl7 -text {Move ship down/left:}]
grid [ttk::entry $moveoptions.downleft] -row 6 -column 1
bind $moveoptions.downleft <KeyPress> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl8 -text {Move ship down:}]
grid [ttk::entry $moveoptions.down] -row 7 -column 1
bind $moveoptions.down <KeyPress> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl9 -text {Move ship down/right:}]
grid [ttk::entry $moveoptions.downright] -row 8 -column 1
bind $moveoptions.downright <KeyPress> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl10 -text {Move ship to destination:}]
grid [ttk::entry $moveoptions.moveto] -row 9 -column 1
bind $moveoptions.moveto <KeyPress> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl11 -text {Set full stop for ship:}]
grid [ttk::entry $moveoptions.fullstop] -row 10 -column 1
bind $moveoptions.fullstop <KeyPress> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl12 -text {Set quarter speed for ship:}]
grid [ttk::entry $moveoptions.quarterspeed] -row 11 -column 1
bind $moveoptions.quarterspeed <KeyPress> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl13 -text {Set half speed for ship:}]
grid [ttk::entry $moveoptions.halfspeed] -row 12 -column 1
bind $moveoptions.halfspeed <KeyPress> {SetShortcut %W %K}
grid [ttk::label $moveoptions.lbl14 -text {Set full speed for ship:}]
grid [ttk::entry $moveoptions.fullspeed] -row 13 -column 1
bind $moveoptions.fullspeed <KeyPress> {SetShortcut %W %K}
$optionsframe.notebook add $moveoptions -text {Movement keys}
# Menu keys options
set menuoptions [ttk::frame $optionsframe.notebook.menu]
grid [ttk::label $menuoptions.lbl1 -text {Ship information:}]
grid [ttk::entry $menuoptions.shipinfo] -row 0 -column 1
bind $menuoptions.shipinfo <KeyPress> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl2 -text {Ship cargo information:}]
grid [ttk::entry $menuoptions.cargo] -row 1 -column 1
bind $menuoptions.cargo <KeyPress> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl3 -text {Crew information:}]
grid [ttk::entry $menuoptions.crew] -row 2 -column 1
bind $menuoptions.crew <KeyPress> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl4 -text {Ship orders:}]
grid [ttk::entry $menuoptions.orders] -row 3 -column 1
bind $menuoptions.orders <KeyPress> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl5 -text {Crafting orders:}]
grid [ttk::entry $menuoptions.crafts] -row 4 -column 1
bind $menuoptions.crafts <KeyPress> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl6 -text {Last messages:}]
grid [ttk::entry $menuoptions.messages] -row 5 -column 1
bind $menuoptions.messages <KeyPress> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl7 -text {List of known bases:}]
grid [ttk::entry $menuoptions.bases] -row 6 -column 1
bind $menuoptions.bases <KeyPress> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl8 -text {List of known events:}]
grid [ttk::entry $menuoptions.events] -row 7 -column 1
bind $menuoptions.events <KeyPress> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl9 -text {List of accepted missions:}]
grid [ttk::entry $menuoptions.missions] -row 8 -column 1
bind $menuoptions.missions <KeyPress> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl10 -text {Stories:}]
grid [ttk::entry $menuoptions.stories] -row 9 -column 1
bind $menuoptions.stories <KeyPress> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl11 -text {Wait orders:}]
grid [ttk::entry $menuoptions.waitorders] -row 10 -column 1
bind $menuoptions.waitorders <KeyPress> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl12 -text {Game statistics:}]
grid [ttk::entry $menuoptions.gamestats] -row 11 -column 1
bind $menuoptions.gamestats <KeyPress> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl13 -text {Help:}]
grid [ttk::entry $menuoptions.help] -row 12 -column 1
bind $menuoptions.help <KeyPress> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl14 -text {Game options:}]
grid [ttk::entry $menuoptions.gameoptions] -row 13 -column 1
bind $menuoptions.gameoptions <KeyPress> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl15 -text {Quit from game:}]
grid [ttk::entry $menuoptions.quit] -row 14 -column 1
bind $menuoptions.quit <KeyPress> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl16 -text {Resign from game:}]
grid [ttk::entry $menuoptions.resign] -row 15 -column 1
bind $menuoptions.resign <KeyPress> {SetShortcut %W %K}
grid [ttk::label $menuoptions.lbl17 -text {Show menu:}]
grid [ttk::entry $menuoptions.menu] -row 16 -column 1
bind $menuoptions.menu <KeyPress> {SetShortcut %W %K}
$optionsframe.notebook add $menuoptions -text {Menu keys}
# Map keys options
set mapoptions [ttk::frame $optionsframe.notebook.map]
grid [ttk::label $mapoptions.lbl1 -text {Center map on player ship:}]
grid [ttk::entry $mapoptions.center] -row 0 -column 1
bind $mapoptions.center <KeyPress> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl2 -text {Center map on home base:}]
grid [ttk::entry $mapoptions.centerhomebase] -row 1 -column 1
bind $mapoptions.centerhomebase <KeyPress> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl3 -text {Move map to left:}]
grid [ttk::entry $mapoptions.mapleft] -row 2 -column 1
bind $mapoptions.mapleft <KeyPress> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl4 -text {Move map to right:}]
grid [ttk::entry $mapoptions.mapright] -row 3 -column 1
bind $mapoptions.mapright <KeyPress> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl5 -text {Move map up:}]
grid [ttk::entry $mapoptions.mapup] -row 4 -column 1
bind $mapoptions.mapup <KeyPress> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl6 -text {Move map down:}]
grid [ttk::entry $mapoptions.mapdown] -row 5 -column 1
bind $mapoptions.mapdown <KeyPress> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl7 -text {Move map up/left:}]
grid [ttk::entry $mapoptions.mapupleft] -row 6 -column 1
bind $mapoptions.mapupleft <KeyPress> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl8 -text {Move map up/right:}]
grid [ttk::entry $mapoptions.mapupright] -row 7 -column 1
bind $mapoptions.mapupright <KeyPress> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl9 -text {Move map down/left:}]
grid [ttk::entry $mapoptions.mapdownleft] -row 8 -column 1
bind $mapoptions.mapdownleft <KeyPress> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl10 -text {Move map down/right:}]
grid [ttk::entry $mapoptions.mapdownright] -row 9 -column 1
bind $mapoptions.mapdownright <KeyPress> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl11 -text {Move cursor up/left:}]
grid [ttk::entry $mapoptions.cursorupleft] -row 10 -column 1
bind $mapoptions.cursorupleft <KeyPress> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl12 -text {Move cursor up:}]
grid [ttk::entry $mapoptions.cursorup] -row 11 -column 1
bind $mapoptions.cursorup <KeyPress> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl13 -text {Move cursor up/right:}]
grid [ttk::entry $mapoptions.cursorupright] -row 12 -column 1
bind $mapoptions.cursorupright <KeyPress> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl14 -text {Move cursor left:}]
grid [ttk::entry $mapoptions.cursorleft] -row 13 -column 1
bind $mapoptions.cursorleft <KeyPress> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl15 -text {Move cursor right:}]
grid [ttk::entry $mapoptions.cursorright] -row 14 -column 1
bind $mapoptions.cursorright <KeyPress> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl16 -text {Move cursor down/left:}]
grid [ttk::entry $mapoptions.cursordownleft] -row 15 -column 1
bind $mapoptions.cursordownleft <KeyPress> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl17 -text {Move cursor down:}]
grid [ttk::entry $mapoptions.cursordown] -row 16 -column 1
bind $mapoptions.cursordown <KeyPress> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl18 -text {Move cursor donw/right:}]
grid [ttk::entry $mapoptions.cursordownright] -row 17 -column 1
bind $mapoptions.cursordownright <KeyPress> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl19 -text {Press mouse left button:}]
grid [ttk::entry $mapoptions.clickmouse] -row 18 -column 1
bind $mapoptions.clickmouse <KeyPress> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl20 -text {Zoom in map:}]
grid [ttk::entry $mapoptions.zoomin] -row 19 -column 1
bind $mapoptions.zoomin <KeyPress> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl21 -text {Zoom out map:}]
grid [ttk::entry $mapoptions.zoomout] -row 20 -column 1
bind $mapoptions.zoomout <KeyPress> {SetShortcut %W %K}
grid [ttk::label $mapoptions.lbl22 -text {Show move map options:}]
grid [ttk::entry $mapoptions.mapoptions] -row 21 -column 1
bind $mapoptions.mapoptions <KeyPress> {SetShortcut %W %K}
$optionsframe.notebook add $mapoptions -text {Map keys}
# Interface options
set ioptions [ttk::frame $optionsframe.notebook.interface]
grid [ttk::label $ioptions.lbl1 -text {Animations enabled:}]
grid [ttk::checkbutton $ioptions.animations] -row 0 -column 1
grid [ttk::label $ioptions.lbl2 -text {Type of animations:}]
grid [ttk::combobox $ioptions.speed -state readonly -values [list {Crossfade} {Slide right} {Slide left}] -width 10] -row 1 -column 1
grid [ttk::label $ioptions.lbl3 -text {Interface theme:}]
grid [ttk::combobox $ioptions.theme -state readonly -width 10] -row 2 -column 1
grid [ttk::label $ioptions.lbl4 -text {Show tooltips:}]
grid [ttk::checkbutton $ioptions.showtooltips] -row 3 -column 1
grid [ttk::label $ioptions.lbl5 -text {Show last messages:}]
grid [ttk::checkbutton $ioptions.showmessages] -row 4 -column 1
grid [ttk::label $ioptions.lbl6 -text {Full screen mode:}]
grid [ttk::checkbutton $ioptions.fullscreen] -row 5 -column 1
grid [ttk::label $ioptions.lbl7 -text {Full screen shortcut:}]
grid [ttk::entry $ioptions.fullscreenkey] -row 6 -column 1
bind $ioptions.fullscreenkey <KeyPress> {SetShortcut %W %K}
grid [ttk::label $ioptions.lbl8 -text {Close messages after:}]
grid [ttk::spinbox $ioptions.closemessages -from 1 -to 60 -validate key -validatecommand {ValidateSpinbox %S %s 60}] -row 7 -column 1
grid [ttk::label $ioptions.lbl9 -text {Show numeric values:}]
grid [ttk::checkbutton $ioptions.shownumbers] -row 8 -column 1
grid [ttk::label $ioptions.lbl10 -text {Size of map font:}]
grid [ttk::spinbox $ioptions.mapfont -from 3 -to 50 -validate key -validatecommand {ValidateSpinbox %S %s 50}] -row 9 -column 1
grid [ttk::label $ioptions.lbl11 -text {Size of help font:}]
grid [ttk::spinbox $ioptions.helpfont -from 3 -to 50 -validate key -validatecommand {ValidateSpinbox %S %s 50}] -row 10 -column 1
grid [ttk::label $ioptions.lbl12 -text {Size of interface font:}]
grid [ttk::spinbox $ioptions.interfacefont -from 3 -to 50 -validate key -validatecommand {ValidateSpinbox %S %s 50}] -row 11 -column 1
grid [ttk::button $ioptions.setdefault -text {Set default size for fonts}] -columnspan 2
$optionsframe.notebook add $ioptions -text {Interface}
# Info options
set infooptions [ttk::frame $optionsframe.notebook.info]
grid [ttk::label $infooptions.lbl1 -text {Data directory path:}]
grid [ttk::label $infooptions.data] -row 0 -column 1
grid [ttk::label $infooptions.lbl2 -text {Save directory path:}]
grid [ttk::label $infooptions.save] -row 1 -column 1
grid [ttk::label $infooptions.lbl3 -text {Documentation directory path:}]
grid [ttk::label $infooptions.docs] -row 2 -column 1
grid [ttk::label $infooptions.lbl4 -text {Modifications directory path:}]
grid [ttk::label $infooptions.mods] -row 3 -column 1
$optionsframe.notebook add $infooptions -text {Info}
# test code
pack $optionsframe -fill both -expand true
pack .paned -fill both -expand true
