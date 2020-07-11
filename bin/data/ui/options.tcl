# test code
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
grid [ttk::checkbutton $goptions.autorest] -row 0 -column 1
grid [ttk::label $goptions.lbl2 -text {Default speed after undocking:}]
grid [ttk::combobox $goptions.speed -state readonly -values [list {Full stop} {Quarted speed} {Half speed} {Full speed}] -width 10] -row 1 -column 1
grid [ttk::label $goptions.lbl3 -text {Auto center map after set destination:}]
grid [ttk::checkbutton $goptions.autocenter] -row 2 -column 1
grid [ttk::label $goptions.lbl4 -text {Auto set base after finished mission:}]
grid [ttk::checkbutton $goptions.autoreturn] -row 3 -column 1
grid [ttk::label $goptions.lbl5 -text {Auto finish missions:}]
grid [ttk::checkbutton $goptions.autofinish] -row 4 -column 1
grid [ttk::label $goptions.lbl6 -text {Auto ask for bases:}]
grid [ttk::checkbutton $goptions.autoaskforbases] -row 5 -column 1
grid [ttk::label $goptions.lbl7 -text {Auto ask for events:}]
grid [ttk::checkbutton $goptions.autoaskforevents] -row 6 -column 1
grid [ttk::label $goptions.lbl8 -text {Low level of fuel:}]
grid [ttk::spinbox $goptions.fuel -from 1 -to 10000 -validate key] -row 7 -column 1
grid [ttk::label $goptions.lbl9 -text {Low level of drinks:}]
grid [ttk::spinbox $goptions.drinks -from 1 -to 10000 -validate key] -row 8 -column 1
grid [ttk::label $goptions.lbl10 -text {Low level of food:}]
grid [ttk::spinbox $goptions.food -from 1 -to 10000 -validate key] -row 9 -column 1
grid [ttk::label $goptions.lbl11 -text {Stop auto movement:}]
grid [ttk::combobox $goptions.automovestop -state readonly -values [list {Never} {Any ship} {Friendly ship} {Enemy ship}] -width 10] -row 10 -column 1
grid [ttk::label $goptions.lbl12 -text {Messages limit:}]
grid [ttk::spinbox $goptions.messageslimit -from 10 -to 5000 -validate key] -row 11 -column 1
grid [ttk::label $goptions.lbl13 -text {Saved messages:}]
grid [ttk::spinbox $goptions.savedmessages -from 5 -to 20 -validate key] -row 12 -column 1
grid [ttk::label $goptions.lbl14 -text {Messages order:}]
grid [ttk::combobox $goptions.messagesorder -state readonly -values [list {Older messages first} {Newer messages first}] -width 16] -row 13 -column 1
grid [ttk::label $goptions.lbl15 -text {Autosave game:}]
grid [ttk::combobox $goptions.autosave -state readonly -values [list {Never} {After dock to base} {After undock from base} {Every game day} {Every game month} {Every game year}] -width 18] -row 14 -column 1
$optionsframe.notebook add $goptions -text {General}
# Movement keys options
set specialkey {}
# Set proper shortcut, validate it and check if it is not set somewhere
proc SetShortcut {field key} {
   global specialkey moveoptions menuotions mapoptions
   set fields [list $moveoptions.upleft $moveoptions.up $moveoptions.upright $moveoptions.left $moveoptions.wait $moveoptions.right $moveoptions.downleft $moveoptions.down $moveoptions.downright $moveoptions.moveto $moveoptions.fullstop $moveoptions.quarterspeed $moveoptions.halfspeed $moveoptions.fullspeed]
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
$optionsframe.notebook add $menuoptions -text {Menu keys}
# Map keys options
set mapoptions [ttk::frame $optionsframe.notebook.map]
$optionsframe.notebook add $mapoptions -text {Map keys}
# Interface options
set ioptions [ttk::frame $optionsframe.notebook.interface]
$optionsframe.notebook add $ioptions -text {Interface}
# Info options
set infooptions [ttk::frame $optionsframe.notebook.info]
$optionsframe.notebook add $infooptions -text {Info}
# test code
pack $optionsframe -fill both -expand true
pack .paned -fill both -expand true
