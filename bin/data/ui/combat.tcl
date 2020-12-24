# Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
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

set combatframe [ttk::frame .gameframe.paned.combatframe]

# Ship to ship combat
# Player ship crew orders
grid [ttk::labelframe $combatframe.crew -text {Your ship crew orders:}] -padx 5 -pady {0 5}
set combatcanvas [canvas $combatframe.crew.canvas -yscrollcommand [list $combatframe.crew.scrolly set] -xscrollcommand [list $combatframe.crew.scrollx set]]
pack [ttk::scrollbar $combatframe.crew.scrolly -orient vertical -command [list $combatcanvas yview]] -side right -fill y
pack [ttk::scrollbar $combatframe.crew.scrollx -orient horizontal -command [list $combatcanvas xview]] -fill x -side bottom
pack $combatcanvas -side top -fill both -expand true
ttk::frame $combatcanvas.frame
grid [ttk::label $combatcanvas.frame.position -text {Position}]
grid [ttk::label $combatcanvas.frame.name -text {Name}] -row 0 -column 1
grid [ttk::label $combatcanvas.frame.order -text {Order}] -row 0 -column 2
grid [ttk::label $combatcanvas.frame.pilotlabel -text {Pilot:}] -row 1 -sticky w -padx {5 0} -pady {0 5}
grid [ttk::combobox $combatcanvas.frame.pilotcrew -state readonly -width 10] -row 1 -column 1 -pady {0 5}
bind $combatcanvas.frame.pilotcrew <Return> {InvokeButton $combatframe.next}
grid [ttk::combobox $combatcanvas.frame.pilotorder -state readonly -values [list {Go closer} {Keep distance} {Evade} {Escape}]] -row 1 -column 2 -padx {0 5} -pady {0 5}
bind $combatcanvas.frame.pilotorder <Return> {InvokeButton $combatframe.next}
bind $combatcanvas.frame.pilotorder <<ComboboxSelected>> {SetCombatOrder pilot}
grid [ttk::label $combatcanvas.frame.engineerlabel -text {Engineer:}] -row 2 -sticky w -padx {5 0} -pady {5 0}
grid [ttk::combobox $combatcanvas.frame.engineercrew -state readonly -width 10] -row 2 -column 1 -pady {5 0}
bind $combatcanvas.frame.engineercrew <Return> {InvokeButton $combatframe.next}
grid [ttk::combobox $combatcanvas.frame.engineerorder -state readonly -values [list {All stop} {Quarter speed} {Half speed} {Full speed}]] -row 2 -column 2 -padx {0 5} -pady {5 0}
bind $combatcanvas.frame.engineerorder <Return> {InvokeButton $combatframe.next}
bind $combatcanvas.frame.engineerorder <<ComboboxSelected>> {SetCombatOrder engineer}
$combatcanvas create window 0 0 -anchor nw -window $combatcanvas.frame
::autoscroll::autoscroll $combatframe.crew.scrolly
::autoscroll::autoscroll $combatframe.crew.scrollx
# Player ship damage
grid [ttk::labelframe $combatframe.damage -text {Your ship damage:}] -sticky we -padx 5 -pady {5 0}
set combatcanvas [canvas $combatframe.damage.canvas -yscrollcommand [list $combatframe.damage.scrolly set] -xscrollcommand [list $combatframe.damage.scrollx set]]
pack [ttk::scrollbar $combatframe.damage.scrolly -orient vertical -command [list $combatcanvas yview]] -side right -fill y
pack [ttk::scrollbar $combatframe.damage.scrollx -orient horizontal -command [list $combatcanvas xview]] -fill x -side bottom
pack $combatcanvas -side top -fill both -expand true
ttk::frame $combatcanvas.frame
$combatcanvas create window 0 0 -anchor nw -window $combatcanvas.frame
::autoscroll::autoscroll $combatframe.damage.scrolly
::autoscroll::autoscroll $combatframe.damage.scrollx
# Enemy ship info
grid [ttk::labelframe $combatframe.enemy -text {Enemy info:}] -sticky we -padx 5 -pady {0 5} -column 1 -row 0
set combatcanvas [canvas $combatframe.enemy.canvas -yscrollcommand [list $combatframe.enemy.scrolly set] -xscrollcommand [list $combatframe.enemy.scrollx set]]
pack [ttk::scrollbar $combatframe.enemy.scrolly -orient vertical -command [list $combatcanvas yview]] -side right -fill y
pack [ttk::scrollbar $combatframe.enemy.scrollx -orient horizontal -command [list $combatcanvas xview]] -fill x -side bottom
pack $combatcanvas -side top -fill both -expand true
ttk::label $combatcanvas.info -wraplength 350
$combatcanvas create window 0 0 -anchor nw -window $combatcanvas.info
::autoscroll::autoscroll $combatframe.enemy.scrolly
::autoscroll::autoscroll $combatframe.enemy.scrollx
# Enemy ship info damage
grid [ttk::labelframe $combatframe.status -text {Enemy ship status:}] -sticky we -padx 5 -pady {5 0} -column 1 -row 1
set combatcanvas [canvas $combatframe.status.canvas -yscrollcommand [list $combatframe.status.scrolly set] -xscrollcommand [list $combatframe.status.scrollx set]]
pack [ttk::scrollbar $combatframe.status.scrolly -orient vertical -command [list $combatcanvas yview]] -side right -fill y
pack [ttk::scrollbar $combatframe.status.scrollx -orient horizontal -command [list $combatcanvas xview]] -fill x -side bottom
pack $combatcanvas -side top -fill both -expand true
ttk::frame $combatcanvas.frame
$combatcanvas create window 0 0 -anchor nw -window $combatcanvas.frame
::autoscroll::autoscroll $combatframe.status.scrolly
::autoscroll::autoscroll $combatframe.status.scrollx
grid [ttk::button $combatframe.next -text {Next turn [Enter]} -command NextTurn] -columnspan 2 -sticky we
bind $combatframe.next <Return> {InvokeButton $combatframe.next}
focus $combatframe.next

# Boarding combat
grid [ttk::labelframe $combatframe.left -text {Your crew:}] -sticky n
grid [ttk::label $combatframe.left.name -text {Name}] -row 0 -column 0
grid [ttk::label $combatframe.left.health -text {Health}] -row 0 -column 1
grid [ttk::label $combatframe.left.order -text {Order}] -row 0 -column 2
grid [ttk::labelframe $combatframe.right -text {Enemy's crew:}] -row 1 -column 1 -sticky n
grid [ttk::label $combatframe.right.name -text {Name}] -row 0 -column 0
grid [ttk::label $combatframe.right.health -text {Health}] -row 0 -column 1
grid [ttk::label $combatframe.right.order -text {Order}] -row 0 -column 2
grid remove $combatframe.left
grid remove $combatframe.right

# Configure main combat grid
grid columnconfigure $combatframe 0 -weight 1
grid columnconfigure $combatframe 1 -weight 1
grid rowconfigure $combatframe 0 -weight 1
grid rowconfigure $combatframe 1 -weight 1
