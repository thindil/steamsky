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

ttk::frame .goalsdialog -style Dialog.TFrame
set view [ttk::treeview .goalsdialog.view -show tree -yscrollcommand [list .goalsdialog.yscroll set] -height 9]
set selectbutton [ttk::button .goalsdialog.selectbutton -text {Select goal}]
grid $view
$view column #0 -width 450 -stretch 1
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
grid $selectbutton -row 2 -columnspan 2 -sticky we
grid [ttk::button .goalsdialog.closebutton -text {Close (Escape)} -command {CloseDialog .goalsdialog}] -row 3 -columnspan 2 -sticky we
bind .goalsdialog <Escape> {.goalsdialog.closebutton invoke}
::autoscroll::autoscroll .goalsdialog.yscroll
place .goalsdialog -in . -relx 0.1 -rely 0.1
