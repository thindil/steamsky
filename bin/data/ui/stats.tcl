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

ttk::frame .gameframe.paned.statsframe
set statscanvas [canvas .gameframe.paned.statsframe.canvas -yscrollcommand [list .gameframe.paned.statsframe.scrolly set] -xscrollcommand [list .gameframe.paned.statsframe.scrollx set]]
pack [ttk::scrollbar .gameframe.paned.statsframe.scrolly -orient vertical -command [list $statscanvas yview]] -side right -fill y
pack $statscanvas -side top -fill both
pack [ttk::scrollbar .gameframe.paned.statsframe.scrollx -orient horizontal -command [list $statscanvas xview]] -fill x
::autoscroll::autoscroll .gameframe.paned.statsframe.scrolly
::autoscroll::autoscroll .gameframe.paned.statsframe.scrollx
set statsframe [ttk::frame $statscanvas.stats]
grid [ttk::frame $statsframe.left] -sticky nwes -padx 5
grid [ttk::label $statsframe.left.stats] -sticky w
grid [ttk::label $statsframe.left.crafts] -sticky w
grid [ttk::frame $statsframe.left.craftsframe] -sticky w
grid [ttk::treeview $statsframe.left.craftsframe.craftsview -show headings -columns [list name amount] -selectmode none -yscrollcommand [list $statsframe.left.craftsframe.scrolly set]] -sticky nwes
$statsframe.left.craftsframe.craftsview heading name -text {Name}
$statsframe.left.craftsframe.craftsview column name -width 250
$statsframe.left.craftsframe.craftsview heading amount -text {Amount}
$statsframe.left.craftsframe.craftsview column amount -width 75 -anchor center
grid [ttk::scrollbar $statsframe.left.craftsframe.scrolly -orient vertical -command [list $statsframe.left.craftsframe.craftsview yview]] -row 0 -column 1 -sticky ns
::autoscroll::autoscroll $statsframe.left.craftsframe.scrolly
grid [ttk::label $statsframe.left.missions] -sticky w
grid [ttk::frame $statsframe.left.missionsframe] -sticky w
grid [ttk::treeview $statsframe.left.missionsframe.missionsview -show headings -columns [list name amount] -selectmode none -yscrollcommand [list $statsframe.left.missionsframe.scrolly set]]
$statsframe.left.missionsframe.missionsview heading name -text {Name}
$statsframe.left.missionsframe.missionsview column name -width 250
$statsframe.left.missionsframe.missionsview heading amount -text {Amount}
$statsframe.left.missionsframe.missionsview column amount -width 75 -anchor center
grid [ttk::scrollbar $statsframe.left.missionsframe.scrolly -orient vertical -command [list $statsframe.left.missionsframe.missionsview yview]] -row 0 -column 1 -sticky ns
::autoscroll::autoscroll $statsframe.left.missionsframe.scrolly
grid [ttk::button $statsframe.left.goal -text {Goals} -command {ShowGoals $statsframe.left.goal}] -sticky w
grid [ttk::label $statsframe.left.goals] -sticky w
grid [ttk::frame $statsframe.left.goalsframe] -sticky w
grid [ttk::treeview $statsframe.left.goalsframe.goalsview -show headings -columns [list name amount] -selectmode none -yscrollcommand [list $statsframe.left.goalsframe.scrolly set]]
$statsframe.left.goalsframe.goalsview heading name -text {Name}
$statsframe.left.goalsframe.goalsview column name -width 250
$statsframe.left.goalsframe.goalsview heading amount -text {Amount}
$statsframe.left.goalsframe.goalsview column amount -width 75 -anchor center
grid [ttk::scrollbar $statsframe.left.goalsframe.scrolly -orient vertical -command [list $statsframe.left.goalsframe.goalsview yview]] -row 0 -column 1 -sticky ns
::autoscroll::autoscroll $statsframe.left.goalsframe.scrolly
grid [ttk::frame $statsframe.right] -row 0 -column 1 -sticky nwes -padx 5
grid [ttk::label $statsframe.right.destroyed] -sticky w
grid [ttk::frame $statsframe.right.destroyedframe] -sticky w
grid [ttk::treeview $statsframe.right.destroyedframe.destroyedview -show headings -columns [list name amount] -selectmode none -yscrollcommand [list $statsframe.right.destroyedframe.scrolly set]]
$statsframe.right.destroyedframe.destroyedview heading name -text {Name}
$statsframe.right.destroyedframe.destroyedview column name -width 250
$statsframe.right.destroyedframe.destroyedview heading amount -text {Amount}
$statsframe.right.destroyedframe.destroyedview column amount -width 75 -anchor center
grid [ttk::scrollbar $statsframe.right.destroyedframe.scrolly -orient vertical -command [list $statsframe.right.destroyedframe.destroyedview yview]] -row 0 -column 1 -sticky ns
::autoscroll::autoscroll $statsframe.right.destroyedframe.scrolly
grid [ttk::label $statsframe.right.killed] -sticky w
grid [ttk::frame $statsframe.right.killedframe] -sticky w
grid [ttk::treeview $statsframe.right.killedframe.killedview -show headings -columns [list name amount] -selectmode none -yscrollcommand [list $statsframe.left.killedframe.scrolly set]]
$statsframe.right.killedframe.killedview heading name -text {Name}
$statsframe.right.killedframe.killedview column name -width 250
$statsframe.right.killedframe.killedview heading amount -text {Amount}
$statsframe.right.killedframe.killedview column amount -width 75 -anchor center
grid [ttk::scrollbar $statsframe.right.killedframe.scrolly -orient vertical -command [list $statsframe.right.killedframe.destroyedview yview]] -row 0 -column 1 -sticky ns
::autoscroll::autoscroll $statsframe.right.killedframe.scrolly
grid columnconfigure .gameframe.paned.statsframe 0 -weight 1
