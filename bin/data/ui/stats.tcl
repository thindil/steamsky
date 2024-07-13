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

ttk::frame .gameframe.paned.statsframe
set statscanvas [canvas .gameframe.paned.statsframe.canvas \
   -yscrollcommand [list .gameframe.paned.statsframe.scrolly set] \
   -xscrollcommand [list .gameframe.paned.statsframe.scrollx set]]
pack [ttk::scrollbar .gameframe.paned.statsframe.scrolly -orient vertical \
   -command [list $statscanvas yview]] -side right -fill y
pack $statscanvas -side top -fill both
pack [ttk::scrollbar .gameframe.paned.statsframe.scrollx -orient horizontal \
   -command [list $statscanvas xview]] -fill x
::autoscroll::autoscroll .gameframe.paned.statsframe.scrolly
::autoscroll::autoscroll .gameframe.paned.statsframe.scrollx
SetScrollbarBindings $statscanvas .gameframe.paned.statsframe.scrolly
set statsframe [ttk::frame $statscanvas.stats]
grid [ttk::frame $statsframe.left] -sticky nwes -padx 5
SetScrollbarBindings $statsframe.left .gameframe.paned.statsframe.scrolly
grid [ttk::frame $statsframe.left.points] -sticky w
SetScrollbarBindings $statsframe.left.points .gameframe.paned.statsframe.scrolly
grid [ttk::label $statsframe.left.points.lblpoints -text {Points: }] -sticky w
SetScrollbarBindings $statsframe.left.points.lblpoints \
   .gameframe.paned.statsframe.scrolly
grid [ttk::label $statsframe.left.points.points -style Golden.TLabel] \
   -sticky w -row 0 -column 1
SetScrollbarBindings $statsframe.left.points.points \
   .gameframe.paned.statsframe.scrolly
grid [ttk::frame $statsframe.left.time] -sticky w
SetScrollbarBindings $statsframe.left.time .gameframe.paned.statsframe.scrolly
grid [ttk::label $statsframe.left.time.lbltime -text {Time passed:}] -sticky w
SetScrollbarBindings $statsframe.left.time.lbltime .gameframe.paned.statsframe.scrolly
grid [ttk::label $statsframe.left.time.time -style Golden.TLabel] -sticky w \
   -row 0 -column 1
SetScrollbarBindings $statsframe.left.time.time .gameframe.paned.statsframe.scrolly
grid [ttk::frame $statsframe.left.bases] -sticky w
SetScrollbarBindings $statsframe.left.bases \
   .gameframe.paned.statsframe.scrolly
grid [ttk::label $statsframe.left.bases.lblbases -text {Bases visited:}] -sticky w
SetScrollbarBindings $statsframe.left.bases.lblbases \
   .gameframe.paned.statsframe.scrolly
grid [ttk::label $statsframe.left.bases.bases -style Golden.TLabel] -sticky w \
   -row 0 -column 1
SetScrollbarBindings $statsframe.left.bases.bases .gameframe.paned.statsframe.scrolly
grid [ttk::frame $statsframe.left.map] -sticky w
SetScrollbarBindings $statsframe.left.map .gameframe.paned.statsframe.scrolly
grid [ttk::label $statsframe.left.map.lblmap -text {Map discovered:}] -sticky w
SetScrollbarBindings $statsframe.left.map.lblmap .gameframe.paned.statsframe.scrolly
grid [ttk::label $statsframe.left.map.map -style Golden.TLabel] -sticky w \
   -row 0 -column 1
SetScrollbarBindings $statsframe.left.map.map .gameframe.paned.statsframe.scrolly
grid [ttk::frame $statsframe.left.distance] -sticky w
SetScrollbarBindings $statsframe.left.distance \
   .gameframe.paned.statsframe.scrolly
grid [ttk::label $statsframe.left.distance.lbldistance -text {Distance traveled:}] -sticky w
SetScrollbarBindings $statsframe.left.distance.lbldistance \
   .gameframe.paned.statsframe.scrolly
grid [ttk::label $statsframe.left.distance.distance -style Golden.TLabel] -sticky w \
   -row 0 -column 1
SetScrollbarBindings $statsframe.left.distance.distance .gameframe.paned.statsframe.scrolly
grid [ttk::frame $statsframe.left.crafts] -sticky w
SetScrollbarBindings $statsframe.left.crafts .gameframe.paned.statsframe.scrolly
grid [ttk::label $statsframe.left.crafts.lblcrafts -text {Crafting orders finished:}] -sticky w
SetScrollbarBindings $statsframe.left.crafts.lblcrafts .gameframe.paned.statsframe.scrolly
grid [ttk::label $statsframe.left.crafts.crafts -style Golden.TLabel] -sticky w \
   -row 0 -column 1
SetScrollbarBindings $statsframe.left.crafts.crafts .gameframe.paned.statsframe.scrolly
grid [ttk::frame $statsframe.left.craftsframe] -sticky w
grid [ttk::treeview $statsframe.left.craftsframe.craftsview -show headings \
   -columns [list name amount] -selectmode none \
   -yscrollcommand [list $statsframe.left.craftsframe.scrolly set]] \
   -sticky nwes
$statsframe.left.craftsframe.craftsview heading name -text {Name} \
   -command {SortFinishedCrafting 1}
$statsframe.left.craftsframe.craftsview column name -width 250
$statsframe.left.craftsframe.craftsview heading amount -text {Amount} \
   -command {SortFinishedCrafting 2}
$statsframe.left.craftsframe.craftsview column amount -width 75 -anchor center
tooltip::tooltip $statsframe.left.craftsframe.craftsview \
   {The list of finished crafting orders}
grid [ttk::scrollbar $statsframe.left.craftsframe.scrolly -orient vertical \
   -command [list $statsframe.left.craftsframe.craftsview yview]] -row 0 \
   -column 1 -sticky ns
::autoscroll::autoscroll $statsframe.left.craftsframe.scrolly
grid [ttk::frame $statsframe.left.missions] -sticky w
SetScrollbarBindings $statsframe.left.missions \
   .gameframe.paned.statsframe.scrolly
grid [ttk::label $statsframe.left.missions.lblmissions -text {Missions completed:}] -sticky w
SetScrollbarBindings $statsframe.left.missions.lblmissions \
   .gameframe.paned.statsframe.scrolly
grid [ttk::label $statsframe.left.missions.missions -style Golden.TLabel] -sticky w \
   -row 0 -column 1
SetScrollbarBindings $statsframe.left.missions.missions \
   .gameframe.paned.statsframe.scrolly
grid [ttk::frame $statsframe.left.missionsframe] -sticky w
grid [ttk::treeview $statsframe.left.missionsframe.missionsview \
   -show headings -columns [list name amount] -selectmode none \
   -yscrollcommand [list $statsframe.left.missionsframe.scrolly set]]
$statsframe.left.missionsframe.missionsview heading name -text {Name} \
   -command {SortFinishedMissions 1}
$statsframe.left.missionsframe.missionsview column name -width 250
$statsframe.left.missionsframe.missionsview heading amount -text {Amount} \
   -command {SortFinishedMissions 2}
$statsframe.left.missionsframe.missionsview column amount -width 75 \
   -anchor center
tooltip::tooltip $statsframe.left.missionsframe.missionsview \
   {The list of finished missions}
grid [ttk::scrollbar $statsframe.left.missionsframe.scrolly -orient vertical \
   -command [list $statsframe.left.missionsframe.missionsview yview]] -row 0 \
   -column 1 -sticky ns
::autoscroll::autoscroll $statsframe.left.missionsframe.scrolly
grid [ttk::button $statsframe.left.goal -text {Goals} \
   -command {ShowGoals $statsframe.left.goal}] -sticky w
grid [ttk::frame $statsframe.left.goals] -sticky w
SetScrollbarBindings $statsframe.left.goals .gameframe.paned.statsframe.scrolly
grid [ttk::label $statsframe.left.goals.lblgoals -text {Finished goals:}] -sticky w
SetScrollbarBindings $statsframe.left.goals.lblgoals .gameframe.paned.statsframe.scrolly
grid [ttk::label $statsframe.left.goals.goals -style Golden.TLabel] -sticky w \
   -row 0 -column 1
SetScrollbarBindings $statsframe.left.goals.goals .gameframe.paned.statsframe.scrolly
grid [ttk::frame $statsframe.left.goalsframe] -sticky w
grid [ttk::treeview $statsframe.left.goalsframe.goalsview -show headings \
   -columns [list name amount] -selectmode none \
   -yscrollcommand [list $statsframe.left.goalsframe.scrolly set]]
$statsframe.left.goalsframe.goalsview heading name -text {Name} \
   -command {SortFinishedGoals 1}
$statsframe.left.goalsframe.goalsview column name -width 250
$statsframe.left.goalsframe.goalsview heading amount -text {Amount} \
   -command {SortFinishedGoals 2}
$statsframe.left.goalsframe.goalsview column amount -width 75 -anchor center
tooltip::tooltip $statsframe.left.goalsframe.goalsview \
   {The list of finished goals}
grid [ttk::scrollbar $statsframe.left.goalsframe.scrolly -orient vertical \
   -command [list $statsframe.left.goalsframe.goalsview yview]] -row 0 \
   -column 1 -sticky ns
::autoscroll::autoscroll $statsframe.left.goalsframe.scrolly
grid [ttk::frame $statsframe.right] -row 0 -column 1 -sticky nwes -padx 5
SetScrollbarBindings $statsframe.right .gameframe.paned.statsframe.scrolly
grid [ttk::frame $statsframe.right.destroyed] -sticky w
SetScrollbarBindings $statsframe.right.destroyed \
   .gameframe.paned.statsframe.scrolly
grid [ttk::label $statsframe.right.destroyed.lbldestroyed -text {Destroyed ships:}] -sticky w
SetScrollbarBindings $statsframe.right.destroyed.lbldestroyed \
   .gameframe.paned.statsframe.scrolly
grid [ttk::label $statsframe.right.destroyed.destroyed -style Golden.TLabel] -sticky w \
   -row 0 -column 1
SetScrollbarBindings $statsframe.right.destroyed.destroyed \
   .gameframe.paned.statsframe.scrolly
grid [ttk::frame $statsframe.right.destroyedframe] -sticky w
grid [ttk::treeview $statsframe.right.destroyedframe.destroyedview \
   -show headings -columns [list name amount] -selectmode none \
   -yscrollcommand [list $statsframe.right.destroyedframe.scrolly set]]
$statsframe.right.destroyedframe.destroyedview heading name -text {Name} \
   -command {SortDestroyedShips 1}
$statsframe.right.destroyedframe.destroyedview column name -width 250
$statsframe.right.destroyedframe.destroyedview heading amount -text {Amount} \
   -command {SortDestroyedShips 2}
$statsframe.right.destroyedframe.destroyedview column amount -width 75 \
   -anchor center
tooltip::tooltip $statsframe.right.destroyedframe.destroyedview \
   {The list of destroyed ships}
grid [ttk::scrollbar $statsframe.right.destroyedframe.scrolly \
   -orient vertical \
   -command [list $statsframe.right.destroyedframe.destroyedview yview]] \
   -row 0 -column 1 -sticky ns
::autoscroll::autoscroll $statsframe.right.destroyedframe.scrolly
grid [ttk::frame $statsframe.right.killed] -sticky w
SetScrollbarBindings $statsframe.right.killed \
   .gameframe.paned.statsframe.scrolly
grid [ttk::label $statsframe.right.killed.lblkilled -text {Killed enemies:}] -sticky w
SetScrollbarBindings $statsframe.right.killed.lblkilled \
   .gameframe.paned.statsframe.scrolly
grid [ttk::label $statsframe.right.killed.killed -style Golden.TLabel] -sticky w \
   -row 0 -column 1
SetScrollbarBindings $statsframe.right.killed.killed \
   .gameframe.paned.statsframe.scrolly
grid [ttk::frame $statsframe.right.killedframe] -sticky w
grid [ttk::treeview $statsframe.right.killedframe.killedview -show headings \
   -columns [list name amount] -selectmode none \
   -yscrollcommand [list $statsframe.right.killedframe.scrolly set]]
$statsframe.right.killedframe.killedview heading name -text {Name} \
   -command {SortKilledMobs 1}
$statsframe.right.killedframe.killedview column name -width 250
$statsframe.right.killedframe.killedview heading amount -text {Amount} \
   -command {SortKilledMobs 2}
$statsframe.right.killedframe.killedview column amount -width 75 \
   -anchor center
tooltip::tooltip $statsframe.right.killedframe.killedview \
   {The list of killed enemies in melee combat}
grid [ttk::scrollbar $statsframe.right.killedframe.scrolly -orient vertical \
   -command [list $statsframe.right.killedframe.destroyedview yview]] -row 0 \
   -column 1 -sticky ns
::autoscroll::autoscroll $statsframe.right.killedframe.scrolly
grid columnconfigure .gameframe.paned.statsframe 0 -weight 1
