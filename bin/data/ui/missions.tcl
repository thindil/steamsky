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

ttk::frame .gameframe.paned.missionsframe
set missionscanvas [canvas .gameframe.paned.missionsframe.canvas \
   -yscrollcommand [list .gameframe.paned.missionsframe.scrolly set] \
   -xscrollcommand [list .gameframe.paned.missionsframe.scrollx set]]
pack [ttk::scrollbar .gameframe.paned.missionsframe.scrolly -orient vertical \
   -command [list $missionscanvas yview]] -side right -fill y
pack $missionscanvas -side top -fill both
pack [ttk::scrollbar .gameframe.paned.missionsframe.scrollx -orient horizontal \
   -command [list $missionscanvas xview]] -fill x
SetScrollbarBindings $missionscanvas .gameframe.paned.missionsframe.scrolly
::autoscroll::autoscroll .gameframe.paned.missionsframe.scrolly
::autoscroll::autoscroll .gameframe.paned.missionsframe.scrollx
set missionsframe [ttk::frame $missionscanvas.missions]
grid [ttk::frame $missionsframe.missions] -sticky n -padx 5 -pady 5
SetScrollbarBindings $missionsframe .gameframe.paned.missionsframe.scrolly
# Label with information how many missions are available in the base
grid [ttk::frame $missionsframe.missionslabel] -sticky w -padx 5
SetScrollbarBindings $missionsframe.missionslabel \
   .gameframe.paned.missionsframe.scrolly
grid [ttk::label $missionsframe.missionslabel.missionslbl1 -text {You can take }] \
   -sticky w
SetScrollbarBindings $missionsframe.missionslabel.missionslbl1 \
   .gameframe.paned.missionsframe.scrolly
grid [ttk::label $missionsframe.missionslabel.missionslbl2 -style Golden.TLabel] \
   -sticky w -column 1 -row 0
SetScrollbarBindings $missionsframe.missionslabel.missionslbl2 \
   .gameframe.paned.missionsframe.scrolly
grid [ttk::label $missionsframe.missionslabel.missionslbl3 -text { more missions in from base.}] \
   -sticky w -column 2 -row 0
SetScrollbarBindings $missionsframe.missionslabel.missionslbl3 \
   .gameframe.paned.missionsframe.scrolly
