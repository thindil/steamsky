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

ttk::frame .gameframe.paned.missionsframe
set missionscanvas [canvas .gameframe.paned.missionsframe.canvas -yscrollcommand [list .gameframe.paned.missionsframe.scrolly set] -xscrollcommand [list .gameframe.paned.missionsframe.scrollx set]]
pack [ttk::scrollbar .gameframe.paned.missionsframe.scrolly -orient vertical -command [list $missionscanvas yview]] -side right -fill y
pack $missionscanvas -side top -fill both
pack [ttk::scrollbar .gameframe.paned.missionsframe.scrollx -orient horizontal -command [list $missionscanvas xview]] -fill x
::autoscroll::autoscroll .gameframe.paned.missionsframe.scrolly
::autoscroll::autoscroll .gameframe.paned.missionsframe.scrollx
set missionsframe [ttk::frame $missionscanvas.missions]
grid [ttk::treeview $missionsframe.missionsview -show headings -columns [list name distance]] -sticky n -padx 5 -pady 5
$missionsframe.missionsview heading name -text {Name}
$missionsframe.missionsview column name -width 250
$missionsframe.missionsview heading distance -text {Distance}
$missionsframe.missionsview column distance -width 100 -anchor center
bind $missionsframe.missionsview <<TreeviewSelect>> ShowMissionInfo
grid [ttk::frame $missionsframe.info] -column 1 -row 0
grid [ttk::labelframe $missionsframe.info.info -text {Mission Info:}] -padx 5 -pady 5
grid [text $missionsframe.info.info.text -wrap char -height 10 -width 40] -padx 5 -pady 5
$missionsframe.info.info.text tag configure red -foreground red
$missionsframe.info.info.text tag configure yellow -foreground yellow
set reward 1.0
grid [ttk::frame $missionsframe.info.reward] -sticky we
grid [ttk::label $missionsframe.info.reward.label -text {Reward:}] -padx 5
grid [ttk::scale $missionsframe.info.reward.amount -from 0.0 -to 2.0 -variable reward -command ShowMissionInfo -length 300] -column 1 -row 0 -padx 5
tooltip::tooltip $missionsframe.info.reward "Move left - more reputation from mission but less money,\nmove right - more money from mission but less reputation."
grid [ttk::label $missionsframe.info.missioninfo]
grid [ttk::button $missionsframe.info.show -text {Show mission on map} -command {ShowMission}]
grid [ttk::button $missionsframe.info.set -command {SetMission}]
