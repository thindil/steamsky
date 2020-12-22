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

ttk::frame .gameframe.paned.baseframe
set basecanvas [canvas .gameframe.paned.baseframe.canvas -yscrollcommand [list .gameframe.paned.baseframe.scrolly set] -xscrollcommand [list .gameframe.paned.baseframe.scrollx set]]
pack [ttk::scrollbar .gameframe.paned.baseframe.scrolly -orient vertical -command [list $basecanvas yview]] -side right -fill y
pack $basecanvas -side top -fill both
pack [ttk::scrollbar .gameframe.paned.baseframe.scrollx -orient horizontal -command [list $basecanvas xview]] -fill x
set baseframe [ttk::frame $basecanvas.base]
grid [ttk::entry $baseframe.search -validate key -validatecommand {SearchRecipes %P}] -columnspan 2
# Items list
grid [ttk::frame $baseframe.items] -sticky nwes
set baseview [ttk::treeview $baseframe.items.view -yscrollcommand [list $baseframe.items.scrolly set]]
$baseview heading #0 -text {Name}
grid $baseview -sticky nwes
grid [ttk::scrollbar $baseframe.items.scrolly -orient vertical -command [list $baseview yview]] -row 0 -column 1 -sticky ns
# Item info
set infoframe [ttk::frame $baseframe.info]
grid [ttk::label $infoframe.info]
grid [ttk::label $infoframe.money]
grid [ttk::button $infoframe.accept]
grid $infoframe -column 1 -row 1
