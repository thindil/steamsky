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

ttk::frame .gameframe.paned.baseframe
set basecanvas [canvas .gameframe.paned.baseframe.canvas -yscrollcommand [list .gameframe.paned.baseframe.scrolly set] -xscrollcommand [list .gameframe.paned.baseframe.scrollx set]]
pack [ttk::scrollbar .gameframe.paned.baseframe.scrolly -orient vertical -command [list $basecanvas yview]] -side right -fill y
pack $basecanvas -side top -fill both
pack [ttk::scrollbar .gameframe.paned.baseframe.scrollx -orient horizontal -command [list $basecanvas xview]] -fill x
SetScrollbarBindings $basecanvas .gameframe.paned.baseframe.scrolly
::autoscroll::autoscroll .gameframe.paned.baseframe.scrolly
::autoscroll::autoscroll .gameframe.paned.baseframe.scrollx
set baseframe [ttk::frame $basecanvas.base]
SetScrollbarBindings $baseframe .gameframe.paned.baseframe.scrolly
grid [ttk::entry $baseframe.search -validate key -validatecommand {SearchRecipes %P}] -columnspan 2 -sticky w -padx 5 -pady 5
tooltip::tooltip $baseframe.search "Search for the selected recipe."
