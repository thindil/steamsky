# Copyright (c) 2020-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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

ttk::frame .gameframe.paned.craftframe
set craftcanvas [canvas .gameframe.paned.craftframe.canvas \
   -yscrollcommand [list .gameframe.paned.craftframe.scrolly set] \
   -xscrollcommand [list .gameframe.paned.craftframe.scrollx set]]
pack [ttk::scrollbar .gameframe.paned.craftframe.scrolly -orient vertical \
   -command [list $craftcanvas yview]] -side right -fill y
pack $craftcanvas -side top -fill both
pack [ttk::scrollbar .gameframe.paned.craftframe.scrollx -orient horizontal \
   -command [list $craftcanvas xview]] -fill x
::autoscroll::autoscroll .gameframe.paned.craftframe.scrolly
::autoscroll::autoscroll .gameframe.paned.craftframe.scrollx
set craftframe [ttk::frame $craftcanvas.craft]
grid [ttk::frame $craftframe.sframe] -sticky w
grid [ttk::label $craftframe.sframe.searchlabel -text {Name:}]
tooltip::tooltip $craftframe.sframe.searchlabel \
   {Search for the selected recipe.}
grid [ttk::entry $craftframe.sframe.search -validate key \
   -validatecommand {ShowCrafting 1 %P} -width 30] -sticky w -row 0 -column 1
tooltip::tooltip $craftframe.sframe.search {Search for the selected recipe.}
grid [ttk::label $craftframe.sframe.showlabel -text {Show:}]
tooltip::tooltip $craftframe.sframe.showlabel \
   {Show only the selected type of recipes.}
grid [ttk::combobox $craftframe.sframe.show \
   -values [list {All} {Craftable only} {Non-craftable only}] -width 15 -state readonly] \
   -sticky w -row 1 -column 1
tooltip::tooltip $craftframe.sframe.show {Show only the selected type of recipes.}
$craftframe.sframe.show current 0
bind $craftframe.sframe.show <<ComboboxSelected>> {ShowCrafting 1}
SetScrollbarBindings $craftcanvas .gameframe.paned.craftframe.scrolly
SetScrollbarBindings $craftframe .gameframe.paned.craftframe.scrolly
