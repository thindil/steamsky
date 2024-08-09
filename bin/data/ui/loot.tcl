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

ttk::frame .gameframe.paned.lootframe
set lootcanvas [canvas .gameframe.paned.lootframe.canvas \
   -yscrollcommand [list .gameframe.paned.lootframe.scrolly set] \
   -xscrollcommand [list .gameframe.paned.lootframe.scrollx set]]
pack [ttk::scrollbar .gameframe.paned.lootframe.scrolly -orient vertical \
   -command [list $lootcanvas yview]] -side right -fill y
pack $lootcanvas -side top -fill both
SetScrollbarBindings $lootcanvas .gameframe.paned.lootframe.scrolly
pack [ttk::scrollbar .gameframe.paned.lootframe.scrollx -orient horizontal \
   -command [list $lootcanvas xview]] -fill x
::autoscroll::autoscroll .gameframe.paned.lootframe.scrolly
::autoscroll::autoscroll .gameframe.paned.lootframe.scrollx
set lootframe [ttk::frame $lootcanvas.loot]
SetScrollbarBindings $lootframe .gameframe.paned.lootframe.scrolly
# Type of items to show
grid [ttk::frame $lootframe.options] -sticky w -padx 5 -pady 5
SetScrollbarBindings $lootframe.options .gameframe.paned.lootframe.scrolly
grid [ttk::label $lootframe.options.typelabel -text {Type:}]
SetScrollbarBindings $lootframe.options.typelabel \
   .gameframe.paned.lootframe.scrolly
grid [ttk::combobox $lootframe.options.type -state readonly] -column 1 -row 0
bind $lootframe.options.type <<ComboboxSelected>> \
   {ShowLoot [$lootframe.options.type get]}
grid [ttk::frame $lootframe.options.info] -sticky nw \
   -columnspan 2
grid [ttk::label $lootframe.options.info.playerinfo -wraplength 300 \
   -text {Free cargo space: }]
grid [ttk::label $lootframe.options.info.playerinfo2 -wraplength 300 \
   -style Golden.TLabel] -row 0 -column 1
SetScrollbarBindings $lootframe.options.info \
   .gameframe.paned.tradeframe.scrolly
