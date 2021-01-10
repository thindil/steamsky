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

ttk::frame .gameframe.paned.tradeframe
set tradecanvas [canvas .gameframe.paned.tradeframe.canvas -yscrollcommand [list .gameframe.paned.tradeframe.scrolly set] -xscrollcommand [list .gameframe.paned.tradeframe.scrollx set]]
pack [ttk::scrollbar .gameframe.paned.tradeframe.scrolly -orient vertical -command [list $tradecanvas yview]] -side right -fill y
pack $tradecanvas -side top -fill both
pack [ttk::scrollbar .gameframe.paned.tradeframe.scrollx -orient horizontal -command [list $tradecanvas xview]] -fill x
::autoscroll::autoscroll .gameframe.paned.tradeframe.scrolly
::autoscroll::autoscroll .gameframe.paned.tradeframe.scrollx
set tradeframe [ttk::frame $tradecanvas.trade]
# Type of items to show
grid [ttk::frame $tradeframe.options] -sticky w
grid [ttk::label $tradeframe.options.typelabel -text {Type:}]
grid [ttk::combobox $tradeframe.options.type -state readonly] -column 1 -row 0
bind $tradeframe.options.type <<ComboboxSelected>> {ShowTrade [$tradeframe.options.type get]}
grid [ttk::entry $tradeframe.options.search -validate key -validatecommand {SearchTrade %P}] -column 2 -row 0
grid [ttk::label $tradeframe.options.playerinfo -wraplength 300] -sticky nw -columnspan 2
grid [ttk::label $tradeframe.options.baseinfo -wraplength 300] -sticky nw -column 2 -row 1
# Trade list
set tradelist [ttk::frame $tradeframe.list]
grid $tradelist -sticky nwes
grid [ttk::label $tradelist.name -text Name]
grid [ttk::label $tradelist.type -text Type] -row 0 -column 1
grid [ttk::label $tradelist.durability -text Durability] -row 0 -column 2
grid [ttk::label $tradelist.price -text Price] -row 0 -column 3
grid [ttk::label $tradelist.profit -text Profit] -row 0 -column 4
grid [ttk::label $tradelist.owned -text Owned] -row 0 -column 5
grid [ttk::label $tradelist.available -text Available] -row 0 -column 6
