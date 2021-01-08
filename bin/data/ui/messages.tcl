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

ttk::frame .gameframe.paned.messagesframe
set messagescanvas [canvas .gameframe.paned.messagesframe.canvas -xscrollcommand [list .gameframe.paned.messagesframe.scrollx set]]
pack $messagescanvas -side top -fill both
pack [ttk::scrollbar .gameframe.paned.messagesframe.scrollx -orient horizontal -command [list $messagescanvas xview]] -fill x
::autoscroll::autoscroll .gameframe.paned.messagesframe.scrollx
set messagesframe [ttk::frame $messagescanvas.messages]
# Messages options
grid [ttk::frame $messagesframe.options] -sticky w
grid [ttk::combobox $messagesframe.options.types -values [list All Combat Trade Orders Craft Others Missions] -state readonly -width 10]
bind $messagesframe.options.types <<ComboboxSelected>> SelectMessages
$messagesframe.options.types current 0
grid [ttk::entry $messagesframe.options.search -validate key -validatecommand {SearchMessages %P} -width 30] -row 0 -column 1
tooltip::tooltip $messagesframe.options.search {Search for the selected text in the messages.}
grid [ttk::button $messagesframe.options.delete -text {Delete all messages} -command DeleteMessages] -row 0 -column 2
# Messages list
grid [ttk::frame $messagesframe.list] -sticky nwes
set messagesview2 [text $messagesframe.list.view -width 10 -height 10 -yscrollcommand [list $messagesframe.list.scrolly set]]
$messagesview2 tag configure yellow -foreground yellow
$messagesview2 tag configure green -foreground #4e9a06
$messagesview2 tag configure red -foreground red
$messagesview2 tag configure cyan -foreground cyan
$messagesview2 tag configure blue -foreground #3465a4
$messagesview2 tag configure gray -foreground {dim gray}
pack [ttk::scrollbar $messagesframe.list.scrolly -orient vertical -command [list $messagesview2 yview]] -side right -fill y
pack $messagesview2 -side top -fill both -expand true
::autoscroll::autoscroll $messagesframe.list.scrolly
bind $messagescanvas <Configure> {
   $messagesview2 configure -height [expr [winfo height $messagescanvas] / [font metrics InterfaceFont -linespace] - 1]
}
