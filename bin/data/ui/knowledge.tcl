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

set knowledgeframe [ttk::frame .gameframe.paned.knowledgeframe]
# Bases list
grid [ttk::labelframe $knowledgeframe.bases -text {Known bases:}] -sticky nwes -padx 4
set knowledgecanvas [canvas $knowledgeframe.bases.canvas -yscrollcommand [list $knowledgeframe.bases.scrolly set] -xscrollcommand [list $knowledgeframe.bases.scrollx set]]
pack [ttk::scrollbar $knowledgeframe.bases.scrolly -orient vertical -command [list $knowledgecanvas yview]] -side right -fill y
pack [ttk::scrollbar $knowledgeframe.bases.scrollx -orient horizontal -command [list $knowledgecanvas xview]] -fill x -side bottom
pack $knowledgecanvas -side top -fill both -expand true
ttk::frame $knowledgecanvas.frame
# Minimize/maximize button
grid [ttk::button $knowledgecanvas.frame.maxmin -style Small.TButton -text "[format %c 0xf106]" -command {KnowledgeMaxMin bases show}] -sticky w
tooltip::tooltip $knowledgecanvas.frame.maxmin {Maximize/minimize the list of known bases}
# List of bases options
grid [ttk::frame $knowledgecanvas.frame.options] -columnspan 6 -sticky w -padx 5
grid [ttk::label $knowledgecanvas.frame.options.typeslbl -text {Type:}]
grid [ttk::combobox $knowledgecanvas.frame.options.types -state readonly -width 10] -row 0 -column 1
bind $knowledgecanvas.frame.options.types <<ComboboxSelected>> {ShowBases}
tooltip::tooltip $knowledgecanvas.frame.options.types {Show only the selected type bases}
grid [ttk::label $knowledgecanvas.frame.options.statuslbl -text {Status:}] -row 0 -column 2
grid [ttk::combobox $knowledgecanvas.frame.options.status -state readonly -values [list {Any} {Only not visited} {Only visited}] -width 10] -row 0 -column 3
bind $knowledgecanvas.frame.options.status <<ComboboxSelected>> {ShowBases}
tooltip::tooltip $knowledgecanvas.frame.options.status {Show only the selected status bases}
$knowledgecanvas.frame.options.status current 0
grid [ttk::label $knowledgecanvas.frame.options.ownerlbl -text {Owner:}] -row 0 -column 4
grid [ttk::combobox $knowledgecanvas.frame.options.owner -state readonly -width 10] -row 0 -column 5
bind $knowledgecanvas.frame.options.owner <<ComboboxSelected>> {ShowBases}
tooltip::tooltip $knowledgecanvas.frame.options.owner {Show only the selected owner bases}
grid [ttk::label $knowledgecanvas.frame.options.searchlbl -text {Name:}]
grid [ttk::entry $knowledgecanvas.frame.options.search -validate key -validatecommand {ShowBases %P} -width 20] -row 1 -column 1 -columnspan 6 -sticky w
tooltip::tooltip $knowledgecanvas.frame.options.search {Search for a base with the selected name}
# List of bases
$knowledgecanvas create window 0 0 -anchor nw -window $knowledgecanvas.frame
::autoscroll::autoscroll $knowledgeframe.bases.scrolly
::autoscroll::autoscroll $knowledgeframe.bases.scrollx
# Accepted missions list
grid [ttk::labelframe $knowledgeframe.missions -text {Accepted missions:}] -sticky nwes -padx 4
set knowledgecanvas [canvas $knowledgeframe.missions.canvas -yscrollcommand [list $knowledgeframe.missions.scrolly set] -xscrollcommand [list $knowledgeframe.missions.scrollx set]]
pack [ttk::scrollbar $knowledgeframe.missions.scrolly -orient vertical -command [list $knowledgecanvas yview]] -side right -fill y
pack [ttk::scrollbar $knowledgeframe.missions.scrollx -orient horizontal -command [list $knowledgecanvas xview]] -fill x -side bottom
pack $knowledgecanvas -side top -fill both -expand true
ttk::frame $knowledgecanvas.frame
grid columnconfigure $knowledgecanvas.frame 1 -weight 1
# Minimize/maximize button
grid [ttk::button $knowledgecanvas.frame.maxmin -style Small.TButton -text "[format %c 0xf106]" -command {KnowledgeMaxMin missions show}] -sticky w
tooltip::tooltip $knowledgecanvas.frame.maxmin {Maximize/minimize the list of accepted missions}
$knowledgecanvas create window 0 0 -anchor nw -window $knowledgecanvas.frame
::autoscroll::autoscroll $knowledgeframe.missions.scrolly
::autoscroll::autoscroll $knowledgeframe.missions.scrollx
# Known events list
grid [ttk::labelframe $knowledgeframe.events -text {Known events:}] -row 0 -column 1 -sticky nwes -padx 4
set knowledgecanvas [canvas $knowledgeframe.events.canvas -yscrollcommand [list $knowledgeframe.events.scrolly set] -xscrollcommand [list $knowledgeframe.events.scrollx set]]
pack [ttk::scrollbar $knowledgeframe.events.scrolly -orient vertical -command [list $knowledgecanvas yview]] -side right -fill y
pack [ttk::scrollbar $knowledgeframe.events.scrollx -orient horizontal -command [list $knowledgecanvas xview]] -fill x -side bottom
pack $knowledgecanvas -side top -fill both -expand true
ttk::frame $knowledgecanvas.frame
grid columnconfigure $knowledgecanvas.frame 1 -weight 1
# Minimize/maximize button
grid [ttk::button $knowledgecanvas.frame.maxmin -style Small.TButton -text "[format %c 0xf106]" -command {KnowledgeMaxMin events show}] -sticky w
tooltip::tooltip $knowledgecanvas.frame.maxmin {Maximize/minimize the list of known events}
$knowledgecanvas create window 0 0 -anchor nw -window $knowledgecanvas.frame
::autoscroll::autoscroll $knowledgeframe.events.scrolly
::autoscroll::autoscroll $knowledgeframe.events.scrollx
# Known stories list
grid [ttk::labelframe $knowledgeframe.stories -text {Known stories:}] -row 1 -column 1 -sticky nwes -padx 4
set knowledgecanvas [canvas $knowledgeframe.stories.canvas -yscrollcommand [list $knowledgeframe.stories.scrolly set] -xscrollcommand [list $knowledgeframe.stories.scrollx set]]
pack [ttk::scrollbar $knowledgeframe.stories.scrolly -orient vertical -command [list $knowledgecanvas yview]] -side right -fill y
pack [ttk::scrollbar $knowledgeframe.stories.scrollx -orient horizontal -command [list $knowledgecanvas xview]] -fill x -side bottom
pack $knowledgecanvas -side top -fill both -expand true
ttk::frame $knowledgecanvas.frame
grid columnconfigure $knowledgecanvas.frame 1 -weight 1
# Minimize/maximize button
grid [ttk::button $knowledgecanvas.frame.maxmin -style Small.TButton -text "[format %c 0xf106]" -command {KnowledgeMaxMin stories show}] -sticky w
tooltip::tooltip $knowledgecanvas.frame.maxmin {Maximize/minimize the list of known stories}
$knowledgecanvas create window 0 0 -anchor nw -window $knowledgecanvas.frame
::autoscroll::autoscroll $knowledgeframe.stories.scrolly
::autoscroll::autoscroll $knowledgeframe.stories.scrollx

# Configure main knowledge info grid
grid columnconfigure $knowledgeframe 0 -weight 1
grid columnconfigure $knowledgeframe 1 -weight 1
grid rowconfigure $knowledgeframe 0 -weight 1
grid rowconfigure $knowledgeframe 1 -weight 1
