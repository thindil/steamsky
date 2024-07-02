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

set shipinfoframe [ttk::frame .gameframe.paned.shipinfoframe]
# General ship info
grid [ttk::labelframe $shipinfoframe.general -text {General Info:}] \
   -sticky nwes -padx 4
set shipcanvas [canvas $shipinfoframe.general.canvas \
   -yscrollcommand [list $shipinfoframe.general.scrolly set] \
   -xscrollcommand [list $shipinfoframe.general.scrollx set]]
pack [ttk::scrollbar $shipinfoframe.general.scrolly -orient vertical \
   -command [list $shipcanvas yview]] -side right -fill y
pack [ttk::scrollbar $shipinfoframe.general.scrollx -orient horizontal \
   -command [list $shipcanvas xview]] -fill x -side bottom
pack $shipcanvas -side top -fill both -expand true
SetScrollbarBindings $shipcanvas $shipinfoframe.general.scrolly
ttk::frame $shipcanvas.frame
grid columnconfigure $shipcanvas.frame 1 -weight 1
SetScrollbarBindings $shipcanvas.frame $shipinfoframe.general.scrolly
# Minimize/maximize button
grid [ttk::button $shipcanvas.frame.maxmin -style Small.TButton \
   -image movemapupicon -command {ShipMaxMin general show}] -sticky w \
   -padx 5
tooltip::tooltip $shipcanvas.frame.maxmin \
   {Maximize/minimize the ship general info}
# Ship name
grid [ttk::label $shipcanvas.frame.namelbl -text {Name:}] -sticky w -padx 5
tooltip::tooltip $shipcanvas.frame.namelbl {The name of your ship}
SetScrollbarBindings $shipcanvas.frame.namelbl $shipinfoframe.general.scrolly
grid [ttk::label $shipcanvas.frame.name -textvariable shipname \
   -style Golden.TLabel] -column 1 -row 1 -sticky w
tooltip::tooltip $shipcanvas.frame.name {The name of your ship}
SetScrollbarBindings $shipcanvas.frame.name $shipinfoframe.general.scrolly
grid [ttk::button $shipcanvas.frame.rename -style Small.TButton -command {
      GetString {Enter a new name:} shipname {Renaming the ship} {Rename}
   }] -column 2 -row 1 -sticky w
tooltip::tooltip $shipcanvas.frame.rename {Set a new name for the ship}
bind $shipcanvas.frame.name <Enter> SetShipName
# Upgrade progress
grid [ttk::label $shipcanvas.frame.upgradelabel -text {Upgrade:}] -sticky w \
   -columnspan 3 -padx 5
SetScrollbarBindings $shipcanvas.frame.upgradelabel \
   $shipinfoframe.general.scrolly
grid [ttk::label $shipcanvas.frame.upgradelbl -style Golden.TLabel] -column 1 \
   -row 2 -columnspan 2 -sticky w
SetScrollbarBindings $shipcanvas.frame.upgradelbl \
   $shipinfoframe.general.scrolly
grid [ttk::progressbar $shipcanvas.frame.upgrade \
   -orient horizontal -maximum 1.0] -sticky we -columnspan 2 -padx 5
SetScrollbarBindings $shipcanvas.frame.upgrade $shipinfoframe.general.scrolly
tooltip::tooltip $shipcanvas.frame.upgrade \
   {The current ship's upgrade progress}
grid [ttk::button $shipcanvas.frame.cancelupgrade -style Small.TButton \
   -command StopUpgrading] -row 3 -column 2 -sticky w
tooltip::tooltip $shipcanvas.frame.cancelupgrade {Stop the current upgrade}
# Repair priority
grid [ttk::label $shipcanvas.frame.repairlabel] -sticky we -padx 5
tooltip::tooltip $shipcanvas.frame.repairlabel \
   {If damaged, the module will be repaired as the first}
SetScrollbarBindings $shipcanvas.frame.repairlabel \
   $shipinfoframe.general.scrolly
grid [ttk::label $shipcanvas.frame.repairlbl -style Golden.TLabel] -column 1 \
   -row 4 -sticky w
SetScrollbarBindings $shipcanvas.frame.upgradelbl \
   $shipinfoframe.general.scrolly
tooltip::tooltip $shipcanvas.frame.repairlbl \
   {If damaged, the module will be repaired as the first}
grid [ttk::button $shipcanvas.frame.cancelpriority -style Small.TButton \
   -command {SetRepair remove}] -row 4 -column 2 -sticky w
tooltip::tooltip $shipcanvas.frame.cancelpriority {Remove the repair priority}
# Ship destination
grid [ttk::label $shipcanvas.frame.destinationlabel] -sticky we -padx 5
tooltip::tooltip $shipcanvas.frame.destinationlabel \
   {The current travel destination of your ship}
SetScrollbarBindings $shipcanvas.frame.destinationlabel \
   $shipinfoframe.general.scrolly
grid [ttk::label $shipcanvas.frame.destinationlbl -style Golden.TLabel] -column 1 \
   -row 5 -sticky w
tooltip::tooltip $shipcanvas.frame.destinationlbl \
   {The current travel destination of your ship}
SetScrollbarBindings $shipcanvas.frame.upgradelbl \
   $shipinfoframe.general.scrolly
grid [ttk::button $shipcanvas.frame.canceldestination -style Small.TButton \
   -command {ResetDestination}] -row 5 -column 2 -sticky w
tooltip::tooltip $shipcanvas.frame.canceldestination \
   {Reset the ship destination}
# Ship home base
grid [ttk::label $shipcanvas.frame.homelabel] -sticky we -padx 5
tooltip::tooltip $shipcanvas.frame.homelabel {Your ship the current home base}
SetScrollbarBindings $shipcanvas.frame.homelabel $shipinfoframe.general.scrolly
grid [ttk::label $shipcanvas.frame.homelbl -style Golden.TLabel] -sticky we \
   -padx 5 -row 6 -column 1
tooltip::tooltip $shipcanvas.frame.homelbl {Your ship the current home base}
SetScrollbarBindings $shipcanvas.frame.homelbl $shipinfoframe.general.scrolly
grid [ttk::button $shipcanvas.frame.showhome -style Small.TButton \
   -command {ShowShipInfo;update;MoveMap centeronhome}] -row 6 -column 2 \
   -sticky w
tooltip::tooltip $shipcanvas.frame.showhome {Show the home base on map}
# Ship weight
grid [ttk::label $shipcanvas.frame.weight] -sticky we -padx 5
tooltip::tooltip $shipcanvas.frame.weight \
   "The ship weight. The more heavy is ship, the slower it fly\nand need stronger engines"
SetScrollbarBindings $shipcanvas.frame.weight $shipinfoframe.general.scrolly
grid [ttk::label $shipcanvas.frame.weight2 -style Golden.TLabel] -columnspan 2 \
   -sticky w -padx 5 -row 7 -column 1
tooltip::tooltip $shipcanvas.frame.weight2 \
   "The ship weight. The more heavy is ship, the slower it fly\nand need stronger engines"
SetScrollbarBindings $shipcanvas.frame.weight2 $shipinfoframe.general.scrolly
$shipcanvas create window 0 0 -anchor nw -window $shipcanvas.frame
::autoscroll::autoscroll $shipinfoframe.general.scrolly
::autoscroll::autoscroll $shipinfoframe.general.scrollx
# Ship modules
grid [ttk::labelframe $shipinfoframe.modules -text {Modules Info:}] \
   -sticky nwes -padx 4
set shipcanvas [canvas $shipinfoframe.modules.canvas \
   -yscrollcommand [list $shipinfoframe.modules.scrolly set] \
   -xscrollcommand [list $shipinfoframe.modules.scrollx set]]
pack [ttk::scrollbar $shipinfoframe.modules.scrolly -orient vertical \
   -command [list $shipcanvas yview]] -side right -fill y
pack [ttk::scrollbar $shipinfoframe.modules.scrollx -orient horizontal \
   -command [list $shipcanvas xview]] -fill x -side bottom
pack $shipcanvas -side top -fill both -expand true
SetScrollbarBindings $shipcanvas $shipinfoframe.modules.scrolly
ttk::frame $shipcanvas.frame
$shipcanvas create window 0 0 -anchor nw -window $shipcanvas.frame
grid columnconfigure $shipcanvas.frame 1 -weight 1
SetScrollbarBindings $shipcanvas.frame $shipinfoframe.modules.scrolly
grid [ttk::button $shipcanvas.frame.maxmin -style Small.TButton \
   -image movemapupicon -command {ShipMaxMin modules show}] -sticky w \
   -padx 5
tooltip::tooltip $shipcanvas.frame.maxmin \
   {Maximize/minimize the ship modules info}
::autoscroll::autoscroll $shipinfoframe.modules.scrolly
::autoscroll::autoscroll $shipinfoframe.modules.scrollx
# Crew info
grid [ttk::labelframe $shipinfoframe.crew -text {Crew Info:}] -row 0 -column 1 \
   -sticky nwes -padx 4
set shipcanvas [canvas $shipinfoframe.crew.canvas \
   -yscrollcommand [list $shipinfoframe.crew.scrolly set] \
   -xscrollcommand [list $shipinfoframe.crew.scrollx set]]
pack [ttk::scrollbar $shipinfoframe.crew.scrolly -orient vertical \
   -command [list $shipcanvas yview]] -side right -fill y
pack [ttk::scrollbar $shipinfoframe.crew.scrollx -orient horizontal \
   -command [list $shipcanvas xview]] -fill x -side bottom
pack $shipcanvas -side top -fill both -expand true
SetScrollbarBindings $shipcanvas $shipinfoframe.crew.scrolly
ttk::frame $shipcanvas.frame
grid columnconfigure $shipcanvas.frame 1 -weight 1
SetScrollbarBindings $shipcanvas.frame $shipinfoframe.crew.scrolly
grid [ttk::button $shipcanvas.frame.maxmin -style Small.TButton \
   -image movemapupicon -command {ShipMaxMin crew show}] -sticky w \
   -padx 5
tooltip::tooltip $shipcanvas.frame.maxmin \
   {Maximize/minimize the ship crew info}
$shipcanvas create window 0 0 -anchor nw -window $shipcanvas.frame
::autoscroll::autoscroll $shipinfoframe.crew.scrolly
::autoscroll::autoscroll $shipinfoframe.crew.scrollx
# Cargo info
grid [ttk::labelframe $shipinfoframe.cargo -text {Cargo Info:}] -row 1 \
   -column 1 -sticky nwes -padx 4
set shipcanvas [canvas $shipinfoframe.cargo.canvas \
   -yscrollcommand [list $shipinfoframe.cargo.scrolly set] \
   -xscrollcommand [list $shipinfoframe.cargo.scrollx set]]
pack [ttk::scrollbar $shipinfoframe.cargo.scrolly -orient vertical \
   -command [list $shipcanvas yview]] -side right -fill y
pack [ttk::scrollbar $shipinfoframe.cargo.scrollx -orient horizontal \
   -command [list $shipcanvas xview]] -fill x -side bottom
pack $shipcanvas -side top -fill both -expand true
SetScrollbarBindings $shipcanvas $shipinfoframe.cargo.scrolly
ttk::frame $shipcanvas.frame
grid columnconfigure $shipcanvas.frame 1 -weight 1
SetScrollbarBindings $shipcanvas.frame $shipinfoframe.cargo.scrolly
grid [ttk::button $shipcanvas.frame.maxmin -style Small.TButton \
   -image movemapupicon -command {ShipMaxMin cargo show}] -sticky w \
   -padx 5
tooltip::tooltip $shipcanvas.frame.maxmin \
   {Maximize/minimize the ship cargo info}
grid [ttk::frame $shipcanvas.frame.freeframe] -sticky w -padx 5
grid [ttk::label $shipcanvas.frame.freeframe.freespace -text {Free cargo space:}] -padx 5
SetScrollbarBindings $shipcanvas.frame.freeframe.freespace $shipinfoframe.cargo.scrolly
grid [ttk::label $shipcanvas.frame.freeframe.freespace2 -style Golden.TLabel] \
   -padx 5 -row 0 -column 1
SetScrollbarBindings $shipcanvas.frame.freeframe.freespace2 $shipinfoframe.cargo.scrolly
grid [ttk::frame $shipcanvas.frame.selecttype] -sticky w
grid [ttk::label $shipcanvas.frame.selecttype.label -text {Type:}] -padx 5
SetScrollbarBindings $shipcanvas.frame.selecttype.label \
   $shipinfoframe.cargo.scrolly
grid [ttk::combobox $shipcanvas.frame.selecttype.combo -state readonly] -row 0 \
   -column 1
bind $shipcanvas.frame.selecttype.combo <<ComboboxSelected>> ShowCargo
tooltip::tooltip $shipcanvas.frame.selecttype \
   {Show only items with the selected type}
$shipcanvas create window 0 0 -anchor nw -window $shipcanvas.frame
::autoscroll::autoscroll $shipinfoframe.cargo.scrolly
::autoscroll::autoscroll $shipinfoframe.cargo.scrollx
# Configure main ship info grid
grid columnconfigure $shipinfoframe 0 -weight 1
grid columnconfigure $shipinfoframe 1 -weight 1
grid rowconfigure $shipinfoframe 0 -weight 1
grid rowconfigure $shipinfoframe 1 -weight 1
