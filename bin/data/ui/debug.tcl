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

toplevel .debugdialog -class Dialog -background [ttk::style lookup . -background] -relief solid -borderwidth 2
wm title .debugdialog {Steam Sky - Debug menu}
if {$tcl_platform(os) == "Linux"} {
   wm attributes .debugdialog -type dialog
}
grid [ttk::frame .debugdialog.buttons]
grid [ttk::frame .debugdialog.main] -column 1 -row 0
proc ShowFrame {framename} {
   Refresh
   grid remove [grid slaves .debugdialog.main]
   grid $framename
}
grid [ttk::button .debugdialog.buttons.ship -text Ship -command {ShowFrame .debugdialog.main.ship}]
grid [ttk::button .debugdialog.buttons.crew -text Crew -command {ShowFrame .debugdialog.main.crew}]
grid [ttk::button .debugdialog.buttons.cargo -text Cargo -command {ShowFrame .debugdialog.main.cargo}]
grid [ttk::button .debugdialog.buttons.bases -text Bases -command {ShowFrame .debugdialog.main.bases}]
grid [ttk::button .debugdialog.buttons.world -text World -command {ShowFrame .debugdialog.main.world}]
grid [ttk::button .debugdialog.buttons.refresh -text Refresh -command Refresh]
grid [ttk::button .debugdialog.buttons.save -text {Save game} -command DebugSaveGame]
# Ship options
set shipframe [ttk::frame .debugdialog.main.ship]
grid [ttk::button $shipframe.move -text {Move ship} -command DebugMoveShip]
grid [ttk::label $shipframe.lblx -text {X:}] -column 1 -row 0
grid [ttk::spinbox $shipframe.x -from 1 -to 1024 -validate key -validatecommand {ValidateSpinbox %W %P}] -column 2 -row 0
grid [ttk::label $shipframe.lbly -text {Y:}] -column 3 -row 0
grid [ttk::spinbox $shipframe.y -from 1 -to 1024 -validate key -validatecommand {ValidateSpinbox %W %P}] -column 4 -row 0
grid [ttk::label $shipframe.modulelbl -text {Module:}]
grid [ttk::combobox $shipframe.module -state readonly] -column 1 -row 1 -columnspan 3
bind $shipframe.module <<ComboboxSelected>> RefreshModule
grid [ttk::label $shipframe.protolbl -text {Prototype:}]
grid [ttk::entry $shipframe.proto]  -column 1 -row 2 -columnspan 3
grid [ttk::label $shipframe.weightlbl -text {Weight:}]
grid [ttk::spinbox $shipframe.weight -from 0 -to 100000 -validate key -validatecommand {ValidateSpinbox %W %P}] -column 1 -row 3 -columnspan 3
grid [ttk::label $shipframe.durlbl -text {Durability:}]
grid [ttk::spinbox $shipframe.dur -from 0 -to 1000 -validate key -validatecommand {ValidateSpinbox %W %P}] -column 1 -row 4 -columnspan 3
grid [ttk::label $shipframe.maxdurlbl -text {Max durability:}]
grid [ttk::spinbox $shipframe.maxdur -from 0 -to 1000 -validate key -validatecommand {ValidateSpinbox %W %P}] -column 1 -row 5 -columnspan 3
grid [ttk::label $shipframe.upgradelbl -text {Upgrade progress:}]
grid [ttk::spinbox $shipframe.upgrade -from 0 -to 1000000 -validate key -validatecommand {ValidateSpinbox %W %P}] -column 1 -row 6 -columnspan 3
grid [ttk::button $shipframe.change -text Change -command DebugUpdateModule] -columnspan 4
# Crew options
set crewframe [ttk::frame .debugdialog.main.crew]
grid [ttk::label $crewframe.memberlbl -text Member]
grid [ttk::combobox $crewframe.member -state readonly] -column 1 -row 0
bind $crewframe.member <<ComboboxSelected>> RefreshMember
grid [ttk::label $crewframe.healthlbl -text Health]
grid [ttk::spinbox $crewframe.health -from 1 -to 100 -validate key -validatecommand {ValidateSpinbox %W %P}] -column 1 -row 1
grid [ttk::label $crewframe.thirstlbl -text Thirst]
grid [ttk::spinbox $crewframe.thirst -from 0 -to 100 -validate key -validatecommand {ValidateSpinbox %W %P}] -column 1 -row 2
grid [ttk::label $crewframe.hungerlbl -text Hunger]
grid [ttk::spinbox $crewframe.hunger -from 0 -to 100 -validate key -validatecommand {ValidateSpinbox %W %P}] -column 1 -row 3
grid [ttk::label $crewframe.tiredlbl -text Tired]
grid [ttk::spinbox $crewframe.tired -from 0 -to 100 -validate key -validatecommand {ValidateSpinbox %W %P}] -column 1 -row 4
grid [ttk::label $crewframe.moralelbl -text Morale]
grid [ttk::spinbox $crewframe.morale -from 0 -to 100 -validate key -validatecommand {ValidateSpinbox %W %P}] -column 1 -row 5
grid [ttk::label $crewframe.loyaltylbl -text Loyalty]
grid [ttk::spinbox $crewframe.loyalty -from 0 -to 100 -validate key -validatecommand {ValidateSpinbox %W %P}] -column 1 -row 6
grid [ttk::frame $crewframe.stats] -column 2 -row 1
grid [ttk::label $crewframe.stats.name -text Name]
grid [ttk::label $crewframe.stats.level -text Level] -column 1 -row 0
grid [ttk::frame $crewframe.skills] -column 3 -row 1
grid [ttk::label $crewframe.skills.name -text Name]
grid [ttk::label $crewframe.skills.level -text Level] -column 1 -row 0
grid [ttk::frame $crewframe.addskill] -column 3 -row 6
grid [ttk::button $crewframe.addskill.add -text Add -command DebugAddSkill]
grid [ttk::combobox $crewframe.addskill.skills -state readonly] -column 1 -row 0
grid [ttk::button $crewframe.change -text Change -command DebugUpdateMember] -columnspan 4
# Cargo options
set cargoframe [ttk::frame .debugdialog.main.cargo]
grid [ttk::button $cargoframe.addbutton -text Add -command DebugAddItem]
grid [ttk::entry $cargoframe.add] -column 1 -row 0
grid [ttk::label $cargoframe.amountlbl -text {Amount:}] -column 2 -row 0
grid [ttk::spinbox $cargoframe.amount -from 1 -to 1000000 -validate key -validatecommand {ValidateSpinbox %W %P}] -column 3 -row 0
grid [ttk::button $cargoframe.updatebutton -text Update -command DebugUpdateItem]
grid [ttk::combobox $cargoframe.update -state readonly] -column 1 -row 1
grid [ttk::label $cargoframe.amount2lbl -text {Amount:}] -column 2 -row 1
grid [ttk::spinbox $cargoframe.updateamount -from 1 -to 1000000 -validate key -validatecommand {ValidateSpinbox %W %P}] -column 3 -row 1
bind $cargoframe.update <<ComboboxSelected>> RefreshCargo
# Bases options
set basesframe [ttk::frame .debugdialog.main.bases]
grid [ttk::label $basesframe.lbl1 -text {Base:}]
grid [ttk::entry $basesframe.name] -column 1 -row 0
bind $basesframe.name <Return> RefreshBase
grid [ttk::label $basesframe.lbl2 -text {Type:}]
grid [ttk::combobox $basesframe.type -state readonly] -column 1 -row 1
grid [ttk::label $basesframe.lbl3 -text {Owner:}]
grid [ttk::combobox $basesframe.owner -state readonly] -column 1 -row 2
grid [ttk::label $basesframe.lbl4 -text {Size:}]
grid [ttk::combobox $basesframe.size -state readonly -values [list Small Medium Big]] -column 1 -row 3
grid [ttk::label $basesframe.lbl5 -text {Population:}]
grid [ttk::spinbox $basesframe.population -from 0 -to 10000 -validate key -validatecommand {ValidateSpinbox %W %P}] -column 1 -row 4
grid [ttk::label $basesframe.lbl6 -text {Reputation:}]
grid [ttk::spinbox $basesframe.reputation -from -100 -to 100 -validate key -validatecommand {ValidateSpinbox %W %P}] -column 1 -row 5
grid [ttk::label $basesframe.lbl7 -text {Money:}]
grid [ttk::spinbox $basesframe.money -from 1 -to 1000000 -validate key -validatecommand {ValidateSpinbox %W %P}] -column 1 -row 6
grid [ttk::button $basesframe.update -text {Update} -command DebugUpdateBase] -columnspan 2
# World options
set worldframe [ttk::frame .debugdialog.main.world]
grid [ttk::label $worldframe.shiplbl -text {Ship:}]
grid [ttk::entry $worldframe.ship] -column 1 -row 0
grid [ttk::label $worldframe.xlbl -text {X:}]
grid [ttk::spinbox $worldframe.x -from 1 -to 1024 -validate key -validatecommand {ValidateSpinbox %W %P}] -column 1 -row 1
$worldframe.x set 1
grid [ttk::label $worldframe.ylbl -text {Y:}]
grid [ttk::spinbox $worldframe.y -from 1 -to 1024 -validate key -validatecommand {ValidateSpinbox %W %P}] -column 1 -row 2
$worldframe.y set 1
grid [ttk::label $worldframe.durationlbl -text {Duration:}]
grid [ttk::spinbox $worldframe.duration -from 60 -to 1000 -validate key -validatecommand {ValidateSpinbox %W %P}] -column 1 -row 3
$worldframe.duration set 60
grid [ttk::button $worldframe.addship -text {Add ship} -command DebugAddShip]
grid [ttk::label $worldframe.baselbl -text {Base:}] -column 2 -row 0
grid [ttk::entry $worldframe.base] -column 3 -row 0
grid [ttk::label $worldframe.eventlbl -text {Event:}] -column 2 -row 1
grid [ttk::combobox $worldframe.event -state readonly -values [list Disease {Double price} {Full docks}]] -column 3 -row 1
bind $worldframe.event <<ComboboxSelected>> ToggleItemEntry
grid [ttk::label $worldframe.itemlbl -text {Item:}] -column 2 -row 2
grid [ttk::entry $worldframe.item] -column 3 -row 2
grid [ttk::label $worldframe.duration2lbl -text {Duration:}] -column 2 -row 3
grid [ttk::spinbox $worldframe.baseduration -from 15 -to 12000 -validate key -validatecommand {ValidateSpinbox %W %P}] -column 3 -row 3
grid [ttk::button $worldframe.addevent -text {Add event} -command DebugAddEvent] -column 2 -row 4
grid [ttk::button $worldframe.deleteevent -text {Delete event} -command DebugDeleteEvent]
grid [ttk::combobox $worldframe.delete -state readonly] -column 1 -row 5
grid $shipframe
wm geometry .debugdialog +[expr ([winfo vrootwidth .debugdialog] / 2) - 200]+[expr [winfo vrootheight .debugdialog] / 3]
