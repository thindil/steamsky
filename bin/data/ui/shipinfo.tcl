set shipinfoframe [ttk::frame .paned.shipinfoframe]
# General ship info
grid [ttk::labelframe $shipinfoframe.general -text {General info:}] -sticky nwes
set shipcanvas [canvas $shipinfoframe.general.canvas -yscrollcommand [list $shipinfoframe.general.scrolly set] -xscrollcommand [list $shipinfoframe.general.scrollx set]]
pack [ttk::scrollbar $shipinfoframe.general.scrolly -orient vertical -command [list $shipcanvas yview]] -side right -fill y
pack [ttk::scrollbar $shipinfoframe.general.scrollx -orient horizontal -command [list $shipcanvas xview]] -fill x -side bottom
pack $shipcanvas -side top -fill both -expand true
ttk::frame $shipcanvas.frame
grid columnconfigure $shipcanvas.frame 1 -weight 1
grid [ttk::button $shipcanvas.frame.maxmin -style Toolbutton -text "[format %c 0x2191]" -command {ShipMaxMin general show}]
tooltip::tooltip $shipcanvas.frame.maxmin {Maximize/minimize the ship general info}
# Ship name
grid [ttk::label $shipcanvas.frame.name] -columnspan 2 -sticky w
grid [ttk::button $shipcanvas.frame.rename -text "[format %c 0xf044]" -style Toolbutton -command {
   if {[getstring::tk_getString .gs text "Enter a new name:"]} {
      SetShipName $text
   }
}] -column 2 -row 1 -sticky w
tooltip::tooltip $shipcanvas.frame.rename {Set a new name for the ship}
bind $shipcanvas.frame.name <Enter> SetShipName
# Upgrade progress
grid [ttk::label $shipcanvas.frame.upgradelabel -text {Upgrade:}] -sticky w -columnspan 3
grid [ttk::progressbar $shipcanvas.frame.upgrade -orient horizontal -maximum 1.0 -style green.Horizontal.TProgressbar] -sticky we -columnspan 2
tooltip::tooltip $shipcanvas.frame.upgrade {The current ship's upgrade progress}
grid [ttk::button $shipcanvas.frame.cancelupgrade -text "[format %c 0xf04d]" -style Toolbutton -command StopUpgrading] -row 3 -column 2 -sticky w
tooltip::tooltip $shipcanvas.frame.cancelupgrade {Stop the current upgrade}
# Repair priority
grid [ttk::label $shipcanvas.frame.repairlabel] -columnspan 2 -sticky we
grid [ttk::button $shipcanvas.frame.cancelpriority -text "[format %c 0xf05e]" -style Toolbutton -command {SetRepair remove}] -row 4 -column 2 -sticky w
tooltip::tooltip $shipcanvas.frame.cancelpriority {Remove the repair priority}
# Ship destination
grid [ttk::label $shipcanvas.frame.destinationlabel] -columnspan 2 -sticky we
grid [ttk::button $shipcanvas.frame.canceldestination -text "[format %c 0xf05e]" -style Toolbutton -command {ResetDestination}] -row 5 -column 2 -sticky w
tooltip::tooltip $shipcanvas.frame.canceldestination {Reset the ship destination}
# Ship home base
grid [ttk::label $shipcanvas.frame.homelabel] -columnspan 2 -sticky we
# Ship weight
grid [ttk::label $shipcanvas.frame.weight] -columnspan 2 -sticky we
$shipcanvas create window [expr [winfo reqwidth $shipcanvas.frame] / 2] [expr [winfo reqheight $shipcanvas.frame] / 2] -window $shipcanvas.frame
# Ship modules
grid [ttk::labelframe $shipinfoframe.modules -text {Modules}] -sticky nwes
grid [ttk::treeview $shipinfoframe.modules.modules -show tree] -row 1 -columnspan 3 -sticky nwes
bind $shipinfoframe.modules.modules <<TreeviewSelect>> ShowModuleInfo
# Crew info
grid [ttk::labelframe $shipinfoframe.crew -text {Crew Info:}] -row 0 -column 1 -sticky nwes
grid [ttk::label $shipinfoframe.crew.name -text {Name}]
grid [ttk::label $shipinfoframe.crew.order -text {Order}] -column 1 -row 0
grid [ttk::label $shipinfoframe.crew.health -text {Health}] -column 2 -row 0
grid [ttk::label $shipinfoframe.crew.fatigue -text {Fatigue}] -column 3 -row 0
grid [ttk::label $shipinfoframe.crew.thirst -text {Thirst}] -column 4 -row 0
grid [ttk::label $shipinfoframe.crew.hunter -text {Hunger}] -column 5 -row 0
grid [ttk::label $shipinfoframe.crew.morale -text {Morale}] -column 6 -row 0
# Detailed info about the selected ship's module
grid [ttk::labelframe $shipinfoframe.cargo -text {Module Info:}] -row 1 -column 1 -sticky nwes
grid [ttk::label $shipinfoframe.cargo.damagelbl -text {Damage:}]
grid [ttk::progressbar $shipinfoframe.cargo.damage -orient horizontal -maximum 1.0] -row 0 -column 1
grid [ttk::label $shipinfoframe.cargo.cleanlbl -text {Clean:}]
grid [ttk::progressbar $shipinfoframe.cargo.clean -orient horizontal -maximum 1.0] -row 1 -column 1
grid [ttk::label $shipinfoframe.cargo.qualitylbl -text {Quality:}]
grid [ttk::progressbar $shipinfoframe.cargo.quality -orient horizontal -maximum 1.0] -row 2 -column 1
grid [ttk::label $shipinfoframe.cargo.upgradelbl -text {Upgrade:}]
grid [ttk::progressbar $shipinfoframe.cargo.upgrade -orient horizontal -maximum 1.0] -row 3 -column 1
set moduleinfo [text $shipinfoframe.cargo.info -wrap char -height 10 -width 40]
$moduleinfo tag configure red -foreground red
grid $moduleinfo -row 4 -columnspan 2 -sticky nwes
# Buttons with ship's module actions
grid [ttk::frame $shipinfoframe.cargo.options] -columnspan 2
grid [ttk::button $shipinfoframe.cargo.options.durability -text {Upgrade durability} -command {SetUpgrade 1}] -columnspan 2
grid [ttk::button $shipinfoframe.cargo.options.upgrade1 -text {Upgrade1} -command {SetUpgrade 2}] -columnspan 2
grid [ttk::button $shipinfoframe.cargo.options.upgrade2 -text {Upgrade2} -command {SetUpgrade 3}] -columnspan 2
grid [ttk::button $shipinfoframe.cargo.options.assigncrew -text {Assign crew} -command {AssignModule crew}]
grid [ttk::combobox $shipinfoframe.cargo.options.crewcombo -state readonly] -column 1 -row 3
grid [ttk::button $shipinfoframe.cargo.options.assignammo -text {Assign as ammo} -command {AssignModule ammo}]
grid [ttk::combobox $shipinfoframe.cargo.options.ammocombo -state readonly] -column 1 -row 4
grid [ttk::button $shipinfoframe.cargo.options.train -text {Train} -command {AssignModule skill}]
grid [ttk::combobox $shipinfoframe.cargo.options.traincombo -state readonly] -column 1 -row 5
grid [ttk::button $shipinfoframe.cargo.options.disable -text {Disable engine} -command DisableEngine] -columnspan 2
grid [ttk::button $shipinfoframe.cargo.options.continue -text {Continue upgrading} -command {SetUpgrade 4}] -columnspan 2
grid [ttk::button $shipinfoframe.cargo.options.repair -text {Repair as first} -command {SetRepair assign}] -columnspan 2
# Configure main ship info grid
grid columnconfigure $shipinfoframe 0 -weight 1
grid columnconfigure $shipinfoframe 1 -weight 1
grid rowconfigure $shipinfoframe 0 -weight 1
grid rowconfigure $shipinfoframe 1 -weight 1
