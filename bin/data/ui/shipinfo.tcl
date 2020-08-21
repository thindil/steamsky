set shipinfoframe [ttk::frame .paned.shipinfoframe]
$shipinfoframe configure -height [.paned sashpos 0]
grid [ttk::frame $shipinfoframe.left] -sticky nwes
# General ship info
grid [ttk::labelframe $shipinfoframe.left.general -text {General info:}]
set shipcanvas [canvas $shipinfoframe.left.general.canvas -yscrollcommand [list $shipinfoframe.left.general.scrolly set] -xscrollcommand [list $shipinfoframe.left.general.scrollx set]]
pack [ttk::scrollbar $shipinfoframe.left.general.scrolly -orient vertical -command [list $shipcanvas yview]] -side right -fill y
pack $shipcanvas -side top -fill both
pack [ttk::scrollbar $shipinfoframe.left.general.scrollx -orient horizontal -command [list $shipcanvas xview]] -fill x
ttk::frame $shipcanvas.frame
grid columnconfigure $shipcanvas.frame 1 -weight 1
# Ship name
grid [ttk::label $shipcanvas.frame.name -wraplength 300] -columnspan 2 -sticky w
grid [ttk::button $shipcanvas.frame.rename -text "[format %c 0xf044]" -style Toolbutton -command {
   if {[getstring::tk_getString .gs text "Enter a new name:"]} {
      SetShipName $text
   }
}] -column 2 -row 0 -sticky w
tooltip::tooltip $shipcanvas.frame.rename {Set a new name for the ship}
bind $shipcanvas.frame.name <Enter> SetShipName
# Upgrade progress
grid [ttk::label $shipcanvas.frame.upgradelabel -text {Upgrade:}] -sticky w -columnspan 3
grid [ttk::progressbar $shipcanvas.frame.upgrade -orient horizontal -maximum 1.0 -style green.Horizontal.TProgressbar] -sticky we -columnspan 2
tooltip::tooltip $shipcanvas.frame.upgrade {The current ship's upgrade progress}
grid [ttk::button $shipcanvas.frame.cancelupgrade -text "[format %c 0xf04d]" -style Toolbutton -command StopUpgrading] -row 2 -column 2 -sticky w
tooltip::tooltip $shipcanvas.frame.cancelupgrade {Stop the current upgrade}
# Repair priority
grid [ttk::label $shipcanvas.frame.repairlabel] -row 3 -columnspan 2 -sticky we
grid [ttk::button $shipcanvas.frame.cancelpriority -text "[format %c 0xf05e]" -style Toolbutton -command {SetRepair remove}] -row 3 -column 2 -sticky w
tooltip::tooltip $shipcanvas.frame.cancelpriority {Remove the repair priority}
# Ship info
grid [ttk::label $shipcanvas.frame.info] -row 4 -columnspan 3 -sticky we
$shipcanvas create window [expr [winfo reqwidth $shipcanvas.frame] / 2] [expr [winfo reqheight $shipcanvas.frame] / 2] -window $shipcanvas.frame
# Ship modules
grid [ttk::labelframe $shipinfoframe.left.modules -text {Modules}] -sticky nwes
grid [ttk::treeview $shipinfoframe.left.modules.modules -show tree] -row 5 -columnspan 3 -sticky nwes
bind $shipinfoframe.left.modules.modules <<TreeviewSelect>> ShowModuleInfo
grid [ttk::frame $shipinfoframe.right] -column 1 -row 0 -sticky nwes
# Crew info
grid [ttk::labelframe $shipinfoframe.right.crew -text {Crew Info:}]
grid [ttk::label $shipinfoframe.right.crew.name -text {Name}]
grid [ttk::label $shipinfoframe.right.crew.order -text {Order}] -column 1 -row 0
grid [ttk::label $shipinfoframe.right.crew.health -text {Health}] -column 2 -row 0
grid [ttk::label $shipinfoframe.right.crew.fatigue -text {Fatigue}] -column 3 -row 0
grid [ttk::label $shipinfoframe.right.crew.thirst -text {Thirst}] -column 4 -row 0
grid [ttk::label $shipinfoframe.right.crew.hunter -text {Hunger}] -column 5 -row 0
grid [ttk::label $shipinfoframe.right.crew.morale -text {Morale}] -column 6 -row 0
# Detailed info about the selected ship's module
grid [ttk::labelframe $shipinfoframe.right.module -text {Module Info:}]
ttk::label $shipinfoframe.right.module.damagelbl -text {Damage:}
ttk::progressbar $shipinfoframe.right.module.damage -orient horizontal -maximum 1.0
ttk::label $shipinfoframe.right.module.cleanlbl -text {Clean:}
ttk::progressbar $shipinfoframe.right.module.clean -orient horizontal -maximum 1.0
ttk::label $shipinfoframe.right.module.qualitylbl -text {Quality:}
ttk::progressbar $shipinfoframe.right.module.quality -orient horizontal -maximum 1.0
ttk::label $shipinfoframe.right.module.upgradelbl -text {Upgrade:}
ttk::progressbar $shipinfoframe.right.module.upgrade -orient horizontal -maximum 1.0
set moduleinfo [text $shipinfoframe.right.module.info -wrap char -height 10 -width 40]
$moduleinfo tag configure red -foreground red
grid $moduleinfo -row 4 -columnspan 2
# Buttons with ship's module actions
grid [ttk::labelframe $shipinfoframe.right.options -text {Module Options:}]
grid [ttk::button $shipinfoframe.right.options.durability -text {Upgrade durability} -command {SetUpgrade 1}] -columnspan 2
grid [ttk::button $shipinfoframe.right.options.upgrade1 -text {Upgrade1} -command {SetUpgrade 2}] -columnspan 2
grid [ttk::button $shipinfoframe.right.options.upgrade2 -text {Upgrade2} -command {SetUpgrade 3}] -columnspan 2
grid [ttk::button $shipinfoframe.right.options.assigncrew -text {Assign crew} -command {AssignModule crew}]
grid [ttk::combobox $shipinfoframe.right.options.crewcombo -state readonly] -column 1 -row 3
grid [ttk::button $shipinfoframe.right.options.assignammo -text {Assign as ammo} -command {AssignModule ammo}]
grid [ttk::combobox $shipinfoframe.right.options.ammocombo -state readonly] -column 1 -row 4
grid [ttk::button $shipinfoframe.right.options.train -text {Train} -command {AssignModule skill}]
grid [ttk::combobox $shipinfoframe.right.options.traincombo -state readonly] -column 1 -row 5
grid [ttk::button $shipinfoframe.right.options.disable -text {Disable engine} -command DisableEngine] -columnspan 2
grid [ttk::button $shipinfoframe.right.options.continue -text {Continue upgrading} -command {SetUpgrade 4}] -columnspan 2
grid [ttk::button $shipinfoframe.right.options.repair -text {Repair as first} -command {SetRepair assign}] -columnspan 2
