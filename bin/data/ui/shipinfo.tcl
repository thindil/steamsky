ttk::frame .paned.shipinfoframe
set shipinfocanvas [canvas .paned.shipinfoframe.canvas -yscrollcommand [list .paned.shipinfoframe.scrolly set] -xscrollcommand [list .paned.shipinfoframe.scrollx set]]
pack [ttk::scrollbar .paned.shipinfoframe.scrolly -orient vertical -command [list $shipinfocanvas yview]] -side right -fill y
pack $shipinfocanvas -side top -fill both
pack [ttk::scrollbar .paned.shipinfoframe.scrollx -orient horizontal -command [list $shipinfocanvas xview]] -fill x
# General ship info
set shipinfoframe [ttk::frame $shipinfocanvas.shipinfo]
grid [ttk::frame $shipinfoframe.left] -sticky nwes
grid columnconfigure $shipinfoframe.left 1 -weight 1
# Ship name
grid [ttk::label $shipinfoframe.left.namelbl -text {Name:}] -sticky w
grid [ttk::label $shipinfoframe.left.name -wraplength 100] -column 1 -row 0 -sticky w
grid [ttk::button $shipinfoframe.left.rename -text "[format %c 0xf044]" -style Toolbutton -command {
   if {[getstring::tk_getString .gs text "Enter a new name:"]} {
      SetShipName $text
   }
}] -column 2 -row 0 -sticky w
tooltip::tooltip $shipinfoframe.left.rename {Set a new name for the ship}
bind $shipinfoframe.left.name <Enter> SetShipName
# Upgrade progress
ttk::label $shipinfoframe.left.upgradelabel -text {Upgrade:}
ttk::label $shipinfoframe.left.upgradename
ttk::progressbar $shipinfoframe.left.upgrade -orient horizontal -maximum 1.0 -style green.Horizontal.TProgressbar
tooltip::tooltip $shipinfoframe.left.upgrade {The current ship's upgrade progress}
ttk::button $shipinfoframe.left.cancel -text "[format %c 0xf04d]" -style Toolbutton -command StopUpgrading
tooltip::tooltip $shipinfoframe.left.cancel {Stop the current upgrade}
# Ship info
grid [ttk::label $shipinfoframe.left.info] -row 3 -columnspan 3 -sticky we
# Ship modules
grid [ttk::treeview $shipinfoframe.left.modules -show tree] -row 4 -columnspan 3 -sticky we
bind $shipinfoframe.left.modules <<TreeviewSelect>> ShowModuleInfo
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
grid [ttk::button $shipinfoframe.right.options.remove -text {Remove repair priority} -command {SetRepair remove}] -columnspan 2
