ttk::frame .paned.shipinfoframe
set shipinfocanvas [canvas .paned.shipinfoframe.canvas -yscrollcommand [list .paned.shipinfoframe.scrolly set] -xscrollcommand [list .paned.shipinfoframe.scrollx set]]
grid $shipinfocanvas -sticky nwes
grid [ttk::scrollbar .paned.shipinfoframe.scrollx -orient horizontal -command [list $shipinfocanvas xview]] -row 1 -column 0 -columnspan 2 -sticky we
grid [ttk::scrollbar .paned.shipinfoframe.scrolly -orient vertical -command [list $shipinfocanvas yview]] -row 0 -column 1 -sticky ns
set shipinfoframe [ttk::frame $shipinfocanvas.shipinfo]
grid [ttk::frame $shipinfoframe.left] -sticky nwes
grid [ttk::label $shipinfoframe.left.namelbl -text {Name:}]
grid [ttk::entry $shipinfoframe.left.name] -column 1 -row 0
bind $shipinfoframe.left.name <Enter> SetShipName
ttk::label $shipinfoframe.left.upgradelabel
ttk::progressbar $shipinfoframe.left.upgrade -orient horizontal -maximum 1.0
grid [ttk::label $shipinfoframe.left.info] -row 2 -columnspan 2
grid [ttk::treeview $shipinfoframe.left.modules -show tree] -row 3 -columnspan 2
bind $shipinfoframe.left.modules <<TreeviewSelect>> ShowModuleInfo
grid [ttk::frame $shipinfoframe.right] -column 1 -row 0 -sticky nwes
grid [ttk::labelframe $shipinfoframe.right.crew -text {Crew Info:}]
grid [ttk::label $shipinfoframe.right.crew.name -text {Name}]
grid [ttk::label $shipinfoframe.right.crew.order -text {Order}] -column 1 -row 0
grid [ttk::label $shipinfoframe.right.crew.health -text {Health}] -column 2 -row 0
grid [ttk::label $shipinfoframe.right.crew.fatigue -text {Fatigue}] -column 3 -row 0
grid [ttk::label $shipinfoframe.right.crew.thirst -text {Thirst}] -column 4 -row 0
grid [ttk::label $shipinfoframe.right.crew.hunter -text {Hunger}] -column 5 -row 0
grid [ttk::label $shipinfoframe.right.crew.morale -text {Morale}] -column 6 -row 0
grid [ttk::labelframe $shipinfoframe.right.module -text {Module Info:}]
ttk::label $shipinfoframe.right.module.damagelbl -text {Damage:}
ttk::progressbar $shipinfoframe.right.module.damage -orient horizontal -maximum 1.0
grid [ttk::label $shipinfoframe.right.module.info] -row 1
ttk::label $shipinfoframe.right.module.cleanlbl -text {Clean:}
ttk::progressbar $shipinfoframe.right.module.clean -orient horizontal -maximum 1.0
ttk::label $shipinfoframe.right.module.qualitylbl -text {Quality:}
ttk::progressbar $shipinfoframe.right.module.quality -orient horizontal -maximum 1.0
ttk::label $shipinfoframe.right.module.upgradelbl -text {Upgrade:}
ttk::progressbar $shipinfoframe.right.module.upgrade -orient horizontal -maximum 1.0
grid [ttk::labelframe $shipinfoframe.right.options -text {Module Options:}]
ttk::button $shipinfoframe.right.options.durability -text {Upgrade durability}
ttk::button $shipinfoframe.right.options.durability1 -text {Upgrade1}
ttk::button $shipinfoframe.right.options.durability2 -text {Upgrade2}
ttk::button $shipinfoframe.right.options.assigncrew -text {Assign crew}
ttk::button $shipinfoframe.right.options.assignammo -text {Assign as ammo}
ttk::button $shipinfoframe.right.options.train -text {Train}
ttk::button $shipinfoframe.right.options.disable -text {Disable engine}
ttk::button $shipinfoframe.right.options.continue -text {Continue upgrading}
ttk::button $shipinfoframe.right.options.stop -text {Stop upgrading}
ttk::button $shipinfoframe.right.options.repair -text {Repair as first}
ttk::button $shipinfoframe.right.options.remove -text {Remove repair priority}
