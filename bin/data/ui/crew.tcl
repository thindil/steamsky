ttk::frame .paned.crewframe
set crewcanvas [canvas .paned.crewframe.canvas -yscrollcommand [list .paned.crewframe.scrolly set] -xscrollcommand [list .paned.crewframe.scrollx set]]
grid $crewcanvas -sticky nwes
grid [ttk::scrollbar .paned.crewframe.scrollx -orient horizontal -command [list $crewcanvas xview]] -row 1 -column 0 -columnspan 2 -sticky we
grid [ttk::scrollbar .paned.crewframe.scrolly -orient vertical -command [list $crewcanvas yview]] -row 0 -column 1 -sticky ns
set crewframe [ttk::frame $crewcanvas.crew]
# Crew list
grid [ttk::frame $crewframe.crew] -sticky nwes
grid [ttk::label $crewframe.crew.name -text {Name}]
grid [ttk::label $crewframe.crew.order -text {Order}] -row 0 -column 1
# Crew member info
grid [ttk::frame $crewframe.info] -sticky nwes -row 0 -column 1
grid [ttk::labelframe $crewframe.info.info -text {Member Info}]
grid [ttk::label $crewframe.info.info.label]
grid [ttk::label $crewframe.info.info.healthlbl]
grid [ttk::progressbar $crewframe.info.info.health] -row 1 -column 1
grid [ttk::label $crewframe.info.info.tiredlbl]
grid [ttk::progressbar $crewframe.info.info.tired] -row 2 -column 1
grid [ttk::label $crewframe.info.info.thirstlbl]
grid [ttk::progressbar $crewframe.info.info.thirst] -row 3 -column 1
grid [ttk::label $crewframe.info.info.hungerlbl]
grid [ttk::progressbar $crewframe.info.info.hunger] -row 4 -column 1
grid [ttk::label $crewframe.info.info.moralelbl]
grid [ttk::progressbar $crewframe.info.info.morale] -row 5 -column 1
grid [ttk::labelframe $crewframe.info.info.stats -text Statistics]
grid [ttk::labelframe $crewframe.info.info.skills -text Skills]
# Crew member priorities
grid [ttk::labelframe $crewframe.info.priorities -text {Orders priorities}]
grid [ttk::label $crewframe.info.priorities.name -text {Priority}]
grid [ttk::label $crewframe.info.priorities.level -text {Level}] -row 0 -column 1
# Options
grid [ttk::button $crewframe.info.inventory -text {Inventory}]
grid [ttk::button $crewframe.info.dismiss -text {Dismiss}]
grid [ttk::button $crewframe.info.clean -text {Clean ship everyone}]
grid [ttk::button $crewframe.info.repair -text {Repair ship everyone}]
