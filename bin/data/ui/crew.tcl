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
grid [ttk::label $crewframe.info.priorities.name1 -text {Piloting}]
grid [ttk::combobox $crewframe.info.priorities.level1 -values [list None Normal Highest] -state readonly] -row 1 -column 1
bind $crewframe.info.priorities.level1 <<ComboboxSelected>> {SetPriority 1 [$crewframe.info.priorities.level1 current]}
grid [ttk::label $crewframe.info.priorities.name2 -text {Engineering}]
grid [ttk::combobox $crewframe.info.priorities.level2 -values [list None Normal Highest] -state readonly] -row 2 -column 1
bind $crewframe.info.priorities.level1 <<ComboboxSelected>> {SetPriority 2 [$crewframe.info.priorities.level2 current]}
grid [ttk::label $crewframe.info.priorities.name3 -text {Operating guns}]
grid [ttk::combobox $crewframe.info.priorities.level3 -values [list None Normal Highest] -state readonly] -row 3 -column 1
bind $crewframe.info.priorities.level1 <<ComboboxSelected>> {SetPriority 3 [$crewframe.info.priorities.level3 current]}
grid [ttk::label $crewframe.info.priorities.name4 -text {Repair ship}]
grid [ttk::combobox $crewframe.info.priorities.level4 -values [list None Normal Highest] -state readonly] -row 4 -column 1
bind $crewframe.info.priorities.level1 <<ComboboxSelected>> {SetPriority 4 [$crewframe.info.priorities.level4 current]}
grid [ttk::label $crewframe.info.priorities.name5 -text {Manufacturing}]
grid [ttk::combobox $crewframe.info.priorities.level5 -values [list None Normal Highest] -state readonly] -row 5 -column 1
bind $crewframe.info.priorities.level1 <<ComboboxSelected>> {SetPriority 5 [$crewframe.info.priorities.level5 current]}
grid [ttk::label $crewframe.info.priorities.name6 -text {Upgrading ship}]
grid [ttk::combobox $crewframe.info.priorities.level6 -values [list None Normal Highest] -state readonly] -row 6 -column 1
bind $crewframe.info.priorities.level1 <<ComboboxSelected>> {SetPriority 6 [$crewframe.info.priorities.level6 current]}
grid [ttk::label $crewframe.info.priorities.name7 -text {Talking in bases}]
grid [ttk::combobox $crewframe.info.priorities.level7 -values [list None Normal Highest] -state readonly] -row 7 -column 1
bind $crewframe.info.priorities.level1 <<ComboboxSelected>> {SetPriority 7 [$crewframe.info.priorities.level7 current]}
grid [ttk::label $crewframe.info.priorities.name8 -text {Healing wounded}]
grid [ttk::combobox $crewframe.info.priorities.level8 -values [list None Normal Highest] -state readonly] -row 8 -column 1
bind $crewframe.info.priorities.level1 <<ComboboxSelected>> {SetPriority 8 [$crewframe.info.priorities.level8 current]}
grid [ttk::label $crewframe.info.priorities.name9 -text {Cleaning ship}]
grid [ttk::combobox $crewframe.info.priorities.level9 -values [list None Normal Highest] -state readonly] -row 9 -column 1
bind $crewframe.info.priorities.level1 <<ComboboxSelected>> {SetPriority 9 [$crewframe.info.priorities.level9 current]}
grid [ttk::label $crewframe.info.priorities.name10 -text {Defend ship}]
grid [ttk::combobox $crewframe.info.priorities.level10 -values [list None Normal Highest] -state readonly] -row 10 -column 1
bind $crewframe.info.priorities.level1 <<ComboboxSelected>> {SetPriority 10 [$crewframe.info.priorities.level10 current]}
grid [ttk::label $crewframe.info.priorities.name11 -text {Board enemy ship}]
grid [ttk::combobox $crewframe.info.priorities.level11 -values [list None Normal Highest] -state readonly] -row 11 -column 1
bind $crewframe.info.priorities.level1 <<ComboboxSelected>> {SetPriority 11 [$crewframe.info.priorities.level11 current]}
grid [ttk::label $crewframe.info.priorities.name12 -text {Train skill}]
grid [ttk::combobox $crewframe.info.priorities.level12 -values [list None Normal Highest] -state readonly] -row 12 -column 1
bind $crewframe.info.priorities.level1 <<ComboboxSelected>> {SetPriority 12 [$crewframe.info.priorities.level12 current]}
# Options
grid [ttk::button $crewframe.info.inventory -text {Inventory}]
grid [ttk::button $crewframe.info.dismiss -text {Dismiss}]
grid [ttk::button $crewframe.info.clean -text {Clean ship everyone}]
grid [ttk::button $crewframe.info.repair -text {Repair ship everyone}]
