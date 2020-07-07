# test code
ttk::frame .paned
set basesframe [ttk::frame .paned.bases]
# normal code
#ttk::frame .paned.basesframe
#set basescanvas [canvas .paned.basesframe.canvas -yscrollcommand [list .paned.basesframe.scrolly set] -xscrollcommand [list .paned.basesframe.scrollx set]]
#grid $basescanvas -sticky nwes
#grid [ttk::scrollbar .paned.basesframe.scrollx -orient horizontal -command [list $basescanvas xview]] -row 1 -column 0 -columnspan 2 -sticky we
#grid [ttk::scrollbar .paned.basesframe.scrolly -orient vertical -command [list $basescanvas yview]] -row 0 -column 1 -sticky ns
#set basesframe [ttk::frame $basescanvas.bases]
# List of bases options
grid [ttk::frame $basesframe.options]
grid [ttk::label $basesframe.options.typeslbl -text {Type:}]
grid [ttk::combobox $basesframe.options.types -state readonly] -row 0 -column 1
grid [ttk::label $basesframe.options.statuslbl -text {Status:}] -row 0 -column 2
grid [ttk::combobox $basesframe.options.status -state readonly] -row 0 -column 3
grid [ttk::label $basesframe.options.ownerlbl -text {Owner:}] -row 0 -column 4
grid [ttk::combobox $basesframe.options.owner -state readonly] -row 0 -column 5
grid [ttk::entry $basesframe.options.search -validate key] -row 0 -column 6
# Bases list
grid [ttk::frame $basesframe.list] -sticky nwes
set basesview [ttk::treeview $basesframe.list.view -yscrollcommand [list $basesframe.list.scrolly set] -show headings -columns [list name distance population size owner type]]
$basesview heading name -text {Name}
$basesview heading distance -text {Distance}
$basesview heading population -text {Population}
$basesview heading size -text {Size}
$basesview heading owner -text {Owner}
$basesview heading type -text {Type}
grid $basesview -sticky nwes
grid [ttk::scrollbar $basesframe.list.scrolly -orient vertical -command [list $basesview yview]] -row 0 -column 1 -sticky ns
# Base info
set baseframe [ttk::frame $basesframe.base]
grid [ttk::labelframe $baseframe.info -text {Base Info:}]
grid [text $baseframe.info.text -wrap char -height 10 -width 40] -columnspan 3
grid [ttk::label $baseframe.info.reputationlbl -text {Reputation:}]
grid [ttk::progressbar $baseframe.info.minusreputation] -row 1 -column 1
grid [ttk::progressbar $baseframe.info.plusreputation] -row 1 -column 2
# test code
grid $baseframe
pack $basesframe -fill both -expand true
pack .paned -fill both -expand true
