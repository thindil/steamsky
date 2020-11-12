# normal code
ttk::frame .gameframe.paned.schoolframe
set schoolcanvas [canvas .gameframe.paned.schoolframe.canvas -yscrollcommand [list .gameframe.paned.schoolframe.scrolly set] -xscrollcommand [list .gameframe.paned.schoolframe.scrollx set]]
pack [ttk::scrollbar .gameframe.paned.schoolframe.scrolly -orient vertical -command [list $schoolcanvas yview]] -side right -fill y
pack $schoolcanvas -side top -fill both
pack [ttk::scrollbar .gameframe.paned.schoolframe.scrollx -orient horizontal -command [list $schoolcanvas xview]] -fill x
set schoolframe [ttk::frame $schoolcanvas.school]
# Crew members list
grid [ttk::frame $schoolframe.crew] -sticky nwes
set schoolview [ttk::treeview $schoolframe.crew.view -yscrollcommand [list $schoolframe.crew.scrolly set]]
$schoolview heading #0 -text {Name}
grid $schoolview -sticky nwes
bind $schoolview <<TreeviewSelect>> ShowTrainingInfo
grid [ttk::scrollbar $schoolframe.crew.scrolly -orient vertical -command [list $schoolview yview]] -row 0 -column 1 -sticky ns
# Skills list
set schoolskillsframe [ttk::frame $schoolframe.skills]
set schoolskillsview [ttk::treeview $schoolskillsframe.view -show headings -columns [list name price] -yscrollcommand [list $schoolskillsframe.scrolly set]]
$schoolskillsview heading name -text {Name}
$schoolskillsview heading price -text {Price}
grid $schoolskillsview -sticky nwes
grid [ttk::scrollbar $schoolskillsframe.scrolly -orient vertical -command [list $schoolskillsview yview]] -row 0 -column 1 -sticky ns
# Skills options
grid [ttk::label $schoolskillsframe.money]
grid [ttk::button $schoolskillsframe.train -text {Train selected skill} -command TrainSkill]
grid $schoolskillsframe -row 0 -column 1 -sticky nwes
