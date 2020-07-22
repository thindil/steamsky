# normal code
ttk::frame .paned.schoolframe
set schoolcanvas [canvas .paned.schoolframe.canvas -yscrollcommand [list .paned.schoolframe.scrolly set] -xscrollcommand [list .paned.schoolframe.scrollx set]]
grid $schoolcanvas -sticky nwes
grid [ttk::scrollbar .paned.schoolframe.scrollx -orient horizontal -command [list $schoolcanvas xview]] -row 1 -column 0 -columnspan 2 -sticky we
grid [ttk::scrollbar .paned.schoolframe.scrolly -orient vertical -command [list $schoolcanvas yview]] -row 0 -column 1 -sticky ns
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
grid [ttk::button $schoolskillsframe.train -text {Train selected skill}]
grid $schoolskillsframe -row 0 -column 1 -sticky nwes
