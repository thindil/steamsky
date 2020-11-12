ttk::frame .gameframe.paned.statsframe
set statscanvas [canvas .gameframe.paned.statsframe.canvas -yscrollcommand [list .gameframe.paned.statsframe.scrolly set] -xscrollcommand [list .gameframe.paned.statsframe.scrollx set]]
pack [ttk::scrollbar .gameframe.paned.statsframe.scrolly -orient vertical -command [list $statscanvas yview]] -side right -fill y
pack $statscanvas -side top -fill both
pack [ttk::scrollbar .gameframe.paned.statsframe.scrollx -orient horizontal -command [list $statscanvas xview]] -fill x
set statsframe [ttk::frame $statscanvas.stats]
grid [ttk::frame $statsframe.left] -sticky nwes
grid [ttk::label $statsframe.left.stats]
grid [ttk::label $statsframe.left.crafts]
grid [ttk::treeview $statsframe.left.craftsview -show headings -columns [list name amount] -selectmode none]
$statsframe.left.craftsview heading name -text {Name}
$statsframe.left.craftsview heading amount -text {Amount}
grid [ttk::label $statsframe.left.missions]
grid [ttk::treeview $statsframe.left.missionsview -show headings -columns [list name amount] -selectmode none]
$statsframe.left.missionsview heading name -text {Name}
$statsframe.left.missionsview heading amount -text {Amount}
grid [ttk::button $statsframe.left.goal -text {Goals} -command {ShowGoals $statsframe.left.goal}]
grid [ttk::label $statsframe.left.goals]
grid [ttk::treeview $statsframe.left.goalsview -show headings -columns [list name amount] -selectmode none]
$statsframe.left.goalsview heading name -text {Name}
$statsframe.left.goalsview heading amount -text {Amount}
grid [ttk::frame $statsframe.right] -row 0 -column 1 -sticky nwes
grid [ttk::label $statsframe.right.destroyed]
grid [ttk::treeview $statsframe.right.destroyedview -show headings -columns [list name amount] -selectmode none]
$statsframe.right.destroyedview heading name -text {Name}
$statsframe.right.destroyedview heading amount -text {Amount}
grid [ttk::label $statsframe.right.killed]
grid [ttk::treeview $statsframe.right.killedview -show headings -columns [list name amount] -selectmode none]
$statsframe.right.killedview heading name -text {Name}
$statsframe.right.killedview heading amount -text {Amount}
grid columnconfigure .gameframe.paned.statsframe 0 -weight 1
