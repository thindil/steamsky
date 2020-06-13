ttk::frame .paned.statsframe
set statscanvas [canvas .paned.statsframe.canvas -yscrollcommand [list .paned.statsframe.scrolly set] -xscrollcommand [list .paned.statsframe.scrollx set]]
grid $statscanvas -sticky nwes
grid [ttk::scrollbar .paned.statsframe.scrollx -orient horizontal -command [list $statscanvas xview]] -row 1 -column 0 -columnspan 2 -sticky we
grid [ttk::scrollbar .paned.statsframe.scrolly -orient vertical -command [list $statscanvas yview]] -row 0 -column 1 -sticky ns
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
grid [ttk::button $statsframe.left.goal -text {Goals}]
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
$statscanvas create window [expr [winfo reqwidth $statsframe] / 2] [expr [winfo reqheight $statsframe] / 2] -window $statsframe
grid columnconfigure .paned.statsframe 0 -weight 1
# Move line below to Ada (need to get position of paned sash)
$statscanvas configure -width [winfo reqwidth $statsframe] -height [winfo reqheight $statsframe] -scrollregion [$statscanvas bbox all]
