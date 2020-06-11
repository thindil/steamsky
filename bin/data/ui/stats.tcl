ttk::frame .paned.statsframe
grid [ttk::frame .paned.statsframe.left] -sticky nwes
grid [ttk::label .paned.statsframe.left.stats]
grid [ttk::label .paned.statsframe.left.crafts]
grid [ttk::treeview .paned.statsframe.left.craftsview -show headings -columns [list name amount] -selectmode none]
.paned.statsframe.left.craftsview heading name -text {Name}
.paned.statsframe.left.craftsview heading amount -text {Amount}
grid [ttk::label .paned.statsframe.left.missions]
grid [ttk::treeview .paned.statsframe.left.missionsview -show headings -columns [list name amount] -selectmode none]
.paned.statsframe.left.missionsview heading name -text {Name}
.paned.statsframe.left.missionsview heading amount -text {Amount}
grid [ttk::button .paned.statsframe.left.goal -text {Goals}]
grid [ttk::label .paned.statsframe.left.goals]
grid [ttk::treeview .paned.statsframe.left.goalsview -show headings -columns [list name amount] -selectmode none]
.paned.statsframe.left.goalsview heading name -text {Name}
.paned.statsframe.left.goalsview heading amount -text {Amount}
grid [ttk::frame .paned.statsframe.right] -row 0 -column 1 -sticky nwes
grid [ttk::label .paned.statsframe.right.destroyed]
grid [ttk::treeview .paned.statsframe.right.destroyedview -show headings -columns [list name amount] -selectmode none]
.paned.statsframe.right.destroyedview heading name -text {Name}
.paned.statsframe.right.destroyedview heading amount -text {Amount}
grid [ttk::label .paned.statsframe.right.killed]
grid [ttk::treeview .paned.statsframe.right.killedview -show headings -columns [list name amount] -selectmode none]
.paned.statsframe.right.killedview heading name -text {Name}
.paned.statsframe.right.killedview heading amount -text {Amount}
