ttk::frame .paned.shipinfoframe
set shipinfocanvas [canvas .paned.shipinfoframe.canvas -yscrollcommand [list .paned.shipinfoframe.scrolly set] -xscrollcommand [list .paned.shipinfoframe.scrollx set]]
grid $shipinfocanvas -sticky nwes
grid [ttk::scrollbar .paned.shipinfoframe.scrollx -orient horizontal -command [list $shipinfocanvas xview]] -row 1 -column 0 -columnspan 2 -sticky we
grid [ttk::scrollbar .paned.shipinfoframe.scrolly -orient vertical -command [list $shipinfocanvas yview]] -row 0 -column 1 -sticky ns
set shipinfoframe [ttk::frame $shipinfocanvas.shipinfo]
grid [ttk::frame $shipinfoframe.left] -sticky nwes
grid [ttk::label $shipinfoframe.left.namelbl -text {Name:}]
grid [ttk::entry $shipinfoframe.left.name] -column 1 -row 0
