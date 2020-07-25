ttk::frame .paned.baseframe
set basecanvas [canvas .paned.baseframe.canvas -yscrollcommand [list .paned.baseframe.scrolly set] -xscrollcommand [list .paned.baseframe.scrollx set]]
grid $basecanvas -sticky nwes
grid [ttk::scrollbar .paned.baseframe.scrollx -orient horizontal -command [list $basecanvas xview]] -row 1 -column 0 -columnspan 2 -sticky we
grid [ttk::scrollbar .paned.baseframe.scrolly -orient vertical -command [list $basecanvas yview]] -row 0 -column 1 -sticky ns
set baseframe [ttk::frame $basecanvas.base]
grid [ttk::entry $baseframe.search] -columnspan 2
# Items list
grid [ttk::frame $baseframe.items] -sticky nwes
set baseview [ttk::treeview $baseframe.items.view -yscrollcommand [list $baseframe.items.scrolly set]]
$baseview heading #0 -text {Name}
grid $baseview -sticky nwes
grid [ttk::scrollbar $baseframe.items.scrolly -orient vertical -command [list $baseview yview]] -row 0 -column 1 -sticky ns
# Item info
set infoframe [ttk::frame $baseframe.info]
grid [ttk::label $infoframe.info]
grid [ttk::label $infoframe.money]
grid [ttk::button $infoframe.accept]
grid $infoframe -column 1 -row 1
