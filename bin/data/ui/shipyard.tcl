# test code
ttk::frame .paned
set shipyardframe [ttk::frame .paned.shipyard]
# normal code
#ttk::frame .paned.shipyardframe
#set shipyardcanvas [canvas .paned.shipyardframe.canvas -yscrollcommand [list .paned.shipyardframe.scrolly set] -xscrollcommand [list .paned.shipyardframe.scrollx set]]
#grid $shipyardcanvas -sticky nwes
#grid [ttk::scrollbar .paned.shipyardframe.scrollx -orient horizontal -command [list $shipyardcanvas xview]] -row 1 -column 0 -columnspan 2 -sticky we
#grid [ttk::scrollbar .paned.shipyardframe.scrolly -orient vertical -command [list $shipyardcanvas yview]] -row 0 -column 1 -sticky ns
#set shipyardframe [ttk::frame $shipyardcanvas.shipyard]
grid [ttk::notebook $shipyardframe.notebook] -sticky nwes
# Install modules
set sinstall [ttk::frame $shipyardframe.notebook.install]
grid [ttk::frame $sinstall.modules] -sticky nwes
set shipyardview [ttk::treeview $sinstall.modules.view -yscrollcommand [list $sinstall.modules.scrolly set]]
$shipyardview heading #0 -text {Name}
grid $shipyardview -sticky nwes
grid [ttk::scrollbar $sinstall.modules.scrolly -orient vertical -command [list $shipyardview yview]] -row 0 -column 1 -sticky ns
# Module info
set infoframe [ttk::frame $sinstall.info]
grid [ttk::labelframe $infoframe.info -text {Module info:}]
set moduleinfo [text $infoframe.info.info -wrap char -height 10 -width 40]
$moduleinfo tag configure red -foreground red
grid [ttk::label $infoframe.money]
grid [ttk::button $infoframe.install -text {Install module}]
grid $infoframe -column 1 -row 1
$shipyardframe.notebook add $sinstall -text {Install}
# test code
pack $shipyardframe -fill both -expand true
pack .paned -fill both -expand true
