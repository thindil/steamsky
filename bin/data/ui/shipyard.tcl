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
grid [ttk::frame $sinstall.options] -sticky we -columnspan 2
grid [ttk::label $sinstall.options.label -text "Show modules:"]
grid [ttk::combobox $sinstall.options.modules -state readonly -values [list {Any} {Engines} {Cabins} {Cockpits} {Turrets} {Guns} {Cargo bays} {Hulls} {Armors} {Battering rams} {Alchemy labs} {Furnaces} {Water collectors}]] -row 0 -column 1
$sinstall.options.modules current 0
grid [ttk::entry $sinstall.options.search] -row 0 -column 2
grid [ttk::frame $sinstall.modules] -sticky nwes
set shipyardview [ttk::treeview $sinstall.modules.view -show headings -columns [list name type size material] -yscrollcommand [list $sinstall.modules.scrolly set]]
$shipyardview heading name -text {Name}
$shipyardview heading type -text {Type}
$shipyardview heading size -text {Size}
$shipyardview heading material -text {Material}
grid $shipyardview -sticky nwes
grid [ttk::scrollbar $sinstall.modules.scrolly -orient vertical -command [list $shipyardview yview]] -row 0 -column 1 -sticky ns
# Module info
set infoframe [ttk::frame $sinstall.info]
grid [ttk::labelframe $infoframe.info -text {Module info:}]
set moduleinfo [text $infoframe.info.info -wrap char -height 10 -width 40]
$moduleinfo tag configure red -foreground red
grid $moduleinfo -sticky nwes
grid [ttk::label $infoframe.money]
grid [ttk::button $infoframe.install -text {Install module}]
grid $infoframe -column 1 -row 1
$shipyardframe.notebook add $sinstall -text {Install}
# test code
pack $shipyardframe -fill both -expand true
pack .paned -fill both -expand true
