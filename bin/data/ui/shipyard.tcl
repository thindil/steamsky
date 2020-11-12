ttk::frame .gameframe.paned.shipyardframe
set shipyardcanvas [canvas .gameframe.paned.shipyardframe.canvas -yscrollcommand [list .gameframe.paned.shipyardframe.scrolly set] -xscrollcommand [list .gameframe.paned.shipyardframe.scrollx set]]
pack [ttk::scrollbar .gameframe.paned.shipyardframe.scrolly -orient vertical -command [list $shipyardcanvas yview]] -side right -fill y
pack $shipyardcanvas -side top -fill both
pack [ttk::scrollbar .gameframe.paned.shipyardframe.scrollx -orient horizontal -command [list $shipyardcanvas xview]] -fill x
set shipyardframe [ttk::frame $shipyardcanvas.shipyard]
grid [ttk::notebook $shipyardframe.notebook] -sticky nwes
# Install modules
set sinstall [ttk::frame $shipyardframe.notebook.install]
grid [ttk::frame $sinstall.options] -sticky we -columnspan 2
grid [ttk::label $sinstall.options.label -text "Show modules:"]
grid [ttk::combobox $sinstall.options.modules -state readonly -values [list {Any} {Engines} {Cabins} {Cockpits} {Turrets} {Guns} {Cargo bays} {Hulls} {Armors} {Battering rams} {Alchemy labs} {Furnaces} {Water collectors} {Workshops} {Greenhouses} {Medical rooms} {Harpoon guns} {Training rooms}]] -row 0 -column 1
$sinstall.options.modules current 0
bind $sinstall.options.modules <<ComboboxSelected>> {ShowShipyard [$sinstall.options.modules current]}
grid [ttk::entry $sinstall.options.search -validate key -validatecommand {SearchShipyard %P}] -row 0 -column 2
grid [ttk::frame $sinstall.modules] -sticky nwes
set shipyardview [ttk::treeview $sinstall.modules.view -show headings -columns [list name type size material] -yscrollcommand [list $sinstall.modules.scrolly set]]
$shipyardview heading name -text {Name}
$shipyardview heading type -text {Type}
$shipyardview heading size -text {Size}
$shipyardview heading material -text {Material}
grid $shipyardview -sticky nwes
bind $shipyardview <<TreeviewSelect>> ShowInstallInfo
grid [ttk::scrollbar $sinstall.modules.scrolly -orient vertical -command [list $shipyardview yview]] -row 0 -column 1 -sticky ns
# Module info
set infoframe [ttk::frame $sinstall.info]
grid [ttk::labelframe $infoframe.info -text {Module info:}]
set moduleinfo [text $infoframe.info.info -wrap char -height 10 -width 40]
$moduleinfo tag configure red -foreground red
grid $moduleinfo -sticky nwes
grid [ttk::label $infoframe.money]
grid [ttk::button $infoframe.install -text {Install module} -command {ManipulateModule install}]
grid $infoframe -column 1 -row 1
$shipyardframe.notebook add $sinstall -text {Install}
# Remove modules
set sremove [ttk::frame $shipyardframe.notebook.remove]
grid [ttk::frame $sremove.modules] -sticky nwes
set shipyardview [ttk::treeview $sremove.modules.view -show headings -columns [list name type size material] -yscrollcommand [list $sremove.modules.scrolly set]]
$shipyardview heading name -text {Name}
$shipyardview heading type -text {Type}
$shipyardview heading size -text {Size}
$shipyardview heading material -text {Material}
grid $shipyardview -sticky nwes
bind $shipyardview <<TreeviewSelect>> ShowRemoveInfo
grid [ttk::scrollbar $sremove.modules.scrolly -orient vertical -command [list $shipyardview yview]] -row 0 -column 1 -sticky ns
# Module info
set infoframe [ttk::frame $sremove.info]
grid [ttk::labelframe $infoframe.info -text {Module info:}]
set moduleinfo [text $infoframe.info.info -wrap char -height 10 -width 40]
$moduleinfo tag configure red -foreground red
grid $moduleinfo -sticky nwes
grid [ttk::label $infoframe.info.damagelbl]
grid [ttk::progressbar $infoframe.info.damage -orient horizontal -maximum 1.0] -row 1 -column 1
grid [ttk::label $infoframe.info.description]
grid [ttk::label $infoframe.money]
grid [ttk::button $infoframe.remove -text {Remove module} -command {ManipulateModule remove}]
grid $infoframe -column 1 -row 0
$shipyardframe.notebook add $sremove -text {Remove}
