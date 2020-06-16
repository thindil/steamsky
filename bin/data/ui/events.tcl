ttk::frame .paned.eventsframe
set eventscanvas [canvas .paned.eventsframe.canvas -yscrollcommand [list .paned.eventsframe.scrolly set] -xscrollcommand [list .paned.eventsframe.scrollx set]]
grid $eventscanvas -sticky nwes
grid [ttk::scrollbar .paned.eventsframe.scrollx -orient horizontal -command [list $eventscanvas xview]] -row 1 -column 0 -columnspan 2 -sticky we
grid [ttk::scrollbar .paned.eventsframe.scrolly -orient vertical -command [list $eventscanvas yview]] -row 0 -column 1 -sticky ns
set eventsframe [ttk::frame $eventscanvas.events]
grid [ttk::treeview $eventsframe.eventsview -show headings -columns [list name distance]]
$eventsframe.eventsview heading name -text {Name}
$eventsframe.eventsview heading distance -text {Distance}
bind $eventsframe.eventsview <<TreeviewSelect>> ShowEventInfo
grid [ttk::frame $eventsframe.info] -column 1 -row 0
grid [ttk::labelframe $eventsframe.info.info -text {Event Info:}]
grid [ttk::label $eventsframe.info.info.label]
grid [ttk::button $eventsframe.info.show -text {Show event on map}]
grid [ttk::button $eventsframe.info.set -text {Set event as destination for ship}]
