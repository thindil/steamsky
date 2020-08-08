ttk::frame .paned.eventsframe
set eventscanvas [canvas .paned.eventsframe.canvas -yscrollcommand [list .paned.eventsframe.scrolly set] -xscrollcommand [list .paned.eventsframe.scrollx set]]
pack [ttk::scrollbar .paned.eventsframe.scrolly -orient vertical -command [list $eventscanvas yview]] -side right -fill y
pack $eventscanvas -side top -fill both
pack [ttk::scrollbar .paned.eventsframe.scrollx -orient horizontal -command [list $eventscanvas xview]] -fill x
set eventsframe [ttk::frame $eventscanvas.events]
grid [ttk::treeview $eventsframe.eventsview -show headings -columns [list name distance]]
$eventsframe.eventsview heading name -text {Name}
$eventsframe.eventsview heading distance -text {Distance}
bind $eventsframe.eventsview <<TreeviewSelect>> ShowEventInfo
grid [ttk::frame $eventsframe.info] -column 1 -row 0
grid [ttk::labelframe $eventsframe.info.info -text {Event Info:}]
grid [ttk::label $eventsframe.info.info.label]
grid [ttk::button $eventsframe.info.show -text {Show event on map} -command {ShowEvent}]
grid [ttk::button $eventsframe.info.set -text {Set event as destination for ship} -command {SetEvent}]
