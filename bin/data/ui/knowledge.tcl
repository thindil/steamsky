set knowledgeframe [ttk::frame .paned.knowledgeframe]
# Bases list
grid [ttk::labelframe $knowledgeframe.bases -text {Known bases:}] -sticky nwes
set knowledgecanvas [canvas $knowledgeframe.bases.canvas -yscrollcommand [list $knowledgeframe.bases.scrolly set] -xscrollcommand [list $knowledgeframe.bases.scrollx set]]
pack [ttk::scrollbar $knowledgeframe.bases.scrolly -orient vertical -command [list $knowledgecanvas yview]] -side right -fill y
pack [ttk::scrollbar $knowledgeframe.bases.scrollx -orient horizontal -command [list $knowledgecanvas xview]] -fill x -side bottom
pack $knowledgecanvas -side top -fill both -expand true
ttk::frame $knowledgecanvas.frame
grid columnconfigure $knowledgecanvas.frame 1 -weight 1
# Minimize/maximize button
grid [ttk::button $knowledgecanvas.frame.maxmin -style Header.Toolbutton -text "[format %c 0xf106]" -command {KnowledgeMaxMin general show}]
tooltip::tooltip $knowledgecanvas.frame.maxmin {Maximize/minimize the list of known bases}
$knowledgecanvas create window 0 0 -anchor nw -window $knowledgecanvas.frame
::autoscroll::autoscroll $knowledgeframe.bases.scrolly
::autoscroll::autoscroll $knowledgeframe.bases.scrollx
# Accepted missions list
grid [ttk::labelframe $knowledgeframe.missions -text {Accepted missions:}] -sticky nwes
set knowledgecanvas [canvas $knowledgeframe.missions.canvas -yscrollcommand [list $knowledgeframe.missions.scrolly set] -xscrollcommand [list $knowledgeframe.missions.scrollx set]]
pack [ttk::scrollbar $knowledgeframe.missions.scrolly -orient vertical -command [list $knowledgecanvas yview]] -side right -fill y
pack [ttk::scrollbar $knowledgeframe.missions.scrollx -orient horizontal -command [list $knowledgecanvas xview]] -fill x -side bottom
pack $knowledgecanvas -side top -fill both -expand true
ttk::frame $knowledgecanvas.frame
grid columnconfigure $knowledgecanvas.frame 1 -weight 1
grid [ttk::button $knowledgecanvas.frame.maxmin -style Header.Toolbutton -text "[format %c 0xf106]" -command {KnowledgeMaxMin missions show}] -sticky w
tooltip::tooltip $knowledgecanvas.frame.maxmin {Maximize/minimize the list of accepted missions}
$knowledgecanvas create window 0 0 -anchor nw -window $knowledgecanvas.frame
::autoscroll::autoscroll $knowledgeframe.missions.scrolly
::autoscroll::autoscroll $knowledgeframe.missions.scrollx
# Known events list
grid [ttk::labelframe $knowledgeframe.events -text {Known events:}] -row 0 -column 1 -sticky nwes
set knowledgecanvas [canvas $knowledgeframe.events.canvas -yscrollcommand [list $knowledgeframe.events.scrolly set] -xscrollcommand [list $knowledgeframe.events.scrollx set]]
pack [ttk::scrollbar $knowledgeframe.events.scrolly -orient vertical -command [list $knowledgecanvas yview]] -side right -fill y
pack [ttk::scrollbar $knowledgeframe.events.scrollx -orient horizontal -command [list $knowledgecanvas xview]] -fill x -side bottom
pack $knowledgecanvas -side top -fill both -expand true
ttk::frame $knowledgecanvas.frame
grid columnconfigure $knowledgecanvas.frame 1 -weight 1
grid [ttk::button $knowledgecanvas.frame.maxmin -style Header.Toolbutton -text "[format %c 0xf106]" -command {KnowledgeMaxMin events show}] -sticky w
tooltip::tooltip $knowledgecanvas.frame.maxmin {Maximize/minimize the list of known events}
$knowledgecanvas create window 0 0 -anchor nw -window $knowledgecanvas.frame
::autoscroll::autoscroll $knowledgeframe.events.scrolly
::autoscroll::autoscroll $knowledgeframe.events.scrollx
# Known stories list
grid [ttk::labelframe $knowledgeframe.stories -text {Known stories:}] -row 1 -column 1 -sticky nwes
set knowledgecanvas [canvas $knowledgeframe.stories.canvas -yscrollcommand [list $knowledgeframe.stories.scrolly set] -xscrollcommand [list $knowledgeframe.stories.scrollx set]]
pack [ttk::scrollbar $knowledgeframe.stories.scrolly -orient vertical -command [list $knowledgecanvas yview]] -side right -fill y
pack [ttk::scrollbar $knowledgeframe.stories.scrollx -orient horizontal -command [list $knowledgecanvas xview]] -fill x -side bottom
pack $knowledgecanvas -side top -fill both -expand true
ttk::frame $knowledgecanvas.frame
grid columnconfigure $knowledgecanvas.frame 1 -weight 1
grid [ttk::button $knowledgecanvas.frame.maxmin -style Header.Toolbutton -text "[format %c 0xf106]" -command {KnowledgeMaxMin stories show}] -sticky w
tooltip::tooltip $knowledgecanvas.frame.maxmin {Maximize/minimize the list of known stories}
$knowledgecanvas create window 0 0 -anchor nw -window $knowledgecanvas.frame
::autoscroll::autoscroll $knowledgeframe.stories.scrolly
::autoscroll::autoscroll $knowledgeframe.stories.scrollx
# Configure main ship info grid
grid columnconfigure $knowledgeframe 0 -weight 1
grid columnconfigure $knowledgeframe 1 -weight 1
grid rowconfigure $knowledgeframe 0 -weight 1
grid rowconfigure $knowledgeframe 1 -weight 1
