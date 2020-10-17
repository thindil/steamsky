set knowledgeframe [ttk::frame .paned.knowledgeframe]
# Bases list
grid [ttk::labelframe $knowledgeframe.general -text {Known bases:}] -sticky nwes
set knowledgecanvas [canvas $knowledgeframe.general.canvas -yscrollcommand [list $knowledgeframe.general.scrolly set] -xscrollcommand [list $knowledgeframe.general.scrollx set]]
pack [ttk::scrollbar $knowledgeframe.general.scrolly -orient vertical -command [list $knowledgecanvas yview]] -side right -fill y
pack [ttk::scrollbar $knowledgeframe.general.scrollx -orient horizontal -command [list $knowledgecanvas xview]] -fill x -side bottom
pack $knowledgecanvas -side top -fill both -expand true
ttk::frame $knowledgecanvas.frame
grid columnconfigure $knowledgecanvas.frame 1 -weight 1
# Minimize/maximize button
grid [ttk::button $knowledgecanvas.frame.maxmin -style Header.Toolbutton -text "[format %c 0xf106]" -command {KnowledgeMaxMin general show}]
tooltip::tooltip $knowledgecanvas.frame.maxmin {Maximize/minimize the list of known bases}
$knowledgecanvas create window 0 0 -anchor nw -window $knowledgecanvas.frame
::autoscroll::autoscroll $knowledgeframe.general.scrolly
::autoscroll::autoscroll $knowledgeframe.general.scrollx
# Accepted missions list
grid [ttk::labelframe $knowledgeframe.modules -text {Accepted missions:}] -sticky nwes
set knowledgecanvas [canvas $knowledgeframe.modules.canvas -yscrollcommand [list $knowledgeframe.modules.scrolly set] -xscrollcommand [list $knowledgeframe.modules.scrollx set]]
pack [ttk::scrollbar $knowledgeframe.modules.scrolly -orient vertical -command [list $knowledgecanvas yview]] -side right -fill y
pack [ttk::scrollbar $knowledgeframe.modules.scrollx -orient horizontal -command [list $knowledgecanvas xview]] -fill x -side bottom
pack $knowledgecanvas -side top -fill both -expand true
ttk::frame $knowledgecanvas.frame
grid columnconfigure $knowledgecanvas.frame 1 -weight 1
grid [ttk::button $knowledgecanvas.frame.maxmin -style Header.Toolbutton -text "[format %c 0xf106]" -command {KnowledgeMaxMin modules show}] -sticky w
tooltip::tooltip $knowledgecanvas.frame.maxmin {Maximize/minimize the list of accepted missions}
$knowledgecanvas create window 0 0 -anchor nw -window $knowledgecanvas.frame
::autoscroll::autoscroll $knowledgeframe.modules.scrolly
::autoscroll::autoscroll $knowledgeframe.modules.scrollx
# Known events list
grid [ttk::labelframe $knowledgeframe.crew -text {Known events:}] -row 0 -column 1 -sticky nwes
set knowledgecanvas [canvas $knowledgeframe.crew.canvas -yscrollcommand [list $knowledgeframe.crew.scrolly set] -xscrollcommand [list $knowledgeframe.crew.scrollx set]]
pack [ttk::scrollbar $knowledgeframe.crew.scrolly -orient vertical -command [list $knowledgecanvas yview]] -side right -fill y
pack [ttk::scrollbar $knowledgeframe.crew.scrollx -orient horizontal -command [list $knowledgecanvas xview]] -fill x -side bottom
pack $knowledgecanvas -side top -fill both -expand true
ttk::frame $knowledgecanvas.frame
grid columnconfigure $knowledgecanvas.frame 1 -weight 1
grid [ttk::button $knowledgecanvas.frame.maxmin -style Header.Toolbutton -text "[format %c 0xf106]" -command {KnowledgeMaxMin crew show}] -sticky w
tooltip::tooltip $knowledgecanvas.frame.maxmin {Maximize/minimize the list of known events}
$knowledgecanvas create window 0 0 -anchor nw -window $knowledgecanvas.frame
::autoscroll::autoscroll $knowledgeframe.crew.scrolly
::autoscroll::autoscroll $knowledgeframe.crew.scrollx
# Known stories list
grid [ttk::labelframe $knowledgeframe.cargo -text {Known stories:}] -row 1 -column 1 -sticky nwes
set knowledgecanvas [canvas $knowledgeframe.cargo.canvas -yscrollcommand [list $knowledgeframe.cargo.scrolly set] -xscrollcommand [list $knowledgeframe.cargo.scrollx set]]
pack [ttk::scrollbar $knowledgeframe.cargo.scrolly -orient vertical -command [list $knowledgecanvas yview]] -side right -fill y
pack [ttk::scrollbar $knowledgeframe.cargo.scrollx -orient horizontal -command [list $knowledgecanvas xview]] -fill x -side bottom
pack $knowledgecanvas -side top -fill both -expand true
ttk::frame $knowledgecanvas.frame
grid columnconfigure $knowledgecanvas.frame 1 -weight 1
grid [ttk::button $knowledgecanvas.frame.maxmin -style Header.Toolbutton -text "[format %c 0xf106]" -command {KnowledgeMaxMin cargo show}] -sticky w
tooltip::tooltip $knowledgecanvas.frame.maxmin {Maximize/minimize the list of known stories}
$knowledgecanvas create window 0 0 -anchor nw -window $knowledgecanvas.frame
::autoscroll::autoscroll $knowledgeframe.cargo.scrolly
::autoscroll::autoscroll $knowledgeframe.cargo.scrollx
# Configure main ship info grid
grid columnconfigure $knowledgeframe 0 -weight 1
grid columnconfigure $knowledgeframe 1 -weight 1
grid rowconfigure $knowledgeframe 0 -weight 1
grid rowconfigure $knowledgeframe 1 -weight 1
