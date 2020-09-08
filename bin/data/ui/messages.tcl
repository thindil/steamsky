ttk::frame .paned.messagesframe
set messagescanvas [canvas .paned.messagesframe.canvas -yscrollcommand [list .paned.messagesframe.scrolly set] -xscrollcommand [list .paned.messagesframe.scrollx set]]
pack [ttk::scrollbar .paned.messagesframe.scrolly -orient vertical -command [list $messagescanvas yview]] -side right -fill y
pack $messagescanvas -side top -fill both
pack [ttk::scrollbar .paned.messagesframe.scrollx -orient horizontal -command [list $messagescanvas xview]] -fill x
set messagesframe [ttk::frame $messagescanvas.messages]
# Messages options
grid [ttk::frame $messagesframe.options]
grid [ttk::combobox $messagesframe.options.types -values [list All Combat Trade Orders Craft Others Missions] -state readonly]
bind $messagesframe.options.types <<ComboboxSelected>> SelectMessages
$messagesframe.options.types current 0
grid [ttk::entry $messagesframe.options.search -validate key -validatecommand {SearchMessages %P}] -row 0 -column 1
grid [ttk::button $messagesframe.options.delete -text {Delete all messages} -command DeleteMessages] -row 0 -column 2
# Messages list
grid [ttk::frame $messagesframe.list] -sticky nwes
set messagesview [ttk::treeview $messagesframe.list.view -xscrollcommand [list $messagesframe.list.scrollx set] -yscrollcommand [list $messagesframe.list.scrolly set] -show tree -selectmode none]
$messagesview tag configure yellow -foreground yellow
$messagesview tag configure green -foreground green
$messagesview tag configure red -foreground red
$messagesview tag configure blue -foreground blue
$messagesview tag configure cyan -foreground cyan
pack [ttk::scrollbar $messagesframe.list.scrolly -orient vertical -command [list $messagesview yview]] -side right -fill y
pack [ttk::scrollbar $messagesframe.list.scrollx -orient horizontal -command [list $messagesview xview]] -fill x -side bottom
pack $messagesview -side top -fill both -expand true
