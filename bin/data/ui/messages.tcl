ttk::frame .paned.messagesframe
set messagescanvas [canvas .paned.messagesframe.canvas -yscrollcommand [list .paned.messagesframe.scrolly set] -xscrollcommand [list .paned.messagesframe.scrollx set]]
grid $messagescanvas -sticky nwes
grid [ttk::scrollbar .paned.messagesframe.scrollx -orient horizontal -command [list $messagescanvas xview]] -row 1 -column 0 -columnspan 2 -sticky we
grid [ttk::scrollbar .paned.messagesframe.scrolly -orient vertical -command [list $messagescanvas yview]] -row 0 -column 1 -sticky ns
set messagesframe [ttk::frame $messagescanvas.messages]
# Messages options
grid [ttk::frame $messagesframe.options]
grid [ttk::combobox $messagesframe.options.types -values [list All Combat Trade Orders Craft Others Missions] -state readonly]
$messagesframe.options.types current 0
grid [ttk::entry $messagesframe.options.search -validate key] -row 0 -column 1
grid [ttk::button $messagesframe.options.delete -text {Delete all messages}] -row 0 -column 2
# Messages list
grid [ttk::frame $messagesframe.list] -sticky nwes
set messagesview [ttk::treeview $messagesframe.list.view -yscrollcommand [list $messagesframe.list.scrolly set] -show tree -selectmode none]
$messagesview tag configure yellow -foreground yellow
$messagesview tag configure green -foreground green
$messagesview tag configure red -foreground red
$messagesview tag configure blue -foreground blue
$messagesview tag configure cyan -foreground cyan
grid $messagesview -sticky nwes
grid [ttk::scrollbar $messagesframe.list.scrolly -orient vertical -command [list $messagesview yview]] -row 0 -column 1 -sticky ns
