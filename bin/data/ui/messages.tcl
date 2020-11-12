ttk::frame .gameframe.paned.messagesframe
set messagescanvas [canvas .gameframe.paned.messagesframe.canvas -yscrollcommand [list .gameframe.paned.messagesframe.scrolly set] -xscrollcommand [list .gameframe.paned.messagesframe.scrollx set]]
pack [ttk::scrollbar .gameframe.paned.messagesframe.scrolly -orient vertical -command [list $messagescanvas yview]] -side right -fill y
pack $messagescanvas -side top -fill both
pack [ttk::scrollbar .gameframe.paned.messagesframe.scrollx -orient horizontal -command [list $messagescanvas xview]] -fill x
set messagesframe [ttk::frame $messagescanvas.messages]
# Messages options
grid [ttk::frame $messagesframe.options] -sticky w
grid [ttk::combobox $messagesframe.options.types -values [list All Combat Trade Orders Craft Others Missions] -state readonly]
bind $messagesframe.options.types <<ComboboxSelected>> SelectMessages
$messagesframe.options.types current 0
grid [ttk::entry $messagesframe.options.search -validate key -validatecommand {SearchMessages %P}] -row 0 -column 1
grid [ttk::button $messagesframe.options.delete -text {Delete all messages} -command DeleteMessages] -row 0 -column 2
# Messages list
grid [ttk::frame $messagesframe.list] -sticky nwes
set messagesview2 [text $messagesframe.list.view]
$messagesview2 tag configure yellow -foreground yellow
$messagesview2 tag configure green -foreground #4e9a06
$messagesview2 tag configure red -foreground red
$messagesview2 tag configure cyan -foreground cyan
$messagesview2 tag configure blue -foreground #3465a4
$messagesview2 tag configure gray -foreground {dim gray}
pack $messagesview2 -side top -fill both -expand true
