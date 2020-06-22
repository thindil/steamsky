toplevel .help
wm title .help {Steam Sky - Help}
grid [ttk::panedwindow .help.paned]
.help.paned add [ttk::treeview .help.paned.topics -show tree]
.help.paned add [ttk::frame .help.paned.content]
pack [ttk::scrollbar .help.paned.content.scroll -orient vertical -command [list .help.paned.content.view yview]] -side right -fill y
set helpview [text .help.paned.content.view -wrap char -yscrollcommand [list .help.paned.content.scroll set] -font {-family Roboto -size 14}]
$helpview tag configure special -foreground yellow -font {-family Roboto -size 14 -weight bold}
$helpview tag configure bold -font {-family Roboto -size 14 -weight bold}
$helpview tag configure underline -font {-family Roboto -size 14 -underline true}
$helpview tag configure italic -font {-family Roboto -size 14 -slant italic}
pack $helpview -side top -fill both
bind .help <Escape> {destroy .help}
