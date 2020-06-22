grid [ttk::panedwindow .help] -sticky nwes
.help add [ttk::treeview .help.topics -show tree]
.help add [ttk::frame .help.content]
pack [ttk::scrollbar .help.content.scroll -orient vertical -command [list .help.content.view yview]] -side right -fill y
set helpview [text .help.content.view -wrap char -yscrollcommand [list .help.content.scroll set] -font {-family Roboto -size 14}]
$helpview tag configure special -foreground yellow -font {-family Roboto -size 14 -weight bold}
$helpview tag configure bold -font {-family Roboto -size 14 -weight bold}
$helpview tag configure underline -font {-family Roboto -size 14 -underline true}
$helpview tag configure italic -font {-family Roboto -size 14 -slant italic}
pack $helpview -side top -fill both
