toplevel .help -class Dialog
wm title .help {Steam Sky - Help}
wm transient .help .
if {$tcl_platform(os) == "Linux"} {
   wm attributes .help -type dialog
}
grid [ttk::panedwindow .help.paned]
.help.paned add [ttk::frame .help.paned.topics]
pack [ttk::scrollbar .help.paned.topics.scroll -orient vertical -command [list .help.paned.topics.view yview]] -side right -fill y
pack [ttk::treeview .help.paned.topics.view -show tree -yscrollcommand [list .help.paned.topics.scroll set]] -side top -fill both
.help.paned add [ttk::frame .help.paned.content]
pack [ttk::scrollbar .help.paned.content.scroll -orient vertical -command [list .help.paned.content.view yview]] -side right -fill y
set helpview [text .help.paned.content.view -wrap word -yscrollcommand [list .help.paned.content.scroll set] -font HelpFont -width 70]
$helpview tag configure special -foreground yellow -font {-family Roboto -size 14 -weight bold}
$helpview tag configure bold -font {-family Roboto -size 14 -weight bold}
$helpview tag configure underline -font {-family Roboto -size 14 -underline true}
$helpview tag configure italic -font {-family Roboto -size 14 -slant italic}
pack $helpview -side top -fill both
bind .help <Escape> {CloseHelp}
