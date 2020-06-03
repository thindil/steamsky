# Game menu
menu .gamemenu -title {Steam Sky - menu}
# Orders menu
menu .orders -title {Steam Sky - orders} -tearoff false
# Game header
ttk::frame .header
grid [ttk::menubutton .header.menubutton -text {Menu} -menu .gamemenu] -sticky w
ttk::button .header.closebutton -text {Close}
grid [ttk::label .header.time -text {1600-03-01}] -row 0 -column 2
tooltip::tooltip .header.time {The game time}
grid columnconfigure .header .header.time -weight 1
grid [ttk::label .header.nofuel -text "[format %c 0xf2c9]" -font {-family "Font Awesome 5 Free Solid" -size 14}] -row 0 -column 3 -sticky e
grid [ttk::label .header.nofood -text "[format %c 0xf787]" -font {-family "Font Awesome 5 Free Solid" -size 14}] -row 0 -column 4 -sticky e
grid [ttk::label .header.nodrink -text "[format %c 0xf72f]" -font {-family "Font Awesome 5 Free Solid" -size 14}] -row 0 -column 5 -sticky e
grid [ttk::label .header.overloaded -text "[format %c 0xf55b]" -font {-family "Font Awesome 5 Free Solid" -size 14}] -row 0 -column 6 -sticky e
grid [ttk::button .header.pilot -text "[format %c 0xf655]" -style Header.Toolbutton] -row 0 -column 7 -sticky e
grid [ttk::button .header.engineer -text "[format %c 0xf013]" -style Header.Toolbutton] -row 0 -column 8 -sticky e
grid [ttk::button .header.gunner -text "[format %c 0xf4fb]" -style Header.Toolbutton] -row 0 -column 9 -sticky e
grid [ttk::button .header.talk -text "[format %c 0xf651]" -style Header.Toolbutton] -row 0 -column 10 -sticky e
grid [ttk::button .header.repairs -text "[format %c 0xf54a]" -style Header.Toolbutton] -row 0 -column 11 -sticky e
grid [ttk::button .header.upgrade -text "[format %c 0xf6e3]" -style Header.Toolbutton] -row 0 -column 12 -sticky e
grid [ttk::button .header.clean -text "[format %c 0xf458]" -style Header.Toolbutton] -row 0 -column 13 -sticky e
grid [ttk::button .header.crafting -text "[format %c 0xf0e3]" -style Header.Toolbutton] -row 0 -column 14 -sticky e
grid .header -sticky we
ttk::panedwindow .paned
# Game map
.paned add [ttk::frame .paned.mapframe]
grid [text .paned.mapframe.map -bg black -fg white -font {-family {Hack NF} -size 16}] -sticky nwes
.paned.mapframe.map tag configure unvisited -background #1f2223
.paned.mapframe.map tag configure yellow -foreground yellow
.paned.mapframe.map tag configure green -foreground #4e9a06
.paned.mapframe.map tag configure red -foreground red
.paned.mapframe.map tag configure cyan -foreground cyan
.paned.mapframe.map tag configure lime -foreground lime
.paned.mapframe.map tag configure red2 -foreground #a40000
.paned.mapframe.map tag configure red3 -foreground #732727
.paned.mapframe.map tag configure green2 -foreground #73d216
set mframe [ttk::frame .paned.mapframe.buttons]
grid [ttk::button $mframe.show -text "[format %c 0x2b9d]" -style Toolbutton] -columnspan 5 -sticky we
grid [ttk::button $mframe.left -text "[format %c 0x2b9c]" -style Toolbutton] -rowspan 3 -row 1 -column 0 -sticky ns
grid [ttk::button $mframe.nw -text {NW} -style Toolbutton] -row 1 -column 1
grid [ttk::button $mframe.n -text {N} -style Toolbutton] -column 2 -row 1
grid [ttk::button $mframe.ne -text {NE} -style Toolbutton] -column 3 -row 1
grid [ttk::button $mframe.right -text "[format %c 0x2b9e]" -style Toolbutton] -rowspan 3 -row 1 -column 4 -sticky ns
grid [ttk::button $mframe.w -text {W} -style Toolbutton] -row 2 -column 1
grid [ttk::button $mframe.wait -text {...} -style Toolbutton] -column 2 -row 2
grid [ttk::button $mframe.e -text {E} -style Toolbutton] -column 3 -row 2
grid [ttk::button $mframe.sw -text {SW} -style Toolbutton] -row 3 -column 1
grid [ttk::button $mframe.s -text {S} -style Toolbutton] -column 2 -row 3
grid [ttk::button $mframe.se -text {SE} -style Toolbutton] -column 3 -row 3
grid [ttk::button $mframe.hide -text "[format %c 0x2b9f]" -style Toolbutton] -columnspan 5 -row 4 -sticky we
grid $mframe -row 0 -column 0 -sticky se
grid [ttk::frame .paned.mapframe.info] -column 0 -row 0 -sticky ne
grid [ttk::label .paned.mapframe.info.info] -sticky nwes
grid rowconfigure .paned.mapframe 0 -weight 1
grid columnconfigure .paned.mapframe 0 -weight 1
# Last messages
.paned add [ttk::frame .paned.controls]
grid [ttk::labelframe .paned.controls.messages] -sticky w
grid [text .paned.controls.messages.view -wrap word -yscrollcommand [list .paned.controls.messages.scroll set]] -sticky nwes
grid [ttk::scrollbar .paned.controls.messages.scroll -orient vertical -command [list .paned.controls.messages.view yview]] -sticky ns -column 1 -row 0
# Movement buttons
set bframe [ttk::frame .paned.controls.buttons]
grid $bframe -row 0 -column 1 -sticky nw
grid [ttk::combobox $bframe.speed -state readonly -values [list {Full stop} {Quarted speed} {Half speed} {Full speed}] -width 10] -columnspan 2 -sticky we
grid [ttk::button $bframe.moveto -text {Move to}] -row 0 -column 2
grid [ttk::button $bframe.nw -text {NW}] -row 1
grid [ttk::button $bframe.n -text {N}] -column 1 -row 1
grid [ttk::button $bframe.ne -text {NE}] -column 2 -row 1
grid [ttk::button $bframe.w -text {W}] -row 2
grid [ttk::button $bframe.wait -text {Wait}] -column 1 -row 2
grid [ttk::button $bframe.e -text {E}] -column 2 -row 2
grid [ttk::button $bframe.sw -text {SW}] -row 3
grid [ttk::button $bframe.s -text {S}] -column 1 -row 3
grid [ttk::button $bframe.se -text {SE}] -column 2 -row 3
grid columnconfigure .paned.controls .paned.controls.messages -weight 1
grid .paned -sticky nwes
grid rowconfigure . .paned -weight 1
grid columnconfigure . .paned -weight 1
