# Game menu
menu .gamemenu -title {Steam Sky - menu}
.gamemenu add command -label {Ship information}
.gamemenu add command -label {Ship cargo}
.gamemenu add command -label {Crew information}
.gamemenu add cascade -label {Ship orders} -menu .gamemenu.orders
.gamemenu add command -label {Crafting}
.gamemenu add command -label {Last messages}
.gamemenu add command -label {List of known bases}
.gamemenu add command -label {List of known events}
.gamemenu add command -label {Accepted missions}
.gamemenu add command -label {Stories}
.gamemenu add command -label {Wait orders}
.gamemenu add command -label {Game statistics}
.gamemenu add command -label {Help}
.gamemenu add command -label {Game options}
.gamemenu add command -label {Quit from game}
.gamemenu add command -label {Resign from game}
# Orders menu
menu .gamemenu.orders -title {Steam Sky - orders}
.gamemenu.orders add command -label {Story}
.gamemenu.orders add command -label {Complete mission}
.gamemenu.orders add command -label {Attack} -underline 0
.gamemenu.orders add command -label {Deliver medicines for free} -underline 0
.gamemenu.orders add command -label {Deliver medicines for price} -underline 8
.gamemenu.orders add command -label {Undock} -underline 0
.gamemenu.orders add command -label {Escape} -underline 3
.gamemenu.orders add command -label {Trade} -underline 0
.gamemenu.orders add command -label {School} -underline 0
.gamemenu.orders add command -label {Recruit} -underline 0
.gamemenu.orders add command -label {Ask for events} -underline 8
.gamemenu.orders add command -label {Ask for bases} -underline 8
.gamemenu.orders add command -label {Pray}
.gamemenu.orders add command -label {Heal wounded} -underline 5
.gamemenu.orders add command -label {Repair ship} -underline 2
.gamemenu.orders add command -label {Shipyard} -underline 2
.gamemenu.orders add command -label {Buy recipes} -underline 2
.gamemenu.orders add command -label {Missions} -underline 0
.gamemenu.orders add command -label {Patrol area} -underline 0
.gamemenu.orders add command -label {Loot} -underline 0
.gamemenu.orders add command -label {Set as home} -underline 7
.gamemenu.orders add command -label {Close} -underline 0
# Game header
ttk::frame .header
grid [ttk::menubutton .header.menubutton -text {Menu} -menu .gamemenu] -sticky w
grid [ttk::button .header.closebutton -text {Close}] -row 0 -column 1 -sticky w
grid [ttk::label .header.time -text {1600-03-01}] -row 0 -column 2
grid columnconfigure .header .header.time -weight 1
grid [ttk::label .header.nofuel] -row 0 -column 3 -sticky e
grid [ttk::label .header.nofood] -row 0 -column 4 -sticky e
grid [ttk::label .header.nodrink] -row 0 -column 5 -sticky e
grid [ttk::label .header.overloaded] -row 0 -column 6 -sticky e
grid [ttk::button .header.pilot -text {[P]} -style Toolbutton] -row 0 -column 7 -sticky e
grid [ttk::button .header.engineer -text {[E]} -style Toolbutton] -row 0 -column 8 -sticky e
grid [ttk::button .header.gunner -text {[G]} -style Toolbutton] -row 0 -column 9 -sticky e
grid [ttk::button .header.talk -text {[T]} -style Toolbutton] -row 0 -column 10 -sticky e
grid [ttk::button .header.repairs -text {[R]} -style Toolbutton] -row 0 -column 11 -sticky e
grid [ttk::button .header.upgrade -text {[U]} -style Toolbutton] -row 0 -column 12 -sticky e
grid [ttk::button .header.clean -text {[C]} -style Toolbutton] -row 0 -column 13 -sticky e
grid [ttk::button .header.crafting -text {[M]} -style Toolbutton] -row 0 -column 14 -sticky e
grid .header -sticky we
ttk::panedwindow .paned
# Game map
.paned add [ttk::frame .paned.mapframe]
grid [text .paned.mapframe.map] -sticky nwes
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
grid [ttk::combobox $bframe.speed -state readonly -values [list {Full stop} {Quarted speed} {Half speed} {Full speed}]]
grid [ttk::button $bframe.moveto -text {Move to}] -row 0 -column 1 -columnspan 2
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
