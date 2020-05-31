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
ttk::frame .header
grid [ttk::menubutton .header.menubutton -text {Menu} -menu .gamemenu] -sticky w
grid [ttk::button .header.closebutton -text {Close}] -row 0 -column 1 -sticky w
grid [ttk::label .header.time -text {1600-03-01}] -row 0 -column 2 -sticky we
grid [ttk::label .header.nofuel] -row 0 -column 3 -sticky e
grid [ttk::label .header.nofood] -row 0 -column 4 -sticky e
grid [ttk::label .header.nodrink] -row 0 -column 5 -sticky e
grid [ttk::label .header.overloaded] -row 0 -column 6 -sticky e
grid [ttk::button .header.pilot -text {[P]} -style Toolbutton] -row 0 -column 7 -sticky e
grid .header -sticky we
