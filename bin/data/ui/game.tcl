ttk::frame .header
menu .gamemenu -title {Steam Sky - menu}
.gamemenu add command -label {Ship information}
.gamemenu add command -label {Ship cargo}
.gamemenu add command -label {Crew information}
.gamemenu add command -label {Ship orders}
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
grid [ttk::menubutton .header.menubutton -text {Menu} -menu .gamemenu] -sticky w
grid .header -sticky we
