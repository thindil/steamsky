ttk::frame .gameframe.paned.combatframe
set combatcanvas [canvas .gameframe.paned.combatframe.canvas -yscrollcommand [list .gameframe.paned.combatframe.scrolly set] -xscrollcommand [list .gameframe.paned.combatframe.scrollx set]]
pack [ttk::scrollbar .gameframe.paned.combatframe.scrolly -orient vertical -command [list $combatcanvas yview]] -side right -fill y
pack $combatcanvas -side top -fill both
pack [ttk::scrollbar .gameframe.paned.combatframe.scrollx -orient horizontal -command [list $combatcanvas xview]] -fill x
::autoscroll::autoscroll .gameframe.paned.combatframe.scrolly
::autoscroll::autoscroll .gameframe.paned.combatframe.scrollx

# Ship to ship combat
set combatframe [ttk::frame $combatcanvas.combat]
grid [ttk::button $combatframe.next -text {Next turn [Enter]} -command NextTurn] -columnspan 2 -sticky we
bind $combatframe.next <Return> {InvokeButton $combatframe.next}
focus $combatframe.next
grid [ttk::frame $combatframe.left] -sticky nw
grid [ttk::labelframe $combatframe.left.crew -text {Your ship crew orders:}] -padx 5 -pady {0 5}
grid [ttk::label $combatframe.left.crew.position -text {Position}]
grid [ttk::label $combatframe.left.crew.name -text {Name}] -row 0 -column 1
grid [ttk::label $combatframe.left.crew.order -text {Order}] -row 0 -column 2
grid [ttk::label $combatframe.left.crew.pilotlabel -text {Pilot:}] -row 1 -sticky w -padx {5 0} -pady {0 5}
grid [ttk::combobox $combatframe.left.crew.pilotcrew -state readonly -width 10] -row 1 -column 1 -pady {0 5}
bind $combatframe.left.crew.pilotcrew <Return> {InvokeButton $combatframe.next}
grid [ttk::combobox $combatframe.left.crew.pilotorder -state readonly -values [list {Go closer} {Keep distance} {Evade} {Escape}]] -row 1 -column 2 -padx {0 5} -pady {0 5}
bind $combatframe.left.crew.pilotorder <Return> {InvokeButton $combatframe.next}
bind $combatframe.left.crew.pilotorder <<ComboboxSelected>> {SetCombatOrder pilot}
grid [ttk::label $combatframe.left.crew.engineerlabel -text {Engineer:}] -row 2 -sticky w -padx {5 0} -pady {5 0}
grid [ttk::combobox $combatframe.left.crew.engineercrew -state readonly -width 10] -row 2 -column 1 -pady {5 0}
bind $combatframe.left.crew.engineercrew <Return> {InvokeButton $combatframe.next}
grid [ttk::combobox $combatframe.left.crew.engineerorder -state readonly -values [list {All stop} {Quarter speed} {Half speed} {Full speed}]] -row 2 -column 2 -padx {0 5} -pady {5 0}
bind $combatframe.left.crew.engineerorder <Return> {InvokeButton $combatframe.next}
bind $combatframe.left.crew.engineerorder <<ComboboxSelected>> {SetCombatOrder engineer}
grid [ttk::labelframe $combatframe.left.damage -text {Your ship damage:}] -sticky we -padx 5 -pady {5 0}
grid [ttk::frame $combatframe.right] -row 1 -column 1
grid [ttk::labelframe $combatframe.right.enemy -text {Enemy info:}] -sticky we -padx 5 -pady {0 5}
grid [ttk::label $combatframe.right.enemy.info -wraplength 350]
grid [ttk::labelframe $combatframe.right.status -text {Enemy ship status:}] -sticky we -padx 5 -pady {5 0}
grid [ttk::labelframe $combatframe.right.boarding -text {Boarding party:}]

# Boarding combat
set boardingframe [ttk::frame $combatcanvas.boarding]
grid [ttk::button $boardingframe.next -text {Next turn [Enter]} -command NextTurn] -columnspan 2 -sticky we
bind . <Return> {InvokeButton $boardingframe.next}
grid [ttk::labelframe $boardingframe.left -text {Your crew:}]
grid [ttk::label $boardingframe.left.name -text {Name}] -row 0 -column 0
grid [ttk::label $boardingframe.left.health -text {Health}] -row 0 -column 1
grid [ttk::label $boardingframe.left.order -text {Order}] -row 0 -column 2
grid [ttk::labelframe $boardingframe.right -text {Enemy's crew:}] -row 1 -column 1
grid [ttk::label $boardingframe.right.name -text {Name}] -row 0 -column 0
grid [ttk::label $boardingframe.right.health -text {Health}] -row 0 -column 1
grid [ttk::label $boardingframe.right.order -text {Order}] -row 0 -column 2
