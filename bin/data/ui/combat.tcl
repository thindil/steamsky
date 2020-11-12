ttk::frame .gameframe.paned.combatframe
set combatcanvas [canvas .gameframe.paned.combatframe.canvas -yscrollcommand [list .gameframe.paned.combatframe.scrolly set] -xscrollcommand [list .gameframe.paned.combatframe.scrollx set]]
pack [ttk::scrollbar .gameframe.paned.combatframe.scrolly -orient vertical -command [list $combatcanvas yview]] -side right -fill y
pack $combatcanvas -side top -fill both
pack [ttk::scrollbar .gameframe.paned.combatframe.scrollx -orient horizontal -command [list $combatcanvas xview]] -fill x

# Ship to ship combat
set combatframe [ttk::frame $combatcanvas.combat]
grid [ttk::frame $combatframe.left]
grid [ttk::frame $combatframe.left.crew]
grid [ttk::label $combatframe.left.crew.position -text {Position}]
grid [ttk::label $combatframe.left.crew.name -text {Name}] -row 0 -column 1
grid [ttk::label $combatframe.left.crew.order -text {Order}] -row 0 -column 2
grid [ttk::label $combatframe.left.crew.pilotlabel -text {Pilot:}] -row 1
grid [ttk::combobox $combatframe.left.crew.pilotcrew -state readonly] -row 1 -column 1
grid [ttk::combobox $combatframe.left.crew.pilotorder -state readonly -values [list {Go closer} {Keep distance} {Evade} {Escape}]] -row 1 -column 2
bind $combatframe.left.crew.pilotorder <<ComboboxSelected>> {SetCombatOrder pilot}
grid [ttk::label $combatframe.left.crew.engineerlabel -text {Engineer:}] -row 2
grid [ttk::combobox $combatframe.left.crew.engineercrew -state readonly] -row 2 -column 1
grid [ttk::combobox $combatframe.left.crew.engineerorder -state readonly -values [list {All stop} {Quarter speed} {Half speed} {Full speed}]] -row 2 -column 2
bind $combatframe.left.crew.engineerorder <<ComboboxSelected>> {SetCombatOrder engineer}
grid [ttk::labelframe $combatframe.left.damage -text {Ship damage:}]
grid [ttk::frame $combatframe.right] -row 0 -column 1
grid [ttk::labelframe $combatframe.right.enemy -text {Enemy info:}]
grid [ttk::label $combatframe.right.enemy.info]
grid [ttk::label $combatframe.right.enemy.description]
grid [ttk::frame $combatframe.right.enemy.damage]
grid [ttk::labelframe $combatframe.right.boarding -text {Boarding party:}]
grid [ttk::button $combatframe.next -text {Next turn [Space]} -command NextTurn] -columnspan 2 -sticky we
bind . <space> {InvokeButton $combatframe.next}

# Boarding combat
set boardingframe [ttk::frame $combatcanvas.boarding]
grid [ttk::frame $boardingframe.left]
grid [ttk::frame $boardingframe.left.crew]
grid [ttk::label $boardingframe.left.crew.name -text {Name}] -row 0 -column 0
grid [ttk::label $boardingframe.left.crew.health -text {Health}] -row 0 -column 1
grid [ttk::label $boardingframe.left.crew.order -text {Order}] -row 0 -column 2
grid [ttk::frame $boardingframe.right] -row 0 -column 1
grid [ttk::frame $boardingframe.right.enemy]
grid [ttk::label $boardingframe.right.enemy.name -text {Name}] -row 0 -column 0
grid [ttk::label $boardingframe.right.enemy.health -text {Health}] -row 0 -column 1
grid [ttk::label $boardingframe.right.enemy.order -text {Order}] -row 0 -column 2
grid [ttk::button $boardingframe.next -text {Next turn [Space]} -command NextTurn] -columnspan 2 -sticky we
bind . <space> {InvokeButton $boardingframe.next}
