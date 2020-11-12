ttk::frame .gameframe.paned.lootframe
set lootcanvas [canvas .gameframe.paned.lootframe.canvas -yscrollcommand [list .gameframe.paned.lootframe.scrolly set] -xscrollcommand [list .gameframe.paned.lootframe.scrollx set]]
pack [ttk::scrollbar .gameframe.paned.lootframe.scrolly -orient vertical -command [list $lootcanvas yview]] -side right -fill y
pack $lootcanvas -side top -fill both
pack [ttk::scrollbar .gameframe.paned.lootframe.scrollx -orient horizontal -command [list $lootcanvas xview]] -fill x
set lootframe [ttk::frame $lootcanvas.loot]
# Type of items to show
grid [ttk::frame $lootframe.options]
grid [ttk::label $lootframe.options.typelabel -text {Type:}]
grid [ttk::combobox $lootframe.options.type -state readonly] -column 1 -row 0
bind $lootframe.options.type <<ComboboxSelected>> {ShowLoot [$lootframe.options.type get]}
# Loot list
grid [ttk::frame $lootframe.loot] -sticky nwes
set lootview [ttk::treeview $lootframe.loot.view -columns [list name type durability owned available] -show headings -yscrollcommand [list $lootframe.loot.scrolly set]]
$lootview heading name -text {Name}
$lootview heading type -text {Type}
$lootview heading durability -text {Durability}
$lootview heading owned -text {Owned}
$lootview heading available -text {Available}
grid $lootview -sticky nwes
bind $lootview <<TreeviewSelect>> ShowLootItemInfo
grid [ttk::scrollbar $lootframe.loot.scrolly -orient vertical -command [list $lootview yview]] -row 0 -column 1 -sticky ns
# Item info
set itemframe [ttk::frame $lootframe.item]
grid [ttk::labelframe $itemframe.info -text {Item Info:}]
grid [text $itemframe.info.text -wrap char -height 10 -width 40]
# Item actions
grid [ttk::label $itemframe.shipspace]
grid [ttk::frame $itemframe.takeframe]
grid [ttk::button $itemframe.takeframe.take -text {Take} -command {LootItem take}]
grid [ttk::label $itemframe.takeframe.amountlbl] -column 1 -row 0
grid [ttk::spinbox $itemframe.takeframe.amount -from 1 -validate key] -column 2 -row 0
grid [ttk::label $itemframe.takeframe.orlbl -text {or}] -column 3 -row 0
grid [ttk::button $itemframe.takeframe.takemax -text {Take all} -command {LootItem takeall}] -column 4 -row 0
grid [ttk::frame $itemframe.dropframe]
grid [ttk::button $itemframe.dropframe.drop -text {Drop} -command {LootItem drop}]
grid [ttk::label $itemframe.dropframe.amountlbl] -column 1 -row 0
grid [ttk::spinbox $itemframe.dropframe.amount -from 1 -validate key] -column 2 -row 0
grid [ttk::label $itemframe.dropframe.orlbl -text {or}] -column 3 -row 0
grid [ttk::button $itemframe.dropframe.dropmax -text {Drop all} -command {LootItem dropall}] -column 4 -row 0
grid [ttk::label $itemframe.dropframe.error -style Headerred.TLabel] -columnspan 4
grid $itemframe -row 1 -column 1 -sticky nwes
