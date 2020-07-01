ttk::frame .paned.cargoframe
set cargocanvas [canvas .paned.cargoframe.canvas -yscrollcommand [list .paned.cargoframe.scrolly set] -xscrollcommand [list .paned.cargoframe.scrollx set]]
grid $cargocanvas -sticky nwes
grid [ttk::scrollbar .paned.cargoframe.scrollx -orient horizontal -command [list $cargocanvas xview]] -row 1 -column 0 -columnspan 2 -sticky we
grid [ttk::scrollbar .paned.cargoframe.scrolly -orient vertical -command [list $cargocanvas yview]] -row 0 -column 1 -sticky ns
set cargoframe [ttk::frame $cargocanvas.cargo]
# Type of items to show
grid [ttk::frame $cargoframe.type]
grid [ttk::label $cargoframe.type.label -text {Type:}]
grid [ttk::combobox $cargoframe.type.combo -state readonly] -row 0 -column 1
bind $cargoframe.type.combo <<ComboboxSelected>> {ShowCargoInfo [$cargoframe.type.combo get]}
# Cargo list
grid [ttk::frame $cargoframe.cargo] -sticky nwes
set cargoview [ttk::treeview $cargoframe.cargo.view -columns [list name durability type amount weight] -show headings -yscrollcommand [list $cargoframe.cargo.scrolly set]]
$cargoview heading name -text {Name}
$cargoview heading durability -text {Durability}
$cargoview heading type -text {Type}
$cargoview heading amount -text {Amount}
$cargoview heading weight -text {Weight (in kg)}
grid $cargoview -sticky nwes
bind $cargoview <<TreeviewSelect>> ShowCargoItemInfo
grid [ttk::scrollbar $cargoframe.cargo.scrolly -orient vertical -command [list $cargoview yview]] -row 0 -column 1 -sticky ns
# Item info
set itemframe [ttk::frame $cargoframe.item]
grid [ttk::labelframe $itemframe.info -text {Item Info:}]
grid [text $itemframe.info.text -wrap char -height 10 -width 40]
# Item actions
grid [ttk::frame $itemframe.dropframe]
grid [ttk::button $itemframe.dropframe.button -text {Drop} -command {DropItem [$cargoframe.type.combo get]}]
grid [ttk::spinbox $itemframe.dropframe.amount -from 1 -increment 1 -validate key] -row 0 -column 1
grid [ttk::label $itemframe.dropframe.error -style Headerred.TLabel] -columnspan 2
grid [ttk::frame $itemframe.giveframe]
grid [ttk::button $itemframe.giveframe.button -text {Give} -command {GiveItem [$cargoframe.type.combo get]}]
grid [ttk::spinbox $itemframe.giveframe.amount -from 1 -increment 1 -validate key] -row 0 -column 1
grid [ttk::label $itemframe.giveframe.to -text {To:}] -row 0 -column 2
grid [ttk::combobox $itemframe.giveframe.member -state readonly] -row 0 -column 3
grid [ttk::label $itemframe.giveframe.error -style Headerred.TLabel] -columnspan 4
grid $itemframe -row 1 -column 1 -sticky nwes
