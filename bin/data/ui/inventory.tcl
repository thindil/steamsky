ttk::frame .paned.inventoryframe
set inventorycanvas [canvas .paned.inventoryframe.canvas -yscrollcommand [list .paned.inventoryframe.scrolly set] -xscrollcommand [list .paned.inventoryframe.scrollx set]]
grid $inventorycanvas -sticky nwes
grid [ttk::scrollbar .paned.inventoryframe.scrollx -orient horizontal -command [list $inventorycanvas xview]] -row 1 -column 0 -columnspan 2 -sticky we
grid [ttk::scrollbar .paned.inventoryframe.scrolly -orient vertical -command [list $inventorycanvas yview]] -row 0 -column 1 -sticky ns
set inventoryframe [ttk::frame $inventorycanvas.inventory]
# Items list
grid [ttk::frame $inventoryframe.list] -sticky nwes
set inventoryview [ttk::treeview $inventoryframe.list.view -columns [list name used durability type amount weight] -show headings -yscrollcommand [list $inventoryframe.list.scrolly set]]
$inventoryview heading name -text {Name}
$inventoryview heading used -text {Used}
$inventoryview heading durability -text {Durability}
$inventoryview heading type -text {Type}
$inventoryview heading amount -text {Amount}
$inventoryview heading weight -text {Weight (in kg)}
grid $inventoryview -sticky nwes
grid [ttk::scrollbar $inventoryframe.list.scrolly -orient vertical -command [list $inventoryview yview]] -row 0 -column 1 -sticky ns
# Item info
set itemframe [ttk::frame $inventoryframe.item]
grid [ttk::labelframe $itemframe.info -text {Item Info:}]
grid [text $itemframe.info.text -wrap char -height 10 -width 40]
# Item actions
grid [ttk::checkbutton $itemframe.use -variable useitem -text {Used}]
grid [ttk::button $itemframe.button -text {Move}]
grid [ttk::spinbox $itemframe.amount -from 1 -increment 1 -validate key] -row 2 -column 1
grid [ttk::label $itemframe.label -text {to ship cargo.}] -row 2 -column 2
