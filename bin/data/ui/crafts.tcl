ttk::frame .paned.craftframe
set craftcanvas [canvas .paned.craftframe.canvas -yscrollcommand [list .paned.craftframe.scrolly set] -xscrollcommand [list .paned.craftframe.scrollx set]]
grid $craftcanvas -sticky nwes
grid [ttk::scrollbar .paned.craftframe.scrollx -orient horizontal -command [list $craftcanvas xview]] -row 1 -column 0 -columnspan 2 -sticky we
grid [ttk::scrollbar .paned.craftframe.scrolly -orient vertical -command [list $craftcanvas yview]] -row 0 -column 1 -sticky ns
set craftframe [ttk::frame $craftcanvas.craft]
# Recipes list
grid [ttk::frame $craftframe.list] -sticky nwes
set craftview [ttk::treeview $craftframe.list.view -yscrollcommand [list $craftframe.list.scrolly set]]
$craftview heading #0 -text {Name}
$craftview tag configure gray -foreground gray
grid $craftview -sticky nwes
grid [ttk::scrollbar $craftframe.list.scrolly -orient vertical -command [list $craftview yview]] -row 0 -column 1 -sticky ns
# Recipe info
set recipeframe [ttk::frame $craftframe.item]
grid $recipeframe -row 0 -column 1
grid [ttk::labelframe $recipeframe.info -text {Recipe Info:}] -columnspan 4
grid [text $recipeframe.info.text -wrap char -height 10 -width 40]
$recipeframe.info.text tag configure red -foreground red
# Recipe actions
grid [ttk::button $recipeframe.button -text Craft]
grid [ttk::spinbox $recipeframe.amount -from 1 -increment 1 -validate key] -column 1 -row 1
grid [ttk::label $recipeframe.label -text in] -column 2 -row 1
grid [ttk::combobox $recipeframe.workshop -state readonly] -column 3 -row 1
