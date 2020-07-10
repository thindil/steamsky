ttk::frame .paned.storiesframe
set storiescanvas [canvas .paned.storiesframe.canvas -yscrollcommand [list .paned.storiesframe.scrolly set] -xscrollcommand [list .paned.storiesframe.scrollx set]]
grid $storiescanvas -sticky nwes
grid [ttk::scrollbar .paned.storiesframe.scrollx -orient horizontal -command [list $storiescanvas xview]] -row 1 -column 0 -columnspan 2 -sticky we
grid [ttk::scrollbar .paned.storiesframe.scrolly -orient vertical -command [list $storiescanvas yview]] -row 0 -column 1 -sticky ns
set storiesframe [ttk::frame $storiescanvas.stories]
# List of stories options
grid [ttk::frame $storiesframe.options]
grid [ttk::combobox $storiesframe.options.titles -state readonly]
grid [ttk::button $storiesframe.options.show -text {Show on map}] -row 0 -column 1
grid [ttk::button $storiesframe.options.set -text {Set as destintion for ship}] -row 0 -column 2
# stories list
grid [ttk::frame $storiesframe.list] -sticky nwes
set storiesview [ttk::treeview $storiesframe.list.view -yscrollcommand [list $storiesframe.list.scrolly set] -show tree -selectmode none]
grid $storiesview -sticky nwes
grid [ttk::scrollbar $storiesframe.list.scrolly -orient vertical -command [list $storiesview yview]] -row 0 -column 1 -sticky ns
