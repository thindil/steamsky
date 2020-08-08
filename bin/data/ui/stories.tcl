ttk::frame .paned.storiesframe
set storiescanvas [canvas .paned.storiesframe.canvas -yscrollcommand [list .paned.storiesframe.scrolly set] -xscrollcommand [list .paned.storiesframe.scrollx set]]
pack [ttk::scrollbar .paned.storiesframe.scrolly -orient vertical -command [list $storiescanvas yview]] -side right -fill y
pack $storiescanvas -side top -fill both
pack [ttk::scrollbar .paned.storiesframe.scrollx -orient horizontal -command [list $storiescanvas xview]] -fill x
set storiesframe [ttk::frame $storiescanvas.stories]
# List of stories options
grid [ttk::frame $storiesframe.options]
grid [ttk::combobox $storiesframe.options.titles -state readonly]
bind $storiesframe.options.titles <<ComboboxSelected>> ShowStory
grid [ttk::button $storiesframe.options.show -text {Show on map} -command ShowStoryLocation] -row 0 -column 1
grid [ttk::button $storiesframe.options.set -text {Set as destintion for ship} -command SetStory] -row 0 -column 2
# stories list
grid [ttk::frame $storiesframe.list] -sticky nwes
set storiesview [text $storiesframe.list.view -yscrollcommand [list $storiesframe.list.scrolly set]]
grid $storiesview -sticky nwes
grid [ttk::scrollbar $storiesframe.list.scrolly -orient vertical -command [list $storiesview yview]] -row 0 -column 1 -sticky ns
