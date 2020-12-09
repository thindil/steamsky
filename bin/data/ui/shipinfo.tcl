set shipinfoframe [ttk::frame .gameframe.paned.shipinfoframe]
# General ship info
grid [ttk::labelframe $shipinfoframe.general -text {General Info:}] -sticky nwes -padx 4
set shipcanvas [canvas $shipinfoframe.general.canvas -yscrollcommand [list $shipinfoframe.general.scrolly set] -xscrollcommand [list $shipinfoframe.general.scrollx set]]
pack [ttk::scrollbar $shipinfoframe.general.scrolly -orient vertical -command [list $shipcanvas yview]] -side right -fill y
pack [ttk::scrollbar $shipinfoframe.general.scrollx -orient horizontal -command [list $shipcanvas xview]] -fill x -side bottom
pack $shipcanvas -side top -fill both -expand true
ttk::frame $shipcanvas.frame
grid columnconfigure $shipcanvas.frame 1 -weight 1
# Minimize/maximize button
grid [ttk::button $shipcanvas.frame.maxmin -style Small.TButton -text "[format %c 0xf106]" -command {ShipMaxMin general show}]
tooltip::tooltip $shipcanvas.frame.maxmin {Maximize/minimize the ship general info}
# Ship name
grid [ttk::label $shipcanvas.frame.namelbl -text {Name:}] -sticky w
grid [ttk::label $shipcanvas.frame.name -textvariable shipname] -column 1 -row 1 -sticky w
grid [ttk::button $shipcanvas.frame.rename -text "[format %c 0xf044]" -style Small.TButton -command {
   GetString {Enter a new name:} shipname
}] -column 2 -row 1 -sticky w
tooltip::tooltip $shipcanvas.frame.rename {Set a new name for the ship}
bind $shipcanvas.frame.name <Enter> SetShipName
# Upgrade progress
grid [ttk::label $shipcanvas.frame.upgradelabel -text {Upgrade:}] -sticky w -columnspan 3
grid [ttk::progressbar $shipcanvas.frame.upgrade -orient horizontal -maximum 1.0] -sticky we -columnspan 2
tooltip::tooltip $shipcanvas.frame.upgrade {The current ship's upgrade progress}
grid [ttk::button $shipcanvas.frame.cancelupgrade -text "[format %c 0xf04d]" -style Small.TButton -command StopUpgrading] -row 3 -column 2 -sticky w
tooltip::tooltip $shipcanvas.frame.cancelupgrade {Stop the current upgrade}
# Repair priority
grid [ttk::label $shipcanvas.frame.repairlabel] -columnspan 2 -sticky we
grid [ttk::button $shipcanvas.frame.cancelpriority -text "[format %c 0xf05e]" -style Small.TButton -command {SetRepair remove}] -row 4 -column 2 -sticky w
tooltip::tooltip $shipcanvas.frame.cancelpriority {Remove the repair priority}
# Ship destination
grid [ttk::label $shipcanvas.frame.destinationlabel] -columnspan 2 -sticky we
grid [ttk::button $shipcanvas.frame.canceldestination -text "[format %c 0xf05e]" -style Small.TButton -command {ResetDestination}] -row 5 -column 2 -sticky w
tooltip::tooltip $shipcanvas.frame.canceldestination {Reset the ship destination}
# Ship home base
grid [ttk::label $shipcanvas.frame.homelabel] -columnspan 2 -sticky we
grid [ttk::button $shipcanvas.frame.showhome -text "[format %c 0xf06e]" -style Small.TButton -command {ShowShipInfo;update;MoveMap centeronhome}] -row 6 -column 2 -sticky w
tooltip::tooltip $shipcanvas.frame.showhome {Show the home base on map}
# Ship weight
grid [ttk::label $shipcanvas.frame.weight] -columnspan 2 -sticky we
$shipcanvas create window 0 0 -anchor nw -window $shipcanvas.frame
::autoscroll::autoscroll $shipinfoframe.general.scrolly
::autoscroll::autoscroll $shipinfoframe.general.scrollx
# Ship modules
grid [ttk::labelframe $shipinfoframe.modules -text {Modules Info:}] -sticky nwes -padx 4
set shipcanvas [canvas $shipinfoframe.modules.canvas -yscrollcommand [list $shipinfoframe.modules.scrolly set] -xscrollcommand [list $shipinfoframe.modules.scrollx set]]
pack [ttk::scrollbar $shipinfoframe.modules.scrolly -orient vertical -command [list $shipcanvas yview]] -side right -fill y
pack [ttk::scrollbar $shipinfoframe.modules.scrollx -orient horizontal -command [list $shipcanvas xview]] -fill x -side bottom
pack $shipcanvas -side top -fill both -expand true
ttk::frame $shipcanvas.frame
grid columnconfigure $shipcanvas.frame 1 -weight 1
grid [ttk::button $shipcanvas.frame.maxmin -style Small.TButton -text "[format %c 0xf106]" -command {ShipMaxMin modules show}] -sticky w
tooltip::tooltip $shipcanvas.frame.maxmin {Maximize/minimize the ship modules info}
grid [ttk::label $shipcanvas.frame.name -text {Name}]
grid [ttk::label $shipcanvas.frame.durability -text {Durability}] -column 1 -row 1
$shipcanvas create window 0 0 -anchor nw -window $shipcanvas.frame
::autoscroll::autoscroll $shipinfoframe.modules.scrolly
::autoscroll::autoscroll $shipinfoframe.modules.scrollx
# Crew info
grid [ttk::labelframe $shipinfoframe.crew -text {Crew Info:}] -row 0 -column 1 -sticky nwes -padx 4
set shipcanvas [canvas $shipinfoframe.crew.canvas -yscrollcommand [list $shipinfoframe.crew.scrolly set] -xscrollcommand [list $shipinfoframe.crew.scrollx set]]
pack [ttk::scrollbar $shipinfoframe.crew.scrolly -orient vertical -command [list $shipcanvas yview]] -side right -fill y
pack [ttk::scrollbar $shipinfoframe.crew.scrollx -orient horizontal -command [list $shipcanvas xview]] -fill x -side bottom
pack $shipcanvas -side top -fill both -expand true
ttk::frame $shipcanvas.frame
grid columnconfigure $shipcanvas.frame 1 -weight 1
grid [ttk::button $shipcanvas.frame.maxmin -style Small.TButton -text "[format %c 0xf106]" -command {ShipMaxMin crew show}] -sticky w
tooltip::tooltip $shipcanvas.frame.maxmin {Maximize/minimize the ship crew info}
$shipcanvas create window 0 0 -anchor nw -window $shipcanvas.frame
::autoscroll::autoscroll $shipinfoframe.crew.scrolly
::autoscroll::autoscroll $shipinfoframe.crew.scrollx
# Cargo info
grid [ttk::labelframe $shipinfoframe.cargo -text {Cargo Info:}] -row 1 -column 1 -sticky nwes -padx 4
set shipcanvas [canvas $shipinfoframe.cargo.canvas -yscrollcommand [list $shipinfoframe.cargo.scrolly set] -xscrollcommand [list $shipinfoframe.cargo.scrollx set]]
pack [ttk::scrollbar $shipinfoframe.cargo.scrolly -orient vertical -command [list $shipcanvas yview]] -side right -fill y
pack [ttk::scrollbar $shipinfoframe.cargo.scrollx -orient horizontal -command [list $shipcanvas xview]] -fill x -side bottom
pack $shipcanvas -side top -fill both -expand true
ttk::frame $shipcanvas.frame
grid columnconfigure $shipcanvas.frame 1 -weight 1
grid [ttk::button $shipcanvas.frame.maxmin -style Small.TButton -text "[format %c 0xf106]" -command {ShipMaxMin cargo show}] -sticky w
tooltip::tooltip $shipcanvas.frame.maxmin {Maximize/minimize the ship cargo info}
grid [ttk::frame $shipcanvas.frame.selecttype] -columnspan 5 -sticky w
grid [ttk::label $shipcanvas.frame.selecttype.label -text {Type:}]
grid [ttk::combobox $shipcanvas.frame.selecttype.combo -state readonly] -row 0 -column 1
bind $shipcanvas.frame.selecttype.combo <<ComboboxSelected>> ShowCargo
grid [ttk::label $shipcanvas.frame.name -text {Name}]
grid [ttk::label $shipcanvas.frame.durability -text {Durability}] -column 1 -row 2
grid [ttk::label $shipcanvas.frame.type -text {Type}] -column 2 -row 2
grid [ttk::label $shipcanvas.frame.amount -text {Amount}] -column 3 -row 2
grid [ttk::label $shipcanvas.frame.weight -text {Weight}] -column 4 -row 2
$shipcanvas create window 0 0 -anchor nw -window $shipcanvas.frame
::autoscroll::autoscroll $shipinfoframe.cargo.scrolly
::autoscroll::autoscroll $shipinfoframe.cargo.scrollx
# Configure main ship info grid
grid columnconfigure $shipinfoframe 0 -weight 1
grid columnconfigure $shipinfoframe 1 -weight 1
grid rowconfigure $shipinfoframe 0 -weight 1
grid rowconfigure $shipinfoframe 1 -weight 1
