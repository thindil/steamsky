ttk::frame .paned.missionsframe
set missionscanvas [canvas .paned.missionsframe.canvas -yscrollcommand [list .paned.missionsframe.scrolly set] -xscrollcommand [list .paned.missionsframe.scrollx set]]
grid $missionscanvas -sticky nwes
grid [ttk::scrollbar .paned.missionsframe.scrollx -orient horizontal -command [list $missionscanvas xview]] -row 1 -column 0 -columnspan 2 -sticky we
grid [ttk::scrollbar .paned.missionsframe.scrolly -orient vertical -command [list $missionscanvas yview]] -row 0 -column 1 -sticky ns
set missionsframe [ttk::frame $missionscanvas.missions]
grid [ttk::treeview $missionsframe.missionsview -show headings -columns [list name distance]]
$missionsframe.missionsview heading name -text {Name}
$missionsframe.missionsview heading distance -text {Distance}
bind $missionsframe.missionsview <<TreeviewSelect>> ShowMissionInfo
grid [ttk::frame $missionsframe.info] -column 1 -row 0
grid [ttk::labelframe $missionsframe.info.info -text {Mission Info:}]
grid [text $missionsframe.info.info.text -wrap char -height 10 -width 40]
$missionsframe.info.info.text tag configure red -foreground red
$missionsframe.info.info.text tag configure yellow -foreground yellow
set reward 1.0
grid [ttk::scale $missionsframe.info.reward -from 0.0 -to 2.0 -variable reward]
tooltip::tooltip $missionsframe.info.reward "Move left - more reputation from mission but less money,\nmove right - more money from mission but less reputation."
grid [ttk::label $missionsframe.info.missioninfo]
grid [ttk::button $missionsframe.info.show -text {Show mission on map} -command {ShowMission}]
grid [ttk::button $missionsframe.info.set -command {SetMission}]
