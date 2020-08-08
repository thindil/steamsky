ttk::frame .paned.recruitframe
set recruitcanvas [canvas .paned.recruitframe.canvas -yscrollcommand [list .paned.recruitframe.scrolly set] -xscrollcommand [list .paned.recruitframe.scrollx set]]
pack [ttk::scrollbar .paned.recruitframe.scrolly -orient vertical -command [list $recruitcanvas yview]] -side right -fill y
pack $recruitcanvas -side top -fill both
pack [ttk::scrollbar .paned.recruitframe.scrollx -orient horizontal -command [list $recruitcanvas xview]] -fill x
set recruitframe [ttk::frame $recruitcanvas.recruit]
# Recruits list
grid [ttk::frame $recruitframe.recruits] -sticky nwes
set recruitview [ttk::treeview $recruitframe.recruits.view -yscrollcommand [list $recruitframe.recruits.scrolly set]]
$recruitview heading #0 -text {Name}
grid $recruitview -sticky nwes
bind $recruitview <<TreeviewSelect>> ShowRecruitInfo
grid [ttk::scrollbar $recruitframe.recruits.scrolly -orient vertical -command [list $recruitview yview]] -row 0 -column 1 -sticky ns
# Recruit info
set infoframe [ttk::frame $recruitframe.recruit]
grid [ttk::labelframe $infoframe.info -text {Recruit Info:}]
grid [ttk::label $infoframe.info.info]
grid [ttk::labelframe $infoframe.info.stats -text {Statistics:}]
grid [ttk::labelframe $infoframe.info.skills -text {Skills:}]
grid [ttk::labelframe $infoframe.info.equipment -text {Equipment:}]
grid [ttk::treeview $infoframe.info.equipment.view -yscrollcommand [list $infoframe.info.equipment.scrolly set] -show tree -selectmode none] -sticky nwes
grid [ttk::scrollbar $infoframe.info.equipment.scrolly -orient vertical -command [list $infoframe.info.equipment.view yview]] -row 0 -column 1 -sticky ns
grid [ttk::label $infoframe.info.initialcost]
# Recruit actions
grid [ttk::label $infoframe.dailylbl -text {Daily payment: }]
grid [ttk::scale $infoframe.daily -from 0 -command NegotiateHire]
grid [ttk::label $infoframe.percentlbl -text {Percent of profit from trades: 0%}]
grid [ttk::scale $infoframe.percent -from 0 -to 10 -command NegotiateHire]
grid [ttk::label $infoframe.contractlbl -text {Contract time:}]
grid [ttk::combobox $infoframe.contract -state readonly -values [list {Pernament} {100 days} {30 days} {20 days} {10 days}]]
bind $infoframe.contract <<ComboboxSelected>> NegotiateHire
$infoframe.contract current 0
grid [ttk::label $infoframe.money]
grid [ttk::label $infoframe.cost]
grid [ttk::button $infoframe.hire -text {Hire} -command Hire]
grid $infoframe -column 1 -row 0
