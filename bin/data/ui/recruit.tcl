# Copyright (c) 2020-2021 Bartek thindil Jasicki <thindil@laeran.pl>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

ttk::frame .gameframe.paned.recruitframe
set recruitcanvas [canvas .gameframe.paned.recruitframe.canvas -yscrollcommand [list .gameframe.paned.recruitframe.scrolly set] -xscrollcommand [list .gameframe.paned.recruitframe.scrollx set]]
pack [ttk::scrollbar .gameframe.paned.recruitframe.scrolly -orient vertical -command [list $recruitcanvas yview]] -side right -fill y
pack $recruitcanvas -side top -fill both
pack [ttk::scrollbar .gameframe.paned.recruitframe.scrollx -orient horizontal -command [list $recruitcanvas xview]] -fill x
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
