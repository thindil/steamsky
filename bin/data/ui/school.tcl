# Copyright (c) 2020-2024 Bartek thindil Jasicki
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

ttk::frame .gameframe.paned.schoolframe
set schoolcanvas [canvas .gameframe.paned.schoolframe.canvas \
   -yscrollcommand [list .gameframe.paned.schoolframe.scrolly set] \
   -xscrollcommand [list .gameframe.paned.schoolframe.scrollx set]]
pack [ttk::scrollbar .gameframe.paned.schoolframe.scrolly -orient vertical \
   -command [list $schoolcanvas yview]] -side right -fill y
pack $schoolcanvas -side top -fill both
SetScrollbarBindings $schoolcanvas .gameframe.paned.schoolframe.scrolly
pack [ttk::scrollbar .gameframe.paned.schoolframe.scrollx -orient horizontal \
   -command [list $schoolcanvas xview]] -fill x
::autoscroll::autoscroll .gameframe.paned.schoolframe.scrolly
::autoscroll::autoscroll .gameframe.paned.schoolframe.scrollx
set schoolframe [ttk::frame $schoolcanvas.school]
SetScrollbarBindings $schoolframe .gameframe.paned.schoolframe.scrolly
set traintype amount
grid [ttk::frame $schoolframe.money] -sticky w -pady 5 -padx 5
grid [ttk::label $schoolframe.money.moneylbl] -sticky w
grid [ttk::label $schoolframe.money.money] -sticky w -column 1 -row 0
grid [ttk::frame $schoolframe.setting] -padx 5
grid [ttk::button $schoolframe.setting.train -text {Train:} \
   -command TrainSkill]
tooltip::tooltip $schoolframe.setting.train \
   {Train the selected skill of the selected crew member}
grid [ttk::combobox $schoolframe.setting.crew -state readonly] -row 0 \
   -column 1
tooltip::tooltip $schoolframe.setting.crew \
   {Select the crew member which skills will be trained}
bind $schoolframe.setting.crew <<ComboboxSelected>> SetSchoolSkills
grid [ttk::label $schoolframe.setting.skilllbl -text {in}] -row 0 -column 2 \
   -padx 5
grid [ttk::combobox $schoolframe.setting.skill -state readonly -width 27] -row 0 \
   -column 3
tooltip::tooltip $schoolframe.setting.skill \
   {Select the skill whichVy will be trained}
bind $schoolframe.setting.skill <<ComboboxSelected>> {
   $schoolframe.amountbox.amount set 1
   UpdateSchoolCost $schoolframe.amountbox.amount 1
   UpdateSchoolSelectedCost
}
grid [ttk::frame $schoolframe.amountbox] -sticky w
grid [ttk::radiobutton $schoolframe.amountbox.radioamount \
   -text {Selected amount of times} -variable traintype -value amount] \
   -columnspan 2 -sticky w -padx 5 -pady {5 0}
tooltip::tooltip $schoolframe.amountbox.radioamount \
   {Train the selected skill the selected amount of times}
grid [ttk::label $schoolframe.amountbox.amountlbl -text {Amount:}] -sticky w \
   -padx {50 0}
tooltip::tooltip $schoolframe.amountbox.amountlbl \
   {Enter amount of training sessions between 1 and 100}
grid [ttk::spinbox $schoolframe.amountbox.amount -from 1 -to 100 \
   -validate key \
   -validatecommand {ValidateSpinbox %W %P $schoolcanvas.school.setting.train;UpdateSchoolCost %W %P} \
   -width 5] -row 1 -column 1 -sticky w
tooltip::tooltip $schoolframe.amountbox.amount \
   {Enter amount of training sessions between 1 and 100}
$schoolframe.amountbox.amount set 1
bind $schoolframe.amountbox.amount <<Increment>> \
   {UpdateSchoolCost %W [expr [$schoolframe.amountbox.amount get] + 1]}
bind $schoolframe.amountbox.amount <<Decrement>> \
   {UpdateSchoolCost %W [expr [$schoolframe.amountbox.amount get] - 1]}
grid [ttk::label $schoolframe.amountbox.costlbl -text {Minimal cost:}] \
   -sticky w -padx {50 0}
tooltip::tooltip $schoolframe.amountbox.costlbl \
   {Minimal cost of training. The real cost can be higher that this.}
grid [ttk::label $schoolframe.amountbox.cost] -row 2 -column 1 -sticky w
tooltip::tooltip $schoolframe.amountbox.cost \
   {Minimal cost of training. The real cost can be higher that this.}
grid [ttk::frame $schoolframe.costbox] -sticky w
grid [ttk::radiobutton $schoolframe.costbox.radioamount \
   -text {Selected maximum cost of training} -variable traintype -value cost] \
   -columnspan 2 -sticky w -padx 5 -pady {5 0}
tooltip::tooltip $schoolframe.costbox.radioamount \
   "Train the selected skill as long as you don't spend the selected\namount of money"
grid [ttk::label $schoolframe.costbox.amountlbl -text {Cost:}] -sticky w \
   -padx {50 0}
tooltip::tooltip $schoolframe.costbox.amountlbl \
   {Enter amount of money which you want to spend}
grid [ttk::spinbox $schoolframe.costbox.amount -validate key \
   -validatecommand {ValidateSpinbox %W %P $schoolcanvas.school.setting.train} \
   -width 10] -row 1 -column 1 -sticky w
tooltip::tooltip $schoolframe.costbox.amount \
   {Enter amount of money which you want to spend}
$schoolframe.costbox.amount set 1
