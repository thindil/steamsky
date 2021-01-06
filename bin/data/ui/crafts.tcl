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

ttk::frame .gameframe.paned.craftframe
set craftcanvas [canvas .gameframe.paned.craftframe.canvas -yscrollcommand [list .gameframe.paned.craftframe.scrolly set] -xscrollcommand [list .gameframe.paned.craftframe.scrollx set]]
pack [ttk::scrollbar .gameframe.paned.craftframe.scrolly -orient vertical -command [list $craftcanvas yview]] -side right -fill y
pack $craftcanvas -side top -fill both
pack [ttk::scrollbar .gameframe.paned.craftframe.scrollx -orient horizontal -command [list $craftcanvas xview]] -fill x
::autoscroll::autoscroll .gameframe.paned.craftframe.scrolly
::autoscroll::autoscroll .gameframe.paned.craftframe.scrollx
set craftframe [ttk::frame $craftcanvas.craft]
# Recipes list
grid [ttk::frame $craftframe.list] -sticky nwes
set craftview [ttk::treeview $craftframe.list.view -yscrollcommand [list $craftframe.list.scrolly set]]
$craftview heading #0 -text {Name}
$craftview column #0 -width 300 -minwidth 300
$craftview tag configure gray -foreground gray -font OverstrikedFont
grid $craftview -sticky nwes
bind $craftview <<TreeviewSelect>> ShowRecipeInfo
grid [ttk::scrollbar $craftframe.list.scrolly -orient vertical -command [list $craftview yview]] -row 0 -column 1 -sticky ns
grid columnconfigure $craftframe 0 -weight 1
grid rowconfigure $craftframe 0 -weight 1
# Recipe info
set recipeframe [ttk::frame $craftframe.item]
grid $recipeframe -row 0 -column 1
grid [ttk::labelframe $recipeframe.info -text {Recipe Info:}]
grid [text $recipeframe.info.text -wrap char -height 10 -width 40]
$recipeframe.info.text tag configure red -foreground red
update
# Recipe actions
grid [ttk::frame $recipeframe.set] -sticky w
grid [ttk::button $recipeframe.set.button -text Craft -command SetCrafting]
grid [ttk::label $recipeframe.set.maxamount] -column 1 -row 0
grid [ttk::spinbox $recipeframe.set.amount -from 1 -increment 1 -validate key -validatecommand {ValidateSpinbox %W %P} -width 5] -column 2 -row 0
grid [ttk::label $recipeframe.set.label -text {in workshop:}]
grid [ttk::combobox $recipeframe.set.workshop -state readonly] -column 1 -row 1 -columnspan 2
grid [ttk::label $recipeframe.error -style Headerred.TLabel -wraplength [winfo reqwidth $recipeframe]] -sticky w
