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

toplevel .help -class Dialog
wm title .help {Steam Sky - Help}
wm transient .help .
if {$tcl_platform(os) == "Linux"} {
   wm attributes .help -type dialog
}
grid [ttk::panedwindow .help.paned] -sticky nwes
grid columnconfigure .help .help.paned -weight 1
grid rowconfigure .help .help.paned -weight 1
.help.paned add [ttk::frame .help.paned.topics]
pack [ttk::scrollbar .help.paned.topics.scroll -orient vertical -command [list .help.paned.topics.view yview]] -side right -fill y
pack [ttk::treeview .help.paned.topics.view -show tree -yscrollcommand [list .help.paned.topics.scroll set] -style Help.Treeview] -side top -fill both
tooltip::tooltip .help.paned.topics.view {Click on the help topic to see it content}
.help.paned add [ttk::frame .help.paned.content]
pack [ttk::scrollbar .help.paned.content.scroll -orient vertical -command [list .help.paned.content.view yview]] -side right -fill y
set helpview [text .help.paned.content.view -wrap word -yscrollcommand [list .help.paned.content.scroll set] -font HelpFont -width 70]
$helpview tag configure special -foreground yellow -font BoldHelpFont
$helpview tag configure bold -font BoldHelpFont
$helpview tag configure underline -font UnderlineHelpFont
$helpview tag configure italic -font ItalicHelpFont
pack $helpview -side top -fill both -padx {10 0}
bind .help <Escape> {CloseHelp}
bind .help.paned.content <Configure> {
   $helpview configure -height [expr [winfo height .help.paned.content] / [font metrics HelpFont -linespace]]
}
