proc InvokeButton {name} {
   if {[winfo ismapped $name] == "1"} {
      focus $name
      $name invoke
   }
}

# Main Menu
ttk::frame .mainmenu
pack [ttk::label .mainmenu.logo -text {Steam Sky} -font {Rye 70}]
pack [ttk::label .mainmenu.version -text {Version 5.1 development}]
pack [ttk::button .mainmenu.newgame -text {New game} -underline 0]
ttk::button .mainmenu.loadgame -text {Load game} -underline 0
ttk::button .mainmenu.halloffame -text {Hall of Fame} -underline 0 -command {
   bind . <Alt-b> {InvokeButton .hofmenu.back}
   bind . <Escape> {InvokeButton .hofmenu.back}
   pack forget .mainmenu
   pack .hofmenu -fill both -expand true
   ShowHallOfFame
}
pack [ttk::button .mainmenu.news -text {News} -underline 1 -command {
   bind . <Alt-s> {InvokeButton .newsmenu.showall}
   bind . <Alt-b> {InvokeButton .newsmenu.back}
   bind . <Escape> {InvokeButton .newsmenu.back}
   pack forget .mainmenu
   pack .newsmenu -fill both -expand true
   ShowNews false
}]
pack [ttk::button .mainmenu.about -text {About} -underline 0 -command {
   bind . <Alt-s> {InvokeButton .aboutmenu.showlicense}
   bind . <Alt-b> {InvokeButton .aboutmenu.back}
   bind . <Escape> {InvokeButton .aboutmenu.back}
   pack forget .mainmenu
   pack .aboutmenu -fill both -expand true
}]
pack [ttk::button .mainmenu.quit -text {Quit} -command exit -underline 0]
bind . <Alt-n> {InvokeButton .mainmenu.newgame}
bind . <Alt-l> {InvokeButton .mainmenu.loadgame}
bind . <Alt-h> {InvokeButton .mainmenu.halloffame}
bind . <Alt-e> {InvokeButton .mainmenu.news}
bind . <Alt-a> {InvokeButton .mainmenu.about}
bind . <Alt-q> {InvokeButton .mainmenu.quit}

# About menu
ttk::frame .aboutmenu
grid [ttk::label .aboutmenu.about -text {Roguelike in the sky with a steampunk theme}] -columnspan 3
grid [ttk::button .aboutmenu.website -text {Website} -style Toolbutton -command {OpenLink https://thindil.itch.io/steam-sky}] -row 1 -columnspan 3
grid [ttk::button .aboutmenu.mail -text {(c)2016-2020 Bartek thindil Jasicki} -style Toolbutton -command {OpenLink mailto:thindil@laeran.pl}] -row 2 -columnspan 3
grid [ttk::button .aboutmenu.getinvolved -text {Get involved} -command {
   pack forget .aboutmenu
   pack .showfilemenu -fill both -expand true
   ShowFile CONTRIBUTING.md
}] -row 3 -sticky e
grid [ttk::button .aboutmenu.modify -text {Modify game} -command {
   pack forget .aboutmenu
   pack .showfilemenu -fill both -expand true
   ShowFile MODDING.md
}] -row 3 -column 1
grid [ttk::button .aboutmenu.readme -text {README} -command {
   pack forget .aboutmenu
   pack .showfilemenu -fill both -expand true
   ShowFile README.md
}] -row 3 -column 2 -sticky w
grid [ttk::label .aboutmenu.license -text {Steam Sky is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.} -wraplength 590] -row 4 -columnspan 3
grid [ttk::label .aboutmenu.license2 -text {Steam Sky is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.} -wraplength 590] -row 5 -columnspan 3
grid [ttk::button .aboutmenu.showlicense -text {Show full license} -underline 0 -command {
   pack forget .aboutmenu
   pack .showfilemenu -fill both -expand true
   ShowFile COPYING
}] -row 6 -column 1 -sticky e
grid [ttk::button .aboutmenu.back -text {Back to main menu} -underline 0 -command {
   bind . <Alt-s> {}
   bind . <Alt-b> {}
   bind . <Escape> {}
   pack forget .aboutmenu
   pack .mainmenu -fill both -expand true
}] -row 6 -column 2 -sticky e

# Show file content
ttk::frame .showfilemenu
pack [ttk::button .showfilemenu.back -text {Back} -underline 0 -command {
   bind . <Alt-b> {}
   bind . <Escape> {}
   pack forget .showfilemenu
   pack .mainmenu -fill both -expand true
}] -side bottom -anchor e
pack [ttk::scrollbar .showfilemenu.scroll -orient vertical -command [list .showfilemenu.text yview]] -side right -fill y
pack [text .showfilemenu.text -wrap char -yscrollcommand {.showfilemenu.scroll set}] -side top -fill both -expand true

# News menu
ttk::frame .newsmenu
grid [text .newsmenu.text -wrap word -yscrollcommand {.newsmenu.scroll set}] -sticky nesw -columnspan 2
grid [ttk::scrollbar .newsmenu.scroll -orient vertical -command [list .newsmenu.text yview]] -column 2 -row 0 -sticky ns
grid [ttk::button .newsmenu.showall -text {Show all changes} -underline 0] -row 1 -column 0 -sticky e
grid [ttk::button .newsmenu.back -text {Back to menu} -underline 0 -command {
   bind . <Alt-s> {}
   bind . <Alt-b> {}
   bind . <Escape> {}
   pack forget .newsmenu
   pack .mainmenu -fill both -expand true
}] -row 1 -column 1 -sticky e
grid columnconfigure .newsmenu 0 -weight 1
grid rowconfigure .newsmenu 0 -weight 1

# Hall of Fame menu
ttk::frame .hofmenu
grid [ttk::treeview .hofmenu.view -yscrollcommand {.hofmenu.yscroll set} -xscrollcommand {.hofmenu.xscroll set} -show headings -columns [list position name points diedfrom] -selectmode none] -sticky nesw
.hofmenu.view heading position -text {Position}
.hofmenu.view column position -width 100
.hofmenu.view heading name -text {Name}
.hofmenu.view column name -width 150
.hofmenu.view heading points -text {Points}
.hofmenu.view column points -width 100
.hofmenu.view heading diedfrom -text {Died from}
grid [ttk::scrollbar .hofmenu.yscroll -orient vertical -command [list .hofmenu.view yview]] -column 1 -row 0 -sticky ns
grid [ttk::scrollbar .hofmenu.xscroll -orient horizontal -command [list .hofmenu.view xview]] -column 0 -row 1 -columnspan 2 -sticky we
grid [ttk::button .hofmenu.back -text {Back to menu} -command {
   bind . <Alt-b> {}
   bind . <Escape> {}
   pack forget .hofmenu
   pack .mainmenu -fill both -expand true
}] -row 2 -column 0 -columnspan 2 -sticky e
grid columnconfigure .hofmenu 0 -weight 1
grid rowconfigure .hofmenu 0 -weight 1
