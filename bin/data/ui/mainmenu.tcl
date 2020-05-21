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
ttk::button .mainmenu.halloffame -text {Hall of Fame} -underline 0
pack [ttk::button .mainmenu.news -text {News} -underline 1]
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
grid [ttk::button .aboutmenu.getinvolved -text {Get involved}] -row 3 -sticky e
grid [ttk::button .aboutmenu.modify -text {Modify game}] -row 3 -column 1
grid [ttk::button .aboutmenu.readme -text {README}] -row 3 -column 2 -sticky w
grid [ttk::label .aboutmenu.license -text {Steam Sky is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.} -wraplength 590] -row 4 -columnspan 3
grid [ttk::label .aboutmenu.license2 -text {Steam Sky is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.} -wraplength 590] -row 5 -columnspan 3
grid [ttk::button .aboutmenu.showlicense -text {Show full license} -underline 0] -row 6 -column 1 -sticky e
grid [ttk::button .aboutmenu.back -text {Back to main menu} -underline 0 -command {
   bind . <Alt-s> {}
   bind . <Alt-b> {}
   bind . <Escape> {}
   pack forget .aboutmenu
   pack .mainmenu -fill both -expand true
}] -row 6 -column 2 -sticky e
