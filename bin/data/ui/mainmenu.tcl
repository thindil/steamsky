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
pack [ttk::button .mainmenu.newgame -text {New game} -underline 0 -command {
   set newtab difficulty
   bind . <Alt-s> {InvokeButton .newgamemenu.buttonsbox2.start}
   bind . <Alt-b> {InvokeButton .newgamemenu.buttonsbox2.back}
   bind . <Alt-p> {InvokeButton .newgamemenu.buttonsbox.player}
   bind . <Alt-d> {InvokeButton .newgamemenu.buttonsbox.difficulty}
   bind . <Escape> {InvokeButton .newgamemenu.buttonsbox2.back}
   pack forget .mainmenu
   pack .newgamemenu -fill both -expand true
   .newgamemenu.buttonsbox.player invoke
}]
ttk::button .mainmenu.loadgame -text {Load game} -underline 0 -command {
   bind . <Alt-d> {InvokeButton .loadmenu.delete}
   bind . <Alt-l> {InvokeButton .loadmenu.load}
   bind . <Alt-b> {InvokeButton .loadmenu.back}
   bind . <Escape> {InvokeButton .loadmenu.back}
   pack forget .mainmenu
   pack .loadmenu -fill both -expand true
   focus .loadmenu.load
   ShowLoadGame
}
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

# Load game menu
ttk::frame .loadmenu
grid [ttk::treeview .loadmenu.view -yscrollcommand {.loadmenu.yscroll set} -xscrollcommand {.loadmenu.xscroll set} -show headings -columns [list playername shipname lastsaved]] -sticky nesw -columnspan 3
.loadmenu.view heading playername -text {Player name}
.loadmenu.view column playername -width 150
.loadmenu.view heading shipname -text {Ship name}
.loadmenu.view column shipname -width 150
.loadmenu.view heading lastsaved -text {Last saved}
bind .loadmenu.view <<TreeviewSelect>> {InvokeButton .loadmenu.load}
grid [ttk::scrollbar .loadmenu.yscroll -orient vertical -command [list .loadmenu.view yview]] -column 3 -row 0 -sticky ns
grid [ttk::scrollbar .loadmenu.xscroll -orient horizontal -command [list .loadmenu.view xview]] -column 0 -row 1 -columnspan 3 -sticky we
grid [ttk::button .loadmenu.delete -text {Delete game} -command DeleteGame -underline 0] -row 2 -column 0 -sticky e
grid [ttk::button .loadmenu.load -text {Load game} -underline 0 -command LoadGame] -row 2 -column 1 -sticky e
grid [ttk::button .loadmenu.back -text {Back to main menu} -underline 0 -command {
   bind . <Alt-b> {}
   bind . <Alt-l> {}
   bind . <Alt-d> {}
   bind . <Escape> {}
   pack forget .loadmenu
   pack .mainmenu -fill both -expand true
}] -row 2 -column 2 -sticky e
grid columnconfigure .loadmenu 0 -weight 1
grid rowconfigure .loadmenu 0 -weight 1

# New game setting menu
ttk::frame .newgamemenu
grid [ttk::frame .newgamemenu.buttonsbox] -columnspan 2
grid [ttk::radiobutton .newgamemenu.buttonsbox.player -text Player -state selected -style Toolbutton -value player -variable newtab -underline 0 -command {
   grid forget .newgamemenu.difficultysetting
   grid .newgamemenu.playersetting -sticky nwes -row 1
}] -sticky e
grid [ttk::radiobutton .newgamemenu.buttonsbox.difficulty -text Difficulty -style Toolbutton -value difficulty -variable newtab -underline 0 -command {
   grid forget .newgamemenu.playersetting
   grid .newgamemenu.difficultysetting -sticky nwes -row 1
}] -column 1 -row 0 -sticky w
ttk::frame .newgamemenu.playersetting
grid [ttk::label .newgamemenu.playersetting.labelplayername -text {Character name:}]
grid [ttk::entry .newgamemenu.playersetting.playername -width 15] -row 0 -column 1
grid [ttk::label .newgamemenu.playersetting.labelgender -text {Character gender:}] -row 1
grid [ttk::combobox .newgamemenu.playersetting.gender -state readonly -values [list Male Female] -width 14] -row 1 -column 1
.newgamemenu.playersetting.gender set Male
grid [ttk::label .newgamemenu.playersetting.labelshipname -text {Ship name:}] -row 2
grid [ttk::entry .newgamemenu.playersetting.shipname -width 15] -row 2 -column 1
grid [ttk::label .newgamemenu.playersetting.labelgoal -text {Character goal:}] -row 3
grid [ttk::button .newgamemenu.playersetting.goal -text {Random}] -row 3 -column 1
grid [ttk::label .newgamemenu.playersetting.labelfaction -text {Character faction:}] -row 4
grid [ttk::combobox .newgamemenu.playersetting.faction -state readonly -width 14] -row 4 -column 1
grid [ttk::label .newgamemenu.playersetting.labelcareer -text {Character career:}] -row 5
grid [ttk::combobox .newgamemenu.playersetting.career -state readonly -width 14] -row 5 -column 1
grid [ttk::label .newgamemenu.playersetting.labelbase -text {Starting base type:}] -row 6
grid [ttk::combobox .newgamemenu.playersetting.base -state readonly -width 14] -row 6 -column 1
ttk::frame .newgamemenu.difficultysetting
grid [ttk::label .newgamemenu.difficultysetting.difficultylabel -text {Difficulty level:}]
grid [ttk::combobox .newgamemenu.difficultysetting.difficultylevel -state readonly -values [list {Very Easy} Easy Normal Hard {Very Hard}] -width 7] -column 1 -row 0
.newgamemenu.difficultysetting.difficultylevel set Normal
grid [ttk::label .newgamemenu.difficultysetting.enemydamagelabel -text {Enemy ship damage:}] -row 1
grid [ttk::spinbox .newgamemenu.difficultysetting.enemydamage -from 1 -to 500 -increment 1.0 -width 5] -column 1 -row 1
.newgamemenu.difficultysetting.enemydamage set 100
grid [ttk::label .newgamemenu.difficultysetting.playerdamagelabel -text {Player ship damage:}] -row 2
grid [ttk::spinbox .newgamemenu.difficultysetting.playerdamage -from 1 -to 500 -increment 1.0 -width 5] -column 1 -row 2
.newgamemenu.difficultysetting.playerdamage set 100
grid [ttk::label .newgamemenu.difficultysetting.enemymeleedamagelabel -text {Enemy damage in melee combat:} -wraplength 150] -row 3
grid [ttk::spinbox .newgamemenu.difficultysetting.enemymeleedamage -from 1 -to 500 -increment 1.0 -width 5] -column 1 -row 3
.newgamemenu.difficultysetting.enemymeleedamage set 100
grid [ttk::label .newgamemenu.difficultysetting.playermeleedamagelabel -text {Player crew damage in melee combat:} -wraplength 150] -row 4
grid [ttk::spinbox .newgamemenu.difficultysetting.playermeleedamage -from 1 -to 500 -increment 1.0 -width 5] -column 1 -row 4
.newgamemenu.difficultysetting.playermeleedamage set 100
grid [ttk::label .newgamemenu.difficultysetting.experiencelabel -text {Experience gained:}] -row 5
grid [ttk::spinbox .newgamemenu.difficultysetting.experience -from 1 -to 500 -increment 1.0 -width 5] -column 1 -row 5
.newgamemenu.difficultysetting.experience set 100
grid [ttk::label .newgamemenu.difficultysetting.reputationlabel -text {Reputation gained:}] -row 6
grid [ttk::spinbox .newgamemenu.difficultysetting.reputation -from 1 -to 500 -increment 1.0 -width 5] -column 1 -row 6
.newgamemenu.difficultysetting.reputation set 100
grid [ttk::label .newgamemenu.difficultysetting.upgradelabel -text {Upgrade cost:}] -row 7
grid [ttk::spinbox .newgamemenu.difficultysetting.upgrade -from 1 -to 500 -increment 1.0 -width 5] -column 1 -row 7
.newgamemenu.difficultysetting.upgrade set 100
grid [ttk::button .newgamemenu.difficultysetting.random -text Random] -row 8 -columnspan 2 -sticky we
grid [ttk::label .newgamemenu.difficultysetting.randomizelabel -text {Randomize difficulty on game start} -wraplength 150] -row 9
grid [ttk::checkbutton .newgamemenu.difficultysetting.randomize] -row 9 -column 1
grid [ttk::label .newgamemenu.difficultysetting.totalpoints -text {Total gained points: 100%}] -row 10 -columnspan 2
grid [ttk::labelframe .newgamemenu.info -text Info] -row 1 -column 1 -sticky nwes
grid [ttk::label .newgamemenu.info.text -wraplength 200] -sticky nwes
.newgamemenu.info.text configure -text {General player character settings. Select field which you want to set to see more information about.}
grid [ttk::frame .newgamemenu.buttonsbox2] -row 2 -columnspan 2
grid [ttk::button .newgamemenu.buttonsbox2.start -text {Start game} -underline 0] -sticky e
grid [ttk::button .newgamemenu.buttonsbox2.back -text {Back to menu} -underline 0 -command {
   bind . <Alt-s> {}
   bind . <Alt-b> {}
   bind . <Alt-p> {}
   bind . <Alt-d> {}
   bind . <Escape> {}
   pack forget .newgamemenu
   pack .mainmenu -fill both -expand true
}] -column 1 -row 0 -sticky w
grid columnconfigure .newgamemenu .newgamemenu.info -weight 3
grid rowconfigure .newgamemenu .newgamemenu.info -weight 3
