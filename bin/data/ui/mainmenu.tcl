package require tooltip

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
set windowid {}
set playertooltips [list "General player character settings. Select field which you want to set to see more information about." "Enter character name or press Enter key for random name." "Select the gender of your character." "Enter ship name or press Enter for random ship name." "Select starting goal for your character.\nYou can change it later in game." "Select your faction from a list. Factions have the biggest impact on game.\nThey determine the amount of bases and some playing styles.\nMore information about each faction can be found after selecting it.\nYou can't change this later." "Select your career from a list. Careers have some impact on gameplay\n(each have bonuses to gaining experience in some fields plus\nthey determine your starting ship and crew). More info about each\ncareer can be found after selecting it. You can't change career later." "Select type of base in which you will start the game.\nThis may have some impact on game difficulty."]
set difficultytooltips [list "Set difficulty of new game. Each value can be between 1 and 500. Each change has an impact not only on the game's difficulty but also on amount of points gained in the game. Select a field to get more information about it." "Select game difficulty preset level." "Percentage of damage done by enemy ships in combat.\nLowering it makes the  game easier but lowers the\namount of score gained as well." "Percentage of damage done by the player's ship in combat.\nRaising it makes the game easier but lowers the amount\nof score gained as well." "Percentage of damage done by enemies in melee combat.\nLowering it makes the game easier but lowers the\namount of score gained as well." "Percentage of damage done by player's crew (and player character)\nin melee combat. Raising it makes the game easier but lowers the\namount of score gained as well." "Percentage of experience gained by player and their crew from actions.\nRaising it makes the game easier but lowers the amount of score gained as well." "Percentage of reputation in bases gained or lost by player in sky bases\ndue to player actions. Raising it makes the game easier but lowers the\namount of score gained as well." "Percentage of the standard material cost and time needed\nfor upgrading ship modules. Lowering it makes the game\neasier but lowers the amount of score gained as well." "Percentage of the standard prices for services in bases (docking, repairing ship,\nrecruiting new crew members, etc). Lowering it makes the game easier but lowers\nthe amount of score gained as well." "Select random values for all settings." "If you select this option, all difficulty settings will be\nrandomized during start new game. Not recommended for new players."]
grid [ttk::frame .newgamemenu.buttonsbox] -columnspan 3
grid [ttk::radiobutton .newgamemenu.buttonsbox.player -text Player -state selected -style Toolbutton -value player -variable newtab -underline 0 -command {
   .newgamemenu.info.text configure -state normal
   .newgamemenu.info.text delete 1.0 end
   .newgamemenu.info.text insert end [lindex $playertooltips 0]
   .newgamemenu.info.text configure -state disabled
   .newgamemenu.canvas delete $windowid
   set windowid [.newgamemenu.canvas create window [expr [winfo reqwidth .newgamemenu.canvas.player] / 2] [expr [winfo reqheight .newgamemenu.canvas.player] / 2] -window .newgamemenu.canvas.player]
   .newgamemenu.canvas configure -width [winfo reqwidth .newgamemenu.canvas.player] -height [winfo reqheight .newgamemenu.canvas.player] -scrollregion [.newgamemenu.canvas bbox all]
}] -sticky e
grid [ttk::radiobutton .newgamemenu.buttonsbox.difficulty -text Difficulty -style Toolbutton -value difficulty -variable newtab -underline 0 -command {
   .newgamemenu.info.text configure -state normal
   .newgamemenu.info.text delete 1.0 end
   .newgamemenu.info.text insert end [lindex $difficultytooltips 0]
   .newgamemenu.info.text configure -state disabled
   .newgamemenu.canvas delete $windowid
   set windowid [.newgamemenu.canvas create window [expr [winfo reqwidth .newgamemenu.canvas.difficulty] / 2] [expr [winfo reqheight .newgamemenu.canvas.difficulty] / 2] -window .newgamemenu.canvas.difficulty]
   .newgamemenu.canvas configure -width [winfo reqwidth .newgamemenu.canvas.difficulty] -height [winfo reqheight .newgamemenu.canvas.difficulty] -scrollregion [.newgamemenu.canvas bbox all]
}] -column 1 -row 0 -sticky w
grid [canvas .newgamemenu.canvas -yscrollcommand [list .newgamemenu.scrollbar set]] -sticky nwes -row 1
grid [ttk::scrollbar .newgamemenu.scrollbar -orient vertical -command [list .newgamemenu.canvas yview]] -sticky ns -row 1 -column 1
ttk::frame .newgamemenu.canvas.player
grid [ttk::label .newgamemenu.canvas.player.labelplayername -text {Character name:}]
grid [ttk::entry .newgamemenu.canvas.player.playername -width 15] -row 0 -column 1
tooltip::tooltip .newgamemenu.canvas.player.playername [lindex $playertooltips 1]
grid [ttk::label .newgamemenu.canvas.player.labelgender -text {Character gender:}] -row 1
grid [ttk::combobox .newgamemenu.canvas.player.gender -state readonly -values [list Male Female] -width 14] -row 1 -column 1
tooltip::tooltip .newgamemenu.canvas.player.gender [lindex $playertooltips 2]
grid [ttk::label .newgamemenu.canvas.player.labelshipname -text {Ship name:}] -row 2
grid [ttk::entry .newgamemenu.canvas.player.shipname -width 15] -row 2 -column 1
tooltip::tooltip .newgamemenu.canvas.player.shipname [lindex $playertooltips 3]
grid [ttk::label .newgamemenu.canvas.player.labelgoal -text {Character goal:}] -row 3
grid [ttk::button .newgamemenu.canvas.player.goal -text {Random}] -row 3 -column 1
tooltip::tooltip .newgamemenu.canvas.player.goal [lindex $playertooltips 4]
grid [ttk::label .newgamemenu.canvas.player.labelfaction -text {Character faction:}] -row 4
grid [ttk::combobox .newgamemenu.canvas.player.faction -state readonly -width 14] -row 4 -column 1
tooltip::tooltip .newgamemenu.canvas.player.faction [lindex $playertooltips 5]
grid [ttk::label .newgamemenu.canvas.player.labelcareer -text {Character career:}] -row 5
grid [ttk::combobox .newgamemenu.canvas.player.career -state readonly -width 14] -row 5 -column 1
tooltip::tooltip .newgamemenu.canvas.player.career [lindex $playertooltips 6]
grid [ttk::label .newgamemenu.canvas.player.labelbase -text {Starting base type:}] -row 6
grid [ttk::combobox .newgamemenu.canvas.player.base -state readonly -width 14] -row 6 -column 1
tooltip::tooltip .newgamemenu.canvas.player.base [lindex $playertooltips 7]
ttk::frame .newgamemenu.canvas.difficulty
grid [ttk::label .newgamemenu.canvas.difficulty.difficultylabel -text {Difficulty level:}]
grid [ttk::combobox .newgamemenu.canvas.difficulty.difficultylevel -state readonly -values [list {Very Easy} Easy Normal Hard {Very Hard}] -width 7] -column 1 -row 0
.newgamemenu.canvas.difficulty.difficultylevel set Normal
tooltip::tooltip .newgamemenu.canvas.difficulty.difficultylevel [lindex $difficultytooltips 1]
grid [ttk::label .newgamemenu.canvas.difficulty.enemydamagelabel -text {Enemy ship damage:}] -row 1
grid [ttk::spinbox .newgamemenu.canvas.difficulty.enemydamage -from 1 -to 500 -increment 1.0 -width 5] -column 1 -row 1
.newgamemenu.canvas.difficulty.enemydamage set 100
tooltip::tooltip .newgamemenu.canvas.difficulty.enemydamage [lindex $difficultytooltips 2]
grid [ttk::label .newgamemenu.canvas.difficulty.playerdamagelabel -text {Player ship damage:}] -row 2
grid [ttk::spinbox .newgamemenu.canvas.difficulty.playerdamage -from 1 -to 500 -increment 1.0 -width 5] -column 1 -row 2
.newgamemenu.canvas.difficulty.playerdamage set 100
tooltip::tooltip .newgamemenu.canvas.difficulty.playerdamage [lindex $difficultytooltips 3]
grid [ttk::label .newgamemenu.canvas.difficulty.enemymeleedamagelabel -text {Enemy damage in melee combat:} -wraplength 150] -row 3
grid [ttk::spinbox .newgamemenu.canvas.difficulty.enemymeleedamage -from 1 -to 500 -increment 1.0 -width 5] -column 1 -row 3
.newgamemenu.canvas.difficulty.enemymeleedamage set 100
tooltip::tooltip .newgamemenu.canvas.difficulty.enemymeleedamage [lindex $difficultytooltips 4]
grid [ttk::label .newgamemenu.canvas.difficulty.playermeleedamagelabel -text {Player crew damage in melee combat:} -wraplength 150] -row 4
grid [ttk::spinbox .newgamemenu.canvas.difficulty.playermeleedamage -from 1 -to 500 -increment 1.0 -width 5] -column 1 -row 4
.newgamemenu.canvas.difficulty.playermeleedamage set 100
tooltip::tooltip .newgamemenu.canvas.difficulty.playermeleedamage [lindex $difficultytooltips 5]
grid [ttk::label .newgamemenu.canvas.difficulty.experiencelabel -text {Experience gained:}] -row 5
grid [ttk::spinbox .newgamemenu.canvas.difficulty.experience -from 1 -to 500 -increment 1.0 -width 5] -column 1 -row 5
.newgamemenu.canvas.difficulty.experience set 100
tooltip::tooltip .newgamemenu.canvas.difficulty.experience [lindex $difficultytooltips 6]
grid [ttk::label .newgamemenu.canvas.difficulty.reputationlabel -text {Reputation gained:}] -row 6
grid [ttk::spinbox .newgamemenu.canvas.difficulty.reputation -from 1 -to 500 -increment 1.0 -width 5] -column 1 -row 6
.newgamemenu.canvas.difficulty.reputation set 100
tooltip::tooltip .newgamemenu.canvas.difficulty.reputation [lindex $difficultytooltips 7]
grid [ttk::label .newgamemenu.canvas.difficulty.upgradelabel -text {Upgrade cost:}] -row 7
grid [ttk::spinbox .newgamemenu.canvas.difficulty.upgrade -from 1 -to 500 -increment 1.0 -width 5] -column 1 -row 7
.newgamemenu.canvas.difficulty.upgrade set 100
tooltip::tooltip .newgamemenu.canvas.difficulty.upgrade [lindex $difficultytooltips 8]
grid [ttk::label .newgamemenu.canvas.difficulty.priceslabel -text {Prices in bases:}] -row 8
grid [ttk::spinbox .newgamemenu.canvas.difficulty.prices -from 1 -to 500 -increment 1.0 -width 5] -column 1 -row 8
.newgamemenu.canvas.difficulty.prices set 100
tooltip::tooltip .newgamemenu.canvas.difficulty.prices [lindex $difficultytooltips 9]
grid [ttk::button .newgamemenu.canvas.difficulty.random -text Random] -row 9 -columnspan 2 -sticky we
tooltip::tooltip .newgamemenu.canvas.difficulty.random [lindex $difficultytooltips 10]
grid [ttk::label .newgamemenu.canvas.difficulty.randomizelabel -text {Randomize difficulty on game start} -wraplength 150] -row 10
tooltip::tooltip .newgamemenu.canvas.difficulty.randomizelabel [lindex $difficultytooltips 11]
grid [ttk::checkbutton .newgamemenu.canvas.difficulty.randomize] -row 10 -column 1
tooltip::tooltip .newgamemenu.canvas.difficulty.randomize [lindex $difficultytooltips 11]
grid [ttk::label .newgamemenu.canvas.difficulty.totalpoints -text {Total gained points: 100%}] -row 11 -columnspan 2
grid [ttk::labelframe .newgamemenu.info -text Info] -row 1 -column 2 -sticky nwes
pack [ttk::scrollbar .newgamemenu.info.scroll -orient vertical -command [list .newgamemenu.info.text yview]] -side right -fill y
pack [text .newgamemenu.info.text -wrap word -yscrollcommand [list .newgamemenu.info.scroll set]] -expand true -fill both -side top
grid [ttk::frame .newgamemenu.buttonsbox2] -row 2 -columnspan 3
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
