# Copyright 2024-2026 Bartek thindil Jasicki
#
# This file is part of Steam Sky.
#
# Steam Sky is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Steam Sky is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

## Provides code related to showing the main game's menu, like creating it.

import std/[os, tables]
import contracts
import ../[basestypes, careers, config, game, game2, tk]
import dialogs2, errordialog, goalsui, mainmenucommands, showmainmenu,
    table, themes, utilsui, utilsui2

proc createMainMenu*() {.raises: [], tags: [ReadDirEffect,
    WriteIOEffect, TimeEffect, RootEffect], contractual.} =
  ## Create the main menu UI
  let
    uiDirectory: string = dataDirectory.string & "ui" & DirSep
    iconPath: string = uiDirectory & "images" & DirSep & "icon.png"
  const mainWindow: string = "."
  if not fileExists(filename = iconPath):
    tclEval(script = "wm withdraw " & mainWindow)
    tclEval(script = "tk_messageBox -message {Couldn't not find the game data files and the game have to stop. Are you sure that directory \"" &
        dataDirectory.string & "\" is the proper place where the game data files exists?} -icon error -type ok")
    tclEval(script = "exit 1")
    return
  mainmenucommands.addCommands()
  dialogs2.addCommands()
  utilsui.addCommands()
  goalsui.addCommands()
  table.addCommands()
  let icon: string = tclEval2(script = "image create photo logo -file {" &
      iconPath & "}")
  tclEval(script = "wm iconphoto . -default " & icon)
  try:
    tclEvalFile(fileName = themesList[gameSettings.interfaceTheme].fileName)
  except:
    showError(message = "Can't eval interface theme file.")
    return
  tclEval(script = "ttk::style theme use " & gameSettings.interfaceTheme)
  loadThemeImages()
  tclEval(script = """
      proc InvokeButton {name} {
         set focused [focus]
         if {$focused != {} && [winfo class $focused] == "TEntry"} {
            return
         } elseif {[winfo ismapped $name] == "1"} {
            focus $name
            $name invoke
         }
      }

      # Main Menu
      ttk::frame .mainmenu -style Main.TFrame
      pack [ttk::label .mainmenu.logo -image logo] -pady {15 0}
      pack [ttk::label .mainmenu.version]
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
      tooltip::tooltip .mainmenu.newgame {Set and start a new game}
      ttk::button .mainmenu.loadgame -text {Load game} -underline 0 -command {
         bind . <Alt-b> {InvokeButton .loadmenu.back}
         bind . <Escape> {InvokeButton .loadmenu.back}
         pack forget .mainmenu
         pack .loadmenu -fill both -expand true
         ShowLoadGame
      }
      tooltip::tooltip .mainmenu.loadgame {Load one of the previously saved games}
      ttk::button .mainmenu.halloffame -text {Hall of Fame} -underline 0 -command {
         bind . <Alt-b> {InvokeButton .hofmenu.back}
         bind . <Escape> {InvokeButton .hofmenu.back}
         pack forget .mainmenu
         pack .hofmenu -fill both -expand true
         ShowHallOfFame
      }
      tooltip::tooltip .mainmenu.halloffame \
         {Show your previous the bests scores in the game}
      pack [ttk::button .mainmenu.news -text {News} -underline 1 -command {
         bind . <Alt-s> {InvokeButton .newsmenu.showall}
         bind . <Alt-b> {InvokeButton .newsmenu.back}
         bind . <Escape> {InvokeButton .newsmenu.back}
         pack forget .mainmenu
         pack .newsmenu -fill both -expand true
         ShowNews false
      }]
      tooltip::tooltip .mainmenu.news {The list of changes to the game}
      pack [ttk::button .mainmenu.about -text {About} -underline 0 -command {
         bind . <Alt-s> {InvokeButton .aboutmenu.showlicense}
         bind . <Alt-b> {InvokeButton .aboutmenu.back}
         bind . <Escape> {InvokeButton .aboutmenu.back}
         pack forget .mainmenu
         pack .aboutmenu -fill both -expand true
      }]
      tooltip::tooltip .mainmenu.about {General information about the game}
      pack [ttk::button .mainmenu.quit -text {Quit} -command exit -underline 0]
      tooltip::tooltip .mainmenu.quit {Quit from the game}
      bind . <Alt-n> {InvokeButton .mainmenu.newgame}
      bind . <Alt-l> {InvokeButton .mainmenu.loadgame}
      bind . <Alt-h> {InvokeButton .mainmenu.halloffame}
      bind . <Alt-e> {InvokeButton .mainmenu.news}
      bind . <Alt-a> {InvokeButton .mainmenu.about}
      bind . <Alt-q> {InvokeButton .mainmenu.quit}

      # About menu
      ttk::frame .aboutmenu -style Main.TFrame
      grid [ttk::label .aboutmenu.about \
         -text {Roguelike in the sky with a steampunk theme}] -columnspan 3 -pady 2
      grid [ttk::button .aboutmenu.website -text {Website} -style Link.Toolbutton \
         -command {OpenLink https://thindil.itch.io/steam-sky}] -row 1 -columnspan 3
      tooltip::tooltip .aboutmenu.website \
         {Visit the game website: https://thindil.itch.io/steam-sky}
      grid [ttk::button .aboutmenu.mail -text {(c)2016-2026 Bartek thindil Jasicki} \
         -style Link.Toolbutton -command {OpenLink mailto:thindil@laeran.pl.eu.org}] \
         -row 2 -columnspan 3
      tooltip::tooltip .aboutmenu.mail {Send a mail to the game creator}
      grid [ttk::button .aboutmenu.getinvolved -text {Get involved} -command {
         pack forget .aboutmenu
         pack .showfilemenu -fill both -expand true
         ShowFile CONTRIBUTING.md
      }] -row 3 -sticky e
      tooltip::tooltip .aboutmenu.getinvolved \
         {Guide how to help with creating the game, report bugs, etc}
      grid [ttk::button .aboutmenu.modify -text {Modify game} -command {
         pack forget .aboutmenu
         pack .showfilemenu -fill both -expand true
         ShowFile MODDING.md
      }] -row 3 -column 1
      tooltip::tooltip .aboutmenu.modify {Guide how to modify the game}
      grid [ttk::button .aboutmenu.readme -text {README} -command {
         pack forget .aboutmenu
         pack .showfilemenu -fill both -expand true
         ShowFile README.md
      }] -row 3 -column 2 -sticky w
      tooltip::tooltip .aboutmenu.readme {Some technical information about the game}
      grid [ttk::label .aboutmenu.license \
         -text {Steam Sky is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.} \
         -wraplength 590] -row 4 -columnspan 3 -padx 2
      grid [ttk::label .aboutmenu.license2 \
         -text {Steam Sky is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.} \
         -wraplength 590] -row 5 -columnspan 3
      grid [ttk::button .aboutmenu.showlicense -text {Show full license} \
         -underline 0 -command {
            pack forget .aboutmenu
            pack .showfilemenu -fill both -expand true
            ShowFile COPYING
         }] -row 6 -column 1 -sticky e
      tooltip::tooltip .aboutmenu.showlicense \
         {Show full legal text of GNU GPLv3 license}
      grid [ttk::button .aboutmenu.back -text {Back to main menu} -underline 0 \
         -command {
            bind . <Alt-s> {}
            bind . <Alt-b> {}
            bind . <Escape> {}
            pack forget .aboutmenu
            pack .mainmenu -fill both -expand true
         }] -row 6 -column 2 -sticky e
      tooltip::tooltip .aboutmenu.back {Back to the main menu}

      # Show file content
      ttk::frame .showfilemenu -style Main.TFrame
      pack [ttk::button .showfilemenu.back -text {Back} -underline 0 -command {
         bind . <Alt-b> {}
         bind . <Escape> {}
         pack forget .showfilemenu
         pack .mainmenu -fill both -expand true
      }] -side bottom -anchor e -pady 2 -padx 2
      tooltip::tooltip .showfilemenu.back {Back to the main menu}
      pack [ttk::scrollbar .showfilemenu.scroll -orient vertical \
         -command [list .showfilemenu.text yview]] -side right -fill y -pady 2 \
         -padx 2
      pack [text .showfilemenu.text -wrap char \
         -yscrollcommand {.showfilemenu.scroll set} -font HelpFont] -side top \
         -fill both -expand true -pady 2 -padx 2
      ::autoscroll::autoscroll .showfilemenu.scroll""")
  tclEval(script = """
      # News menu
      ttk::frame .newsmenu -style Main.TFrame
      grid [text .newsmenu.text -wrap word -yscrollcommand {.newsmenu.scroll set} \
         -font HelpFont] -sticky nesw -columnspan 2 -pady 2 -padx 2
      grid [ttk::scrollbar .newsmenu.scroll -orient vertical \
         -command [list .newsmenu.text yview]] -column 2 -row 0 -sticky ns -pady 2 \
         -padx 2
      ::autoscroll::autoscroll .newsmenu.scroll
      grid [ttk::button .newsmenu.showall -text {Show all changes} -underline 0] \
         -row 1 -column 0 -sticky e -pady 2
      tooltip::tooltip .newsmenu.showall \
         {Show all changes to the game since previous big stable version}
      grid [ttk::button .newsmenu.back -text {Back to menu} -underline 0 -command {
         bind . <Alt-s> {}
         bind . <Alt-b> {}
         bind . <Escape> {}
         pack forget .newsmenu
         pack .mainmenu -fill both -expand true
      }] -row 1 -column 1 -sticky e -pady 2 -padx 2
      tooltip::tooltip .newsmenu.back {Back to the main menu}
      grid columnconfigure .newsmenu 0 -weight 1
      grid rowconfigure .newsmenu 0 -weight 1

      # Hall of Fame menu
      ttk::frame .hofmenu -style Main.TFrame
      grid [ttk::treeview .hofmenu.view -yscrollcommand {.hofmenu.yscroll set} \
         -xscrollcommand {.hofmenu.xscroll set} -show headings \
         -columns [list position name points diedfrom] -selectmode none] \
         -sticky nesw -padx 2 -pady 2
      .hofmenu.view heading position -text {Position}
      .hofmenu.view column position -width 100
      .hofmenu.view heading name -text {Name}
      .hofmenu.view column name -width 150
      .hofmenu.view heading points -text {Points}
      .hofmenu.view column points -width 100
      .hofmenu.view heading diedfrom -text {Died from}
      grid [ttk::scrollbar .hofmenu.yscroll -orient vertical \
         -command [list .hofmenu.view yview]] -column 1 -row 0 -sticky ns -padx 2 \
         -pady 2
      ::autoscroll::autoscroll .hofmenu.yscroll
      grid [ttk::scrollbar .hofmenu.xscroll -orient horizontal \
         -command [list .hofmenu.view xview]] -column 0 -row 1 -columnspan 2 \
         -sticky we
      ::autoscroll::autoscroll .hofmenu.xscroll
      grid [ttk::button .hofmenu.back -text {Back to menu} -command {
         bind . <Alt-b> {}
         bind . <Escape> {}
         pack forget .hofmenu
         pack .mainmenu -fill both -expand true
      }] -row 2 -column 0 -columnspan 2 -sticky e -pady 2 -padx 2
      tooltip::tooltip .hofmenu.back {Back to the main menu}
      grid columnconfigure .hofmenu 0 -weight 1
      grid rowconfigure .hofmenu 0 -weight 1

      # Load game menu
      ttk::frame .loadmenu -style Main.TFrame
      grid [ttk::frame .loadmenu.list] -sticky we -padx {2cm 2}
      grid [ttk::button .loadmenu.back -text {Back to main menu} -underline 0 \
         -command {
            bind . <Alt-b> {}
            pack forget .loadmenu
            pack .mainmenu -fill both -expand true
         }] -sticky e -padx 2 -pady 2
      tooltip::tooltip .loadmenu.back {Back to the main menu}
      grid columnconfigure .loadmenu 0 -weight 1
      grid rowconfigure .loadmenu 0 -weight 1

      # New game setting menu
      set windowid {}
      set playertooltips \
         [list \
         "General player character settings. Select field which you want to set to see more information about." \
         "Enter character name." "Select the gender of your character." \
         "Enter ship name." \
         "Select starting goal for your character.\nYou can change it later in game." \
         "Select your faction from a list. Factions have the biggest impact on game.\nThey determine the amount of bases and some playing styles.\nMore information about each faction can be found after selecting it.\nYou can't change this later." \
         "Select your career from a list. Careers have some impact on gameplay\n(each have bonuses to gaining experience in some fields plus\nthey determine your starting ship and crew). More info about each\ncareer can be found after selecting it. You can't change career later." \
         "Select type of base in which you will start the game.\nThis may have some impact on game difficulty."]
      set difficultytooltips [list \
         "Set difficulty of new game. Each value can be between 1 and 500. Each change has an impact not only on the game's difficulty but also on amount of points gained in the game. Select a field to get more information about it." \
         "Select game difficulty preset level." "Percentage of damage done by enemy ships in combat.\nLowering it makes the  game easier but lowers the\namount of score gained as well." \
         "Percentage of damage done by the player's ship in combat.\nRaising it makes the game easier but lowers the amount\nof score gained as well." \
         "Percentage of damage done by enemies in melee combat.\nLowering it makes the game easier but lowers the\namount of score gained as well." \
         "Percentage of damage done by player's crew (and player character)\nin melee combat. Raising it makes the game easier but lowers the\namount of score gained as well." \
         "Percentage of experience gained by player and their crew from actions.\nRaising it makes the game easier but lowers the amount of score gained as well." \
         "Percentage of reputation in bases gained or lost by player in sky bases\ndue to player actions. Raising it makes the game easier but lowers the\namount of score gained as well." \
         "Percentage of the standard material cost and time needed\nfor upgrading ship modules. Lowering it makes the game\neasier but lowers the amount of score gained as well." \
         "Percentage of the standard prices for services in bases (docking, repairing ship,\nrecruiting new crew members, etc). Lowering it makes the game easier but lowers\nthe amount of score gained as well." \
         "Select random values for all settings." \
         "If you select this option, all difficulty settings will be\nrandomized during start new game. Not recommended for new players."]
      proc SetInfo {name index} {
         global playertooltips
         global difficultytooltips
         .newgamemenu.info.text configure -state normal
         .newgamemenu.info.text delete 1.0 end
         if {$name == "player"} {
            .newgamemenu.info.text insert end [lindex $playertooltips $index]
         } else {
            .newgamemenu.info.text insert end [lindex $difficultytooltips $index]
         }
         .newgamemenu.info.text configure -state disabled
      }
      proc SetPoints {{difficulty Custom}} {
         set values [list [.newgamemenu.canvas.difficulty.enemydamage get] \
            [.newgamemenu.canvas.difficulty.playerdamage get] \
            [.newgamemenu.canvas.difficulty.enemymeleedamage get] \
            [.newgamemenu.canvas.difficulty.playermeleedamage get] \
            [.newgamemenu.canvas.difficulty.experience get] \
            [.newgamemenu.canvas.difficulty.reputation get] \
            [.newgamemenu.canvas.difficulty.upgrade get] \
            [.newgamemenu.canvas.difficulty.prices get]]
         set totalpoints 0
         for {set i 0} {$i < 8} {incr i} {
            set value [regsub -all {[^0-9]} [lindex $values $i] {}]
            if {$value == ""} {
               set value 1
            } elseif {$value < 1} {
               set value 1
            } elseif {$value > 500} {
               set value 500
            }
            if {$i == 1 || $i == 3 || $i == 4 || $i == 5} {
               if {$value < 100} {
                  set value [expr 100 + ((100 - $value) * 4)]
               } elseif {$value > 100} {
                  set value [expr 100 - $value]
               }
            }
            set totalpoints [expr $totalpoints + $value]
         }
         set totalpoints [expr $totalpoints  / 8]
         if {$totalpoints < 1} {
            set totalpoints 1
         }
         .newgamemenu.canvas.difficulty.totalpoints configure \
            -text "Total gained points: $totalpoints%"
         .newgamemenu.canvas.difficulty.difficultylevel set $difficulty
         return true
      }""")
  tclEval(script = """
      ttk::frame .newgamemenu -style Main.TFrame
      grid [ttk::frame .newgamemenu.buttonsbox] -columnspan 3 -pady {5 2}
      grid [ttk::radiobutton .newgamemenu.buttonsbox.player -text Player \
         -state selected -style Radio.Toolbutton -value player -variable newtab \
         -underline 0 -command {
         .newgamemenu.info.text configure -state normal
         .newgamemenu.info.text delete 1.0 end
         .newgamemenu.info.text insert end [lindex $playertooltips 0]
         .newgamemenu.info.text configure -state disabled
         ::autoscroll::unautoscroll .newgamemenu.scrollbar
         .newgamemenu.canvas delete $windowid
         set windowid [.newgamemenu.canvas create window 0 0 -anchor nw \
            -window .newgamemenu.canvas.player]
         .newgamemenu.canvas configure \
            -width [winfo reqwidth .newgamemenu.canvas.player] \
            -height [winfo reqheight .newgamemenu.canvas.player] \
            -scrollregion [.newgamemenu.canvas bbox all]
         ::autoscroll::autoscroll .newgamemenu.scrollbar
      }] -sticky e
      tooltip::tooltip .newgamemenu.buttonsbox.player \
         {Show settings for your character.}
      grid [ttk::radiobutton .newgamemenu.buttonsbox.difficulty -text Difficulty \
         -style Radio.Toolbutton -value difficulty -variable newtab -underline 0 \
         -command {
            .newgamemenu.info.text configure -state normal
            .newgamemenu.info.text delete 1.0 end
            .newgamemenu.info.text insert end [lindex $difficultytooltips 0]
            .newgamemenu.info.text configure -state disabled
            .newgamemenu.canvas delete $windowid
            set windowid [.newgamemenu.canvas create window 0 0 -anchor nw \
               -window .newgamemenu.canvas.difficulty]
            .newgamemenu.canvas configure \
               -width [winfo reqwidth .newgamemenu.canvas.difficulty] \
               -height [winfo reqheight .newgamemenu.canvas.difficulty] \
               -scrollregion [.newgamemenu.canvas bbox all]
         }] -column 1 -row 0 -sticky w
      tooltip::tooltip .newgamemenu.buttonsbox.difficulty \
         {Show settings for the game difficulty.}
      grid [canvas .newgamemenu.canvas \
         -yscrollcommand [list .newgamemenu.scrollbar set]] -sticky nwes -row 1 \
         -padx 2
      grid [ttk::scrollbar .newgamemenu.scrollbar -orient vertical \
         -command [list .newgamemenu.canvas yview]] -sticky ns -row 1 -column 1
      ttk::frame .newgamemenu.canvas.player
      grid [ttk::label .newgamemenu.canvas.player.labelplayername \
         -text {Character name:}] -sticky e -padx {0 5}
      grid [ttk::entry .newgamemenu.canvas.player.playername -width 15] -row 0 \
         -column 1 -pady 3
      tooltip::tooltip .newgamemenu.canvas.player.playername \
         [lindex $playertooltips 1]
      grid [ttk::button .newgamemenu.canvas.player.randomplayer \
         -style Toolbutton -command {RandomName player}] -row 0 -column 2 \
         -padx {5 0}
      tooltip::tooltip .newgamemenu.canvas.player.randomplayer \
         "Select a random name for the character,\nbased on the character gender"
      bind .newgamemenu.canvas.player.playername <FocusIn> {SetInfo player 1}
      grid [ttk::label .newgamemenu.canvas.player.labelgender \
         -text {Character gender:}] -row 1 -sticky e -padx {0 5}
      grid [ttk::frame .newgamemenu.canvas.player.gender] -row 1 -column 1 -pady 3
      grid [ttk::radiobutton .newgamemenu.canvas.player.gender.male \
         -style Toolbutton -value M -variable playergender \
         -command {SetInfo player 2}] -padx {0 5}
      tooltip::tooltip .newgamemenu.canvas.player.gender.male Male
      grid [ttk::radiobutton .newgamemenu.canvas.player.gender.female \
         -style Toolbutton -value F -variable playergender \
         -command {SetInfo player 2}] -row 0 -column 1
      tooltip::tooltip .newgamemenu.canvas.player.gender.female Female
      grid [ttk::label .newgamemenu.canvas.player.labelshipname -text {Ship name:}] \
         -row 2 -sticky e -padx {0 5}
      grid [ttk::entry .newgamemenu.canvas.player.shipname -width 15] -row 2 \
         -column 1 -pady 3
      tooltip::tooltip .newgamemenu.canvas.player.shipname [lindex $playertooltips 3]
      bind .newgamemenu.canvas.player.shipname <FocusIn> {SetInfo player 3}
      grid [ttk::button .newgamemenu.canvas.player.randomship \
         -style Toolbutton -command {RandomName ship}] -row 2 -column 2 \
         -padx {5 0}
      tooltip::tooltip .newgamemenu.canvas.player.randomship \
         "Select a random name for the ship"
      grid [ttk::label .newgamemenu.canvas.player.labelgoal -text {Character goal:}] \
         -row 3 -sticky e -padx {0 5}
      grid [ttk::button .newgamemenu.canvas.player.goal -text {Random} \
         -command {ShowGoals .newgamemenu.canvas.player.goal}] -row 3 -column 1 \
         -columnspan 2 -pady 3
      tooltip::tooltip .newgamemenu.canvas.player.goal [lindex $playertooltips 4]
      bind .newgamemenu.canvas.player.goal <FocusIn> {SetInfo player 4}
      grid [ttk::label .newgamemenu.canvas.player.labelfaction \
         -text {Character faction:}] -row 4 -sticky e -padx {0 5}
      grid [ttk::combobox .newgamemenu.canvas.player.faction -state readonly \
         -width 16] -row 4 -column 1 -columnspan 2 -pady 3
      tooltip::tooltip .newgamemenu.canvas.player.faction [lindex $playertooltips 5]
      bind .newgamemenu.canvas.player.faction <FocusIn> SetFaction
      bind .newgamemenu.canvas.player.faction <<ComboboxSelected>> SetFaction
      grid [ttk::label .newgamemenu.canvas.player.labelcareer \
         -text {Character career:}] -row 5 -sticky e -padx {0 5}
      grid [ttk::combobox .newgamemenu.canvas.player.career -state readonly \
         -width 16] -row 5 -column 1 -columnspan 2 -pady 3
      tooltip::tooltip .newgamemenu.canvas.player.career [lindex $playertooltips 6]
      bind .newgamemenu.canvas.player.career <FocusIn> {SetCareer}
      bind .newgamemenu.canvas.player.career <<ComboboxSelected>> SetCareer
      grid [ttk::label .newgamemenu.canvas.player.labelbase \
         -text {Starting base type:}] -row 6 -sticky e -padx {0 5}
      grid [ttk::combobox .newgamemenu.canvas.player.base -state readonly -width 16] \
         -row 6 -column 1 -columnspan 2 -pady 3
      tooltip::tooltip .newgamemenu.canvas.player.base [lindex $playertooltips 7]
      bind .newgamemenu.canvas.player.base <FocusIn> {SetBase}
      bind .newgamemenu.canvas.player.base <<ComboboxSelected>> SetBase
      ttk::frame .newgamemenu.canvas.difficulty
      SetScrollbarBindings .newgamemenu.canvas.difficulty .newgamemenu.scrollbar
      grid [ttk::label .newgamemenu.canvas.difficulty.difficultylabel \
         -text {Difficulty level:}] -sticky e -padx {0 5}
      SetScrollbarBindings .newgamemenu.canvas.difficulty.difficultylabel \
         .newgamemenu.scrollbar
      grid [ttk::combobox .newgamemenu.canvas.difficulty.difficultylevel \
         -state readonly \
         -values [list {Very Easy} Easy Normal Hard {Very Hard} Custom] -width 7] \
         -column 1 -row 0 -pady 3
      bind .newgamemenu.canvas.difficulty.difficultylevel <<ComboboxSelected>> {
         set level [.newgamemenu.canvas.difficulty.difficultylevel get]
         switch $level {
            "Very Easy" {
               .newgamemenu.canvas.difficulty.enemydamage set 10
               .newgamemenu.canvas.difficulty.playerdamage set 450
               .newgamemenu.canvas.difficulty.enemymeleedamage set 10
               .newgamemenu.canvas.difficulty.playermeleedamage set 450
               .newgamemenu.canvas.difficulty.experience set 450
               .newgamemenu.canvas.difficulty.reputation set 450
               .newgamemenu.canvas.difficulty.upgrade set 10
               .newgamemenu.canvas.difficulty.prices set 10
            }
            "Easy" {
               .newgamemenu.canvas.difficulty.enemydamage set 50
               .newgamemenu.canvas.difficulty.playerdamage set 250
               .newgamemenu.canvas.difficulty.enemymeleedamage set 50
               .newgamemenu.canvas.difficulty.playermeleedamage set 250
               .newgamemenu.canvas.difficulty.experience set 250
               .newgamemenu.canvas.difficulty.reputation set 250
               .newgamemenu.canvas.difficulty.upgrade set 50
               .newgamemenu.canvas.difficulty.prices set 50
            }
            "Normal" {
               .newgamemenu.canvas.difficulty.enemydamage set 100
               .newgamemenu.canvas.difficulty.playerdamage set 100
               .newgamemenu.canvas.difficulty.enemymeleedamage set 100
               .newgamemenu.canvas.difficulty.playermeleedamage set 100
               .newgamemenu.canvas.difficulty.experience set 100
               .newgamemenu.canvas.difficulty.reputation set 100
               .newgamemenu.canvas.difficulty.upgrade set 100
               .newgamemenu.canvas.difficulty.prices set 100
            }
            "Hard" {
               .newgamemenu.canvas.difficulty.enemydamage set 250
               .newgamemenu.canvas.difficulty.playerdamage set 50
               .newgamemenu.canvas.difficulty.enemymeleedamage set 250
               .newgamemenu.canvas.difficulty.playermeleedamage set 50
               .newgamemenu.canvas.difficulty.experience set 50
               .newgamemenu.canvas.difficulty.reputation set 50
               .newgamemenu.canvas.difficulty.upgrade set 250
               .newgamemenu.canvas.difficulty.prices set 250
            }
            "Very Hard" {
               .newgamemenu.canvas.difficulty.enemydamage set 450
               .newgamemenu.canvas.difficulty.playerdamage set 10
               .newgamemenu.canvas.difficulty.enemymeleedamage set 450
               .newgamemenu.canvas.difficulty.playermeleedamage set 10
               .newgamemenu.canvas.difficulty.experience set 10
               .newgamemenu.canvas.difficulty.reputation set 10
               .newgamemenu.canvas.difficulty.upgrade set 450
               .newgamemenu.canvas.difficulty.prices set 450
            }
         }
         SetPoints $level
      }""")
  tclEval(script = """
      tooltip::tooltip .newgamemenu.canvas.difficulty.difficultylevel \
         [lindex $difficultytooltips 1]
      bind .newgamemenu.canvas.difficulty.difficultylevel <FocusIn> \
         {SetInfo difficulty 1}
      grid [ttk::label .newgamemenu.canvas.difficulty.enemydamagelabel \
         -text {Enemy ship damage:}] -row 1 -sticky e -padx {0 5}
      SetScrollbarBindings .newgamemenu.canvas.difficulty.enemydamagelabel \
         .newgamemenu.scrollbar
      grid [ttk::spinbox .newgamemenu.canvas.difficulty.enemydamage -from 1 -to 500 \
         -increment 1.0 -width 5 -validate focusout \
         -validatecommand SetPoints -command SetPoints] -column 1 -row 1 -pady 3
      tooltip::tooltip .newgamemenu.canvas.difficulty.enemydamage \
         [lindex $difficultytooltips 2]
      bind .newgamemenu.canvas.difficulty.enemydamage <FocusIn> \
         {SetInfo difficulty 2}
      grid [ttk::label .newgamemenu.canvas.difficulty.playerdamagelabel \
         -text {Player ship damage:}] -row 2 -sticky e -padx {0 5}
      SetScrollbarBindings .newgamemenu.canvas.difficulty.playerdamagelabel \
         .newgamemenu.scrollbar
      grid [ttk::spinbox .newgamemenu.canvas.difficulty.playerdamage -from 1 -to 500 \
         -increment 1.0 -width 5 -validate focusout -validatecommand SetPoints \
         -command SetPoints] -column 1 -row 2 -pady 3
      tooltip::tooltip .newgamemenu.canvas.difficulty.playerdamage \
         [lindex $difficultytooltips 3]
      bind .newgamemenu.canvas.difficulty.playerdamage <FocusIn> \
         {SetInfo difficulty 3}
      grid [ttk::label .newgamemenu.canvas.difficulty.enemymeleedamagelabel \
         -text {Enemy damage in melee combat:} -wraplength 150] -row 3 -sticky e \
         -padx {0 5}
      SetScrollbarBindings .newgamemenu.canvas.difficulty.enemymeleedamagelabel \
         .newgamemenu.scrollbar
      grid [ttk::spinbox .newgamemenu.canvas.difficulty.enemymeleedamage -from 1 \
         -to 500 -increment 1.0 -width 5 -validate focusout \
         -validatecommand SetPoints -command SetPoints] -column 1 -row 3 -pady 3
      tooltip::tooltip .newgamemenu.canvas.difficulty.enemymeleedamage \
         [lindex $difficultytooltips 4]
      bind .newgamemenu.canvas.difficulty.enemymeleedamage <FocusIn> \
         {SetInfo difficulty 4}
      grid [ttk::label .newgamemenu.canvas.difficulty.playermeleedamagelabel \
         -text {Player crew damage in melee combat:} -wraplength 150] -row 4 \
         -sticky e -padx {0 5}
      SetScrollbarBindings .newgamemenu.canvas.difficulty.playermeleedamagelabel \
         .newgamemenu.scrollbar
      grid [ttk::spinbox .newgamemenu.canvas.difficulty.playermeleedamage -from 1 \
         -to 500 -increment 1.0 -width 5 -validate focusout \
         -validatecommand SetPoints -command SetPoints] -column 1 -row 4 -pady 3
      tooltip::tooltip .newgamemenu.canvas.difficulty.playermeleedamage \
         [lindex $difficultytooltips 5]
      bind .newgamemenu.canvas.difficulty.playermeleedamage <FocusIn> \
         {SetInfo difficulty 5}
      grid [ttk::label .newgamemenu.canvas.difficulty.experiencelabel \
         -text {Experience gained:}] -row 5 -sticky e -padx {0 5}
      SetScrollbarBindings .newgamemenu.canvas.difficulty.experiencelabel \
         .newgamemenu.scrollbar
      grid [ttk::spinbox .newgamemenu.canvas.difficulty.experience -from 1 -to 500 \
         -increment 1.0 -width 5 -validate focusout -validatecommand SetPoints \
         -command SetPoints] -column 1 -row 5 -pady 3
      tooltip::tooltip .newgamemenu.canvas.difficulty.experience \
         [lindex $difficultytooltips 6]
      bind .newgamemenu.canvas.difficulty.experience <FocusIn> {SetInfo difficulty 6}
      grid [ttk::label .newgamemenu.canvas.difficulty.reputationlabel \
         -text {Reputation gained:}] -row 6 -sticky e -padx {0 5}
      SetScrollbarBindings .newgamemenu.canvas.difficulty.reputationlabel \
         .newgamemenu.scrollbar
      grid [ttk::spinbox .newgamemenu.canvas.difficulty.reputation -from 1 -to 500 \
         -increment 1.0 -width 5 -validate focusout -validatecommand SetPoints \
         -command SetPoints] -column 1 -row 6 -pady 3
      tooltip::tooltip .newgamemenu.canvas.difficulty.reputation \
         [lindex $difficultytooltips 7]
      bind .newgamemenu.canvas.difficulty.reputation <FocusIn> {SetInfo difficulty 7}
      grid [ttk::label .newgamemenu.canvas.difficulty.upgradelabel \
         -text {Upgrade cost:}] -row 7 -sticky e -padx {0 5}
      SetScrollbarBindings .newgamemenu.canvas.difficulty.upgradelabel \
         .newgamemenu.scrollbar
      grid [ttk::spinbox .newgamemenu.canvas.difficulty.upgrade -from 1 -to 500 \
         -increment 1.0 -width 5 -validate focusout -validatecommand SetPoints \
         -command SetPoints] -column 1 -row 7 -pady 3
      tooltip::tooltip .newgamemenu.canvas.difficulty.upgrade \
         [lindex $difficultytooltips 8]
      bind .newgamemenu.canvas.difficulty.upgrade <FocusIn> {SetInfo difficulty 8}
      grid [ttk::label .newgamemenu.canvas.difficulty.priceslabel \
         -text {Prices in bases:}] -row 8 -sticky e -padx {0 5}
      SetScrollbarBindings .newgamemenu.canvas.difficulty.priceslabel \
         .newgamemenu.scrollbar
      grid [ttk::spinbox .newgamemenu.canvas.difficulty.prices -from 1 -to 500 \
         -increment 1.0 -width 5 -validate focusout -validatecommand SetPoints \
         -command SetPoints] -column 1 -row 8 -pady 3
      tooltip::tooltip .newgamemenu.canvas.difficulty.prices \
         [lindex $difficultytooltips 9]
      bind .newgamemenu.canvas.difficulty.prices <FocusIn> {SetInfo difficulty 9}
      grid [ttk::button .newgamemenu.canvas.difficulty.random -text Random -command {
         .newgamemenu.canvas.difficulty.enemydamage set [expr { int(499 * rand()) \
            + 1 }]
         .newgamemenu.canvas.difficulty.playerdamage set [expr { int(499 * rand()) \
            + 1 }]
         .newgamemenu.canvas.difficulty.enemymeleedamage set [expr { int(499 * \
            rand()) + 1 }]
         .newgamemenu.canvas.difficulty.playermeleedamage set [expr { int(499 * \
            rand()) + 1 }]
         .newgamemenu.canvas.difficulty.experience set [expr { int(499 * rand()) + \
            1 }]
         .newgamemenu.canvas.difficulty.reputation set [expr { int(499 * rand()) + \
            1 }]
         .newgamemenu.canvas.difficulty.upgrade set [expr { int(499 * rand()) + 1 }]
         .newgamemenu.canvas.difficulty.prices set [expr { int(499 * rand()) + 1 }]
         SetPoints
      }] -row 9 -columnspan 2 -sticky we -pady 3 -padx 5
      tooltip::tooltip .newgamemenu.canvas.difficulty.random \
         [lindex $difficultytooltips 10]
      bind .newgamemenu.canvas.difficulty.random <FocusIn> {SetInfo difficulty 10}
      grid [ttk::label .newgamemenu.canvas.difficulty.randomizelabel \
         -text {Randomize difficulty on game start} -wraplength 150] -row 10
      SetScrollbarBindings .newgamemenu.canvas.difficulty.randomizelabel \
         .newgamemenu.scrollbar
      tooltip::tooltip .newgamemenu.canvas.difficulty.randomizelabel \
         [lindex $difficultytooltips 11]
      grid [ttk::checkbutton .newgamemenu.canvas.difficulty.randomize] -row 10 \
         -column 1
      tooltip::tooltip .newgamemenu.canvas.difficulty.randomize \
         [lindex $difficultytooltips 11]
      bind .newgamemenu.canvas.difficulty.randomize <FocusIn> {SetInfo difficulty 11}
      grid [ttk::label .newgamemenu.canvas.difficulty.totalpoints \
         -text {Total gained points: 100%}] -row 11 -columnspan 2
      SetScrollbarBindings .newgamemenu.canvas.difficulty.totalpoints \
         .newgamemenu.scrollbar
      grid [ttk::labelframe .newgamemenu.info -text Info] -row 1 -column 2 \
         -sticky nwes -padx 2
      pack [ttk::scrollbar .newgamemenu.info.scroll -orient vertical \
         -command [list .newgamemenu.info.text yview]] -side right -fill y
      pack [text .newgamemenu.info.text -wrap word \
         -yscrollcommand [list .newgamemenu.info.scroll set]] -expand true \
         -fill both -side top
      ::autoscroll::autoscroll .newgamemenu.info.scroll
      grid [ttk::frame .newgamemenu.buttonsbox2] -row 2 -columnspan 3 -pady 2
      grid [ttk::button .newgamemenu.buttonsbox2.start -text {Start game} \
         -underline 0 -command {
            bind . <Alt-s> {}
            bind . <Alt-b> {}
            bind . <Alt-p> {}
            bind . <Alt-d> {}
            bind . <Escape> {}
            pack forget .newgamemenu
            NewGame
            focus .
         }] -sticky e -padx 3 -pady 3
      tooltip::tooltip .newgamemenu.buttonsbox2.start {Start the game.}
      grid [ttk::button .newgamemenu.buttonsbox2.back -text {Back to menu} \
         -underline 0 -command {
            bind . <Alt-s> {}
            bind . <Alt-b> {}
            bind . <Alt-p> {}
            bind . <Alt-d> {}
            bind . <Escape> {}
            pack forget .newgamemenu
            pack .mainmenu -fill both -expand true
         }] -column 1 -row 0 -sticky w -padx 3 -pady 3
      tooltip::tooltip .newgamemenu.buttonsbox2.back {Back to the main menu.}
      grid columnconfigure .newgamemenu .newgamemenu.info -weight 3
      grid rowconfigure .newgamemenu .newgamemenu.info -weight 3
  """)
  if not gameSettings.showTooltips:
    tclEval(script = "tooltip::tooltip disable")
  setFonts(newSize = gameSettings.mapFontSize, fontType = mapFont)
  setFonts(newSize = gameSettings.helpFontSize, fontType = helpFont)
  setFonts(newSize = gameSettings.interfaceFontSize, fontType = interfaceFont)
  const versionLabel: string = ".mainmenu.version"
  tclEval(script = versionLabel & " configure -text {" & gameVersion & " development}")
  try:
    dataError = loadGameData()
  except:
    dataError = getCurrentExceptionMsg()
    showMainMenu()
  if dataError.len > 0:
    return
  const playerFrameName: string = ".newgamemenu.canvas.player"
  var textEntry: string = playerFrameName & ".playername"
  tclEval(script = textEntry & " delete 0 end")
  tclEval(script = textEntry & " insert 0 {" & newGameSettings.playerName & "}")
  tclSetVar(varName = "playergender", newValue = $newGameSettings.playerGender)
  textEntry = playerFrameName & ".shipname"
  tclEval(script = textEntry & " delete 0 end")
  tclEval(script = textEntry & " insert 0 {" & newGameSettings.shipName & "}")
  var values: string = ""
  for faction in factionsList.values:
    if faction.careers.len > 0:
      values = values & " {" & faction.name & "}"
  values.add(y = " Random")
  var comboBox: string = playerFrameName & ".faction"
  tclEval(script = comboBox & " configure -values [list" & values & "]")
  if newGameSettings.playerFaction == "random":
    tclEval(script = comboBox & " set Random")
  else:
    try:
      discard tclEval(script = comboBox & " set {" & factionsList[
          newGameSettings.playerFaction].name & "}")
    except:
      showError(message = "Can't set player's faction.")
  tclEval(script = "SetFaction")
  comboBox = playerFrameName & ".career"
  if newGameSettings.playerCareer == "random":
    tclEval(script = comboBox & " set Random")
  else:
    try:
      discard tclEval(script = comboBox & " set {" & careersList[
          newGameSettings.playerCareer].name & "}")
    except:
      showError(message = "Can't set player's career")
  comboBox = playerFrameName & ".base"
  try:
    discard tclEval(script = comboBox & " set " & (
        if newGameSettings.startingBase == "Any": "Any" else: "{" &
        basesTypesList[newGameSettings.startingBase].name & "}"))
  except:
    showError(message = "Can't set starting base.")
  const difficultyFrameName: string = ".newgamemenu.canvas.difficulty"
  comboBox = difficultyFrameName & ".difficultylevel"
  var spinBox: string = difficultyFrameName & ".enemydamage"
  tclEval(script = spinBox & " set " & $((newGameSettings.enemyDamageBonus *
      100.0).Natural))
  spinBox = difficultyFrameName & ".playerdamage"
  tclEval(script = spinBox & " set " & $((newGameSettings.playerDamageBonus *
      100.0).Natural))
  spinBox = difficultyFrameName & ".enemymeleedamage"
  tclEval(script = spinBox & " set " & $((
      newGameSettings.enemyMeleeDamageBonus * 100.0).Natural))
  spinBox = difficultyFrameName & ".playermeleedamage"
  tclEval(script = spinBox & " set " & $((
      newGameSettings.playerMeleeDamageBonus * 100.0).Natural))
  spinBox = difficultyFrameName & ".experience"
  tclEval(script = spinBox & " set " & $((newGameSettings.experienceBonus *
      100.0).Natural))
  spinBox = difficultyFrameName & ".reputation"
  tclEval(script = spinBox & " set " & $((newGameSettings.reputationBonus *
      100.0).Natural))
  spinBox = difficultyFrameName & ".upgrade"
  tclEval(script = spinBox & " set " & $((newGameSettings.upgradeCostBonus *
      100.0).Natural))
  spinBox = difficultyFrameName & ".prices"
  tclEval(script = spinBox & " set " & $((newGameSettings.pricesBonus *
      100.0).Natural))
  tclEval(script = "SetPoints")
  tclEval(script = comboBox & " current " & $(
      newGameSettings.difficultyLevel.ord))
  tclEval(script = "event generate " & comboBox & " <<ComboboxSelected>>")
  var button: string = ".newgamemenu.canvas.player.randomplayer"
  tclEval(script = button & " configure -image randomicon")
  button = ".newgamemenu.canvas.player.randomship"
  tclEval(script = button & " configure -image randomicon")
  button = ".newgamemenu.canvas.player.gender.male"
  tclEval(script = button & " configure -image maleicon")
  button = ".newgamemenu.canvas.player.gender.female"
  tclEval(script = button & " configure -image femaleicon")
  showMainMenu()
