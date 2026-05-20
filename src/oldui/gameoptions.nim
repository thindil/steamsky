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

## Provides code related to the game's options, like showing them, setting
## their values, etc.

import std/[os, strutils, tables]
import contracts, nimalyzer
import ../[config, game, tk, types]
import coreui, combatui, errordialog, mapsui, themes, utilsui2

proc showOptionsTabCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], cdecl, contractual,
    ruleOff: "params".} =
  ## Show the selected options tab
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowOptionsTab
  let
    optionsCanvas: string = mainPaned & ".optionsframe.canvas"
    optionsFrame: string = optionsCanvas & ".options"
    oldFrame: string = tclEval2(script = "grid slaves " & optionsFrame & " -row 1")
  tclEval(script = "grid remove " & oldFrame)
  let frame: string = optionsFrame & "." & tclGetVar(varName = "newtab")
  tclEval(script = "grid " & frame & " -sticky nwes -padx 10")
  tclEval(script = "update")
  tclEval(script = optionsCanvas & " configure -scrollregion [list " & tclEval2(
      script = optionsCanvas & " bbox all") & "]")
  return tclOk

type AccelData = object
  shortcut, entryName, configName: string

var accels: array[53, AccelData] = [AccelData(shortcut: menuAccelerators[1],
    entryName: ".menu.shipinfo", configName: "ShipInfo"), AccelData(
    shortcut: menuAccelerators[2], entryName: ".menu.orders",
    configName: "Orders"), AccelData(shortcut: menuAccelerators[3],
    entryName: ".menu.crafts", configName: "Crafting"), AccelData(
    shortcut: menuAccelerators[4], entryName: ".menu.messages",
    configName: "LastMessages"), AccelData(shortcut: menuAccelerators[5],
    entryName: ".menu.knowledge", configName: "Knowledge"), AccelData(
    shortcut: menuAccelerators[6], entryName: ".menu.waitorders",
    configName: "WaitOrders"), AccelData(shortcut: menuAccelerators[7],
    entryName: ".menu.gamestats", configName: "GameStats"), AccelData(
    shortcut: menuAccelerators[8], entryName: ".menu.help", configName: "Help"),
    AccelData(shortcut: menuAccelerators[9], entryName: ".menu.gameoptions",
    configName: "GameOptions"), AccelData(shortcut: menuAccelerators[10],
    entryName: ".menu.quit", configName: "Quit"), AccelData(
    shortcut: menuAccelerators[11], entryName: ".menu.resign",
    configName: "Resign"), AccelData(shortcut: mapAccelerators[1],
    entryName: ".menu.menu", configName: "GameMenu"), AccelData(
    shortcut: mapAccelerators[2], entryName: ".map.mapoptions",
    configName: "MapOptions"), AccelData(shortcut: mapAccelerators[3],
    entryName: ".map.zoomin", configName: "ZoomInMap"), AccelData(
    shortcut: mapAccelerators[4], entryName: ".map.zoomout",
    configName: "ZoomOutMap"), AccelData(shortcut: mapAccelerators[5],
    entryName: ".movement.upleft", configName: "MoveUpLeft"), AccelData(
    shortcut: mapAccelerators[6], entryName: ".movement.up",
    configName: "MoveUp"), AccelData(shortcut: mapAccelerators[7],
    entryName: ".movement.upright", configName: "MoveUpRight"), AccelData(
    shortcut: mapAccelerators[8], entryName: ".movement.left",
    configName: "MoveLeft"), AccelData(shortcut: mapAccelerators[10],
    entryName: ".movement.wait", configName: "WaitInPlace"), AccelData(
    shortcut: mapAccelerators[9], entryName: ".movement.right",
    configName: "MoveRight"), AccelData(shortcut: mapAccelerators[11],
    entryName: ".movement.downleft", configName: "MoveDownLeft"), AccelData(
    shortcut: mapAccelerators[12], entryName: ".movement.down",
    configName: "MoveDown"), AccelData(shortcut: mapAccelerators[13],
    entryName: ".movement.downright", configName: "MoveDownRight"), AccelData(
    shortcut: mapAccelerators[14], entryName: ".movement.moveto",
    configName: "MoveTo"), AccelData(shortcut: mapAccelerators[15],
    entryName: ".map.center", configName: "CenterMap"), AccelData(
    shortcut: mapAccelerators[16], entryName: ".map.centerhomebase",
    configName: "CenterMapOnHomeBase"), AccelData(shortcut: mapAccelerators[17],
    entryName: ".map.mapupleft", configName: "MoveMapUpLeft"), AccelData(
    shortcut: mapAccelerators[18], entryName: ".map.mapup",
    configName: "MoveMapUp"), AccelData(shortcut: mapAccelerators[19],
    entryName: ".map.mapupright", configName: "MoveMapUpRight"), AccelData(
    shortcut: mapAccelerators[20], entryName: ".map.mapleft",
    configName: "MoveMapLeft"), AccelData(shortcut: mapAccelerators[21],
    entryName: ".map.mapright", configName: "MoveMapRight"), AccelData(
    shortcut: mapAccelerators[22], entryName: ".map.mapdownleft",
    configName: "MoveMapDownLeft"), AccelData(shortcut: mapAccelerators[23],
    entryName: ".map.mapdown", configName: "MoveMapDown"), AccelData(
    shortcut: mapAccelerators[24], entryName: ".map.mapdownright",
    configName: "MoveMapDownRight"), AccelData(shortcut: mapAccelerators[25],
    entryName: ".map.cursorupleft", configName: "MoveCursorUpLeft"), AccelData(
    shortcut: mapAccelerators[26], entryName: ".map.cursorup",
    configName: "MoveCursorUp"), AccelData(shortcut: mapAccelerators[27],
    entryName: ".map.cursorupright", configName: "MoveCursorUpRight"),
    AccelData(shortcut: mapAccelerators[28], entryName: ".map.cursorleft",
    configName: "MoveCursorLeft"), AccelData(shortcut: mapAccelerators[29],
    entryName: ".map.cursorright", configName: "MoveCursorRight"), AccelData(
    shortcut: mapAccelerators[30], entryName: ".map.cursordownleft",
    configName: "MoveCursorDownLeft"), AccelData(shortcut: mapAccelerators[31],
    entryName: ".map.cursordown", configName: "MoveCursorDown"), AccelData(
    shortcut: mapAccelerators[32], entryName: ".map.cursordownright",
    configName: "MoveCursorDownRight"), AccelData(shortcut: mapAccelerators[33],
    entryName: ".map.clickmouse", configName: "LeftClickMouse"), AccelData(
    shortcut: mapAccelerators[34], entryName: ".movement.fullstop",
    configName: "FullStop"), AccelData(shortcut: mapAccelerators[35],
    entryName: ".movement.quarterspeed", configName: "QuarterSpeed"), AccelData(
    shortcut: mapAccelerators[36], entryName: ".movement.halfspeed",
    configName: "HalfSpeed"), AccelData(shortcut: mapAccelerators[37],
    entryName: ".movement.fullspeed", configName: "FullSpeed"), AccelData(
    shortcut: fullScreenAccel, entryName: ".interface.fullscreenkey",
    configName: "FullScreen"), AccelData(shortcut: generalAccelerators[0],
    entryName: ".ui.resizefirst", configName: "ResizeFirst"), AccelData(
    shortcut: generalAccelerators[1], entryName: ".ui.resizesecond",
    configName: "ResizeSecond"), AccelData(shortcut: generalAccelerators[2],
    entryName: ".ui.resizethird", configName: "ResizeThird"), AccelData(
    shortcut: generalAccelerators[3], entryName: ".ui.resizefourth",
    configName: "ResizeFourth")]

proc showOptionsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Show the selected options tab
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowOptions
  tclSetVar(varName = "newtab", newValue = "general")
  var optionsFrame: string = mainPaned & ".optionsframe"
  let optionsCanvas: string = optionsFrame & ".canvas"
  type WidgetData = object
    name, value: string
  if tclEval2(script = "winfo exists " & optionsCanvas) == "0":
    tclEval(script = """
      ttk::frame .gameframe.paned.optionsframe
      set optionscanvas [canvas .gameframe.paned.optionsframe.canvas \
         -yscrollcommand [list .gameframe.paned.optionsframe.scrolly set] \
         -xscrollcommand [list .gameframe.paned.optionsframe.scrollx set]]
      pack [ttk::scrollbar .gameframe.paned.optionsframe.scrolly -orient vertical \
         -command [list $optionscanvas yview]] -side right -fill y
      pack $optionscanvas -side top -fill both
      pack [ttk::scrollbar .gameframe.paned.optionsframe.scrollx -orient horizontal \
         -command [list $optionscanvas xview]] -fill x
      SetScrollbarBindings $optionscanvas .gameframe.paned.optionsframe.scrolly
      set optionsframe [ttk::frame $optionscanvas.options]
      SetScrollbarBindings $optionsframe .gameframe.paned.optionsframe.scrolly
      # Tabs buttons
      set buttonsframe [ttk::frame $optionsframe.buttons]
      grid [ttk::radiobutton $buttonsframe.general -text General -state selected \
         -style Radio.Toolbutton -value general -variable newtab \
         -command ShowOptionsTab] -row 0 -column 1
      tooltip::tooltip $buttonsframe.general \
         "General settings of the game, like auto-movement, etc"
      grid [ttk::radiobutton $buttonsframe.movement -text {Movement keys} \
         -style Radio.Toolbutton -value movement -variable newtab \
         -command ShowOptionsTab] -row 0 -column 2
      tooltip::tooltip $buttonsframe.movement \
         "Change keyboard shortcuts related to move the ship"
      grid [ttk::radiobutton $buttonsframe.menu -text {Menu keys} \
         -style Radio.Toolbutton -value menu -variable newtab \
         -command ShowOptionsTab] -row 0 -column 3
      tooltip::tooltip $buttonsframe.menu \
         "Change keyboard shortcuts used to activate the general\ngame's menu options"
      grid [ttk::radiobutton $buttonsframe.map -text {Map keys} \
         -style Radio.Toolbutton -value map -variable newtab \
         -command ShowOptionsTab] -row 0 -column 4
      tooltip::tooltip $buttonsframe.map \
         "Change keyboard shortcuts used to manipulate (move,\nzoom, etc) the map"
      grid [ttk::radiobutton $buttonsframe.ui -text {General keys} \
         -style Radio.Toolbutton -value ui -variable newtab \
         -command ShowOptionsTab] -row 0 -column 5
      tooltip::tooltip $buttonsframe.ui \
         "Change keyboard shortcuts related to the game interface"
      grid [ttk::radiobutton $buttonsframe.interface -text Interface \
         -style Radio.Toolbutton -value interface -variable newtab \
         -command ShowOptionsTab] -row 0 -column 6
      tooltip::tooltip $buttonsframe.interface \
         "Settings related to the game interface, like font\nsize, etc"
      grid [ttk::radiobutton $buttonsframe.info -text Info \
         -style Radio.Toolbutton -value info -variable newtab \
         -command ShowOptionsTab] -row 0 -column 7
      tooltip::tooltip $buttonsframe.info \
         "Information about the game directories paths"
      grid $buttonsframe -sticky w -padx 5 -pady 5
      # General options
      set goptions [ttk::frame $optionsframe.general]
      grid [ttk::label $goptions.lbl1 -text {Auto rest when crew is tired:}] \
         -sticky w
      tooltip::tooltip $goptions.lbl1 \
         {Wait for crew is rested when pilot or engineer are too tired to work.}
      grid [ttk::checkbutton $goptions.autorest] -row 0 -column 1 -sticky w
      tooltip::tooltip $goptions.autorest \
         {Wait for crew is rested when pilot or engineer are too tired to work.}
      grid [ttk::label $goptions.lbl2 -text {Default speed after undocking:}] \
         -sticky w
      tooltip::tooltip $goptions.lbl2 {Default speed of ship after undock from base.}
      grid [ttk::combobox $goptions.speed -state readonly \
         -values [list {Full stop} {Quarted speed} {Half speed} {Full speed}] \
         -width 10] -row 1 -column 1 -sticky w
      tooltip::tooltip $goptions.speed \
         {Default speed of ship after undock from base.}
      grid [ttk::label $goptions.lbl3 \
         -text {Auto center map after set destination:}] -sticky w
      tooltip::tooltip $goptions.lbl3 \
         {After set destination for ship, center map on ship.}
      grid [ttk::checkbutton $goptions.autocenter] -row 2 -column 1 -sticky w
      tooltip::tooltip $goptions.autocenter \
         {After set destination for ship, center map on ship.}
      grid [ttk::label $goptions.lbl4 -text {Auto set base after finished mission:}] \
         -sticky w
      tooltip::tooltip $goptions.lbl4 \
         "After finished mission, set skybase from which\nmission was taken as a destination for ship."
      grid [ttk::checkbutton $goptions.autoreturn] -row 3 -column 1 -sticky w
      tooltip::tooltip $goptions.autoreturn \
         "After finished mission, set skybase from\nwhich mission was taken as a destination for ship."
      grid [ttk::label $goptions.lbl5 -text {Auto set destination after accepting mission:}] \
         -sticky w
      tooltip::tooltip $goptions.lbl5 \
         "After accepting a mission, set its target as a destination for ship."
      grid [ttk::checkbutton $goptions.autodestination] -row 4 -column 1 -sticky w
      tooltip::tooltip $goptions.autodestination \
         "After accepting a mission, set its target as a destination for ship."
      grid [ttk::label $goptions.lbl6 -text {Auto finish missions:}] -sticky w
      tooltip::tooltip $goptions.lbl6 \
         "Auto finish missions when ship is near corresponding skybase.\nMissions will not be finished if there is no trader on position\nor when there is Double Price event in the base."
      grid [ttk::checkbutton $goptions.autofinish] -row 5 -column 1 -sticky w
      tooltip::tooltip $goptions.autofinish \
         "Auto finish missions when ship is near corresponding skybase.\nMissions will not be finished if there is no trader on position\nor when there is Double Price event in the base."
      grid [ttk::label $goptions.lbl7 -text {Auto ask for bases:}] -sticky w
      tooltip::tooltip $goptions.lbl7 \
         {Auto ask for bases when ship end docking to bases.}
      grid [ttk::checkbutton $goptions.autoaskforbases] -row 6 -column 1 -sticky w
      tooltip::tooltip $goptions.autoaskforbases \
         {Auto ask for bases when ship end docking to bases.}
      grid [ttk::label $goptions.lbl8 -text {Auto ask for events:}] -sticky w
      tooltip::tooltip $goptions.lbl8 \
         {Auto ask for events when ship end docking to bases.}
      grid [ttk::checkbutton $goptions.autoaskforevents] -row 7 -column 1 -sticky w
      tooltip::tooltip $goptions.autoaskforevents \
         {Auto ask for events when ship end docking to bases.}
      grid [ttk::label $goptions.lbl9 -text {Low level of fuel:}] -sticky w
      tooltip::tooltip $goptions.lbl9 \
         "Amount of fuel below which you will see warning about\nlow level of. Enter value between 1 and 10 000."
      grid [ttk::spinbox $goptions.fuel -from 1 -to 10000 -validate key \
         -validatecommand {ValidateSpinbox %W %P {}} -width 5] -row 8 -column 1 \
         -sticky w
      tooltip::tooltip $goptions.fuel \
         "Amount of fuel below which you will see warning about\nlow level of. Enter value between 1 and 10 000."
      grid [ttk::label $goptions.lbl10 -text {Low level of drinks:}] -sticky w
      tooltip::tooltip $goptions.lbl10 \
         "Amount of drinks below which you will see warning\nabout low level of. Enter value between 1 and 10 000."
      grid [ttk::spinbox $goptions.drinks -from 1 -to 10000 -validate key \
         -validatecommand {ValidateSpinbox %W %P {}} -width 5] -row 9 -column 1 \
         -sticky w
      tooltip::tooltip $goptions.drinks \
         "Amount of drinks below which you will see warning\nabout low level of. Enter value between 1 and 10 000."
      grid [ttk::label $goptions.lbl11 -text {Low level of food:}] -sticky w
      tooltip::tooltip $goptions.lbl11 \
         "Amount of food below which you will see warning\nabout low level of. Enter value between 1 and 10 000."
      grid [ttk::spinbox $goptions.food -from 1 -to 10000 -validate key \
         -validatecommand {ValidateSpinbox %W %P {}} -width 5] -row 10 -column 1 \
         -sticky w
      tooltip::tooltip $goptions.food \
         "Amount of food below which you will see warning\nabout low level of. Enter value between 1 and 10 000."
      grid [ttk::label $goptions.lbl12 -text {Stop auto movement:}] -sticky w
      tooltip::tooltip $goptions.lbl12 \
         "Set when auto move ship should stop: never,\non meet any ship, on meet friendly ship or\non meet enemy ship."
      grid [ttk::combobox $goptions.automovestop -state readonly \
         -values [list {Never} {Any ship} {Friendly ship} {Enemy ship}] -width 10]\
         -row 11 -column 1 -sticky w
      tooltip::tooltip $goptions.automovestop \
         "Set when auto move ship should stop: never,\non meet any ship, on meet friendly ship or\non meet enemy ship."
      grid [ttk::label $goptions.lbl13 -text {Messages limit:}] -sticky w
      tooltip::tooltip $goptions.lbl13 \
         "Amount of messages stored in game. If new message arrive\nwhen limit is reached, oldest message will be deleted. Enter\nvalue between 10 and 5000."
      grid [ttk::spinbox $goptions.messageslimit -from 10 -to 5000 -validate key \
         -validatecommand {ValidateSpinbox %W %P {}} -width 5] -row 12 -column 1 \
         -sticky w
      tooltip::tooltip $goptions.messageslimit \
         "Amount of messages stored in game. If new message arrive\nwhen limit is reached, oldest message will be deleted. Enter\nvalue between 10 and 5000."
      grid [ttk::label $goptions.lbl14 -text {Saved messages:}] -sticky w
      tooltip::tooltip $goptions.lbl14 \
         "Maximum amount of last messages saved to file.\nEnter value between 5 and 200."
      grid [ttk::spinbox $goptions.savedmessages -from 5 -to 200 -validate key \
         -validatecommand {ValidateSpinbox %W %P {}} -width 5] -row 13 -column 1 \
         -sticky w
      tooltip::tooltip $goptions.savedmessages \
         "Maximum amount of last messages saved to file.\nEnter value between 5 and 200."
      grid [ttk::label $goptions.lbl15 -text {Messages order:}] -sticky w
      tooltip::tooltip $goptions.lbl15 \
         "In what order show messages in game. If Older first\nwill be select, then older messages will appear at top\nof the lists. Otherwise newer messages will be at top."
      grid [ttk::combobox $goptions.messagesorder -state readonly \
         -values [list {Older messages first} {Newer messages first}] -width 16] \
         -row 14 -column 1 -sticky w
      tooltip::tooltip $goptions.messagesorder \
         "In what order show messages in game. If Older first\nwill be select, then older messages will appear at top\nof the lists. Otherwise newer messages will be at top."
      grid [ttk::label $goptions.lbl16 -text {Autosave game:}] -sticky w
      tooltip::tooltip $goptions.lbl16 \
         {How often game should be automatically saved to disk.}
      grid [ttk::combobox $goptions.autosave -state readonly \
         -values [list {Never} {After dock to base} {After undock from base} \
         {Every game day} {Every game month} {Every game year}] -width 18] \
         -row 15 -column 1 -sticky w
      tooltip::tooltip $goptions.autosave \
         {How often game should be automatically saved to disk.}
      grid [ttk::label $goptions.lbl17 -text {Wait time:}] -sticky w
      tooltip::tooltip $goptions.lbl17 \
         {How much minutes will pass after press the Wait button.}
      grid [ttk::spinbox $goptions.waitinterval -from 1 -to 1440 -validate key \
         -validatecommand {ValidateSpinbox %W %P {}} -width 6] -row 16 -column 1 \
         -sticky w
      tooltip::tooltip $goptions.waitinterval \
         "How much minutes will pass after press the Wait button.\nEnter value between 1 and 1440"
      SetScrollbarBindings $goptions .gameframe.paned.optionsframe.scrolly
      for {set i 1} {$i < 18} {incr i} {
         SetScrollbarBindings $goptions.lbl$i .gameframe.paned.optionsframe.scrolly
      }""")
    tclEval(script = """
      # Movement keys options
      set specialkey {}
      # Set proper shortcut, validate it and check if it is not set somewhere
      proc SetShortcut {field key} {
         global specialkey moveoptions menuoptions mapoptions ioptions uioptions
         set fields [list $moveoptions.upleft $moveoptions.up $moveoptions.upright \
            $moveoptions.left $moveoptions.wait $moveoptions.right \
            $moveoptions.downleft $moveoptions.down $moveoptions.downright \
            $moveoptions.moveto $moveoptions.fullstop $moveoptions.quarterspeed \
            $moveoptions.halfspeed $moveoptions.fullspeed $menuoptions.shipinfo \
            $menuoptions.orders $menuoptions.crafts $menuoptions.messages \
            $menuoptions.knowledge $menuoptions.waitorders $menuoptions.gamestats \
            $menuoptions.help $menuoptions.gameoptions $menuoptions.quit \
            $menuoptions.resign $menuoptions.menu $mapoptions.center \
            $mapoptions.centerhomebase $mapoptions.mapleft $mapoptions.mapright \
            $mapoptions.mapup $mapoptions.mapdown $mapoptions.mapupleft \
            $mapoptions.mapupright $mapoptions.mapdownleft $mapoptions.mapdownright \
            $mapoptions.cursorupleft $mapoptions.cursorup $mapoptions.cursorupright \
            $mapoptions.cursorleft $mapoptions.cursorright \
            $mapoptions.cursordownleft $mapoptions.cursordown \
            $mapoptions.cursordownright $mapoptions.clickmouse $mapoptions.zoomin \
            $mapoptions.zoomout $mapoptions.mapoptions $ioptions.fullscreenkey \
            $uioptions.resizefirst $uioptions.resizesecond $uioptions.resizethird \
            $uioptions.resizefourth]
         if {$key == "Control_L" || $key == "Control_R" || $key == "Alt_L" || \
            $key == "Alt_R" || $key == "Shift_L" || $key == "Shift_R"} {
            set specialkey [string range $key 0 [expr [string length $key] - 3]]
            return
         }
         if {[string length $key] == 1} {
            set key "[string tolower $key]"
         }
         if {$specialkey != {}} {
            set value "$specialkey-$key"
         } else {
            set value "$key"
         }
         foreach keyentry $fields {
            if {$keyentry != $field && [$keyentry get] == $value} {
               $field delete [expr [string length [$field get]] - 1] end
               return
            }
         }
         $field delete 0 end
         $field insert end $value
         set specialkey {}
      }
      set moveoptions [ttk::frame $optionsframe.movement]
      grid [ttk::label $moveoptions.lbl1 -text {Move ship up/left:}] -sticky w
      tooltip::tooltip $moveoptions.lbl1 \
         "Key used to move ship up and left. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $moveoptions.upleft -width 15] -row 0 -column 1 -sticky w
      tooltip::tooltip $moveoptions.upleft \
         "Key used to move ship up and left. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $moveoptions.upleft <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $moveoptions.lbl2 -text {Move ship up:}] -sticky w
      tooltip::tooltip $moveoptions.lbl2 \
         "Key used to move ship up. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $moveoptions.up -width 15] -row 1 -column 1 -sticky w
      tooltip::tooltip $moveoptions.up \
         "Key used to move ship up. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $moveoptions.up <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $moveoptions.lbl3 -text {Move ship up/right:}] -sticky w
      tooltip::tooltip $moveoptions.lbl3 \
         "Key used to move ship up and right. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $moveoptions.upright -width 15] -row 2 -column 1 -sticky w
      tooltip::tooltip $moveoptions.upright \
         "Key used to move ship up and right. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $moveoptions.upright <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $moveoptions.lbl4 -text {Move ship left:}] -sticky w
      tooltip::tooltip $moveoptions.lbl4 \
         "Key used to move ship left. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $moveoptions.left -width 15] -row 3 -column 1 -sticky w
      tooltip::tooltip $moveoptions.left \
         "Key used to move ship left. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $moveoptions.left <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $moveoptions.lbl5 -text {Wait in place or move 1 field:}] \
         -sticky w
      tooltip::tooltip $moveoptions.lbl5 \
         "Key used to wait 1 minute or move 1 field. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $moveoptions.wait -width 15] -row 4 -column 1 -sticky w
      tooltip::tooltip $moveoptions.wait \
         "Key used to wait 1 minute or move 1 field. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $moveoptions.wait <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $moveoptions.lbl6 -text {Move ship right:}] -sticky w
      tooltip::tooltip $moveoptions.lbl6 \
         "Key used to move ship right. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $moveoptions.right -width 15] -row 5 -column 1 -sticky w
      tooltip::tooltip $moveoptions.right \
         "Key used to move ship right. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $moveoptions.right <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $moveoptions.lbl7 -text {Move ship down/left:}] -sticky w
      tooltip::tooltip $moveoptions.lbl7 \
         "Key used to move ship down and left. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $moveoptions.downleft -width 15] -row 6 -column 1 -sticky w
      tooltip::tooltip $moveoptions.downleft \
         "Key used to move ship down and left. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $moveoptions.downleft <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $moveoptions.lbl8 -text {Move ship down:}] -sticky w
      tooltip::tooltip $moveoptions.lbl8 \
         "Key used to move ship down. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $moveoptions.down -width 15] -row 7 -column 1 -sticky w
      tooltip::tooltip $moveoptions.down \
         "Key used to move ship up and left. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $moveoptions.down <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $moveoptions.lbl9 -text {Move ship down/right:}] -sticky w
      tooltip::tooltip $moveoptions.lbl9 \
         "Key used to move ship down and right. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $moveoptions.downright -width 15] -row 8 -column 1 -sticky w
      tooltip::tooltip $moveoptions.downright \
         "Key used to move ship down and right. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $moveoptions.downright <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $moveoptions.lbl10 -text {Move ship to destination:}] \
         -sticky w
      tooltip::tooltip $moveoptions.lbl10 \
         "Key used to move ship its destination. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $moveoptions.moveto -width 15] -row 9 -column 1 -sticky w
      tooltip::tooltip $moveoptions.moveto \
         "Key used to move ship its destination. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $moveoptions.moveto <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $moveoptions.lbl11 -text {Set full stop for ship:}] -sticky w
      tooltip::tooltip $moveoptions.lbl11 \
         "Key used to set full stop for the ship. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $moveoptions.fullstop -width 15] -row 10 -column 1 -sticky w
      tooltip::tooltip $moveoptions.fullstop \
         "Key used to set full stop for the ship. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $moveoptions.fullstop <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $moveoptions.lbl12 -text {Set quarter speed for ship:}] \
         -sticky w
      tooltip::tooltip $moveoptions.lbl12 \
         "Key used to set quarter speed for the ship. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $moveoptions.quarterspeed -width 15] -row 11 -column 1 \
         -sticky w
      tooltip::tooltip $moveoptions.quarterspeed \
         "Key used to set quarter speed for the ship. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $moveoptions.quarterspeed <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $moveoptions.lbl13 -text {Set half speed for ship:}] -sticky w
      tooltip::tooltip $moveoptions.lbl13 \
         "Key used to set half speed for the ship. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $moveoptions.halfspeed -width 15] -row 12 -column 1 -sticky w
      tooltip::tooltip $moveoptions.halfspeed \
         "Key used to set half speed for the ship. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $moveoptions.halfspeed <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $moveoptions.lbl14 -text {Set full speed for ship:}] \
         -sticky w
      tooltip::tooltip $moveoptions.lbl14 \
         "Key used to set full speed. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $moveoptions.fullspeed -width 15] -row 13 -column 1 -sticky w
      tooltip::tooltip $moveoptions.fullspeed \
         "Key used to set full speed for the ship. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $moveoptions.fullspeed <KeyRelease> {SetShortcut %W %K}
      grid [ttk::button $moveoptions.reset -text {Reset movement keys to default} \
         -command {ResetKeys movement}] -row 14 -columnspan 2 -sticky w
      tooltip::tooltip $moveoptions.reset \
         "Reset all movement keys to the default settings"
      SetScrollbarBindings $moveoptions .gameframe.paned.optionsframe.scrolly
      for {set i 1} {$i < 15} {incr i} {
         SetScrollbarBindings $moveoptions.lbl$i \
            .gameframe.paned.optionsframe.scrolly
      }""")
    tclEval(script = """
      # Menu keys options
      set menuoptions [ttk::frame $optionsframe.menu]
      grid [ttk::label $menuoptions.lbl1 -text {Ship information:}] -sticky w
      tooltip::tooltip $menuoptions.lbl1 \
         "Key used to show ship info screen. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $menuoptions.shipinfo -width 15] -row 0 -column 1 -sticky w
      tooltip::tooltip $menuoptions.shipinfo \
         "Key used to show ship info screen. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $menuoptions.shipinfo <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $menuoptions.lbl2 -text {Ship orders:}] -sticky w
      tooltip::tooltip $menuoptions.lbl2 \
         "Key used to show ship orders menu. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $menuoptions.orders -width 15] -row 1 -column 1 -sticky w
      tooltip::tooltip $menuoptions.orders \
         "Key used to show ship orders menu. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $menuoptions.orders <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $menuoptions.lbl3 -text {Crafting orders:}] -sticky w
      tooltip::tooltip $menuoptions.lbl3 \
         "Key used to show crafting screen. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $menuoptions.crafts -width 15] -row 2 -column 1 -sticky w
      tooltip::tooltip $menuoptions.crafts \
         "Key used to show crafting screen. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $menuoptions.crafts <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $menuoptions.lbl4 -text {Last messages:}] -sticky w
      tooltip::tooltip $menuoptions.lbl4 \
         "Key used to show messages screen. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $menuoptions.messages -width 15] -row 3 -column 1 -sticky w
      tooltip::tooltip $menuoptions.messages \
         "Key used to show messages screen. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $menuoptions.messages <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $menuoptions.lbl5 -text {Knowledge lists:}] -sticky w
      tooltip::tooltip $menuoptions.lbl5 \
         "Key used to show knowledge screen. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $menuoptions.knowledge -width 15] -row 4 -column 1 -sticky w
      tooltip::tooltip $menuoptions.knowledge \
         "Key used to show knowledge screen. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $menuoptions.knowledge <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $menuoptions.lbl6 -text {Wait orders:}] -sticky w
      tooltip::tooltip $menuoptions.lbl6 \
         "Key used to show wait orders menu. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $menuoptions.waitorders -width 15] -row 5 -column 1 -sticky w
      tooltip::tooltip $menuoptions.waitorders \
         "Key used to show wait orders. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $menuoptions.waitorders <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $menuoptions.lbl7 -text {Game statistics:}] -sticky w
      tooltip::tooltip $menuoptions.lbl7 \
         "Key used to show game statistics screen. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $menuoptions.gamestats -width 15] -row 6 -column 1 -sticky w
      tooltip::tooltip $menuoptions.gamestats \
         "Key used to show game statistics screen. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $menuoptions.gamestats <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $menuoptions.lbl8 -text {Help:}] -sticky w
      tooltip::tooltip $menuoptions.lbl8 \
         "Key used to show help window. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $menuoptions.help -width 15] -row 7 -column 1 -sticky w
      tooltip::tooltip $menuoptions.help \
         "Key used to show help window. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $menuoptions.help <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $menuoptions.lbl9 -text {Game options:}] -sticky w
      tooltip::tooltip $menuoptions.lbl9 \
         "Key used to show game options screen. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $menuoptions.gameoptions -width 15] -row 8 -column 1 -sticky w
      tooltip::tooltip $menuoptions.gameoptions \
         "Key used to show game options screen. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $menuoptions.gameoptions <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $menuoptions.lbl10 -text {Quit from game:}] -sticky w
      tooltip::tooltip $menuoptions.lbl10 \
         "Key used to quit from the game. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $menuoptions.quit -width 15] -row 9 -column 1 -sticky w
      tooltip::tooltip $menuoptions.quit \
         "Key used to quit from the game. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $menuoptions.quit <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $menuoptions.lbl11 -text {Resign from game:}] -sticky w
      tooltip::tooltip $menuoptions.lbl11 \
         "Key used to resign from the game. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $menuoptions.resign -width 15] -row 10 -column 1 -sticky w
      tooltip::tooltip $menuoptions.resign \
         "Key used to resign from the game. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $menuoptions.resign <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $menuoptions.lbl12 -text {Show menu:}] -sticky w
      tooltip::tooltip $menuoptions.lbl12 \
         "Key used to show main menu. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $menuoptions.menu -width 15] -row 11 -column 1 -sticky w
      tooltip::tooltip $menuoptions.menu \
         "Key used to show main menu. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $menuoptions.menu <KeyRelease> {SetShortcut %W %K}
      grid [ttk::button $menuoptions.reset -text {Reset menu keys to default} \
         -command {ResetKeys menu}] -row 12 -columnspan 2 -sticky w
      tooltip::tooltip $menuoptions.reset \
         "Reset all menu keys to the default settings"
      SetScrollbarBindings $menuoptions .gameframe.paned.optionsframe.scrolly
      for {set i 1} {$i < 13} {incr i} {
         SetScrollbarBindings $menuoptions.lbl$i \
            .gameframe.paned.optionsframe.scrolly
      }""")
    tclEval(script = """
      # Map keys options
      set mapoptions [ttk::frame $optionsframe.map]
      grid [ttk::label $mapoptions.lbl1 -text {Center map on player ship:}] \
         -sticky w
      tooltip::tooltip $mapoptions.lbl1 \
         "Key used to center map on player ship. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $mapoptions.center -width 15] -row 0 -column 1 -sticky w
      tooltip::tooltip $mapoptions.center \
         "Key used to center map on player ship. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $mapoptions.center <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $mapoptions.lbl2 -text {Center map on home base:}] -sticky w
      tooltip::tooltip $mapoptions.lbl2 \
         "Key used to center map on home base. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $mapoptions.centerhomebase -width 15] -row 1 -column 1 \
         -sticky w
      tooltip::tooltip $mapoptions.centerhomebase \
         "Key used to center map on home base. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $mapoptions.centerhomebase <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $mapoptions.lbl3 -text {Move map to left:}] -sticky w
      tooltip::tooltip $mapoptions.lbl3 \
         "Key used to move map left. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $mapoptions.mapleft -width 15] -row 2 -column 1 -sticky w
      tooltip::tooltip $mapoptions.mapleft \
         "Key used to move map left. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $mapoptions.mapleft <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $mapoptions.lbl4 -text {Move map to right:}] -sticky w
      tooltip::tooltip $mapoptions.lbl4 \
         "Key used to move map right. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $mapoptions.mapright -width 15] -row 3 -column 1 -sticky w
      tooltip::tooltip $mapoptions.mapright \
         "Key used to move map right. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $mapoptions.mapright <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $mapoptions.lbl5 -text {Move map up:}] -sticky w
      tooltip::tooltip $mapoptions.lbl5 \
         "Key used to move map up. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $mapoptions.mapup -width 15] -row 4 -column 1 -sticky w
      tooltip::tooltip $mapoptions.mapup \
         "Key used to move map up. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $mapoptions.mapup <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $mapoptions.lbl6 -text {Move map down:}] -sticky w
      tooltip::tooltip $mapoptions.lbl6 \
         "Key used to move map down. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $mapoptions.mapdown -width 15] -row 5 -column 1 -sticky w
      tooltip::tooltip $mapoptions.mapdown \
         "Key used to move map down. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $mapoptions.mapdown <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $mapoptions.lbl7 -text {Move map up/left:}] -sticky w
      tooltip::tooltip $mapoptions.lbl7 \
         "Key used to move map up and left. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $mapoptions.mapupleft -width 15] -row 6 -column 1 -sticky w
      tooltip::tooltip $mapoptions.mapupleft \
         "Key used to move map up and left. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $mapoptions.mapupleft <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $mapoptions.lbl8 -text {Move map up/right:}] -sticky w
      tooltip::tooltip $mapoptions.lbl8 \
         "Key used to move map up and right. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $mapoptions.mapupright -width 15] -row 7 -column 1 -sticky w
      tooltip::tooltip $mapoptions.mapupright \
         "Key used to move map up and right. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $mapoptions.mapupright <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $mapoptions.lbl9 -text {Move map down/left:}] -sticky w
      tooltip::tooltip $mapoptions.lbl9 \
         "Key used to move map down and left. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $mapoptions.mapdownleft -width 15] -row 8 -column 1 -sticky w
      tooltip::tooltip $mapoptions.mapdownleft \
         "Key used to move map down and left. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $mapoptions.mapdownleft <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $mapoptions.lbl10 -text {Move map down/right:}] -sticky w
      tooltip::tooltip $mapoptions.lbl10 \
         "Key used to move map down and right. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $mapoptions.mapdownright -width 15] -row 9 -column 1 -sticky w
      tooltip::tooltip $mapoptions.mapdownright \
         "Key used to move map down and right. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $mapoptions.mapdownright <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $mapoptions.lbl11 -text {Move cursor up/left:}] -sticky w
      tooltip::tooltip $mapoptions.lbl11 \
         "Key used to move cursor up and left. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $mapoptions.cursorupleft -width 15] -row 10 -column 1 \
         -sticky w
      tooltip::tooltip $mapoptions.cursorupleft \
         "Key used to move cursor up and left. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $mapoptions.cursorupleft <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $mapoptions.lbl12 -text {Move cursor up:}] -sticky w
      tooltip::tooltip $mapoptions.lbl12 \
         "Key used to move cursor up. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $mapoptions.cursorup -width 15] -row 11 -column 1 -sticky w
      tooltip::tooltip $mapoptions.cursorup \
         "Key used to move cursor up. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $mapoptions.cursorup <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $mapoptions.lbl13 -text {Move cursor up/right:}] -sticky w
      tooltip::tooltip $mapoptions.lbl13 \
         "Key used to move cursor up and right. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $mapoptions.cursorupright -width 15] -row 12 -column 1 \
         -sticky w
      tooltip::tooltip $mapoptions.cursorupright \
         "Key used to move cursor up and right. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $mapoptions.cursorupright <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $mapoptions.lbl14 -text {Move cursor left:}] -sticky w
      tooltip::tooltip $mapoptions.lbl14 \
         "Key used to move cursor left. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $mapoptions.cursorleft -width 15] -row 13 -column 1 -sticky w
      tooltip::tooltip $mapoptions.cursorleft \
         "Key used to move cursor left. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $mapoptions.cursorleft <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $mapoptions.lbl15 -text {Move cursor right:}] -sticky w
      tooltip::tooltip $mapoptions.lbl15 \
         "Key used to move cursor right. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $mapoptions.cursorright -width 15] -row 14 -column 1 \
         -sticky w
      tooltip::tooltip $mapoptions.cursorright \
         "Key used to move cursor right. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $mapoptions.cursorright <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $mapoptions.lbl16 -text {Move cursor down/left:}] -sticky w
      tooltip::tooltip $mapoptions.lbl16 \
         "Key used to move cursor down left. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $mapoptions.cursordownleft -width 15] -row 15 -column 1 \
         -sticky w
      tooltip::tooltip $mapoptions.cursordownleft \
         "Key used to move cursor down left. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $mapoptions.cursordownleft <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $mapoptions.lbl17 -text {Move cursor down:}] -sticky w
      tooltip::tooltip $mapoptions.lbl17 \
         "Key used to move cursor down. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $mapoptions.cursordown -width 15] -row 16 -column 1 -sticky w
      tooltip::tooltip $mapoptions.cursordown \
         "Key used to move cursor down. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $mapoptions.cursordown <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $mapoptions.lbl18 -text {Move cursor down/right:}] -sticky w
      tooltip::tooltip $mapoptions.lbl18 \
         "Key used to move cursor down and right. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $mapoptions.cursordownright -width 15] -row 17 -column 1 \
         -sticky w
      tooltip::tooltip $mapoptions.cursordownright \
         "Key used to move cursor down and right. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $mapoptions.cursordownright <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $mapoptions.lbl19 -text {Press mouse button:}] -sticky w
      tooltip::tooltip $mapoptions.lbl19 \
         "Key used to emulate mouse button. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $mapoptions.clickmouse -width 15] -row 18 -column 1 -sticky w
      tooltip::tooltip $mapoptions.clickmouse \
         "Key used to emulate mouse button. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $mapoptions.clickmouse <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $mapoptions.lbl20 -text {Zoom in map:}] -sticky w
      tooltip::tooltip $mapoptions.lbl20 \
         "Key used to zoom in map. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $mapoptions.zoomin -width 15] -row 19 -column 1 -sticky w
      tooltip::tooltip $mapoptions.zoomin \
         "Key used to zoom in map. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $mapoptions.zoomin <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $mapoptions.lbl21 -text {Zoom out map:}] -sticky w
      tooltip::tooltip $mapoptions.lbl21 \
         "Key used to zoom out map. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $mapoptions.zoomout -width 15] -row 20 -column 1 -sticky w
      tooltip::tooltip $mapoptions.zoomout \
         "Key used to zoom out map. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $mapoptions.zoomout <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $mapoptions.lbl22 -text {Show move map options:}] -sticky w
      tooltip::tooltip $mapoptions.lbl22 \
         "Key used to show move map options. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      grid [ttk::entry $mapoptions.mapoptions -width 15] -row 21 -column 1 -sticky w
      tooltip::tooltip $mapoptions.mapoptions \
         "Key used to show move map options. Select the field\nand press the desired key. To use special key, press it\nthe first then the desired key"
      bind $mapoptions.mapoptions <KeyRelease> {SetShortcut %W %K}
      grid [ttk::button $mapoptions.reset -text {Reset map keys to default} \
         -command {ResetKeys map}] -row 22 -columnspan 2 -sticky w
      tooltip::tooltip $menuoptions.reset \
         "Reset all map keys to the default settings"
      SetScrollbarBindings $mapoptions .gameframe.paned.optionsframe.scrolly
      for {set i 1} {$i < 23} {incr i} {
         SetScrollbarBindings $mapoptions.lbl$i .gameframe.paned.optionsframe.scrolly
      }""")
    tclEval(script = """
      # General keys options
      set uioptions [ttk::frame $optionsframe.ui]
      grid [ttk::label $uioptions.lbl1 -text {Resize first section:}] -sticky w
      tooltip::tooltip $uioptions.lbl1 \
         "Key used to resize (maximize or minimize) the first section\nof information (like ship info, knowledge or in combat).\nSelect the field and press the desired key. To use special key,\npress it the first then the desired key"
      grid [ttk::entry $uioptions.resizefirst -width 15] -row 0 -column 1 -sticky w
      tooltip::tooltip $uioptions.resizefirst \
         "Key used to resize (maximize or minimize) the first section\nof information (like ship info, knowledge or in combat).\nSelect the field and press the desired key. To use special key,\npress it the first then the desired key"
      bind $uioptions.resizefirst <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $uioptions.lbl2 -text {Resize second section:}] -sticky w
      tooltip::tooltip $uioptions.lbl2 \
         "Key used to resize (maximize or minimize) the second section\nof information (like ship info, knowledge or in combat).\nSelect the field and press the desired key. To use special key,\npress it the first then the desired key"
      grid [ttk::entry $uioptions.resizesecond -width 15] -row 1 -column 1 -sticky w
      tooltip::tooltip $uioptions.resizesecond \
         "Key used to resize (maximize or minimize) the second section\nof information (like ship info, knowledge or in combat).\nSelect the field and press the desired key. To use special key,\npress it the first then the desired key"
      bind $uioptions.resizesecond <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $uioptions.lbl3 -text {Resize third section:}] -sticky w
      tooltip::tooltip $uioptions.lbl3 \
         "Key used to resize (maximize or minimize) the third section\nof information (like ship info, knowledge or in combat).\nSelect the field and press the desired key. To use special key,\npress it the first then the desired key"
      grid [ttk::entry $uioptions.resizethird -width 15] -row 2 -column 1 -sticky w
      tooltip::tooltip $uioptions.resizethird \
         "Key used to resize (maximize or minimize) the third section\nof information (like ship info, knowledge or in combat).\nSelect the field and press the desired key. To use special key,\npress it the first then the desired key"
      bind $uioptions.resizethird <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $uioptions.lbl4 -text {Resize fourth section:}] -sticky w
      tooltip::tooltip $uioptions.lbl4 \
         "Key used to resize (maximize or minimize) the fourth section\nof information (like ship info, knowledge or in combat).\nSelect the field and press the desired key. To use special key,\npress it the first then the desired key"
      grid [ttk::entry $uioptions.resizefourth -width 15] -row 3 -column 1 -sticky w
      tooltip::tooltip $uioptions.resizefourth \
         "Key used to resize (maximize or minimize) the fourth section\nof information (like ship info, knowledge or in combat).\nSelect the field and press the desired key. To use special key,\npress it the first then the desired key"
      bind $uioptions.resizefourth <KeyRelease> {SetShortcut %W %K}
      grid [ttk::button $uioptions.reset -text {Reset general keys to default} \
         -command {ResetKeys general}] -row 12 -columnspan 2 -sticky w
      tooltip::tooltip $uioptions.reset \
         "Reset all general keys to the default settings"
      SetScrollbarBindings $uioptions .gameframe.paned.optionsframe.scrolly
      for {set i 1} {$i < 5} {incr i} {
         SetScrollbarBindings $uioptions.lbl$i \
            .gameframe.paned.optionsframe.scrolly
      }
      # Interface options
      set ioptions [ttk::frame $optionsframe.interface]
      grid [ttk::label $ioptions.lbl1 -text {Interface theme:}] -sticky w
      tooltip::tooltip $ioptions.lbl1 {Select UI theme.}
      grid [ttk::combobox $ioptions.theme -state readonly -width 15] -row 0 \
         -column 1 -sticky w
      tooltip::tooltip $ioptions.theme {Select UI theme.}
      grid [ttk::label $ioptions.lbl2 -text {Use right mouse button:}] -sticky w
      tooltip::tooltip $ioptions.lbl2 \
         {Use right mouse button to show various menus in the game.}
      grid [ttk::checkbutton $ioptions.rightbutton] -row 1 -column 1 -sticky w
      tooltip::tooltip $ioptions.rightbutton \
         {Use right mouse button to show various menus in the game.}
      grid [ttk::label $ioptions.lbl3 -text {Show tooltips:}] -sticky w
      tooltip::tooltip $ioptions.lbl3 \
         {Show help tooltips for various game elements.}
      grid [ttk::checkbutton $ioptions.showtooltips] -row 2 -column 1 -sticky w
      tooltip::tooltip $ioptions.showtooltips \
         {Show help tooltips for various game elements.}
      grid [ttk::label $ioptions.lbl4 -text {Show last messages:}] -sticky w
      tooltip::tooltip $ioptions.lbl4 \
         {Show last messages window in every place in the game.}
      grid [ttk::checkbutton $ioptions.showmessages] -row 3 -column 1 -sticky w
      tooltip::tooltip $ioptions.showmessages \
         {Show last messages window in every place in the game.}
      grid [ttk::label $ioptions.lbl5 -text {Full screen mode:}] -sticky w
      tooltip::tooltip $ioptions.lbl5 {Run the game in full screen mode.}
      grid [ttk::checkbutton $ioptions.fullscreen] -row 4 -column 1 -sticky w
      tooltip::tooltip $ioptions.fullscreen {Run the game in full screen mode.}
      grid [ttk::label $ioptions.lbl6 -text {Full screen shortcut:}] -sticky w
      tooltip::tooltip $ioptions.lbl6 {Key used to switch full screen mode.}
      grid [ttk::entry $ioptions.fullscreenkey -width 15] -row 5 -column 1 -sticky w
      tooltip::tooltip $ioptions.fullscreenkey \
         {Key used to switch full screen mode.}
      bind $ioptions.fullscreenkey <KeyRelease> {SetShortcut %W %K}
      grid [ttk::label $ioptions.lbl7 -text {Close messages after:}] -sticky w
      tooltip::tooltip $ioptions.lbl7 \
         {Auto close game messages after that amount of seconds.}
      grid [ttk::spinbox $ioptions.closemessages -from 1 -to 60 -validate key \
         -validatecommand {ValidateSpinbox %W %P {}} -width 5] -row 6 -column 1 \
         -sticky w
      tooltip::tooltip $ioptions.closemessages \
         {Auto close game messages after that amount of seconds.}
      grid [ttk::label $ioptions.lbl8 -text {Show numeric values:}] -sticky w
      tooltip::tooltip $ioptions.lbl8 \
         "Show numeric values of many statistics, like crew\nabilities, weapons strength, etc."
      grid [ttk::checkbutton $ioptions.shownumbers] -row 7 -column 1 -sticky w
      tooltip::tooltip $ioptions.shownumbers \
         "Show numeric values of many statistics, like crew\nabilities, weapons strength, etc."
      grid [ttk::label $ioptions.lbl12 -text {Amount items on lists:}] -sticky w
      tooltip::tooltip $ioptions.lbl12 \
         "The amount of items displayed on various lists in\nthe game like crew members, modules, etc."
      grid [ttk::spinbox $ioptions.listslimit -from 5 -to 100 -validate key \
         -validatecommand {ValidateSpinbox %W %P {}} -width 5] -row 8 -column 1 \
         -sticky w
      tooltip::tooltip $ioptions.listslimit \
         "The amount of items displayed on various lists in\nthe game like crew members, modules, etc."
      grid [ttk::label $ioptions.lbl9 -text {Size of map font:}] -sticky w
      tooltip::tooltip $ioptions.lbl9 \
         {Size (in pixels) of font used to draw game map.}
      grid [ttk::spinbox $ioptions.mapfont -from 3 -to 50 -validate key \
         -validatecommand {ValidateSpinbox %W %P {}} -width 5] -row 9 -column 1 \
         -sticky w
      bind $ioptions.mapfont <FocusOut> {SetFonts %W}
      tooltip::tooltip $ioptions.mapfont \
         {Size (in pixels) of font used to draw game map.}
      grid [ttk::label $ioptions.lbl10 -text {Size of help font:}] -sticky w
      tooltip::tooltip $ioptions.lbl10 \
         {Size (in pixels) of font used mainly in help.}
      grid [ttk::spinbox $ioptions.helpfont -from 3 -to 50 -validate key \
         -validatecommand {ValidateSpinbox %W %P {}} -width 5] -row 10 -column 1 \
         -sticky w
      bind $ioptions.helpfont <FocusOut> {SetFonts %W}
      tooltip::tooltip $ioptions.helpfont \
         {Size (in pixels) of font used mainly in help.}
      grid [ttk::label $ioptions.lbl11 -text {Size of interface font:}] -sticky w
      tooltip::tooltip $ioptions.lbl11 \
         {Size (in pixels) of font used in interface (for example, here).}
      grid [ttk::spinbox $ioptions.interfacefont -from 3 -to 50 -validate key \
         -validatecommand {ValidateSpinbox %W %P {}} -width 5] -row 11 -column 1 \
         -sticky w
      bind $ioptions.interfacefont <FocusOut> {SetFonts %W}
      tooltip::tooltip $ioptions.interfacefont \
         {Size (in pixels) of font used in interface (for example, here).}
      grid [ttk::button $ioptions.setdefault -text {Set default size for fonts} \
         -command SetDefaultFonts] -columnspan 2
      SetScrollbarBindings $ioptions .gameframe.paned.optionsframe.scrolly
      for {set i 1} {$i < 13} {incr i} {
         SetScrollbarBindings $ioptions.lbl$i .gameframe.paned.optionsframe.scrolly
      }""")
    tclEval(script = """
      # Info options
      set infooptions [ttk::frame $optionsframe.info]
      grid [ttk::label $infooptions.lbl1 -text {Data directory path:}] -sticky w
      tooltip::tooltip $infooptions.lbl1 {Place where all standard game data are.}
      grid [ttk::label $infooptions.data -wraplength 500] -row 0 -column 1 -sticky w
      tooltip::tooltip $infooptions.data {Place where all standard game data are.}
      grid [ttk::label $infooptions.lbl2 -text {Save directory path:}] -sticky w
      tooltip::tooltip $infooptions.lbl2 \
         {Place where all game saves and configuration files are.}
      grid [ttk::label $infooptions.save -wraplength 500] -row 1 -column 1 -sticky w
      tooltip::tooltip $infooptions.save \
         {Place where all game saves and configuration files are.}
      grid [ttk::label $infooptions.lbl3 -text {Documentation directory path:}] \
         -sticky w
      tooltip::tooltip $infooptions.lbl3 {Place where are game documentation files.}
      grid [ttk::label $infooptions.docs -wraplength 500] -row 2 -column 1 -sticky w
      tooltip::tooltip $infooptions.docs {Place where are game documentation files.}
      grid [ttk::label $infooptions.lbl4 -text {Modifications directory path:}] \
         -sticky w
      tooltip::tooltip $infooptions.lbl4 \
         {Place where you should put all modifications files.}
      grid [ttk::label $infooptions.mods -wraplength 500] -row 3 -column 1 -sticky w
      tooltip::tooltip $infooptions.mods \
         {Place where you should put all modifications files.}
      SetScrollbarBindings $infooptions .gameframe.paned.optionsframe.scrolly
      for {set i 1} {$i < 5} {incr i} {
         SetScrollbarBindings $infooptions.lbl$i \
            .gameframe.paned.optionsframe.scrolly
      }
      SetScrollbarBindings $infooptions.data .gameframe.paned.optionsframe.scrolly
      SetScrollbarBindings $infooptions.save .gameframe.paned.optionsframe.scrolly
      SetScrollbarBindings $infooptions.docs .gameframe.paned.optionsframe.scrolly
      SetScrollbarBindings $infooptions.mods .gameframe.paned.optionsframe.scrolly
      ::autoscroll::autoscroll .gameframe.paned.optionsframe.scrolly
      ::autoscroll::autoscroll .gameframe.paned.optionsframe.scrollx
    """)
    tclEval(script = "bind " & optionsFrame & " <Configure> {ResizeCanvas %W.canvas %w %h}")
    let labelsArray: array[4, WidgetData] = [WidgetData(name: "data",
        value: dataDirectory.string), WidgetData(name: "save", value: saveDirectory.string),
        WidgetData(name: "docs", value: docDirectory.string), WidgetData(name: "mods",
        value: modsDirectory.string)]
    for label in labelsArray:
      let labelName: string = optionsCanvas & ".options.info." & label.name
      tclEval(script = labelName & " configure -text {" & label.value & " }")
    var themesNames: string = ""
    for theme in themesList.values:
      themesNames &= " {" & theme.name & "}"
    let comboBox: string = optionsFrame & ".canvas.options.interface.theme"
    tclEval(script = comboBox & " configure -values [list" & themesNames & "]")
  elif tclEval2(script = "winfo ismapped " & optionsCanvas) == "1":
    tclEval(script = "grid remove " & closeButton)
    showSkyMap(clear = true)
    return tclOk
  optionsFrame = optionsCanvas & ".options.general"
  tclEval(script = "grid " & optionsFrame & " -sticky nwes -padx 10")
  let checkboxArray: array[12, WidgetData] = [WidgetData(name: optionsCanvas &
      ".options.general.autorest", value: (
      if gameSettings.autoRest: "1" else: "0")), WidgetData(
      name: optionsCanvas & ".options.general.autocenter", value: (
      if gameSettings.autoCenter: "1" else: "0")), WidgetData(
      name: optionsCanvas & ".options.general.autoreturn", value: (
      if gameSettings.autoReturn: "1" else: "0")), WidgetData(
      name: optionsCanvas & ".options.general.autodestination", value:
    (if gameSettings.autoDestination: "1" else: "0")), WidgetData(
    name: optionsCanvas & ".options.general.autofinish", value: (
    if gameSettings.autoFinish: "1" else: "0")), WidgetData(
    name: optionsCanvas & ".options.general.autoaskforbases", value: (
    if gameSettings.autoAskForBases: "1" else: "0")), WidgetData(
    name: optionsCanvas & ".options.general.autoaskforevents", value: (
    if gameSettings.autoAskForEvents: "1" else: "0")), WidgetData(
    name: optionsCanvas & ".options.interface.rightbutton", value: (
    if gameSettings.rightButton: "1" else: "0")), WidgetData(
    name: optionsCanvas & ".options.interface.showtooltips", value: (
    if gameSettings.showTooltips: "1" else: "0")), WidgetData(
    name: optionsCanvas & ".options.interface.showmessages", value: (
    if gameSettings.showLastMessages: "1" else: "0")), WidgetData(
    name: optionsCanvas & ".options.interface.fullscreen", value: (
    if gameSettings.fullScreen: "1" else: "0")), WidgetData(
    name: optionsCanvas & ".options.interface.shownumbers", value: (
    if gameSettings.showNumbers: "1" else: "0"))]
  for checkBox in checkboxArray:
    tclSetVar(varName = checkBox.name, newValue = checkBox.value)
  let spinboxArray: array[11, WidgetData] = [WidgetData(name: optionsCanvas &
      ".options.general.fuel", value: $gameSettings.lowFuel), WidgetData(
      name: optionsCanvas & ".options.general.drinks",
      value: $gameSettings.lowDrinks), WidgetData(name: optionsCanvas &
      ".options.general.food", value: $gameSettings.lowFood), WidgetData(
      name: optionsCanvas & ".options.general.messageslimit",
      value: $gameSettings.messagesLimit), WidgetData(name: optionsCanvas &
      ".options.general.savedmessages", value: $gameSettings.savedMessages),
      WidgetData(name: optionsCanvas & ".options.interface.closemessages",
      value: $gameSettings.autoCloseMessagesTime), WidgetData(
      name: optionsCanvas & ".options.interface.mapfont",
      value: $gameSettings.mapFontSize), WidgetData(name: optionsCanvas &
      ".options.interface.interfacefont",
      value: $gameSettings.interfaceFontSize), WidgetData(name: optionsCanvas &
      ".options.interface.helpfont", value: $gameSettings.helpFontSize),
      WidgetData(name: optionsCanvas & ".options.interface.listslimit",
      value: $gameSettings.listsLimit), WidgetData(name: optionsCanvas &
      ".options.general.waitinterval", value: $gameSettings.waitMinutes)]
  for spinBox in spinboxArray:
    tclEval(script = spinBox.name & " set " & spinBox.value)
  let comboboxArray: array[4, WidgetData] = [WidgetData(name: optionsCanvas &
      ".options.general.speed", value: $(gameSettings.undockSpeed.ord - 1)),
      WidgetData(name: optionsCanvas & ".options.general.automovestop",
      value: $(gameSettings.autoMoveStop.ord)), WidgetData(name: optionsCanvas &
      ".options.general.messagesorder", value: $(
      gameSettings.messagesOrder.ord)), WidgetData(name: optionsCanvas &
      ".options.general.autosave", value: $(gameSettings.autoSave.ord))]
  for comboBox in comboboxArray:
    tclEval(script = comboBox.name & " current " & comboBox.value)
  optionsFrame = optionsCanvas & ".options.interface"
  var comboBox: string = optionsFrame & ".theme"
  let theme: ThemeRecord = try:
        themesList[gameSettings.interfaceTheme]
      except:
        return showError(message = "Can't find theme '" &
            gameSettings.interfaceTheme & "'")
  tclEval(script = comboBox & " set {" & theme.name & "}")
  optionsFrame = optionsCanvas & ".options"
  for i in 0..10:
    accels[i].shortcut = menuAccelerators[i + 1]
  for i in 0..36:
    accels[i + 11].shortcut = mapAccelerators[i + 1]
  accels[11 + 37].shortcut = fullScreenAccel
  for i in 0..3:
    accels[i + 11 + 37 + 1].shortcut = generalAccelerators[i]
  for accel in accels:
    let keyEntry: string = optionsFrame & accel.entryName
    tclEval(script = keyEntry & " delete 0 end")
    tclEval(script = keyEntry & " insert 0 " & accel.shortcut)
  if tclEval2(script = closeButton & " cget -command") == "ShowCombatUI":
    tclEval(script = closeButton & " configure -command {CloseOptions combat}")
  else:
    tclEval(script = closeButton & " configure -command {CloseOptions map}")
  tclEval(script = "grid " & closeButton & " -row 0 -column 1")
  tclEval(script = optionsCanvas & " configure -height " & tclEval2(
      script = mainPaned & " cget -height") & " -width " & tclEval2(
      script = mainPaned & " cget -width"))
  tclEval(script = "update")
  tclEval(script = optionsCanvas & " create window 0 0 -anchor nw -window " & optionsFrame)
  tclEval(script = "update")
  tclEval(script = optionsCanvas & " configure -scrollregion [list " & tclEval2(
      script = optionsCanvas & " bbox all") & "]")
  showScreen(newScreenName = "optionsframe")
  return showOptionsTabCommand(clientData = clientData, interp = interp,
      argc = argc, argv = argv)

proc setFontsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Set the selected font
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SetFonts fontfield
  ## Fontfield is the name of the spinbox which value changed.
  const frameName: string = ".gameframe.paned.optionsframe.canvas.options.interface"
  let
    spinBox: string = $argv[1]
    newSize: Positive = try:
        tclEval2(script = spinBox & " get").parseInt
      except:
        return showError(message = "Can't get the new size.")
  if spinBox == frameName & "mapfont":
    setFonts(newSize = newSize, fontType = mapFont)
  elif spinBox == frameName & ".helpfont":
    setFonts(newSize = newSize, fontType = helpFont)
  else:
    setFonts(newSize = newSize, fontType = interfaceFont)
  loadThemeImages()
  return tclOk

proc setDefaultFontsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [WriteIOEffect,
        TimeEffect, RootEffect], cdecl, contractual.} =
  ## Set the default values for fonts
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SetDefaultFonts
  const
    spinboxNames: array[3, string] = ["map", "interface", "help"]
    fontTypesNames: array[3, FontTypes] = [mapFont, interfaceFont, helpFont]
  for index, name in spinboxNames:
    let spinbox: string = ".gameframe.paned.optionsframe.canvas.options.interface." &
        name & "font"
    tclEval(script = spinbox & " set " & $defaultFontSizes[index])
    setFonts(newSize = defaultFontSizes[index], fontType = fontTypesNames[index])
  loadThemeImages()
  return tclOk

proc closeOptionsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    WriteIOEffect, TimeEffect, RootEffect, RootEffect], cdecl, contractual.} =
  ## Save all options and back to the map
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## CloseOptions oldscreen
  ## Oldscreen is name of the screen to which the game should return.
  ## Can be 'map' or 'combat'.
  tclEval(script = closeButton & " configure -command ShowSkyMap")
  tclEval(script = "grid remove " & closeButton)
  const rootName: string = ".gameframe.paned.optionsframe.canvas.options"

  proc getCheckboxValue(checkboxName: string): bool {.raises: [], tags: [],
      contractual.} =
    ## Get the state of the selected checkbox
    ##
    ## * checkboxName - the Tcl name of the checkbox
    ##
    ## Returns true if the checkbox is checked, otherwise false
    return tclGetVar(varName = rootName & checkboxName) == "1"

  gameSettings.autoRest = getCheckboxValue(checkboxName = ".general.autorest")

  proc getComboboxValue(comboboxName: string): Natural {.raises: [], tags: [
      RootEffect], contractual.} =
    ## Get the index of the value of the selected combobox
    ##
    ## * comboboxName - the Tcl name of the combobox
    ##
    ## Returns the index of the currently selected value in the combobox
    let comboBox: string = rootName & comboboxName
    try:
      return tclEval2(script = comboBox & " current").parseInt
    except:
      showError(message = "Can't get the value of the combo box.")
      return 0

  gameSettings.undockSpeed = try:
      (getComboboxValue(comboboxName = ".general.speed") + 1).ShipSpeed
    except:
      return showError(message = "Can't get undock speed.")
  gameSettings.autoCenter = getCheckboxValue(
      checkboxName = ".general.autocenter")
  gameSettings.autoReturn = getCheckboxValue(
      checkboxName = ".general.autoreturn")
  gameSettings.autoDestination = getCheckboxValue(
      checkboxName = ".general.autodestination")
  gameSettings.autoFinish = getCheckboxValue(
      checkboxName = ".general.autofinish")
  gameSettings.autoAskForBases = getCheckboxValue(
      checkboxName = ".general.autoaskforbases")
  gameSettings.autoAskForEvents = getCheckboxValue(
      checkboxName = ".general.autoaskforevents")

  proc getSpinboxValue(spinboxName: string): Natural {.raises: [], tags: [
      RootEffect], contractual.} =
    ## Get the the value of the selected spinbox
    ##
    ## * spinboxName - the Tcl name of the spinbox
    ##
    ## Returns the current value in the spinbox
    let spinBox: string = rootName & spinboxName
    try:
      return tclEval2(script = spinBox & " get").parseInt
    except:
      showError(message = "Can't get the value of the spin box.")
      return 0

  gameSettings.lowFuel = try:
      getSpinboxValue(spinboxName = ".general.fuel")
    except:
      return showError(message = "Can't get low fuel.")
  gameSettings.lowDrinks = try:
      getSpinboxValue(spinboxName = ".general.drinks")
    except:
      return showError(message = "Can't get low drinks.")
  gameSettings.lowFood = try:
      getSpinboxValue(spinboxName = ".general.food")
    except:
      return showError(message = "Can't get low food.")
  gameSettings.autoMoveStop = try:
      getComboboxValue(comboboxName = ".general.automovestop").AutoMoveBreak
    except:
      return showError(message = "Can't get low auto move stop.")
  gameSettings.messagesLimit = try:
      getSpinboxValue(spinboxName = ".general.messageslimit")
    except:
      return showError(message = "Can't get messages limit.")
  gameSettings.savedMessages = try:
      getSpinboxValue(spinboxName = ".general.savedmessages")
    except:
      return showError(message = "Can't get saved messages.")
  gameSettings.waitMinutes = try:
      getSpinboxValue(spinboxName = ".general.waitinterval")
    except:
      return showError(message = "Can't get wait minutes.")
  gameSettings.messagesOrder = try:
      getComboboxValue(comboboxName = ".general.messagesorder").MessagesOrder
    except:
      return showError(message = "Can't get messages order.")
  gameSettings.autoSave = try:
      getComboboxValue(comboboxName = ".general.autosave").AutoSaveTime
    except:
      return showError(message = "Can't get auto save.")
  let
    themeCombobox: string = rootName & ".interface.theme"
    themeName: string = tclEval2(script = themeCombobox & " get")
  for index, theme in themesList:
    if theme.name == $themeName:
      gameSettings.interfaceTheme = index
      break
  tclEval(script = "ttk::style theme use " & gameSettings.interfaceTheme)
  setTheme()
  const mapView: string = ".gameframe.paned.mapframe.map"
  if tclGetVar(varName = rootName & ".interface.rightbutton") == "1":
    gameSettings.rightButton = true
    tclEval(script = "bind " & mapView & " <Button-3> {ShowDestinationMenu %X %Y}")
    tclEval(script = "bind " & mapView & " <Button-1> {}")
  else:
    gameSettings.rightButton = false
    tclEval(script = "bind " & mapView & " <Button-1> {ShowDestinationMenu %X %Y}")
    tclEval(script = "bind " & mapView & " <Button-3> {}")
  if tclGetVar(varName = rootName & ".interface.showtooltips") == "1":
    gameSettings.showTooltips = true
    tclEval(script = "tooltip::tooltip enable")
  else:
    gameSettings.showTooltips = false
    tclEval(script = "tooltip::tooltip disable")
  gameSettings.showLastMessages = tclGetVar(varName = rootName &
      ".interface.showmessages") == "1"
  if tclGetVar(varName = rootName & ".interface.fullscreen") == "1":
    gameSettings.fullScreen = true
    tclEval(script = "wm attributes . -fullscreen 1")
  else:
    gameSettings.fullScreen = false
    tclEval(script = "wm attributes . -fullscreen 0")
  gameSettings.autoCloseMessagesTime = try:
      getSpinboxValue(spinboxName = ".interface.closemessages")
    except:
      return showError(message = "Can't get close messages time.")
  gameSettings.showNumbers = getCheckboxValue(
      checkboxName = ".interface.shownumbers")
  gameSettings.mapFontSize = try:
      getSpinboxValue(spinboxName = ".interface.mapfont")
    except:
      return showError(message = "Can't get map font size.")
  gameSettings.helpFontSize = try:
      getSpinboxValue(spinboxName = ".interface.helpfont")
    except:
      return showError(message = "Can't get help font size.")
  gameSettings.interfaceFontSize = try:
      getSpinboxValue(spinboxName = ".interface.interfacefont")
    except:
      return showError(message = "Can't get interface font size.")
  gameSettings.listsLimit = try:
      getSpinboxValue(spinboxName = ".interface.listslimit")
    except:
      return showError(message = "Can't get lists limit.")
  try:
    saveConfig()
  except:
    return showError(message = "Can't save configuration file.")
  for index, accel in accels.mpairs:
    var
      pos: int = accel.shortcut.rfind(sub = '-')
      keyName: string = ""
    if pos > -1:
      keyName = accel.shortcut[0..pos] & "KeyPress-" & accel.shortcut[pos +
          1..^1]
    else:
      keyName = "KeyPress-" & accel.shortcut
    tclEval(script = "bind . <" & keyName & "> {}")
    case index
    of 0..10:
      menuAccelerators[index + 1] = tclEval2(script = rootName &
          accel.entryName & " get")
      pos = menuAccelerators[index + 1].rfind(sub = '-')
      keyName = ""
      if pos > -1:
        keyName = menuAccelerators[index + 1][0 .. pos] & "KeyPress-" &
            menuAccelerators[index + 1][pos + 1..^1]
      else:
        keyName = "KeyPress-" & menuAccelerators[index + 1]
      tclEval(script = "bind . <" & keyName & "> {InvokeMenu " &
          menuAccelerators[index + 1] & "}")
    of 11..47:
      mapAccelerators[index - 10] = tclEval2(script = rootName &
          accel.entryName & " get")
    of 48:
      fullScreenAccel = tclEval2(script = rootName & accels[48].entryName & " get")
    else:
      generalAccelerators[index - 49] = tclEval2(script = rootName &
          accel.entryName & " get")
    accel.shortcut = tclEval2(script = rootName & accel.entryName & " get")
  let keyFile: File = try:
        open(filename = saveDirectory.string & "keys.cfg", mode = fmWrite)
      except:
        return showError(message = "Can't open keys configuration file.")
  for accel in accels:
    try:
      keyFile.writeLine(x = accel.configName & " = " & accel.shortcut)
    except:
      return showError(message = "Can't save keyboard accelerator.")
  keyFile.close
  setKeys()
  if argv[1] == "map":
    showSkyMap(clear = true)
  else:
    showCombatUi(newCombat = false)
  return tclOk

proc resetKeysCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], cdecl,
        contractual.} =
  ## Reset the selected group of keys to their default values
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ResetKeys group
  ## Group is the group of keys which will be resetted. Possible values are
  ## movement, map, menu
  case argv[1]
  of "movement":
    let defaultMovementAccels: array[14, AccelData] = [AccelData(
        shortcut: if DirSep == '\\': "Home" else: "KP_Home",
        entryName: ".movement.upleft", configName: ""), AccelData(
        shortcut: if DirSep == '\\': "Up" else: "KP_Up",
        entryName: ".movement.up", configName: ""), AccelData(
        shortcut: if DirSep == '\\': "Prior" else: "KP_Prior",
        entryName: ".movement.upright", configName: ""), AccelData(
        shortcut: if DirSep == '\\': "Left" else: "KP_Left",
        entryName: ".movement.left", configName: ""), AccelData(
        shortcut: if DirSep == '\\': "Clear" else: "KP_Begin",
        entryName: ".movement.wait", configName: ""), AccelData(
        shortcut: if DirSep == '\\': "Right" else: "KP_Right",
        entryName: ".movement.right", configName: ""), AccelData(
        shortcut: if DirSep == '\\': "End" else: "KP_End",
        entryName: ".movement.downleft", configName: ""), AccelData(
        shortcut: if DirSep == '\\': "Down" else: "KP_Down",
        entryName: ".movement.down", configName: ""), AccelData(
        shortcut: if DirSep == '\\': "Next" else: "KP_Next",
        entryName: ".movement.downright", configName: ""), AccelData(
        shortcut: if DirSep == '\\': "slash" else: "KP_Divide",
        entryName: ".movement.moveto", configName: ""), AccelData(
        shortcut: "Control-a", entryName: ".movement.fullstop", configName: ""),
        AccelData(shortcut: "Control-b", entryName: ".movement.quarterspeed",
        configName: ""), AccelData(shortcut: "Control-c",
        entryName: ".movement.halfspeed", configName: ""), AccelData(
        shortcut: "Control-d", entryName: ".movement.fullspeed",
        configName: "")]
    for accel in defaultMovementAccels:
      let keyEntry: string = ".gameframe.paned.optionsframe.canvas.options" &
          accel.entryName
      tclEval(script = keyEntry & " delete 0 end")
      tclEval(script = keyEntry & " insert 0 " & accel.shortcut)
  of "menu":
    const defaultMenuAccels: array[12, AccelData] = [AccelData(shortcut: "s",
        entryName: ".menu.shipinfo", configName: ""), AccelData(shortcut: "o",
        entryName: ".menu.orders", configName: ""), AccelData(shortcut: "r",
        entryName: ".menu.crafts", configName: ""), AccelData(shortcut: "m",
        entryName: ".menu.messages", configName: ""), AccelData(shortcut: "k",
        entryName: ".menu.knowledge", configName: ""), AccelData(shortcut: "w",
        entryName: ".menu.waitorders", configName: ""), AccelData(shortcut: "g",
        entryName: ".menu.gamestats", configName: ""), AccelData(shortcut: "F1",
        entryName: ".menu.help", configName: ""), AccelData(shortcut: "p",
        entryName: ".menu.gameoptions", configName: ""), AccelData(
        shortcut: "q", entryName: ".menu.quit", configName: ""), AccelData(
        shortcut: "x", entryName: ".menu.resign", configName: ""), AccelData(
        shortcut: "e", entryName: ".menu.menu", configName: "")]
    for accel in defaultMenuAccels:
      let keyEntry: string = ".gameframe.paned.optionsframe.canvas.options" &
          accel.entryName
      tclEval(script = keyEntry & " delete 0 end")
      tclEval(script = keyEntry & " insert 0 " & accel.shortcut)
  of "map":
    let defaultMapAccels: array[23, AccelData] = [AccelData(
        shortcut: "Shift-Return", entryName: ".map.center", configName: ""),
        AccelData(shortcut: "Shift-h", entryName: ".map.centerhomebase",
        configName: ""), AccelData(shortcut: "Shift-" & (if DirSep ==
        '\\': "Home" else: "KP_Home"), entryName: ".map.mapupleft",
        configName: ""), AccelData(shortcut: "Shift-" & (if DirSep ==
        '\\': "Up" else: "KP_Up"), entryName: "map.mapup", configName: ""),
        AccelData(shortcut: "Shift-" & (if DirSep ==
        '\\': "Prior" else: "KP_Prior"), entryName: ".map.mapupright",
        configName: ""), AccelData(shortcut: "Shift-" & (if DirSep ==
        '\\': "Left" else: "KP_Left"), entryName: ".map.mapleft",
        configName: ""),
        AccelData(shortcut: "Shift-" & (if DirSep ==
        '\\': "Right" else: "KP_Right"), entryName: ".mapmapright",
        configName: ""),
        AccelData(shortcut: "Shift-" & (if DirSep ==
            '\\': "End" else: "KP_End"),
        entryName: ".map.mapdownleft", configName: ""), AccelData(
        shortcut: "Shift-" & (if DirSep == '\\': "Down" else: "KP_Down"),
        entryName: ".map.mapdown", configName: ""), AccelData(
        shortcut: "Shift-" & (if DirSep == '\\': "Next" else: "KP_Next"),
        entryName: ".map.mapdownright", configName: ""), AccelData(
        shortcut: "Control-" & (if DirSep == '\\': "Home" else: "KP_Home"),
        entryName: ".map.cursorupleft", configName: ""), AccelData(
        shortcut: "Control-" & (if DirSep == '\\': "Up" else: "KP_Up"),
        entryName: ".map.cursorup", configName: ""), AccelData(
        shortcut: "Control-" & (if DirSep == '\\': "Prior" else: "KP_Prior"),
        entryName: ".map.cursorupright", configName: ""), AccelData(
        shortcut: "Control-" & (if DirSep == '\\': "Left" else: "KP_Left"),
        entryName: ".map.cursorleft", configName: ""), AccelData(
        shortcut: "Control-" & (if DirSep == '\\': "Right" else: "KP_Right"),
        entryName: ".map.cursorright", configName: ""), AccelData(
        shortcut: "Control-" & (if DirSep == '\\': "End" else: "KP_End"),
        entryName: ".map.cursordownleft", configName: ""), AccelData(
        shortcut: "Control-" & (if DirSep == '\\': "Down" else: "KP_Down"),
        entryName: ".map.cursordown", configName: ""), AccelData(
        shortcut: "Control-" & (if DirSep == '\\': "Next" else: "KP_Next"),
        entryName: ".map.cursordownright", configName: ""), AccelData(
        shortcut: "Control-" & (if DirSep == '\\': "Begin" else: "Return"),
        entryName: ".map.clickmouse", configName: ""), AccelData(
        shortcut: "Control-a", entryName: ".movement.fullstop", configName: ""),
        AccelData(shortcut: "Control-b", entryName: ".movement.quarterspeed",
        configName: ""), AccelData(shortcut: "Control-c",
        entryName: ".movement.halfspeed", configName: ""), AccelData(
        shortcut: "Control-d", entryName: ".movement.fullspeed",
        configName: "")]
    for accel in defaultMapAccels:
      let keyEntry: string = ".gameframe.paned.optionsframe.canvas.options" &
          accel.entryName
      tclEval(script = keyEntry & " delete 0 end")
      tclEval(script = keyEntry & " insert 0 " & accel.shortcut)
  of "general":
    const defaultGeneralAccels: array[4, AccelData] = [AccelData(
        shortcut: "Alt-a", entryName: ".ui.resizefirst", configName: ""),
        AccelData(shortcut: "Alt-b", entryName: ".ui.resizesecond",
        configName: ""), AccelData(shortcut: "Alt-c",
        entryName: ".ui.resizethird", configName: ""), AccelData(
        shortcut: "Alt-d", entryName: ".ui.resizefourth", configName: "")]
    for accel in defaultGeneralAccels:
      let keyEntry: string = ".gameframe.paned.optionsframe.canvas.options" &
          accel.entryName
      tclEval(script = keyEntry & " delete 0 end")
      tclEval(script = keyEntry & " insert 0 " & accel.shortcut)
  else:
    discard
  return tclOk

proc addCommands*() {.raises: [], tags: [WriteIOEffect, TimeEffect, RootEffect],
    contractual.} =
  ## Adds Tcl commands related to the crew UI
  try:
    addCommand(name = "ShowOptionsTab", nimProc = showOptionsTabCommand)
    addCommand(name = "ShowOptions", nimProc = showOptionsCommand)
    addCommand(name = "SetFonts", nimProc = setFontsCommand)
    addCommand(name = "SetDefaultFonts", nimProc = setDefaultFontsCommand)
    addCommand(name = "CloseOptions", nimProc = closeOptionsCommand)
    addCommand(name = "ResetKeys", nimProc = resetKeysCommand)
  except:
    showError(message = "Can't add a Tcl command.")
