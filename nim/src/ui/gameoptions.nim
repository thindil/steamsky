# Copyright 2024 Bartek thindil Jasicki
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

import std/[os, tables]
import ../[config, game, tk]
import coreui, mapsui, themes, utilsui2

proc showOptionsTabCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
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
    optionsCanvas = mainPaned & ".optionsframe.canvas"
    optionsFrame = optionsCanvas & ".options"
    oldFrame = tclEval2(script = "grid slaves " & optionsFrame & " -row 1")
  tclEval(script = "grid remove " & oldFrame)
  let frame = optionsFrame & "." & tclGetVar(varName = "newtab")
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
    argv: cstringArray): TclResults {.exportc.} =
  tclSetVar(varName = "newtab", newValue = "general")
  var optionsFrame = mainPaned & ".optionsframe"
  let optionsCanvas = optionsFrame & ".canvas"
  type WidgetData = object
    name, value: string
  if tclEval2(script = "winfo exists " & optionsCanvas) == "0":
    tclEvalFile(fileName = dataDirectory & "ui" & DirSep & "options.tcl")
    tclEval(script = "bind " & optionsFrame & " <Configure> {ResizeCanvas %W.canvas %w %h}")
    let labelsArray: array[4, WidgetData] = [WidgetData(name: "data",
        value: dataDirectory), WidgetData(name: "save", value: saveDirectory),
        WidgetData(name: "docs", value: docDirectory), WidgetData(name: "mods",
        value: modsDirectory)]
    for label in labelsArray:
      let labelName = optionsCanvas & ".options.info." & label.name
      tclEval(script = labelName & " configure -text {" & label.value & " }")
    var themesNames = ""
    for theme in themesList.values:
      themesNames &= " {" & theme.name & "}"
    let comboBox = optionsFrame & ".canvas.options.interface.theme"
    tclEval(script = comboBox & " configure -values [list" & themesNames & "]")
  elif tclEval2(script = "winfo ismapped " & optionsCanvas) == "1":
    tclEval(script = "grid remove " & closeButton)
    showSkyMap(clear = true)
    return tclOk
  optionsFrame = optionsCanvas & ".options.general"
  tclEval(script = "grid " & optionsFrame & " -sticky nwes -padx 10")
  let checkboxArray: array[11, WidgetData] = [WidgetData(name: optionsCanvas &
      ".options.general.autorest", value: (
      if gameSettings.autoRest: "1" else: "0")), WidgetData(
      name: optionsCanvas & ".options.general.autocenter", value: (
      if gameSettings.autoCenter: "1" else: "0")), WidgetData(
      name: optionsCanvas & ".options.general.autoreturn", value: (
      if gameSettings.autoReturn: "1" else: "0")), WidgetData(
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
  var comboBox = optionsFrame & ".theme"
  let theme = try:
        themesList[gameSettings.interfaceTheme]
      except:
        return showError(message = "Can't find theme '" &
            gameSettings.interfaceTheme & "'")
  tclEval(script = comboBox & " set {" & theme.name & "}")
  optionsFrame = optionsCanvas & ".options"
  for i in 0 .. 10:
    accels[i].shortcut = menuAccelerators[i + 1]
  for i in 0 .. 36:
    accels[i + 11].shortcut = mapAccelerators[i + 1]
  accels[11 + 37].shortcut = fullScreenAccel
  for i in 0 .. 3:
    accels[i + 11 + 37 + 1].shortcut = generalAccelerators[i]
  for accel in accels:
    let keyEntry = optionsFrame & accel.entryName
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

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the crew UI
  try:
    discard
#    addCommand("ShowOptionsTab", showOptionsTabCommand)
#    addCommand("ShowOptions", showOptionsCommand)
  except:
    showError(message = "Can't add a Tcl command.")
