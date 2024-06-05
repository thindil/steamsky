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
import coreui, mapsui, themes

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

var accels: array[14, AccelData] = [AccelData(shortcut: menuAccelerators[1], entryName: ".menu.shipinfo", configName: "ShipInfo"), AccelData( shortcut: menuAccelerators[2], entryName: ".menu.orders", configName: "Orders"), AccelData(shortcut: menuAccelerators[3], entryName: ".menu.crafts", configName: "Crafting"), AccelData( shortcut: menuAccelerators[4], entryName: ".menu.messages", configName: "LastMessages"), AccelData(shortcut: menuAccelerators[5], entryName: ".menu.knowledge", configName: "Knowledge"), AccelData( shortcut: menuAccelerators[6], entryName: ".menu.waitorders", configName: "WaitOrders"), AccelData(shortcut: menuAccelerators[7], entryName: ".menu.gamestats", configName: "GameStats"), AccelData( shortcut: menuAccelerators[8], entryName: ".menu.help", configName: "Help"), AccelData(shortcut: menuAccelerators[9], entryName: ".menu.gameoptions", configName: "GameOptions"), AccelData( shortcut: menuAccelerators[10], entryName: ".menu.quit", configName: "Quit"), AccelData(shortcut: menuAccelerators[11], entryName: ".menu.resign", configName: "Resign"), AccelData( shortcut: mapAccelerators[1], entryName: ".menu.menu", configName: "GameMenu"), AccelData(shortcut: mapAccelerators[2], entryName: ".map.mapoptions", configName: "MapOptions"), AccelData( shortcut: mapAccelerators[3], entryName: ".map.zoomin", configName: "ZoomInMap")]

proc showOptionsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.exportc.} =
  tclSetVar(varName = "newtab", newValue = "general")
  var optionsFrame = mainPaned & ".optionsframe"
  let optionsCanvas = optionsFrame & ".canvas"
  type WidgetData = object
    name, value: string
  if tclEval2(script = "winfo exists " & optionsCanvas) == "0":
    tclEvalFile(fileName = dataDirectory & DirSep & "options.tcl")
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
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the crew UI
  try:
    discard
#    addCommand("ShowOptionsTab", showOptionsTabCommand)
#    addCommand("ShowOptions", showOptionsCommand)
  except:
    showError(message = "Can't add a Tcl command.")
