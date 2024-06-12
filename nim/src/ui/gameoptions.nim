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

import std/[os, strutils, tables]
import ../[config, game, tk, types]
import coreui, combatui, mapsui, themes, utilsui2

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
  ## ShowOptions
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

proc setFontsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
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
  let
    frameName = ".gameframe.paned.optionsframe.canvas.options.interface"
    spinBox = $argv[1]
    newSize = try:
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
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
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
    let spinbox = ".gameframe.paned.optionsframe.canvas.options.interface." &
        name & "font"
    tclEval(script = spinbox & " set " & $defaultFontSizes[index])
    setFonts(newSize = defaultFontSizes[index], fontType = fontTypesNames[index])
  loadThemeImages()
  return tclOk

proc closeOptionsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.exportc.} =
  tclEval(script = closeButton & " configure -command ShowSkyMap")
  tclEval(script = "grid remove " & closeButton)
  const rootName = ".gameframe.paned.optionsframe.canvas.options"

  proc getCheckboxValue(checkboxName: string): bool =
    return tclGetVar(varName = rootName & " checkboxName") == "1"

  gameSettings.autoRest = getCheckboxValue(checkboxName = ".general.autorest")

  proc getComboboxValue(comboboxName: string): Natural =
    let comboBox = rootName & comboboxName
    return tclEval2(script = comboBox & " current").parseInt

  gameSettings.undockSpeed = (getComboboxValue(
      comboboxName = ".general.speed") + 1).ShipSpeed
  gameSettings.autoCenter = getCheckboxValue(
      checkboxName = ".general.autocenter")
  gameSettings.autoReturn = getCheckboxValue(
      checkboxName = ".general.autoreturn")
  gameSettings.autoFinish = getCheckboxValue(
      checkboxName = ".general.autofinish")
  gameSettings.autoAskForBases = getCheckboxValue(
      checkboxName = ".general.autoaskforbases")
  gameSettings.autoAskForEvents = getCheckboxValue(
      checkboxName = ".general.autoaskforevents")

  proc getSpinboxValue(spinboxName: string): Natural =
    let spinBox = rootName & spinboxName
    return tclEval2(script = spinBox & " get").parseInt

  gameSettings.lowFuel = getSpinboxValue(spinboxName = ".general.fuel")
  gameSettings.lowDrinks = getSpinboxValue(spinboxName = ".general.drinks")
  gameSettings.lowFood = getSpinboxValue(spinboxName = ".general.food")
  gameSettings.autoMoveStop = getComboboxValue(
      comboboxName = ".general.automovestop").AutoMoveBreak
  gameSettings.messagesLimit = getSpinboxValue(
      spinboxName = ".general.messageslimit")
  gameSettings.savedMessages = getSpinboxValue(
      spinboxName = ".general.savedMessages")
  gameSettings.waitMinutes = getSpinboxValue(
      spinboxName = ".general.waitinterval")
  gameSettings.messagesOrder = getComboboxValue(
      comboboxName = ".general.messagesorder").MessagesOrder
  gameSettings.autoSave = getComboboxValue(
      comboboxName = ".general.autosave").AutoSaveTime
  let
    themeCombobox = rootName & ".interface.theme"
    themeName = tclEval2(script = themeCombobox & " get")
  for index, theme in themesList.pairs:
    if theme.name == $themeName:
      gameSettings.interfaceTheme = index
      break
  tclEval(script = "ttk::style theme use " & gameSettings.interfaceTheme)
  setTheme()
  let mapView = ".gameframe.paned.mapframe.map"
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
  gameSettings.autoCloseMessagesTime = getSpinboxValue(
      spinboxName = ".interface.closemessages")
  gameSettings.showNumbers = getCheckboxValue(
      checkboxName = ".interface.shownumbers")
  gameSettings.mapFontSize = getSpinboxValue(spinboxName = ".interface.mapfont")
  gameSettings.helpFontSize = getSpinboxValue(
      spinboxName = ".interface.helpfont")
  gameSettings.interfaceFontSize = getSpinboxValue(
      spinboxName = ".interface.interfacefont")
  gameSettings.listsLimit = getSpinboxValue(
      spinboxName = ".interface.listslimit")
  saveConfig()
  for index, accel in accels.mpairs:
    var
      pos = accel.shortcut.rfind(sub = '-')
      keyName = ""
    if pos > -1:
      keyName = accel.shortcut[0 .. pos] & "KeyPress-" & accel.shortcut[pos +
          1 .. ^1]
    else:
      keyName = "KeyPress-" & accel.shortcut
    tclEval(script = "bind . <" & keyName & "> {}")
    if index < 11:
      menuAccelerators[index + 1] = tclEval2(script = accel.entryName & " get")
      pos = menuAccelerators[index + 1].rfind(sub = '-')
      keyName = ""
      if pos > -1:
        keyName = menuAccelerators[index + 1][0 .. pos] & "KeyPress-" &
            menuAccelerators[index + 1][pos + 1 .. ^1]
      else:
        keyName = "KeyPress-" & menuAccelerators[index + 1]
      tclEval(script = "bind . <" & keyName & "> {InvokeMenu " &
          menuAccelerators[index + 1] & "}")
    elif index < 48:
      mapAccelerators[index - 10] = tclEval2(script = accel.entryName & " get")
    elif index == 48:
      fullScreenAccel = tclEval2(script = accels[48].entryName & " get")
    else:
      generalAccelerators[index - 48] = tclEval2(script = accel.entryName & " get")
    accel.shortcut = tclEval2(script = accel.entryName & " get")
  let keyFile: File = open(saveDirectory & "keys.cfg", fmWrite)
  for accel in accels:
    keyFile.writeLine(accel.configName & " = " & accel.shortcut)
  keyFile.close
  setKeys()
  if argv[1] == "map":
    showSkyMap(clear = true)
  else:
    showCombatUi(newCombat = false)
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the crew UI
  try:
    discard
#    addCommand("ShowOptionsTab", showOptionsTabCommand)
#    addCommand("ShowOptions", showOptionsCommand)
#    addCommand("SetFonts", setFontsCommand)
#    addCommand("SetDefaultFonts", setDefaultFontsCommand)
#    addCommand("CloseOptions", closeOptionsCommand)
  except:
    showError(message = "Can't add a Tcl command.")
