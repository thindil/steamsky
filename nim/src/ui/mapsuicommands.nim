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

import std/strutils
import ../[config, crew2, events2, game, game2, maps, messages, missions2,
    shipscargo, shipscrew, shipsmovement, tk, types]
import combatui, coreui, dialogs, ordersmenu, statisticsui

const buttonNames: array[1 .. 13, string] = ["show", "nw", "n", "ne", "w",
    "wait", "e", "sw", "s", "se", "hide", "left", "right"]

proc hideMapButtonsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].} =
  ## Hide buttons used to move the map
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## HideMapButtons
  for i in 2 .. 13:
    let buttonName = mainPaned & ".mapframe.buttons." & buttonNames[i]
    tclEval(script = "grid remove " & buttonName)
  let buttonName = mainPaned & ".mapframe.buttons.show"
  tclEval(script = "grid " & buttonName)
  return tclOk

proc showMapButtonsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].} =
  ## Show buttons used to move the map
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowMapButtons
  let buttonsBox = mainPaned & ".mapframe.buttons"
  for i in 2 .. 11:
    let buttonName = buttonsBox & "." & buttonNames[i]
    tclEval(script = "grid " & buttonName)
  var buttonName = buttonsBox & ".show"
  tclEval(script = "grid remove " & buttonName)
  buttonName = (if tclEval2(script = "grid info " & buttonsBox).contains(
      "-sticky es"): buttonsBox & ".left" else: buttonsBox & ".right")
  tclEval(script = "grid " & buttonName)
  return tclOk

proc moveMapButtonsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].} =
  ## Move buttons used to move the map to the right or left corner
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## MoveMapButtons buttonname
  ## Buttonname is the name of the button which was clicked
  let buttonsBox = mainPaned & ".mapframe.buttons"
  var button = buttonsBox & "." & $argv[1]
  tclEval(script = "grid remove " & button)
  if argv[1] == "left":
    button = buttonsBox & ".right"
    tclEval(script = "grid configure " & buttonsBox & " -sticky sw")
  else:
    button = buttonsBox & ".left"
    tclEval(script = "grid configure " & buttonsBox & " -sticky se")
  tclEval(script = "grid " & button)
  return tclOk

proc moveMapInfoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].} =
  ## Move the map cell info frame when the mouse enters it
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## MoveMapInfo
  let mapInfoFrame = mainPaned & ".mapframe.info"
  tclEval(script = "grid configure " & mapInfoFrame & " -sticky " & (
      if tclEval2(script = "grid info " & mapInfoFrame).find("-sticky ne") ==
      -1: "ne" else: "wn"))
  return tclOk

proc drawMapCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].}
  ## Draw the sky map
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## DrawMap
proc zoomMapCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].}
  ## Zoom in or our the sky map
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ZoomMap

proc updateMapInfoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].}
  ## Update the information about the selected map's cell
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## UpdateMapInfo x y
  ## X and Y are coordinates of the map cell which info will be show

proc showDestinationMenuCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].}
  ## Create and show the destination menu dialog
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowDestinationMenu x y
  ## X and Y are the map coordinates for which the destination menu will be show

proc setShipDestinationCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].}
  ## Set the current map cell as the destination for the player's ship
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SetDestination

proc moveMapCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].}
  ## Move the map in the selected direction
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## MoveMap direction
  ## Direction in which the map will be moved

proc moveShipCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [
        WriteIOEffect, RootEffect].}
  ## Move the player's ship in the selected direction and check what happened
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## MoveShip direction
  ## Direction in which the player's ship will be moved

proc quitGameCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].} =
  ## Ask the player if they wants to quit from the game and if yes, save the
  ## game and show the main menu
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## QuitGame
  showQuestion(question = "Are you sure want to quit?", res = "quit")
  return tclOk

proc resignGameCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].} =
  ## Resing from the game - if the player resigned, kill they character and
  ## continue as for death of the player's character
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ResignGame
  showQuestion(question = "Are you sure want to resign from game?",
      res = "resign")
  return tclOk

proc showStatsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].} =
  ## Show the screen with the player's game statistics
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowStats
  tclEval(script = "grid " & closeButton & " -row 0 -column 1")
  showStatistics()
  return tclOk

proc showSkyMapCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].}
  ## Show the sky map
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowSkyMap ?previouscommand?
  ## Previouscommand is command to show previous screen. Some screens require
  ## to do special actions when closing them

proc moveMouseCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].} =
  ## Move the mouse cursor with keyboard
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## MoveCursor direction
  ## Direction is the direction in which the mouse cursor should be moves or
  ## click if emulate clicking with the left or right button
  let mapView = mainPaned & ".mapframe.map"
  if tclEval2(script = "focus") != mapView:
    tclEval(script = "focus -force " & mapView)
    return tclOk
  if argv[1] == "click":
    tclEval(script = "event generate " & mapView & " <Button-" & (
        if gameSettings.rightButton: "3" else: "1") & "> -x " & $argv[2] &
        " -y " & $argv[3])
  elif argv[1] == "nw":
    tclEval(script = "event generate " & mapView &
        " <Motion> -warp 1 -x [expr " & $argv[2] & "-5] -y [expr " & $argv[3] & "-5]")
  elif argv[1] == "n":
    tclEval(script = "event generate " & mapView & " <Motion> -warp 1 -x " &
        $argv[2] & " -y [expr " & $argv[3] & "-5]")
  elif argv[1] == "ne":
    tclEval(script = "event generate " & mapView &
        " <Motion> -warp 1 -x [expr " & $argv[2] & "+5] -y [expr " & $argv[3] & "-5]")
  elif argv[1] == "w":
    tclEval(script = "event generate " & mapView &
        " <Motion> -warp 1 -x [expr " & $argv[2] & "-5] -y " & $argv[3])
  elif argv[1] == "e":
    tclEval(script = "event generate " & mapView &
        " <Motion> -warp 1 -x [expr " & $argv[2] & "+5] -y " & $argv[3])
  elif argv[1] == "sw":
    tclEval(script = "event generate " & mapView &
        " <Motion> -warp 1 -x [expr " & $argv[2] & "-5] -y [expr " & $argv[3] & "+5]")
  elif argv[1] == "s":
    tclEval(script = "event generate " & mapView & " <Motion> -warp 1 -x " &
        $argv[2] & " -y [expr " & $argv[3] & "+5]")
  elif argv[1] == "se":
    tclEval(script = "event generate " & mapView &
        " <Motion> -warp 1 -x [expr " & $argv[2] & "+5] -y [expr " & $argv[3] & "+5]")
  return tclOk

proc toggleFullScreenCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults {.sideEffect, raises: [], tags: [].} =
  ## Toggle the game's full screen mode
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ToggleFullScreen
  if tclEval2(script = "wm attributes . -fullscreen") == "0":
    tclEval(script = "wm attributes . -fullscreen 1")
    gameSettings.fullScreen = true
  else:
    tclEval(script = "wm attributes . -fullscreen 0")
    gameSettings.fullScreen = false
  return tclOk

proc addCommands*() =
  addCommand("HideMapButtons", hideMapButtonsCommand)
  addCommand("ShowMapButtons", showMapButtonsCommand)
  addCommand("MoveMapButtons", moveMapButtonsCommand)
  addCommand("MoveMapInfo", moveMapInfoCommand)
  addCommand("DrawMap", drawMapCommand)
  addCommand("ZoomMap", zoomMapCommand)
  addCommand("UpdateMapInfo", updateMapInfoCommand)
  addCommand("ShowDestinationMenu", showDestinationMenuCommand)
  addCommand("SetDestination", setShipDestinationCommand)
  addCommand("MoveMap", moveMapCommand)
  addCommand("MoveShip", moveShipCommand)
  addCommand("QuitGame", quitGameCommand)
  addCommand("ResignGame", resignGameCommand)
  addCommand("ShowStats", showStatsCommand)
  addCommand("ShowSkyMap", showSkyMapCommand)
  addCommand("MoveCursor", moveMouseCommand)
  addCommand("ToggleFullScreen", toggleFullScreenCommand)

import std/tables
import mapsui, themes

proc drawMapCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults =
  let mapView = mainPaned & ".mapframe.map"
  try:
    discard tclEval(script = mapView & " configure -width [expr [winfo width $mapview] / [font measure MapFont {" &
        themesList[gameSettings.interfaceTheme].emptyMapIcon & "}]]")
  except:
    tclEval(script = "bgerror {Can't set map width. Reason: " &
        getCurrentExceptionMsg() & "}")
    return tclOk
  tclEval(script = mapView & " configure -height [expr [winfo height $mapview] / [font metrics MapFont -linespace]]")
  if tclGetVar(varName = "refreshmap") == "1":
    drawMap()
  return tclOk

proc zoomMapCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults =
  gameSettings.mapFontSize = (if argv[1] == "raise": gameSettings.mapFontSize +
      1 else: gameSettings.mapFontSize - 1)
  if gameSettings.mapFontSize < 3:
    gameSettings.mapFontSize = 3
  elif gameSettings.mapFontSize > 50:
    gameSettings.mapFontSize = 50
  tclEval(script = "font configure MapFont -size " & $gameSettings.mapFontSize)
  tclSetVar(varName = "refreshmap", newValue = "1")
  return drawMapCommand(clientData = clientData, interp = interp, argc = argc, argv = argv)

var mapX, mapY = 0

proc updateMapInfoCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults =
  let
    mapView = mainPaned & ".mapframe.map"
    mapIndex = tclEval2(script = mapView & " index @" & $argv[1] & "," & $argv[2])
  try:
    if startY + (mapIndex[0 .. mapIndex.find(".") - 1]).parseInt - 1 < 1:
      return tclOk
    mapY = startY + (mapIndex[0 .. mapIndex.find(".") - 1]).parseInt - 1
    if mapY > 1_024:
      return tclOk
  except:
    tclEval(script = "bgerror {Can't set map Y coordinate. Reason: " &
        getCurrentExceptionMsg() & "}")
    return tclOk
  try:
    if startX + (mapIndex[mapIndex.find(".") + 1 .. ^1]).parseInt < 1:
      return tclOk
    mapX = startX + (mapIndex[mapIndex.find(".") + 1 .. ^1]).parseInt
    if mapX > 1_024:
      return tclOk
  except:
    tclEval(script = "bgerror {Can't set map X coordinate. Reason: " &
        getCurrentExceptionMsg() & "}")
    return tclOk
  updateMapInfo(x = mapX, y = mapY)
  return tclOk

proc showDestinationMenuCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults =
  if mapX == 0 or mapY == 0 and updateMapInfoCommand(clientData = clientData,
      interp = interp, argc = argc, argv = argv) != tclOk:
    return tclError
  let destinationDialog = createDialog(name = ".gameframe.destinationmenu",
      title = "Set destination", parentName = ".gameframe")
  if playerShip.skyX == mapX and playerShip.skyY == mapY:
    tclEval(script = "CloseDialog " & destinationDialog)
    return showOrdersCommand(clientData = clientData, interp = interp,
        argc = argc, argv = argv)
  var button = destinationDialog & ".set"
  tclEval(script = "ttk::button " & button &
      " -text {Set destination} -command {SetDestination;CloseDialog " &
      destinationDialog & "}")
  tclEval(script = "grid " & button & " -sticky we -padx 5")
  let dialogCloseButton = destinationDialog & ".button"
  tclEval(script = "ttk::button " & dialogCloseButton &
      " -text Close -command {CloseDialog " & destinationDialog & "}")
  tclEval(script = "bind " & button & " <Escape> {" & dialogCloseButton & " invoke;break}")
  if playerShip.speed != docked:
    tclEval(script = "bind " & button & " <Tab> {focus " & destinationDialog & ".setandmove;break}")
    button = destinationDialog & ".setandmove"
    tclEval(script = "ttk::button " & button &
        " -text {Set destination and move} -command {SetDestination;MoveShip moveto;CloseDialog " &
        destinationDialog & "}")
    tclEval(script = "grid " & button & " -sticky we -padx 5")
    tclEval(script = "bind " & button & " <Escape> {" & dialogCloseButton & " invoke;break}")
    if playerShip.destinationX > 0 and playerShip.destinationY > 0:
      tclEval(script = "bind " & button & " <Tab> {focus " & destinationDialog & ".move;break}")
      button = destinationDialog & ".move"
      tclEval(script = "ttk::button " & button &
          " -text {Move to} -command {MoveShip moveto;CloseDialog " &
          destinationDialog & "}")
      tclEval(script = "grid " & button & " -sticky we -padx 5")
      tclEval(script = "bind " & button & " <Escape> {" & dialogCloseButton & " invoke;break}")
      tclEval(script = "bind " & button & " <Tab> {focus " & destinationDialog & ".button;break}")
  tclEval(script = "grid " & dialogCloseButton & " -sticky we -padx 5 -pady {0 5}")
  tclEval(script = "bind " & dialogCloseButton & " <Tab> {focus " &
      destinationDialog & ".set;break}")
  tclEval(script = "bind " & dialogCloseButton & " <Escape> {" &
      dialogCloseButton & " invoke;break}")
  showDialog(dialog = destinationDialog, parentFrame = ".gameframe")
  return tclOk

proc setShipDestinationCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults =
  playerShip.destinationX = mapX
  playerShip.destinationY = mapY
  addMessage(message = "You set the travel destination for your ship.",
      mType = orderMessage)
  if gameSettings.autoCenter:
    centerX = playerShip.skyX
    centerY = playerShip.skyY
  drawMap()
  updateMoveButtons()
  return tclOk

proc moveMapCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults =
  let mapView = mainPaned & ".mapframe.map"
  if tclEval2(script = "winfo ismapped " & mapView) == "0":
    return tclOk
  let
    mapHeight = try:
        tclEval2(script = mapView & " cget -height").parseInt
      except:
        tclEval(script = "bgerror {Can't get the map's height. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    mapWidth = try:
        tclEval2(script = mapView & " cget -width").parseInt
      except:
        tclEval(script = "bgerror {Can't get the map's width. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    dialogName = ".gameframe.movemapdialog"
  if argv[1] == "centeronship":
    centerX = playerShip.skyX
    centerY = playerShip.skyY
  elif argv[1] == "movemapto":
    var spinBox = dialogName & ".x"
    centerX = try:
        tclEval2(script = spinBox & " get").parseInt
      except:
        tclEval(script = "bgerror {Can't set center X. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
    spinBox = dialogName & ".y"
    centerY = try:
        tclEval2(script = spinBox & " get").parseInt
      except:
        tclEval(script = "bgerror {Can't set center Y. Reason: " &
            getCurrentExceptionMsg() & "}")
        return tclOk
  elif argv[1] == "n":
    centerY = (if centerY - (mapHeight / 3).int < 1: (mapHeight /
        3).int else: centerY - (mapHeight / 3).int)
  elif argv[1] == "s":
    centerY = (if centerY + (mapHeight / 3).int > 1_024: (mapHeight /
        3).int else: centerY + (mapHeight / 3).int)
  elif argv[1] == "w":
    centerX = (if centerX - (mapWidth / 3).int < 1: (mapWidth /
        3).int else: centerX - (mapWidth / 3).int)
  elif argv[1] == "e":
    centerX = (if centerX + (mapWidth / 3).int > 1_024: (mapWidth /
        3).int else: centerX + (mapWidth / 3).int)
  elif argv[1] == "nw":
    centerY = (if centerY - (mapHeight / 3).int < 1: (mapHeight /
        3).int else: centerY - (mapHeight / 3).int)
    centerX = (if centerX - (mapWidth / 3).int < 1: (mapWidth /
        3).int else: centerX - (mapWidth / 3).int)
  elif argv[1] == "ne":
    centerY = (if centerY - (mapHeight / 3).int < 1: (mapHeight /
        3).int else: centerY - (mapHeight / 3).int)
    centerX = (if centerX + (mapWidth / 3).int > 1_024: (mapWidth /
        3).int else: centerX + (mapWidth / 3).int)
  elif argv[1] == "sw":
    centerY = (if centerY + (mapHeight / 3).int > 1_024: (mapHeight /
        3).int else: centerY + (mapHeight / 3).int)
    centerX = (if centerX - (mapWidth / 3).int < 1: (mapWidth /
        3).int else: centerX - (mapWidth / 3).int)
  elif argv[1] == "se":
    centerY = (if centerY + (mapHeight / 3).int > 1_024: (mapHeight /
        3).int else: centerY + (mapHeight / 3).int)
    centerX = (if centerX + (mapWidth / 3).int > 1_024: (mapWidth /
        3).int else: centerX + (mapWidth / 3).int)
  elif argv[1] == "centeronhome":
    centerX = skyBases[playerShip.homeBase].skyX
    centerY = skyBases[playerShip.homeBase].skyY
  drawMap()
  return tclOk

proc moveShipCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults =
  var
    res = 0
    message = ""
    newX, newY = 0
    startsCombat = false

  proc updateCoordinates() =
    if playerShip.destinationX > playerShip.skyX:
      newX = 1
    elif playerShip.destinationX < playerShip.skyX:
      newX = -1
    if playerShip.destinationY > playerShip.skyY:
      newY = 1
    elif playerShip.destinationY < playerShip.skyY:
      newY = -1

  if argv[1] == "n":
    res = try:
        moveShip(x = 0, y = -1, message = message)
      except:
        tclEval(script = "bgerror {Can't move the ship. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
  elif argv[1] == "s":
    res = try:
        moveShip(x = 0, y = 1, message = message)
      except:
        tclEval(script = "bgerror {Can't move the ship. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
  elif argv[1] == "e":
    res = try:
        moveShip(x = 1, y = 0, message = message)
      except:
        tclEval(script = "bgerror {Can't move the ship. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
  elif argv[1] == "w":
    res = try:
        moveShip(x = -1, y = 0, message = message)
      except:
        tclEval(script = "bgerror {Can't move the ship. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
  elif argv[1] == "sw":
    res = try:
        moveShip(x = -1, y = 1, message = message)
      except:
        tclEval(script = "bgerror {Can't move the ship. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
  elif argv[1] == "se":
    res = try:
        moveShip(x = 1, y = 1, message = message)
      except:
        tclEval(script = "bgerror {Can't move the ship. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
  elif argv[1] == "nw":
    res = try:
        moveShip(x = -1, y = -1, message = message)
      except:
        tclEval(script = "bgerror {Can't move the ship. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
  elif argv[1] == "ne":
    res = try:
        moveShip(x = 1, y = -1, message = message)
      except:
        tclEval(script = "bgerror {Can't move the ship. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
  elif argv[1] == "waitormove":
    if playerShip.destinationX == 0 and playerShip.destinationY == 0:
      res = 1
      try:
        updateGame(minutes = gameSettings.waitMinutes)
        waitInPlace(minutes = gameSettings.waitMinutes)
      except:
        tclEval(script = "bgerror {Can't update the game. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
    else:
      updateCoordinates()
      res = try:
          moveShip(x = newX, y = newY, message = message)
        except:
          tclEval(script = "bgerror {Can't move the ship. Reason: " &
              getCurrentExceptionMsg() & "}")
          return
      if playerShip.destinationX == playerShip.skyX and
          playerShip.destinationY == playerShip.skyY:
        addMessage(message = "You reached your travel destination.",
            mType = orderMessage)
        playerShip.destinationX = 0
        playerShip.destinationY = 0
        if gameSettings.autoFinish:
          message = try:
              autoFinishMissions()
            except:
              tclEval(script = "bgerror {Can't finish missions. Reason: " &
                  getCurrentExceptionMsg() & "}")
              return
        res = 4
  elif argv[1] == "moveto":
    while true:
      newX = 0
      newY = 0
      updateCoordinates()
      res = try:
          moveShip(x = newX, y = newY, message = message)
        except:
          tclEval(script = "bgerror {Can't move the ship. Reason: " &
              getCurrentExceptionMsg() & "}")
          return
      if res == 0:
        break
      startsCombat = try:
          checkForEvent()
        except:
          tclEval(script = "bgerror {Can't check for events. Reason: " &
              getCurrentExceptionMsg() & "}")
          return
      if startsCombat:
        res = 4
        break
      if res == 8:
        try:
          waitForRest()
        except:
          tclEval(script = "bgerror {Can't wait for rest of the crew. Reason: " &
              getCurrentExceptionMsg() & "}")
          return
        try:
          if "sentientships" notin factionsList[playerShip.crew[
              0].faction].flags and (findMember(order = pilot) == -1 or
              findMember(order = engineer) == 0):
            try:
              waitForRest()
            except:
              tclEval(script = "bgerror {Can't wait for rest of the crew. Reason: " &
                  getCurrentExceptionMsg() & "}")
              return
        except:
          tclEval(script = "bgerror {Can't check do faction has sentientships flags. Reason: " &
              getCurrentExceptionMsg() & "}")
          return
        res = 1
        startsCombat = try:
            checkForEvent()
          except:
            tclEval(script = "bgerror {Can't check for events. Reason: " &
                getCurrentExceptionMsg() & "}")
            return
        if startsCombat:
          res = 4
          break
      if gameSettings.autoMoveStop != never and skyMap[playerShip.skyX][
          playerShip.skyY].eventIndex > -1:
        let eventIndex = skyMap[playerShip.skyX][playerShip.skyY].eventIndex
        case gameSettings.autoMoveStop
        of any:
          if eventsList[eventIndex].eType in {enemyShip, trader, friendlyShip, enemyPatrol}:
            res = 0
            break
        of friendly:
          if eventsList[eventIndex].eType in {trader, friendlyShip}:
            res = 0
            break
        of enemy:
          if eventsList[eventIndex].eType in {enemyShip, enemyPatrol}:
            res = 0
            break
        of never:
          discard
      let messageDialog = ".message"
      if tclEval2(script = "winfo exists " & messageDialog) == "0":
        try:
          if getItemAmount(itemType = fuelType) <= gameSettings.lowFuel:
            showMessage(text = "Your fuel level is dangerously low.",
                title = "Low fuel level")
            res = 4
            break
          elif getItemsAmount(iType = "Food") <= gameSettings.lowFood:
            showMessage(text = "Your food level is dangerously low.",
                title = "Low food level")
            res = 4
            break
          elif getItemsAmount(iType = "Drinks") <= gameSettings.lowDrinks:
            showMessage(text = "Your drinks level is dangerously low.",
                title = "Low drinks level")
            res = 4
            break
        except:
          tclEval(script = "bgerror {Can't check low level of items. Reason: " &
              getCurrentExceptionMsg() & "}")
          return
      if playerShip.destinationX == playerShip.skyX and
          playerShip.destinationY == playerShip.skyY:
        addMessage("You reached your travel destination.", mType = orderMessage)
        playerShip.destinationX = 0
        playerShip.destinationY = 0
        if gameSettings.autoFinish:
          message = try:
              autoFinishMissions()
            except:
              tclEval(script = "bgerror {Can't finish missions. Reason: " &
                  getCurrentExceptionMsg() & "}")
              return
        res = 4
        break
      if res in 6 .. 7:
        break
  case res
  # Ship moved, check for events
  of 1:
    startsCombat = try:
        checkForEvent()
      except:
        tclEval(script = "bgerror {Can't check for events. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
    if not startsCombat and gameSettings.autoFinish:
      message = try:
          autoFinishMissions()
        except:
          tclEval(script = "bgerror {Can't finish missions. Reason: " &
              getCurrentExceptionMsg() & "}")
          return
  # Ship moved, but pilot needs rest, confirm
  of 6:
    showQuestion(question = "You don't have pilot on duty. Do you want to wait until your pilot rest?",
        res = "nopilot")
    return tclOk
  # Ship moved, but engineer needs rest, confirm
  of 7:
    showQuestion(question = "You don't have engineer on duty. Do you want to wait until your pilot rest?",
        res = "nopilot")
    return tclOk
  # Ship moved, but crew needs rest, autorest
  of 8:
    startsCombat = try:
        checkForEvent()
      except:
        tclEval(script = "bgerror {Can't check for events. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
    if not startsCombat:
      try:
        waitForRest()
      except:
        tclEval(script = "bgerror {Can't wait for rest of the crew. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
      try:
        if "sentientships" notin factionsList[playerShip.crew[
            0].faction].flags and (findMember(order = pilot) == -1 or
                findMember(
            order = engineer) == -1):
          waitForRest()
      except:
        tclEval(script = "bgerror {Can't check do faction has sentientships flags. Reason: " &
            getCurrentExceptionMsg() & "}")
        return
      startsCombat = try:
          checkForEvent()
        except:
          tclEval(script = "bgerror {Can't check for events. Reason: " &
              getCurrentExceptionMsg() & "}")
          return
    if not startsCombat and gameSettings.autoFinish:
      message = try:
          autoFinishMissions()
        except:
          tclEval(script = "bgerror {Can't finish missions. Reason: " &
              getCurrentExceptionMsg() & "}")
          return
  else:
    discard
  if message.len > 0:
    showMessage(text = message, title = "Message")
  if startsCombat:
    showCombatUi()
  else:
    showSkyMap()
  return tclOk

proc showSkyMapCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: openArray[cstring]): TclResults =
  if argc == 1:
    tclEval(script = "grid remove " & closeButton)
    showSkyMap(clear = true)
  else:
    tclEval(script = $argv[1])
  tclEval(script = "focus .")
  return tclOk

# Temporary code for interfacing with Ada

proc addAdaMapsCommands() {.raises: [], tags: [RootEffect], exportc.} =
  try:
    addCommands()
  except:
    echo getCurrentExceptionMsg()

