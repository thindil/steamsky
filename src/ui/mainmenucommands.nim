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

## Provides code related to the Tcl commands for the main menu like
## opening links, loading the game, starting a new one, etc.

import std/[algorithm, os, osproc, strutils, tables, times]
import contracts, nimalyzer
import ../[basestypes, config, game, game2, gamesaveload, goals,
    halloffame, ships2, shipscrew, tk, utils]
import coreui, dialogs, errordialog, mapsui, showmainmenu, table, utilsui2

proc openLinkCommand*(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [ReadIOEffect,
    ExecIOEffect, RootEffect], cdecl, contractual, contractual,
    ruleOff: "params".} =
  ## Open the selected link in a proper program
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## OpenLink url
  ## Url is link which will be opened
  let command: string = try:
        findExe(exe = (if hostOS == "windows": "start" elif hostOS ==
          "macosx": "open" else: "xdg-open"))
      except:
        return showError(message = "Can't find the program to open the link")
  if command.len == 0:
    showMessage(text = "Can't open the link. Reason: no program to open it.",
        parentFrame = ".", title = "Can't open the link.")
    return tclOk
  try:
    discard execCmd(command = command & " " & $argv[1])
  except:
    showError(message = "Can't open the link")
  return tclOk

proc showFileCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    ReadDirEffect, ReadIOEffect, WriteIOEffect, TimeEffect, RootEffect], cdecl,
        contractual.} =
  ## Show the selected file content
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowFile filename
  ## Filename is the name of the file in the documentation directory which
  ## will be show
  const textView: string = ".showfilemenu.text"
  tclEval(script = textView & " configure -state normal")
  tclEval(script = textView & " delete 1.0 end")
  let fileName: string = $argv[1]
  if fileExists(filename = docDirectory.string & fileName):
    try:
      for line in lines(filename = docDirectory.string & fileName):
        tclEval(script = textView & " insert end {" & line & "\n}")
    except:
      showError(message = "Can't read file '" & fileName & "'.")
  else:
    tclEval(script = textView & " insert end {Can't find file to load. Did '" &
        fileName & "' file is in '" & docDirectory.string & "' directory?}")
  tclEval(script = textView & " configure -state disabled")
  tclEval(script = "bind . <Alt-b> {InvokeButton .showfilemenu.back}")
  tclEval(script = "bind . <Escape> {InvokeButton .showfilemenu.back}")
  return tclOk

var allNews: bool = false

proc showNewsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    ReadIOEffect, ReadDirEffect, WriteIOEffect, TimeEffect, RootEffect], cdecl,
        contractual.} =
  ## Show the list of changes in the game, all or just recent, since the last
  ## release
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowNews boolean
  ## If boolean is true, show all news, otherwise only recent
  const allNewsButton: string = ".newsmenu.showall"
  if argv[1] == "false":
    allNews = false
    tclEval(script = allNewsButton & " configure -text {Show all changes} -command {ShowNews true}")
    tclEval(script = "tooltip::tooltip " & allNewsButton & " \"Show all changes to the game since previous big stable version\"")
  else:
    allNews = true
    tclEval(script = allNewsButton & " configure -text {Show only newest changes} -command {ShowNews false}")
    tclEval(script = "tooltip::tooltip " & allNewsButton & " \"Show only changes to the game since previous relese\"")
  const textView: string = ".newsmenu.text"
  tclEval(script = textView & " configure -state normal")
  tclEval(script = textView & " delete 1.0 end")
  if fileExists(filename = docDirectory.string & "CHANGELOG.md"):
    try:
      var index: Natural = 0
      for line in lines(filename = docDirectory.string & "CHANGELOG.md"):
        index.inc
        if index < 6:
          continue
        if (not allNews) and line.len > 1 and line[0 .. 2] == "## ":
          break
        tclEval(script = textView & " insert end {" & line & "\n}")
    except:
      showError(message = "Can't read file 'CHANGELOG.md'")
  else:
    tclEval(script = textView & " insert end {Can't find file to load. Did 'CHANGELOG.md' file is in '" &
        docDirectory.string & "' directory?}")
  tclEval(script = textView & " configure -state disabled")
  return tclOk

proc showHallOfFameCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], cdecl,
        contractual.} =
  ## Show the hall of fame screen
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowHallOfFame
  const hofView: string = ".hofmenu.view"
  tclEval(script = hofView & " delete [list [" & hofView & " children {}]]")
  for index, entry in hallOfFameArray:
    if entry.points == 0:
      break
    tclEval(script = hofView & " insert {} end -values [list " & $index & " " &
        entry.name & " " & $entry.points & " " & entry.deathReason & "]")
  return tclOk

proc deleteGameCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], cdecl,
        contractual.} =
  ## Delete a saved game file
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## DeleteGame file
  ## File is the name of the saved game to delete
  tclSetVar(varName = "deletesave", newValue = $argv[1])
  showQuestion(question = "Are you sure you want delete this savegame?",
      res = "deletesave", inGame = false)
  return tclOk

proc setFactionCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
        WriteIOEffect, TimeEffect, RootEffect], cdecl, contractual.} =
  ## Set faction destription and available bases and careers
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SetFaction
  const frameName: string = ".newgamemenu.canvas.player"
  var comboBox: string = frameName & ".faction"
  let factionName: string = tclEval2(script = comboBox & " get")
  var label: string = ""

  proc updateInfo(newText: string) {.raises: [], tags: [], contractual.} =
    ## Update the info section with iformation about the selected faction and
    ## career
    ##
    ## * newText -  the new text to show
    const infoText: string = ".newgamemenu.info.text"
    tclEval(script = infoText & " configure -state normal")
    tclEval(script = infoText & " delete 1.0 end")
    tclEval(script = infoText & " insert end {Select your faction from a list. Factions have the biggest impact on game. They determine the amount of bases and some playing styles. More information about each faction can be found after selecting it. You can't change this later.\n\n}")
    tclEval(script = infoText & " insert end " & newText)
    tclEval(script = infoText & " configure -state disabled")

  if factionName == "Random":
    label = frameName & ".labelcareer"
    tclEval(script = "grid remove " & label)
    comboBox = frameName & ".career"
    tclEval(script = comboBox & " set Random")
    tclEval(script = "grid remove " & comboBox)
    label = frameName & ".labelbase"
    tclEval(script = "grid remove " & label)
    comboBox = frameName & ".base"
    tclEval(script = comboBox & " set Any")
    tclEval(script = "grid remove " & comboBox)
    updateInfo(newText = "{Faction, career and base type will be randomly selected for you during creating new game. Not recommended for new player.}")
    return tclOk
  label = frameName & ".labelcareer"
  tclEval(script = "grid " & label)
  comboBox = frameName & ".career"
  tclEval(script = "grid " & comboBox)
  label = frameName & ".labelbase"
  tclEval(script = "grid " & label)
  comboBox = frameName & ".base"
  tclEval(script = "grid " & comboBox)
  let genderFrame: string = frameName & ".gender"
  for faction in factionsList.values:
    if faction.name != factionName:
      continue
    if "nogender" in faction.flags:
      label = frameName & ".labelgender"
      tclEval(script = "grid remove " & label)
      tclEval(script = "grid remove " & genderFrame)
      tclSetVar(varName = "playergender", newValue = "M")
    else:
      label = frameName & ".labelgender"
      tclEval(script = "grid " & label)
      tclEval(script = "grid " & genderFrame)
    var values: string = ""
    for career in faction.careers.values:
      values.add(y = " " & career.name)
    values.add(y = " Random")
    comboBox = frameName & ".career"
    tclEval(script = comboBox & " configure -values [list " & values & "]")
    tclEval(script = comboBox & " set General")
    values = " Any"
    for baseType in faction.basesTypes.keys:
      try:
        values.add(y = " {" & basesTypesList[baseType].name & "}")
      except:
        return showError(message = "Can't add base type.")
    comboBox = frameName & ".base"
    tclEval(script = comboBox & " configure -values [list " & values & "]")
    tclEval(script = comboBox & " set Any")
    updateInfo(newText = "{" & faction.description & "}")
    break
  return tclOk

proc setCareerCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], cdecl,
        contractual.} =
  ## Set career description
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SetCareer
  const frameName: string = ".newgamemenu.canvas.player"
  var comboBox: string = frameName & ".faction"
  let factionName: string = tclEval2(script = comboBox & " get")
  comboBox = frameName & ".career"
  let careerName: string = tclEval2(script = comboBox & " get")
  const infoText: string = ".newgamemenu.info.text"
  tclEval(script = infoText & " configure -state normal")
  tclEval(script = infoText & " delete 1.0 end")
  tclEval(script = infoText & " insert end {Select your career from a list. Careers have some impact on gameplay (each have bonuses to gaining experience in some fields plus they determine your starting ship and crew). More info about each career can be found after selecting it. You can't change career later.\n\n}")
  for faction in factionsList.values:
    if faction.name == factionName:
      for career in faction.careers.values:
        if career.name == careerName:
          tclEval(script = infoText & " insert end {" & career.description & "}")
          break
      break
  if careerName == "Random":
    tclEval(script = infoText & " insert end {Career will be randomly selected for you during creating new game. Not recommended for new player.}")
  tclEval(script = infoText & " configure -state disabled")
  return tclOk

proc setBaseCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], cdecl,
        contractual.} =
  ## Set starting base description
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SetBase
  const comboBox: string = ".newgamemenu.canvas.player.base"
  let baseName: string = tclEval2(script = comboBox & " get")
  const infoText: string = ".newgamemenu.info.text"
  tclEval(script = infoText & " configure -state normal")
  tclEval(script = infoText & " delete 1.0 end")
  tclEval(script = infoText & " insert end {Select your starting base type from a list. Your starting base is your home base, where you can gain faster experience. Home base can be changed later. Some types of bases are better starting points than others. More info about each base type can be found after selecting it.\n\n}")
  for baseType in basesTypesList.values:
    if baseType.name == baseName:
      tclEval(script = infoText & " insert end {" & baseType.description & "}")
      break
  if baseName == "Any":
    tclEval(script = infoText & " insert end {Start the game in randomly selected base type.}")
  tclEval(script = infoText & " configure -state disabled")
  return tclOk

proc randomNameCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], cdecl,
        contractual.} =
  ## Generate random player or ship name
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## RandomName type
  ## Type is type of name which should be generated. Possible options are
  ## player or ship
  const comboBox: string = ".newgamemenu.canvas.player.faction"
  let factionName: string = tclEval2(script = comboBox & " get")
  var factionIndex: string = ""
  for index, faction in factionsList:
    if faction.name == factionName:
      factionIndex = index
      break
  let nameEntry: string = ".newgamemenu.canvas.player." & $argv[1] & "name"
  if argv[1] == "player":
    var gender: char = 'M'
    gender = tclGetVar(varName = "playergender")[0]
    tclEval(script = nameEntry & " delete 0 end")
    tclEval(script = nameEntry & " insert end " & generateMemberName(
        gender = gender, factionIndex = factionIndex))
    return tclOk
  tclEval(script = nameEntry & " delete 0 end")
  tclEval(script = nameEntry & " insert end " & generateShipName(
      factionIndex = factionIndex))
  return tclOk

proc startGame*() {.raises: [], tags: [WriteIOEffect, TimeEffect, RootEffect,
    ReadIOEffect, RootEffect], contractual.} =
  ##  Start the game
  const mainWindow: string = "."
  var x: int = try:
      ((tclEval2(script = "winfo vrootwidth " & mainWindow).parseInt -
        gameSettings.windowWidth) / 2).int
    except:
      showError(message = "Can't get window X position")
      return
  if x < 0:
    x = 0
  var y: int = try:
      ((tclEval2(script = "winfo vrootheight " & mainWindow).parseInt -
        gameSettings.windowHeight) / 2).int
    except:
      showError(message = "Can't get window Y position")
      return
  if y < 0:
    y = 0
  tclEval(script = "wm geometry . " & $gameSettings.windowWidth & "x" &
      $gameSettings.windowHeight & "+" & $x & "+" & $y)
  createGameUi()

proc newGameCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    ReadIOEffect, WriteIOEffect, TimeEffect, RootEffect, RootEffect], cdecl,
        contractual.} =
  ## Set all parameters and start a new game
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## NewGame
  newGameSettings.playerGender = tclGetVar(varName = "playergender")[0]
  const playerFrameName: string = ".newgamemenu.canvas.player"
  let goalButton: string = playerFrameName & ".goal"
  if tclEval2(script = goalButton & " cget -text") == "Random":
    clearCurrentGoal()
    currentGoal = try:
        goalsList[getRandom(min = 1, max = goalsList.len)]
      except:
        try:
          goalsList[getRandom(min = 1, max = goalsList.len)]
        except:
          return showError(message = "Can't set the current goal.")
  var textEntry: string = playerFrameName & ".playername"
  newGameSettings.playerName = tclEval2(script = textEntry & " get")
  textEntry = playerFrameName & ".shipname"
  newGameSettings.shipName = tclEval2(script = textEntry & " get")
  var comboBox: string = playerFrameName & ".faction"
  if tclEval2(script = comboBox & " get") == "Random":
    newGameSettings.playerFaction = "random"
  else:
    block setFaction:
      for index, faction in factionsList:
        if faction.name == tclEval2(script = comboBox & " get"):
          newGameSettings.playerFaction = index
          comboBox = playerFrameName & ".career"
          for key, career in faction.careers:
            if career.name == tclEval2(script = comboBox & " get"):
              newGameSettings.playerCareer = key
              break setFaction
  comboBox = playerFrameName & ".career"
  if tclEval2(script = comboBox & " get") == "Random":
    newGameSettings.playerCareer = "random"
  comboBox = playerFrameName & ".base"
  newGameSettings.startingBase = "Any"
  for index, baseType in basesTypesList:
    if baseType.name == tclEval2(script = comboBox & " get"):
      newGameSettings.startingBase = index
      break
  const difficultyFrameName: string = ".newgamemenu.canvas.difficulty"
  comboBox = difficultyFrameName & ".difficultylevel"
  newGameSettings.difficultyLevel = try:
      tclEval2(script = comboBox &
        " current").parseInt.DifficultyType
    except:
      return showError(message = "Can't set difficulty.")
  var spinBox: string = difficultyFrameName & ".enemydamage"
  newGameSettings.enemyDamageBonus = try:
      tclEval2(script = spinBox &
        " get").parseFloat / 100.0
    except:
      return showError(message = "Can't set enemy bonus.")
  spinBox = difficultyFrameName & ".playerdamage"
  newGameSettings.playerDamageBonus = try:
      tclEval2(script = spinBox &
        " get").parseFloat / 100.0
    except:
      return showError(message = "Can't set player bonus.")
  spinBox = difficultyFrameName & ".enemymeleedamage"
  newGameSettings.enemyMeleeDamageBonus = try:
      tclEval2(script = spinBox &
        " get").parseFloat / 100.0
    except:
      return showError(message = "Can't set enemy melee bonus.")
  spinBox = difficultyFrameName & ".playermeleedamage"
  newGameSettings.playerMeleeDamageBonus = try:
      tclEval2(script = spinBox &
        " get").parseFloat / 100.0
    except:
      return showError(message = "Can't set player melee bonus.")
  spinBox = difficultyFrameName & ".experience"
  newGameSettings.experienceBonus = try:
      tclEval2(script = spinBox &
        " get").parseFloat / 100.0
    except:
      return showError(message = "Can't set experience bonus.")
  spinBox = difficultyFrameName & ".reputation"
  newGameSettings.reputationBonus = try:
      tclEval2(script = spinBox &
        " get").parseFloat / 100.0
    except:
      return showError(message = "Can't set reputation bonus.")
  spinBox = difficultyFrameName & ".upgrade"
  newGameSettings.upgradeCostBonus = try:
      tclEval2(script = spinBox &
        " get").parseFloat / 100.0
    except:
      return showError(message = "Can't set upgrade cost bonus.")
  spinBox = difficultyFrameName & ".prices"
  newGameSettings.pricesBonus = try:
      tclEval2(script = spinBox &
        " get").parseFloat / 100.0
    except:
      return showError(message = "Can't set prices bonus.")
  try:
    newGame()
  except:
    return showError(message = "Can't start the new game.")
  startGame()
  return tclOk

proc showLoadGameMenuCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [], cdecl,
        contractual.} =
  ## Show available options for the selected saved game
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowLoadGameMenu file
  ## File is the filename of the saved game to manipulate
  let loadMenu: string = createDialog(name = ".loadfilemenu", title = "Actions",
      parentName = ".")

  proc addButton(name, label, command: string) {.raises: [], tags: [],
      contractual.} =
    ## Add a button to the selected saved game menu
    ##
    ## * name    - the name of the button
    ## * label   - the text to display on the button
    ## * command - the Tcl command to execute when the button is pressed
    let button: string = loadMenu & name
    tclEval(script = "ttk::button " & button & " -text {" & label &
        "} -command {CloseDialog " & loadMenu & " .;" & command & "}")
    tclEval(script = "grid " & button & " -sticky we -padx 5" & (
        if command.len == 0: " -pady {0 3}" else: ""))
    tclEval(script = "bind " & button & " <Escape> {CloseDialog " & loadMenu & " .;break}")
    if command.len == 0:
      tclEval(script = "bind " & button & " <Tab> {focus " & loadMenu & ".load;break}")
      tclEval(script = "focus " & button)

  addButton(name = ".load", label = "Load the game", command = "LoadGame {" &
      $argv[1] & "}")
  addButton(name = ".delete", label = "Delete the game",
      command = "DeleteGame {" & $argv[1] & "}")
  addButton(name = ".close", label = "Close", command = "")
  showDialog(dialog = loadMenu, parentFrame = ".")
  return tclOk

proc showMainMenuCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
        RootEffect], cdecl, contractual.} =
  ## Clear the main game window and show main menu
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowMainMenu
  tclEval(script = closeButton & " configure -command ShowSkyMap")
  tclSetVar(varName = "gamestate", newValue = "general")
  tclEval(script = "grid remove " & closeButton)
  showScreen(newScreenName = "mapframe")
  tclEval(script = "DrawMap")
  tclEval(script = "update")
  showMainMenu()
  return tclOk

proc loadGameCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    WriteIOEffect, TimeEffect, RootEffect, ReadIOEffect, RootEffect], cdecl,
        contractual.} =
  ## Load the selected save file and start the game
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## LoadGame file
  ## File is the name of the saved game which will be loaded
  tclEval(script = "pack forget .loadmenu")
  saveName = $argv[1]
  try:
    loadGame()
    startGame()
  except:
    showMainMenu()
    showMessage(text = "Can't load this game. Reason: " &
        getCurrentExceptionMsg(), parentFrame = ".",
        title = "Can't load the game")
  return tclOk

type SaveSortOrders = enum
  playerAsc, playerDesc, shipAsc, shipDesc, timeAsc, timeDesc

{.push ruleOff: "varDeclared".}
var
  loadTable: TableWidget
  saveSortOrder: SaveSortOrders = timeDesc
{.pop ruleOn: "varDeclared".}

proc showLoadGameCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    ReadDirEffect, RootEffect], cdecl, contractual.} =
  ## Show the list of available saved games
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## ShowLoadGame
  if loadTable.rowHeight == 0:
    loadTable = createTable(parent = ".loadmenu.list", headers = @[
        "Player name", "Ship name", "Last saved"], command = "SortSaves",
        tooltipText = "Press mouse button to sort the saved games.")
  else:
    clearTable(table = loadTable)
  type SaveRecord = object
    playerName, shipName, saveTime, fileName: string
  var saves: seq[SaveRecord] = @[]
  try:
    for file in walkFiles(pattern = saveDirectory.string & "*.sav"):
      let
        (_, name, _) = splitFile(path = file)
        parts = name.split(sep = '_')
      try:
        if parts.len == 3:
          saves.add(y = SaveRecord(playerName: parts[0], shipName: parts[1],
              saveTime: file.getLastModificationTime.format(f =
                  "yyyy-MM-dd hh:mm:ss"),
              fileName: file))
        else:
          saves.add(y = SaveRecord(playerName: "Unknown", shipName: "Unknown",
              saveTime: file.getLastModificationTime.format(f =
                  "yyyy-MM-dd hh:mm:ss"),
              fileName: file))
      except:
        showError(message = "Can't add information about the save file.")
        return
  except:
    showError(message = "Can't read saved games files")

  proc sortSaves(x, y: SaveRecord): int {.raises: [], tags: [], contractual.} =
    ## Check how to sort the selected saves on the list
    ##
    ## * x - the first save to sort
    ## * y - the second save to sort
    ##
    ## Returns 1 if the x save should go first, otherwise -1
    case saveSortOrder
    of playerAsc:
      if x.playerName < y.playerName:
        return 1
      return -1
    of playerDesc:
      if x.playerName > y.playerName:
        return 1
      return -1
    of shipAsc:
      if x.shipName < y.shipName:
        return 1
      return -1
    of shipDesc:
      if x.shipName > y.shipName:
        return 1
      return -1
    of timeAsc:
      if x.saveTime < y.saveTime:
        return 1
      return -1
    of timeDesc:
      if x.saveTime > y.saveTime:
        return 1
      return -1
  saves.sort(cmp = sortSaves)
  for save in saves:
    addButton(table = loadTable, text = save.playerName,
        tooltip = "Press mouse " & (
        if gameSettings.rightButton: "right" else: "left") &
        " button to show available option", command = "ShowLoadGameMenu {" &
        save.fileName & "}", column = 1)
    addButton(table = loadTable, text = save.shipName,
        tooltip = "Press mouse " & (
        if gameSettings.rightButton: "right" else: "left") &
        " button to show available option", command = "ShowLoadGameMenu {" &
        save.fileName & "}", column = 2)
    addButton(table = loadTable, text = save.saveTime,
        tooltip = "Press mouse " & (
        if gameSettings.rightButton: "right" else: "left") &
        " button to show available option", command = "ShowLoadGameMenu {" &
        save.fileName & "}", column = 3, newRow = true)
  updateTable(table = loadTable)
  if loadTable.row == 1:
    tclEval(script = "bind . <Alt-b> {}")
    tclEval(script = "bind . <Escape> {}")
    tclEval(script = "pack forget .loadmenu")
    showMainMenu()
  return tclOk

proc sortSavesCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.raises: [], tags: [
    WriteIOEffect, TimeEffect, RootEffect, RootEffect], cdecl, contractual.} =
  ## Sort the saved games list
  ##
  ## * clientData - the additional data for the Tcl command
  ## * interp     - the Tcl interpreter on which the command was executed
  ## * argc       - the amount of arguments entered for the command
  ## * argv       - the list of the command's arguments
  ##
  ## The procedure always return tclOk
  ##
  ## Tcl:
  ## SortSaves x
  ## X is X axis coordinate where the player clicked the mouse button
  let column: Natural = try:
      getColumnNumber(table = loadTable, xPosition = ($argv[1]).parseInt)
    except:
      return showError(message = "Can't get the column number.")
  case column
  of 1:
    if saveSortOrder == playerAsc:
      saveSortOrder = playerDesc
    else:
      saveSortOrder = playerAsc
  of 2:
    if saveSortOrder == shipAsc:
      saveSortOrder = shipDesc
    else:
      saveSortOrder = shipAsc
  of 3:
    if saveSortOrder == timeAsc:
      saveSortOrder = timeDesc
    else:
      saveSortOrder = timeAsc
  else:
    discard
  return showLoadGameCommand(clientData = clientData, interp = interp,
      argc = argc, argv = argv)

proc addCommands*() {.raises: [], tags: [WriteIOEffect, TimeEffect,
    RootEffect], contractual.} =
  ## Adds Tcl commands related to the main menu
  try:
    addCommand(name = "OpenLink", nimProc = openLinkCommand)
    addCommand(name = "ShowFile", nimProc = showFileCommand)
    addCommand(name = "ShowNews", nimProc = showNewsCommand)
    addCommand(name = "ShowHallOfFame", nimProc = showHallOfFameCommand)
    addCommand(name = "DeleteGame", nimProc = deleteGameCommand)
    addCommand(name = "SetFaction", nimProc = setFactionCommand)
    addCommand(name = "SetCareer", nimProc = setCareerCommand)
    addCommand(name = "SetBase", nimProc = setBaseCommand)
    addCommand(name = "RandomName", nimProc = randomNameCommand)
    addCommand(name = "NewGame", nimProc = newGameCommand)
    addCommand(name = "ShowLoadGameMenu", nimProc = showLoadGameMenuCommand)
    addCommand(name = "ShowMainMenu", nimProc = showMainMenuCommand)
    addCommand(name = "LoadGame", nimProc = loadGameCommand)
    addCommand(name = "ShowLoadGame", nimProc = showLoadGameCommand)
    addCommand(name = "SortSaves", nimProc = sortSavesCommand)
  except:
    showError(message = "Can't add a Tcl command.")
