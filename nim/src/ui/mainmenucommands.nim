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

import std/[os, osproc, strutils, tables]
import ../[basestypes, config, game, game2, goals, halloffame, ships2,
    shipscrew, tk, utils]
import dialogs, errordialog

proc openLinkCommand*(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
        ReadIOEffect, ExecIOEffect, RootEffect], exportc.} =
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
  let command = try:
        findExe(exe = (if hostOs == "windows": "start" elif hostOs ==
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
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
    ReadDirEffect, ReadIOEffect, WriteIOEffect, TimeEffect], exportc.} =
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
  let textView = ".showfilemenu.text"
  tclEval(script = textView & " configure -state normal")
  tclEval(script = textView & " delete 1.0 end")
  let fileName = $argv[1]
  if fileExists(filename = docDirectory & fileName):
    try:
      for line in lines(docDirectory & fileName):
        tclEval(script = textView & " insert end {" & line & "\n}")
    except:
      showError(message = "Can't read file '" & fileName & "'.")
  else:
    tclEval(script = textView & " insert end {Can't find file to load. Did '" &
        fileName & "' file is in '" & docDirectory & "' directory?}")
  tclEval(script = textView & " configure -state disabled")
  tclEval(script = "bind . <Alt-b> {InvokeButton .showfilemenu.back}")
  tclEval(script = "bind . <Escape> {InvokeButton .showfilemenu.back}")
  return tclOk

var allNews: bool = false

proc showNewsCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
    ReadIOEffect, ReadDirEffect, WriteIOEffect, TimeEffect], exportc.} =
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
  let allNewsButton = ".newsmenu.showall"
  if argv[1] == "false":
    allNews = false
    tclEval(script = allNewsButton & " configure -text {Show all changes} -command {ShowNews true}")
    tclEval(script = "tooltip::tooltip " & allNewsButton & " \"Show all changes to the game since previous big stable version\"")
  else:
    allNews = true
    tclEval(script = allNewsButton & " configure -text {Show only newest changes} -command {ShowNews false}")
    tclEval(script = "tooltip::tooltip " & allNewsButton & " \"Show only changes to the game since previous relese\"")
  let textView = ".newsmenu.text"
  tclEval(script = textView & " configure -state normal")
  tclEval(script = textView & " delete 1.0 end")
  if fileExists(filename = docDirectory & "CHANGELOG.md"):
    try:
      var index = 0
      for line in lines(docDirectory & "CHANGELOG.md"):
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
        docDirectory & "' directory?}")
  tclEval(script = textView & " configure -state disabled")
  return tclOk

proc showHallOfFameCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
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
  let hofView = ".hofmenu.view"
  tclEval(script = hofView & " delete [list [" & hofView & " children {}]]")
  for index, entry in hallOfFameArray:
    if entry.points == 0:
      break
    tclEval(script = hofView & " insert {} end -values [list " & $index & " " &
        entry.name & " " & $entry.points & " " & entry.deathReason & "]")
  return tclOk

proc deleteGameCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
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
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
        WriteIOEffect, TimeEffect], exportc.} =
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
  let frameName = ".newgamemenu.canvas.player"
  var comboBox = frameName & ".faction"
  let factionName = tclEval2(script = comboBox & " get")
  var label = ""

  proc updateInfo(newText: string) =
    let infoText = ".newgamemenu.info.text"
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
  let genderFrame = frameName & ".gender"
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
    var values = ""
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
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
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
  let frameName = ".newgamemenu.canvas.player"
  var comboBox = frameName & ".faction"
  let factionName = tclEval2(script = comboBox & " get")
  comboBox = frameName & ".career"
  let
    careerName = tclEval2(script = comboBox & " get")
    infoText = ".newgamemenu.info.text"
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
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
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
  let
    comboBox = ".newgamemenu.canvas.player.base"
    baseName = tclEval2(script = comboBox & " get")
    infoText = ".newgamemenu.info.text"
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
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
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
  let
    comboBox = ".newgamemenu.canvas.player.faction"
    factionName = tclEval2(script = comboBox & " get")
  var factionIndex = ""
  for index, faction in factionsList:
    if faction.name == factionName:
      factionIndex = index
      break
  let nameEntry = ".newgamemenu.canvas.player." & $argv[1] & "name"
  if argv[1] == "player":
    var gender = 'M'
    gender = tclGetVar(varName = "playergender")[0]
    tclEval(script = nameEntry & " delete 0 end")
    tclEval(script = nameEntry & " insert end " & generateMemberName(
        gender = gender, factionIndex = factionIndex))
    return tclOk
  tclEval(script = nameEntry & " delete 0 end")
  tclEval(script = nameEntry & " insert end " & generateShipName(
      factionIndex = factionIndex))
  return tclOk

proc newGameCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [
    ReadIOEffect, WriteIOEffect, TimeEffect], exportc.} =
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
  let
    playerFrameName = ".newgamemenu.canvas.player"
    goalButton = playerFrameName & ".goal"
  if tclEval2(script = goalButton & " cget -text") == "Random":
    clearCurrentGoal()
    currentGoal = try:
        goalsList[getRandom(min = 1, max = goalsList.len)]
      except:
        return showError(message = "Can't set the current goal.")
  var textEntry = playerFrameName & ".playername"
  newGameSettings.playerName = tclEval2(script = textEntry & " get")
  textEntry = playerFrameName & ".shipname"
  newGameSettings.shipName = tclEval2(script = textEntry & " get")
  var comboBox = playerFrameName & ".faction"
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
  let difficultyFrameName = ".newgamemenu.canvas.difficulty"
  comboBox = difficultyFrameName & ".difficultylevel"
  newGameSettings.difficultyLevel = try:
      tclEval2(script = comboBox &
        " current").parseInt.DifficultyType
    except:
      return showError(message = "Can't set difficulty.")
  var spinBox = difficultyFrameName & ".enemydamage"
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
  # startGame()
  return tclOk

proc showLoadGameMenuCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.sideEffect, raises: [], tags: [], exportc.} =
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
  let loadMenu = createDialog(name = ".loadfilemenu", title = "Actions",
      parentName = ".")

  proc addButton(name, label, command: string) =
    let button = loadMenu & name
    tclEval(script = "ttk::button " & button & " -text {" & label &
        "} -command {CloseDialog " & loadMenu & " .;" & command & "}")
    tclEval(script = "grid " & button & " -sticky we -padx 5" & (
        if command.len == 0: " -pady {0 3}" else: ""))
    tclEval(script = "bind " & button & " <Escape> {CloseDialog " & loadMenu & " .;break}")
    if command.len == 0:
      tclEval(script = "bind " & button & " <Tab> {focus " & loadMenu & ".load;break}")
      tclEval(script = "focus " & button)

  addButton(name = ".load", label = "Load the game", command = "LoadGame " &
      $argv[1])
  addButton(name = ".delete", label = "Delete the game",
      command = "DeleteGame " & $argv[1])
  addButton(name = ".close", label = "Close", command = "")
  showDialog(dialog = loadMenu, parentFrame = ".")
  return tclOk

proc addCommands*() =
  discard
#  addCommand("OpenLink", openLinkCommand)
#  addCommand("ShowFile", showFileCommand)
#  addCommand("ShowNews", showNewsCommand)
#  addCommand("ShowHallOfFame", showHallOfFameCommand)
#  addCommand("DeleteGame", deleteGameCommand)
#  addCommand("SetFaction", setFactionCommand)
#  addCommand("SetCareer", setCareerCommand)
#  addCommand("SetBase", setBaseCommand)
#  addCommand("RandomName", randomNameCommand)
#  addCommand("NewGame", newGameCommand)
#  addCommand("ShowLoadGameMenu", showLoadGameMenuCommand)
